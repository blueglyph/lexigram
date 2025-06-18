// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(dead_code)]
#![allow(unused)]

pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::mem::take;
use std::ops::{Deref, DerefMut};
use iter_index::IndexerIterator;
use vectree::VecTree;
use crate::cproduct::CProduct;
use crate::dfa::TokenId;
use crate::{CollectJoin, General, Normalized, gnode, vaddi, prodf, hashset, LL1, LR, sym, btreeset, prod};
use crate::grammar::NTConversion::{MovedTo, Removed};
use crate::log::{BufLog, Logger};
use crate::symbol_table::SymbolTable;
use crate::take_until::TakeUntilIterator;

pub type VarId = u16;
pub type FactorId = VarId;

#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Symbol {
    T(TokenId),         // terminal
    NT(VarId),          // non-terminal
    #[default] Empty,   // empty symbol
    End                 // end of stream
}

impl Symbol {
    pub fn is_end(&self) -> bool {
        matches!(self, Symbol::End)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Symbol::Empty)
    }

    pub fn is_t(&self) -> bool {
        matches!(self, Symbol::T(_))
    }

    pub fn is_nt(&self) -> bool {
        matches!(self, Symbol::NT(_))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum GrNode {
    Symbol(Symbol),
    Concat,
    Or,
    Maybe,
    Plus,
    Star,
    /// L-form attribute of a factor or a `+` / `*` repetition expression.
    /// - `+` and `*` expressions are either folded or iterative (also called "low latency", since the listener is
    ///   called back immediately after parsing each item, whereas the folded form is only called once all the
    ///   items have been parsed and gathered).
    ///   - The default form is folded. With that form, the items of the repetitions are automatically gathered and handed
    ///     to the listener callback as an array (if the items have a value) once all items have been parsed. For example,
    ///     `A -> a (b)* c` gives a context with the values of a, b, c as variant
    ///     `enum CtxA { A { a: String, b: Vec<String>, c: String } }`.
    ///   - If the L-form is specified, the listener callback is called at each iteration, with a context giving the parsed
    ///     items of that iteration which have a value. The NT used in that loop is defined with the L-form (`LForm(VarId)`),
    ///     and its value serves as accumulator to fold all the successive items into a single value presented in the context
    ///     of the factor that includes the `+` or `*` repetition. For example, `A -> a (<L=AIter> b)* c` uses `AIter`,
    ///     and each time a `b` value is parsed, the listener callback receives a context variant
    ///     `enum CtxAIter { AIter1 { iter: SynAIter, b: String } }`. The callback must return the new `SynAIter` value.
    ///     Once all the iterations are parsed, `c` is parsed, and the listener callback receives the context for `A`:
    ///     `CtxA { A { a: String, star: SynAIter, c: String } }`.
    /// - Right-recursive rules are either stacked or "low-latency".
    ///   - The default form is stacked. A rule `A -> id A | stop` parsing "id1 id2 id3 stop1" yields a sequence
    ///     - `A -> id1 A(1)`, `A(1) -> id2 A(2)`, `A(2) -> id3 A(3)`, `A(3) -> stop1`
    ///
    ///     Since it's recursive, the listener callback is first called for A(3), then A(2), A(1), and finally A. The parser
    ///     puts the intermediate values of `id` on the stack, and once `stop1` is reached, it calls the callback with it,
    ///     then unstacks all the `id` values for the successive callbacks with `id = id3`, `id2`, and finally `id1`,
    ///     together with the loop value `A`, which is updated each time by the callback.
    ///   - The low-latency form, whose `VarId` points to its own NT, calls the listener callback at each iteration and
    ///     doesn't accumulate values on the stack. In the example above, it's first called with `id = id1`, `id2`, `id3`,
    ///     and finally `stop = stop1`, together with the loop value `A`, which is updated each time by the callback.
    LForm(VarId),   // applied to NT
    RAssoc          // applied to factor
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Empty => write!(f, "ε"),
            Symbol::T(id) => write!(f, ":{id}"),
            Symbol::NT(id) => write!(f, "{id}"),
            Symbol::End => write!(f, "$"),
        }
    }
}

impl Display for GrNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GrNode::Symbol(s) => write!(f, "{s}"),
            GrNode::Concat => write!(f, "&"),
            GrNode::Or => write!(f, "|"),
            GrNode::Maybe => write!(f, "?"),
            GrNode::Plus => write!(f, "+"),
            GrNode::Star => write!(f, "*"),
            GrNode::LForm(v) => write!(f, "<L={v}>"),
            GrNode::RAssoc => write!(f, "<R>")
        }
    }
}

impl Symbol {
    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        symbol_table.map(|t| t.get_name(self)).unwrap_or(self.to_string())
    }

    pub fn to_str_ext(&self, symbol_table: Option<&SymbolTable>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(t) = symbol_table {
            if t.is_symbol_t_data(self) {
                result.push_str(&format!("({ext})"));
            }
        }
        result
    }
}

impl GrNode {
    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        match self {
            GrNode::Symbol(s) => symbol_table.map(|t| t.get_name(s)).unwrap_or(s.to_string()),
            GrNode::LForm(v) => format!("<L={}>", symbol_table.map(|t| t.get_name(&Symbol::NT(*v))).unwrap_or(v.to_string())),
            _ => self.to_string()
        }
    }
}

// ---------------------------------------------------------------------------------------------

/// Simple index object that returns `Original(<value>)` on the first `index.get()`, then
/// `Copy(<value>)` on subsequent calls. The indices are stored on 31 bits, keeping one bit
/// for the 'original' flag. Trying to store larger values triggers a panic.
#[derive(Clone, Copy)]
pub struct Dup {
    index: u32
}

#[derive(Clone, Copy, Debug)]
enum DupVal {
    Original(u32),
    Copy(u32)
}

impl Dup {
    const MASK: u32 = 1 << (u32::BITS - 1);

    fn new(index: usize) -> Self {
        assert!(index < Self::MASK as usize);
        Self { index: index as u32 }
    }

    fn get(&mut self) -> DupVal {
        let idx = self.index;
        if idx & Self::MASK == 0 {
            self.index |= Self::MASK;
            DupVal::Original(idx)
        } else {
            DupVal::Copy(idx & !Self::MASK)
        }
    }

    fn peek(&self) -> usize {
        (self.index & !Self::MASK) as usize
    }
}

impl std::fmt::Debug for Dup {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Dup{{")?;
        if self.index & Self::MASK == 0 {
            write!(f, "{}}}", self.index)
        } else {
            write!(f, "copy {}}}", self.index & !Dup::MASK)
        }
    }
}

// ---------------------------------------------------------------------------------------------

pub type GrTree = VecTree<GrNode>;

/// Adds methods to GrTree.
///
/// _NOTE: We must create a trait for GrTree since we can't implement functions for an external type,
/// and a type alias is not considered as a new type._
pub trait GrTreeExt {
    fn get_dup(&mut self, dup_index: &mut Dup) -> usize;
    fn to_str(&self, start_node: Option<usize>, symbol_table: Option<&SymbolTable>) -> String;
}

impl GrTreeExt for GrTree {
    fn get_dup(&mut self, dup_index: &mut Dup) -> usize {
        match dup_index.get() {
            DupVal::Original(index) => index as usize,
            DupVal::Copy(index) => {
                let node = self.get(index as usize).clone();
                self.add(None, node)
            }
        }
    }

    fn to_str(&self, start_node: Option<usize>, symbol_table: Option<&SymbolTable>) -> String {
        let tfmt = GrTreeFmt {
            tree: &self,
            show_ids: false,
            show_depth: false,
            symbol_table,
            start_node,
        };
        tfmt.to_string()
    }
}

struct GrTreeFmt<'a> {
    tree: &'a GrTree,
    show_ids: bool,
    show_depth: bool,
    symbol_table: Option<&'a SymbolTable>,
    start_node: Option<usize>
}

impl<'a> GrTreeFmt<'a> {
    fn snode(&self, node: &GrNode, node_id: usize, depth: u32) -> String {
        let show_ids = self.show_ids;
        let show_depth = self.show_depth;
        let mut result = String::new();
        if show_depth {
            result.push_str(&depth.to_string());
            result.push('>');
        }
        if show_ids {
            result.push_str(&node_id.to_string());
            result.push(':');
        }
        let name = if let GrNode::Symbol(sym) = node {
            sym.to_str(self.symbol_table)
        } else {
            node.to_string()
        };
        result.push_str(name.as_str());
        result
    }
}

impl Display for GrTreeFmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let start_node = self.start_node.unwrap_or(self.tree.get_root().expect("the tree must have a defined root"));
        let mut stack = Vec::<String>::new();
        for node in self.tree.iter_depth_at(start_node) {
            let n = node.num_children();
            if n > 0 {
                let children = stack.drain(stack.len() - n..).join(", ");
                stack.push(format!("{}({children})", self.snode(&node, node.index, node.depth)));
            } else {
                stack.push(self.snode(&node, node.index, node.depth));
            }
        }
        write!(f, "{}", stack.pop().unwrap_or("empty".to_string()))
    }
}

// ---------------------------------------------------------------------------------------------

// easier to use than an enum
pub mod ruleflag {
    use crate::btreemap;

    /// Star or Plus repeat child factor.
    /// Set by `RuleTreeSet<General>::normalize_plus_or_star()` in `flags`.
    pub const CHILD_REPEAT: u32 = 1;
    /// Right-recursive child NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const R_RECURSION: u32 = 2;
    /// Left-recursive child NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_L_RECURSION: u32 = 4;
    /// Left-recursive, ambiguous child NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_AMBIGUITY: u32 = 8;
    /// Child NT created to regroup the independent factors when transforming an ambiguous, recursive rule.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_INDEPENDENT_AMBIGUITY: u32 = 16;
    /// Left-factorized parent NT.
    /// Set by `ProdRuleSet<T>::left_factorize()` in `flags`.
    pub const PARENT_L_FACTOR: u32 = 32;
    /// Left-factorized child NT.
    /// Set by `ProdRuleSet<T>::left_factorize()` in `flags`.
    pub const CHILD_L_FACTOR: u32 = 64;
    /// Low-latency non-terminal factor, used with `CHILD_REPEAT` or `R_RECURSION`.
    /// Set by `ProdRuleSet<General>::from(rules: From<RuleTreeSet<Normalized>>` in `flags`.
    pub const L_FORM: u32 = 128;
    /// Right-associative factor.
    /// Set by `ProdRuleSet<General>::from(rules: From<RuleTreeSet<Normalized>>` in factors.
    pub const R_ASSOC: u32 = 256;
    /// Left-recursive parent NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const PARENT_L_RECURSION: u32 = 512;
    /// Left-recursive, ambiguous parent NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const PARENT_AMBIGUITY: u32 = 1024;
    /// Star or Plus repeat parent factor.
    /// Set by `RuleTreeSet<General>::normalize_plus_or_star()` in `flags`.
    pub const PARENT_REPEAT: u32 = 2048;
    /// CHILD_REPEAT and PARENT_REPEAT is +, not * (used with both flags)
    pub const REPEAT_PLUS: u32 = 4096;
    /// GREEDY factor: is expected to generate an ambiguity in the parsing table
    pub const GREEDY: u32 = 8192;

    pub const TRANSF_PARENT: u32 = R_RECURSION | PARENT_L_FACTOR | PARENT_L_RECURSION | PARENT_AMBIGUITY | PARENT_REPEAT;
    pub const TRANSF_CHILD: u32 = CHILD_REPEAT | CHILD_L_RECURSION | CHILD_AMBIGUITY | CHILD_L_FACTOR;
    pub const TRANSF_CHILD_AMB: u32 = CHILD_AMBIGUITY | R_RECURSION | L_FORM;
    pub const FACTOR_INFO: u32 = L_FORM | R_ASSOC | GREEDY;

    pub fn to_string(flags: u32) -> Vec<String> {
        static NAMES: [(u32, &str); 14] = [
            (CHILD_REPEAT               , "child_+_or_*"),
            (R_RECURSION                , "right_rec"),
            (CHILD_L_RECURSION          , "child_left_rec"),
            (CHILD_AMBIGUITY            , "child_amb"),
            (CHILD_INDEPENDENT_AMBIGUITY, "child_ind_amb"),
            (PARENT_L_FACTOR            , "parent_left_fact"),
            (CHILD_L_FACTOR             , "child_left_fact"),
            (L_FORM                     , "L-form"),
            (R_ASSOC                    , "R-assoc"),
            (PARENT_L_RECURSION         , "parent_left_rec"),
            (PARENT_AMBIGUITY           , "parent_amb"),
            (PARENT_REPEAT              , "parent_+_or_*"),
            (REPEAT_PLUS                , "plus"),
            (GREEDY                     , "greedy"),
        ];
        NAMES.iter().filter_map(|(f, t)| if flags & f != 0 { Some(t.to_string()) } else { None } ).collect()
    }

    pub fn factor_info_to_string(mut flags: u32) -> Vec<String> {
        static NAMES: [(u32, &str); 3] = [(L_FORM, "L"), (R_ASSOC, "R"), (GREEDY, "G")];
        let mut v: Vec<String> = NAMES.iter().filter_map(|(f, t)|
            if flags & f != 0 {
                flags &= !f;
                Some(t.to_string())
            } else {
                None
            }).collect();
        if flags != 0 {
            v.push(flags.to_string())
        }
        v
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NTConversion {
    /// Removed because not used
    Removed,
    /// + or * child moved to L-form holder
    MovedTo(VarId)
}

#[derive(Clone, Debug)]
pub struct RuleTreeSet<T> {
    trees: Vec<GrTree>,
    start: Option<VarId>,
    symbol_table: Option<SymbolTable>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    // priority: Vec<Vec<u16>>, // factor -> priority
    nt_conversion: HashMap<VarId, NTConversion>,
    log: BufLog,
    _phantom: PhantomData<T>
}

// Methods for both General and Normalized forms. There can only be immutable methods
// in the normalized form.
impl<T> RuleTreeSet<T> {
    pub fn get_log(&self) -> &BufLog {
        &self.log
    }

    pub fn get_tree(&self, var: VarId) -> Option<&GrTree> {
        self.trees.get(var as usize)
    }

    /// Returns all the non-empty trees
    pub fn get_trees_iter(&self) -> impl Iterator<Item=(VarId, &GrTree)> {
        self.trees.iter().index().filter_map(|(id, t)| if t.is_empty() { None } else { Some((id, t)) })
    }

    /// Returns all the variables corresponding to a non-empty tree
    pub fn get_vars(&self) -> impl Iterator<Item=VarId> + '_ {
        (0..self.trees.len()).filter_map(|id| if self.trees[id].is_empty() { None } else { Some(id as VarId) })
    }

    /// Returns a variable ID that doesn't exist yet.
    pub fn get_next_available_var(&self) -> VarId {
        self.trees.len() as VarId
    }

    /// Returns a set of all the terminals used in the ruleset.
    pub fn get_terminals(&self) -> HashSet<TokenId> {
        let mut tset = self.trees.iter()
            .flat_map(|t| t.iter_depth_simple())
            .filter_map(|x| if let GrNode::Symbol(Symbol::T(t)) = x.deref() { Some(*t) } else { None })
            .collect::<HashSet<_>>();
        tset
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table);
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        self.symbol_table.as_ref()
    }

    /// Sets the starting production rule.
    pub fn set_start(&mut self, start: VarId) {
        self.start = Some(start);
    }
}

// Mutable methods for the General form.
impl RuleTreeSet<General> {
    pub fn new() -> Self {
        Self::with_log(BufLog::new())
    }

    pub fn with_log(log: BufLog) -> Self {
        RuleTreeSet {
            trees: Vec::new(),
            start: None,
            symbol_table: None,
            flags: Vec::new(),
            parent: Vec::new(),
            log,
            nt_conversion: HashMap::new(),
            _phantom: PhantomData,
        }
    }

    /// Gets the tree corresponding to `var`. Creates it if it doesn't exist yet.
    pub fn get_tree_mut(&mut self, var: VarId) -> &mut GrTree {
        let var = var as usize;
        if var >= self.trees.len() {
            self.trees.resize(var + 1, GrTree::new());
        }
        &mut self.trees[var]
    }

    /// Sets the tree corresponding to `var`. If the variable already exists,
    /// the tree is replaced. Otherwise, the set is enlarged to add it.
    pub fn set_tree(&mut self, var: VarId, tree: GrTree) {
        let var = var as usize;
        if var >= self.trees.len() {
            if var > self.trees.len() {
                if self.trees.capacity() < var + 1 {
                    // if capacity = 2, var = 3 => we need 4, so 2 more
                    self.trees.reserve(var + 1 - self.trees.capacity())
                }
                self.trees.resize(var, GrTree::new());
            }
            self.trees.push(tree);
        } else {
            self.trees[var] = tree;
        }
    }

    /// Normalizes all the production rules.
    pub fn normalize(&mut self) {
        let vars = self.get_vars().to_vec();
        for var in vars {
            self.normalize_var(var);
        }
        self.flags.resize(self.trees.len(), 0);
        self.parent.resize(self.trees.len(), None);
    }

    /// Transforms the production rule tree into a list of rules in normalized format:
    /// `var -> &(leaf_1, leaf_2, ...leaf_n)`
    ///
    /// The product may have to be split if operators like `+` or `*` are used. In this
    /// case, new non-terminals are created, with increasing IDs starting from
    /// `new_var`.
    pub fn normalize_var(&mut self, var: VarId) {
        const VERBOSE: bool = false;
        const VERBOSE_CC: bool = false;
        if VERBOSE { println!("normalize_var({})", Symbol::NT(var).to_str(self.get_symbol_table())); }
        let mut new_var = self.get_next_available_var();
        let orig = take(&mut self.trees[var as usize]);
        let mut new = GrTree::new();
        let mut stack = Vec::<usize>::new();    // indices in new
        for sym in orig.iter_depth() {
            let n = sym.num_children();
            if VERBOSE { println!("- old {}:{}", sym.index, sym.deref()); }
            if n == 0 {
                stack.push(new.add(None, orig.get(sym.index).clone()));
                if VERBOSE { print!("  leaf: "); }
            } else {
                match sym.deref() {
                    // we must rearrange the operations so that any item on the stack is only
                    // one of those patterns:
                    // - a leaf
                    // - a &(leaves)
                    // - a |(&(leaves) or leaves)
                    GrNode::Concat | GrNode::Or => {
                        let children = stack.split_off(stack.len() - n);
                        let new_id = if children.iter().all(|&idx| !matches!(new.get(idx), GrNode::Concat|GrNode::Or)) {
                            if VERBOSE { print!("  trivial {}: children={children:?}\n  ", sym.deref()); }
                            // trivial case with only leaves as children (could be removed and treated as a general case)
                            new.addci_iter(None, sym.clone(), children)
                        } else {
                            if let GrNode::Or = sym.deref() {
                                if VERBOSE { println!("  or: children={children:?}"); }
                                // if parent sym is p:|
                                // - preserving the children's order:
                                //   - attach '|' children's children directly under p (discarding the '|' children)
                                //   - attach '&' children under p
                                // - push p back to stack
                                // ex: P: AB | (C|D) | E | (FG|HI)             -> P: AB | C | D | E | FG | HI
                                //        |(&(A,B),|(C,D),E,|(&(F,G),&(H,I)))        |(&(A,B),C,D,E,&(F,G),&(H,I))
                                let mut new_children = Vec::new();
                                for id in children {
                                    match new.get(id) {
                                        GrNode::Symbol(_) | GrNode::Concat => {
                                            if VERBOSE { println!("  - child {id} is {}", new.get(id)); }
                                            new_children.push(id);
                                        }
                                        GrNode::Or => {
                                            if VERBOSE { println!("  - child {id} is | with children {:?}", new.children(id)); }
                                            new_children.extend(new.children(id));
                                        }
                                        x => panic!("unexpected node type under | node: {x}"),
                                    }
                                }
                                new.addci_iter(None, gnode!(|), new_children)
                            } else { // GrNode::Concat
                                if VERBOSE_CC { println!("  &: children={children:?}"); }
                                // if parent sym is p:&
                                // - merge adjacent leaves and '&' children (optional)
                                // - cartesian product of all '|' children's children and '&' children,
                                //       duplicating nodes are required
                                // - add r:'|' node to tree, attaching the new '&' nodes under it
                                // - push r to stack
                                // ex: P: AB & (C|D) & E & (FG|H)        -> P: ABCEFG | ABCEH | ABDEFG | ABDEH
                                //        &(&(A,B),|(C,D),E,|(&(F,G),H))      |(&(A,B,C,E,F,G),&(A,B,C,E,H),&(A,B,D,E,F,G),&(A,B,D,E,H)

                                // we store the dups in an array and reference them by index, because there will be multiple instances
                                // pointing to the same Dup and we can't do that with mutable references (which must be unique):
                                let mut dups = Vec::<Vec<Dup>>::new();
                                let concats_children = children.into_iter()
                                    // iterations: &(A,B) -> |(C,D) -> E -> |(&(F,G),H))
                                    .flat_map(|id| {
                                        if VERBOSE_CC { print!("      FL {}: ", new.get(id)); }
                                        match new.get(id) {
                                            GrNode::Concat =>
                                                new.children(id).iter().map(|idc| vec![vaddi(&mut dups, [Dup::new(*idc)])]).to_vec(),
                                            GrNode::Or => {
                                                let children = new.children(id).to_vec();
                                                vec![children.into_iter().map(|idc| {
                                                    if let GrNode::Concat = new.get(idc) {
                                                        let idc_children = new.children(idc).iter().map(|i| Dup::new(*i)).to_vec();
                                                        vaddi(&mut dups, idc_children)
                                                    } else {
                                                        vaddi(&mut dups, [Dup::new(idc)])
                                                    }
                                                }).to_vec()]
                                            }
                                            _ => vec![vec![vaddi(&mut dups, [Dup::new(id)])]],
                                        }
                                    })
                                    // [d(A)] -> [d(B)] -> [d(C),d(D)] -> [d(E)] -> [d(&(d(F),d(G))),d(H)]
                                    // .inspect(|x| println!("      >> {}", x.iter().map(|i| format!("_{i}")).join(", ")))
                                    .cproduct()
                                    // .inspect(|x| println!("      << {}", x.iter().map(|i| format!("_{i}")).join(", ")))
                                    // [dup(A),dup(B),dup(C),dup(E),d(&)] -> [dup(A),dup(B),dup(C),dup(E),d(H)] ->
                                    //       [dup(A),dup(B),dup(D),dup(E),d(&)] -> [dup(A),dup(B),dup(D),dup(E),d(H)]
                                    .map(|dup_ids| dup_ids.into_iter()
                                        .flat_map(|dup_id| dups.get_mut(dup_id).unwrap().iter_mut()
                                            .map(|dup| new.get_dup(dup)).to_vec()).to_vec()
                                    )
                                    // .inspect(|x| println!("      :: {}", x.iter().map(|i| format!("{i}")).join(", ")))
                                    .to_vec();
                                    // [A,B,C,E,F,G] -> [A',B',C',E',H] -> [A'',B'',D,E'',F',G'] -> [A''',B''',D',E''',H']
                                let concats = concats_children.into_iter()
                                    .map(|children_ids| new.addci_iter(None, gnode!(&), children_ids))
                                    .to_vec();
                                    // Vec<node id of &-branch>
                                new.addci_iter(None, gnode!(|), concats)
                            }
                        };
                        stack.push(new_id);
                    }
                    GrNode::Maybe => {
                        if n != 1 {
                            self.log.add_error(format!("normalize_var({}): ? should only have one child; found {n}: {}",
                                                       Symbol::NT(var).to_str(self.get_symbol_table()),
                                                       orig.to_str(Some(sym.index), self.get_symbol_table())));
                        } else {
                            // self              new
                            // -------------------------------
                            // ?(A)           -> |(A,ε)
                            // ?(&(A,B))      -> |(&(A,B),ε)
                            // ?(|(&(A,B),C)) -> |(&(A,B),C,ε)
                            if VERBOSE { print!("  ?: "); }
                            let child = stack.pop().unwrap();
                            let empty = new.add(None, gnode!(e));
                            let id = match new.get(child) {
                                GrNode::Or => {
                                    new.add(Some(child), gnode!(e));
                                    child
                                }
                                _ => new.addci_iter(None, gnode!(|), [child, empty])
                            };
                            stack.push(id);
                        }
                    }
                    GrNode::Plus => {
                        if n != 1 {
                            self.log.add_error(format!("normalize_var({}): + should only have one child; found {n}: {}",
                                                       Symbol::NT(var).to_str(self.get_symbol_table()),
                                                       orig.to_str(Some(sym.index), self.get_symbol_table())));
                        } else {
                            // create new production rule:
                            // P -> αβ+γ becomes P -> αQγ
                            //                   Q -> βQ | β
                            //
                            // self              new  new(Q=next_var_id)               simpler format
                            // --------------------------------------------------------------------------------------------------
                            // +(A)           -> Q    |(&(A,Q), A')                    AQ|A
                            // +(&(A,B))      -> Q    |(&(A,B,Q),&(A',B'))             ABQ|AB
                            // +(|(&(A,B),C)) -> Q    |(&(A,B,Q),&(C,Q'),&(A',B'),C')  (AB|C)Q | (AB|C) = ABQ|CQ | AB|C
                            if VERBOSE { print!("  +"); }
                            self.normalize_plus_or_star(&mut stack, &mut new, var, &mut new_var, true);
                        }
                    }
                    GrNode::Star => {
                        if n != 1 {
                            self.log.add_error(format!("normalize_var({}): * should only have one child; found {n}: {}",
                                                       Symbol::NT(var).to_str(self.get_symbol_table()),
                                                       orig.to_str(Some(sym.index), self.get_symbol_table())));
                        } else {
                            // create new production rule:
                            // P -> αβ*γ becomes P -> αQγ
                            //                   Q -> βQ | ε
                            //
                            // self              new  new(Q=next_var_id)     simpler format
                            // -----------------------------------------------------------------------
                            // *(A)           -> Q    |(&(A,Q), ε)           AQ|ε
                            // *(&(A,B))      -> Q    |(&(A,B,Q),ε)          ABQ|ε
                            // *(|(&(A,B),C)) -> Q    |(&(A,B,Q),&(C,Q'),ε)  (AB|C)Q | ε = ABQ|CQ | ε
                            if VERBOSE { print!("  *"); }
                            self.normalize_plus_or_star(&mut stack, &mut new, var, &mut new_var, false);
                        }
                    }
                    _ => panic!("Unexpected {}", sym.deref())
                }
            }
            if VERBOSE {
                println!("stack: {}", stack.iter()
                    .map(|id| {
                        let children = new.children(*id);
                        format!("{id}:{}{}", new.get(*id), if children.is_empty() { "".to_string() } else { format!("({})", children.iter().join(",")) })
                    }).join(", ")
                );
            }
        }
        if stack.len() != 1 {
            self.log.add_error(format!("normalize_var({}): error while normalizing the rules, {} remaining nodes instead of 1",
                                       Symbol::NT(var).to_str(self.get_symbol_table()), stack.len()));
            return;
        }
        if VERBOSE_CC { println!("Final stack id: {}", stack[0]); }
        new.set_root(stack.pop().unwrap());
        self.set_tree(var, new);
    }

    fn normalize_plus_or_star(&mut self, stack: &mut Vec<usize>, new: &mut VecTree<GrNode>, var: VarId, new_var: &mut VarId, is_plus: bool) {
        const VERBOSE: bool = false;
        self.symbol_table.as_ref().map(|st| assert_eq!(st.get_num_nt(), self.trees.len(), "number of nt in symbol table doesn't match num_nt"));
        let mut qtree = GrTree::new();
        let child = stack.pop().unwrap();
        let mut lform_nt = None;
        // See comments in `normalize` near the calls to this method for details about the operations below
        match new.get(child) {
            GrNode::Symbol(s) => {
                if VERBOSE { print!("({child}:{s}) "); }
                // note: we cannot use the child id in qtree!
                let or = qtree.add_root(gnode!(|));
                let cc = qtree.addc(Some(or), gnode!(&), GrNode::Symbol(s.clone()));
                qtree.add(Some(cc), gnode!(nt *new_var));
                qtree.add(Some(or), if is_plus { GrNode::Symbol(s.clone()) } else { gnode!(e) });
            }
            GrNode::Concat => {
                let children = new.children(child);
                if VERBOSE { print!("({child}:&({})) ", children.iter().join(", ")); }
                let or = qtree.add_root(gnode!(|));
                let cc1 = qtree.add_from_tree_iter(Some(or), new.iter_depth_at(child).inspect(|n| {
                    if let &GrNode::LForm(v) = n.deref() {
                        lform_nt = Some(v); // TODO: check that it's not already set (uniqueness)
                    }
                }));
                qtree.add(Some(cc1), gnode!(nt *new_var));
                if is_plus {
                    qtree.add_from_tree(Some(or), &new, Some(child));
                } else {
                    qtree.add(Some(or), gnode!(e));
                }
            }
            GrNode::Or => {
                let children = new.children(child);
                if VERBOSE { print!("({child}:|({})) ", children.iter().join(", ")); }
                let or = qtree.add_root(gnode!(|));
                for id_child in children {
                    let child = new.get(*id_child);
                    match child {
                        GrNode::Symbol(s) => {
                            qtree.addc_iter(Some(or), gnode!(&), [GrNode::Symbol(s.clone()), gnode!(nt *new_var)]);
                            if is_plus {
                                qtree.add(Some(or), GrNode::Symbol(s.clone()));
                            }
                        }
                        GrNode::Concat => {
                            let cc = qtree.add_from_tree_iter(Some(or), new.iter_depth_at(*id_child).inspect(|n| {
                                if let &GrNode::LForm(v) = n.deref() {
                                    lform_nt = Some(v); // TODO: check that it's not already set (uniqueness)
                                }
                            }));
                            qtree.add(Some(cc), gnode!(nt *new_var));
                            if is_plus {
                                qtree.add_from_tree(Some(or), &new, Some(*id_child));
                            }
                        }
                        x => panic!("unexpected node type under a | node: {x}"),
                    }
                }
                if !is_plus {
                    qtree.add(Some(or), gnode!(e));
                }
            }
            _ => panic!("Unexpected node type under a + node: {}", new.get(child))
        }
        if let Some(v) = lform_nt {
            // `new_var` replaces `v`
            self.nt_conversion.insert(v, MovedTo(*new_var));
        }
        self.symbol_table.as_mut().map(|st| {
            if let Some(v) = lform_nt {
                let name = st.remove_nt_name(v);
                if VERBOSE {
                    println!("table = {st:?}");
                    println!("L-FORM({v}) found, using name of NT({v}) = '{name}' for new NT({new_var})");
                }
                //st.add_var_prime_name(var, *new_var, Some(name));
                assert_eq!(st.add_non_terminal(name), *new_var);
            } else {
                // st.add_var_prime_name(var, *new_var, None);
                assert_eq!(st.add_child_nonterminal(var), *new_var);
            }
        });
        let id = new.add(None, gnode!(nt *new_var));
        assert!(*new_var as usize >= self.trees.len() || self.trees[*new_var as usize].is_empty(), "overwriting tree {new_var}");
        self.set_tree(*new_var, qtree);
        self.flags.resize(*new_var as usize + 1, 0);
        self.parent.resize(*new_var as usize + 1, None);
        let plus_flag = if is_plus { ruleflag::REPEAT_PLUS } else { 0 };
        self.flags[*new_var as usize] = ruleflag::CHILD_REPEAT | plus_flag;
        self.flags[var as usize] |= ruleflag::PARENT_REPEAT | plus_flag;
        self.parent[*new_var as usize] = Some(var);
        if VERBOSE {
            println!("=> {}: parent {}, child {}",
                     if is_plus { "+" } else { "*" },
                     Symbol::NT(var).to_str(self.get_symbol_table()),
                     Symbol::NT(*new_var).to_str(self.get_symbol_table()));
        }
        // We rectify the parent/child relationship in case of cascaded + or *. Since we perform a
        // bottom-up reconstruction, a rule like A -> ( ( a )+ b )+ will yield
        //   A -> A_2
        //   A_1 -> a A_1 | ε       parent: A
        //   A_2 -> A_1 b A_2 | ε   parent: A
        // We want A_1's parent to be A_2. We keep the wrong order in the parent chain: A_1 -> A_2 -> A,
        // which is unfortunate in some later tests but still easier than changing everything here.
        let mut rectify_maybe = None;
        for node in self.get_tree(*new_var).unwrap().iter_depth_simple() {
            if let GrNode::Symbol(Symbol::NT(child)) = node.deref() {
                if *child != *new_var && self.flags[*child as usize] & ruleflag::CHILD_REPEAT != 0 {
                    rectify_maybe = Some(*child);
                    break;
                }
            }
        }
        if let Some(child) = rectify_maybe {
            self.parent[child as usize] = Some(*new_var);
            self.flags[*new_var as usize] |= ruleflag::PARENT_REPEAT;
            if VERBOSE {
                println!("=> rectify {}'s parent as {}",
                         Symbol::NT(child).to_str(self.get_symbol_table()),
                         Symbol::NT(*new_var).to_str(self.get_symbol_table()));
            }
        }
        *new_var += 1;
        stack.push(id);
    }
}

impl From<RuleTreeSet<General>> for RuleTreeSet<Normalized> {
    /// Transforms a `General` ruleset to a `Normalized` ruleset
    fn from(mut rules: RuleTreeSet<General>) -> Self {
        // We handle the errors by transmitting the log to the next construct rather than returning a `Result` type.
        // This allows to cascade the transforms without getting a complicated error resolving system while preserving
        // the information about the errors easily.
        if rules.log.has_no_errors() {
            rules.normalize();
        }
        RuleTreeSet::<Normalized> {
            trees: rules.trees,
            start: rules.start,
            symbol_table: rules.symbol_table,
            flags: rules.flags,
            parent: rules.parent,
            nt_conversion: rules.nt_conversion,
            log: rules.log,
            _phantom: PhantomData
        }
    }
}

// impl From<RuleTreeSet<Normalized>> for RuleTreeSet<General> {
//     /// Transforms a `Normalized` ruleset to a `General` ruleset
//     fn from(mut rules: RuleTreeSet<Normalized>) -> Self {
//         RuleTreeSet::<General> { trees: rules.trees, next_var: rules.next_var, _phantom: PhantomData }
//     }
// }

// ---------------------------------------------------------------------------------------------

/// Stores a normalized production rule, where each factor (e.g. `BC`) is stored in
/// a `Vec<GrNode>` and all the factors are stored in a `Vec`.
///
/// ## Example
/// `A -> BC | D | ε`
///
/// where A=0, B=1, C=2, D=3, is stored as:
///
/// `[[gnode!(nt 1), gnode!(nt 2)],[gnode!(nt 3)],[gnode!(e)]]`
///
/// (where the representation of vectors has been simplified to square brackets).
pub type ProdRule = Vec<ProdFactor>;

/// Stores a factor of a normalized production rule, along with accompanying flags.
/// The `ProdFactor` type behaves like a `Vec<Symbol>` (`Deref` / `DerefMut`), but must be
/// created with `ProdFactor::new(f: Vec<Symbol)`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ProdFactor {
    v: Vec<Symbol>,
    flags: u32          // only for L_FORM and R_ASSOC
}

impl ProdFactor {
    pub fn new(v: Vec<Symbol>) -> Self {
        ProdFactor { v, flags: 0 }
    }

    pub fn with_flags(v: Vec<Symbol>, flags: u32) -> Self {
        ProdFactor { v, flags }
    }

    pub fn symbols(&self) -> &Vec<Symbol> {
        &self.v
    }

    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        let mut s = if self.flags & ruleflag::FACTOR_INFO != 0 {
            format!("<{}>", ruleflag::factor_info_to_string(self.flags).join(","))
        } else {
            String::new()
        };
        s.push_str(&self.v.iter().map(|symbol|
            symbol_table.map(|t| t.get_name(symbol)).unwrap_or(symbol.to_string())
        ).join(" "));
        s
    }

    pub fn to_rule_str(&self, nt: VarId, symbol_table: Option<&SymbolTable>) -> String {
        format!("{} -> {}", Symbol::NT(nt).to_str(symbol_table), self.to_str(symbol_table))
    }

    fn factor_first(&self, first: &HashMap<Symbol, HashSet<Symbol>>) -> HashSet<Symbol> {
        // factor.iter().map(|s| first.get(s).unwrap().clone()).take_until(|h| !h.contains(&Symbol::Empty)).flatten().collect()
        let mut new = HashSet::<Symbol>::new();
        new.extend(first[&self.v[0]].iter().filter(|s| *s != &Symbol::Empty));
        let mut trail = true;
        for i in 0..self.v.len() - 1 {
            let sym_i = &self.v[i];
            if first[sym_i].contains(&Symbol::Empty) {
                new.extend(first[&self.v[i + 1]].iter().filter(|s| *s != &Symbol::Empty));
            } else {
                trail = false;
                break;
            }
        }
        if trail && first[self.last().unwrap()].contains(&Symbol::Empty) {
            new.insert(Symbol::Empty);
        }
        new
    }

    fn is_greedy(&self) -> bool {
        self.flags & ruleflag::GREEDY != 0
    }
}

impl Deref for ProdFactor {
    type Target = Vec<Symbol>;

    fn deref(&self) -> &Self::Target {
        &self.v
    }
}

impl DerefMut for ProdFactor {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.v
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum FactorType { Independant, LeftAssoc, Prefix, RightAssoc, Suffix }

impl FactorType {
    fn from(nt: &Symbol, factor: &ProdFactor) -> Self {
        let left = matches!(factor.first(), Some(var) if var == nt);
        let right = factor.len() > 1 && matches!(factor.last(), Some(var) if var == nt);
        match (left, right) {
            (false, false) => FactorType::Independant,
            (false, true)  => FactorType::Prefix,
            (true, false)  => FactorType::Suffix,
            (true, true)   => if factor.flags & ruleflag::R_ASSOC != 0 { FactorType::RightAssoc } else { FactorType::LeftAssoc },
        }
    }

    fn is_binary(&self) -> bool {
        self == &FactorType::LeftAssoc || self == &FactorType::RightAssoc
    }
}

#[derive(Debug, Clone)]
struct FactorInfo {
    pred_priority: Option<FactorId>,
    ivar: usize,
    ty: FactorType
}

pub fn prod_to_string(prod: &ProdRule, symbol_table: Option<&SymbolTable>) -> String {
    prod.iter().map(|factor| factor.to_str(symbol_table)).join(" | ")
}

#[derive(Debug)]
pub struct LLParsingTable {
    pub num_nt: usize,
    pub num_t: usize,
    pub factors: Vec<(VarId, ProdFactor)>,
    pub table: Vec<FactorId>,
    pub flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    pub parent: Vec<Option<VarId>>, // NT -> parent NT
}

impl LLParsingTable {
    pub fn get_top_parent(&self, nt: VarId) -> VarId {
        let mut var = nt;
        while let Some(parent) = self.parent[var as usize] {
            var = parent;
        }
        var
    }
}

#[derive(Clone, Debug)]
pub struct ProdRuleSet<T> {
    prods: Vec<ProdRule>,
    num_nt: usize,
    num_t: usize,
    symbol_table: Option<SymbolTable>,
    flags: Vec<u32>,
    parent: Vec<Option<VarId>>,
    start: Option<VarId>,
    nt_conversion: HashMap<VarId, NTConversion>,
    pub(crate) log: BufLog,
    dont_remove_recursion: bool,
    _phantom: PhantomData<T>
}

impl<T> ProdRuleSet<T> {
    /// Returns the starting production rule.
    pub fn get_start(&self) -> Option<VarId> {
        self.start
    }

    /// Sets the starting production rule.
    pub fn set_start(&mut self, start: VarId) {
        self.start = Some(start);
    }

    /// Returns a variable ID that doesn't exist yet.
    pub fn get_next_available_var(&self) -> VarId {
        self.prods.len() as VarId   // we don't use self.num_nt for safety reason
    }

    /// Returns all the non-empty prods
    pub fn get_prods_iter(&self) -> impl Iterator<Item=(VarId, &ProdRule)> {
        self.prods.iter().index().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id, p)) })
    }

    pub fn get_prods_iter_mut(&mut self) -> impl Iterator<Item=(VarId, &mut ProdRule)> {
        self.prods.iter_mut().enumerate().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id as VarId, p)) })
    }

    pub fn get_factors(&self) -> impl Iterator<Item=(VarId, &ProdFactor)> {
        self.prods.iter().enumerate()
            .flat_map(|(v, p)| p.iter().map(move |f| (v as VarId, f)))
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table);
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        self.symbol_table.as_ref()
    }

    pub fn get_num_nt(&self) -> usize {
        self.num_nt
    }

    pub fn get_num_t(&self) -> usize {
        self.num_t
    }

    pub fn get_log(&self) -> &BufLog {
        &self.log
    }

    pub fn give_symbol_table(&mut self) -> Option<SymbolTable> {
        take(&mut self.symbol_table)
    }

    pub fn give_nt_conversion(&mut self) -> HashMap<VarId, NTConversion> {
        take(&mut self.nt_conversion)
    }

    /// Adds new flags to `flags[nt]` by or'ing them.
    /// If necessary, extends the `flags` array first.
    fn set_flags(&mut self, nt: VarId, new_flags: u32) {
        let nt = nt as usize;
        if nt >= self.flags.len() {
            self.flags.resize(nt + 1, 0);
        }
        self.flags[nt] |= new_flags;
    }

    fn get_flags(&self, nt: VarId) -> u32 {
        let nt = nt as usize;
        if nt < self.flags.len() {
            self.flags[nt]
        } else {
            0
        }
    }

    fn set_parent(&mut self, child: VarId, parent: VarId) {
        let child = child as usize;
        if child >= self.parent.len() {
            self.parent.resize(child + 1, None);
        }
        self.parent[child] = Some(parent);
    }

    fn get_parent(&self, child: VarId) -> Option<VarId> {
        let child = child as usize;
        if child >= self.parent.len() {
            None
        } else {
            self.parent[child]
        }
    }

    /// Calculates `num_t` and `num_nt` (done right after importing rules).
    /// - `num_t` is calculated on the basis of the higher symbol found in the production rules,
    /// so we can drop any unused symbol that is higher and keep the table width down. We can't
    /// compact the table by removing lower unused symbols, if any, because they are defined by
    /// the lexer.
    /// - `num_nt` is simply the number of production rules.
    fn calc_num_symbols(&mut self) {
        self.num_nt = self.prods.len();
        self.num_t = self.prods.iter().map(|p|
            p.iter().map(|f|
                f.iter().filter_map(|s|
                    if let Symbol::T(v) = s { Some(*v + 1) } else { None }
                ).max().unwrap_or(0)
            ).max().unwrap_or(0)
        ).max().unwrap_or(0) as usize;
        self.flags.resize(self.num_nt, 0);
        self.parent.resize(self.num_nt, None);
    }

    /// Simplifies the productions by removing unnecessary empty symbols.
    fn simplify(&mut self) {
        for p in &mut self.prods {
            let mut has_empty = false;
            let mut i = 0;
            while i < p.len() {
                let f = p.get_mut(i).unwrap();
                let mut j = 0;
                while j < f.len() {
                    if f[j].is_empty() && (j > 0 || j + 1 < f.len()) {
                        f.v.remove(j);
                    } else {
                        j += 1;
                    }
                }
                if f.len() == 1 && f[0].is_empty() {
                    if has_empty {
                        p.remove(i);
                    } else {
                        has_empty = true;
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }
        }
    }

    /// Removes the unused non-terminals and renumbers everything accordingly.
    /// Note that we don't remove unused T symbols because it would create a coherency problem with the lexer.
    ///
    /// Returns the conversion `HashMap[old symbol => new symbol]` for non-terminals.
    fn cleanup_symbols(&mut self, keep: &mut HashSet<Symbol>) {
        const VERBOSE: bool = false;
        let mut nt_content = (0..self.num_nt).map(|v| if self.parent[v].is_none() { Some(v as VarId) } else { None }).to_vec();
        for (from, cnv) in self.nt_conversion.iter() {
            // right now, it only contains MovedTo items from the normalization
            let MovedTo(to) = cnv else { panic!("{cnv:?} unexpected") };
            nt_content[*from as usize] = None;
            nt_content[*to as usize] = Some(*from);
        }
        if VERBOSE {
            println!("Removing unused non-terminals:");
            let mut all_h = self.prods.iter().flat_map(|p| p.iter().map(|x| &x.v).flatten()).cloned().collect::<HashSet<_>>();
            all_h.extend((0..self.num_nt).map(|i| Symbol::NT(i as VarId)));
            let mut all = all_h.into_iter().collect::<Vec<_>>();
            all.sort();
            println!("  current NT symbols: {}", all.iter().filter_map(|s|
                if let Symbol::NT(v) = s { Some(format!("{}", s.to_str(self.get_symbol_table()))) } else { None }).join(", "));
            println!("  current  T symbols: {}", all.iter().filter_map(|s|
                if let Symbol::T(_) = s { Some(s.to_str(self.get_symbol_table())) } else { None }).join(" "));
            let mut used = keep.iter().collect::<Vec<_>>();
            used.sort();
            println!("  used NT symbols:    {}", used.iter().filter_map(|s| if let Symbol::NT(v) = s { Some((v, s)) } else { None })
                .enumerate().map(|(new_id, (id, s))| format!("{}({id} -> {new_id})", s.to_str(self.get_symbol_table()))).join(", "));
            println!("  used  T symbols:    {}", used.iter().filter_map(|s|
                if let Symbol::T(_) = s { Some(s.to_str(self.get_symbol_table())) } else { None }).join(" "));
            println!("  nt_conversion: {:?}", self.nt_conversion);
            println!("  nt_content: {}", nt_content.iter().enumerate()
                .filter_map(|(v, f)| if let Some(from) = f { Some(format!("{v}:{from}")) } else { None } )
                .join(", ")
            );
        }
        self.nt_conversion.clear();
        let new_num_nt = keep.iter().filter(|s| matches!(s, Symbol::NT(_))).count() as VarId;
        let mut new_v = new_num_nt;
        let mut conv = HashMap::<VarId, VarId>::new();
        for i in (0..self.num_nt).rev() {
            let v = i as VarId;
            let symbol = Symbol::NT(v);
            if !keep.contains(&symbol) {
                if VERBOSE { println!("- deleting {}", symbol.to_str(self.get_symbol_table())); }
                if self.parent[i].is_none() {
                    self.nt_conversion.insert(v, Removed);
                }
                nt_content.remove(i);
                self.prods.remove(i);
                self.start = self.start.map(|s| if s >= v { s - 1 } else { s });
                self.symbol_table.as_mut().map(|t| t.remove_non_terminal(v));
                self.flags.remove(i);
                self.parent.remove(i);
            } else {
                new_v -= 1;
                conv.insert(v, new_v);
                if VERBOSE { println!("- {symbol:?} -> {:?}", Symbol::NT(new_v)); }
            }
        }
        for p in &mut self.prods {
            for f in p {
                for s in &mut f.v {
                    if let Symbol::NT(s_var) = s {
                        if let Some(new) = conv.get(s_var) {
                            *s = Symbol::NT(*new);
                        }
                    }
                }
            }
        }
        for p in &mut self.parent {
            if let Some(parent) = p {
                if let Some(new) = conv.get(parent) {
                    *p = Some(*new);
                }
            }
        }
        keep.retain(|s| !matches!(s, Symbol::NT(_)));
        keep.extend((0..new_num_nt as VarId).map(|v| Symbol::NT(v)));
        self.num_nt = new_num_nt as usize;
        if VERBOSE {
            println!("-> nt_content: {}", nt_content.iter().enumerate()
                .filter_map(|(v, f)| if let Some(from) = f { Some(format!("{v}:{from}")) } else { None })
                .join(", ")
            );
        }
        for (to, f) in nt_content.into_iter().index() {
            if let Some(from) = f {
                if from != to {
                    self.nt_conversion.insert(from, MovedTo(to as VarId));
                } else {
                    if self.nt_conversion.get(&from) == Some(&Removed) {
                        self.nt_conversion.remove(&from);
                    }
                }
            }
        };
        if VERBOSE { println!("-> nt_conversion: {:?}", self.nt_conversion); }
    }

    pub fn calc_first(&mut self) -> HashMap<Symbol, HashSet<Symbol>> {
        const VERBOSE: bool = false;
        assert!(self.start.is_some(), "start NT symbol not defined");
        assert!(!self.prods.is_empty(), "no rules");
        let mut symbols = HashSet::<Symbol>::new();
        let mut stack = vec![Symbol::NT(self.start.unwrap())];
        while let Some(sym) = stack.pop() {
            if !symbols.contains(&sym) {
                symbols.insert(sym);
                if let Symbol::NT(v) = sym {
                    stack.extend(self.prods[v as usize].iter().map(|x| &x.v).flatten());
                }
            }
        }
        if symbols.iter().filter(|s| matches!(s, Symbol::NT(v))).count() != self.num_nt {
            let nt_removed = (0..self.num_nt as VarId)
                // warnings about symbols that are not used but that have not been moved because of a */+ L-form:
                .filter(|v| !symbols.contains(&Symbol::NT(*v)) && !matches!(self.nt_conversion.get(v), Some(MovedTo(_))))
                .map(|v| Symbol::NT(v))
                .to_vec();
            if !nt_removed.is_empty() {
                self.log.add_warning(format!("calc_first: unused non-terminals: {}",
                                             nt_removed.into_iter().map(|s| format!("{:?} = {}", s, s.to_str(self.get_symbol_table()))).join(", ")));
            }
            self.cleanup_symbols(&mut symbols);
        }

        let unused_t = (0..self.num_t)
            .filter_map(|t_id| {
                let s = Symbol::T(t_id as VarId);
                if !symbols.contains(&s) { Some(format!("T({t_id}) = {}", s.to_str(self.get_symbol_table()))) } else { None }
            }).to_vec();
        if unused_t.len() > 0 {
            self.log.add_warning(format!("calc_first: unused terminals: {}", unused_t.join(", ")))
        }

        let mut first = symbols.into_iter().map(|sym| {
            match &sym {
                Symbol::T(_) | Symbol::Empty => (sym, hashset![sym]),
                Symbol::NT(_) => (sym, HashSet::new()),
                Symbol::End => panic!("found reserved symbol {sym:?} in production rules"),
            }
        }).collect::<HashMap<_, _>>();
        let mut change = true;
        let rules = (0..self.num_nt as VarId).filter(|var| first.contains_key(&Symbol::NT(*var))).to_vec();
        if VERBOSE { println!("rules: {}", rules.iter().map(|v| Symbol::NT(*v).to_str(self.symbol_table.as_ref())).join(", ")); }
        while change {
            change = false;
            for i in &rules {
                let prod = &self.prods[*i as usize];
                let symbol = Symbol::NT(*i as VarId);
                if VERBOSE { println!("- {} -> {}", symbol.to_str(self.symbol_table.as_ref()), prod_to_string(prod, self.symbol_table.as_ref())); }
                let num_items = first[&symbol].len();
                for factor in prod {
                    if VERBOSE { println!("  - {}", factor.to_str(self.symbol_table.as_ref())); }
                    assert!(factor.len() > 0, "empty factor for {}: {}",
                            symbol.to_str(self.symbol_table.as_ref()), factor.to_str(self.symbol_table.as_ref()));
                    if VERBOSE {
                        print!("    [0] {}", factor[0].to_str(self.symbol_table.as_ref()));
                        println!(", first = {}", first[&factor[0]].iter().map(|s| s.to_str(self.symbol_table.as_ref())).join(", "));
                    }
                    let new = factor.factor_first(&first);
                    let _n = first.get(&symbol).unwrap().len();
                    first.get_mut(&symbol).unwrap().extend(new);
                    if VERBOSE {
                        if first.get(&symbol).unwrap().len() > _n {
                            println!("    first[{}] -> {}", symbol.to_str(self.get_symbol_table()),
                                     first.get(&symbol).unwrap().iter().map(|s| s.to_str(self.get_symbol_table())).join(", "));
                        }
                    }
                }
                change |= first[&symbol].len() > num_items;
            }
            if VERBOSE && change { println!("---------------------------- again"); }
        }
        if self.num_nt == 0 {
            self.log.add_error("calc_first: no non-terminal in grammar".to_string());
        }
        if self.num_t == 0 {
            self.log.add_error("calc_first: no terminal in grammar".to_string());
        }
        first
    }

    pub fn calc_follow(&self, first: &HashMap<Symbol, HashSet<Symbol>>) -> HashMap<Symbol, HashSet<Symbol>> {
        const VERBOSE: bool = false;
        assert!(self.start.is_some(), "start NT symbol not defined");
        let mut follow = first.iter()
            .filter_map(|(s, _)| if matches!(s, Symbol::NT(_)) { Some((*s, HashSet::<Symbol>::new())) } else { None })
            .collect::<HashMap<_, _>>();
        follow.get_mut(&Symbol::NT(self.start.unwrap())).unwrap().insert(Symbol::End);
        let rules = (0..self.num_nt as VarId).filter(|var| follow.contains_key(&Symbol::NT(*var))).to_vec();
        let mut change = true;
        while change {
            change = false;
            for i in &rules {
                let prod = &self.prods[*i as usize];
                let symbol = Symbol::NT(*i as VarId);
                if VERBOSE { println!("- {} -> {}", symbol.to_str(self.symbol_table.as_ref()), prod_to_string(prod, self.symbol_table.as_ref())); }
                for factor in prod {
                    if VERBOSE { println!("  - {}", factor.to_str(self.symbol_table.as_ref())); }
                    let mut trail = follow.get(&symbol).unwrap().clone();
                    for sym_i in factor.iter().rev() {
                        if let Symbol::NT(v) = sym_i {
                            let num_items = follow.get(sym_i).unwrap().len();
                            follow.get_mut(sym_i).unwrap().extend(&trail);
                            if VERBOSE {
                                if follow.get(sym_i).unwrap().len() > num_items {
                                    println!("    follow[{}] -> {}", sym_i.to_str(self.get_symbol_table()),
                                             follow.get(sym_i).unwrap().iter().map(|s| s.to_str(self.get_symbol_table())).join(", "));
                                }
                            }
                            change |= follow.get(sym_i).unwrap().len() > num_items;
                            if first[sym_i].contains(&Symbol::Empty) {
                                trail.extend(first[sym_i].iter().filter(|s| *s != &Symbol::Empty));
                            } else {
                                trail.clear();
                                trail.extend(&first[sym_i]);
                            }
                        } else {
                            trail.clear();
                            trail.insert(*sym_i);
                        }
                    }
                }
            }
            if VERBOSE && change { println!("---------------------------- again"); }
        }
        follow
    }

    pub fn new() -> Self {
        Self {
            prods: Vec::new(),
            num_nt: 0,
            num_t: 0,
            symbol_table: None,
            flags: Vec::new(),
            parent: Vec::new(),
            start: None,
            nt_conversion: HashMap::new(),
            log: BufLog::new(),
            dont_remove_recursion: false,
            _phantom: PhantomData
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            prods: Vec::with_capacity(capacity),
            num_nt: 0,
            num_t: 0,
            symbol_table: None,
            flags: Vec::with_capacity(capacity),
            parent: Vec::with_capacity(capacity),
            start: None,
            nt_conversion: HashMap::new(),
            log: BufLog::new(),
            dont_remove_recursion: false,
            _phantom: PhantomData
        }
    }

    /// Eliminates left recursion from production rules, removes potential ambiguity, and updates the symbol table if provided.
    /// ```eq
    /// A -> A α1 | ... | A αm | A β1 A | ... | A βn A | γ1 | ... | γk
    ///      ^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^
    ///       left recursion           ambiguity
    /// ```
    /// becomes
    /// ```eq
    /// A   -> γ1 A_1 | ... | γk A_1
    /// A_0 -> γ1 | ... | γk
    /// A_1 -> α1 A_1 | ... | αm A_1 | β1 A_0 A_1 | ... | βn A_0 A_1 | ε
    /// ```
    /// (if `k` = 1, `A_0` is unnecessary)
    ///
    /// It requires left-/right-associative reconstruction during parsing, since it transforms the
    /// productions into right-associative ones.
    ///
    /// Note that it could reduce the number of steps in the parsing if we also took into account
    /// other instances of `A` in the middle, like `A β1 A δ1`, but it's not strictly necessary
    /// for an LL(1) grammar so we don't do it.
    pub fn remove_left_recursion(&mut self) {
        if self.dont_remove_recursion {
            return;
        }
        let method = 2;
        if method == 2 {
            return self.remove_recursion();
        }

        const VERBOSE: bool = false;
        const NEW_METHOD: bool = true;  // new ambiguous transformations
        let mut extra = Vec::<ProdRule>::new();
        let mut new_var = self.get_next_available_var();
        // we must take prods out because of the borrow checker and other &mut borrows we need later...
        let mut prods = take(&mut self.prods);
        for (var, prod) in prods.iter_mut().index() {
            let symbol = Symbol::NT(var);
            if prod.iter().any(|p| *p.first().unwrap() == symbol) {
                if VERBOSE {
                    println!("- left recursion: {}", format!("{} -> {}",
                        Symbol::NT(var).to_str(self.get_symbol_table()),
                        prod_to_string(prod, self.get_symbol_table())));
                }
                let (mut recursive, mut fine) : (Vec<_>, Vec<_>) = prod.iter().cloned()
                    .partition(|factor| *factor.first().unwrap() == symbol);
                let (mut ambiguous, mut left) : (Vec<_>, Vec<_>) = recursive.into_iter()
                    .partition(|factor| *factor.last().unwrap() == symbol);
                // => ambiguous: factors with left & right recursion (A -> A α A)
                //    left:      factors with left recursion         (A -> A β)
                //    fine:      factors without left recursion      (A -> γ A | δ)
                if fine.is_empty() || left.iter().any(|f| f.len() < 2) {
                    let mut msg = format!("remove_left_recursion: recursive production: {}", prod_to_string(prod, self.get_symbol_table()));
                    if fine.is_empty() {
                        msg.push_str(&format!("\n- requires factors not starting with {}", symbol.to_str(self.get_symbol_table())));
                    }
                    if let Some(x) = left.iter().find(|f| f.len() < 2) {
                        msg.push_str(&format!("\n- {}", x.to_str(self.get_symbol_table())));
                    }
                    self.log.add_error(msg);
                    continue;
                }

                // apply the transformations
                // - if ambiguous: copies `fine` rule to new variable var_ambig (A_1 -> γ A | δ)
                // - the ProdRule is pushed later into `extra`, the rules added after all the current rules
                let var_ambig = if !ambiguous.is_empty() && fine.len() > 1 {
                    // if more than one independent factor, moves them in a new NT to simplify the resulting rules
                    let var_ambig = new_var;
                    if let Some(table) = &mut self.symbol_table {
                        // table.add_var_prime_name(var, new_var, None);
                        assert_eq!(table.add_child_nonterminal(var), new_var);
                    }
                    self.set_flags(var_ambig, ruleflag::CHILD_INDEPENDENT_AMBIGUITY);
                    self.set_parent(var_ambig, var);
                    new_var += 1;
                    Some(var_ambig)
                } else {
                    None
                };

                // - creates a new var (new_var++) for `left` and, if non-empty, `ambiguous` rules that will be merged later
                let var_prime = new_var;
                new_var += 1;
                self.set_flags(var_prime, ruleflag::CHILD_L_RECURSION | if ambiguous.is_empty() { 0 } else { ruleflag::TRANSF_CHILD_AMB });
                self.set_flags(var, ruleflag::PARENT_L_RECURSION | if ambiguous.is_empty() { 0 } else { ruleflag::PARENT_AMBIGUITY });
                self.set_parent(var_prime, var);
                if let Some(table) = &mut self.symbol_table {
                    // table.add_var_prime_name(var, var_prime, None);
                    assert_eq!(table.add_child_nonterminal(var), var_prime);
                }
                let symbol_prime = Symbol::NT(var_prime);
                if let Some(var_ambig) = var_ambig {
                    // we need to know var_prime, so we can only create that rule content here:
                    let independant = if NEW_METHOD {
                        // `A_1 -> γ A | δ` becomes `A_1 -> γ A_1 A_2 | δ` because we must manage the priorities later in the parser.
                        // If we leave `A`, then all the stricly left-recursive factors have a lower priority than all the ambiguous ones
                        // (since `A` is entirely evaluated before `A_1 -> γ A`)
                        let mut rule = ProdRule::new();
                        for f in &fine {
                            let mut new_f = Vec::<Symbol>::new();
                            for s in &f.v {
                                if s == &symbol {
                                    new_f.push(Symbol::NT(var_ambig));
                                    new_f.push(symbol_prime);
                                } else {
                                    new_f.push(*s);
                                }
                            }
                            rule.push(ProdFactor::with_flags(new_f, f.flags));
                        }
                        rule
                    } else {
                        fine.clone()
                    };
                    extra.push(independant);
                }
                if VERBOSE {
                    print!("- adding non-terminal {var_prime} ({})", symbol_prime.to_str(self.get_symbol_table()));
                    if let Some(v) = var_ambig {
                        print!(" and non-terminal {v} ({})", Symbol::NT(v).to_str(self.get_symbol_table()));
                    }
                    println!(", deriving from {var} ({})", symbol.to_str((self.get_symbol_table())));
                }

                // - if ambiguous, edits the factors in `ambiguous` rule:
                for factor in &mut ambiguous {
                    factor.pop();
                    factor.remove(0);
                    if factor.last().map(|s| *s == symbol).unwrap_or(false) {
                        self.log.add_error(format!("remove_left_recursion: cannot remove recursion from {}", prod_to_string(prod, self.get_symbol_table())));
                        continue;
                    }
                    if let Some(v) = var_ambig {
                        // orig if k > 1:   A   -> A α1 A | A α2 A | A β | γ A | δ (or k factors 'γi' and/or 'δj')
                        // => new:          A_1 -> γ A | δ
                        // => add to prime: A_2 -> α1 A_1 A_2 | α2 A_1 A_2 | β A_2
                        factor.push(Symbol::NT(v));
                    } else {
                        // orig if k = 1:   A   -> A α1 A | A α2 A | A β | δ (or one factor 'γ A' instead of the factor 'δ')
                        // => add to prime: A_2 -> α1 δ A_2 | α1 δ A_2 | β A_2
                        factor.v.extend(fine[0].v.clone());
                    }
                    factor.push(symbol_prime);
                }
                if NEW_METHOD && var_ambig.is_some() {
                    // change A -> A_1 A_2
                    let symbol_ambig = Symbol::NT(var_ambig.unwrap());
                    fine.clear();
                    fine.push(ProdFactor::new(vec![symbol_ambig, symbol_prime]));
                } else {
                    // change A -> γ A A_2 | δ A_2
                    for factor in &mut fine {
                        factor.push(symbol_prime.clone());
                    }
                }
                for factor in &mut left {
                    // add strictly left-recursive factors to prime: A_2 -> β A_2
                    factor.remove(0);
                    if *factor.first().unwrap() == symbol {
                        self.log.add_error(format!("remove_left_recursion: cannot remove recursion from {}", prod_to_string(prod, self.get_symbol_table())));
                        continue;
                    }
                    factor.push(symbol_prime.clone());
                }
                left.extend(ambiguous);
                left.push(ProdFactor::new(vec![Symbol::Empty]));
                *prod = fine;
                extra.push(left);
            } else if prod.iter().any(|p| *p.last().unwrap() == symbol) {
                // only right-recursive: nothing to change, but applies flags
                if self.get_flags(var) & ruleflag::CHILD_REPEAT == 0 {
                    self.set_flags(var, ruleflag::R_RECURSION);
                }
            }

        }
        self.prods = prods;
        self.prods.extend(extra);
        self.num_nt = self.prods.len();
    }

    // Does the following transformation for each var with left- or right-recursive factors
    // General form: A -> α A | A β | A γ A | δ
    pub fn remove_recursion(&mut self) {
        /// Maximum number of P/I factors that are distributed before creating a new nonterminal to hold them.
        /// They are never distributed in presence of a binary (L/R) because it would likely induce left factorization.
        const MAX_DISTRIB_LEN: Option<usize> = None; // always distributing makes for smaller tables
        const VERBOSE: bool = false;

        self.symbol_table.as_ref().map(|st| assert_eq!(st.get_num_nt(), self.num_nt, "number of nt in symbol table doesn't match num_nt"));
        if VERBOSE {
            println!("ORIGINAL:");
            print_production_rules(&self, false);
        }
        let mut var_new = self.get_next_available_var() as usize;
        let var_new0 = var_new;
        // we must take prods out because of the borrow checker and other &mut borrows we need later...
        let mut prods = take(&mut self.prods);
        for var in 0..var_new0 {
            let mut prod = prods.get_mut(var).unwrap();
            let var = var as VarId;
            let symbol = Symbol::NT(var);
            let var_name = symbol.to_str(self.get_symbol_table());
            let mut extra_prods = Vec::<ProdRule>::new();
            if prod.iter().any(|p| !p.is_empty() && (p.first().unwrap() == &symbol)) {
                if VERBOSE {
                    println!("processing: {}", format!("{var_name} -> {}",
                        prod_to_string(prod, self.get_symbol_table())));
                }

                let (mut indep, mut factors) : (Vec<_>, Vec<_>) = take(prod).into_iter()
                    .partition(|factor| factor.first().unwrap() != &symbol && factor.last().unwrap() != &symbol);
                factors.reverse();
                if indep.is_empty() {
                    self.log.add_error(format!("recursive rules must have at least one independent factor: {} -> {}",
                                       Symbol::NT(var).to_str(self.get_symbol_table()),
                                       prod_to_string(prod, self.get_symbol_table())));
                    prod.extend(factors);
                    prod.extend(indep);
                    continue;
                }

                // below, the variable indices are mostly related to the variables that will be created to transform
                // the current rule. E[0] is the current `var`, E[1], ..., E[n-1] the children.

                let mut var_i = 0;        // index of E[i] variable used in a rule (... | α[i] E[i] | ...)
                let mut rule_var_i = 0;   // index of E[i] variable defined as a rule (E[i] -> ...)
                let mut var_factors: Vec<Vec<FactorId>> = vec![vec![]]; // pr_rule[i] = factors present in E[i]
                let mut indep_factors = Vec::<FactorId>::new();
                let mut pr_info = Vec::<FactorInfo>::new();  // information on each factor: type, priority, ...
                let mut has_binary = false;

                for (i, f) in factors.iter().index::<FactorId>() {
                    let ty = FactorType::from(&symbol, f);
                    has_binary |= ty == FactorType::LeftAssoc || ty == FactorType::RightAssoc;
                    var_i = match ty {
                        FactorType::Independant => panic!("there can't be an independent factor in `factors`"),
                        FactorType::LeftAssoc => var_i + 1,
                        FactorType::Prefix
                        | FactorType::RightAssoc => if var_i > rule_var_i || var_factors[var_i].is_empty() { var_i } else { var_i + 1 },
                        FactorType::Suffix => var_i,
                    };
                    let fact = FactorInfo {
                        pred_priority: if ty == FactorType::Prefix { None } else { Some(i) },
                        ivar: var_i,
                        ty
                    };
                    pr_info.push(fact);
                    let top_maybe = match ty {
                        FactorType::Independant => panic!("there can't be an independent factor in `factors`"),
                        FactorType::LeftAssoc => Some(var_i - 1),    // uses factor in rules of < priority
                        FactorType::Prefix => None,                     // uses factor in independent rule only
                        FactorType::RightAssoc
                        | FactorType::Suffix => Some(var_i),         // uses factor in rules of <= priority
                    };
                    if let Some(top) = top_maybe {
                        rule_var_i = top;
                        var_factors.resize(1 + top, vec![]);
                        (0..=top).for_each(|v| var_factors[v].push(i));
                    } else {
                        indep_factors.push(i);
                    }
                    if VERBOSE {
                        println!("- [{i:2}] {:10}: {:10} => var_i = {var_i:2}, rule_var_i = {rule_var_i:2}, {{{}}}",
                                 f.to_str(self.get_symbol_table()),
                                 format!("{ty:?}"), // "debug ignores width" bug https://github.com/rust-lang/rust/issues/55584
                                 var_factors.iter().enumerate().map(|(i, vf)| format!("{i}: {}", vf.iter().join(","))).join("  "));
                    }
                };
                assert!(var_i <= rule_var_i + 1, "var_i = {var_i}, rule_var_i = {rule_var_i}");

                // (var, prime) for each rule except independent factors. CAUTION! Includes the independent NT if last op is left-assoc
                let need_indep = indep.len() + indep_factors.len() > 1 &&
                    (MAX_DISTRIB_LEN.map(|max| indep.len() + indep_factors.len() > max).unwrap_or(false) || has_binary || rule_var_i < var_i);
                let num_indep = if need_indep { 1 } else { 0 };
                let mut var_i_nt = Vec::<(VarId, VarId)>::with_capacity(var_i + 1);
                var_i_nt.push((var, var_new as VarId));
                var_i_nt.extend((0..var_i).map(|i| ((var_new + i*2 + 1) as VarId, (var_new + i*2 + 2) as VarId)));
                var_new += rule_var_i * 2 + 1 + num_indep;
                if VERBOSE { println!("adding {} variables (w/o independent factors), need_indep = {need_indep}, var_new: {} -> {var_new}",
                                      rule_var_i * 2 + 1 + num_indep, var_new - (rule_var_i * 2 + 1 + num_indep)); }
                let nt_indep_maybe = if need_indep { Some(var_new as VarId - 1) } else { None };
                if var_new > VarId::MAX as usize {
                    self.log.add_error(format!("too many nonterminals when expanding {var_name}: {var_new} > {}", VarId::MAX));
                    return;
                }

                // A -> αi A | A βj | A γk A | δl  becomes  A[p] -> A[indep] ( βj | γk E[pk] )*
                //                                      -> | A[p]  -> A[indep] Ab[p]
                //                                         | Ab[p] -> βj Ab[p] | γk A[var] Ab[p] | ε

                // prepares the operation factor parts, which will be assembled in each rule according to their priority
                // (the Ab[p] loop nonterminals will be added later since they're specific to each rule)
                let new_factors = factors.iter().zip(&pr_info).map(|(f, FactorInfo { ivar, ty, .. })| {
                    let mut new_f: ProdFactor;
                    match ty {
                        FactorType::LeftAssoc | FactorType::RightAssoc => {
                            new_f = ProdFactor::new(f.v[1..f.len() - 1].to_owned());
                            if need_indep || *ivar <= rule_var_i {
                                new_f.v.push(Symbol::NT(var_i_nt[*ivar].0));
                            } else {
                                new_f.v.extend(&indep[0].v);
                            }
                            if ty == &FactorType::RightAssoc {
                                new_f.flags = ruleflag::R_ASSOC;
                            }
                        }
                        FactorType::Suffix => {
                            new_f = ProdFactor::new(f.v[1..f.len()].to_owned());
                        }
                        FactorType::Prefix => {
                            new_f = ProdFactor::new(f.v[..f.len() - 1].to_owned());
                            new_f.v.push(Symbol::NT(var_i_nt[*ivar].0));
                        }
                        FactorType::Independant => panic!("there can't be an independent factor in `factors`"),
                    }
                    new_f
                }).to_vec();
                let mut used_sym = HashSet::<Symbol>::new();
                let mut prod_indep = new_factors.iter()
                    .zip(&pr_info)
                    .filter_map(|(nf, FactorInfo { ty, .. })| if ty == &FactorType::Prefix { Some(nf.clone()) } else { None })
                    .to_vec();
                // when the lowest priority factor is P or R, all the factors of the first variable rule will sprout an ambiguity:
                let greedy_prologue = pr_info[0].ty == FactorType::Prefix && need_indep || pr_info[0].ty == FactorType::RightAssoc;
                for (i, fs) in var_factors.into_iter().enumerate() {
                    let (nt, nt_loop) = var_i_nt[i];
                    let mut prod_nt = if let Some(nt_indep) = nt_indep_maybe {
                        prod!(nt nt_indep, nt nt_loop)
                    } else {
                        // distributes the independent factors (only works if there are no L/R types in the original rule)
                        prod_indep.iter().cloned()
                            .chain(indep.iter().map(|f| {
                            let mut new_f = f.clone();
                            new_f.push(sym!(nt nt_loop));
                            new_f
                        })).collect()
                    };
                    let mut new_used_sym = Vec::<Symbol>::new();
                    let mut prod_nt_loop = fs.iter().enumerate().rev().map(|(j, &f_id)| {
                        let mut f = new_factors[f_id as usize].clone();
                        f.v.push(Symbol::NT(nt_loop));
                        let sym = f.first().unwrap();
                        let is_used_sym = used_sym.contains(sym);
                        if !is_used_sym {
                            new_used_sym.push(*sym);
                        }
                        if is_used_sym || (i == 0 && greedy_prologue) {
                            f.flags |= ruleflag::GREEDY;
                        }
                        if !has_binary && pr_info[f_id as usize].ty == FactorType::Suffix {
                            self.set_flags(nt, ruleflag::PARENT_L_RECURSION);
                            self.set_flags(nt_loop, ruleflag::CHILD_L_RECURSION);
                        }
                        f
                    }).to_vec();
                    used_sym.extend(new_used_sym);
                    prod_nt_loop.push(prodf!(e));
                    if i == 0 {
                        *prod = prod_nt;
                        extra_prods.push(prod_nt_loop);
                        self.symbol_table.as_mut().map(|t| {
                            // t.add_non_terminal(format!("{var_name}_{}", if rule_var_i + num_indep > 0 { "b" } else { "1" }));
                            assert_eq!(t.add_child_nonterminal(var), var_i_nt[0].1);
                        });
                    } else {
                        extra_prods.extend([prod_nt, prod_nt_loop]);
                        self.symbol_table.as_mut().map(|t| {
                            // t.add_non_terminal(format!("{var_name}_{i}"));
                            // t.add_non_terminal(format!("{var_name}_{i}b"));
                            assert_eq!(t.add_child_nonterminal(var), var_i_nt[i].0);
                            assert_eq!(t.add_child_nonterminal(var), var_i_nt[i].1);
                        });
                    }
                }
                if need_indep {
                    // self.symbol_table.as_mut().map(|t| t.add_non_terminal(format!("{var_name}_{}", rule_var_i + 1)));
                    self.symbol_table.as_mut().map(|t| {
                        // t.add_non_terminal(format!("{var_name}_{}", rule_var_i + 1))
                        assert_eq!(t.add_child_nonterminal(var), nt_indep_maybe.unwrap());
                    });
                }
                if VERBOSE {
                    println!("new factors: {}", new_factors.iter().enumerate()
                        .filter_map(|(i, nf)| if nf.is_empty() { None } else { Some(format!("[{i}] {}", nf.to_str(self.get_symbol_table()))) }).join(", "));
                    println!("vars: {}{}", var_i_nt.iter().take(rule_var_i + 1).enumerate().map(|(i, (v1, v2))|
                        format!("[{i}]:{},{}", Symbol::NT(*v1).to_str(self.get_symbol_table()),
                                Symbol::NT(*v2).to_str(self.get_symbol_table()))).join("  "),
                                if need_indep { format!("  [{}]:{}", var_i, Symbol::NT(var_i_nt[var_i].0).to_str(self.get_symbol_table() )) } else { String::new() });
                }
                if has_binary {
                    self.set_flags(var, ruleflag::PARENT_AMBIGUITY);
                } else if !prod_indep.is_empty() && !need_indep {
                    self.set_flags(var, ruleflag::R_RECURSION);
                }
                for (nt, nt_prime) in var_i_nt.into_iter().take(rule_var_i + 1) {
                    if nt != var {
                        self.set_parent(nt, var);
                    }
                    self.set_parent(nt_prime, nt);
                }
                if let Some(nt_indep) = nt_indep_maybe {
                    if !has_binary && prod_indep.iter().any(|p| p.last() == Some(&Symbol::NT(nt_indep))) {
                        self.set_flags(nt_indep, ruleflag::R_RECURSION);
                    }
                    prod_indep.extend(indep);
                    self.set_parent(nt_indep, var);
                    extra_prods.push(prod_indep);
                }
                self.parent.resize(var_new, None);
                self.flags.resize(var_new, 0);
            } else if prod.iter().any(|p| !p.is_empty() && p.last().unwrap() == &symbol) {
                if self.get_flags(var) & ruleflag::CHILD_REPEAT == 0 {
                    self.set_flags(var, ruleflag::R_RECURSION);
                }
            }
            prods.extend(extra_prods);
        }
        if VERBOSE {
            println!("#prods: {}, #flags: {}, #parents:{}", prods.len(), self.flags.len(), self.parent.len());
            if let Some(ref mut table) = self.symbol_table {
                println!("table: {} ({})", table.get_non_terminals().join(", "), table.get_num_nt());
            }
        }
        self.prods = prods;
        self.num_nt = self.prods.len();
        if VERBOSE {
            println!("FINAL:");
            print_production_rules(&self, false);
            println!("FLAGS:\n{}", self.flags.iter().index::<VarId>()
                .map(|(v, f)| format!("- ({v:2}) {}: {}", Symbol::NT(v).to_str(self.get_symbol_table()), ruleflag::to_string(*f).join(", "))).join("\n"));
            if let Some(table) = &self.symbol_table {
                println!("Symbol table:\n-NT: {}\n-T: {}",
                         table.get_non_terminals().iter().enumerate().map(|(i, nt)| format!("{i}:{nt}")).join(", "),
                         table.get_terminals().iter().enumerate().map(|(i, (t, txt))| format!("{i}:{t}{}", if let Some(st) = txt { format!(" ({st})") } else { String::new() })).join(", "))
            }
        }
    }

    /// Factorizes all the left symbols that are common to several factors by rejecting the non-common part
    /// to a new non-terminal. Updates the symbol table if provided.
    ///
    /// After the factorization, every child has an NT index greater than its parent, even if the
    /// factorization is performed several times on the same rule.
    ///
    /// Algorithm:
    /// - for each NT
    ///     - sort factors
    ///     - repeat as long as there are common starting symbols:
    ///         - take first group of >1 factors starting with the same symbol `α[0]`
    ///         - extract the number of starting symbols common to all factors of the group (1 or more): `α`
    ///         - create a new NT with the group, where `α` has been removed at the beginning
    pub fn left_factorize(&mut self) {
        fn similarity(a: &ProdFactor, b: &ProdFactor) -> usize {
            a.iter().zip(b.iter()).take_while(|(a, b)| a == b).count()
        }

        const VERBOSE: bool = false;
        let mut new_var = self.get_next_available_var();
        // we must take prods out because of the borrow checker and other &mut borrows we need later...
        let mut prods = take(&mut self.prods);
        let mut i = 0;
        while i < prods.len() {
            let mut prod = &mut prods[i];
            let var = i as VarId;
            i += 1;
            if prod.len() < 2 {
                continue
            }
            let mut factors = prod.clone();
            let mut extra = Vec::<ProdRule>::new();
            let mut changed = false;
            factors.sort();
            if VERBOSE { println!("{var}: {} -> {}", Symbol::NT(var).to_str(self.get_symbol_table()), factors.iter().map(|f| f.to_str(self.get_symbol_table())).join(" | ")); }
            while factors.len() > 1 {
                let simi = factors.windows(2).enumerate()
                    .map(|(j, x)| (j, similarity(&x[0], &x[1])))
                    .skip_while(|(_, s)| *s == 0)
                    .take_while(|(_, s)| *s != 0)
                    .to_vec();
                if simi.is_empty() {
                    if VERBOSE { println!(" nothing to factorize"); }
                    break;
                }
                changed = true;
                let min = simi.iter().map(|(_, s)| *s).min().unwrap();
                let start = simi[0].0;
                let stop = start + simi.len();
                let mut factorized = ProdFactor::new(factors[start].v.iter().take(min).cloned().to_vec());
                let mut child = factors.drain(start..=stop).to_vec();
                if child.iter().all(|f| f.is_greedy()) {
                    factorized.flags |= ruleflag::GREEDY;
                }
                for f in &mut child {
                    f.v.drain(0..min);
                }
                if child[0].v.is_empty() {
                    let empty = child.remove(0);
                    child.push(ProdFactor::with_flags(vec![Symbol::Empty], empty.flags));
                }
                let var_prime = new_var;
                new_var += 1;
                self.set_flags(var, ruleflag::PARENT_L_FACTOR);
                self.set_flags(var_prime, ruleflag::CHILD_L_FACTOR);
                if let Some(table) = &mut self.symbol_table {
                    // table.add_var_prime_name(var, var_prime, None);
                    assert_eq!(table.add_child_nonterminal(var), var_prime);
                }
                self.set_parent(var_prime, var);
                let symbol_prime = Symbol::NT(var_prime);
                factorized.v.push(symbol_prime);
                factors.insert(start, factorized);
                if VERBOSE {
                    println!(" - similarity: {} => {}", simi.iter().map(|(j, s)| format!("{j}:{s}")).join(", "), min);
                    println!("   factorize: {}", child.iter().map(|f| f.to_str(self.get_symbol_table())).join(" | "));
                    println!("   left:      {}", factors.iter().map(|f| f.to_str(self.get_symbol_table())).join(" | "));
                }
                extra.push(child);
            }
            if changed {
                *prod = factors;
                prods.extend(extra);
            }
        }
        self.prods = prods;
        self.num_nt = self.prods.len();
    }

    pub(crate) fn remove_ambiguity(&self) {
        todo!()
    }

    /// Moves the flags of the mask from the factors to the NT flags
    fn transfer_factor_flags(&mut self) {
        // add other flags here if necessary:
        const FLAG_MASK: u32 = ruleflag::L_FORM;

        for (v, prod) in self.prods.iter_mut().enumerate() {
            let flags = prod.iter().fold(0, |acc, f| acc | f.flags) & FLAG_MASK;
            self.flags[v] |= flags;
            for f in prod.iter_mut() {
                f.flags &= !FLAG_MASK;
            }
        }
    }

    fn check_flags(&mut self) {
        const FLAG_CHECK_MASK: u32 = ruleflag::L_FORM | ruleflag::CHILD_REPEAT | ruleflag::R_RECURSION;
        for v in 0..self.num_nt {
            if self.flags[v] & FLAG_CHECK_MASK == ruleflag::L_FORM {
                // it's also fine to have L-form on a l-factor children of a right-recursive parent
                if self.flags[v] & ruleflag::CHILD_L_FACTOR == 0 || self.flags[self.parent[v].unwrap() as usize] & ruleflag::R_RECURSION == 0 {
                    self.log.add_error(format!("{} has an illegal flag L-Form (only used with +, *, or right recursion): {}",
                                               Symbol::NT(v as VarId).to_str(self.get_symbol_table()),
                                               ruleflag::to_string(self.flags[v]).join(" ")
                    ));
                }
            }
        }
    }
}

impl ProdRuleSet<LL1> {
    /// Creates the table for predictive top-down parsing.
    ///
    /// Returns:
    /// - `num_nt` = number of non-terminals
    /// - `num_t` = number of terminals (including the end symbol)
    /// - `factors`, the production factors: (VarId, ProdFactor) where the first value is the non-terminal index and the second one of its factors
    /// - the table of `num_nt * num_t` values, where `table[nt_index * num_nt + t_index]` gives the index of the production factor for
    /// the non-terminal index `nt_index` and the terminal index `t_index`. A value >= `factors.len()` stands for a syntactic error.
    fn calc_table(&mut self, first: &HashMap<Symbol, HashSet<Symbol>>, follow: &HashMap<Symbol, HashSet<Symbol>>, error_recovery: bool) -> LLParsingTable {
        fn add_table(table: &mut Vec<Vec<FactorId>>, num_t: usize, nt_id: VarId, t_id: VarId, f_id: FactorId) {
            let pos = nt_id as usize * num_t + t_id as usize;
            table[pos].push(f_id);
        }
        const VERBOSE: bool = false;
        const DISABLE_FILTER: bool = false;
        let factors = self.prods.iter().index().filter(|(v, _)| DISABLE_FILTER || first.contains_key(&Symbol::NT(*v)))
            .flat_map(|(v, x)| x.iter().map(move |f| (v, f.clone()))).to_vec();
        let error_skip = factors.len() as FactorId; // table entry for syntactic error; recovery by skipping input symbol
        let error_pop = error_skip + 1;             // table entry for syntactic error; recovery by popping T or NT from stack
        let num_nt = self.num_nt;
        let num_t = self.num_t + 1;
        let end = (num_t - 1) as VarId; // index of end symbol
        let mut used_t = HashSet::<Symbol>::new();
        let mut table: Vec<Vec<FactorId>> = vec![vec![]; num_nt * num_t];
        for (f_id, (nt_id, factor)) in factors.iter().index() {
            used_t.extend(factor.iter().filter(|s| s.is_t()));
            if VERBOSE { println!("- {f_id}: {} -> {}  => {}", Symbol::NT(*nt_id).to_str(self.get_symbol_table()),
                                  factor.to_str(self.get_symbol_table()),
                                  factor.factor_first(first).iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut has_end = false;
            let mut has_empty = false;
            for s in factor.factor_first(first) {
                match s {
                    Symbol::Empty => {
                        has_empty = true;
                        for s in &follow[&Symbol::NT(*nt_id)] {
                            match s {
                                Symbol::T(t_id) => add_table(&mut table, num_t, *nt_id, *t_id, f_id),
                                Symbol::End     => add_table(&mut table, num_t, *nt_id, end, f_id),
                                _ => {}
                            }
                        }
                    }
                    Symbol::T(t_id) => {
                        add_table(&mut table, num_t, *nt_id, t_id, f_id);
                    }
                    Symbol::NT(_) => {}
                    Symbol::End => {
                        has_end = true;
                    }
                }
            }
            if has_empty && has_end {
                add_table(&mut table, num_t, *nt_id, end, end);
            }
        }
        // creates the table and removes ambiguities
        let mut final_table = Vec::<FactorId>::new();
        for nt_id in 0..num_nt {
            for t_id in 0..num_t {
                let pos = nt_id * num_t + t_id;
                final_table.push(match table[pos].len() {
                    0 => {
                        if error_recovery {
                            let sym_t = if t_id < num_t - 1 { Symbol::T(t_id as TokenId) } else { Symbol::End };
                            let sym_nt = Symbol::NT(nt_id as VarId);
                            if follow[&sym_nt].contains(&sym_t) || first[&sym_nt].contains(&sym_t) {
                                error_pop
                            } else {
                                error_skip
                            }
                        } else {
                            error_skip
                        }
                    },
                    1 => *table[pos].first().unwrap(),
                    _ => {
                        // we take the first item which isn't already in another position on the same NT row
                        let greedies = table[pos].iter().filter(|&f_id| factors[*f_id as usize].1.is_greedy()).cloned().to_vec();
                        if greedies.len() == 1 {
                            let chosen = greedies[0];
                            self.log.add_note(
                                format!("calc_table: expected ambiguity for NT '{}', T '{}': {} => <{}> is specified as greedy and has been chosen",
                                        Symbol::NT(nt_id as VarId).to_str(self.get_symbol_table()),
                                        if t_id < self.num_t { Symbol::T(t_id as VarId).to_str(self.get_symbol_table()) } else { "<EOF>".to_string() },
                                        table[pos].iter().map(|f_id|
                                            format!("<{}>", factors[*f_id as usize].1.to_str(self.get_symbol_table()))).join(" or "),
                                        factors[chosen as usize].1.to_str(self.get_symbol_table())
                                ));
                            table[pos] = greedies;
                            chosen
                        } else {
                            let row = (0..num_t).filter(|j| *j != t_id).flat_map(|j| &table[nt_id * num_t + j]).collect::<HashSet<_>>();
                            let chosen = *table[pos].iter().find(|f| !row.contains(f)).unwrap_or(&table[pos][0]);
                            self.log.add_warning(
                                format!("calc_table: ambiguity for NT '{}', T '{}': {} => <{}> has been chosen",
                                        Symbol::NT(nt_id as VarId).to_str(self.get_symbol_table()),
                                        if t_id < self.num_t { Symbol::T(t_id as VarId).to_str(self.get_symbol_table()) } else { "<EOF>".to_string() },
                                        table[pos].iter().map(|f_id|
                                            format!("<{}>", factors[*f_id as usize].1.to_str(self.get_symbol_table()))).join(" or "),
                                        factors[chosen as usize].1.to_str(self.get_symbol_table())
                                ));
                            table[pos] = vec![chosen];
                            chosen
                        }
                    }
                });
            }
        }
        if !(0..num_t - 1).any(|t_id| (0..num_nt).any(|nt_id| final_table[nt_id * num_t + t_id] < error_skip)) {
            self.log.add_error("calc_table: no terminal used in the table".to_string());
        }
        LLParsingTable { num_nt, num_t, factors, table: final_table, flags: self.flags.clone(), parent: self.parent.clone() }
    }

    pub fn create_parsing_table(&mut self, error_recovery: bool) -> LLParsingTable {
        let first = self.calc_first();
        let follow = self.calc_follow(&first);
        self.calc_table(&first, &follow, error_recovery)
    }
}

impl From<RuleTreeSet<Normalized>> for ProdRuleSet<General> {
    fn from(mut rules: RuleTreeSet<Normalized>) -> Self {
        fn children_to_vec(tree: &GrTree, parent_id: usize) -> ProdFactor {
            let mut flags: u32 = 0;
            let factor = tree.children(parent_id).iter()
                .map(|id| tree.get(*id))
                .filter(|node| {
                    match **node {
                        GrNode::RAssoc => {
                            flags |= ruleflag::R_ASSOC;
                            false
                        }
                        GrNode::LForm(_) => {
                            flags |= ruleflag::L_FORM;
                            false
                        }
                        _ => true,
                    }
                })
                .map(|node| {
                match node {
                    GrNode::Symbol(s) => s.clone(),
                    x => panic!("unexpected symbol {x} under &")
                }
            }).to_vec();
            ProdFactor::with_flags(factor, flags)
        }
        let mut prules = Self::with_capacity(rules.trees.len());
        prules.start = rules.start;
        prules.symbol_table = rules.symbol_table;
        prules.flags = rules.flags;
        prules.parent = rules.parent;
        prules.nt_conversion = rules.nt_conversion;
        prules.log = rules.log;
        if !prules.log.has_no_errors() {
            // We handle the errors by transmitting the log to the next construct rather than returning a `Result` type.
            // This allows to cascade the transforms without getting a complicated error resolving system while preserving
            // the information about the errors easily.
            return prules;
        }
        for (var, tree) in rules.trees.iter().index() {
            if !tree.is_empty() {
                let root = tree.get_root().expect("tree {var} has no root");
                let root_sym = tree.get(root);
                let mut prod = match root_sym {
                    GrNode::Symbol(s) => {
                        vec![ProdFactor::new(vec![s.clone()])]
                    },
                    GrNode::Concat => {
                        vec![children_to_vec(tree, root)]
                    },
                    GrNode::Or => tree.children(root).iter()
                        .map(|id| {
                            let child = tree.get(*id);
                            if let GrNode::Symbol(s) = child {
                                ProdFactor::new(vec![s.clone()])
                            } else {
                                assert_eq!(*child, GrNode::Concat, "unexpected symbol {child} under |");
                                children_to_vec(tree, *id)
                            }
                        }).to_vec(),
                    s => panic!("unexpected symbol {s} as root of normalized GrTree for NT {}", Symbol::NT(var).to_str(prules.get_symbol_table()))
                };
                if prod.iter().any(|f| f.flags & ruleflag::L_FORM != 0) {
                    // We keep the L flag on the child of +* normalization if it's intended only for that normalization.
                    // For example:
                    // - A -> (b <L>)+ A | c
                    //   - doesn't have an l-form right recursion
                    //   - has an l-form repetition of b
                    // - A -> <L> (b)+ A | c
                    //   - has an l-form right recursion
                    //   - doesn't have an l-form repetition of b
                    //
                    // while let Some(parent) = prules.get_parent(nt) {
                    //     nt = parent;
                    // }
                    prules.set_flags(var, ruleflag::L_FORM);
                    // not really necessary, but cleaner:
                    for f in prod.iter_mut() {
                        f.flags &= !ruleflag::L_FORM;
                    }
                }
                prules.prods.push(prod);
            } else {
                prules.prods.push(ProdRule::new()); // empty
            }
        }
        prules.calc_num_symbols();
        prules
    }
}

impl From<RuleTreeSet<General>> for ProdRuleSet<General> {
    fn from(rules: RuleTreeSet<General>) -> Self {
        let mut prods = ProdRuleSet::from(RuleTreeSet::<Normalized>::from(rules));
        if prods.log.has_no_errors() {
            prods.simplify();
        }
        prods
    }
}

impl From<ProdRuleSet<General>> for ProdRuleSet<LL1> {
    fn from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.remove_left_recursion();
            rules.left_factorize();
            rules.transfer_factor_flags();
            rules.check_flags();
        }
        ProdRuleSet::<LL1> {
            prods: rules.prods,
            num_nt: rules.num_nt,
            num_t: rules.num_t,
            symbol_table: rules.symbol_table,
            flags: rules.flags,
            parent: rules.parent,
            start: rules.start,
            nt_conversion: rules.nt_conversion,
            log: rules.log,
            dont_remove_recursion: false,
            _phantom: PhantomData,
        }
    }
}

impl From<ProdRuleSet<General>> for ProdRuleSet<LR> {
    fn from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.remove_ambiguity();
            rules.transfer_factor_flags();
            rules.check_flags();
        }
        ProdRuleSet::<LR> {
            prods: rules.prods,
            num_nt: rules.num_nt,
            num_t: rules.num_t,
            symbol_table: rules.symbol_table,
            flags: rules.flags,
            parent: rules.parent,
            start: rules.start,
            nt_conversion: rules.nt_conversion,
            log: rules.log,
            dont_remove_recursion: false,
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

pub fn symbol_to_macro(s: &Symbol) -> String {
    match s {
        Symbol::Empty => "e".to_string(),
        Symbol::T(x) => format!("t {x}"),
        Symbol::NT(x) => format!("nt {x}"),
        Symbol::End => "end".to_string(),
    }
}

pub fn print_production_rules<T>(prods: &ProdRuleSet<T>, as_comment: bool) {
    let prefix = if as_comment { "            // " } else { "    " };
    println!("{prefix}{}", prods.get_prods_iter().map(|(var, p)|
        format!("({var}) {} -> {}",
                Symbol::NT(var).to_str(prods.get_symbol_table()),
                prod_to_string(p, prods.get_symbol_table()))
    ).join(&format!("\n{prefix}")));
}

pub fn print_factors<T>(rules: &ProdRuleSet<T>, factors: &Vec<(VarId, ProdFactor)>) {
    println!("factors:\n{}",
             factors.iter().enumerate().map(|(id, (v, f))|
                 format!("            // - {id}: {} -> {}{}",
                         Symbol::NT(*v).to_str(rules.get_symbol_table()),
                         f.iter().map(|s| s.to_str(rules.get_symbol_table())).join(" "),
                         if f.flags != 0 { format!("     {} ({})", ruleflag::to_string(f.flags).join(" | "), f.flags) } else { "".to_string() }
                 )
    ).join("\n"));
}

pub fn print_prs_factors<T>(rules: &ProdRuleSet<T>) {
    println!("Factors:\n{}",
             rules.get_factors().enumerate().map(|(id, (v, f))|
                 format!("    // - {id}: {} -> {}{}",
                         Symbol::NT(v).to_str(rules.get_symbol_table()),
                         f.iter().map(|s| s.to_str(rules.get_symbol_table())).join(" "),
                         if f.flags != 0 { format!("     {} ({})", ruleflag::to_string(f.flags).join(" | "), f.flags) } else { "".to_string() }
                 )
    ).join("\n"));
}

pub fn print_ll1_table(symbol_table: Option<&SymbolTable>, parsing_table: &LLParsingTable, indent: usize) {
    let LLParsingTable { num_nt, num_t, factors, table, flags, parent } = parsing_table;
    let error_skip = factors.len() as FactorId;
    let error_pop = error_skip + 1;
    let str_nt = (0..*num_nt).map(|i| Symbol::NT(i as VarId).to_str(symbol_table)).to_vec();
    let max_nt_len = str_nt.iter().map(|s| s.len()).max().unwrap();
    let str_t = (0..*num_t).map(|j| if j + 1 < *num_t { Symbol::T(j as VarId).to_str(symbol_table) } else { "$".to_string() }).to_vec();
    let max_t_len = str_t.iter().map(|s| s.len()).max().unwrap().max(2);
    let t_len = str_t.iter().map(|s| s.len().max(3)).to_vec();
    println!("{:<i$}// {:<w$} | {}", "", "", (0..*num_t).map(|j| format!("{:^w$}", str_t[j], w = t_len[j])).join(" "), w = max_nt_len, i = indent);
    println!("{:<i$}// {:-<w$}-+-{:-<t$}", "", "", "", w = max_nt_len, t = *num_t + t_len.iter().sum::<usize>(), i = indent);
    for i in 0..*num_nt {
        print!("{:<i$}// {:<w$} |", "", str_nt[i], w = max_nt_len, i = indent);
        for j in 0..*num_t {
            let value = table[i * num_t + j];
            if value < error_skip {
                print!(" {:^w$}", value, w = t_len[j]);
            } else {
                print!(" {:^w$}", if value == error_pop { "p" } else { "." } , w = t_len[j]);
            }
        }
        println!();
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates a `GrNode` instance.
    ///
    /// # Examples
    /// ```
    /// # use lexigram::dfa::TokenId;
    /// # use lexigram::gnode;
    /// # use lexigram::grammar::{GrNode, Symbol, VarId};
    /// assert_eq!(gnode!([1]), GrNode::Symbol(Symbol::T(1 as TokenId)));
    /// assert_eq!(gnode!(t 2), GrNode::Symbol(Symbol::T(2 as TokenId)));
    /// assert_eq!(gnode!(nt 3), GrNode::Symbol(Symbol::NT(3 as VarId)));
    /// assert_eq!(gnode!(e), GrNode::Symbol(Symbol::Empty));
    /// assert_eq!(gnode!(end), GrNode::Symbol(Symbol::End));
    /// assert_eq!(gnode!(&), GrNode::Concat);
    /// assert_eq!(gnode!(|), GrNode::Or);
    /// assert_eq!(gnode!(?), GrNode::Maybe);
    /// assert_eq!(gnode!(+), GrNode::Plus);
    /// assert_eq!(gnode!(*), GrNode::Star);
    /// assert_eq!(gnode!(L 3), GrNode::LForm(3));
    /// assert_eq!(gnode!(R), GrNode::RAssoc);
    /// ```
    #[macro_export()]
    macro_rules! gnode {
        ([$id:expr]) => { gnode!(t $id) };
        (t $id:expr) => { $crate::grammar::GrNode::Symbol($crate::grammar::Symbol::T($id as $crate::dfa::TokenId)) };
        (nt $id:expr) => { $crate::grammar::GrNode::Symbol($crate::grammar::Symbol::NT($id as $crate::grammar::VarId)) };
        (e) => { $crate::grammar::GrNode::Symbol($crate::grammar::Symbol::Empty) };
        (end) => { $crate::grammar::GrNode::Symbol($crate::grammar::Symbol::End) };
        //
        (&) => { $crate::grammar::GrNode::Concat };
        (|) => { $crate::grammar::GrNode::Or };
        (?) => { $crate::grammar::GrNode::Maybe };
        (+) => { $crate::grammar::GrNode::Plus };
        (*) => { $crate::grammar::GrNode::Star };
        (L $id:expr) => { $crate::grammar::GrNode::LForm($id) };
        (R) => { $crate::grammar::GrNode::RAssoc };
    }

    /// Generates a `Symbol` instance.
    ///
    /// # Examples
    /// ```
    /// # use lexigram::dfa::TokenId;
    /// # use lexigram::sym;
    /// # use lexigram::grammar::{Symbol, VarId};
    /// assert_eq!(sym!(t 2), Symbol::T(2 as TokenId));
    /// assert_eq!(sym!(nt 3), Symbol::NT(3 as VarId));
    /// assert_eq!(sym!(e), Symbol::Empty);
    /// assert_eq!(sym!(end), Symbol::End);
    #[macro_export()]
    macro_rules! sym {
        (t $id:expr) => { $crate::grammar::Symbol::T($id as $crate::dfa::TokenId) };
        (nt $id:expr) => { $crate::grammar::Symbol::NT($id as $crate::grammar::VarId) };
        (e) => { $crate::grammar::Symbol::Empty };
        (end) => { $crate::grammar::Symbol::End };
    }

    #[macro_export()]
    macro_rules! prodflag {
        (L) => { $crate::grammar::ruleflag::L_FORM };
        (R) => { $crate::grammar::ruleflag::R_ASSOC };
        (G) => { $crate::grammar::ruleflag::GREEDY };
        ($f:expr) => { $f };
    }

    /// Generates a production rule factor. A factor is made up of symbols separated by a comma.
    /// Each symbol is either
    /// - a non-terminal: `nt` {integer}
    /// - a terminal: `t` {integer}
    /// - the empty symbol: `e`
    ///
    /// Preceding a factor with `# {integer}` sets a flag value on that factor. The values are:
    /// - 128: L-form (low-latency parsing of that factor)
    /// - 256: R-assoc (right-associative - by default, ambiguous factors like 'E * E' are left-associative)
    ///
    /// # Example
    /// ```
    /// # use lexigram::dfa::TokenId;
    /// # use lexigram::grammar::{ProdFactor, Symbol, VarId};
    /// # use lexigram::{prodf, sym};
    /// assert_eq!(prodf!(nt 1, t 2, e), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    /// assert_eq!(prodf!(#128, nt 1, t 2, e), ProdFactor::with_flags(vec![sym!(nt 1), sym!(t 2), sym!(e)], 128));
    /// assert_eq!(prodf!(#L, nt 1, t 2, e), ProdFactor::with_flags(vec![sym!(nt 1), sym!(t 2), sym!(e)], 128));
    /// ```
    #[macro_export()]
    macro_rules! prodf {
        () => { $crate::grammar::ProdFactor::new(std::vec![]) };
        ($($a:ident $($b:expr)?,)+) => { prodf!($($a $($b)?),+) };
        ($($a:ident $($b:expr)?),*) => { $crate::grammar::ProdFactor::new(std::vec![$($crate::sym!($a $($b)?)),*]) };
        (#$f:literal, $($a:ident $($b:expr)?,)+) => { prodf!(#$f, $($a $($b)?),+) };
        (#$f:literal, $($a:ident $($b:expr)?),*) => { $crate::grammar::ProdFactor::with_flags(std::vec![$($crate::sym!($a $($b)?)),*], $f) };
        (#$f:ident, $($a:ident $($b:expr)?,)+) => { prodf!(#$f, $($a $($b)?),+) };
        (#$f:ident, $($a:ident $($b:expr)?),*) => { $crate::grammar::ProdFactor::with_flags(std::vec![$($crate::sym!($a $($b)?)),*], $crate::prodflag!($f)) };
    }

    #[macro_export()]
    macro_rules! symbols {
        () => { std::vec![] };
        ($($a:ident $($b:literal $(: $num:expr)?)?,)+) => { symbols![$($a $($b $(: $num)?)?),+] };
        ($($a:ident $($b:literal $(: $num:expr)?)?),*) => { std::vec![$($crate::sym!($a $($b $(: $num)?)?)),*] };
    }

    /// Generates a production rule. It is made up of factors separated by a semicolon.
    ///
    /// Example
    /// ```
    /// # use lexigram::dfa::TokenId;
    /// # use lexigram::grammar::{ProdFactor, Symbol, VarId};
    /// # use lexigram::{prod, prodf, sym};
    /// assert_eq!(prod!(nt 1, t 2, nt 1, t 3; nt 2; e),
    ///            vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 ProdFactor::new(vec![sym!(nt  2)]),
    ///                 ProdFactor::new(vec![sym!(e)])]);
    /// assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #128, nt 2; e),
    ///            vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 ProdFactor::with_flags(vec![sym!(nt  2)], 128),
    ///                 ProdFactor::new(vec![sym!(e)])]);
    /// assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #L, nt 2; e),
    ///            vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 ProdFactor::with_flags(vec![sym!(nt  2)], 128),
    ///                 ProdFactor::new(vec![sym!(e)])]);
    /// ```
    #[macro_export()]
    macro_rules! prod {
        () => { std::vec![] };
        ($($(#$f:literal,)? $($a:ident $($b:expr)?),*;)+) => { prod![$($(#$f,)? $($a $($b)?),+);+] };
        ($($(#$f:literal,)? $($a:ident $($b:expr)?),*);*) => { std::vec![$($crate::prodf![$(#$f,)? $($a $($b)?),+]),*]};
        ($($(#$f:ident,)? $($a:ident $($b:expr)?),*;)+) => { prod![$($(#$f,)? $($a $($b)?),+);+] };
        ($($(#$f:ident,)? $($a:ident $($b:expr)?),*);*) => { std::vec![$($crate::prodf![$(#$f,)? $($a $($b)?),+]),*]};
    }
}
