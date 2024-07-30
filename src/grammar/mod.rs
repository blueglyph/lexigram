#![allow(dead_code)]
#![allow(unused)]

pub(crate) mod tests;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use crate::cproduct::CProduct;
use crate::dfa::TokenId;
use crate::{CollectJoin, General, Normalized, gnode, vaddi, prodf, hashset, LL1, LR};
use crate::log::Logger;
use crate::vectree::VecTree;
use crate::symbol_table::SymbolTable;
use crate::take_until::TakeUntilIterator;

pub type VarId = u16;

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
    LForm,  // applied to NT
    RAssoc  // applied to factor
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
            GrNode::LForm => write!(f, "<L>"),
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
            _ => self.to_string()
        }
    }
}

// ---------------------------------------------------------------------------------------------

/// Simple index object that returns `Original(<value>)` on the first `index.get()`, then
/// `Copy(<value>)` on subsequent calls. The indices are stored on 31 bits, keeping one bit
/// for the 'original' flag. Trying to store larger values triggers a panic.
#[derive(Clone, Copy)]
struct Dup {
    index: u32
}

#[derive(Clone, Copy, Debug)]
enum DupVal{
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

type GrTree = VecTree<GrNode>;

impl GrTree {
    fn get_dup(&mut self, dup_index: &mut Dup) -> usize {
        match dup_index.get() {
            DupVal::Original(index) => index as usize,
            DupVal::Copy(index) => {
                let node = self.get(index as usize).clone();
                self.add(None, node)
            }
        }
    }

    pub fn to_str(&self, start_node: Option<usize>, symbol_table: Option<&SymbolTable>) -> String {
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

impl Default for GrTree {
    fn default() -> Self {
        GrTree::new()
    }
}

impl Display for GrTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tree_fmt = GrTreeFmt {
            tree: &self,
            show_ids: f.alternate(),
            show_depth: f.sign_plus(),
            symbol_table: None,
            start_node: None,
        };
        tree_fmt.fmt(f)
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

    /// Star or Plus repeat factor.
    /// Set by `RuleTreeSet<General>::normalize_plus_or_star()` in `flags`.
    pub const CHILD_REPEAT: u32 = 1;
    /// Right-recursive NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const R_RECURSION: u32 = 2;
    /// Left-recursive NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_L_RECURSION: u32 = 4;
    /// Left-recursive, ambiguous NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_AMBIGUITY: u32 = 8;
    /// NT created to regroup the independent factors when transforming an ambiguous, recursive rule.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_INDEPENDENT_AMBIGUITY: u32 = 16;
    /// Left factorization.
    /// Set by `ProdRuleSet<T>::left_factorize()` in `flags`.
    pub const PARENT_L_FACTOR: u32 = 32;
    pub const CHILD_L_FACTOR: u32 = 64;
    /// Low-latency non-terminal
    /// Set by `ProdRuleSet<General>::from(rules: From<RuleTreeSet<Normalized>>` in `flags`.
    pub const L_FORM: u32 = 128;
    /// Right-associative factor.
    /// Set by `ProdRuleSet<General>::from(rules: From<RuleTreeSet<Normalized>>` in factors.
    pub const R_ASSOC: u32 = 256;

    pub fn to_string(flags: u32) -> Vec<String> {
        let names = btreemap![
            CHILD_REPEAT                => "child_+_or_*".to_string(),
            R_RECURSION                 => "right_rec".to_string(),
            CHILD_L_RECURSION           => "child_left_rec".to_string(),
            CHILD_AMBIGUITY             => "child_amb".to_string(),
            CHILD_INDEPENDENT_AMBIGUITY => "child_ind_amb".to_string(),
            PARENT_L_FACTOR             => "parent_left_fact".to_string(),
            CHILD_L_FACTOR              => "child_left_fact".to_string(),
            L_FORM                      => "L-form".to_string(),
            R_ASSOC                     => "R-assoc".to_string(),
        ];
        names.into_iter().filter_map(|(f, t)| if flags & f != 0 { Some(t) } else { None } ).collect::<_>()
    }
}

#[derive(Clone, Debug)]
pub struct RuleTreeSet<T> {
    trees: Vec<GrTree>,
    start: Option<VarId>,
    symbol_table: Option<SymbolTable>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    // priority: Vec<Vec<u16>>, // factor -> priority
    log: Logger,
    _phantom: PhantomData<T>
}

// Methods for both General and Normalized forms. There can only be immutable methods
// in the normalized form.
impl<T> RuleTreeSet<T> {
    pub fn get_tree(&self, var: VarId) -> Option<&GrTree> {
        self.trees.get(var as usize)
    }

    /// Returns all the non-empty trees
    pub fn get_trees_iter(&self) -> impl Iterator<Item=(VarId, &GrTree)> {
        self.trees.iter().enumerate().filter_map(|(id, t)| if t.is_empty() { None } else { Some((id as VarId, t)) })
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
        RuleTreeSet {
            trees: Vec::new(),
            start: None,
            symbol_table: None,
            flags: Vec::new(),
            parent: Vec::new(),
            log: Logger::new(),
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
        let orig = std::mem::take(&mut self.trees[var as usize]);
        let mut new = VecTree::<GrNode>::new();
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
                        let children = stack.drain(stack.len() - n..).to_vec();
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
                            self.symbol_table.as_mut().map(|st| st.add_var_prime_name(var, new_var));
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
                            self.symbol_table.as_mut().map(|st| st.add_var_prime_name(var, new_var));
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
        let mut qtree = VecTree::<GrNode>::new();
        let child = stack.pop().unwrap();
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
                let cc1 = qtree.add_from_tree(Some(or), new.iter_depth_at(child));
                qtree.add(Some(cc1), gnode!(nt *new_var));
                if is_plus {
                    qtree.add_from_tree(Some(or), new.iter_depth_at(child));
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
                            let cc = qtree.add_from_tree(Some(or), new.iter_depth_at(*id_child));
                            qtree.add(Some(cc), gnode!(nt *new_var));
                            if is_plus {
                                qtree.add_from_tree(Some(or), new.iter_depth_at(*id_child));
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
        let id = new.add(None, gnode!(nt *new_var));
        assert!(*new_var as usize >= self.trees.len() || self.trees[*new_var as usize].is_empty(), "overwriting tree {new_var}");
        self.set_tree(*new_var, qtree);
        self.flags.resize(*new_var as usize + 1, 0);
        self.parent.resize(*new_var as usize + 1, None);
        self.flags[*new_var as usize] = ruleflag::CHILD_REPEAT;
        self.parent[*new_var as usize] = Some(var);
        *new_var += 1;
        stack.push(id);
    }
}

impl From<RuleTreeSet<General>> for RuleTreeSet<Normalized> {
    /// Transforms a `General` ruleset to a `Normalized` ruleset
    fn from(mut rules: RuleTreeSet<General>) -> Self {
        rules.normalize();
        RuleTreeSet::<Normalized> {
            trees: rules.trees,
            start: rules.start,
            symbol_table: rules.symbol_table,
            flags: rules.flags,
            parent: rules.parent,
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
    flags: u32
}

impl ProdFactor {
    pub fn new(v: Vec<Symbol>) -> Self {
        ProdFactor { v, flags: 0 }
    }

    pub fn with_flags(v: Vec<Symbol>, flags: u32) -> Self {
        ProdFactor { v, flags }
    }

    pub fn symbols(self) -> Vec<Symbol> {
        self.v
    }

    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        self.v.iter()
            .map(|symbol| symbol_table.map(|t| t.get_name(symbol)).unwrap_or(symbol.to_string()))
            .join(" ")
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

pub fn prod_to_string(prod: &ProdRule, symbol_table: Option<&SymbolTable>) -> String {
    prod.iter().map(|factor| factor.to_str(symbol_table)).join(" | ")
}

#[derive(Debug)]
pub struct LLParsingTable {
    pub num_nt: usize,
    pub num_t: usize,
    pub factors: Vec<(VarId, ProdFactor)>,
    pub table: Vec<VarId>,
    pub flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    pub parent: Vec<Option<VarId>>, // NT -> parent NT
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
    nt_conversion: Option<HashMap<Symbol, Symbol>>,
    log: Logger,
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
        self.prods.iter().enumerate().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id as VarId, p)) })
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

    pub fn symbol_table(self) -> Option<SymbolTable> {
        self.symbol_table
    }

    pub fn get_num_nt(&self) -> usize {
        self.num_nt
    }

    pub fn get_num_t(&self) -> usize {
        self.num_t
    }

    pub fn get_log(&self) -> &Logger {
        &self.log
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
        assert!(self.nt_conversion.is_none(), "cleanup of symbols but there's already an NT conversion table");
        if VERBOSE {
            println!("Removing unused non-terminals:");
            let mut all_h = self.prods.iter().flat_map(|p| p.iter().map(|x| &x.v).flatten()).cloned().collect::<HashSet<_>>();
            all_h.extend((0..self.num_nt).map(|i| Symbol::NT(i as VarId)));
            let mut all = all_h.into_iter().collect::<Vec<_>>();
            all.sort();
            println!("- current NT symbols: {}", all.iter().filter_map(|s|
                if let Symbol::NT(v) = s { Some(format!("{}", s.to_str(self.get_symbol_table()))) } else { None }).join(", "));
            println!("- current  T symbols: {}", all.iter().filter_map(|s|
                if let Symbol::T(_) = s { Some(s.to_str(self.get_symbol_table())) } else { None }).join(" "));
            let mut used = keep.iter().collect::<Vec<_>>();
            used.sort();
            println!("- used NT symbols:    {}", used.iter().filter_map(|s| if let Symbol::NT(v) = s { Some((v, s)) } else { None })
                .enumerate().map(|(new_id, (id, s))| format!("{}({id} -> {new_id})", s.to_str(self.get_symbol_table()))).join(", "));
            println!("- used  T symbols:    {}", used.iter().filter_map(|s|
                if let Symbol::T(_) = s { Some(s.to_str(self.get_symbol_table())) } else { None }).join(" "));
        }
        let new_num_nt = keep.iter().filter(|s| matches!(s, Symbol::NT(_))).count() as VarId;
        let mut new_v = new_num_nt;
        let mut conv = HashMap::<VarId, VarId>::new();
        for i in (0..self.num_nt).rev() {
            let v = i as VarId;
            let symbol = Symbol::NT(v);
            if !keep.contains(&symbol) {
                if VERBOSE { println!("- deleting {}", symbol.to_str(self.get_symbol_table())); }
                self.prods.remove(i);
                self.start = self.start.map(|s| if s >= v { s - 1 } else { s });
                self.symbol_table.as_mut().map(|t| t.remove_non_terminal(v));
                if i < self.flags.len() {
                    self.flags.remove(i);
                }
                if i < self.parent.len() {
                    self.parent.remove(i);
                }
            } else {
                new_v -= 1;
                conv.insert(v, new_v);
                if VERBOSE { println!("  {symbol:?} -> {:?}", Symbol::NT(new_v)); }
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
        self.nt_conversion = Some(conv.into_iter().map(|(k, v)| (Symbol::NT(k), Symbol::NT(v))).collect::<HashMap<_, _>>());
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
        if symbols.iter().filter(|s| matches!(s, Symbol::NT(_))).count() != self.num_nt {
            self.log.add_warning(format!("calc_first: unused non-terminals: {}",
                                 (0..self.num_nt).map(|v| Symbol::NT(v as VarId)).filter_map(|s|
                                     if symbols.contains(&s) { None } else { Some(format!("{:?} = {}", s, s.to_str(self.get_symbol_table()))) }).join(", ")));
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
                    for (i, sym_i) in factor.iter().enumerate().rev() {
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
            nt_conversion: None,
            log: Logger::new(),
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
            nt_conversion: None,
            log: Logger::new(),
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
        const VERBOSE: bool = false;
        let mut extra = Vec::<ProdRule>::new();
        let mut new_var = self.get_next_available_var();
        // we must take prods out because of the borrow checker and other &mut borrows we need later...
        let mut prods = std::mem::take(&mut self.prods);
        for (i, prod) in prods.iter_mut().enumerate() {
            let var = i as VarId;
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
                // apply the transformation
                let var_ambig = if !ambiguous.is_empty() && fine.len() > 1 {
                    // if more than one independent factor, moves them in a new NT to simplify the resulting rules
                    let var_ambig = new_var;
                    if let Some(table) = &mut self.symbol_table {
                        table.add_var_prime_name(var, new_var);
                    }
                    self.set_flags(var_ambig, ruleflag::CHILD_INDEPENDENT_AMBIGUITY);
                    self.set_parent(var_ambig, var);
                    new_var += 1;
                    extra.push(fine.clone());
                    Some(var_ambig)
                } else {
                    None
                };
                let var_prime = new_var;
                new_var += 1;
                self.set_flags(var_prime, ruleflag::CHILD_L_RECURSION | if ambiguous.is_empty() { 0 } else { ruleflag::CHILD_AMBIGUITY });
                self.set_parent(var_prime, var);
                if let Some(table) = &mut self.symbol_table {
                    table.add_var_prime_name(var, var_prime);
                }
                let symbol_prime = Symbol::NT(var_prime);
                if VERBOSE {
                    print!("- adding non-terminal {var_prime} ({})", symbol_prime.to_str(self.get_symbol_table()));
                    if let Some(v) = var_ambig {
                        print!(" and non-terminal {v} ({})", Symbol::NT(v).to_str(self.get_symbol_table()));
                    }
                    println!(", deriving from {var} ({})", symbol.to_str((self.get_symbol_table())));
                }
                for factor in &mut ambiguous {
                    factor.pop();
                    factor.remove(0);
                    if factor.last().map(|s| *s == symbol).unwrap_or(false) {
                        self.log.add_error(format!("remove_left_recursion: cannot remove recursion from {}", prod_to_string(prod, self.get_symbol_table())));
                        continue;
                    }
                    if let Some(v) = var_ambig {
                        // orig if k > 1:   A   -> A β1 A | ... | A βn A | γ1 | ... | γk | (non-recursive part)
                        // => new:          A_0 -> γ1 | ... | γk
                        // => add to prime: A_p -> β1 A_0 A_p | ... | βn A_0 A_p
                        factor.push(Symbol::NT(v));
                    } else {
                        // orig if k = 1:   A   -> A β1 A | ... | A βn A | γ | (non-recursive part)
                        // => add to prime: A_p -> β1 γ A_p | ... | βn γ A_p
                        factor.v.extend(fine[0].v.clone());
                    }
                    factor.push(symbol_prime);
                }
                for factor in &mut fine {
                    // change A -> γ1 A_p | ... | γk A_p
                    factor.push(symbol_prime.clone());
                }
                for factor in &mut left {
                    // add to prime: A_p -> α1 A_p | ... | αm A_p
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

    /// Factorizes all the left symbols that are common to several factors by rejecting the non-common part
    /// to a new non-terminal.Updates the symbol table if provided.
    ///
    /// Finds the longest prefix α common to two or more factors:
    /// ```eq
    /// A -> α β1 | ... | α βm | γ1 | ... | γn;
    /// ```
    /// Puts the different parts into a new production rule:
    /// ```eq
    /// A   -> α A_0 | γ1 | ... | γn ;
    /// A_0 -> β1 | ... | βm;
    /// ```
    /// Reiterates until all factors start with different symbols in every production rule.
    pub fn left_factorize(&mut self) {
        fn similarity(a: &ProdFactor, b: &ProdFactor) -> usize {
            a.iter().zip(b.iter()).take_while(|(a, b)| a == b).count()
        }

        const VERBOSE: bool = false;
        let mut new_var = self.get_next_available_var();
        // we must take prods out because of the borrow checker and other &mut borrows we need later...
        let mut prods = std::mem::take(&mut self.prods);
        let mut start = Some(0);
        let last = self.num_nt;
        while let Some(first) = start {
            let range = first..last;
            start = None;
            let mut extra = Vec::<ProdRule>::new();
            for i in range {
                let mut prod = &mut prods[i];
                if prod.len() < 2 {
                    continue
                }
                let var = i as VarId;
                let mut maybe_child = None;
                let mut factors = prod.clone();
                factors.sort();
                if VERBOSE { println!("{i}: {} -> {}", Symbol::NT(var).to_str(self.get_symbol_table()), prod_to_string(prod, self.get_symbol_table())); }
                while factors.len() > 1 {
                    let mut max = (0, 0);
                    let mut max_len = 0;
                    let simi = factors.windows(2).enumerate().map(|(j, x)| {
                        let s = similarity(&x[0], &x[1]);
                        if s > max.1 {
                            max = (j, s);
                            max_len = 2;
                        } else if s == max.1 && j + 1 == max.0 + max_len {
                            max_len += 1;
                        }
                        s
                    }).to_vec();
                    if max.1 == 0 {
                        if VERBOSE { println!(" nothing to factorize"); }
                        break;
                    }
                    if VERBOSE {
                        let t = self.get_symbol_table();
                        println!(" - sorted: {} => {}", &factors.iter().map(|f| f.to_str(t)).join(" | "), simi.iter().join(", "));
                        println!("   max: {} for {}", max.1, (0..max_len).map(|j| factors[max.0 + j].to_str(t)).join(", "));
                    }
                    let var_prime = new_var;
                    new_var += 1;
                    self.set_flags(var, ruleflag::PARENT_L_FACTOR);
                    let prime_flags = if let Some(child) = maybe_child {
                        self.set_parent(child, var_prime);
                        ruleflag::CHILD_L_FACTOR | ruleflag::PARENT_L_FACTOR
                    } else {
                        ruleflag::CHILD_L_FACTOR
                    };
                    self.set_flags(var_prime, prime_flags);
                    if let Some(table) = &mut self.symbol_table {
                        table.add_var_prime_name(var, var_prime);
                    }
                    self.set_parent(var_prime, var);
                    let symbol_prime = Symbol::NT(var_prime);
                    if VERBOSE {
                        println!("   adding non-terminal {var_prime} ({}), deriving from {var} ({})",
                                 symbol_prime.to_str(self.get_symbol_table()), Symbol::NT(var).to_str((self.get_symbol_table())));
                    }
                    let mut new_prod = ProdRule::new();
                    for j in 0..max_len {
                        new_prod.push(if factors[max.0 + j].len() > max.1 { ProdFactor::new(factors[max.0 + j][max.1..].to_vec()) } else { prodf!(e) })
                    }
                    if VERBOSE { println!("   new {var_prime}: {} -> {}", symbol_prime.to_str(self.get_symbol_table()), prod_to_string(&new_prod, self.get_symbol_table())); }
                    extra.push(new_prod);
                    for j in 1..max_len {
                        factors.remove(max.0);
                    }
                    factors[max.0].truncate(max.1);
                    factors[max.0].push(symbol_prime);
                    *prod = factors.clone();
                    if VERBOSE { println!("   mod {var}: {} -> {}", Symbol::NT(var).to_str(self.get_symbol_table()), prod_to_string(&prod, self.get_symbol_table())); }
                    if start.is_none() { start = Some(i + 1); }
                    maybe_child = Some(var_prime);
                }
            }
            prods.extend(extra);
        }
        self.prods = prods;
        self.num_nt = self.prods.len();
    }

    pub(crate) fn remove_ambiguity(&self) {
        todo!()
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
    pub fn calc_table(&mut self, first: &HashMap<Symbol, HashSet<Symbol>>, follow: &HashMap<Symbol, HashSet<Symbol>>) -> LLParsingTable {
        fn add_table(table: &mut Vec<Vec<VarId>>, error: VarId, num_t: usize, nt_id: VarId, t_id: VarId, f_id: VarId) {
            let pos = nt_id as usize * num_t + t_id as usize;
            table[pos].push(f_id);
        }
        const VERBOSE: bool = false;
        const DISABLE_FILTER: bool = false;
        let factors = self.prods.iter().enumerate().filter(|(v, _)| DISABLE_FILTER || first.contains_key(&Symbol::NT(*v as VarId)))
            .flat_map(|(v, x)| x.iter().map(move |f| (v as VarId, f.clone() as ProdFactor))).to_vec();
        let error = factors.len() as VarId; // table entry for syntactic error
        let num_nt = self.num_nt;
        let num_t = self.num_t + 1;
        let end = (num_t - 1) as VarId; // index of end symbol
        let mut used_t = HashSet::<Symbol>::new();
        let mut table: Vec<Vec<VarId>> = vec![vec![]; num_nt * num_t];
        for (f_id, (nt_id, factor)) in factors.iter().enumerate() {
            used_t.extend(factor.iter().filter(|s| s.is_t()));
            let f_id = f_id as VarId;
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
                                Symbol::T(t_id) => add_table(&mut table, error, num_t, *nt_id, *t_id, f_id),
                                Symbol::End     => add_table(&mut table, error, num_t, *nt_id, end, f_id),
                                _ => {}
                            }
                        }
                    }
                    Symbol::T(t_id) => {
                        add_table(&mut table, error, num_t, *nt_id, t_id, f_id);
                    }
                    Symbol::NT(_) => {}
                    Symbol::End => {
                        has_end = true;
                    }
                }
            }
            if has_empty && has_end {
                add_table(&mut table, error, num_t, *nt_id, end, end);
            }
        }
        // creates the table and removes ambiguities
        let mut final_table = Vec::<VarId>::new();
        for nt_id in 0..num_nt {
            for t_id in 0..num_t {
                let pos = nt_id * num_t + t_id;
                final_table.push(match table[pos].len() {
                    0 => error,
                    1 => *table[pos].first().unwrap(),
                    _ => {
                        // we take the first item which isn't already in another position on the same NT row
                        let row = (0..num_t).filter(|j| *j != t_id).flat_map(|j| &table[nt_id*num_t + j]).collect::<HashSet<_>>();
                        let chosen = *table[pos].iter().find(|f| !row.contains(f)).unwrap_or(&table[pos][0]);
                        self.log.add_warning(
                            format!("calc_table: ambiguity for NT '{}', T '{}': {} => <{}> has been chosen",
                                    Symbol::NT(nt_id as VarId).to_str(self.get_symbol_table()),
                                    Symbol::T(t_id as VarId).to_str(self.get_symbol_table()),
                                    table[pos].iter().map(|f_id|
                                        format!("<{}>", factors[*f_id as usize].1.to_str(self.get_symbol_table()))).join(" or "),
                                    factors[chosen as usize].1.to_str(self.get_symbol_table())
                            ));
                        table[pos] = vec![chosen];
                        chosen
                    }
                });
            }
        }
        if !(0..num_t - 1).any(|t_id| (0..num_nt).any(|nt_id| final_table[nt_id * num_t + t_id] != error)) {
            self.log.add_error("calc_table: no terminal used in the table".to_string());
        }
        LLParsingTable { num_nt, num_t, factors, table: final_table, flags: self.flags.clone(), parent: self.parent.clone() }
    }

    pub fn create_parsing_table(&mut self) -> LLParsingTable {
        let first = self.calc_first();
        let follow = self.calc_follow(&first);
        self.calc_table(&first, &follow)
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
                        GrNode::LForm => {
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
        prules.log = rules.log;
        for (var, tree) in rules.trees.iter().enumerate() {
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
                    s => panic!("unexpected symbol {s} as root of normalized GrTree for NT {}", Symbol::NT(var as VarId).to_str(prules.get_symbol_table()))
                };
                if prod.iter().any(|f| f.flags & ruleflag::L_FORM != 0) {
                    let mut nt = var as VarId;

                    // We keep the L flag on the child of +* normalization if it's intended only for that normalization.
                    // For example:
                    // - A -> A (b <L>)+ | c
                    //   - doesn't have an l-form left recursion
                    //   - has an l-form repetition of b
                    // - A -> A <L> (b)+ | c
                    //   - has an l-form left recursion
                    //   - doesn't have an l-form repetition of b
                    //
                    // while let Some(parent) = prules.get_parent(nt) {
                    //     nt = parent;
                    // }

                    prules.set_flags(nt as VarId, ruleflag::L_FORM);
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
        prods.simplify();
        prods
    }
}

impl From<ProdRuleSet<General>> for ProdRuleSet<LL1> {
    fn from(mut rules: ProdRuleSet<General>) -> Self {
        rules.remove_left_recursion();
        rules.left_factorize();
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
            _phantom: PhantomData,
        }
    }
}

impl From<ProdRuleSet<General>> for ProdRuleSet<LR> {
    fn from(mut rules: ProdRuleSet<General>) -> Self {
        rules.remove_ambiguity();
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
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates a `GrNode` instance.
    ///
    /// # Examples
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::gnode;
    /// # use rlexer::grammar::{GrNode, Symbol, VarId};
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
    /// assert_eq!(gnode!(L), GrNode::LForm);
    /// assert_eq!(gnode!(R), GrNode::RAssoc);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! gnode {
        ([$id:expr]) => { gnode!(t $id) };
        (t $id:expr) => { GrNode::Symbol(Symbol::T($id as TokenId)) };
        (nt $id:expr) => { GrNode::Symbol(Symbol::NT($id as VarId)) };
        (e) => { GrNode::Symbol(Symbol::Empty) };
        (end) => { GrNode::Symbol(Symbol::End) };
        //
        (&) => { GrNode::Concat };
        (|) => { GrNode::Or };
        (?) => { GrNode::Maybe };
        (+) => { GrNode::Plus };
        (*) => { GrNode::Star };
        (L) => { GrNode::LForm };
        (R) => { GrNode::RAssoc };
    }

    /// Generates a `Symbol` instance.
    ///
    /// # Examples
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::sym;
    /// # use rlexer::grammar::{Symbol, VarId};
    /// assert_eq!(sym!(t 2), Symbol::T(2 as TokenId));
    /// assert_eq!(sym!(nt 3), Symbol::NT(3 as VarId));
    /// assert_eq!(sym!(e), Symbol::Empty);
    /// assert_eq!(sym!(end), Symbol::End);
    #[macro_export(local_inner_macros)]
    macro_rules! sym {
        (t $id:literal) => { Symbol::T($id as TokenId) };
        (nt $id:literal) => { Symbol::NT($id as VarId) };
        (e) => { Symbol::Empty };
        (end) => { Symbol::End };
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
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::grammar::{ProdFactor, Symbol, VarId};
    /// # use rlexer::{prodf, sym};
    /// assert_eq!(prodf!(nt 1, t 2, e), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    /// assert_eq!(prodf!(#128, nt 1, t 2, e), ProdFactor::with_flags(vec![sym!(nt 1), sym!(t 2), sym!(e)], 128));
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! prodf {
        () => { std::vec![] };
        ($($a:ident $($b:literal $(: $num:expr)?)?,)+) => { prodf![$($a $($b $(: $num)?)?),+] };
        ($($a:ident $($b:literal $(: $num:expr)?)?),*) => { ProdFactor::new(std::vec![$(sym!($a $($b $(: $num)?)?)),*]) };
        (#$f:expr, $($a:ident $($b:literal $(: $num:expr)?)?,)+) => { prodf![#$f, $($a $($b $(: $num)?)?),+] };
        (#$f:expr, $($a:ident $($b:literal $(: $num:expr)?)?),*) => { ProdFactor::with_flags(std::vec![$(sym!($a $($b $(: $num)?)?)),*], $f) };
    }

    /// Generates a production rule. It is made up of factors separated by a semicolon.
    ///
    /// Example
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::grammar::{ProdFactor, Symbol, VarId};
    /// # use rlexer::{prod, prodf, sym};
    /// assert_eq!(prod!(nt 1, t 2, nt 1, t 3; nt 2; e),
    ///            vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 ProdFactor::new(vec![sym!(nt  2)]),
    ///                 ProdFactor::new(vec![sym!(e)])]);
    /// assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #128, nt 2; e),
    ///            vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 ProdFactor::with_flags(vec![sym!(nt  2)], 128),
    ///                 ProdFactor::new(vec![sym!(e)])]);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! prod {
        () => { std::vec![] };
        ($($(#$f:expr,)? $($a:ident $($b:expr)?),*;)+) => { prod![$($(#$f,)? $($a $($b)?),+);+] };
        ($($(#$f:expr,)? $($a:ident $($b:expr)?),*);*) => { std::vec![$(prodf![$(#$f,)? $($a $($b)?),+]),*]};
    }
}
