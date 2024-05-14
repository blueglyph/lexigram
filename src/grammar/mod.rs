#![allow(dead_code)]
#![allow(unused)]

mod tests;

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use crate::cproduct::CProduct;
use crate::dfa::TokenId;
use crate::{CollectJoin, General, Normalized, gnode, vaddi, prodf};
use crate::vectree::VecTree;
use crate::symbol_table::SymbolTable;

pub type VarId = u16;

#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Symbol {
    #[default] Empty,
    T(TokenId),
    NT(VarId),
    End
}

#[derive(Clone, PartialEq, Debug)]
pub enum GrNode {
    Symbol(Symbol),
    Concat,
    Or,
    Maybe,
    Plus,
    Star,
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
        }
    }
}

impl Symbol {
    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        symbol_table.map(|t| t.get_name(self)).unwrap_or(self.to_string())
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
}

impl Default for GrTree {
    fn default() -> Self {
        GrTree::new()
    }
}

impl Display for GrTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn snode(show_ids: bool, show_depth: bool, node: &GrNode, node_id: usize, depth: u32) -> String {
            let mut result = String::new();
            if show_depth {
                result.push_str(&depth.to_string());
                result.push('>');
            }
            if show_ids {
                result.push_str(&node_id.to_string());
                result.push(':');
            }
            result.push_str(&node.to_string());
            result
        }
        let show_ids = f.alternate();
        let show_depth = f.sign_plus();
        let mut stack = Vec::<String>::new();
        for node in self.iter_depth() {
            let n = node.num_children();
            if n > 0 {
                let children = stack.drain(stack.len() - n..).join(", ");
                stack.push(format!("{}({children})", snode(show_ids, show_depth, &node, node.index, node.depth)));
            } else {
                stack.push(snode(show_ids, show_depth, &node, node.index, node.depth));
            }
        }
        write!(f, "{}", stack.pop().unwrap_or("empty".to_string()))
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct RuleTreeSet<T> {
    trees: Vec<GrTree>,
    next_var: Option<VarId>,
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
}

// Mutable methods for the General form.
impl RuleTreeSet<General> {
    pub fn new() -> Self {
        RuleTreeSet { trees: Vec::new(), next_var: None, _phantom: PhantomData }
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

    /// Forces the next variable ID to be used when a new rule must be created
    /// by the normalization if `var` is `Some(id)`. If `var` is `None`, lets
    /// the normalization determine the next ID.
    pub fn set_next_var(&mut self, var: Option<VarId>) {
        if let Some(v) = var {
            let min = self.trees.len();
            assert!(v as usize >= min, "the minimum value for next_var is {min}");
        }
        self.next_var = var;
    }

    /// Normalizes all the production rules.
    pub fn normalize(&mut self) {
        let vars = self.get_vars().to_vec();
        for var in vars {
            self.normalize_var(var);
        }
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
        let mut new_var = self.next_var.unwrap_or(self.get_next_available_var());
        // let orig = self.trees.remove(&var).unwrap();
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
                        assert_eq!(n, 1);
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
                    GrNode::Plus => {
                        assert_eq!(n, 1);
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
                        let id = self.normalize_plus_or_star(&mut stack, &mut new, &mut new_var, true);
                        stack.push(id);
                    }
                    GrNode::Star => {
                        assert_eq!(n, 1);
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
                        let id = self.normalize_plus_or_star(&mut stack, &mut new, &mut new_var, false);
                        stack.push(id);
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
        assert_eq!(stack.len(), 1);
        if VERBOSE_CC { println!("Final stack id: {}", stack[0]); }
        new.set_root(stack.pop().unwrap());
        self.set_tree(var, new);
        self.next_var = Some(new_var);
    }

    fn normalize_plus_or_star(&mut self, stack: &mut Vec<usize>, new: &mut VecTree<GrNode>, new_var: &mut VarId, is_plus: bool) -> usize {
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
                        x => panic!("unexpected node type under | node: {x}"),
                    }
                }
                if !is_plus {
                    qtree.add(Some(or), gnode!(e));
                }
            }
            _ => panic!("Unexpected {} under + node", new.get(child))
        }
        let id = new.add(None, gnode!(nt *new_var));
        assert!(*new_var as usize >= self.trees.len() || self.trees[*new_var as usize].is_empty(), "overwriting tree {new_var}");
        self.set_tree(*new_var, qtree);
        *new_var += 1;
        id
    }
}

impl From<RuleTreeSet<General>> for RuleTreeSet<Normalized> {
    /// Transforms a `General` ruleset to a `Normalized` ruleset
    fn from(mut rules: RuleTreeSet<General>) -> Self {
        rules.normalize();
        RuleTreeSet::<Normalized> { trees: rules.trees, next_var: rules.next_var, _phantom: PhantomData }
    }
}

impl From<RuleTreeSet<Normalized>> for RuleTreeSet<General> {
    /// Transforms a `Normalized` ruleset to a `General` ruleset
    fn from(mut rules: RuleTreeSet<Normalized>) -> Self {
        RuleTreeSet::<General> { trees: rules.trees, next_var: rules.next_var, _phantom: PhantomData }
    }
}

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
type ProdRule = Vec<Vec<Symbol>>;
type ProdFactor = Vec<Symbol>;

pub fn factor_to_string(factor: &Vec<Symbol>, symbol_table: Option<&SymbolTable>) -> String {
    factor.iter().map(|symbol|
            symbol_table.map(|t| t.get_name(symbol)).unwrap_or(symbol.to_string())
        ).join(" ")
}

pub fn prod_to_string(prod: &ProdRule, symbol_table: Option<&SymbolTable>) -> String {
    prod.iter().map(|factor| factor_to_string(factor, symbol_table)).join(" | ")
}

#[derive(Clone)]
struct LR;
#[derive(Clone)]
struct LL1;

#[derive(Clone)]
struct ProdRuleSet<T> {
    prods: Vec<ProdRule>,
    symbol_table: Option<SymbolTable>,
    start: Option<VarId>,
    _phantom: PhantomData<T>
}

impl<T> ProdRuleSet<T> {
    /// Returns the starting production rule.
    pub fn get_start(&self) -> Option<VarId> {
        self.start
    }

    /// Sets the starting production rule.
    pub fn set_start(&mut self, start: Option<VarId>) {
        self.start = start;
    }

    /// Returns a variable ID that doesn't exist yet.
    pub fn get_next_available_var(&self) -> VarId {
        self.prods.len() as VarId
    }

    /// Returns all the non-empty prods
    pub fn get_prods_iter(&self) -> impl Iterator<Item=(VarId, &ProdRule)> {
        self.prods.iter().enumerate().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id as VarId, p)) })
    }

    pub fn get_prods_iter_mut(&mut self) -> impl Iterator<Item=(VarId, &mut ProdRule)> {
        self.prods.iter_mut().enumerate().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id as VarId, p)) })
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table);
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        self.symbol_table.as_ref()
    }

    pub fn calc_first(&self) -> HashMap<Symbol, HashSet<Symbol>> {
        let first = HashMap::<Symbol, HashSet<Symbol>>::new();


        first
    }
}

impl ProdRuleSet<LR> {
    pub fn new() -> Self {
        Self { prods: Vec::new(), symbol_table: None, start: None, _phantom: PhantomData }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { prods: Vec::with_capacity(capacity), symbol_table: None, start: None, _phantom: PhantomData }
    }

    /// Eliminates left recursion from production rules, and updates the symbol table if provided.
    ///
    /// A -> A α1 | ... | A αm | β1 | ... | βn;
    ///
    /// becomes
    ///
    /// A   -> β1 A_0 | ... | βn A_0;
    /// A_0 -> α1 A_0 | ... | αm A_0 | ε;
    pub fn remove_left_recursion(&mut self) {
        const VERBOSE: bool = false;
        // we must remove either prods or the symbol table from self for the borrow checker
        let mut symbol_table = self.symbol_table.take();
        let mut new_var = self.get_next_available_var();
        let mut extra = Vec::<ProdRule>::new();
        for (i, prod) in self.prods.iter_mut().enumerate() {
            let var = i as VarId;
            let symbol = Symbol::NT(var);
            if prod.iter().any(|p| *p.first().unwrap() == symbol) {
                if VERBOSE {
                    println!("- left recursion: {}", format!("{} -> {}",
                        Symbol::NT(var).to_str(symbol_table.as_ref()),
                        prod_to_string(prod, symbol_table.as_ref())));
                }
                let (mut left, mut fine) : (Vec<_>, Vec<_>) = prod.iter().cloned()
                    .partition(|factor| *factor.first().unwrap() == symbol);
                // apply the transformation
                let var_prime = new_var;
                if let Some(table) = &mut symbol_table {
                    table.add_var_prime_name(var, var_prime);
                }
                let symbol_prime = Symbol::NT(var_prime);
                if VERBOSE { println!("- adding non-terminal {var_prime} ({}), deriving from {var} ({})",
                    symbol_prime.to_str(symbol_table.as_ref()), symbol.to_str((symbol_table.as_ref())));
                }
                for factor in &mut fine {
                    factor.push(symbol_prime.clone());
                }
                for factor in &mut left {
                    factor.remove(0);
                    factor.push(symbol_prime.clone());
                }
                left.push(vec![Symbol::Empty]);
                *prod = fine;
                extra.push(left);
                new_var += 1;
            }
        }
        self.prods.extend(extra);
        self.symbol_table = symbol_table;
    }

    /// Factorizes all the left symbols that are common to several factors by rejecting the non-common part
    /// to a new non-terminal.Updates the symbol table if provided.
    ///
    /// Finds the longest prefix α common to two or more factors:
    ///
    /// A -> α β1 | ... | α βm | γ1 | ... | γn;
    ///
    /// Puts the different parts into a new production rule:
    ///
    /// A   -> α A_0 | γ1 | ... | γn ;
    /// A_0 -> β1 | ... | βm;
    ///
    /// Reiterates until all factors start with different symbols in every production rule.
    pub fn left_factorize(&mut self) {
        fn similarity(a: &ProdFactor, b: &ProdFactor) -> usize {
            a.iter().zip(b.iter()).take_while(|(a, b)| a == b).count()
        }

        const VERBOSE: bool = false;
        // we must remove either prods or the symbol table from self for the borrow checker
        let mut symbol_table = self.symbol_table.take();
        let mut new_var = self.get_next_available_var();
        let mut start = Some(0);
        let last = self.prods.len();
        while let Some(first) = start {
            let range = first..last;
            start = None;
            let mut extra = Vec::<ProdRule>::new();
            for i in range {
                let mut prod = &mut self.prods[i];
                if prod.len() < 2 {
                    continue
                }
                let var = i as VarId;
                let mut factors = prod.clone();
                factors.sort();
                if VERBOSE { println!("{i}: {} -> {}", Symbol::NT(var).to_str(symbol_table.as_ref()), prod_to_string(prod, symbol_table.as_ref())); }
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
                        let t = symbol_table.as_ref();
                        println!(" - sorted: {} => {}", &factors.iter().map(|f| factor_to_string(f, t)).join(" | "), simi.iter().join(", "));
                        println!("   max: {} for {}", max.1, (0..max_len).map(|j| factor_to_string(&factors[max.0 + j], t)).join(", "));
                    }
                    let var_prime = new_var;
                    new_var += 1;
                    if let Some(table) = &mut symbol_table {
                        table.add_var_prime_name(var, var_prime);
                    }
                    let symbol_prime = Symbol::NT(var_prime);
                    if VERBOSE {
                        println!("   adding non-terminal {var_prime} ({}), deriving from {var} ({})",
                                 symbol_prime.to_str(symbol_table.as_ref()), Symbol::NT(var).to_str((symbol_table.as_ref())));
                    }
                    let mut new_prod = ProdRule::new();
                    for j in 0..max_len {
                        new_prod.push(if factors[max.0 + j].len() > max.1 { factors[max.0 + j][max.1..].to_vec() } else { prodf!(e) })
                    }
                    if VERBOSE { println!("   new {var_prime}: {} -> {}", symbol_prime.to_str(symbol_table.as_ref()), prod_to_string(&new_prod, symbol_table.as_ref())); }
                    extra.push(new_prod);
                    for j in 1..max_len {
                        factors.remove(max.0);
                    }
                    factors[max.0].truncate(max.1);
                    factors[max.0].push(symbol_prime);
                    *prod = factors.clone();
                    if VERBOSE { println!("   mod {var}: {} -> {}", Symbol::NT(var).to_str(symbol_table.as_ref()), prod_to_string(&prod, symbol_table.as_ref())); }
                    if start.is_none() { start = Some(i + 1); }
                }
            }
            self.prods.extend(extra);
        }
        self.symbol_table = symbol_table;
    }
}

impl From<RuleTreeSet<Normalized>> for ProdRuleSet<LR> {
    fn from(mut rules: RuleTreeSet<Normalized>) -> Self {
        fn children_to_vec(tree: &GrTree, parent_id: usize) -> Vec<Symbol> {
            tree.children(parent_id).iter().map(|id| {
                match tree.get(*id) {
                    GrNode::Symbol(s) => s.clone(),
                    x => panic!("unexpected symbol {x} under &")
                }
            }).to_vec()
        }
        let mut prules = Self::with_capacity(rules.trees.len());
        for (var, tree) in rules.trees.iter().enumerate() {
            if !tree.is_empty() {
                let root = tree.get_root().expect("tree {var} has no root");
                let root_sym = tree.get(root);
                let prod = match root_sym {
                    GrNode::Symbol(s) => vec![vec![s.clone()]],
                    GrNode::Concat => vec![children_to_vec(tree, root)],
                    GrNode::Or => tree.children(root).iter()
                        .map(|id| {
                            let child = tree.get(*id);
                            if let GrNode::Symbol(s) = child {
                                vec![s.clone()]
                            } else {
                                assert_eq!(*child, GrNode::Concat, "unexpected symbol {child} under |");
                                children_to_vec(tree, *id)
                            }
                        }).to_vec(),
                    s => panic!("unexpected symbol {s} as root of normalized GrTree")
                };
                prules.prods.push(prod);
            } else {
                prules.prods.push(ProdRule::new()); // empty
            }
        }
        prules
    }
}

impl From<RuleTreeSet<General>> for ProdRuleSet<LR> {
    fn from(rules: RuleTreeSet<General>) -> Self {
        ProdRuleSet::from(RuleTreeSet::<Normalized>::from(rules))
    }
}

impl From<ProdRuleSet<LR>> for ProdRuleSet<LL1> {
    fn from(mut rules: ProdRuleSet<LR>) -> Self {
        rules.remove_left_recursion();
        rules.left_factorize();
        ProdRuleSet::<LL1> {
            prods: rules.prods,
            symbol_table: rules.symbol_table,
            start: rules.start,
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[allow(unused)]
mod for_later {
    use super::*;
    pub struct GrammarBuilder {
        rules: ProdRuleSet<LR>,
        symbol_table: Option<SymbolTable>,
    }

    impl GrammarBuilder {
        pub fn new() -> Self {
            GrammarBuilder {
                rules: ProdRuleSet::new(),
                symbol_table: None,
            }
        }

        fn build(&mut self) {
            todo!()
        }
    }

    impl From<RuleTreeSet<Normalized>> for GrammarBuilder {
        fn from(rules: RuleTreeSet<Normalized>) -> Self {
            GrammarBuilder {
                rules: ProdRuleSet::from(rules),
                symbol_table: None,
            }
        }
    }

    impl From<RuleTreeSet<General>> for GrammarBuilder {
        fn from(rules: RuleTreeSet<General>) -> Self {
            let normalized = RuleTreeSet::<Normalized>::from(rules);
            Self::from(normalized)
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
        (t $id:expr) => { Symbol::T($id as TokenId) };
        (nt $id:expr) => { Symbol::NT($id as VarId) };
        (e) => { Symbol::Empty };
        (end) => { Symbol::End };
    }

    /// Generates a production rule factor. A factor is made up of symbols separated by a comma.
    /// Each symbol is either
    /// - a non-terminal: `nt` {integer}
    /// - a terminal: `t` {integer}
    /// - the empty symbol: `e`
    ///
    /// # Example
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::grammar::{Symbol, VarId};
    /// # use rlexer::{prodf, sym};
    /// assert_eq!(prodf!(nt 1, t 2, e), vec![sym!(nt 1), sym!(t 2), sym!(e)]);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! prodf {
        () => { std::vec![] };
        ($($a:ident $($b:expr)?,)+) => { prodf![$($a $($b)?),+] };
        ($($a:ident $($b:expr)?),*) => { std::vec![$(sym!($a $($b)?)),*]};
    }

    /// Generates a production rule. It is made up of factors separated by a semi-colon.
    ///
    /// Example
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::grammar::{Symbol, VarId};
    /// # use rlexer::{prod, prodf, sym};
    /// assert_eq!(prod!(nt 1, t 2, nt 1, t 3; nt 2; e),
    ///            vec![vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)],
    ///                 vec![sym!(nt  2)],
    ///                 vec![sym!(e)]]);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! prod {
        () => { std::vec![] };
        ($($($a:ident $($b:expr)?),*;)+) => { prod![$($($a $($b)?),+);+] };
        ($($($a:ident $($b:expr)?),*);*) => { std::vec![$(prodf![$($a $($b)?),+]),*]};
    }
}
