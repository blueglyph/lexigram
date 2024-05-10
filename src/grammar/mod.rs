#![allow(dead_code)]
#![allow(unused)]

mod tests;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use crate::cproduct::CProduct;
use crate::dfa::TokenId;
use crate::{CollectJoin, gnode, vaddi};
use crate::vectree::VecTree;

pub type VarId = u16;

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub enum Symbol {
    #[default] Empty,
    T(TokenId),
    NT(VarId)
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
            Symbol::T(id) => write!(f, "[{id}]"),
            Symbol::NT(id) => write!(f, "{id}"),
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
    pub fn to_str(&self, symbol_table: &[(String, Option<String>)]) -> String {
        match self {
            Symbol::Empty => "ε".to_string(),
            Symbol::T(id) => format!("'{}'", symbol_table[*id as usize].1.as_ref().unwrap()),
            Symbol::NT(id) => format!("{}", symbol_table[*id as usize].0),
        }
    }
}

impl GrNode {
    pub fn to_str(&self, symbol_table: &[(String, Option<String>)]) -> String {
        match self {
            GrNode::Symbol(s) => s.to_str(symbol_table),
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
    trees: HashMap<VarId, GrTree>,
    next_var: Option<VarId>,
    _phantom: PhantomData<T>
}

/// Marker for general tree form (not normalized). This form may include any
/// operators like `*`, `+`, and `?`, and doesn't have a restriction on depth.
pub struct General;
/// Marker for normalized form. This form may only have `|`, `&`, and symbols,
/// and must have one of the 3 following patterns:
/// - a symbol
/// - a `&` with only symbols as children
/// - a `|` with only `&(symbols)` or symbols as children
pub struct Normalized;

// Methods for both General and Normalized forms. There can only be immutable methods
// in the normalized form.
impl<T> RuleTreeSet<T> {
    pub fn get_tree(&self, var: VarId) -> Option<&GrTree> {
        self.trees.get(&var)
    }

    pub fn get_vars(&self) -> impl Iterator<Item=&VarId> {
        self.trees.keys()
    }

    /// Returns a variable ID that doesn't exist yet.
    pub fn get_next_var(&self) -> VarId {
        self.trees.keys().max().map(|last| last + 1).unwrap_or(0)
    }
}

// Mutable methods for the General form.
impl RuleTreeSet<General> {
    pub fn new() -> Self {
        RuleTreeSet { trees: HashMap::new(), next_var: None, _phantom: PhantomData }
    }

    pub fn new_var(&mut self, var: VarId) -> &mut GrTree {
        self.trees.insert(var, VecTree::new());
        self.trees.get_mut(&var).unwrap()
    }

    pub fn get_tree_mut(&mut self, var: VarId) -> Option<&mut GrTree> {
        self.trees.get_mut(&var)
    }

    pub fn set_next_var(&mut self, var: Option<VarId>) {
        if let Some(v) = var {
            let min = self.get_next_var();
            assert!(v >= min, "the minimum value for next_var is {min}");
        }
        self.next_var = var;
    }

    /// Normalizes all the production rules.
    pub fn normalize(&mut self) {
        let vars = self.get_vars().cloned().to_vec();
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
        let mut new_var = self.next_var.unwrap_or(self.get_next_var());
        let orig = self.trees.remove(&var).unwrap();
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
        self.trees.insert(var, new);
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
        self.trees.insert(*new_var, qtree);
        *new_var += 1;
        id
    }
}

impl From<RuleTreeSet<General>> for RuleTreeSet<Normalized> {
    /// Transforms a `General` ruleset to a `Normalized` ruleset
    fn from(mut value: RuleTreeSet<General>) -> Self {
        value.normalize();
        RuleTreeSet::<Normalized> { trees: value.trees, next_var: value.next_var, _phantom: PhantomData }
    }
}

impl From<RuleTreeSet<Normalized>> for RuleTreeSet<General> {
    /// Transforms a `Normalized` ruleset to a `General` ruleset
    fn from(mut value: RuleTreeSet<Normalized>) -> Self {
        RuleTreeSet::<General> { trees: value.trees, next_var: value.next_var, _phantom: PhantomData }
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
struct Prod(Vec<Vec<GrNode>>);

impl Prod {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

struct RuleProdSet {
    prods: HashMap<VarId, Prod>
}

impl RuleProdSet {
    pub fn new() -> Self {
        Self { prods: HashMap::new() }
    }
}

impl From<RuleTreeSet<Normalized>> for RuleProdSet {
    fn from(mut rules: RuleTreeSet<Normalized>) -> Self {
        fn children_to_vec(tree: &GrTree, parent_id: usize) -> Vec<GrNode> {
            tree.children(parent_id).iter().map(|id| tree.get(*id).clone()).to_vec()
        }
        let mut prods = Self::new();
        for var in rules.get_vars() {
            let tree = rules.get_tree(*var).unwrap();
            let root = tree.get_root().expect("tree {var} has no root");
            let root_sym = tree.get(root);
            let prod = match root_sym {
                GrNode::Symbol(_) => vec![vec![root_sym.clone()]],
                GrNode::Concat => vec![children_to_vec(tree, root)],
                GrNode::Or => tree.children(root).iter()
                    .map(|id| {
                        let child = tree.get(*id);
                        if let GrNode::Symbol(_) = child {
                            vec![child.clone()]
                        } else {
                            assert_eq!(*child, GrNode::Concat, "unexpected symbol {child} under |");
                            children_to_vec(tree, *id)
                        }
                    }).to_vec(),
                s => panic!("unexpected symbol {s} as root of normalized GrTree")
            };
            prods.prods.insert(*var, Prod(prod));
        }
        prods
    }
}

impl From<RuleTreeSet<General>> for RuleProdSet {
    fn from(value: RuleTreeSet<General>) -> Self {
        RuleProdSet::from(RuleTreeSet::<Normalized>::from(value))
    }
}

// ---------------------------------------------------------------------------------------------

pub struct GrammarBuilder {
    rules: RuleProdSet,
    symbols: Vec<(String, Option<String>)>
}

impl GrammarBuilder {
    pub fn new() -> Self {
        GrammarBuilder {
            rules: RuleProdSet::new(),
            symbols: Vec::new()
        }
    }

    fn build(&mut self) {
        todo!()
    }
}

impl From<RuleTreeSet<Normalized>> for GrammarBuilder {
    fn from(value: RuleTreeSet<Normalized>) -> Self {
        GrammarBuilder {
            rules: RuleProdSet::from(value),
            symbols: Vec::new()
        }
    }
}

impl From<RuleTreeSet<General>> for GrammarBuilder {
    fn from(value: RuleTreeSet<General>) -> Self {
        let normalized = RuleTreeSet::<Normalized>::from(value);
        Self::from(normalized)
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
        //
        (&) => { GrNode::Concat };
        (|) => { GrNode::Or };
        (?) => { GrNode::Maybe };
        (+) => { GrNode::Plus };
        (*) => { GrNode::Star };
    }

}
