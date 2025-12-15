// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub mod tests;
pub mod origin;
mod prs;

pub use prs::*;

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::mem::take;
use std::ops::{Deref, DerefMut};
use iter_index::IndexerIterator;
use vectree::VecTree;
use lexigram_core::alt::ruleflag;
use lexigram_core::CollectJoin;
use crate::cproduct::CProduct;
use crate::{alt, gnode, hashset, indent_source, prule, sym, vaddi, General, Normalized, TokenId, VarId, LL1, LR};
use crate::fixed_sym_table::SymInfoTable;
use crate::grammar::NTConversion::{MovedTo, Removed};
use crate::grammar::origin::{FromPRS, FromRTS, Origin};
use lexigram_core::log::{BufLog, LogReader, LogStatus, Logger};
use crate::build::{BuildErrorSource, BuildFrom, HasBuildErrorSource};
use crate::parser::Symbol;
use crate::SymbolTable;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum GrNode {
    Symbol(Symbol),
    Concat,
    Or,
    Maybe,
    Plus,
    Star,
    /// L-form attribute of an alternative or a `+` / `*` repetition expression.
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
    ///     of the alternative that includes the `+` or `*` repetition. For example, `A -> a (<L=AIter> b)* c` uses `AIter`,
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
    RAssoc,         // applied to alternative, right-associative
    PrecEq,         // applied to alternative, same precedence as previous alternative
    Instance,       // instance of * or + in reference origin trees
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
            GrNode::RAssoc => write!(f, "<R>"),
            GrNode::PrecEq => write!(f, "<P>"),
            GrNode::Instance => write!(f, "inst "),
        }
    }
}

impl GrNode {
    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        match self {
            GrNode::Symbol(s) => symbol_table.map(|t| t.get_str(s)).unwrap_or(s.to_string()),
            GrNode::LForm(v) => format!("<L={}>", symbol_table.map(|t| t.get_str(&Symbol::NT(*v))).unwrap_or(v.to_string())),
            _ => self.to_string()
        }
    }

    pub fn gen_source_code(&self) -> String {
        match self {
            GrNode::Symbol(s) => format!("gnode!({})", s.to_macro_item()),
            GrNode::Concat    => "gnode!(&)".to_string(),
            GrNode::Or        => "gnode!(|)".to_string(),
            GrNode::Maybe     => "gnode!(?)".to_string(),
            GrNode::Plus      => "gnode!(+)".to_string(),
            GrNode::Star      => "gnode!(*)".to_string(),
            GrNode::LForm(v)  => format!("gnode!(L {v})"),
            GrNode::RAssoc    => "gnode!(R)".to_string(),
            GrNode::PrecEq    => "gnode!(P)".to_string(),
            GrNode::Instance  => "gnode!(inst)".to_string(),
        }
    }

    pub fn is_modifier(&self) -> bool {
        matches!(self, GrNode::LForm(_) | GrNode::RAssoc | GrNode::PrecEq)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, GrNode::Symbol(Symbol::Empty))
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

/// Builds the string representation of the [`GrTree`], using the [`symbol table`](SymbolTable) if available
/// and optionally starting at node `node`. If `emphasis` contains a node ID, this subpart of the tree
/// is emphasized in the string.
pub fn grtree_to_str_custom(tree: &GrTree, node: Option<usize>, emphasis: Option<usize>, nt: Option<VarId>, symbol_table: Option<&SymbolTable>, simplified: bool, ansi: bool)
                            -> String
{
    fn pr_join(children: Vec<(u32, String)>, str: &str, pr: u32) -> (u32, String) {
        (pr, children.into_iter()
            .map(|(p_ch, ch)| if p_ch >= pr { ch } else { format!("({ch})") })
            .join(str))
    }

    fn pr_append(child: (u32, String), str: &str, pr: u32) -> (u32, String) {
        (pr, if child.0 >= pr { format!("{}{str}", child.1) } else { format!("({}){str}", child.1) })
    }

    const PR_PROD: u32 = 1;
    const PR_TERM: u32 = 2;
    const PR_FACTOR: u32 = 3;
    const PR_ATOM: u32 = 4;

    const BEFORE: &str = " ►► ";
    const AFTER : &str = " ◄◄ ";
    const BEFORE_ANSI: &str = "\u{1b}[4;1;36m";
    const AFTER_ANSI : &str = "\u{1b}[0m";

    let mut children = vec![];
    if tree.is_empty() {
        return "<empty>".to_string();
    }
    let before = if ansi { BEFORE_ANSI } else { BEFORE };
    let after = if ansi { AFTER_ANSI } else { AFTER };
    let top = node.unwrap_or_else(|| tree.get_root().unwrap());
    for node in tree.iter_depth_simple_at(top) {
        let (pr, mut str) = match node.num_children() {
            0 => {
                match node.deref() {
                    GrNode::Symbol(s) => (PR_ATOM, s.to_str_quote(symbol_table)),
                    GrNode::LForm(var) => (PR_ATOM, if simplified || nt == Some(*var) { "<L>".to_string() } else { format!("<L={}>", Symbol::NT(*var).to_str(symbol_table)) }),
                    GrNode::RAssoc => (PR_ATOM, "<R>".to_string()),
                    GrNode::PrecEq => (PR_ATOM, "<P>".to_string()),
                    s => panic!("{s:?} should have children"),
                }
            }
            n => {
                let mut node_children = children.split_off(children.len() - n);
                match node.deref() {
                    GrNode::Concat => pr_join(node_children, " ", PR_TERM),
                    GrNode::Or => pr_join(node_children, " | ", PR_PROD),
                    GrNode::Maybe => pr_append(node_children.pop().unwrap(), "?", PR_FACTOR),
                    GrNode::Plus => pr_append(node_children.pop().unwrap(), "+", PR_FACTOR),
                    GrNode::Star => pr_append(node_children.pop().unwrap(), "*", PR_FACTOR),
                    GrNode::Instance => pr_join(node_children, " ", PR_FACTOR),
                    s => panic!("{s:?} shouldn't have {n} child(ren)"),
                }
            }
        };
        if Some(node.index) == emphasis {
            str = format!("{before}{str}{after}");
        }
        children.push((pr, str));
    }
    children.pop().unwrap().1
}

pub fn grtree_to_str(tree: &GrTree, node: Option<usize>, emphasis: Option<usize>, var: Option<VarId>, symbol_table: Option<&SymbolTable>, simplified: bool) -> String {
    grtree_to_str_custom(tree, node, emphasis, var, symbol_table, simplified, false)
}

pub fn grtree_to_str_ansi(tree: &GrTree, node: Option<usize>, emphasis: Option<usize>, var: Option<VarId>, symbol_table: Option<&SymbolTable>, simplified: bool) -> String {
    grtree_to_str_custom(tree, node, emphasis, var, symbol_table, simplified, true)
}

/// Cleans the empty symbols in a normalized tree. Removes empty terms if `del_empty_terms` is true.
///
/// Returns `Some((is_empty, had_empty_term))` for normalized trees, where
/// * `is_empty` = true if only ε remains
/// * `had_empty_term` = true if the tree had an empty term (was of the form `α | ε`)
///
/// If the top of the tree isn't a symbol, a `&`, or & `|`, the function doesn't process the tree
/// and returns `None`. If something else than those 3 types of nodes is met inside the tree, it's
/// simply ignored.
///
/// The modifiers `<L>`, `<R>`, or `<P>` alone(s) with `ε` in a term will be simplified, but not
/// if there are other items in the term:
///
/// ```text
/// del_empty_terms: true     false
///                  ----     -----
/// a | <L> ε   =>   a        a | ε
/// a | <R>     =>   a        a | ε
/// <P> ε a     =>   <P> a    <P> a
/// ```
fn grtree_cleanup(tree: &mut GrTree, top: Option<usize>, del_empty_term: bool) -> Option<(bool, bool)> {
    const VERBOSE: bool = false;
    let root = top.unwrap_or_else(|| tree.get_root().unwrap());
    let mut had_empty_term = false;
    let (terms, is_or) = match tree.get(root) {
        GrNode::Symbol(s) => {
            let is_empty = *s == Symbol::Empty;
            return Some((is_empty, is_empty));
        }
        GrNode::Concat => (vec![root], false),
        GrNode::Or => (tree.children(root).to_owned(), true),
        // we don't handle those cases:
        GrNode::Maybe | GrNode::Plus | GrNode::Star | GrNode::LForm(_)
        | GrNode::RAssoc | GrNode::PrecEq | GrNode::Instance => { return None }
    };
    let terms_len = terms.len();
    let mut empty_terms = vec![];
    for (term_pos, term) in terms.into_iter().enumerate() {
        match *tree.get(term) {
            GrNode::Concat => {
                let children = tree.children(term);
                let len = children.len();
                let mut empty_pos = vec![];
                let n_modifiers = children.iter().enumerate()
                    .fold(0, |n_mod, (pos, &index)| {
                        let n = tree.get(index);
                        if n.is_empty() {
                            empty_pos.push(pos);
                        }
                        n_mod + if n.is_modifier() { 1 } else { 0 }
                    });
                if VERBOSE { print!("- term {}  => {empty_pos:?} empty, {n_modifiers} modifier", tree.to_str_index(Some(term), None)); }
                if empty_pos.len() + n_modifiers == len {
                    *tree.get_mut(term) = gnode!(e);
                    tree.children_mut(term).clear();
                    empty_terms.push(term_pos);
                    if VERBOSE { println!(" (replacing everything with ε)"); }
                } else if !empty_pos.is_empty() {
                    if VERBOSE { println!("  => (removing {} ε)", empty_pos.len()); }
                    let new_children = tree.children_mut(term);
                    for i in empty_pos.into_iter().rev() {
                        new_children.remove(i);
                    }
                } else {
                    if VERBOSE { println!(" (nothing to do)"); }
                }
            }
            GrNode::Symbol(Symbol::Empty) => {
                empty_terms.push(term_pos);
            }
            n if n.is_modifier() => {   // lone modifier
                *tree.get_mut(term) = gnode!(e);
                empty_terms.push(term_pos);
            }
            _ => {}
        }
    }
    if VERBOSE { println!("  {} empty terms: {empty_terms:?}", empty_terms.len()); }
    if !empty_terms.is_empty() {
        had_empty_term = true;
        if is_or {
            if !del_empty_term && terms_len > 1 || empty_terms.len() == terms_len {
                empty_terms.pop();
            }
            let or_children = tree.children_mut(root);
            for i in empty_terms.into_iter().rev() {
                or_children.remove(i);
            }
            if VERBOSE { println!("or_children => {or_children:?}"); }
        }
    }
    if is_or && tree.children(root).len() == 1 {
        // removes the top | if it has only one child (not entirely necessary but simplifies the rest)
        let subroot_index = tree.children(root)[0];
        let subroot = *tree.get(subroot_index);
        let subroot_children = tree.children(subroot_index).to_owned();
        *tree.get_mut(root) = subroot;
        *tree.children_mut(root) = subroot_children;
    }
    let is_empty = match tree.get(root) {
        GrNode::Symbol(s) => s.is_empty(),
        GrNode::Concat => false, // an empty `&` is simplified to `ε` above // matches!(tree.children(root), &[i] if tree.get(i).is_empty()),
        GrNode::Or => false, // an empty `|` is simplified to `ε` above
        GrNode::Maybe | GrNode::Plus | GrNode::Star | GrNode::LForm(_) | GrNode::RAssoc | GrNode::PrecEq | GrNode::Instance => false,
    };
    Some((is_empty, had_empty_term))
}

/// Removes duplicate 'ε' symbols in `nodes` alts before adding them as children in a `&`.
///
/// ## Examples:
/// * `ε ε` -> `ε`
/// * `ε A` -> `A`
/// * `ε` -> `ε`
fn remove_concat_dup_empty(tree: &GrTree, nodes: &mut Vec<usize>) {
    if nodes.len() > 1 {
        let mut i = 0;
        while i < nodes.len() && nodes.len() > 1 {
            if tree.get(nodes[i]).is_empty() {
                nodes.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

/// Adds methods to GrTree.
///
/// _NOTE: We must create a trait for GrTree since we can't implement functions for an external type,
/// and a type alias is not considered as a new type._
pub trait GrTreeExt {
    fn get_dup(&mut self, dup_index: &mut Dup) -> usize;
    fn to_str(&self, start_node: Option<usize>, symbol_table: Option<&SymbolTable>) -> String;
    fn to_str_index(&self, start_node: Option<usize>, symbol_table: Option<&SymbolTable>) -> String;
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

    fn to_str_index(&self, start_node: Option<usize>, symbol_table: Option<&SymbolTable>) -> String {
        let tfmt = GrTreeFmt {
            tree: &self,
            show_ids: true,
            show_depth: false,
            symbol_table,
            start_node,
        };
        tfmt.to_string()
    }
}

pub struct GrTreeFmt<'a> {
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
            if self.symbol_table.is_some() { sym.to_str_quote(self.symbol_table) } else { sym.to_str(self.symbol_table) }
        } else {
            node.to_str(self.symbol_table)
        };
        result.push_str(name.as_str());
        result
    }
}

impl Display for GrTreeFmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.tree.is_empty() {
            return write!(f, "<empty>");
        }
        let start_node = self.start_node.unwrap_or_else(|| self.tree.get_root().expect("the tree must have a defined root"));
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
        write!(f, "{}", stack.pop().unwrap_or_else(|| "empty".to_string()))
    }
}

// ---------------------------------------------------------------------------------------------

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
    nt_conversion: HashMap<VarId, NTConversion>,
    origin: Origin<(VarId, usize), FromRTS>,
    log: BufLog,
    _phantom: PhantomData<T>
}

impl<T> HasBuildErrorSource for RuleTreeSet<T> {
    const SOURCE: BuildErrorSource = BuildErrorSource::RuleTreeSet;
}

// Methods for both General and Normalized forms. There can only be immutable methods
// in the normalized form.
impl<T> RuleTreeSet<T> {
    pub fn get_num_nt(&self) -> VarId {
        self.trees.len() as VarId
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
        let tset = self.trees.iter()
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

    /// Builds the string representation of the [`rule tree set`](RuleTreeSet) of variable `var`,
    /// optionally starting at node `node`. If `emphasis` contains a node ID, this subpart of the
    /// tree is emphasized in the string.
    pub fn to_str(&self, var: VarId, node: Option<usize>, emphasis: Option<usize>) -> String {
        grtree_to_str(&self.trees[var as usize], node, emphasis, Some(var), self.get_symbol_table(), false)
    }

    pub fn get_log_mut(&mut self) -> &mut BufLog {
        &mut self.log
    }
}

impl<T> LogReader for RuleTreeSet<T> {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        &self.log
    }

    fn give_log(self) -> Self::Item {
        self.log
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
            nt_conversion: HashMap::new(),
            origin: Origin::new(),
            log,
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
        self.log.add_note("original rules:");
        (0..self.trees.len() as VarId)
            .for_each(|v| self.log.add_note(format!("- NT[{v:2}] {} -> {}", Symbol::NT(v).to_str(self.get_symbol_table()), self.to_str(v, None, None))));
        self.log.add_note("normalizing rules...");
        self.check_num_nt_coherency();
        self.normalize_vars();
        self.flags.resize(self.trees.len(), 0);
        self.parent.resize(self.trees.len(), None);
    }

    fn check_num_nt_coherency(&mut self) {
        if let Some(n) = self.symbol_table.as_ref().and_then(|table| Some(table.get_num_nt())) {
            if n != self.trees.len() {
                self.log.add_error(format!("there are {} rules but the symbol table has {n} nonterminal symbols: dropping the table", self.trees.len()));
                self.symbol_table = None;
            }
        }
    }

    /// Transforms the production rule tree into a list of rules in normalized format:
    /// `var -> &(leaf_1, leaf_2, ...leaf_n)`
    ///
    /// The product may have to be split if operators like `+` or `*` are used. In this
    /// case, new nonterminals are created, with increasing IDs starting from
    /// `new_var`.
    fn normalize_vars(&mut self) {
        const VERBOSE: bool = false;
        const VERBOSE_CC: bool = false;
        let mut exclude_nt = HashSet::<VarId>::new();
        for var in 0..self.get_num_nt() {
            if exclude_nt.contains(&var) {
                if VERBOSE { println!("NT[{var}] {} excluded", Symbol::NT(var).to_str(self.get_symbol_table())); }
                continue
            }
            if VERBOSE { println!("normalize_var(NT[{var}] {})", Symbol::NT(var).to_str(self.get_symbol_table())); }
            let mut new_var = self.get_next_available_var();
            let orig = take(&mut self.trees[var as usize]);
            let mut new = GrTree::new();
            let mut orig_new = GrTree::new();
            let mut orig_rep_vars = HashMap::<VarId, usize>::new();
            let mut stack = Vec::<usize>::new();    // indices in new
            for sym in orig.iter_depth() {
                let n = sym.num_children();
                if VERBOSE { println!("- old {}:{}", sym.index, *sym); }
                if n == 0 {
                    let new_id = new.add(None, orig.get(sym.index).clone());
                    stack.push(new_id);
                    if VERBOSE { print!("  leaf: "); }
                } else {
                    match *sym {
                        // we must rearrange the operations so that any item on the stack is only
                        // one of those patterns:
                        // - a leaf
                        // - a &(leaves)
                        // - a |(&(leaves) or leaves)
                        GrNode::Concat | GrNode::Or => {
                            let children = stack.split_off(stack.len() - n);
                            let new_id = if children.iter().all(|&idx| !matches!(new.get(idx), GrNode::Concat|GrNode::Or)) {
                                if VERBOSE { print!("  trivial {}: children={}\n  ", *sym, children.iter().map(|s| new.get(*s).to_str(self.get_symbol_table())).join(", ")); }
                                // trivial case with only leaves as children (could be removed and treated as a general case)
                                new.addci_iter(None, sym.clone(), children)
                            } else {
                                if *sym == GrNode::Or {
                                    if VERBOSE {
                                        // println!("  or: children={}", children.iter().map(|&id| format!("{id}:{}", grtree_to_str(&new, Some(id), None, self.get_symbol_table()))).join(", "));
                                        println!("  or: children={}", children.iter().map(|&id| format!("{}", new.to_str_index(Some(id), self.get_symbol_table()))).join(", "));
                                    }
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
                                } else { // *sym == GrNode::Concat
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
                                        .map(|dup_ids| {
                                            let mut nodes = dup_ids.into_iter()
                                                .flat_map(|dup_id| dups.get_mut(dup_id).unwrap().iter_mut()
                                                    .map(|dup| new.get_dup(dup)).to_vec()).to_vec();
                                            remove_concat_dup_empty(&new, &mut nodes);
                                            nodes
                                        })
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
                                let maybe_child = stack.pop().unwrap();
                                let proceed = match grtree_cleanup(&mut new, Some(maybe_child), true) {
                                    None => {
                                        self.log.add_error(format!(
                                            "unexpected child of ?: {} (should be &, |, or symbol)",
                                            grtree_to_str(&new, Some(maybe_child), None, Some(var), self.get_symbol_table(), false)));
                                        if VERBOSE { println!("ERROR: unexpected child of ?: {}", grtree_to_str(&new, Some(maybe_child), None, Some(var), self.get_symbol_table(), false)); }
                                        return;
                                    }
                                    // (is_empty, had_empty_term)
                                    Some((true, _)) => {
                                        // the child is `ε`
                                        stack.push(maybe_child);
                                        if VERBOSE { println!("child of ? simplified to ε"); }
                                        false
                                    }
                                    _ => true,
                                };
                                if proceed {
                                    let empty = new.add(None, gnode!(e));
                                    let id = match new.get(maybe_child) {
                                        GrNode::Or => {
                                            new.add(Some(maybe_child), gnode!(e));
                                            maybe_child
                                        }
                                        _ => new.addci_iter(None, gnode!(|), [maybe_child, empty])
                                    };
                                    stack.push(id);
                                }
                            }
                        }
                        GrNode::Plus | GrNode::Star => {
                            // + can change to *, so we treat both of them at the same time
                            //
                            // P -> αβ+γ becomes P -> αQγ
                            //                   Q -> βQ | β
                            //
                            //     self              new  new(Q=next_var_id)               simpler format
                            //     ----------------------------------------------------------------------------------------
                            //     +(A)           -> Q    |(&(A,Q), A')                    AQ|A
                            //     +(&(A,B))      -> Q    |(&(A,B,Q),&(A',B'))             ABQ|AB
                            //     +(|(&(A,B),C)) -> Q    |(&(A,B,Q),&(C,Q'),&(A',B'),C')  (AB|C)Q | (AB|C) = ABQ|CQ | AB|C
                            //
                            // P -> αβ*γ becomes P -> αQγ
                            //                   Q -> βQ | ε
                            //
                            //     self              new  new(Q=next_var_id)     simpler format
                            //     -----------------------------------------------------------------------
                            //     *(A)           -> Q    |(&(A,Q), ε)           AQ|ε
                            //     *(&(A,B))      -> Q    |(&(A,B,Q),ε)          ABQ|ε
                            //     *(|(&(A,B),C)) -> Q    |(&(A,B,Q),&(C,Q'),ε)  (AB|C)Q | ε = ABQ|CQ | ε
                            let mut is_plus = *sym == GrNode::Plus;
                            let sym_char = if is_plus { '+' } else { '*' };
                            if VERBOSE { print!("  {sym_char}: "); }
                            if n != 1 {
                                self.log.add_error(format!(
                                    "normalize_var({}): {sym_char} should only have one child; found {n}: {}",
                                    Symbol::NT(var).to_str(self.get_symbol_table()),
                                    orig.to_str(Some(sym.index), self.get_symbol_table())));
                                if VERBOSE { println!("ERROR: found {n} children instead of 1"); }
                                return;
                            }
                            let rep_child = stack.pop().unwrap();
                            if VERBOSE {}
                            let proceed = match grtree_cleanup(&mut new, Some(rep_child), true) {
                                None => {
                                    self.log.add_error(format!(
                                        "unexpected child of {sym_char}: {} (should be &, |, or symbol)",
                                        grtree_to_str(&new, Some(rep_child), None, Some(var), self.get_symbol_table(), false)));
                                    if VERBOSE { println!("ERROR: unexpected child {}", grtree_to_str(&new, Some(rep_child), None, Some(var), self.get_symbol_table(), false)); }
                                    return;
                                }
                                // (is_empty, had_empty_term)
                                Some((true, _)) => {
                                    // the child is `ε`
                                    stack.push(rep_child);
                                    if VERBOSE { println!("child simplified to ε"); }
                                    false
                                }
                                Some((false, true)) => {
                                    // the child had the form `α + ε` and is now `α`, so if the operator was +,
                                    // it must become * since empty must remain a possibility (it doesn't change
                                    // (anything if it was already *)
                                    if is_plus {
                                        is_plus = false;
                                        // sym_char = '*';
                                        if VERBOSE { print!(" becomes * (child lost ε term), "); }
                                    }
                                    true
                                }
                                Some((false, false)) => true, // nothing special, processing below
                            };
                            if proceed {
                                if VERBOSE { println!("=> {}", grtree_to_str(&new, Some(rep_child), None, Some(var), self.get_symbol_table(), false)); }
                                let orig_rep = orig_new.add(None, if is_plus { gnode!(+) } else { gnode!(*) });
                                let orig_rep_child = orig_new.add_from_tree(Some(orig_rep), &new, Some(rep_child));
                                let (id, qvar) = self.normalize_plus_or_star(
                                    rep_child, orig_rep, orig_rep_child, &mut new, &mut orig_new, var, &mut new_var, is_plus, &mut exclude_nt);
                                stack.push(id);
                                orig_rep_vars.insert(qvar, orig_rep); // to replace later
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
            let root = stack.pop().unwrap();
            new.set_root(root);
            match grtree_cleanup(&mut new, None, false) {
                None => {
                    self.log.add_error(format!(
                        "unexpected root of {} -> {} (should be &, |, or symbol)",
                        Symbol::NT(var).to_str(self.get_symbol_table()),
                        grtree_to_str(&new, None, None, Some(var), self.get_symbol_table(), false)));
                    if VERBOSE { println!("ERROR: unexpected root {}", grtree_to_str(&new, None, None, Some(var), self.get_symbol_table(), false)); }
                }
                // (is_empty, had_empty_term)
                Some((true, _)) => {
                    self.log.add_warning(format!("{} is empty", Symbol::NT(var).to_str(self.get_symbol_table())));
                }
                _ => {}
            }
            let orig_root = orig_new.add_from_tree_callback(None, &new, None, |from, to, _| self.origin.add((var, to), (var, from)));
            orig_new.set_root(orig_root);
            while !orig_rep_vars.is_empty() {
                // We must replace new nonterminals with their original (though normalized) +* content, but we can't
                // update `orig_new` while we're iterating it, so we proceed in two steps:
                // - iterate in `orig_new`, locate the new +* nonterminals and put the node indices in orig_rep_nodes
                // - iterate orig_rep_nodes and modify nodes in orig_new
                // Since each replacement can make new nonterminals visible (if they're embedded in one another),
                // we must repeat those steps until all `orig_rep_vars` have been found and replaced.
                let mut orig_rep_nodes = Vec::<(usize, usize)>::new();
                let mut to_remove = Vec::<VarId>::new();
                for node in orig_new.iter_depth() {
                    if let GrNode::Symbol(Symbol::NT(rep_var)) = node.deref() {
                        if let Some(&orig_rep_id) = orig_rep_vars.get(&rep_var) {
                            to_remove.push(*rep_var);
                            orig_rep_nodes.push((node.index, orig_rep_id));
                            self.origin.add((*rep_var, self.get_tree(*rep_var).unwrap().get_root().unwrap()), (var, orig_rep_id));
                        }
                    }
                }
                for (orig_id, child_id) in orig_rep_nodes {
                    *orig_new.get_mut(orig_id) = gnode!(inst);
                    orig_new.attach_child(orig_id, child_id);
                }
                for var in to_remove {
                    orig_rep_vars.remove(&var);
                }
            }
            self.origin.set_tree(var, orig_new);
            self.set_tree(var, new);
        }
    }

    fn normalize_plus_or_star(
        &mut self, rep_child: usize, orig_rep: usize, orig_rep_child: usize,
        new: &mut GrTree, orig_new: &mut GrTree, var: VarId, new_var: &mut VarId, is_plus: bool,
        exclude_nt: &mut HashSet<VarId>
    ) -> (usize, VarId)
    {
        const VERBOSE: bool = false;
        const OPTIMIZE_SUB_OR: bool = false;
        self.symbol_table.as_ref().map(|st| assert_eq!(st.get_num_nt(), self.trees.len(), "number of nt in symbol table doesn't match num_nt"));
        let (mut qvar, mut rvar) = (*new_var, *new_var + 1);
        let mut qtree = GrTree::new();
        let mut rtree = GrTree::new();
        let mut use_rtree = false;

        // finds possible occurences of <L=var>, detects conflicts, and updates qvar/rvar if necessary
        let mut lform_nt = None;
        for node in orig_new.iter_depth_at(orig_rep_child) {
            if let GrNode::LForm(v) = *node {
                if matches!(lform_nt, Some(v2) if v != v2) {
                    let symtab = self.get_symbol_table();
                    self.log.add_error(
                        format!("in {}, {}: conflicting <L={}> and <L={}>",
                                Symbol::NT(var).to_str(symtab),
                                grtree_to_str(orig_new, Some(orig_rep), None, Some(var), symtab, false),
                                Symbol::NT(lform_nt.unwrap()).to_str(symtab), Symbol::NT(v).to_str(symtab)));
                } else {
                    lform_nt = Some(v);
                    (qvar, rvar) = (v, *new_var);
                }
            }
        }

        // See comments in `normalize_var` near the calls to this method for details about the operations below.
        // We copy from the origin tree `orig_new` to trace the new node IDs to the original ones.
        match orig_new.get(orig_rep_child) {
            GrNode::Symbol(s) => {
                if VERBOSE { print!("({rep_child}:{s}) "); }
                // note: we cannot use the child id in qtree!
                let or = qtree.add_root(gnode!(|));
                let cc = qtree.add(Some(or), gnode!(&));
                let child = qtree.add(Some(cc), GrNode::Symbol(s.clone()));
                qtree.add(Some(cc), gnode!(nt qvar));
                let child2 = qtree.add(Some(or), if is_plus { GrNode::Symbol(s.clone()) } else { gnode!(e) });
                self.origin.add((qvar, child), (var, orig_rep_child));     // useful?
                self.origin.add((qvar, cc), (var, orig_rep_child));
                if is_plus {
                    self.origin.add((qvar, child2), (var, orig_rep_child));
                }
            }
            GrNode::Concat => {
                let id_grchildren = new.children(rep_child);
                if VERBOSE { print!("({rep_child}:&({})) ", id_grchildren.iter().join(", ")); }
                let or = qtree.add_root(gnode!(|));
                let cc1 = qtree.add_from_tree_callback(Some(or), orig_new, Some(orig_rep_child), |to, from, _n| {
                    self.origin.add((qvar, to), (var, from))
                });
                let loop_id = qtree.add(Some(cc1), gnode!(nt qvar));
                self.origin.add((qvar, loop_id), (var, orig_rep));
                if is_plus {
                    let loop_id2 = qtree.add_from_tree(Some(or), &new, Some(rep_child));
                    self.origin.add((qvar, loop_id2), (var, orig_rep_child));
                } else {
                    qtree.add(Some(or), gnode!(e));
                }
            }
            #[allow(unreachable_patterns)]
            GrNode::Or => if !OPTIMIZE_SUB_OR {
                let id_grchildren = new.children(rep_child);
                if VERBOSE { print!("({rep_child}:|({})) ", id_grchildren.iter().join(", ")); }
                let orig_id_grchildren = orig_new.children(orig_rep_child);
                let or = qtree.add_root(gnode!(|));
                for orig_id_grchild in orig_id_grchildren {
                    let orig_grchild = orig_new.get(*orig_id_grchild);
                    match orig_grchild {
                        GrNode::Symbol(s) => {
                            let cc = qtree.add(Some(or), gnode!(&));
                            let child = qtree.add_iter(Some(cc), [GrNode::Symbol(s.clone()), gnode!(nt qvar)])[0];
                            self.origin.add((qvar, cc), (var, *orig_id_grchild));
                            self.origin.add((qvar, child), (var, *orig_id_grchild));
                            if is_plus {
                                let plus_or = qtree.add(Some(or), GrNode::Symbol(s.clone()));
                                self.origin.add((qvar, plus_or), (var, *orig_id_grchild));
                            }
                        }
                        GrNode::Concat => {
                            let cc = qtree.add_from_tree_callback(Some(or), orig_new, Some(*orig_id_grchild), |to, from, _n| {
                                self.origin.add((qvar, to), (var, from));
                            });
                            qtree.add(Some(cc), gnode!(nt qvar));
                            if is_plus {
                                qtree.add_from_tree_callback(Some(or), &orig_new, Some(*orig_id_grchild), |to, from, _| {
                                    self.origin.add((qvar, to), (var, from));
                                });
                            }
                        }
                        x => panic!("unexpected node type under a | node: {x}"),
                    }
                }
                if !is_plus {
                    qtree.add(Some(or), gnode!(e));
                }
            }
            // TODO: remove this optimization?
            #[allow(unreachable_patterns)]
            GrNode::Or => if OPTIMIZE_SUB_OR {
                // P -> αβ*γ becomes P -> αQγ         P -> α(β)+γ becomes P -> αQγ
                //                   Q -> βQ | ε                          Q -> βR
                // β can be β1|β2|..., which is distributed               R -> Q | ε
                //
                // self             new(Q=new_var)                   simpler format
                // ---------------------------------------------------------------------------------
                // *(|(&(A,B),C))   |(&(A,B,Q),&(C,Q'),ε)            Q -> ABQ|CQ | ε
                // +(|(&(A,B),C))   |(&(A,B,Q),&(C,Q'),&(A',B'),C')  Q -> ABQ|CQ | AB|C
                //
                // new method:
                // *(|(&(A,B),C))   |(&(A,B,Q),&(C,Q'),ε)            Q -> ABQ|CQ | ε
                // +(|(&(A,B),C))   |(&(A,B,Q),&(C,Q'))              Q -> ABR|CR         R -> Q | ε
                let id_grchildren = new.children(rep_child);
                if VERBOSE { print!("({rep_child}:|({})) ", id_grchildren.iter().join(", ")); }
                let orig_id_grchildren = orig_new.children(orig_rep_child);
                let or = qtree.add_root(gnode!(|));
                for orig_id_grchild in orig_id_grchildren {
                    let orig_grchild = new.get(*orig_id_grchild);
                    match orig_grchild {
                        GrNode::Symbol(s) => {
                            if is_plus {
                                qtree.addc_iter(Some(or), gnode!(&), [GrNode::Symbol(s.clone()), gnode!(nt rvar)]);
                                use_rtree = true;
                            } else {
                                qtree.addc_iter(Some(or), gnode!(&), [GrNode::Symbol(s.clone()), gnode!(nt qvar)]);
                            }
                        }
                        GrNode::Concat => {
                            let cc = qtree.add_from_tree_callback(Some(or), orig_new, Some(*orig_id_grchild), |to, from, _n| {
                                self.origin.add((qvar, to), (var, from));
                            });
                            if is_plus {
                                qtree.add(Some(cc), gnode!(nt rvar));
                            } else {
                                qtree.add(Some(cc), gnode!(nt qvar));
                            }
                        }
                        x => panic!("unexpected node type under a | node: {x}"),
                    }
                }
                if use_rtree {
                    let or1 = rtree.add_root(gnode!(|));
                    rtree.add_iter(Some(or1), [gnode!(nt qvar), gnode!(e)]);
                } else if !is_plus {
                    qtree.add(Some(or), gnode!(e));
                }
            }
            _ => panic!("Unexpected node type under a + node: {}", new.get(rep_child))
        }
        if let Some(v) = lform_nt {
            // `new_var` replaces `v`
            if v == var {
                self.log.add_error(
                    format!("in {}, {}: <L> points to the same nonterminal. It must be a new one, created for the loop.",
                            Symbol::NT(var).to_str(self.get_symbol_table()),
                            grtree_to_str(orig_new, Some(orig_rep), None, Some(var), self.get_symbol_table(), false)));
            } else {
                // self.nt_conversion.insert(v, MovedTo(qvar));
                for mut node in qtree.iter_depth_simple_mut() {
                    if let GrNode::LForm(v2) = node.deref_mut() {
                        if *v2 == v { *v2 = qvar; }
                    }
                }
                for mut node in orig_new.iter_depth_simple_at_mut(orig_rep_child) {
                    if let GrNode::LForm(v2) = node.deref_mut() {
                        if *v2 == v { *v2 = qvar; }
                    }
                }
            }
        }
        self.symbol_table.as_mut().map(|st| {
            if let Some(_v) = lform_nt {
                // let name = st.remove_nt_name(v);
                // if VERBOSE {
                //     println!("L-FORM({v}) found, using name of NT({v}) = '{name}' for new NT({new_var})");
                // }
                // assert_eq!(st.add_nonterminal(name), qvar);
            } else {
                assert_eq!(st.add_child_nonterminal(var), qvar);
            }
            if use_rtree {
                assert_eq!(st.add_child_nonterminal(var), rvar);
            }
        });
        let id = new.add(None, gnode!(nt qvar));
        assert!(qvar as usize >= self.trees.len() || self.trees[qvar as usize].is_empty(), "overwriting tree {new_var}");
        if VERBOSE { println!("qtree: NT[{qvar}] {} -> {}", Symbol::NT(qvar).to_str(self.get_symbol_table()), grtree_to_str(&qtree, None, None, Some(qvar), self.get_symbol_table(), false) /*qtree.to_str(None, self.get_symbol_table())*/); }
        self.set_tree(qvar, qtree);
        exclude_nt.insert(qvar);
        self.flags.resize(rvar as usize, 0);
        self.parent.resize(rvar as usize, None);
        let plus_flag = if is_plus { ruleflag::REPEAT_PLUS } else { 0 };
        self.flags[qvar as usize] = ruleflag::CHILD_REPEAT | plus_flag;
        self.flags[var as usize] |= ruleflag::PARENT_REPEAT | plus_flag;
        self.parent[qvar as usize] = Some(var);
        if use_rtree {
            if VERBOSE { println!("rtree: NT[{rvar}] {} -> {}", Symbol::NT(rvar).to_str(self.get_symbol_table()), grtree_to_str(&rtree, None, None, Some(rvar), self.get_symbol_table(), false)); }
            self.set_tree(rvar, rtree);
            exclude_nt.insert(var);
            self.flags.resize(rvar as usize + 1, 0);
            self.parent.resize(rvar as usize + 1, None);
            self.flags[rvar as usize] |= ruleflag::CHILD_L_FACT;
            self.parent[rvar as usize] = Some(qvar);
            self.flags[qvar as usize] |= ruleflag::PARENT_L_FACTOR;
        }
        if VERBOSE {
            println!("=> new sizes, flags = {}, parent = {}, trees = {} (new_var = {new_var})", self.flags.len(), self.parent.len(), self.trees.len());
            println!("=> {}: parent {}, child {}{}",
                     if is_plus { "+" } else { "*" },
                     Symbol::NT(var).to_str(self.get_symbol_table()),
                     Symbol::NT(qvar).to_str(self.get_symbol_table()),
                     if use_rtree { format!(", child {}", Symbol::NT(rvar).to_str(self.get_symbol_table())) } else { String::new() }
            );
        }
        // We rectify the parent/child relationship in case of cascaded + or *. Since we perform a
        // bottom-up reconstruction, a rule like A -> ( ( a )+ b )+ will yield
        //   A -> A_2
        //   A_1 -> a A_1 | ε       parent: A
        //   A_2 -> A_1 b A_2 | ε   parent: A
        // We want A_1's parent to be A_2. We keep the wrong order in the parent chain: A_1 -> A_2 -> A,
        // which is unfortunate in some later tests but still easier than changing everything here.
        let mut rectify_maybe = None;
        for node in self.get_tree(qvar).unwrap().iter_depth_simple() {
            if let GrNode::Symbol(Symbol::NT(child)) = node.deref() {
                if *child != qvar && self.flags[*child as usize] & ruleflag::CHILD_REPEAT != 0 {
                    rectify_maybe = Some(*child);
                    break;
                }
            }
        }
        if let Some(child) = rectify_maybe {
            self.parent[child as usize] = Some(qvar);
            self.flags[qvar as usize] |= ruleflag::PARENT_REPEAT;
            if VERBOSE {
                println!("=> rectify {}'s parent as {}",
                         Symbol::NT(child).to_str(self.get_symbol_table()),
                         Symbol::NT(qvar).to_str(self.get_symbol_table()));
            }
        }
        *new_var = self.get_next_available_var();
        (id, qvar)
    }
}

impl BuildFrom<RuleTreeSet<General>> for RuleTreeSet<Normalized> {
    /// Transforms a `General` ruleset to a `Normalized` ruleset
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn build_from(mut rules: RuleTreeSet<General>) -> Self {
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
            origin: rules.origin,
            log: rules.log,
            _phantom: PhantomData
        }
    }
}

// impl BuildFrom<RuleTreeSet<Normalized>> for RuleTreeSet<General> {
//     /// Transforms a `Normalized` ruleset to a `General` ruleset
//     fn build_from(mut rules: RuleTreeSet<Normalized>) -> Self {
//         RuleTreeSet::<General> { trees: rules.trees, next_var: rules.next_var, _phantom: PhantomData }
//     }
// }

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates a `GrNode` instance.
    ///
    /// # Examples
    /// ```
    /// # use lexigram_lib::{ VarId, gnode, parser::Symbol};
    /// # use lexigram_lib::grammar::GrNode;
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
    #[macro_export]
    macro_rules! gnode {
        ([$id:expr]) => { gnode!(t $id) };
        (t $id:expr) => { $crate::grammar::GrNode::Symbol($crate::parser::Symbol::T($id as $crate::TokenId)) };
        (nt $id:expr) => { $crate::grammar::GrNode::Symbol($crate::parser::Symbol::NT($id as $crate::VarId)) };
        (e) => { $crate::grammar::GrNode::Symbol($crate::parser::Symbol::Empty) };
        (end) => { $crate::grammar::GrNode::Symbol($crate::parser::Symbol::End) };
        //
        (&) => { $crate::grammar::GrNode::Concat };
        (|) => { $crate::grammar::GrNode::Or };
        (?) => { $crate::grammar::GrNode::Maybe };
        (+) => { $crate::grammar::GrNode::Plus };
        (*) => { $crate::grammar::GrNode::Star };
        (L $id:expr) => { $crate::grammar::GrNode::LForm($id) };
        (R) => { $crate::grammar::GrNode::RAssoc };
        (P) => { $crate::grammar::GrNode::PrecEq };
        (inst) => { $crate::grammar::GrNode::Instance };
    }

    /// Generates a `Symbol` instance.
    ///
    /// # Examples
    /// ```
    /// # use lexigram_lib::{TokenId, VarId, sym};
    /// # use lexigram_lib::parser::Symbol;
    /// assert_eq!(sym!(t 2), Symbol::T(2 as TokenId));
    /// assert_eq!(sym!(nt 3), Symbol::NT(3 as VarId));
    /// assert_eq!(sym!(e), Symbol::Empty);
    /// assert_eq!(sym!(end), Symbol::End);
    #[macro_export]
    macro_rules! sym {
        (t $id:expr) => { $crate::parser::Symbol::T($id as $crate::TokenId) };
        (nt $id:expr) => { $crate::parser::Symbol::NT($id as $crate::VarId) };
        (e) => { $crate::parser::Symbol::Empty };
        (end) => { $crate::parser::Symbol::End };
    }

    #[macro_export]
    macro_rules! altflag {
        (L) => { $crate::grammar::ruleflag::L_FORM };
        (R) => { $crate::grammar::ruleflag::R_ASSOC };
        (G) => { $crate::grammar::ruleflag::GREEDY };
        (P) => { $crate::grammar::ruleflag::PREC_EQ };
        ($f:expr) => { $f };
    }

    /// Generates a production rule alternative. An alternative is made up of symbols separated by a comma.
    /// Each symbol is either
    /// - a non-terminal: `nt` {integer}
    /// - a terminal: `t` {integer}
    /// - the empty symbol: `e`
    ///
    /// Preceding an alternative with `# {integer}` sets a flag value on that alternative. The values are:
    /// - 128: L-form (low-latency parsing of that alternative)
    /// - 256: R-assoc (right-associative - by default, ambiguous alternatives like 'E * E' are left-associative)
    ///
    /// # Example
    /// ```
    /// # use lexigram_core::TokenId;
    /// # use lexigram_core::alt::Alternative;
    /// # use lexigram_lib::{alt, sym};
    /// assert_eq!(alt!(nt 1, t 2, e), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    /// assert_eq!(alt!(#128, nt 1, t 2, e), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    /// assert_eq!(alt!(#L, nt 1, t 2, e), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    /// let x = 256;
    /// let o_id = 4;
    /// assert_eq!(alt!(#(x, o_id), nt 0, t 1, e), Alternative::new(vec![sym!(nt 0), sym!(t 1), sym!(e)]).with_flags(256).with_ambig_alt_id(4));
    /// ```
    #[macro_export]
    macro_rules! alt {
        () => { $crate::grammar::alt::Alternative::new(std::vec![]) };
        ($($a:ident $($b:expr)?,)+) => { alt!($($a $($b)?),+) };
        ($($a:ident $($b:expr)?),*) => { $crate::grammar::alt::Alternative::new(std::vec![$($crate::sym!($a $($b)?)),*]) };
        (#$f:literal, $($a:ident $($b:expr)?,)+) => { alt!(#$f, $($a $($b)?),+) };
        (#$f:literal, $($a:ident $($b:expr)?),*) => { $crate::grammar::alt::Alternative::new(std::vec![$($crate::sym!($a $($b)?)),*]).with_flags($f) };
        // (#$f:ident, $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?,)+) => { alt!(#$f, $(%($v, $id),)? $($a $($b)?),+) };
        // (#$f:ident, $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*)
        //     => { $crate::grammar::alt::Alternative::new(std::vec![$($crate::sym!($a $($b)?)),*]).with_flags($crate::altflag!($f))$(.with_orig($v, $id))? };
        ($(#$f:ident,)? $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?,)+) => { alt!($(#$f,)? $(%($v, $id),)? $($a $($b)?),+) };
        ($(#$f:ident,)? $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*)
            => { $crate::grammar::alt::Alternative::new(std::vec![$($crate::sym!($a $($b)?)),*])$(.with_flags($crate::altflag!($f)))?$(.with_origin($v, $id))? };
        // TODO: change "#" parts below
        (#($f:expr, $o:expr), $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?,)+)
            => { alt!(#($f, $o), $(%($v, $id),)? $($a $($b)?),+) };
        (#($f:expr, $o:expr), $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*)
            => { $crate::grammar::alt::Alternative::new(std::vec![$($crate::sym!($a $($b)?)),*]).with_flags($crate::altflag!($f)).with_ambig_alt_id($o)$(.with_origin($v, $id))? };
        (%($v:expr, $id:expr), $($a:ident $($b:expr)?,)+)
            => { alt!(%($v, $id), $($a $($b)?),+) };
        (%($v:expr, $id:expr), $($a:ident $($b:expr)?),*)
            => { $crate::grammar::alt::Alternative::new(std::vec![$($crate::sym!($a $($b)?)),*]).with_flags($crate::altflag!($f)).with_origin($v, $id) };
    }

    #[macro_export]
    macro_rules! symbols {
        () => { std::vec![] };
        ($($a:ident $($b:literal $(: $num:expr)?)?,)+) => { symbols![$($a $($b $(: $num)?)?),+] };
        ($($a:ident $($b:literal $(: $num:expr)?)?),*) => { std::vec![$($crate::sym!($a $($b $(: $num)?)?)),*] };
    }

    /// Generates a production rule. It is made up of alternatives separated by a semicolon.
    ///
    /// Example
    /// ```
    /// # use lexigram_lib::{TokenId, VarId, parser::Symbol};
    /// # use lexigram_core::alt::Alternative;
    /// # use lexigram_lib::{prule, alt, sym};
    /// assert_eq!(prule!(nt 1, t 2, nt 1, t 3; nt 2; e),
    ///            vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 Alternative::new(vec![sym!(nt  2)]),
    ///                 Alternative::new(vec![sym!(e)])]);
    /// assert_eq!(prule!(nt 1, t 2, nt 1, t 3; #128, nt 2; e),
    ///            vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 Alternative::new(vec![sym!(nt  2)]).with_flags(128),
    ///                 Alternative::new(vec![sym!(e)])]);
    /// assert_eq!(prule!(nt 1, t 2, nt 1, t 3; #L, nt 2; e),
    ///            vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
    ///                 Alternative::new(vec![sym!(nt  2)]).with_flags(128),
    ///                 Alternative::new(vec![sym!(e)])]);
    /// ```
    #[macro_export]
    macro_rules! prule {
        () => { std::vec![] };
        ($($(#$f:literal,)? $($a:ident $($b:expr)?),*;)+) => { prule![$($(#$f,)? $($a $($b)?),+);+] };
        ($($(#$f:literal,)? $($a:ident $($b:expr)?),*);*) => { std::vec![$($crate::alt![$(#$f,)? $($a $($b)?),+]),*]};
        ($($(#$f:ident,)? $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*;)+) => { prule![$($(#$f,)? $(%($v, $id),)? $($a $($b)?),+);+] };
        ($($(#$f:ident,)? $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*);*) => { std::vec![$($crate::alt![$(#$f,)? $(%($v, $id),)? $($a $($b)?),+]),*]};
        // TODO: change "#" part below
        ($($(#($f:expr, $o:expr),)? $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*;)+) => { prule![$($(#($f, $o),)? $(%($v, $id),)? $($a $($b)?),+);+] };
        ($($(#($f:expr, $o:expr),)? $(%($v:expr, $id:expr),)? $($a:ident $($b:expr)?),*);*) => { std::vec![$($crate::alt![$(#($f,$o),)? $(%($v, $id),)? $($a $($b)?),+]),*]};
    }
}
