#![allow(dead_code)]
#![allow(unused)]

mod tests;

use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::cproduct::CProduct;
use crate::dfa::TokenId;
use crate::gnode;
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
#[derive(Clone, Copy, Debug)]
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
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct RuleTree(VecTree<GrNode>);

impl RuleTree {
    fn new() -> Self {
        RuleTree(VecTree::new())
    }

    fn get_dup(&mut self, dup_index: &mut Dup) -> usize {
        match dup_index.get() {
            DupVal::Original(index) => index as usize,
            DupVal::Copy(index) => {
                let node = self.0.get(index as usize).clone();
                self.0.add(None, node)
            }
        }
    }

    /// Transforms the production rule tree into a list of rules in normalized format:
    /// `var -> &(leaf_1, leaf_2, ...leaf_n)`
    ///
    /// The product may have to be split if operators like `+` or `*` are used. In this
    /// case, new non-terminals are created, with increasing IDs starting from
    /// `next_var_id`.
    fn normalize(self, var_id: VarId, next_var_id: VarId) -> Vec<(VarId, Self)> {
        let mut new = RuleTree::new();
        let mut rules = Vec::<(VarId, RuleTree)>::new();
        let mut stack = Vec::<usize>::new();                // indices in new
        for sym in self.0.iter_depth() {
            let n = sym.num_children();
            if n == 0 {
                stack.push(new.0.add(None, self.0.get(sym.index).clone()));
            } else {
                match sym.deref() {
                    GrNode::Concat | GrNode::Or => {
                        // we must rearrange the operations so that any item on the stack is only
                        // one of those combinations:
                        // - a leaf
                        // - a &(leaves)
                        // - a |(&(leaves) or leaves)
                        let children = stack.drain(stack.len() - n..).to_vec();
                        if children.iter().all(|&idx| !matches!(new.0.get(idx), GrNode::Concat|GrNode::Or)) {
                            // trivial case (could be removed and treated as a general case)
                            new.0.addc_iter(None, sym.clone(), children.into_iter().map(|i| self.0.get(i).clone()));
                        } else {
                            if let GrNode::Or = sym.deref() {
                                // if parent sym is p:|
                                // - preserving the children's order:
                                //   - attach '|' children's children directly under p (discarding the '|' children)
                                //   - attach '&' children under p
                                // - push p back to stack
                                // ex: P: AB | (C|D) | E | (F|G)      -> P: AB | C | D | E | F | G
                                //        |(&(A,B),|(C,D),E,|(F,G))         |(&(A,B),C,D,E,F,G)
                                let mut new_children = Vec::new();
                                for id in children {
                                    match new.0.get(id) {
                                        GrNode::Symbol(_) | GrNode::Concat =>
                                            new_children.push(id),
                                        GrNode::Or =>
                                            new_children.extend(new.0.children(id)),
                                        x => panic!("unexpected node type under | node: {x:?}"),
                                    }
                                }
                                new.0.addci_iter(None, gnode!(|), new_children);
                            } else { // GrNode::Concat
                                // if parent sym is p:&
                                // - merge adjacent leaves and '&' children (optional)
                                // - distribute cross-product of all '|' children and '&' children,
                                //       adding new '&' nodes for each product
                                // - add r:'|' node to tree, attaching the new '&' nodes under it
                                // - push r to stack
                                // ex: P: AB & (C|D) & E & (F|G)      -> P: ABCEF | ABCEG | ABDEF | ABDEG
                                //        &(&(A,B),|(C,D),E,|(F,G))         |(&(A,B,C,E,F),&(A,B,C,E,G),&(A,B,D,E,F),&(A,B,D,E,G)
                                let mut index_or = Vec::new();
                                let mut dup_children = children.iter().map(|&id| {
                                    if matches!(new.0.get(id), GrNode::Or) {
                                        index_or.push(id);
                                    }
                                    Dup::new(id)
                                }).collect::<Vec<_>>();


                            }
                            todo!()
                        }
                    }
                    GrNode::Maybe => {
                        assert_eq!(n, 1);
                        let empty = new.0.add(None, gnode!(e));
                        let id = new.0.addci_iter(None, gnode!(|), [stack.pop().unwrap(), empty]);
                        stack.push(id);
                    }
                    GrNode::Plus => {
                        assert_eq!(n, 1);
                        // create new production rule:
                        // P -> αβ+γ becomes P -> αQγ
                        //                   Q -> βQ | β
                        todo!()
                    }
                    GrNode::Star => {
                        assert_eq!(n, 1);
                        // create new production rule:
                        // P -> αβ*γ becomes P -> αQγ
                        //                   Q -> βQ | ε
                        todo!()
                    }
                    _ => panic!("Unexpected {}", sym.deref())
                }
            }
        }
        rules.push((var_id, new));
        rules
    }
}

// ---------------------------------------------------------------------------------------------

pub struct GrammarBuilder {
    rules: Vec<RuleTree>,
    symbols: Vec<(String, Option<String>)>
}

impl GrammarBuilder {
    pub fn new() -> Self {
        GrammarBuilder {
            rules: Vec::new(),
            symbols: Vec::new()
        }
    }

    pub fn from(parsed_rules: Vec<RuleTree>) -> Self {
        GrammarBuilder {
            rules: parsed_rules,
            symbols: Vec::new()
        }
    }

    fn remove_repetitions(&mut self) {

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
