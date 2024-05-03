#![allow(dead_code)]
#![allow(unused)]

mod tests;

use std::fmt::{Display, Formatter};
use crate::dfa::TokenId;
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

#[derive(Clone, Debug)]
pub struct RuleTree(VecTree<GrNode>);

impl RuleTree {
    fn new() -> Self {
        RuleTree(VecTree::new())
    }

    fn remove_repetitions(self) -> Self {
        let new = RuleTree::new();
        todo!()
        new
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
