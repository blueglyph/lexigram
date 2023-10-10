// Copyright 2023 Redglyph

#![allow(unused_imports)]
#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use crate::vectree::VecTree;

mod tests;
mod vectree;

/// Regular expressions / DFA, See:
/// - https://blog.burntsushi.net/regex-internals/
/// - https://en.wikipedia.org/wiki/Tagged_Deterministic_Finite_Automaton
/// - https://arxiv.org/abs/2206.01398
///
/// See also:
/// - Ref: https://en.wikipedia.org/wiki/Comparison_of_parser_generators

#[derive(Clone, Debug, PartialEq)]
pub enum ReType {
    Empty,
    End,
    Char(char),
    String(String),
    Concat,
    Star,
    Or
}

impl ReType {
    pub fn is_empty(&self) -> bool {
        matches!(self, ReType::Empty)
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, ReType::Empty | ReType::End | ReType::Char(_) | ReType::String(_))
    }
}

impl Display for ReType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReType::Empty => write!(f, "-"),
            ReType::End => write!(f, "<end>"),
            ReType::Char(c) => write!(f, "'{c}'"),
            ReType::String(s) => write!(f, "'{s}'"),
            ReType::Concat => write!(f, "&"),
            ReType::Star => write!(f, "*"),
            ReType::Or => write!(f, "|")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReNode {
    id: Option<usize>,
    op: ReType,
    firstpos: Vec<usize>,
    lastpos: Vec<usize>,
    nullable: Option<bool>
}

impl ReNode {
    pub fn new(node: ReType) -> ReNode {
        ReNode { id: None, op: node, firstpos: Vec::new(), lastpos: Vec::new(), nullable: None }
    }

    pub fn is_leaf(&self) -> bool {
        self.op.is_leaf()
    }

    pub fn is_empty(&self) -> bool {
        self.op.is_empty()
    }
}

impl Display for ReNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.op.fmt(f)
    }
}

// ---------------------------------------------------------------------------------------------

pub struct DfaBuilder {
    re: VecTree<ReNode>,
    followpos: HashMap<usize, HashSet<usize>>,
}

impl DfaBuilder {
    pub fn new(re: VecTree<ReNode>) -> DfaBuilder {
        let builder = DfaBuilder {
            re,
            followpos: HashMap::new()
        };
        builder
    }

    // fn calc_edges(&mut self) {
    //     let mut id = 0;
    //
    // }
    //
    // pub fn build_dfa(&mut self) {
    //     self.calc_edges();
    // }
    //
    // pub fn print(&self) {
    //     self.re.print();
    //     self.print_tables();
    // }
    //
    // fn print_tables(&self) {
    //     println!("nullable: {}", self.nullable.iter().enumerate().skip(1).map(|n| format!("{}={}", n.0, n.1)).collect::<Vec<_>>().join(", "));
    // }
}
