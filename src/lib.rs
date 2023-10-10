// Copyright 2023 Redglyph

#![allow(unused_imports)]
#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::ops::DerefMut;

mod tests;

/// Regular expressions / DFA, See:
/// - https://blog.burntsushi.net/regex-internals/
/// - https://en.wikipedia.org/wiki/Tagged_Deterministic_Finite_Automaton
/// - https://arxiv.org/abs/2206.01398
///
/// See also:
/// - Ref: https://en.wikipedia.org/wiki/Comparison_of_parser_generators

#[derive(Clone, Debug, PartialEq)]
pub enum ReNode {
    Char(char),
    Concat(Vec<ReNodeId>),
    Star(Box<ReNodeId>),
    Or(Vec<ReNodeId>)
}

impl Display for ReNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReNode::Char(c) => write!(f, "'{c}'"),
            ReNode::Concat(_) => write!(f, "&"),
            ReNode::Star(_) => write!(f, "*"),
            ReNode::Or(_) => write!(f,"|")
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReNodeId {
    id: RefCell<u32>,
    op: ReNode
}

impl ReNodeId {

    pub fn new(node: ReNode) -> ReNodeId {
        ReNodeId { id: RefCell::new(0), op: node }
    }

    pub fn char(c: char) -> ReNodeId {
        ReNodeId::new(ReNode::Char(c))
    }

    pub fn str(s: &str) -> ReNodeId {
        ReNodeId::new(ReNode::Concat(s.chars().map(|c| ReNodeId::char(c)).collect::<Vec<_>>()))
    }

    pub fn concat<T>(items: T) -> ReNodeId where T: IntoIterator<Item = ReNodeId> {
        ReNodeId::new(ReNode::Concat(items.into_iter().collect()))
    }

    pub fn star(node: ReNodeId) -> ReNodeId {
        ReNodeId::new(ReNode::Star(Box::new(node)))
    }

    pub fn or<T>(items: T) -> ReNodeId where T: IntoIterator<Item = ReNodeId> {
        ReNodeId::new(ReNode::Or(items.into_iter().collect()))
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self.op, ReNode::Char(_))
    }

    pub fn print(&self) {
        self.print_level(0);
    }

    pub fn print_level(&self, level: usize) {
        print!("{: >indent$}{} ", "", format!("[{:2}]", self.id.borrow()), indent = 4 * level);
        match &self.op {
            ReNode::Char(c) => {
                println!("'{c}'")
            }
            ReNode::Concat(v) | ReNode::Or(v) => {
                println!("{}", self.op.to_string());
                for n in v {
                    n.print_level(level + 1);
                }
            }
            ReNode::Star(b) => {
                println!("*");
                b.print_level(level + 1);
            }
        }
    }
}

// impl Display for ReNodeId {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         self.op.fmt(f)
//     }
// }

impl PartialEq for ReNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op
    }
}

enum TreeNode<T> {
    New(T),
    Visited(T)
}

// impl<T: Display> Display for TreeNode<&T> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             TreeNode::New(x) => write!(f, "N({x})"),
//             TreeNode::Visited(x) => write!(f, "V({x})")
//         }
//     }
// }

pub struct DfaBuilder {
    re: ReNodeId,

}

impl DfaBuilder {
    pub fn new(re: ReNodeId) -> DfaBuilder {
        let mut builder = DfaBuilder { re };
        builder.set_ids_bottom_up();
        builder
    }

    fn set_ids_top_down(&mut self) {
        let mut id = 1;
        let mut stack = VecDeque::<&ReNodeId>::new();
        let mut next_node = Some(&self.re);
        while next_node.is_some() {
            let node = next_node.unwrap();
            *node.id.borrow_mut() = id;
            id += 1;
            match &node.op {
                ReNode::Char(_) => {}
                ReNode::Concat(v) => {
                    stack.extend(v)
                }
                ReNode::Star(n) => {
                    stack.push_back(n)
                }
                ReNode::Or(v) => {
                    stack.extend(v)
                }
            }
            next_node = stack.pop_front();
        }
    }

    fn set_ids_bottom_up(&mut self) {
        let mut id = 1;
        let mut stack = Vec::<TreeNode<&ReNodeId>>::new();
        let mut next_node = Some(TreeNode::New(&self.re));
        while next_node.is_some() {
            // println!("node: {:8}  stack: {}",
            //     next_node.as_ref().unwrap().to_string(),
            //     stack.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ")
            // );
            let treenode = next_node.unwrap();
            match treenode {
                TreeNode::New(node) => {
                    match &node.op {
                        ReNode::Char(_) => {
                            *node.id.borrow_mut() = id;
                            id += 1;
                        }
                        ReNode::Concat(v) | ReNode::Or(v) => {
                            stack.push(TreeNode::Visited(node));
                            stack.extend(v.iter().map(|n| TreeNode::New(n)));
                        }
                        ReNode::Star(n) => {
                            stack.push(TreeNode::Visited(node));
                            stack.push(TreeNode::New(n))
                        }
                    }
                }
                TreeNode::Visited(node) => {
                    *node.id.borrow_mut() = id;
                    id += 1;
                }
            }
            next_node = stack.pop();
        }
    }

    pub fn build_dfa(&mut self) {
        self.calc_nullable();
        todo!()
    }

    fn calc_nullable(&mut self) {

    }

}
