// Copyright 2023 Redglyph

#![allow(unused_imports)]
#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use crate::vectree::VecTree;
use crate::take_until::TakeUntilIterator;

mod tests;
mod vectree;
mod take_until;
mod macros;

/// Regular expressions / DFA, See:
/// - https://blog.burntsushi.net/regex-internals/
/// - https://en.wikipedia.org/wiki/Tagged_Deterministic_Finite_Automaton
/// - https://arxiv.org/abs/2206.01398
///
/// See also:
/// - Ref: https://en.wikipedia.org/wiki/Comparison_of_parser_generators

#[derive(Clone, Debug, PartialEq, Default)]
pub enum ReType {
    #[default] Empty,
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

    pub fn is_nullable(&self) -> Option<bool> {
        match self {
            ReType::Empty | ReType::Star => Some(true),
            ReType::End | ReType::Char(_) | ReType::String(_) => Some(false),
            _ => None
        }
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

type Id = u32;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ReNode {
    id: Option<Id>,
    op: ReType,
    firstpos: HashSet<Id>,
    lastpos: HashSet<Id>,
    nullable: Option<bool>
}

impl ReNode {
    pub fn new(node: ReType) -> ReNode {
        ReNode { id: None, op: node, firstpos: HashSet::new(), lastpos: HashSet::new(), nullable: None }
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
        if let Some(id) = self.id {
            write!(f, "{id}:")?;
        }
        self.op.fmt(f)
    }
}

// ---------------------------------------------------------------------------------------------

pub struct DfaBuilder {
    re: VecTree<ReNode>,
    followpos: HashMap<Id, HashSet<Id>>,
}

impl DfaBuilder {
    pub fn new(re: VecTree<ReNode>) -> DfaBuilder {
        let builder = DfaBuilder {
            re,
            followpos: HashMap::new()
        };
        builder
    }

    fn calc_node(&mut self) {
        let mut id = 0;
        for inode in self.re.iter_depth_mut() {
            if inode.data.is_leaf() {
                id += 1;
                inode.data.id = Some(id);
                inode.data.firstpos.insert(id);
                inode.data.lastpos.insert(id);
            } else {
                match inode.data.op {
                    ReType::Concat => {
                        // firstpos = union of all firstpos until the first non-nullable child (included)
                        let mut firstpos = HashSet::<Id>::new();
                        for child in inode.iter_children_data().take_until(|&n| !n.nullable.unwrap()) {
                            firstpos.extend(&child.firstpos);
                        }
                        inode.data.firstpos.extend(firstpos);
                        // lastpos = union of all lastpos until the first non-nullable child (included), starting from the end
                        let mut lastpos = HashSet::<Id>::new();
                        for child in inode.iter_children_data().rev().take_until(|&n| !n.nullable.unwrap()) {
                            lastpos.extend(&child.lastpos);
                        }
                        inode.data.lastpos.extend(lastpos);
                        // followpos:
                        // for all pairs of consecutive children {c[i], c[i+1]},
                        //     for all j in c[i].lastpos
                        //         followpos[j].extend(c[i+1].firstpos)
                        let mut iter = inode.iter_children_data();
                        let mut a = iter.next().unwrap();   // a is c[i]
                        while let Some(b) = iter.next() {   // b is c[i+1]
                            for j in &a.lastpos {
                                if !self.followpos.contains_key(j) {
                                    self.followpos.insert(*j, HashSet::new());
                                }
                                self.followpos.get_mut(j).unwrap().extend(&b.firstpos);
                            }
                            a = b;
                        }
                    }
                    ReType::Star => {
                        // firstpos, lastpos identical to child's
                        let firstpos = inode.iter_children_data().next().unwrap().firstpos.iter().map(|&n| n).collect::<Vec<_>>();
                        inode.data.firstpos.extend(firstpos);
                        let lastpos = inode.iter_children_data().next().unwrap().lastpos.iter().map(|&n| n).collect::<Vec<_>>();
                        inode.data.lastpos.extend(lastpos);
                        // followpos:
                        // for all i in *.lastpos,
                        //     followpos[i].extend(*.firstpos)
                        for i in &inode.data.lastpos {
                            if !self.followpos.contains_key(i) {
                                self.followpos.insert(*i, HashSet::new());
                            }
                            self.followpos.get_mut(i).unwrap().extend(&inode.data.firstpos);
                        }
                    }
                    ReType::Or => {
                        // firstpos, lastpost = union of children's
                        let mut firstpos = HashSet::<Id>::new();
                        for child in inode.iter_children_data() {
                            firstpos.extend(&child.firstpos);
                        }
                        inode.data.firstpos.extend(firstpos);
                        let mut lastpos = HashSet::<Id>::new();
                        for child in inode.iter_children_data() {
                            lastpos.extend(&child.lastpos);
                        }
                        inode.data.lastpos.extend(lastpos);
                    }
                    _ => panic!("{:?}: no way to compute firstpos/...", inode.data)
                }
            }
            if let Some(nullable) = inode.data.op.is_nullable() {
                inode.data.nullable = Some(nullable);
            } else {
                inode.data.nullable = match &inode.data.op {
                    ReType::Concat => Some(inode.iter_children_data().all(|child| child.nullable.unwrap())),
                    ReType::Or => Some(inode.iter_children_data().any(|child| child.nullable.unwrap())),
                    op => panic!("{:?} should have a fixed nullable property", op)
                }
            }
        }
    }

    pub fn build_dfa(&mut self) {
        self.calc_node();
    }

    // pub fn print(&self) {
    //     self.re.print();
    //     self.print_tables();
    // }
    //
    // fn print_tables(&self) {
    //     println!("nullable: {}", self.nullable.iter().enumerate().skip(1).map(|n| format!("{}={}", n.0, n.1)).collect::<Vec<_>>().join(", "));
    // }
}
