// Copyright 2023 Redglyph

#![allow(unused_imports)]
#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
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

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
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

    pub fn is_end(&self) -> bool {
        matches!(self, ReType::End)
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

type StateId = u32;

pub struct DfaBuilder {
    /// Regular Expression tree
    re: VecTree<ReNode>,
    /// `followpos` table, containing the `Id` -> `Id` graph of `re`
    followpos: HashMap<Id, HashSet<Id>>,
    /// `Id` -> node index
    ids: HashMap<Id, usize>,
    states: BTreeMap<BTreeSet<Id>, StateId>,
    state_graph: BTreeMap<StateId, BTreeMap<ReType, StateId>>,
    initial_state: Option<StateId>,
    end_states: BTreeSet<StateId>
}

impl DfaBuilder {
    pub fn new(re: VecTree<ReNode>) -> DfaBuilder {
        let mut builder = DfaBuilder {
            re,
            followpos: HashMap::new(),
            ids: HashMap::new(),
            states: BTreeMap::<BTreeSet<Id>, StateId>::new(),
            state_graph: BTreeMap::new(),
            initial_state: None,
            end_states: BTreeSet::new()
        };
        builder.preprocess_re();
        builder
    }

    /// Replaces ReType::String(s) with a concatenation of ReType::Char(s[i])
    fn preprocess_re(&mut self) {
        let mut nodes = vec![];
        for mut inode in self.re.iter_depth_simple_mut() {
            if matches!(inode.op, ReType::String(_)) {
                // we have to do it again to move the string
                if let ReType::String(s) = std::mem::take(&mut inode.op) {
                    nodes.push((inode.index, s));
                }
            }
        }
        for (index, s) in nodes {
            let node = self.re.get_mut(index);
            match s.len() {
                0 => panic!("empty string item at index {index}"),
                1 => {
                    node.op = ReType::Char(s.chars().nth(0).unwrap());
                },
                _ => {
                    node.op = ReType::Concat;
                    for c in s.chars() {
                        self.re.add(Some(index), ReNode::new(ReType::Char(c)));
                    }
                }
            }
        }
    }

    /// Calculates `firstpos`, `lastpost`, `nullable` for each node, and the `followpos` table.
    fn calc_node_pos(&mut self) {
        let mut id = 0;
        for mut inode in self.re.iter_depth_mut() {
            if inode.is_leaf() {
                id += 1;
                inode.id = Some(id);
                inode.firstpos.insert(id);
                inode.lastpos.insert(id);
                self.ids.insert(id, inode.index);
                if inode.op.is_end() {
                    self.followpos.insert(id, HashSet::new());
                }
            } else {
                match inode.op {
                    ReType::Concat => {
                        // firstpos = union of all firstpos until the first non-nullable child (included)
                        let mut firstpos = HashSet::<Id>::new();
                        for child in inode.iter_children_simple().take_until(|&n| !n.nullable.unwrap()) {
                            firstpos.extend(&child.firstpos);
                        }
                        inode.firstpos.extend(firstpos);
                        // lastpos = union of all lastpos until the first non-nullable child (included), starting from the end
                        let mut lastpos = HashSet::<Id>::new();
                        for child in inode.iter_children_simple().rev().take_until(|&n| !n.nullable.unwrap()) {
                            lastpos.extend(&child.lastpos);
                        }
                        inode.lastpos.extend(lastpos);
                        // followpos:
                        // for all pairs of consecutive children {c[i], c[i+1]},
                        //     for all j in c[i].lastpos
                        //         followpos[j].extend(c[i+1].firstpos)
                        let mut iter = inode.iter_children_simple();
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
                        let firstpos = inode.iter_children_simple().next().unwrap().firstpos.iter().map(|&n| n).collect::<Vec<_>>();
                        inode.firstpos.extend(firstpos);
                        let lastpos = inode.iter_children_simple().next().unwrap().lastpos.iter().map(|&n| n).collect::<Vec<_>>();
                        inode.lastpos.extend(lastpos);
                        // followpos:
                        // for all i in *.lastpos,
                        //     followpos[i].extend(*.firstpos)
                        for i in &inode.lastpos {
                            if !self.followpos.contains_key(i) {
                                self.followpos.insert(*i, HashSet::new());
                            }
                            self.followpos.get_mut(i).unwrap().extend(&inode.firstpos);
                        }
                    }
                    ReType::Or => {
                        // firstpos, lastpost = union of children's
                        let mut firstpos = HashSet::<Id>::new();
                        for child in inode.iter_children_simple() {
                            firstpos.extend(&child.firstpos);   // todo: use BTreeSet instead (faster iter)?
                        }
                        inode.firstpos.extend(firstpos);
                        let mut lastpos = HashSet::<Id>::new();
                        for child in inode.iter_children_simple() {
                            lastpos.extend(&child.lastpos);
                        }
                        inode.lastpos.extend(lastpos);
                    }
                    _ => panic!("{:?}: no way to compute firstpos/...", &*inode)
                }
            }
            if let Some(nullable) = inode.op.is_nullable() {
                inode.nullable = Some(nullable);
            } else {
                inode.nullable = match &inode.op {
                    ReType::Concat => Some(inode.iter_children_simple().all(|child| child.nullable.unwrap())),
                    ReType::Or => Some(inode.iter_children_simple().any(|child| child.nullable.unwrap())),
                    op => panic!("{:?} should have a fixed nullable property", op)
                }
            }
        }
    }

    fn calc_states(&mut self) {
        // initial state from firstpos(top node)
        let mut current_id = 0;
        let key = BTreeSet::from_iter(self.re.get(0).firstpos.iter().map(|&id| id));
        let mut new_states = BTreeSet::<BTreeSet<Id>>::new();
        new_states.insert(key.clone());
        self.states.insert(key, current_id);
        self.initial_state = Some(current_id);

        // unfold all the states
        while let Some(s) = new_states.pop_first() {
            let new_state_id = self.states.get(&s).unwrap().clone();
            let mut trans = BTreeMap::<&ReType, BTreeSet<Id>>::new();
            for (symbol, id) in s.iter().map(|id| (&self.re.get(self.ids[id]).op, *id)) {
                if let Some(ids) = trans.get_mut(symbol) {
                    ids.insert(id);
                } else {
                    let mut ids = BTreeSet::new();
                    ids.insert(id);
                    trans.insert(symbol, ids);
                }
            }
            for (symbol, ids) in trans {
                let mut state = BTreeSet::new();
                for id in ids {
                    state.extend(&self.followpos[&id]);
                }
                let state_id = if let Some(state_id) = self.states.get(&state) {
                    *state_id
                } else {
                    new_states.insert(state.clone());
                    current_id += 1;
                    self.states.insert(state, current_id);
                    current_id
                };
                if let Some(map) = self.state_graph.get_mut(&new_state_id) {
                    map.insert(symbol.clone(), state_id);
                } else {
                    let mut map = BTreeMap::new();
                    map.insert(symbol.clone(), state_id);
                    self.state_graph.insert(new_state_id, map);
                }
                if symbol.is_end() {
                    self.end_states.insert(new_state_id);
                }
            }
        }
    }

    /// Optimizes the number of states from `self.state_graph`.
    ///
    /// # Arguments
    ///
    /// * `separate_end_states` = `true` if different end (accepting) states should be kept apart;
    /// for example, when it's important to differentiate tokens.
    pub fn optimize_graph(&mut self, separate_end_states: bool) {
        let mut groups = Vec::<BTreeSet<StateId>>::new();
        let mut st_to_group = BTreeMap::<StateId, usize>::new();
        // initial partition
        // - all non-end states
        let mut group = BTreeSet::<StateId>::new();
        for st in self.state_graph.keys().filter(|&st| !self.end_states.contains(st)) {
            group.insert(*st);
            st_to_group.insert(*st, 0);
        }
        groups.push(group);
        // - end states
        if separate_end_states {
            for st in &self.end_states {
                st_to_group.insert(*st, groups.len());
                groups.push(BTreeSet::<StateId>::from([*st]));
            }
        } else {
            st_to_group.extend(self.end_states.iter().map(|id| (*id, groups.len())));
            groups.push(self.end_states.clone());
        }
        // dbg!(&partition);
        // dbg!(&st_to_group);
        let mut change = true;
        let mut last_group_id = groups.len() - 1;
        while change {
            let mut changes = Vec::<(StateId, usize, usize)>::new();   // (state, old group, new group)
            for (id, p) in groups.iter().enumerate() {
                // do all states have the same destination group for the same symbol?
                println!("group #{id}: {p:?}:");
                // stores combination -> group index:
                let mut combinations = BTreeMap::<BTreeMap<&ReType, usize>, usize>::new();
                for &st_id in p {
                    let combination = self.state_graph.get(&st_id).unwrap().iter()
                        .filter(|(_, st)| st_to_group.contains_key(st)) // to avoid fake "end" states
                        .map(|(s, st)| { (s, *st_to_group.get(st).unwrap()) })
                        .collect::<BTreeMap<_, _>>();
                    print!("- state {st_id}: {combination:?}");
                    if combinations.is_empty() {
                        combinations.insert(combination, id);   // first one remains in this group
                        println!(" (1st, no change)");
                    } else {
                        if let Some(&group_id) = combinations.get(&combination) {
                            // programs the change if it's one of the new groups
                            if group_id != id {
                                changes.push((st_id, id, group_id));
                                println!(" -> group #{id}");
                            } else {
                                println!(" (no change)");
                            }
                        } else {
                            // creates a new group and programs the change
                            last_group_id += 1;
                            combinations.insert(combination, last_group_id);
                            changes.push((st_id, id, last_group_id));
                            println!(" -> new group #{last_group_id}");
                        }
                    }
                    // if let Some(last) = last_opt {
                    //     if last.eq(&combination) {
                    //         println!(" (same)");
                    //     } else {
                    //         println!(" (diff)");
                    //     };
                    // } else {
                    //     println!(" (1st)");
                    // }
                }
            }
            change = false;
        }
    }

    pub fn build_dfa(&mut self) {
        self.calc_node_pos();
        self.calc_states();
    }
}

// // Two combinations are compatible if, for each symbol, the destinations are in the same group.
// fn search_compatible_group(combinations: &BTreeMap<BTreeMap<&ReType, usize>, usize>, c: &BTreeMap<&ReType, usize>) -> Option<usize> {
//     todo!()
// }
