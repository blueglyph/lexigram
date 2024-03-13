// Copyright 2023 Redglyph

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use crate::vectree::VecTree;
use crate::take_until::TakeUntilIterator;

mod tests;
mod vectree;
mod take_until;
mod macros;
pub mod io;
pub mod lexgen;

/// Regular expressions / DFA, See:
/// - https://blog.burntsushi.net/regex-internals/
/// - https://en.wikipedia.org/wiki/Tagged_Deterministic_Finite_Automaton
/// - https://arxiv.org/abs/2206.01398
///
/// See also:
/// - Ref: https://en.wikipedia.org/wiki/Comparison_of_parser_generators

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct Token(usize);    // token ID

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum ReType {
    #[default] Empty,
    End(Token),
    Char(char),
    String(String),
    Concat,
    Star,
    Plus,
    Or
}

impl ReType {
    pub fn is_empty(&self) -> bool {
        matches!(self, ReType::Empty)
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, ReType::Empty | ReType::End(_) | ReType::Char(_) | ReType::String(_))
    }

    pub fn is_nullable(&self) -> Option<bool> {
        match self {
            ReType::Empty | ReType::Star => Some(true),
            ReType::End(_) | ReType::Char(_) | ReType::String(_) | ReType::Plus => Some(false),
            _ => None
        }
    }

    pub fn is_end(&self) -> bool {
        matches!(self, ReType::End(_))
    }
}

impl Display for ReType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReType::Empty => write!(f, "-"),
            ReType::End(id) => write!(f, "<end:{}>", id.0),
            ReType::Char(c) => write!(f, "'{c}'"),
            ReType::String(s) => write!(f, "'{s}'"),
            ReType::Concat => write!(f, "&"),
            ReType::Star => write!(f, "*"),
            ReType::Plus => write!(f, "+"),
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

type StateId = usize;

pub struct DfaBuilder {
    /// Regular Expression tree
    re: VecTree<ReNode>,
    /// `followpos` table, containing the `Id` -> `Id` graph of `re`
    followpos: HashMap<Id, HashSet<Id>>,
    /// `Id` -> node index
    ids: HashMap<Id, usize>,
}

impl DfaBuilder {
    pub fn new(re: VecTree<ReNode>) -> DfaBuilder {
        let mut builder = DfaBuilder {
            re,
            followpos: HashMap::new(),
            ids: HashMap::new(),
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
                if !inode.is_empty() {
                    inode.firstpos.insert(id);
                    inode.lastpos.insert(id);
                }
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
                        let a = iter.next().unwrap();   // a is c[i]
                        let mut lastpos = a.lastpos.clone();
                        while let Some(b) = iter.next() {   // b is c[i+1]
                            for j in &lastpos {
                                if !self.followpos.contains_key(j) {
                                    self.followpos.insert(*j, HashSet::new());
                                }
                                self.followpos.get_mut(j).unwrap().extend(&b.firstpos);
                            }
                            // we must build the lastpos during the iteration
                            // &(0,1,2,3) = &2(&1(&0(0,1),2),3) => &0.lpos=0.lpos, &1.lpos=lastpos("&",[&0,2])), ...
                            if b.nullable.unwrap() {
                                lastpos.extend(&b.lastpos);
                            } else {
                                lastpos = b.lastpos.clone();
                            }
                        }
                    }
                    ReType::Star | ReType::Plus => {
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

    fn calc_states(&mut self) -> Dfa {
        // initial state from firstpos(top node)
        let mut dfa = Dfa::new();
        let mut current_id = 0;
        let key = BTreeSet::from_iter(self.re.get(0).firstpos.iter().map(|&id| id));
        let mut new_states = BTreeSet::<BTreeSet<Id>>::new();
        new_states.insert(key.clone());
        dfa.states.insert(key, current_id);
        dfa.initial_state = Some(current_id);

        // unfold all the states
        while let Some(s) = new_states.pop_first() {
            let new_state_id = dfa.states.get(&s).unwrap().clone();
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
                let state_id = if let Some(state_id) = dfa.states.get(&state) {
                    *state_id
                } else {
                    new_states.insert(state.clone());
                    current_id += 1;
                    dfa.states.insert(state, current_id);
                    current_id
                };
                if let Some(map) = dfa.state_graph.get_mut(&new_state_id) {
                    if !symbol.is_end() {
                        map.insert(symbol.clone(), state_id);
                    }
                } else {
                    let mut map = BTreeMap::new();
                    if !symbol.is_end() {
                        map.insert(symbol.clone(), state_id);
                    }
                    dfa.state_graph.insert(new_state_id, map);
                }
                if symbol.is_end() {
                    dfa.end_states.insert(new_state_id);
                }
            }
        }
        dfa
    }

    pub fn build(&mut self) -> Dfa {
        self.calc_node_pos();
        self.calc_states()
        // todo: do we want to optimize?
    }
}

// ---------------------------------------------------------------------------------------------

pub struct Dfa {
    states: BTreeMap<BTreeSet<Id>, StateId>,
    state_graph: BTreeMap<StateId, BTreeMap<ReType, StateId>>,
    initial_state: Option<StateId>,
    end_states: BTreeSet<StateId>
}

impl Dfa {
    pub fn new() -> Self {
        Dfa {
            states: BTreeMap::<BTreeSet<Id>, StateId>::new(),
            state_graph: BTreeMap::new(),
            initial_state: None,
            end_states: BTreeSet::new()
        }
    }

    pub fn from_graph<T>(graph: BTreeMap<StateId, BTreeMap<ReType, StateId>>, init_state: StateId, end_states: T) -> Dfa
        where T: IntoIterator<Item=StateId>
    {
        Dfa {
            states: BTreeMap::<BTreeSet<Id>, StateId>::new(),
            state_graph: graph,
            initial_state: Some(init_state),
            end_states: BTreeSet::from_iter(end_states)
        }
    }

    /// Optimizes the number of states from `self.state_graph`. Returns a map to convert old
    /// state ids to new state ids.
    ///
    /// # Arguments
    ///
    /// * `separate_end_states` = `true` if different end (accepting) states should be kept apart;
    /// for example, when it's important to differentiate tokens.
    pub fn optimize(&mut self, separate_end_states: bool) -> BTreeMap::<StateId, StateId> {
        const VERBOSE: bool = false;
        if VERBOSE { println!("-----------------------------------------------------------"); }
        let mut groups = Vec::<BTreeSet<StateId>>::new();
        let mut st_to_group = BTreeMap::<StateId, usize>::new();
        let nbr_non_end_states = self.state_graph.len() - self.end_states.len();
        let mut last_non_end_id = 0;
        let first_ending_id = nbr_non_end_states + 1;

        // initial partition
        // - all non-end states
        let mut group = BTreeSet::<StateId>::new();
        for st in self.state_graph.keys().filter(|&st| !self.end_states.contains(st)) {
            group.insert(*st);
            st_to_group.insert(*st, 0);
        }
        groups.push(group);
        // - reserves a few empty groups for later non-ending groups:
        for _ in 1..first_ending_id {
            groups.push(BTreeSet::new());
        }
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
        let mut last_ending_id = groups.len() - 1;
        let mut change = true;
        while change {
            let mut changes = Vec::<(StateId, usize, usize)>::new();   // (state, old group, new group)
            for (id, p) in groups.iter().enumerate().filter(|(_, g)| !g.is_empty()) {
                // do all states have the same destination group for the same symbol?
                if VERBOSE { println!("group #{id}: {p:?}:"); }
                // stores combination -> group index:
                let mut combinations = BTreeMap::<BTreeMap<&ReType, usize>, usize>::new();
                for &st_id in p {
                    let combination = self.state_graph.get(&st_id).unwrap().iter()
                        .filter(|(_, st)| st_to_group.contains_key(st)) // to avoid fake "end" states
                        .map(|(s, st)| { (s, st_to_group[st]) })
                        .collect::<BTreeMap<_, _>>();
                    if VERBOSE { print!("- state {st_id}{}: {combination:?}", if self.end_states.contains(&st_id) { " <END>" } else { "" }) };
                    if combinations.is_empty() {
                        combinations.insert(combination, id);   // first one remains in this group
                        if VERBOSE { println!(" (1st, no change)"); }
                    } else {
                        if let Some(&group_id) = combinations.get(&combination) {
                            // programs the change if it's one of the new groups
                            if group_id != id {
                                changes.push((st_id, id, group_id));
                                if VERBOSE { println!(" -> group #{group_id}"); }
                            } else {
                                if VERBOSE { println!(" (no change)"); }
                            }
                        } else {
                            // creates a new group and programs the change
                            let new_id = if id < first_ending_id {
                                assert!(last_non_end_id + 1 < first_ending_id, "no more IDs for non-accepting state");
                                last_non_end_id += 1;
                                last_non_end_id
                            } else {
                                last_ending_id += 1;
                                last_ending_id
                            };
                            combinations.insert(combination, new_id);
                            changes.push((st_id, id, new_id));
                            if VERBOSE { println!(" -> new group #{new_id}"); }
                        }
                    }
                }
            }
            change = !changes.is_empty();
            for (st_id, old_group_id, new_group_id) in changes {
                if new_group_id >= groups.len() {
                    groups.push(BTreeSet::<StateId>::new());
                }
                assert!(groups[old_group_id].remove(&st_id));
                groups[new_group_id].insert(st_id);
                assert_eq!(st_to_group.insert(st_id, new_group_id), Some(old_group_id));
            }
            if VERBOSE && change { println!("---"); }
        }
        if VERBOSE { println!("-----------------------------------------------------------"); }
        // removes the gaps in group numbering
        let delta = first_ending_id - last_non_end_id - 1;
        if delta > 0 {
            if VERBOSE {
                println!("removing the gaps in group numbering: (delta={delta})");
                println!("st_to_group: {st_to_group:?}");
                println!("groups: {groups:?}");
            }
            for (_, new_st) in st_to_group.iter_mut() {
                if *new_st > last_non_end_id {
                    *new_st -= delta;
                }
            }
            groups = groups.into_iter().enumerate()
                .filter(|(id, _)| *id <= last_non_end_id || *id >= first_ending_id)
                .map(|(_, g)| g)
                .collect::<Vec<_>>();
            if VERBOSE {
                println!("st_to_group: {st_to_group:?}");
                println!("groups: {groups:?}");
            }
        }
        // stores the new states
        self.end_states = BTreeSet::<StateId>::from_iter(
            groups.iter().enumerate()
                .filter(|(_, g)| g.iter().any(|s| self.end_states.contains(s)))
                .map(|(group_id, _)| group_id as StateId)
        );
        self.initial_state = Some(st_to_group[&self.initial_state.unwrap()] as StateId);
        self.state_graph = self.state_graph.iter()
            .map(|(st_id, map_sym_st)| (
                st_to_group[st_id] as StateId,
                map_sym_st.iter().map(|(sym, st)| (sym.clone(), st_to_group[st] as StateId)).collect::<BTreeMap<_, _>>()))
            .collect::<BTreeMap::<StateId, BTreeMap<ReType, StateId>>>();
        if VERBOSE {
            println!("new_graph:   {:?}", self.state_graph);
            println!("new_initial: {:?}", self.initial_state);
            println!("new_end:     {:?}", self.end_states);
            println!("-----------------------------------------------------------");
        }
        st_to_group
    }
}
