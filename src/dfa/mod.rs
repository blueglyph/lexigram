pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use crate::{btreeset, CollectJoin, escape_char, escape_string};
use crate::segments::{Segments, Seg};
use crate::vectree::VecTree;
use crate::take_until::TakeUntilIterator;

pub type StateId = usize;   // todo: reduce size
pub type TokenId = u16;
pub type ModeId = u16;
pub type ChannelId = u16;

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct Token(pub TokenId);

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub struct Terminal {
    pub token: Option<Token>,
    pub channel: ChannelId,
    pub push_mode: Option<ModeId>,
    pub push_state: Option<StateId>,
    pub pop: bool
}

impl Terminal {
    #[inline]
    pub fn is_only_skip(&self) -> bool {
        self.token.is_none() && self.push_mode.is_none() && self.push_state.is_none() && !self.pop
    }

    #[inline]
    pub fn is_token(&self) -> bool {
        self.token.is_some()
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(tok) = &self.token { write!(f, "<end:{}", tok.0)?; } else { write!(f, "<skip")?; }
        if self.channel != 0 { write!(f, ",ch {}", self.channel)?; }
        if self.push_mode.is_some() || self.push_state.is_some() {
            write!(f, ",push(")?;
            if let Some(m) = self.push_mode { write!(f, "mode {m}")?; }
            if let Some(s) = self.push_state { write!(f, ",state {s}")?; }
            write!(f, ")")?;
        }
        if self.pop { write!(f, ",pop")?; }
        write!(f, ">")
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum ReType { // todo: remove Boxes
    #[default] Empty,
    End(Box<Terminal>),
    Char(char),
    CharRange(Box<Segments>),
    String(Box<String>),
    Concat,
    Star,
    Plus,
    Or,
    Lazy,
}

#[test]
fn retype_size() {
    let size = std::mem::size_of::<ReType>();
    assert!(size <= 16, "size of ReType is too big: {size}");
}

impl ReType {
    pub fn is_empty(&self) -> bool {
        matches!(self, ReType::Empty)
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, ReType::Empty | ReType::End(_) | ReType::Char(_) | ReType::CharRange(_) | ReType::String(_))
    }

    pub fn is_nullable(&self) -> Option<bool> {
        match self {
            ReType::Empty | ReType::Star => Some(true),
            ReType::End(_) | ReType::Char(_) | ReType::CharRange(_) | ReType::String(_) | ReType::Plus => Some(false),
            ReType::Concat | ReType::Or | ReType::Lazy => None,
        }
    }

    pub fn is_end(&self) -> bool {
        matches!(self, ReType::End(_))
    }

    // pub fn apply_chars<F: FnMut(char) -> ()>(&self, mut f: F) {
    //     match self {
    //         ReType::Char(c) => f(*c),
    //         ReType::CharRange(i) => i.0.iter().flat_map(|(a, b)| (*a..=*b)).for_each(|code| f(char::from_u32(code).unwrap())),
    //         _ => {}
    //     }
    // }
}

impl Display for ReType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReType::Empty => write!(f, "-"),
            ReType::End(t) => write!(f, "{t}"),
            ReType::Char(c) => write!(f, "'{}'", escape_char(*c)),
            ReType::CharRange(segments) => write!(f, "[{segments}]"),
            ReType::String(s) => write!(f, "'{}'", escape_string(&s)),
            ReType::Concat => write!(f, "&"),
            ReType::Star => write!(f, "*"),
            ReType::Plus => write!(f, "+"),
            ReType::Or => write!(f, "|"),
            ReType::Lazy => write!(f, "??"),
        }
    }
}

type Id = u32;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ReNode {
    id: Option<Id>,
    op: ReType,
    firstpos: HashSet<Id>,  // todo: use BTreeSet or Vec instead? Move to DfaBuilder?
    lastpos: HashSet<Id>,   // todo: use BTreeSet or Vec instead? Move to DfaBuilder?
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
    /// Regular Expression tree
    re: VecTree<ReNode>,
    /// `followpos` table, containing the `Id` -> `Id` graph of `re`
    followpos: HashMap<Id, HashSet<Id>>,
    /// `lazypos[id_child]` includes `id_lazy` when `id_child` is a child of a lazy operator `id_lazy`
    lazypos: HashMap<Id, HashSet<usize>>,
    /// `Id` -> node index
    ids: HashMap<Id, usize>,
    warnings: Vec<String>,
    errors: Vec<String>
}

impl DfaBuilder {
    pub fn new() -> Self {
        DfaBuilder {
            re: VecTree::new(),
            followpos: HashMap::new(),
            lazypos: HashMap::new(),
            ids: HashMap::new(),
            warnings: Vec::new(),
            errors: Vec::new()
        }
    }

    pub fn from_re(re: VecTree<ReNode>) -> Self {
        let mut builder = DfaBuilder {
            re,
            followpos: HashMap::new(),
            lazypos: HashMap::new(),
            ids: HashMap::new(),
            warnings: Vec::new(),
            errors: Vec::new()
        };
        builder.preprocess_re();
        builder
    }

    pub fn get_re(&self) -> &VecTree<ReNode> {
        &self.re
    }

    pub fn get_warnings(&self) -> &Vec<String> {
        &self.warnings
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn get_messages(&self) -> String {
        let mut result = String::new();
        if !self.warnings.is_empty() {
            result.push_str(&format!("Warnings:\n- {}", self.warnings.join("\n- ")));
        }
        if !self.errors.is_empty() {
            result.push_str(&format!("ERRORS:\n- {}", self.errors.join("\n- ")));
        }
        result
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
                assert_eq!(inode.num_children(), 0);
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
                        assert!(inode.num_children() > 0);
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
                        assert_eq!(inode.num_children(), 1);
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
                        assert!(inode.num_children() > 0);
                        // firstpos, lastpost = union of children's
                        let mut firstpos = HashSet::<Id>::new();
                        for child in inode.iter_children_simple() {
                            firstpos.extend(&child.firstpos);
                        }
                        inode.firstpos.extend(firstpos);
                        let mut lastpos = HashSet::<Id>::new();
                        for child in inode.iter_children_simple() {
                            lastpos.extend(&child.lastpos);
                        }
                        inode.lastpos.extend(lastpos);
                    }
                    ReType::Lazy => {
                        assert_eq!(inode.num_children(), 1);
                        let child = inode.iter_children_simple().next().unwrap();
                        let firstpos = child.firstpos.clone();
                        let lastpos = child.lastpos.clone();
                        inode.firstpos = firstpos;
                        inode.lastpos = lastpos;
                        for ichild in inode.iter_depth_simple().filter(|node| node.is_leaf()) {
                            let ichild_id = ichild.id.unwrap();
                            if !self.lazypos.contains_key(&ichild_id) {
                                self.lazypos.insert(ichild_id, HashSet::new());
                            }
                            self.lazypos.get_mut(&ichild_id).unwrap().insert(inode.index); // FIXME: fake id
                        }
                    }
                    _ => panic!("{:?}: no way to compute firstpos/...", &*inode)
                }
            }
            if let Some(nullable) = inode.op.is_nullable() {
                inode.nullable = Some(nullable);
            } else {
                inode.nullable = match &inode.op {
                    ReType::Concat | ReType::Lazy => Some(inode.iter_children_simple().all(|child| child.nullable.unwrap())),
                    ReType::Or => Some(inode.iter_children_simple().any(|child| child.nullable.unwrap())),
                    op => panic!("{:?} should have a fixed nullable property", op)
                }
            }
        }
    }

    fn calc_states(&mut self) -> Dfa {
        const VERBOSE: bool = false;
        const RESOLVE_END_STATES: bool = true;
        const RM_LAZY_BRANCHES: bool = true;
        // initial state from firstpos(top node)
        let mut dfa = Dfa::new();
        if VERBOSE { println!("new DFA"); }
        let mut current_id = 0;
        let key = BTreeSet::from_iter(self.re.get(0).firstpos.iter().map(|&id| id));
        let mut new_states = BTreeSet::<BTreeSet<Id>>::new();
        new_states.insert(key.clone());
        let mut states = BTreeMap::<BTreeSet<Id>, StateId>::new();
        states.insert(key, current_id);
        dfa.initial_state = Some(current_id);

        // gathers lazy ids and their immediate followpos to remove phantom branches:
        let mut lazy_followpos = self.lazypos.iter().map(|(id, _)| *id).collect::<BTreeSet<Id>>();
        lazy_followpos.extend(self.lazypos.iter().filter_map(|(id, _)| self.followpos.get(id)).flatten());
        if VERBOSE { println!("lazy_followpos = {{{}}}", lazy_followpos.iter().join()); }

        // gets a partition of the symbol segments and changes Char to CharRange
        let mut symbols_part = Segments::empty();
        for id in self.ids.values() {
            let node = self.re.get_mut(*id);
            if let ReType::Char(c) = node.op {
                node.op = ReType::CharRange(Box::new(Segments::from_char(c)));
            }
            if let ReType::CharRange(segments) = &node.op {
                symbols_part.add_partition(&segments);
            }
        }
        if VERBOSE { println!("symbols = {symbols_part:X}"); }

        // prepares the segments and their source ids
        while let Some(s) = new_states.pop_first() {
            let new_state_id = states.get(&s).unwrap().clone();
            let is_lazy_state = s.iter().all(|id| lazy_followpos.contains(id));
            if VERBOSE {
                println!("- state {} = {{{}}}{}", new_state_id, states_to_string(&s), if is_lazy_state { ", lazy state" } else { "" });
            }
            let mut trans = BTreeMap::<Seg, BTreeSet<Id>>::new();   // transitions partitioned by `symbol_parts`
            let mut terminals = BTreeMap::<Id, &Terminal>::new();   // all terminals (used if terminal conflicts)
            let mut first_terminal_id: Option<Id> = None;   // first met terminal id, if any (used if terminal conflicts)
            let mut id_transitions = BTreeSet::<Id>::new(); // ids that are destination from current state (used if terminal conflicts)
            let mut id_terminal: Option<Id> = None;         // selected terminal id, if any (used to remove phantom branches)
            for (symbol, id) in s.iter().map(|id| (&self.re.get(self.ids[id]).op, *id)) {
                if symbol.is_end() {
                    if !dfa.state_graph.contains_key(&new_state_id) {
                        if VERBOSE { println!("  + {symbol} => create state {new_state_id}"); }
                        dfa.state_graph.insert(new_state_id, BTreeMap::new());
                    }
                    if let ReType::End(t) = symbol {
                        id_terminal = Some(id);
                        if first_terminal_id.is_none() {
                            first_terminal_id = Some(id);
                            if new_state_id == 0 {
                                if t.is_only_skip() {
                                    self.warnings.push(format!("<skip> on initial state is a risk of infinite loop on bad input ({t})"));
                                } else if t.is_token() {
                                    self.warnings.push(format!("<token> on initial state returns a token on bad input ({t})"));
                                }
                            }
                        }
                        if RESOLVE_END_STATES {
                            if terminals.contains_key(&id) {
                                panic!("overriding {id} -> {t} in end_states {}",
                                       terminals.iter().map(|(id, t)| format!("{id} {t}")).join());
                            }
                            terminals.insert(id, t);
                        } else {
                            if !dfa.end_states.contains_key(&new_state_id) {
                                dfa.end_states.insert(new_state_id, *t.clone());
                                if VERBOSE { println!("  # end state: id {id} {t}"); }
                            } else if VERBOSE {
                                println!("  # end state: id {id} {t} ## DISCARDED since another one already taken");
                            }
                        }
                    } else {
                        panic!("unexpected END symbol: {symbol:?}");
                    }
                } else {
                    if let ReType::CharRange(segments) = symbol {
                        if !self.followpos.contains_key(&id) {
                            println!("{id} is not in followpos; are you missing an accepting state?");
                            println!("dbg: {segments}");
                            println!("dbg: {id}");
                            println!("dbg: tree = {}", crate::dfa::tree_to_string(&self.re, true));
                            println!("dbg: followpos = {}", crate::dfa::followpos_to_string(&self));
                        }
                        id_transitions.extend(&self.followpos[&id]);
                        let cmp = segments.intersect(&symbols_part);
                        assert!(cmp.internal.is_empty(), "{symbols_part} # {segments} = {cmp}");
                        if VERBOSE { println!("  + {} to {}", &cmp.common, id); }
                        for segment in cmp.common.0.into_iter() {
                            if let Some(ids) = trans.get_mut(&segment) {
                                ids.insert(id);
                            } else {
                                trans.insert(segment, btreeset![id]);
                            }
                        }
                    }
                }
            }
            if RESOLVE_END_STATES {
                if terminals.len() > 1 {
                    if VERBOSE {
                        println!("  # {id_transitions:?}");
                        println!("  # terminal conflict: {}", terminals.iter()
                            .map(|(id, t)| format!("{id} -> {t} (has{} trans)", if id_transitions.contains(id) { "" } else { " no" }))
                            .join());
                    }
                    // The potential terminals are obtained by removing all terminals associated with an id that is already the destination
                    // of at least one transition from this state. The idea is to favour terminals that don't have another chance to be
                    // used, in case of terminal conflict.
                    let mut potentials = terminals.keys().cloned().filter(|id| !id_transitions.contains(id)).collect::<BTreeSet<_>>();
                    let chosen = match potentials.len() {
                        0 => {
                            if VERBOSE { println!("    all ids have transitions => AMBIGUOUS, selecting the first defined terminal"); }
                            self.warnings.push(format!("conflicting terminals for state {new_state_id}, none having other transitions: {}",
                                                       terminals.iter().map(|(id, t)| format!("ID {id} -> terminal {t}")).join()));
                            first_terminal_id.unwrap()
                        }
                        1 => {
                            if VERBOSE { println!("    only one id has no transitions => selecting it"); }
                            potentials.pop_first().unwrap()
                        }
                        n => {
                            self.warnings.push(format!("conflicting terminals for state {new_state_id}, {n} having no other transition: {}",
                                                       terminals.iter().map(|(id, t)| format!("ID {id} -> terminal {t}")).join()));
                            if potentials.contains(&first_terminal_id.unwrap()) {
                                if VERBOSE { println!("    {n} ids have no transitions => AMBIGUOUS, selecting the first defined terminal"); }
                                first_terminal_id.unwrap()
                            } else {
                                if VERBOSE { println!("    {n} ids have no transitions => AMBIGUOUS, selecting the first one of the list"); }
                                potentials.pop_first().unwrap()
                            }
                        }
                    };
                    id_terminal = Some(chosen);
                    let t = terminals.remove(&chosen).unwrap().clone();
                    if VERBOSE { println!("    end state: id {chosen} {t}"); }
                    dfa.end_states.insert(new_state_id, t);
                } else if let Some((id, terminal)) = terminals.pop_first() {
                    id_terminal = Some(id);
                    if VERBOSE { println!("  # end state: id {id} {terminal}"); }
                    dfa.end_states.insert(new_state_id, terminal.clone());
                }
            }

            let has_non_lazy_terminal = id_terminal.map(|id| !lazy_followpos.contains(&id)).unwrap_or(false);

            // finds the destination ids (creating new states if necessary), and populates the symbols for each destination
            let mut map = BTreeMap::<StateId, Segments>::new();
            for (segment, ids) in trans {
                if VERBOSE { print!("  - {} in {}: ", segment, states_to_string(&ids)); }
                if RM_LAZY_BRANCHES && !is_lazy_state && has_non_lazy_terminal && ids.iter().all(|id| lazy_followpos.contains(id)) {
                    if VERBOSE { println!(" => lazy, removed"); }
                    continue;
                }
                let mut state = BTreeSet::new();
                for id in ids {
                    state.extend(&self.followpos[&id]);
                }
                if VERBOSE { print!("follow = {{{}}}", states_to_string(&state)); }
                let state_id = if let Some(state_id) = states.get(&state) {
                    if VERBOSE { println!(" => state {state_id}"); }
                    *state_id
                } else {
                    new_states.insert(state.clone());
                    current_id += 1;
                    if VERBOSE { println!(" => new state {} = {{{}}}", current_id, states_to_string(&state)); }
                    states.insert(state, current_id);
                    current_id
                };
                if let Some(segments) = map.get_mut(&state_id) {
                    segments.insert(segment);
                } else {
                    map.insert(state_id, Segments::new(segment));
                }
            }
            // regroups the symbols per destination
            for segments in map.values_mut() {
                segments.normalize();
            }
            if VERBOSE {
                for (st, int) in &map {
                    println!("  {} -> {}", int, st);
                }
            }
            // finally, updates the graph with the reverse (symbol -> state) data
            dfa.state_graph.insert(new_state_id, map.into_iter().map(|(id, segments)| (segments, id)).collect());
        }
        dfa
    }

    pub fn build(&mut self) -> Dfa {
        self.warnings.clear();
        self.calc_node_pos();
        self.calc_states()
    }

    pub fn build_from_graph<T>(&mut self, graph: BTreeMap<StateId, BTreeMap<Segments, StateId>>, init_state: StateId, end_states: T) -> Option<Dfa>
        where T: IntoIterator<Item=(StateId, Terminal)>
    {
        let mut dfa = Dfa {
            state_graph: graph,
            initial_state: Some(init_state),
            end_states: BTreeMap::from_iter(end_states),
            is_normalized: false,
            first_end_state: None
        };
        dfa.first_end_state = dfa.end_states.keys().min().map(|st| *st);
        dfa.is_normalized = dfa.is_normalized();
        // TODO: add checks
        Some(dfa)
    }

    /// Merges several DFA graphs into one. The graphs represent different modes that are called with the
    /// `Action::pushMode(id)` action.
    pub fn build_from_dfa_modes<T>(&mut self, dfas: T) -> Option<Dfa>
        where T: IntoIterator<Item = (ModeId, Dfa)>
    {
        let mut iter = dfas.into_iter();
        let (idx, mut dfa) = iter.next().expect("no DFA");
        let mut init_states = BTreeMap::new();
        init_states.insert(idx, dfa.initial_state.expect(&format!("no initial state in DFA {idx}")));
        while let Some((idx, new_dfa)) = iter.next() {
            let offset = 1 + dfa.state_graph.keys().max().expect(&format!("empty DFA {idx}"));
            assert!(!init_states.contains_key(&idx), "DFA {idx} defined multiple times");
            init_states.insert(idx, offset + new_dfa.initial_state.expect(&format!("no initial state in DFA {idx}")));
            for (st_from, mut map) in new_dfa.state_graph {
                for (_, st_to) in map.iter_mut() {
                    *st_to += offset;
                }
                assert!(!dfa.state_graph.contains_key(&(st_from + offset)));
                dfa.state_graph.insert(st_from + offset, map);
            }
            dfa.end_states.extend(new_dfa.end_states.into_iter().map(|(st, term)| (st + offset, term)));
        }
        if init_states.len() > 1 {
            for (_, term) in dfa.end_states.iter_mut() {
                term.push_state = term.push_mode.and_then(|m| {
                    let state_opt = init_states.get(&m);
                    if state_opt.is_none() {
                        self.errors.push(format!("unknown mode {m} in merged graph"));
                    }
                    state_opt.cloned()
                });
            }
            dfa.first_end_state = None;
            dfa.is_normalized = false;
        }
        if self.errors.is_empty() {
            Some(dfa)
        } else {
            None
        }
    }
}

// ---------------------------------------------------------------------------------------------

pub struct Dfa {
    pub(crate) state_graph: BTreeMap<StateId, BTreeMap<Segments, StateId>>,
    pub(crate) initial_state: Option<StateId>,
    pub(crate) end_states: BTreeMap<StateId, Terminal>,
    is_normalized: bool, // are states incrementally numeroted from 0, with non-end states < end states?
    pub(crate) first_end_state: Option<StateId>
}

impl Dfa {
    pub fn new() -> Self {
        Dfa {
            state_graph: BTreeMap::new(),
            initial_state: None,
            end_states: BTreeMap::new(),
            is_normalized: false,
            first_end_state: None
        }
    }

    /// Checks if the DFA is normalized: incremental state numbers, starting at 0, with all the accepting states
    /// at the end.
    pub fn is_normalized(&self) -> bool {
        if self.state_graph.is_empty() {
            return true;
        }
        let mut states = self.state_graph.keys().collect::<Vec<_>>();
        states.sort();
        if *states[0] != 0 {
            false
        } else {
            let mut last_end = self.end_states.contains_key(states[0]);
            let mut last: StateId = 0;
            for &st in states.iter().skip(1) {
                let end = self.end_states.contains_key(st);
                if (*st != last + 1) || (!end && last_end) {
                    return false;
                }
                last_end = end;
                last = *st;
            }
            true
        }
    }

    /// Normalizes the DFA: incremental state number0, starting at 0, with all the accepting states
    /// at the end.
    pub fn normalize(&mut self) -> BTreeMap<StateId, StateId> {
        let mut translate = BTreeMap::<StateId, StateId>::new();
        let mut state_graph = BTreeMap::<StateId, BTreeMap<Segments, StateId>>::new();
        let mut end_states = BTreeMap::<StateId, Terminal>::new();
        let nbr_end = self.end_states.len();
        let mut non_end_id = 0;
        let mut end_id = self.state_graph.len() - nbr_end;
        self.first_end_state = Some(end_id);
        for &id in self.state_graph.keys() {
            if let Some(terminal) = self.end_states.get(&id) {
                translate.insert(id, end_id);
                end_states.insert(end_id, terminal.clone());
                end_id += 1;
            } else {
                translate.insert(id, non_end_id);
                non_end_id += 1;
            };
        }
        self.initial_state = self.initial_state.map(|st| *translate.get(&st).unwrap());
        for s in end_states.iter_mut() {
            if let Some(state) = s.1.push_state {
                s.1.push_state = Some(translate[&state])
            }
        }
        self.end_states = end_states;
        for (id, mut trans) in std::mem::take(&mut self.state_graph) {
            for (_, st) in trans.iter_mut() {
                *st = translate[st];
            }
            state_graph.insert(translate[&id], trans);
        }
        self.state_graph = state_graph;
        translate
    }

    /// Optimizes the number of states from `self.state_graph`. Returns a map to convert old
    /// state ids to new state ids.
    pub fn optimize(&mut self) -> BTreeMap<StateId, StateId> {
        const VERBOSE: bool = false;
        // set `separate_end_states` = `true` if different end (accepting) states should be kept apart;
        // for example, when it's important to differentiate tokens.
        let separate_end_states = true;
        if VERBOSE { println!("-----------------------------------------------------------"); }
        let mut groups = Vec::<BTreeSet<StateId>>::new();
        let mut st_to_group = BTreeMap::<StateId, usize>::new();
        let nbr_non_end_states = self.state_graph.len() - self.end_states.len();
        let mut last_non_end_id = 0;
        let first_ending_id = nbr_non_end_states + 1;

        // initial partition
        // - all non-end states
        let mut group = BTreeSet::<StateId>::new();
        for st in self.state_graph.keys().filter(|&st| !self.end_states.contains_key(st)) {
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
            for (st, _) in &self.end_states {
                st_to_group.insert(*st, groups.len());
                groups.push(BTreeSet::<StateId>::from([*st]));
            }
        } else {
            st_to_group.extend(self.end_states.iter().map(|(id, _)| (*id, groups.len())));
            groups.push(BTreeSet::from_iter(self.end_states.keys().map(|st| *st)));
        }
        let mut last_ending_id = groups.len() - 1;
        let mut change = true;
        while change {
            let mut changes = Vec::<(StateId, usize, usize)>::new();   // (state, old group, new group)
            for (id, p) in groups.iter().enumerate().filter(|(_, g)| !g.is_empty()) {
                // do all states have the same destination group for the same symbol?
                if VERBOSE { println!("group #{id}: {p:?}:"); }
                // stores combination -> group index:
                let mut combinations = BTreeMap::<BTreeMap<&Segments, usize>, usize>::new();
                for &st_id in p {
                    let combination = self.state_graph.get(&st_id).unwrap().iter()
                        .filter(|(_, st)| st_to_group.contains_key(st)) // to avoid fake "end" states
                        .map(|(s, st)| { (s, st_to_group[st]) })
                        .collect::<BTreeMap<_, _>>();
                    if VERBOSE { print!("- state {st_id}{}: {combination:?}", if self.end_states.contains_key(&st_id) { " <END>" } else { "" }) };
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
        self.first_end_state = Some(last_non_end_id + 1);
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
        // stores the new states; note that tokens may be lost if `separate_end_states` is false
        // since we take the token of the first accepting state in the group
        self.end_states = BTreeMap::<StateId, Terminal>::from_iter(
            groups.iter().enumerate()
                .filter_map(|(group_id, states)| states.iter()
                    .find_map(|s| self.end_states.get(s).map(|terminal| {
                        let mut new_terminal = terminal.clone();
                        if let Some(state) = &mut new_terminal.push_state {
                            *state = st_to_group[state];
                        }
                        (group_id as StateId, new_terminal)
                    })))
        );

        self.initial_state = Some(st_to_group[&self.initial_state.unwrap()] as StateId);
        self.state_graph = self.state_graph.iter()
            .map(|(st_id, map_sym_st)| (
                st_to_group[st_id] as StateId,
                map_sym_st.iter().map(|(segs, st)| (segs.clone(), st_to_group[st] as StateId)).collect::<BTreeMap<_, _>>()))
            .collect::<BTreeMap::<StateId, BTreeMap<Segments, StateId>>>();
        if VERBOSE {
            println!("new_graph:   {:?}", self.state_graph);
            println!("new_initial: {:?}", self.initial_state);
            println!("new_end:     {:?}", self.end_states);
            println!("-----------------------------------------------------------");
        }
        debug_assert!(self.is_normalized(), "optimized state machine isn't regular\nend_states={:?}\ngraph={:?}",
                      self.end_states, self.state_graph);
        self.is_normalized = true;
        st_to_group
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

fn states_to_string<T: Display>(s: &BTreeSet<T>) -> String {
    s.iter().map(|id| id.to_string()).join()
}

pub(crate) fn followpos_to_string(dfa_builder: &DfaBuilder) -> String {
    let mut fpos = dfa_builder.followpos.iter()
        .map(|(id, ids)| format!("{id:3} -> {}", ids.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")))
        .collect::<Vec<_>>();
    fpos.sort();
    fpos.join("\n")
}

fn node_to_string(tree: &VecTree<ReNode>, index: usize, basic: bool) -> String {
    let node = tree.get(index);
    let mut result = String::new();
    if !basic {
        if node.nullable.is_none() {
            result.push('?');
        } else if node.nullable.unwrap() {
            result.push('!');
        }
    }
    result.push_str(&node.to_string());
    let children = tree.children(index);
    if !children.is_empty() {
        result.push_str("(");
        result.push_str(&children.iter().map(|&c| node_to_string(&tree, c, basic)).collect::<Vec<_>>().join(","));
        result.push_str(")");
    }
    result
}

pub(crate) fn tree_to_string(tree: &VecTree<ReNode>, basic: bool) -> String {
    if tree.len() > 0 {
        node_to_string(tree, 0, basic)
    } else {
        "None".to_string()
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    use std::ops::Add;
    #[allow(unused)]
    use crate::io::{UTF8_HIGH_MIN, UTF8_LOW_MAX, UTF8_MAX, UTF8_MIN};
    use super::*;

    /// Generates an `ReNode` instance.
    ///
    /// # Examples
    /// ```
    /// # use std::collections::BTreeSet;
    /// # use rlexer::{dfa::*, node, io::{UTF8_HIGH_MIN, UTF8_LOW_MAX, UTF8_MAX, UTF8_MIN}};
    /// # use rlexer::segments::{Seg, Segments};
    /// assert_eq!(node!(chr 'a'), ReNode::new(ReType::Char('a')));
    /// assert_eq!(node!(['a'-'z', '0'-'9']), ReNode::new(ReType::CharRange(Box::new(Segments(BTreeSet::from([Seg('a' as u32, 'z' as u32), Seg('0' as u32, '9' as u32)]))))));
    /// assert_eq!(node!(.), ReNode::new(ReType::CharRange(Box::new(Segments(BTreeSet::from([Seg(UTF8_MIN, UTF8_LOW_MAX), Seg(UTF8_HIGH_MIN, UTF8_MAX)]))))));
    /// assert_eq!(node!(str "new"), ReNode::new(ReType::String(Box::new("new".to_string()))));
    /// assert_eq!(node!(=5), ReNode::new(ReType::End(Box::new(Terminal { token: Some(Token(5)), channel: 0, push_mode: None, push_state: None, pop: false }))));
    /// assert_eq!(node!(&), ReNode::new(ReType::Concat));
    /// assert_eq!(node!(|), ReNode::new(ReType::Or));
    /// assert_eq!(node!(*), ReNode::new(ReType::Star));
    /// assert_eq!(node!(+), ReNode::new(ReType::Plus));
    /// assert_eq!(node!(e), ReNode::new(ReType::Empty));
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! node {
        (chr $char:expr) => { ReNode::new(ReType::Char($char)) };
        (chr $char1:expr, $char2:expr $(;$char3:expr, $char4:expr)*) => { ($char1..=$char2)$(.chain($char3..=$char4))*.map(|c| ReNode::new(ReType::Char(c))) };
        ([$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?),+]) => { ReNode::new(ReType::CharRange(Box::new(segments![$($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]))) };
        (~[$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?),+]) => { ReNode::new(ReType::CharRange(Box::new(segments![~ $($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]))) };
        (.) => { node!([DOT]) };
        (str $str:expr) => { ReNode::new(ReType::String(Box::new($str.to_string()))) };
        (&) => { ReNode::new(ReType::Concat) };
        (|) => { ReNode::new(ReType::Or) };
        (*) => { ReNode::new(ReType::Star) };
        (+) => { ReNode::new(ReType::Plus) };
        (e) => { ReNode::new(ReType::Empty) };
        (??) => { ReNode::new(ReType::Lazy) };
        // actions:
        (= $id:expr) => { ReNode::new(ReType::End(Box::new(Terminal { token: Some(Token($id)), channel: 0, push_mode: None, push_state: None, pop: false })) ) };
        ($id:expr) => { ReNode::new(ReType::End(Box::new($id))) };
        //
        ([$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?,)+]) => { node!([$($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]) };
        (~[$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?,)+]) => { node!(~ [$($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]) };
    }

    #[macro_export(local_inner_macros)]
    macro_rules! term {
        (= $id:expr ) =>   { Terminal { token: Some(Token($id)), channel: 0, push_mode: None, push_state: None, pop: false } };
        (skip) =>          { Terminal { token: None, channel: 0, push_mode: None, push_state: None, pop: false } };
        (push $id:expr) => { Terminal { token: None, channel: 0, push_mode: Some($id), push_state: None, pop: false } };
        (pushst $id:expr) => { Terminal { token: None, channel: 0, push_mode: None, push_state: Some($id), pop: false } };
        (pop) =>           { Terminal { token: None, channel: 0, push_mode: None, push_state: None, pop: true } };
        (# $id:expr) =>    { Terminal { token: None, channel: $id, push_mode: None, push_state: None, pop: false } };
    }

    impl Add for Terminal {
        type Output = Terminal;

        fn add(self, rhs: Self) -> Self::Output {
            Terminal {
                token: if self.token.is_some() { self.token } else { rhs.token },
                channel: self.channel + rhs.channel,
                push_mode: if self.push_mode.is_some() { self.push_mode } else { rhs.push_mode },
                push_state: if self.push_state.is_some() { self.push_state } else { rhs.push_state },
                pop: self.pop || rhs.pop
            }
        }
    }

    #[test]
    fn macro_node() {
        assert_eq!(node!([0 - LOW_MAX, HIGH_MIN - MAX]),   ReNode::new(ReType::CharRange(Box::new(Segments::dot()))));
        assert_eq!(node!(~[0 - LOW_MAX, HIGH_MIN - MAX]), ReNode::new(ReType::CharRange(Box::new(Segments::empty()))));

        assert_eq!(node!(chr 'a'), ReNode::new(ReType::Char('a')));
        assert_eq!(node!(['a'-'z', '0'-'9']), ReNode::new(ReType::CharRange(Box::new(Segments(BTreeSet::from([Seg('a' as u32, 'z' as u32), Seg('0' as u32, '9' as u32)]))))));
        assert_eq!(node!(.), ReNode::new(ReType::CharRange(Box::new(Segments(BTreeSet::from([Seg(UTF8_MIN, UTF8_LOW_MAX), Seg(UTF8_HIGH_MIN, UTF8_MAX)]))))));
        assert_eq!(node!(str "new"), ReNode::new(ReType::String(Box::new("new".to_string()))));
        assert_eq!(node!(=5), ReNode::new(ReType::End(Box::new(Terminal { token: Some(Token(5)), channel: 0, push_mode: None, push_state: None, pop: false }))));
        assert_eq!(node!(&), ReNode::new(ReType::Concat));
        assert_eq!(node!(|), ReNode::new(ReType::Or));
        assert_eq!(node!(*), ReNode::new(ReType::Star));
        assert_eq!(node!(+), ReNode::new(ReType::Plus));
        assert_eq!(node!(e), ReNode::new(ReType::Empty));
    }
}