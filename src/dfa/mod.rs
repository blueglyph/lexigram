// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ops::Add;
use vectree::VecTree;
use crate::{btreeset, CollectJoin, escape_char, escape_string, General, Normalized, indent_source};
use crate::log::{BufLog, Logger};
use crate::segments::{Segments, Seg};
use crate::take_until::TakeUntilIterator;

pub type StateId = usize;   // todo: reduce size
pub type TokenId = u16;
pub type ModeId = u16;
pub type ChannelId = u16;

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum ActionOption {
    #[default] Skip,
    Token(TokenId),
    More
}

impl ActionOption {
    pub fn is_skip(&self) -> bool { self == &ActionOption::Skip }
    pub fn is_token(&self) -> bool { matches!(self, ActionOption::Token(_) ) }
    pub fn is_more(&self) -> bool { self == &ActionOption::More }

    pub fn get_token(&self) -> Option<TokenId> {
        if let ActionOption::Token(token) = self {
            Some(*token)
        } else {
            None
        }
    }
}

impl Add for ActionOption {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            ActionOption::Skip => rhs,
            _ => if rhs.is_skip() { self } else { panic!("can't add {self:?} and {rhs:?}") }
        }
    }
}

impl Display for ActionOption {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionOption::Skip => write!(f, "skip"),
            ActionOption::Token(t) => write!(f, "end:{t}"),
            ActionOption::More => write!(f, "more")
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum ModeOption {
    #[default]
    None,
    Mode(ModeId),
    Push(ModeId)
}

impl ModeOption {
    pub fn is_none(&self) -> bool {
        self == &ModeOption::None
    }

    pub fn is_mode(&self) -> bool {
        matches!(self, &ModeOption::Mode(_))
    }

    pub fn is_push(&self) -> bool {
        matches!(self, &ModeOption::Push(_))
    }
}

/// Terminal instructions for the lexer logic.
///
/// Possible actions:
/// * skip           => doesn't return token, drops current string
/// * more           => doesn't return token, keeps current string for next rule
/// * push(n)        => pushes mode and switches to mode `n`
/// * pop            => pops next mode from the stack
/// * channel #      => defines output channel
///
/// By default, `push`, `pop`, `channel` or no specified action outputs a token (`token = Some(..)`).
/// If a `skip` or `more` action is specified, no token is returned (`token = None`).
#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub struct Terminal {
    pub action: ActionOption,
    pub channel: ChannelId,
    pub mode: ModeOption,
    pub mode_state: Option<StateId>,
    pub pop: bool
}

impl Terminal {
    #[inline]
    pub fn is_only_skip(&self) -> bool {
        self.action.is_skip() && self.mode.is_none() && self.mode_state.is_none() && !self.pop
    }

    #[inline]
    pub fn is_token(&self) -> bool {
        self.action.is_token()
    }

    #[inline]
    pub fn get_token(&self) -> Option<TokenId> {
        self.action.get_token()
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}", self.action)?;
        if self.channel != 0 { write!(f, ",ch {}", self.channel)?; }
        if !self.mode.is_none() || self.mode_state.is_some() {
            match self.mode {
                ModeOption::None => {}
                ModeOption::Mode(m) => write!(f, ",mode({m}")?,
                ModeOption::Push(m) => write!(f, ",push({m}")?,
            }
            if let Some(s) = self.mode_state { write!(f, ",state {s}")?; }
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
    let size = size_of::<ReType>();
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

    pub fn empty() -> ReNode { ReNode::new(ReType::Empty) }

    pub fn end(t: Terminal) -> ReNode { ReNode::new(ReType::End(Box::new(t))) }

    pub fn char(c: char) -> ReNode { ReNode::new(ReType::Char(c)) }

    pub fn char_range(s: Segments) -> ReNode { ReNode::new(ReType::CharRange(Box::new(s))) }

    pub fn string<T: Into<String>>(s: T) -> ReNode { ReNode::new(ReType::String(Box::new(s.into()))) }

    pub fn concat() -> ReNode { ReNode::new(ReType::Concat) }

    pub fn star() -> ReNode { ReNode::new(ReType::Star) }

    pub fn plus() -> ReNode { ReNode::new(ReType::Plus) }

    pub fn or() -> ReNode { ReNode::new(ReType::Or) }

    pub fn lazy() -> ReNode { ReNode::new(ReType::Lazy) }

    pub fn is_leaf(&self) -> bool {
        self.op.is_leaf()
    }

    pub fn is_empty(&self) -> bool {
        self.op.is_empty()
    }

    pub fn get_type(&self) -> &ReType {
        &self.op
    }

    pub fn is_nullable(&self) -> Option<bool> {
        self.nullable
    }

    pub fn get_mut_type(&mut self) -> &mut ReType {
        &mut self.op
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
    log: BufLog
}

impl DfaBuilder {
    pub fn new() -> Self {
        DfaBuilder {
            re: VecTree::new(),
            followpos: HashMap::new(),
            lazypos: HashMap::new(),
            ids: HashMap::new(),
            log: BufLog::new()
        }
    }

    pub fn from_re(re: VecTree<ReNode>) -> Self {
        let mut builder = DfaBuilder {
            re,
            followpos: HashMap::new(),
            lazypos: HashMap::new(),
            ids: HashMap::new(),
            log: BufLog::new()
        };
        builder.preprocess_re();
        builder
    }

    #[inline(always)]
    pub fn get_re(&self) -> &VecTree<ReNode> {
        &self.re
    }

    #[inline(always)]
    pub fn get_log(&self) -> &BufLog {
        &self.log
    }

    #[inline(always)]
    pub fn num_warnings(&self) -> usize {
        self.log.num_warnings()
    }

    #[inline(always)]
    pub fn get_warnings(&self) -> impl Iterator<Item = &String> {
        self.log.get_warnings()
    }

    #[inline(always)]
    pub fn num_errors(&self) -> usize {
        self.log.num_errors()
    }

    #[inline(always)]
    pub fn get_errors(&self) -> impl Iterator<Item = &String> {
        self.log.get_errors()
    }

    pub fn get_messages(&self) -> String {
        let mut result = String::new();
        if self.log.num_warnings() > 0 {
            result.push_str(&format!("Warnings:\n- {}", self.log.get_warnings().join("\n- ")));
        }
        if self.log.num_errors() > 0 {
            result.push_str(&format!("ERRORS:\n- {}", self.log.get_errors().join("\n- ")));
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
                        self.re.add(Some(index), ReNode::char(c));
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
                        let firstpos = inode.iter_children_simple().next().unwrap().firstpos.iter().map(|&n| n).to_vec();
                        inode.firstpos.extend(firstpos);
                        let lastpos = inode.iter_children_simple().next().unwrap().lastpos.iter().map(|&n| n).to_vec();
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

    fn calc_states(&mut self) -> Dfa<General> {
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
        if VERBOSE { println!("lazy_followpos = {{{}}}", lazy_followpos.iter().join(", ")); }

        // gets a partition of the symbol segments and changes Char to CharRange
        let mut symbols_part = Segments::empty();
        for id in self.ids.values() {
            let node = self.re.get_mut(*id);
            if let ReType::Char(c) = node.op {
                node.op = ReType::CharRange(Box::new(Segments::from(c)));
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
                                    self.log.add_warning(format!("<skip> on initial state is a risk of infinite loop on bad input ({t})"));
                                } else if t.is_token() {
                                    self.log.add_warning(format!("<token> on initial state returns a token on bad input ({t})"));
                                }
                            }
                        }
                        if RESOLVE_END_STATES {
                            if terminals.contains_key(&id) {
                                panic!("overriding {id} -> {t} in end_states {}",
                                       terminals.iter().map(|(id, t)| format!("{id} {t}")).join(", "));
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
                            println!("dbg: tree = {}", tree_to_string(&self.re, None, true));
                            println!("dbg: followpos = {}", followpos_to_string(&self));
                        }
                        id_transitions.extend(&self.followpos[&id]);
                        let cmp = segments.intersect(&symbols_part);
                        assert!(cmp.internal.is_empty(), "{symbols_part} # {segments} = {cmp}");
                        if VERBOSE { println!("  + {} to {}", &cmp.common, id); }
                        for segment in cmp.common.into_iter() {
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
                            .join(", "));
                    }
                    // The potential terminals are obtained by removing all terminals associated with an id that is already the destination
                    // of at least one transition from this state. The idea is to favour terminals that don't have another chance to be
                    // used, in case of terminal conflict.
                    let mut potentials = terminals.keys().cloned().filter(|id| !id_transitions.contains(id)).collect::<BTreeSet<_>>();
                    let chosen = match potentials.len() {
                        0 => {
                            if VERBOSE { println!("    all ids have transitions => AMBIGUOUS, selecting the first defined terminal"); }
                            self.log.add_warning(format!("conflicting terminals for state {new_state_id}, none having other transitions: {}",
                                                       terminals.iter().map(|(id, t)| format!("ID {id} -> terminal {t}")).join(", ")));
                            first_terminal_id.unwrap()
                        }
                        1 => {
                            if VERBOSE { println!("    only one id has no transitions => selecting it"); }
                            potentials.pop_first().unwrap()
                        }
                        n => {
                            self.log.add_warning(format!("conflicting terminals for state {new_state_id}, {n} having no other transition: {}",
                                                       terminals.iter().map(|(id, t)| format!("ID {id} -> terminal {t}")).join(", ")));
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
        // transfers all the log messages to the Dfa
        dfa.log.extend(std::mem::replace(&mut self.log, BufLog::new()));
        dfa
    }

    pub fn build(&mut self) -> Dfa<General> {
        self.log.clear();
        self.calc_node_pos();
        self.calc_states()
    }

    pub fn build_from_graph<T>(&mut self, graph: BTreeMap<StateId, BTreeMap<Segments, StateId>>, init_state: StateId, end_states: T) -> Option<Dfa<General>>
        where T: IntoIterator<Item=(StateId, Terminal)>
    {
        let mut dfa = Dfa::<General> {
            state_graph: graph,
            initial_state: Some(init_state),
            end_states: BTreeMap::from_iter(end_states),
            first_end_state: None,
            log: BufLog::new(),
            _phantom: PhantomData
        };
        dfa.first_end_state = dfa.end_states.keys().min().map(|st| *st);
        // TODO: add checks
        Some(dfa)
    }

    /// Merges several DFA graphs into one. The graphs represent different modes that are called with the
    /// `Action::pushMode(id)` action.
    pub fn build_from_dfa_modes<T, U>(&mut self, dfas: T) -> Option<Dfa<General>>
        where T: IntoIterator<Item = (ModeId, Dfa<U>)>
    {
        let mut iter = dfas.into_iter();
        let (idx, mut dfa) = iter.next().expect("no DFA");
        let mut init_states = BTreeMap::new();
        init_states.insert(idx, dfa.initial_state.expect(&format!("no initial state in DFA {idx}")));
        while let Some((idx, new_dfa)) = iter.next() {
            dfa.log.extend(new_dfa.log);
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
                term.mode_state = match term.mode {
                    ModeOption::None => None,
                    ModeOption::Mode(m) | ModeOption::Push(m) => {
                        let state_opt = init_states.get(&m);
                        if state_opt.is_none() {
                            self.log.add_error(format!("unknown mode {m} in merged graph"));
                        }
                        state_opt.cloned()
                    }
                };
            }
            dfa.first_end_state = None;
        }
        if self.log.num_errors() == 0 {
            Some(Dfa::<General> {
                state_graph: dfa.state_graph,
                initial_state: dfa.initial_state,
                end_states: dfa.end_states,
                first_end_state: dfa.first_end_state,
                log: dfa.log,
                _phantom: PhantomData,
            })
        } else {
            None
        }
    }
}

// ---------------------------------------------------------------------------------------------

pub struct Dfa<T> {
    state_graph: BTreeMap<StateId, BTreeMap<Segments, StateId>>,
    initial_state: Option<StateId>,
    end_states: BTreeMap<StateId, Terminal>,
    first_end_state: Option<StateId>,
    pub(crate) log: BufLog,
    _phantom: PhantomData<T>
}

impl Dfa<General> {
    pub fn new() -> Dfa<General> {
        Dfa::<General> {
            state_graph: BTreeMap::new(),
            initial_state: None,
            end_states: BTreeMap::new(),
            first_end_state: None,
            log: BufLog::new(),
            _phantom: PhantomData
        }
    }
}

impl<T> Dfa<T> {
    pub fn get_state_graph(&self) -> &BTreeMap<StateId, BTreeMap<Segments, StateId>> {
        &self.state_graph
    }

    pub fn get_initial_state(&self) -> &Option<StateId> {
        &self.initial_state
    }

    pub fn get_end_states(&self) -> &BTreeMap<StateId, Terminal> {
        &self.end_states
    }

    pub fn get_first_end_state(&self) -> &Option<StateId> {
        &self.first_end_state
    }

    /// Checks if the DFA is normalized: incremental state numbers, starting at 0, with all the accepting states
    /// at the end.
    pub fn is_normalized(&self) -> bool {
        if self.state_graph.is_empty() {
            return true;
        }
        let mut states = self.state_graph.keys().to_vec();
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
    pub fn normalize(mut self) -> Dfa<Normalized> {
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
            if let Some(state) = s.1.mode_state {
                s.1.mode_state = Some(translate[&state])
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
        Dfa::<Normalized> {
            state_graph: self.state_graph,
            initial_state: self.initial_state,
            end_states: self.end_states,
            first_end_state: self.first_end_state,
            log: self.log,
            _phantom: PhantomData,
        }
    }

    /// Optimizes the number of states from `self.state_graph`. Returns a map to convert old
    /// state ids to new state ids.
    pub fn optimize(mut self) -> Dfa<Normalized> {
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
                .to_vec();
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
                        if let Some(state) = &mut new_terminal.mode_state {
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
        Dfa::<Normalized> {
            state_graph: self.state_graph,
            initial_state: self.initial_state,
            end_states: self.end_states,
            first_end_state: self.first_end_state,
            log: self.log,
            _phantom: PhantomData,
        }
    }
}

impl Dfa<Normalized> {
    pub fn build_tables_source_code(&self, indent: usize) -> String {
        let mut source = Vec::<String>::new();
        source.push("let dfa_tables = DfaTables::new(".to_string());
        source.push("    btreemap![".to_string());
        source.extend(graph_to_code(&self.state_graph, None, 8));
        source.push("    ],".to_string());
        source.push(format!("    {:?},", self.initial_state));
        source.push("    btreemap![".to_string());
        let es = self.end_states.iter().map(|(s, t)| format!("{} => {}", s, term_to_string(t))).to_vec();
        source.extend(es.chunks(6).map(|v| format!("        {},", v.join(", "))));
        source.push("    ],".to_string());
        source.push(format!("    {:?},", self.first_end_state));
        source.push(");".to_string());
        indent_source(vec![source], indent)
    }
}

// ---------------------------------------------------------------------------------------------

pub struct DfaTables {
    state_graph: BTreeMap<StateId, BTreeMap<Segments, StateId>>,
    initial_state: Option<StateId>,
    end_states: BTreeMap<StateId, Terminal>,
    first_end_state: Option<StateId>,
}

impl DfaTables {
    pub fn new(
        state_graph: BTreeMap<StateId, BTreeMap<Segments, StateId>>,
        initial_state: Option<StateId>,
        end_states: BTreeMap<StateId, Terminal>,
        first_end_state: Option<StateId>,
    ) -> Self {
        DfaTables { state_graph, initial_state, end_states, first_end_state }
    }

    pub fn make_dfa(self) -> Dfa<Normalized> {
        Dfa {
            state_graph: self.state_graph,
            initial_state: self.initial_state,
            end_states: self.end_states,
            first_end_state: self.first_end_state,
            log: BufLog::new(),
            _phantom: PhantomData
        }
    }
}

// ---------------------------------------------------------------------------------------------

impl From<Dfa<General>> for Dfa<Normalized> {
    fn from(dfa: Dfa<General>) -> Self {
        dfa.normalize()
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

fn states_to_string<T: Display>(s: &BTreeSet<T>) -> String {
    s.iter().map(|id| id.to_string()).join(", ")
}

pub(crate) fn followpos_to_string(dfa_builder: &DfaBuilder) -> String {
    let mut fpos = dfa_builder.followpos.iter()
        .map(|(id, ids)| format!("{id:3} -> {}", ids.iter().map(|x| x.to_string()).join(", ")))
        .to_vec();
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
        result.push_str(&children.iter().map(|&c| node_to_string(&tree, c, basic)).to_vec().join(","));
        result.push_str(")");
    }
    result
}

// ---------------------------------------------------------------------------------------------
// Debug functions left in the code for the integration tests

#[allow(unused)]
/// Debug function to display the content of a tree.
pub fn tree_to_string(tree: &VecTree<ReNode>, root: Option<usize>, basic: bool) -> String {
    if tree.len() > 0 {
        node_to_string(tree, root.unwrap_or_else(|| tree.get_root().unwrap()), basic)
    } else {
        "None".to_string()
    }
}

#[allow(unused)]
pub(crate) fn term_to_string(t: &Terminal) -> String {
    let mut str = Vec::<String>::new();
    match &t.action {
        ActionOption::Skip => str.push("term!(skip)".to_string()),
        ActionOption::Token(t) => str.push(format!("term!(={t})")),
        ActionOption::More => str.push("term!(more)".to_string())
    }
    if t.channel != 0 {
        str.push(format!("term!(#{})", t.channel));
    }
    match t.mode {
        ModeOption::None => {}
        ModeOption::Mode(m) => str.push(format!("term!(mode {m})")),
        ModeOption::Push(m) => str.push(format!("term!(push {m})")),
    }
    if let Some(id) = t.mode_state {
        str.push(format!("term!(pushst {})", id));
    }
    if t.pop {
        str.push("term!(pop)".to_string());
    }
    str.join(" + ")
}

#[allow(unused)]
/// Debug function to display the content of a Dfa.
pub fn print_dfa<T>(dfa: &Dfa<T>, indent: usize) {
    // println!("  graph:      {:?}", dfa.state_graph);
    println!("Initial state: {}", if let Some(st) = dfa.initial_state { st.to_string() } else { "none".to_string() });
    println!("Graph:");
    print_graph(&dfa.state_graph, Some(&dfa.end_states), indent);
    println!("End states:\n{: >indent$}{}", " ", dfa.end_states.iter().map(|(s, t)| format!("{} => {}", s, term_to_string(t))).join(", "), indent=indent);
}

fn graph_to_code(
    state_graph: &BTreeMap<StateId, BTreeMap<Segments, StateId>>,
    end_states: Option<&BTreeMap<StateId, Terminal>>,
    indent: usize,
) -> Vec<String> {
    let s = String::from_utf8(vec![32; indent]).unwrap();
    state_graph.iter().map(|(state, trans)| {
        let mut has_neg = false;
        let mut tr = trans.iter().map(|(sym, st)| {
            let s = (sym.to_string(), st.to_string());
            has_neg |= s.0.starts_with('~');
            s
        }).to_vec();
        if has_neg {
            for (sym, _) in &mut tr {
                if !sym.ends_with(']') {
                    sym.insert(0, '[');
                    sym.push(']');
                }
            }
        }
        format!("{s}{} => branch!({}),{}",
                state,
                tr.into_iter().map(|(sym, st)| format!("{sym} => {st}")).join(", "),
                end_states.and_then(|map| map.get(&state).map(|token| format!(" // {}", token))).unwrap_or(String::new()),
        )
    }).collect()
}


/// Debug function to display the graph of a Dfa.
pub fn print_graph(state_graph: &BTreeMap<StateId, BTreeMap<Segments, StateId>>, end_states: Option<&BTreeMap<StateId, Terminal>>, indent: usize) {
    let src = graph_to_code(state_graph, end_states, indent);
    println!("{}", src.join("\n"));
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    use std::ops::Add;
    use super::*;

    /// Generates an `ReNode` instance.
    ///
    /// # Examples
    /// ```
    /// # use std::collections::BTreeSet;
    /// # use lexigram_lib::{dfa::*, node, io::{UTF8_HIGH_MIN, UTF8_LOW_MAX, UTF8_MAX, UTF8_MIN}};
    /// # use lexigram_lib::segments::{Seg, Segments};
    /// assert_eq!(node!(chr 'a'), ReNode::char('a'));
    /// assert_eq!(node!(['a'-'z', '0'-'9']), ReNode::char_range(Segments::from([Seg('a' as u32, 'z' as u32), Seg('0' as u32, '9' as u32)])));
    /// assert_eq!(node!(.), ReNode::char_range(Segments::from([Seg(UTF8_MIN, UTF8_LOW_MAX), Seg(UTF8_HIGH_MIN, UTF8_MAX)])));
    /// assert_eq!(node!(str "new"), ReNode::string("new"));
    /// assert_eq!(node!(=5), ReNode::end(Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false }));
    /// assert_eq!(node!(&), ReNode::concat());
    /// assert_eq!(node!(|), ReNode::or());
    /// assert_eq!(node!(*), ReNode::star());
    /// assert_eq!(node!(+), ReNode::plus());
    /// assert_eq!(node!(e), ReNode::empty());
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! node {
        (chr $char:expr) => { $crate::dfa::ReNode::char($char) };
        (chr $char1:expr, $char2:expr $(;$char3:expr, $char4:expr)*) => { ($char1..=$char2)$(.chain($char3..=$char4))*.map(|c| $crate::dfa::ReNode::char(c)) };
        ([$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?),+]) => { $crate::dfa::ReNode::char_range(segments![$($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]) };
        (~[$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?),+]) => { $crate::dfa::ReNode::char_range(segments![~ $($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]) };
        (.) => { node!([DOT]) };
        (str $str:expr) => { $crate::dfa::ReNode::string($str) };
        (&) => { $crate::dfa::ReNode::concat() };
        (|) => { $crate::dfa::ReNode::or() };
        (*) => { $crate::dfa::ReNode::star() };
        (+) => { $crate::dfa::ReNode::plus() };
        (e) => { $crate::dfa::ReNode::empty() };
        (??) => { $crate::dfa::ReNode::lazy() };
        // actions:
        (= $id:expr) => { $crate::dfa::ReNode::end($crate::dfa::Terminal {
            action: $crate::dfa::ActionOption::Token($id),
            channel: 0,
            mode: $crate::dfa::ModeOption::None,
            mode_state: None,
            pop: false
        }) };
        ($id:expr) => { $crate::dfa::ReNode::end($id) };
        //
        ([$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?,)+]) => { node!([$($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]) };
        (~[$($($a1:literal)?$($a2:ident)? $(- $($b1:literal)?$($b2:ident)?)?,)+]) => { node!(~ [$($($a1)?$($a2)?$(- $($b1)?$($b2)?)?),+]) };
    }

    #[macro_export(local_inner_macros)]
    macro_rules! term {
        (= $id:expr ) =>     { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Token($id),channel: 0,   mode: $crate::dfa::ModeOption::None,      mode_state: None,      pop: false } };
        (more) =>            { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::More,      channel: 0,   mode: $crate::dfa::ModeOption::None,      mode_state: None,      pop: false } };
        (skip) =>            { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Skip,      channel: 0,   mode: $crate::dfa::ModeOption::None,      mode_state: None,      pop: false } };
        (mode $id:expr) =>   { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Skip,      channel: 0,   mode: $crate::dfa::ModeOption::Mode($id), mode_state: None,      pop: false } };
        (push $id:expr) =>   { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Skip,      channel: 0,   mode: $crate::dfa::ModeOption::Push($id), mode_state: None,      pop: false } };
        (pushst $id:expr) => { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Skip,      channel: 0,   mode: $crate::dfa::ModeOption::None,      mode_state: Some($id), pop: false } };
        (pop) =>             { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Skip,      channel: 0,   mode: $crate::dfa::ModeOption::None,      mode_state: None,      pop: true  } };
        (# $id:expr) =>      { $crate::dfa::Terminal { action: $crate::dfa::ActionOption::Skip,      channel: $id, mode: $crate::dfa::ModeOption::None,      mode_state: None,      pop: false } };
    }

    impl Add for Terminal {
        type Output = Terminal;

        fn add(self, rhs: Self) -> Self::Output {
            Terminal {
                // token: if self.token.is_some() { self.token } else { rhs.token },
                action: self.action + rhs.action,
                channel: self.channel + rhs.channel,
                mode: if !self.mode.is_none() { self.mode } else { rhs.mode },
                mode_state: if self.mode_state.is_some() { self.mode_state } else { rhs.mode_state },
                pop: self.pop || rhs.pop
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::{branch, btreemap};
        use crate::dfa::{graph_to_code, ActionOption, ModeOption, ReNode, Terminal};
        use crate::io::{UTF8_HIGH_MIN, UTF8_LOW_MAX, UTF8_MAX, UTF8_MIN};
        use crate::segments::{Seg, Segments};

        #[test]
        fn macro_node() {
            assert_eq!(node!([0 - LOW_MAX, HIGH_MIN - MAX]), ReNode::char_range(Segments::dot()));
            assert_eq!(node!(~[0 - LOW_MAX, HIGH_MIN - MAX]), ReNode::char_range(Segments::empty()));

            assert_eq!(node!(chr 'a'), ReNode::char('a'));
            assert_eq!(node!(['a'-'z', '0'-'9']), ReNode::char_range(Segments::from([Seg('a' as u32, 'z' as u32), Seg('0' as u32, '9' as u32)])));
            assert_eq!(node!(.), ReNode::char_range(Segments::from([Seg(UTF8_MIN, UTF8_LOW_MAX), Seg(UTF8_HIGH_MIN, UTF8_MAX)])));
            assert_eq!(node!(str "new"), ReNode::string("new"));
            assert_eq!(node!(=5), ReNode::end(Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false }));
            assert_eq!(node!(&), ReNode::concat());
            assert_eq!(node!(|), ReNode::or());
            assert_eq!(node!(*), ReNode::star());
            assert_eq!(node!(+), ReNode::plus());
            assert_eq!(node!(e), ReNode::empty());
        }

        #[test]
        fn state_graph() {
            let test = btreemap![
            1 => branch!(),
            2 => branch!('A' => 0),
            3 => branch!('B' => 1, 'C' => 2),
            4 => branch!('D'-'F' => 3),
            5 => branch!('0'-'9', '_' => 4),
            6 => branch!(DOT => 5),
            7 => branch!(~['*'] => 6),
            8 => branch!(~['+', '-'] => 7),
            9 => branch!(~['*'] => 8, ['*'] => 9),
        ];
            let s = graph_to_code(&test, None, 4);
            assert_eq!(s, vec![
                "    1 => branch!(),".to_string(),
                "    2 => branch!('A' => 0),".to_string(),
                "    3 => branch!('B' => 1, 'C' => 2),".to_string(),
                "    4 => branch!('D'-'F' => 3),".to_string(),
                "    5 => branch!('0'-'9', '_' => 4),".to_string(),
                "    6 => branch!(DOT => 5),".to_string(),
                "    7 => branch!(~['*'] => 6),".to_string(),
                "    8 => branch!(~['+', '-'] => 7),".to_string(),
                "    9 => branch!(~['*'] => 8, ['*'] => 9),".to_string(),
            ]);
        }
    }
}