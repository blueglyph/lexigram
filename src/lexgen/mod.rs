pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap};
#[cfg(test)]
use crate::dfa::tests::print_graph;
use crate::escape_char;
use crate::segments::{Segments, Seg, SegMap};
use super::dfa::*;

pub type GroupId = u32;

pub struct LexGen {
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: Box<HashMap<char, GroupId>>,
    pub seg_to_group: SegMap<GroupId>,
    pub max_utf8_chars: u32,
    pub nbr_groups: u32,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    pub state_table: Box<[StateId]>,
    pub terminal_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
}

impl LexGen {
    pub fn new() -> Self {
        LexGen {
            ascii_to_group: vec![GroupId::MAX; 128].into_boxed_slice(),
            utf8_to_group: Box::default(),
            seg_to_group: SegMap::new(),
            max_utf8_chars: 128,
            nbr_groups: 0,
            initial_state: 0,
            first_end_state: 0,
            nbr_states: 0,
            state_table: Box::default(),
            terminal_table: Box::default(),
        }
    }

    pub fn from_dfa(dfa: &Dfa) -> Self {
        let mut lexgen = LexGen::new();
        lexgen.build(dfa);
        lexgen
    }

    pub fn build(&mut self, dfa: &Dfa) {
        self.create_input_tables(dfa);
        self.create_state_tables(dfa);
    }

    fn create_input_tables(&mut self, dfa: &Dfa) {
        const VERBOSE: bool = false;
        let symbol_part = partition_symbols(&dfa.state_graph);
        let symbol_to_group = SegMap::from_iter(
            symbol_part.iter().enumerate().flat_map(|(id, i)| i.iter().map(move |ab| (*ab, id as GroupId)))
        );
        self.nbr_groups = symbol_part.len() as GroupId;
        let error_id = self.nbr_groups as GroupId;
        self.ascii_to_group.fill(error_id);
        self.utf8_to_group.clear();
        self.seg_to_group.clear();
        let mut left = self.max_utf8_chars;
        for (seg, group_id) in symbol_to_group {
            if seg.0 < 128 {
                if VERBOSE {
                    println!("ASCII: {}-{} ({}-{}) => {group_id}",
                             escape_char(char::from_u32(seg.0).unwrap()), escape_char(char::from_u32(seg.1.min(127)).unwrap()),
                             seg.0, seg.1.min(127));
                }
                for b in seg.0..=seg.1.min(127) {
                    self.ascii_to_group[b as usize] = group_id;
                }
            }
            if seg.1 >= 128 {
                let low = 128.max(seg.0);
                let high = seg.1.min(low + left - 1);
                for u in low..=high {
                    if VERBOSE { println!("UTF8: {} ({u}) => {group_id}", escape_char(char::from_u32(u).unwrap())); }
                    self.utf8_to_group.insert(char::from_u32(u).unwrap(), group_id);
                }
                left -= 1 + high - low;
                if high < seg.1 {
                    if VERBOSE {
                        println!("SEG: {}-{} ({}-{}) => {group_id}",
                             escape_char(char::from_u32(high + 1).unwrap()), escape_char(char::from_u32(seg.1).unwrap()),
                             high + 1, seg.1);
                    }
                    self.seg_to_group.insert(Seg(high + 1, seg.1), group_id);
                }
            }
        }
    }

    fn create_state_tables(&mut self, dfa: &Dfa) {
        const VERBOSE: bool = true;
        self.initial_state = dfa.initial_state.unwrap();
        self.first_end_state = dfa.first_end_state.unwrap();
        self.nbr_states = dfa.state_graph.len();
        let nbr_states = dfa.state_graph.len();
        // we add one extra table index to allow for the 'error group', which equals nbr_group:
        // state_table[nbr_state * nbr_group + nbr_group] must exist; the content will be ignored.
        let mut state_table = vec!(self.nbr_states; self.nbr_groups as usize * nbr_states + 1);
        for (state_from, trans) in &dfa.state_graph {
            if VERBOSE { println!("state {state_from}"); }
            for (segments, state_to) in trans {
                if VERBOSE { println!("- {segments} -> state {state_to}"); }
                for symbol in segments.chars() {
                    let symbol_group = char_to_group(&self.ascii_to_group, &self.utf8_to_group, &self.seg_to_group, symbol).unwrap_or(self.nbr_groups);
                    state_table[self.nbr_groups as usize * state_from + symbol_group as usize] = *state_to;
                }
            }
        }
        self.state_table = state_table.into_boxed_slice();
        let terminal_table = dfa.end_states.iter()
            .filter_map(|(&st, t)| if st >= self.first_end_state { Some(t.clone()) } else { None })
            .collect::<Vec<_>>();
        self.terminal_table = terminal_table.into_boxed_slice();
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

#[inline]
pub fn char_to_group(ascii_to_group: &[GroupId], utf8_to_group: &HashMap<char, GroupId>, seg_to_group: &SegMap<GroupId>, symbol: char) -> Option<GroupId> {
    if symbol.len_utf8() == 1 {
        Some(ascii_to_group[u8::try_from(symbol).unwrap() as usize])
    } else {
        utf8_to_group.get(&symbol).cloned().or_else(|| seg_to_group.get(symbol as u32))
    }
}

// todo: option to split ASCII range?
fn partition_symbols(g: &BTreeMap<StateId, BTreeMap<Segments, StateId>>) -> Vec<Segments> {
    const VERBOSE: bool = false;
    let mut groups = Vec::new();
    #[cfg(test)] if VERBOSE { print_graph(g, None); }
    for (_state, branches) in g {
        // branches from a given state
        let mut map = BTreeMap::<StateId, Segments>::new();
        for (segments, destination) in branches {
            if let Some(i) = map.get_mut(destination) {
                i.extend(&segments.0);
            } else {
                map.insert(*destination, segments.clone());
            }
        }
        // optimizes the segments, in case it's not already done
        for segments in map.values_mut() {
            segments.normalize();
        }
        #[cfg(test)] if VERBOSE { println!("{_state} => {}", map.values().map(|i| format!("{i:X}")).collect::<Vec<_>>().join(", ")); }
        let mut state_sub = map.into_values().collect::<BTreeSet<Segments>>();
        while let Some(mut sub) = state_sub.pop_first() {
            if VERBOSE { println!("- sub = {sub}"); }
            for i in 0..groups.len() {
                if VERBOSE { println!("  - groups[{i}] = {}", groups[i]); }
                let cmp = sub.intersect(&groups[i]);
                if cmp.common.is_empty() {
                    if VERBOSE { println!("    (disjoints)"); }
                } else {
                    // groups[i] is split { cmp.common, cmp.external }
                    groups[i] = cmp.common;
                    if !cmp.external.is_empty() {
                        if VERBOSE { println!("    -> push {}", cmp.external); }
                        groups.push(cmp.external);
                    }
                    // sub is split { cmp.common, cmp.internal }, and we discard cmp.common since already in groups
                    if VERBOSE { println!("    -> sub = {}", cmp.internal); }
                    sub = cmp.internal;
                    if sub.is_empty() {
                        break;
                    }
                }
            }
            if !sub.is_empty() {
                groups.push(sub);
            }
        }
    }
    #[cfg(test)] if VERBOSE { println!("=> {}", groups.iter().map(|i| format!("{i:X}")).collect::<Vec<_>>().join(", ")); }
    groups
}
