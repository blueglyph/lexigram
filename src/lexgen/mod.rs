pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::Bound::Included;
#[cfg(test)]
use crate::dfa::tests::print_graph;
use crate::escape_char;
use crate::intervals::Intervals;
use super::dfa::*;

pub type GroupId = usize;

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
pub struct Seg(u32, u32);

pub struct SegMap<T>(BTreeMap<Seg, T>);

impl<T: Clone> SegMap<T> {
    pub fn new() -> Self {
        SegMap(BTreeMap::new())
    }

    pub fn from_iter<I: IntoIterator<Item = (Seg, T)>>(iter: I) -> Self {
        SegMap(BTreeMap::from_iter(iter))
    }

    pub fn get(&self, value: u32) -> Option<T> {
        let (Seg(_a, b), data) = self.0.range((Included(&Seg(0, 0)), Included(&Seg(value, u32::MAX)))).next_back()?;
        if *b >= value {
            Some(data.clone())
        } else {
            None
        }
    }
}

pub struct LexGen {
    dfa: Dfa,
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: Box<HashMap<char, GroupId>>,
    pub seg_to_group: SegMap<GroupId>,
    pub nbr_groups: usize,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    pub state_table: Box<[StateId]>,
    pub terminal_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
}

impl LexGen {
    pub fn new(dfa: Dfa) -> Self {
        let mut lexgen = LexGen {
            dfa,
            ascii_to_group: Box::default(),
            utf8_to_group: Box::default(),
            seg_to_group: SegMap::new(),
            nbr_groups: 0,
            initial_state: 0,
            first_end_state: 0,
            nbr_states: 0,
            state_table: Box::default(),
            terminal_table: Box::default(),
        };
        lexgen.create_input_tables();
        lexgen.create_state_tables();
        lexgen
    }

    fn create_input_tables(&mut self) {
        let symbol_part = partition_symbols(&self.dfa.state_graph);
        let mut symbols = Intervals::empty();
        for i in symbol_part {
            symbols.extend(i.0);
        }
        todo!();
        /*
        let mut symbol_to_group = BTreeMap::<char, GroupId>::new();
        for (id, (a, b)) in symbol_part.iter().enumerate() {
            symbol_to_group.extend((*a..=*b).map(|code| (char::from_u32(code).unwrap(), id as GroupId)));
        }
        self.nbr_groups = symbol_part.len();
        let error_id = self.nbr_groups;
        self.ascii_to_group = (0..128_u8)
            .map(|i| *symbol_to_group.get(&char::from(i)).unwrap_or(&error_id))
            .collect::<Vec<_>>().into_boxed_slice();
        self.utf8_to_group = symbols.iter()
            .filter_map(|c| if c.is_ascii() { None } else { symbol_to_group.get(c).map(|g| (*c, *g as GroupId)) })
            .collect::<HashMap<char, GroupId>>().into();
        */
    }

    fn create_state_tables(&mut self) {
        self.initial_state = self.dfa.initial_state.unwrap();
        self.first_end_state = self.dfa.first_end_state.unwrap();
        self.nbr_states = self.dfa.state_graph.len();
        let nbr_states = self.dfa.state_graph.len();
        // we add one extra table index to allow for the 'error group', which equals nbr_group:
        // state_table[nbr_state * nbr_group + nbr_group] must exist; the content will be ignored.
        let mut state_table = vec!(self.nbr_states; self.nbr_groups * nbr_states + 1);
        for (state_from, trans) in &self.dfa.state_graph {
            for (intervals, state_to) in trans {
                for symbol in intervals.chars() {
                    let symbol_group = char_to_group(&self.ascii_to_group, &self.utf8_to_group, symbol);
                    state_table[self.nbr_groups * state_from + symbol_group] = *state_to;
                }
            }
        }
        self.state_table = state_table.into_boxed_slice();
        let terminal_table = self.dfa.end_states.iter()
            .filter_map(|(&st, t)| if st >= self.first_end_state { Some(t.clone()) } else { None })
            .collect::<Vec<_>>();
        self.terminal_table = terminal_table.into_boxed_slice();
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

#[inline]
pub fn char_to_group(ascii_to_group: &[GroupId], utf8_to_group: &HashMap<char, GroupId>, char: char) -> GroupId {
    if char.len_utf8() == 1 {
        ascii_to_group[u8::try_from(char).unwrap() as usize]
    } else {
        utf8_to_group[&char]
    }
}

// todo: option to split ASCII range?
fn partition_symbols(g: &BTreeMap<StateId, BTreeMap<Intervals, StateId>>) -> Vec<Intervals> {
    const VERBOSE: bool = false;
    let mut groups = Vec::new();
    #[cfg(test)] if VERBOSE { print_graph(g, None); }
    for (_state, branches) in g {
        // branches from a given state
        let mut map = BTreeMap::<StateId, Intervals>::new();
        for (intervals, destination) in branches {
            if let Some(i) = map.get_mut(destination) {
                i.extend(&intervals.0);
            } else {
                map.insert(*destination, intervals.clone());
            }
        }
        // optimizes the intervals, in case it's not already done
        for intervals in map.values_mut() {
            intervals.normalize();
        }
        #[cfg(test)] if VERBOSE { println!("{_state} => {}", map.values().map(|i| format!("{i:X}")).collect::<Vec<_>>().join(", ")); }
        let mut state_sub = map.into_values().collect::<BTreeSet<Intervals>>();
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

#[cfg(disabled)]
fn char_groups_to_string<'a, T: IntoIterator<Item=&'a BTreeSet<char>>>(partition: T) -> String {
    partition.into_iter().map(|chars| chars_to_string(chars, true)).collect::<Vec<_>>().join(", ")
}

fn chars_to_string(chars: &BTreeSet<char>, bracket: bool) -> String {
    let mut result = String::new();
    if bracket { result.push('['); }
    result.push_str(&chars.into_iter().map(|c| format!("{}", escape_char(*c))).collect::<String>());
    if bracket { result.push(']'); }
    result
}

#[cfg(disabled)]
fn group_transitions_to_string(p: &BTreeMap<BTreeSet<char>, StateId>) -> String {
    format!("{}",
             p.iter()
                 .map(|(chars, st)|
                     format!("[{}] => {}",
                             chars.into_iter().map(|c| format!("{}", escape_char(*c))).collect::<String>(),
                             st
                     )
                 ).collect::<Vec<_>>().join(", ")
    )
}

