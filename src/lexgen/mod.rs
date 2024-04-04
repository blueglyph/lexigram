pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use crate::escape_char;
use crate::intervals::Intervals;
use super::dfa::*;

pub type GroupId = usize;

pub struct LexGen {
    dfa: Dfa,
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: Box<HashMap<char, GroupId>>,
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
        // let symbols = symbol_part.iter().flatten().map(|c| *c).collect::<BTreeSet<char>>();
        let symbols = symbol_part.chars().collect::<BTreeSet<char>>();
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

fn partition_symbols(g: &BTreeMap<StateId, BTreeMap<Intervals, StateId>>) -> Intervals {
    const VERBOSE: bool = false;
    let mut groups = Intervals::empty();    // todo: pre-fill with ASCII range?
    for i in g.values().flat_map(|x| x.keys()) {
        groups.partition(i);
    }
    groups
    /*
    let mut groups = Vec::<BTreeSet<char>>::new();
    for (st, trans) in g {
        // extracts a subpartition for the current transition by regrouping the symbols:
        // a set of "[chars] => state" instead of a set of "char => state"
        let mut grouped_trans = BTreeMap::<StateId, BTreeSet<char>>::new();
        for (c, st) in trans {
            if grouped_trans.contains_key(st) {
                grouped_trans.get_mut(st).unwrap()
                    .insert(*c);
            } else {
                grouped_trans.insert(*st, BTreeSet::from([*c]));
            }
        }
        let mut sub_p = BTreeMap::<BTreeSet<char>, StateId>::from_iter(grouped_trans.into_iter().map(|(k, v)| (v, k)));
        if VERBOSE { println!("state {}: {}", st, group_transitions_to_string(&sub_p)); }

        // transforms the resulting partition
        while let Some((mut sub, _st)) = sub_p.pop_first() {
            if VERBOSE { println!("- sub = {}", chars_to_string(&sub, true)); }
            for i in 0..groups.len() {
                if VERBOSE { println!("  - groups[i] = {}", chars_to_string(&groups[i], true)); }
                let common = sub.intersection(&groups[i]).cloned().collect::<BTreeSet<_>>();
                if common.is_empty() {
                    // nothing to do, sub will be added to groups if it's not modified by another groups[j]
                    if VERBOSE { println!("    (disjoints)"); }
                } else {
                    // what's not common, in both groups[i] and sub?
                    let extra_group = groups[i].difference(&sub).cloned().collect::<BTreeSet<_>>();
                    let extra_sub = sub.difference(&groups[i]).cloned().collect::<BTreeSet<_>>();
                    if VERBOSE {
                        match true {
                            _ if extra_group.is_empty() && !extra_sub.is_empty() => print!("    group>sub"),
                            _ if !extra_group.is_empty() && extra_sub.is_empty() => print!("    group<sub"),
                            _ => print!("    group=sub"),
                        }
                    }
                    // current group is split -> { common, extra_group }
                    if VERBOSE { print!(" -> groups[i] = {}", chars_to_string(&groups[i], true)); }
                    groups[i] = common;
                    if !extra_group.is_empty() {
                        if VERBOSE { print!(" & push {}", chars_to_string(&extra_group, true)); }
                        groups.push(extra_group);
                    }
                    // sub is split -> { common, extra_sub }, with common already in groups[i] so we don't keep it
                    if VERBOSE { println!(" -> sub = {}", chars_to_string(&extra_sub, true)); }
                    sub = extra_sub;
                    if sub.is_empty() {
                        break; // no need to inspect other groups
                    }

                }
            }
            if !sub.is_empty() {
                if VERBOSE { println!("  -> push {}", chars_to_string(&sub, true)); }
                groups.push(sub);
            }
        }
    }
    if VERBOSE { println!("result -> {}", char_groups_to_string(&groups)); }
    groups
    */
}

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

