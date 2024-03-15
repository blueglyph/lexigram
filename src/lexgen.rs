#![allow(unused)]

use std::collections::{BTreeMap, BTreeSet};
use crate::{Dfa, StateId};

pub struct LexGen {
    dfa: Dfa
}

impl LexGen {
    pub fn new(dfa: Dfa) -> Self {
        LexGen { dfa }
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

pub(crate) fn partition_symbols(g: &BTreeMap<StateId, BTreeMap<char, StateId>>) -> Vec<BTreeSet<char>> {
    const VERBOSE: bool = false;
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
            if VERBOSE { println!("- sub = {}", chars_to_string(&sub)); }
            for i in 0..groups.len() {
                if VERBOSE { println!("  - groups[i] = {}", chars_to_string(&groups[i])); }
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
                    if VERBOSE { print!(" -> groups[i] = {}", chars_to_string(&groups[i])); }
                    groups[i] = common;
                    if !extra_group.is_empty() {
                        if VERBOSE { print!(" & push {}", chars_to_string(&extra_group)); }
                        groups.push(extra_group);
                    }
                    // sub is split -> { common, extra_sub }, with common already in groups[i] so we don't keep it
                    if VERBOSE { println!(" -> sub = {}", chars_to_string(&extra_sub)); }
                    sub = extra_sub;
                    if sub.is_empty() {
                        break; // no need to inspect other groups
                    }

                }
            }
            if !sub.is_empty() {
                if VERBOSE { println!("  -> push {}", chars_to_string(&sub)); }
                groups.push(sub);
            }
        }
    }
    if VERBOSE { println!("result -> {}", char_groups_to_string(&groups)); }
    groups
}

fn char_groups_to_string<'a, T: IntoIterator<Item=&'a BTreeSet<char>>>(partition: T) -> String {
    partition.into_iter().map(|chars| chars_to_string(chars)).collect::<Vec<_>>().join(", ")
}

fn chars_to_string(chars: &BTreeSet<char>) -> String {
    let mut result = "[".to_string();
    result.push_str(&chars.into_iter().map(|c| format!("{c}")).collect::<String>());
    result.push(']');
    result
}

fn group_transitions_to_string(p: &BTreeMap<BTreeSet<char>, StateId>) -> String {
    format!("{}",
             p.iter()
                 .map(|(chars, st)|
                     format!("[{}] => {}",
                             chars.into_iter().map(|c| format!("{c}")).collect::<String>(),
                             st
                     )
                 ).collect::<Vec<_>>().join(", ")
    )
}
