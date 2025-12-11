// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use crate::parser::Symbol;
use crate::{AltId, CollectJoin, SymInfoTable, SymbolTable, VarId};
use crate::grammar::ruleflag;

pub fn alt_to_str<T: SymInfoTable>(f: &Vec<Symbol>, symbol_table: Option<&T>) -> String {
    if f.is_empty() {
        "<empty>".to_string()
    } else {
        f.iter().map(|s| s.to_str_quote(symbol_table)).join(" ")
    }
}

pub fn alt_to_rule_str<T: SymInfoTable>(nt: VarId, f: &Vec<Symbol>, symbol_table: Option<&T>) -> String {
    format!("{} -> {}", Symbol::NT(nt).to_str(symbol_table), alt_to_str(f, symbol_table))
}

/// Stores a production alternative (or alternative body): `A a` or `B` in `A -> A a | B`.
///
/// The [`Alternative`] type behaves like a `Vec<Symbol>` (`Deref` / `DerefMut`), and must be
/// created with [`Alternative::new`].
#[derive(Clone, Eq, PartialOrd, Ord, Debug)]
pub struct Alternative {
    pub(crate) v: Vec<Symbol>,
    pub(crate) flags: u32,          // only for GREEDY, L_FORM and R_ASSOC
    pub(crate) ambig_alt_id: Option<AltId>,
    pub(crate) origin: Option<(VarId, usize)>,
}

impl Alternative {
    pub fn new(v: Vec<Symbol>) -> Self {
        Alternative { v, flags: 0, ambig_alt_id: None, origin: None }
    }

    pub fn with_flags(mut self, flags: u32) -> Self {
        self.flags = flags;
        self
    }

    pub fn with_ambig_alt_id(mut self, ambig_alt_id: AltId) -> Self {
        self.ambig_alt_id = Some(ambig_alt_id);
        self
    }

    pub fn with_origin(mut self, original_var: VarId, original_index: usize) -> Self {
        self.origin = Some((original_var, original_index));
        self
    }

    pub fn symbols(&self) -> &Vec<Symbol> {
        &self.v
    }

    pub fn get_ambig_alt_id(&self) -> Option<AltId> {
        self.ambig_alt_id
    }

    pub fn get_origin(&self) -> Option<(VarId, usize)> {
        self.origin
    }

    pub fn get_flags(&self) -> u32 {
        self.flags
    }

    pub fn to_str<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        let mut s = if self.flags & ruleflag::ALTERNATIVE_INFO != 0 {
            format!("<{}> ", ruleflag::alt_info_to_string(self.flags).join(","))
        } else {
            String::new()
        };
        s.push_str(&alt_to_str(&self.v, symbol_table));
        s
    }

    pub fn to_rule_str(&self, nt: VarId, symbol_table: Option<&SymbolTable>, mut extra_flags: u32) -> String {
        extra_flags = (extra_flags | self.flags) & ruleflag::ALTERNATIVE_INFO;
        let s = if extra_flags != 0 {
            format!("<{}> ", ruleflag::alt_info_to_string(extra_flags).join(","))
        } else {
            String::new()
        };
        format!("{} -> {s}{}", Symbol::NT(nt).to_str(symbol_table), alt_to_str(&self.v, symbol_table))
    }

    pub fn to_macro_item(&self) -> String {
        let mut src = match (self.flags, self.ambig_alt_id, self.origin) {
            (0, None, None) => String::new(),
            (f, None, None) => format!("#{f}, "),
            (f, Some(o), None) => format!("#({f}, {o}), "),
            (0, None, Some((v, id))) => format!("%({v}, {id}), "),
            (f, None, Some((v, id))) => format!("#{f}, %({v}, {id}), "),
            (f, Some(o), Some((v, id))) => format!("#({f}, {o}), %({v}, {id}), "),
        };
        src.push_str(&self.v.iter().map(|s| s.to_macro_item()).join(", "));
        src
    }

    pub fn to_macro(&self) -> String {
        format!("alt!({})", self.to_macro_item())
    }

    pub fn is_sym_empty(&self) -> bool {
        self.v.len() == 1 && self.v[0] == Symbol::Empty
    }

    pub(crate) fn calc_alt_first(&self, first: &HashMap<Symbol, HashSet<Symbol>>) -> HashSet<Symbol> {
        let mut new = HashSet::<Symbol>::new();
        new.extend(first[&self.v[0]].iter().filter(|s| *s != &Symbol::Empty));
        let mut trail = true;
        for i in 0..self.v.len() - 1 {
            let sym_i = &self.v[i];
            if first[sym_i].contains(&Symbol::Empty) {
                new.extend(first[&self.v[i + 1]].iter().filter(|s| *s != &Symbol::Empty));
            } else {
                trail = false;
                break;
            }
        }
        if trail && first[self.last().unwrap()].contains(&Symbol::Empty) {
            new.insert(Symbol::Empty);
        }
        new
    }

    pub(crate) fn is_greedy(&self) -> bool {
        self.flags & ruleflag::GREEDY != 0
    }
}

// we use only `v` (the string of symbols) and `flags` the equality test
impl PartialEq for Alternative {
    fn eq(&self, other: &Self) -> bool {
        self.flags == other.flags && self.v == other.v
    }
}

impl Deref for Alternative {
    type Target = Vec<Symbol>;

    fn deref(&self) -> &Self::Target {
        &self.v
    }
}

impl DerefMut for Alternative {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.v
    }
}