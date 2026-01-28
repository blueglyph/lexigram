// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use crate::fixed_sym_table::SymInfoTable;
use crate::parser::Symbol;
use crate::{AltId, CollectJoin, VarId};

// ---------------------------------------------------------------------------------------------

// easier to use than an enum
pub mod ruleflag {
    /// Star or Plus repeat child alternative.
    /// Set by `RuleTreeSet<General>::normalize_plus_or_star()` in `flags`.
    pub const CHILD_REPEAT: u32 = 1;
    /// Right-recursive child NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const R_RECURSION: u32 = 2;
    /// Left-recursive child NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_L_RECURSION: u32 = 4;
    /// Left-recursive, ambiguous child NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_AMBIGUITY: u32 = 8;
    /// Child NT created to regroup the independent alts when transforming an ambiguous, recursive rule.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const CHILD_INDEPENDENT_AMBIGUITY: u32 = 16;
    /// Left-factorized parent NT.
    /// Set by `ProdRuleSet<T>::left_factorize()` in `flags`.
    pub const PARENT_L_FACTOR: u32 = 32;
    /// Left-factorized child NT.
    /// Set by `ProdRuleSet<T>::left_factorize()` in `flags`.
    pub const CHILD_L_FACT: u32 = 64;
    /// Low-latency non-terminal alternative, used with `CHILD_REPEAT` or `R_RECURSION`.
    /// Set by `ProdRuleSet<General>::build_from(rules: BuildFrom<RuleTreeSet<Normalized>>` in `flags`.
    pub const L_FORM: u32 = 128;
    /// Right-associative alternative.
    /// Set by `ProdRuleSet<General>::build_from(rules: BuildFrom<RuleTreeSet<Normalized>>` in alts.
    pub const R_ASSOC: u32 = 256;
    /// Left-recursive parent NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const PARENT_L_RECURSION: u32 = 512;
    /// Left-recursive, ambiguous parent NT.
    /// Set by `ProdRuleSet<T>::remove_left_recursion()` in `flags`.
    pub const PARENT_AMBIGUITY: u32 = 1024;
    /// Star or Plus repeat parent alternative.
    /// Set by `RuleTreeSet<General>::normalize_plus_or_star()` in `flags`.
    pub const PARENT_REPEAT: u32 = 2048;
    /// CHILD_REPEAT and PARENT_REPEAT is +, not * (used with both flags)
    pub const REPEAT_PLUS: u32 = 4096;
    /// GREEDY alternative: is expected to generate an ambiguity in the parsing table
    pub const GREEDY: u32 = 8192;
    /// Precedence identical to previous alternative (only valid for binary left-/right-associative)
    pub const PREC_EQ: u32 = 16384;
    /// List of token-separated items: α (β α)*, where β only contains fixed terminals and α contains at least one value
    pub const SEP_LIST: u32 = 32768;

    pub const TRANSF_PARENT: u32 = /*R_RECURSION |*/ PARENT_L_FACTOR | PARENT_L_RECURSION | PARENT_AMBIGUITY | PARENT_REPEAT;
    pub const TRANSF_CHILD: u32 = CHILD_REPEAT | CHILD_L_RECURSION | CHILD_AMBIGUITY | CHILD_L_FACT;
    pub const TRANSF_CHILD_AMB: u32 = CHILD_AMBIGUITY | R_RECURSION | L_FORM;
    pub const ALTERNATIVE_INFO: u32 = L_FORM | R_ASSOC | GREEDY | PREC_EQ;
    pub const L_RECURSION: u32 = PARENT_L_RECURSION | CHILD_L_RECURSION;
    pub const CHILD_L_REPEAT: u32 = CHILD_REPEAT | L_FORM;


    pub fn to_string(flags: u32) -> Vec<String> {
        static NAMES: [(u32, &str); 16] = [
            (CHILD_REPEAT               , "child_+_or_*"),
            (R_RECURSION                , "right_rec"),
            (CHILD_L_RECURSION          , "child_left_rec"),
            (CHILD_AMBIGUITY            , "child_amb"),
            (CHILD_INDEPENDENT_AMBIGUITY, "child_ind_amb"),
            (PARENT_L_FACTOR            , "parent_left_fact"),
            (CHILD_L_FACT, "child_left_fact"),
            (L_FORM                     , "L-form"),
            (R_ASSOC                    , "R-assoc"),
            (PARENT_L_RECURSION         , "parent_left_rec"),
            (PARENT_AMBIGUITY           , "parent_amb"),
            (PARENT_REPEAT              , "parent_+_or_*"),
            (REPEAT_PLUS                , "plus"),
            (GREEDY                     , "greedy"),
            (PREC_EQ                    , "prec_eq"),
            (SEP_LIST                   , "sep_list"),
        ];
        NAMES.iter().filter_map(|(f, t)| if flags & f != 0 { Some(t.to_string()) } else { None } ).collect()
    }

    pub fn alt_info_to_string(mut flags: u32) -> Vec<String> {
        static NAMES: [(u32, &str); 4] = [(L_FORM, "L"), (R_ASSOC, "R"), (GREEDY, "G"), (PREC_EQ, "P")];
        let v: Vec<String> = NAMES.iter().filter_map(|(f, t)|
            if flags & f != 0 {
                flags &= !f;
                Some(t.to_string())
            } else {
                None
            }).collect();
        v
    }
}

// ---------------------------------------------------------------------------------------------

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
    pub v: Vec<Symbol>,
    pub flags: u32,          // only for GREEDY, L_FORM and R_ASSOC
    pub ambig_alt_id: Option<AltId>,
    pub origin: Option<(VarId, usize)>,
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

    pub fn to_rule_str<T: SymInfoTable>(&self, nt: VarId, symbol_table: Option<&T>, mut extra_flags: u32) -> String {
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

    pub fn calc_alt_first(&self, first: &HashMap<Symbol, HashSet<Symbol>>) -> HashSet<Symbol> {
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

    pub fn is_greedy(&self) -> bool {
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
