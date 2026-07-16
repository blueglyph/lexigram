// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::marker::PhantomData;
use iter_index::IndexerIterator;
use lexigram_core::log::{BufLog, LogMsg, LogStatus, Logger};
use lexigram_core::parser::Symbol;
use lexigram_core::{AltId, CollectJoin, TokenId, VarId};
use lexigram_core::alt::{ruleflag, Alternative};
use lexigram_core::parser::lr::{LRAction, LRStateId};
use crate::build::BuildFrom;
use crate::grammar::{ProdRule, ProdRuleSet, SepInfo};
use crate::{btreemap, btreeset, item, prule, General, SymbolTable, LR};

/// Dot position in a production rule (alternative). The symbol after the dot is at [value as usize], if it exists.
pub type DotPos = u16;
/// Item index in a state's list of items
pub type ItemId = u16;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LRItem {
    /// position of dot in item (index of following symbol: 0 = • "a" a "a")
    pub pos: DotPos,
    /// nonterminal, index in `ProdRuleSet::prules`
    pub nt: VarId,
    /// index of production alternative in `ProdRuleSet::prules[nt]`
    pub alt_idx: u16,
    /// lookahead
    pub prefix: Option<BTreeSet<TokenId>>,
}

impl LRItem {
    fn prefix(&self) -> Option<impl Iterator<Item=TokenId>> {
        self.prefix.as_ref().map(|p| p.iter().copied())
    }

    fn is_kernel(&self) -> bool {
        self.pos > 0
    }
}

impl PartialOrd for LRItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LRItem {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.pos == 0 || other.pos == 0 {
            // core items (dot pos != 0) have priority, no matter the alt ID
            self.pos.cmp(&other.pos).reverse()
                .then(self.nt.cmp(&other.nt))
                .then(self.alt_idx.cmp(&other.alt_idx))
        } else {
            // core items are compared by alt ID, then by the position of the dot
            self.alt_idx.cmp(&other.alt_idx)
                .then(self.nt.cmp(&other.nt))
                .then(self.pos.cmp(&other.pos).reverse())
        }
    }
}

impl<T> ProdRuleSet<T> {
    #[allow(unused)]
    /// Removes lone ε symbols in productions
    fn remove_empty_symbols(&mut self) {
        for alt in self.prules.iter_mut().flat_map(|r| r.iter_mut().map(|p| &mut p.v)) {
            if alt.len() == 1 && alt[0].is_empty() {
                alt.pop();
            }
        }
    }

    #[allow(unused)]
    fn first_or_follow_to_str(&self, set: &Vec<HashSet<Symbol>>, prefix: &str) -> String {
        let mut result = String::new();
        for var in 0..self.num_nt {
            if !set[var].is_empty() {
                let mut values = set[var].iter().to_vec();
                values.sort();
                result.push_str(&format!(
                    "{prefix}{} -> {}",
                    Symbol::NT(var as VarId).to_str(self.get_symbol_table()),
                    values.iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(", ")));
            }
        }
        result
    }
}


impl ProdRuleSet<LR> {
    /// Adds goal nonterminal and production for extended grammar, required for LR parsing table.
    ///
    /// Returns the original starting nonterminal.
    fn add_lr_goal_nt(&mut self) {
        if !self.has_extra_goal() {
            let orig_start = self.start.unwrap();
            let goal_prod = prule!(nt orig_start);
            self.prules.push(goal_prod);
            self.start = Some(self.num_nt as VarId);
            self.num_nt += 1;
            self.parent.push(None);
            self.flags.push(0);
            self.symbol_table.as_mut().map(|s| {
                let v = s.add_nonterminal("<goal>");
                assert_eq!(v, self.num_nt as VarId - 1);
            });
            self.original_start = Some(orig_start);
        }
    }

    #[cfg(any())]
    /// Removes extended nonterminal and production
    fn remove_lr_goal_nt(&mut self, orig_start: VarId) {
        self.prules.pop().unwrap();
        self.start = Some(orig_start);
        self.num_nt -= 1;
        self.parent.pop();
        self.flags.pop();
        self.symbol_table.as_mut().map(|s| s.remove_nonterminal(self.num_nt as VarId));
    }

    fn item_alt(&self, item: &LRItem) -> &Alternative {
        &self.prules[item.nt as usize][item.alt_idx as usize]
    }

    /// Next symbol of the item, on the right of the dot.
    fn item_symbol(&self, item: &LRItem) -> Option<&Symbol> {
        self.prules[item.nt as usize][item.alt_idx as usize].get(item.pos as usize)
    }

    fn is_item_sym_empty(&self, item: &LRItem) -> bool {
        self.prules[item.nt as usize][item.alt_idx as usize].is_sym_empty()
    }

    pub(crate) fn item_to_str(&self, item: &LRItem) -> String {
        let alt = &self.item_alt(item);
        let left = alt.v[..item.pos as usize].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" ");
        let right = alt.v[item.pos as usize..].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" ");
        format!(
            "{} -> {left}{}•{}{right}{}",
            Symbol::NT(item.nt).to_str(self.get_symbol_table()),
            if !left.is_empty() { " " } else { "" },
            if !right.is_empty() { " " } else { "" },
            if let Some(p) = item.prefix() {
                format!(", [{}]", p.map(|s| self.symbol(s).to_str_quote(self.get_symbol_table())).join(","))
            } else {
                String::new()
            }
        )
    }

    pub(crate) fn items_to_str(&self, items: &[LRItem]) -> String {
        items.iter().map(|i| format!("[{}]", self.item_to_str(i))).join(", ")
    }

    pub(crate) fn states_to_str(&self, states: &[Vec<LRItem>]) -> String {
        states.iter().enumerate()
            .map(|(i, items)| format!("\nstate {i}:{}", items.iter().map(|i| format!("\n  - {}", self.item_to_str(i))).join(""))).join("")
    }

    pub(crate) fn is_item_done(&self, item: &LRItem) -> bool {
        item.pos as usize >= self.item_alt(&item).len()
    }

    fn closure_lr0(&self, items: Vec<LRItem>) -> Vec<LRItem> {
        let mut set_items = HashSet::<LRItem>::from_iter(items);
        loop {
            let n = set_items.len();
            let mut extra = HashSet::new();
            for item in &set_items {
                if let Some(&Symbol::NT(nt)) = self.item_symbol(&item) {
                    for (alt_id, alt) in self.prules[nt as usize].iter().enumerate() {
                        let pos = if alt.is_sym_empty() { 1 } else { 0 };
                        let new_item = item!(nt, alt_id as AltId, pos);
                        extra.insert(new_item.clone());
                    }
                }
            }
            set_items.extend(extra);
            if set_items.len() == n {
                break
            }
        }
        let mut items = Vec::<LRItem>::from_iter(set_items);
        items.sort();
        items
    }

    fn goto_lr0(&self, items: &[LRItem], x: &Symbol) -> Vec<LRItem> {
        let mut s = vec![];
        for item in items {
            if let Some(symbol) = self.item_symbol(&item) {
                if symbol == x {
                    let mut new_item = item.clone();
                    new_item.pos += 1;
                    s.push(new_item);
                }
            }
        }
        self.closure_lr0(s)
    }

    fn calc_states_lr0(&self) -> (Vec<Vec<LRItem>>, Vec<BTreeMap<Symbol, LRStateId>>, Vec<(LRStateId, ItemId)>) {
        const VERBOSE: bool = false;
        let top_nt = self.start.unwrap();
        let mut states = vec![self.closure_lr0(vec![item!(top_nt, 0)])];
        let mut set_states = HashMap::<Vec<LRItem>, LRStateId>::from_iter(states.iter().cloned().index::<LRStateId>().map(|(i, v)| (v, i)));
        let mut gotos = vec![btreemap![]];
        let mut reductions = vec![];
        loop {
            let n = states.len();
            for idx_state in 0..n {
                let state = &states[idx_state];
                let mut new_states = vec![]; // must split because of borrow checker limitation
                let symbols = state.iter()
                    .filter_map(|item| self.item_symbol(item))
                    .collect::<BTreeSet<_>>();
                if VERBOSE {
                    println!("| STATE {idx_state} ----------------------");
                    println!("| items: {}", self.items_to_str(&state));
                    println!("| -> symbols: {}", symbols.iter().map(|s| s.to_str(self.get_symbol_table())).join(", "));
                }
                for symbol in symbols {
                    let items = self.goto_lr0(state.as_slice(), symbol);
                    if !items.is_empty() {
                        if let Some(state_id) = set_states.get(&items) {
                            gotos[idx_state].insert(symbol.clone(), *state_id);
                        } else {
                            let new_state_id = (states.len() + new_states.len()) as LRStateId;
                            if VERBOSE {
                                println!("| -> GOTO(items, {}) = {} => STATE = {new_state_id}", symbol.to_str(self.get_symbol_table()), self.items_to_str(&items));
                            }
                            gotos.push(btreemap![]);
                            gotos[idx_state].insert(symbol.clone(), new_state_id); // [from]: symbol => to
                            reductions.extend(
                                items.iter().index::<ItemId>()
                                    .filter_map(|(id, it)| if self.is_item_done(it) { Some((new_state_id, id)) } else { None }));
                            new_states.push(items.clone());
                            set_states.insert(items, new_state_id);
                        }
                    }
                }
                if VERBOSE && !new_states.is_empty() {
                    println!("| ** new states ** states.len() = {}", states.len());
                    println!("|    {}", new_states.iter().enumerate().map(|(i, x)| format!("{}: {}...", i + states.len(), self.item_to_str(&x[0]))).join(", "));
                }
                states.extend(new_states);
            }
            if set_states.len() == n {
                break;
            }
        }
        // reductions of ε items
        for (state_id, state) in states.iter().index::<LRStateId>() {
            for (item_id, item) in state.iter().index::<ItemId>() {
                if self.is_item_sym_empty(item) {
                    reductions.push((state_id, item_id));
                }
            }
        }
        if VERBOSE {
            println!(
                "calc_states():{}",
                states.iter().enumerate()
                    .map(|(i, items)| format!("\nstate {i}:{}", items.iter().map(|i| format!("\n  - {}", self.item_to_str(i))).join(""))).join(""));
            println!(
                "gotos:{}",
                gotos.iter().enumerate()
                    .map(|(i, g)| format!("\n- {i}: {}", g.iter().map(|(s, t)| format!("{} → {t}", s.to_str_quote(self.get_symbol_table()))).join(", "))).join(""));
        }
        assert!(states.len() < LRStateId::MAX as usize, "too many states ({})", states.len());
        (states, gotos, reductions)
    }

    #[cfg(any())]
    // not necessary unless lookahead is calculated from the reverse production symbols
    // (necessary to profile on bigger grammars to see if more performant)
    fn calc_reverse_gotos(gotos: &[BTreeMap<Symbol, LRStateId>]) -> Vec<BTreeMap<Symbol, Vec<LRStateId>>> {
        let mut rev_gotos = vec![btreemap![]; gotos.len()];
        for (start, symb, dest) in gotos.iter().index::<LRStateId>().flat_map(|(start, g)| g.iter().map(move |(symb, dest)| (start, *symb, *dest))) {
            rev_gotos[dest as usize].entry(symb)
                .and_modify(|v: &mut Vec<LRStateId>| v.push(start))
                .or_insert_with(|| vec![start]);
        }
        rev_gotos
    }

    /// Calculates the LALR lookaheads from LR0 items using Bermudez and Logothetis' algorithm.
    /// Manuel E. Bermudez, George Logothetis, "Simple computation of LALR(1) lookahead sets."
    /// Information Processing Letters, Volume 31, Issue 5, 1989, pp. 233-238.
    /// doi:10.1016/0020-0190(89)90079-3
    fn calc_states_lalr(&mut self) -> (Vec<Vec<LRItem>>, Vec<BTreeMap<Symbol, LRStateId>>, Vec<(LRStateId, ItemId)>) {
        const VERBOSE: bool = false;
        
        self.add_lr_goal_nt();
        let orig_start = self.original_start.unwrap();
        // self.remove_empty_symbols();
        let (mut states, gotos, reductions) = self.calc_states_lr0();
        #[cfg(any())]
        let rev_gotos = Self::calc_reverse_gotos(&gotos);

        // nonterminals and terminals of G', the transition-graph grammar
        let mut symtab_p = SymbolTable::new();          // symbol table for G', for log/debug only
        let mut nts_p = vec![];                         // G' nonterminals
        let mut ts_p = vec![];                          // G' terminals
        let mut nt_to_nt_p = vec![vec![]; self.num_nt]; // nt_to_nt_p[nt] = vec![nt'0, nt'1, ...]
        let mut state_symb_p = vec![];                  // state_symb_p[state].get(symb) = Some(symb')
        for (state, g) in gotos.iter().index::<LRStateId>() {
            let mut map = btreemap![];                  // maps each goto from that state to a symbol of G'
            for &symb in g.keys() {
                match symb {
                    Symbol::T(t) => {
                        map.insert(symb, Symbol::T(ts_p.len() as TokenId)); // luckily, TokenId is the same type as VarId, no need of an enum
                        ts_p.push((state, t));
                        symtab_p.add_terminal(format!("[{state}:{}]", symb.to_str_quote(self.get_symbol_table())), None);
                    }
                    Symbol::NT(nt) => {
                        map.insert(symb, Symbol::NT(nts_p.len() as VarId));
                        nt_to_nt_p[nt as usize].push(nts_p.len() as VarId);
                        nts_p.push((state, nt));
                        symtab_p.add_nonterminal(format!("[{state}:{}]", symb.to_str(self.get_symbol_table())));
                    }
                    Symbol::Empty | Symbol::End => {}
                }
            }
            state_symb_p.push(map);
        }
        let num_nt_p = nts_p.len();
        let num_t_p = ts_p.len();
        if num_nt_p >= VarId::MAX as usize {
            self.log.add_error(format!("too many nonterminals in G' ({num_nt_p})"));
            return (vec![], vec![], vec![]);
        }
        if num_t_p >= VarId::MAX as usize {
            self.log.add_error(format!("too many terminals in G' ({num_t_p})"));
            return (vec![], vec![], vec![]);
        }

        // productions of G'
        let mut prules = vec![ProdRule::new(); num_nt_p];
        let alts = self.prules.iter().enumerate()
            .flat_map(|(nt, alts)| alts.iter().map(move |a| (nt, a)));
        for (nt, Alternative { v: alt, .. }) in alts {
            for &nt_p in &nt_to_nt_p[nt] {
                let mut alt_p = vec![];
                let mut state = nts_p[nt_p as usize].0;
                for symb in alt.iter().filter(|s| !s.is_empty()) {
                    alt_p.push(state_symb_p[state as usize].get(symb).unwrap().clone());
                    state = *gotos[state as usize].get(symb).unwrap();
                }
                if alt_p.is_empty() {
                    alt_p.push(Symbol::Empty);
                }
                prules[nt_p as usize].push(Alternative::new(alt_p));
            }
        }
        if VERBOSE {
            self.print_alts();
            println!("calc_states():{}", self.states_to_str(&states));
            println!(
                "gotos:{}",
                gotos.iter().enumerate()
                    .map(|(i, g)| format!("\n- {i}: {}", g.iter().map(|(s, t)| format!("{} → {t}", s.to_str_quote(self.get_symbol_table()))).join(", "))).join(""));
            #[cfg(any())]
            println!(
                "rev_gotos:{}",
                rev_gotos.iter().enumerate()
                    .map(|(i, g)| format!(
                        "\n- {i}: {}",
                        g.iter().map(|(s, v)| format!("{} → {{{}}}", s.to_str_quote(self.get_symbol_table()), v.iter().map(|s| s.to_string()).join(", "))).join(", "))
                    ).join(""));
            println!("G':");
            symtab_p.dump("");
        }
        let mut g_p = ProdRuleSet::<General> {
            prules: prules,
            origin: Default::default(),
            num_nt: num_nt_p,
            num_t: ts_p.len(),
            symbol_table: Some(symtab_p),
            flags: vec![0; num_nt_p],
            parent: vec![None; num_nt_p],
            start: Some(nt_to_nt_p[orig_start as usize][0]),
            name: None,
            nt_conversion: Default::default(),
            log: Default::default(),
            options: Default::default(),
            first: Vec::new(),
            follow: Vec::new(),
            original_start: None,
            sep_info: SepInfo::None,
            terminal_hooks: Vec::new(),
            _phantom: Default::default(),
        };
        g_p.calc_first();
        g_p.calc_follow();
        for &(state, i_item) in &reductions {
            let item = &mut states[state as usize][i_item as usize];
            if VERBOSE { println!("reduction state {state}: {}", self.item_to_str(item)); }
            let alt = self.item_alt(&item);
            for &nt_p in &nt_to_nt_p[item.nt as usize] {
                let end_state = alt.iter()
                    .filter(|s| !s.is_empty())
                    .fold(nts_p[nt_p as usize].0, |st, symb| *gotos[st as usize].get(symb).unwrap());
                if VERBOSE { print!("- {nt_p}: states {} ---> {end_state}", nts_p[nt_p as usize].0); }
                if end_state == state {
                    let lookahead = g_p.follow[nt_p as usize].iter()
                        .map(|s_p| match s_p {
                            Symbol::T(t_p) => ts_p[*t_p as usize].1,
                            Symbol::NT(_) | Symbol::Empty => { panic!("found {s_p:?} in G' follow set"); }
                            Symbol::End => self.num_t as TokenId,
                        });
                    if VERBOSE { println!(" => [{}]", lookahead.clone().map(|s| self.symbol(s).to_str_quote(self.get_symbol_table())).join(",")); }
                    item.prefix.get_or_insert_default().extend(lookahead);
                } else if VERBOSE {
                    println!(" (no)");
                }
            }
            if item.prefix.is_none() {
                // accept lookahead
                item.prefix = Some(btreeset![self.num_t as TokenId])
            }
        }
        if VERBOSE {
            g_p.print_alts();
            println!("Follow:{}", g_p.first_or_follow_to_str(&g_p.follow, "\n-"));
            println!("States with lookaheads:{}", self.states_to_str(&states));
        }
        #[cfg(any())]
        self.remove_lr_goal_nt(orig_start);
        (states, gotos, reductions)
    }

    pub(crate) fn make_parsing_table_with_states_lalr(&mut self) -> (LRParsingTable, Vec<Vec<LRItem>>) {
        self.log.add_note("- calculating LALR parsing table...");
        let (states, gotos, reductions) = self.calc_states_lalr();
        if !self.log.has_no_errors() {
            return (LRParsingTable::default(), states);
        }
        let num_nt = self.num_nt - 1;       // doesn't include the goal NT
        let num_t_full = self.num_t + 1;    // includes the end symbol
        let num_states = states.len();
        let mut action = vec![LRAction::Error; num_t_full * num_states];
        let mut goto = vec![num_states as LRStateId; num_nt * num_states];    // num_states value = error
        for (s, map) in gotos.into_iter().enumerate() {
            for (symb, s_dest) in map {
                match symb {
                    Symbol::T(t) => action[t as usize + s * num_t_full] = LRAction::Shift(s_dest),
                    Symbol::NT(nt) => goto[nt as usize + s * num_nt] = s_dest,
                    Symbol::Empty => {}
                    Symbol::End => action[self.num_t + s * num_t_full] = LRAction::Shift(s_dest),
                }
            }
        }
        let mut offset = 0;
        let alt_offsets = self.prules.iter().map(|p| {  // nt -> index of its first alt in table alts
            let ofs = offset;
            offset += p.len() as AltId;
            ofs
        }).to_vec();
        for (s, item_id) in reductions {
            let LRItem { nt, alt_idx, prefix, .. } = &states[s as usize][item_id as usize];
            let solve_conflict = self.flags[*nt as usize] & ruleflag::RESOLVE_CONFLICT != 0;
            let action_reduce = LRAction::Reduce(*alt_idx + alt_offsets[*nt as usize]);
            for &t in prefix.as_ref().unwrap() {
                let act = if *nt == self.start.unwrap() {
                    LRAction::Accept
                } else {
                    action_reduce
                };
                let action_cell = &mut action[t as usize + s as usize * num_t_full];
                match (act, *action_cell) {
                    (_, LRAction::Error) => *action_cell = act,
                    (LRAction::Reduce(_), LRAction::Shift(shift)) if solve_conflict => {
                        let mut left_alt_id = *alt_idx as usize;        // left op: what's potentially reduced
                        let nt_red = *nt;
                        let prules = &self.prules[*nt as usize];
                        let (mut is_r, mut is_s) = (false, false);
                        let right_items = states[shift as usize].iter().filter(|item| item.pos > 0).to_vec();
                        for right_item in &right_items {                 // right op: what's potentially shifted
                            let nt_shift = right_item.nt;
                            if nt_shift != nt_red {
                                self.log.add_warning(format!(
                                    "- calc_table: conflict in state {s} for {:?}: {} ({}) vs {} ({}), different nonterminals",
                                    self.symbol(t).to_str(self.get_symbol_table()),
                                    // reduction:
                                    act, prules[left_alt_id].to_rule_str(nt_red, self.get_symbol_table(), 0),
                                    // shift:
                                    *action_cell, self.item_to_str(right_item)));
                            } else {
                                // compare priority of shift_alt_id and alt_id
                                let mut right_alt_id = right_item.alt_idx as usize;
                                let left_alt = &prules[left_alt_id];
                                let is_left_rassoc = left_alt.flags & ruleflag::R_ASSOC != 0;
                                // note: normally, the first alt of this NT mustn't have the flag PREC_EQ, but we check underflow anyway:
                                while left_alt_id > 0 && prules[left_alt_id].flags & ruleflag::PREC_EQ != 0 { left_alt_id -= 1; }
                                while right_alt_id > 0 && prules[right_alt_id].flags & ruleflag::PREC_EQ != 0 { right_alt_id -= 1; }
                                if left_alt_id == right_alt_id {
                                    // same priority: if left is right-assoc => shift, else reduce
                                    if is_left_rassoc { is_s = true; } else { is_r = true; }
                                } else {
                                    // if priority(left) < priority(right) => shift, else reduce
                                    if left_alt_id > right_alt_id { is_s = true; } else { is_r = true; }
                                }
                            }
                        }
                        if is_r != is_s {
                            let resolved = if is_r { act } else { *action_cell };
                            self.log.add_note(format!(
                                "- calc_table: conflict in state {s} for {:?}: {} ({}) vs {} ({}) => resolved as {}",
                                self.symbol(t).to_str(self.get_symbol_table()),
                                // reduction:
                                act, prules[left_alt_id].to_rule_str(nt_red, self.get_symbol_table(), 0),
                                // shift:
                                *action_cell, right_items.iter().map(|r| self.item_to_str(r)).join(", "),
                                resolved));
                            *action_cell = resolved;
                        } else if is_r && is_s {
                            self.log.add_warning(format!(
                                "- calc_table: conflict in state {s} for {:?}: {} ({}) vs {} ({}), conflicting priorities",
                                self.symbol(t).to_str(self.get_symbol_table()),
                                // reduction:
                                act, prules[left_alt_id].to_rule_str(nt_red, self.get_symbol_table(), 0),
                                // shift:
                                *action_cell, right_items.iter().map(|r| self.item_to_str(r)).join(", ")));
                        }
                    }
                    _ => {
                        if act != *action_cell {
                            self.log.add_warning(format!(
                                "- calc_table: conflict in state {s} for {:?}: {}/{}",
                                self.symbol(t).to_str(self.get_symbol_table()),
                                *action_cell, act));
                        }
                    }
                }
            }
        }
        let alts = self.prules.iter().index()
            .flat_map(|(v, x)| x.iter().map(move |a| (v, a.clone())))
            .to_vec();
        let table = LRParsingTable {
            num_nt,
            num_t_full,
            num_states,
            num_goto: num_states,
            alts,
            action,
            goto,
            flags: self.flags.clone(),
            parent: self.parent.clone(),
            init_hook: false,
        };
        (table, states)
    }

    /// Creates the LR parsing table for an LALR parser.
    ///
    /// If an error occurred during the process, it's reported in the log.
    pub fn make_parsing_table_lalr(&mut self, compressed: bool) -> LRParsingTable {
        const BEFORE_ANSI: &str = "\u{1b}[0;37m";
        const AFTER_ANSI : &str = "\u{1b}[0m";
        let (mut table, states) = self.make_parsing_table_with_states_lalr();
        if compressed {
            table.compress_goto();
        }
        table.apply_terminal_hooks(&self.terminal_hooks, &mut self.log);
        let table_str = table.to_str(self.get_symbol_table()).join("\n");
        if self.options.log_states {
            let states_str = states.into_iter().enumerate()
                .map(|(i, items)| format!(
                    "\n- state {i}:{}",
                    items.iter().map(|item| format!(
                        "\n    {}{}{}",
                        if self.options.ansi && !item.is_kernel() { BEFORE_ANSI } else { "" },
                        self.item_to_str(item),
                        if self.options.ansi && !item.is_kernel() { AFTER_ANSI } else { "" },
                    )).join("")
                ))
                .join("");
            self.log.add_note(format!("States:{states_str}"));
        }
        self.log.add_note(format!("Parsing table:\n{table_str}"));
        table
    }
}

impl BuildFrom<ProdRuleSet<General>> for ProdRuleSet<LR> {
    fn build_from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.apply_sep_info_lr();
            rules.adapt_loops();
            rules.transfer_alt_flags();
            rules.check_flags();
            rules.log.add_note("final rule set:");
            rules.log.extend_messages(
                rules.prs_alt_origins_str().into_iter().map(LogMsg::Note)
            );
        }
        ProdRuleSet::<LR> {
            prules: rules.prules,
            origin: rules.origin,
            num_nt: rules.num_nt,
            num_t: rules.num_t,
            symbol_table: rules.symbol_table,
            flags: rules.flags,
            parent: rules.parent,
            start: rules.start,
            name: rules.name,
            nt_conversion: rules.nt_conversion,
            log: rules.log,
            options: rules.options,
            first: rules.first,
            follow: rules.follow,
            original_start: None,
            sep_info: rules.sep_info,
            terminal_hooks: rules.terminal_hooks,
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Default, Clone, Debug)]
pub struct LRParsingTable {
    pub num_nt: usize,                      // doesn't include the goal NT
    pub num_t_full: usize,                  // includes the end symbol
    pub num_states: usize,
    pub num_goto: usize,
    pub alts: Vec<(VarId, Alternative)>,
    pub action: Vec<LRAction>,              // num_states * num_t_full items
    pub goto: Vec<LRStateId>,               // num_goto * num_nt items
    pub flags: Vec<u32>,                    // NT -> flags (+ or * normalization)
    pub parent: Vec<Option<VarId>>,         // NT -> parent NT
    pub init_hook: bool,                    // initial terminal must be intercepted
}

impl LRParsingTable {
    pub(crate) fn symbol(&self, t: TokenId) -> Symbol {
        if self.num_t_full > 1 + t as usize { Symbol::T(t) } else { Symbol::End }
    }

    /// Renumber the states to compress the GOTO table, keeping the used rows at the beginning and
    /// removing the empty ones.
    ///
    /// The resulting GOTO table has `num_goto` * `num_nt` items instead of `num_states` * `num_nt`
    /// for an uncompressed table (`num_goto` <= `num_states`).
    ///
    /// State 0 remains the starting state.
    pub fn compress_goto(&mut self) {
        let (mut first_states, mut last_states): (Vec<LRStateId>, Vec<LRStateId>) = (vec![], vec![]);
        let mut first_goto: Vec<LRStateId> = vec![];
        let (mut first_action, mut last_action): (Vec<LRAction>, Vec<LRAction>) = (vec![], vec![]);
        let goto_empty = self.num_states as LRStateId;
        // the starting state automatically remains in row 0, since that state always has at least one goto to the accepting state,
        // so there's no need to process it differently
        for (state, (gotos, actions)) in self.goto.chunks(self.num_nt).zip(self.action.chunks(self.num_t_full)).index::<LRStateId>() {
            if gotos.iter().any(|goto| *goto != goto_empty) {
                first_states.push(state);
                first_goto.extend(gotos);
                first_action.extend(actions);
            } else {
                last_states.push(state);
                last_action.extend(actions);
            }
        }
        self.num_goto = first_states.len();
        let mut order = vec![self.num_states as LRStateId; self.num_states + 1];
        // updates the state numbers in gotos and shifts:
        for (new, old) in first_states.into_iter().chain(last_states).index() {
            order[old as usize] = new;
        }
        self.goto = first_goto.into_iter().map(|g| order[g as usize]).collect();
        self.action = first_action.into_iter().chain(last_action).map(|a| match a {
            LRAction::Shift(s) => LRAction::Shift(order[s as usize]),
            _ => a,
        }).collect();
    }

    /// Transforms the `action` table and `init_hook` to allow the parser to call a listener
    /// hook before specific tokens are checked. This allows the user to transform any other
    /// token that may be extracted, like an `Id` to a `Type`, before that token influences
    /// the parsing.
    ///
    /// For example, in the following table, state 1 jumps to states 3, 4, or 5 depending on
    /// the token. If `Id` may actually be a declared type, like in some languages, it must
    /// be determined before the token is used, so before jumping to state 1. The `s1` of
    /// state 0 is transformed to `sh0`, which will call the listener hook right after the
    /// token is extracted, but before it's used to fetch the next state.
    ///
    /// ```text
    ///   | Type "{" "}" Id  ";" "=" Num "print"  $  | prog inst | productions
    /// --+------------------------------------------+-----------+--------------------------
    /// 0 |  -   s1   -   -   -   -   -     -     -  |  2    -   | 0: prog -> "{" inst "}"
    /// 1 |  s3   -   -  s4   -   -   -    s5     -  |  -    6   | 1: inst -> Type Id ";"
    /// 2 |  -    -   -   -   -   -   -     -    acc |  -    -   | 2: inst -> Id "=" Num ";"
    /// 3 |  -    -   -  s7   -   -   -     -     -  |  -    -   | 3: inst -> "print" Id ";"
    /// [...]
    /// ```
    pub fn apply_terminal_hooks(&mut self, terminal_hooks: &[TokenId], log: &mut BufLog) {
        log.add_note(format!("apply terminal hooks to parsing table"));
        for &t in terminal_hooks {
            let tu = t as usize;
            // we only need to spot the states where a Shift or a Reduce action is performed
            // on the terminal `t`, and change the Shift actions to those states
            for state in 0..self.num_states {
                if matches!(self.action[state * self.num_t_full + tu], LRAction::Shift(_) | LRAction::ShiftHook(_) | LRAction::Reduce(_)) {
                    // change all the Shift actions going to that state to ShiftHook
                    if state != 0 {
                        let mut n = 0;
                        for action in self.action.iter_mut() {
                            if *action == LRAction::Shift(state as LRStateId) {
                                *action = LRAction::ShiftHook(state as LRStateId);
                                n += 1;
                            }
                        }
                        log.add_note(format!("- token {t}: {n} Shift action(s) to state {state} are changed"));
                    } else {
                        // no shift to state 0, however, we must hook the first terminal
                        log.add_note(format!("- token {t}: used in state 0, so init_hook is true"));
                        self.init_hook = true;
                    }
                }
            }
        }
    }

    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> Vec<String> {
        let mut lines = vec![];
        let &LRParsingTable { num_nt, num_t_full, num_states, ref action, ref goto, .. } = self;
        let max_sw = (num_states as LRStateId - 1).ilog10() as usize + 1;
        let max_sws = LRAction::Shift(num_states as LRStateId - 1).to_string().len()
            .max(LRAction::Shift(num_states as LRStateId - 1).to_string().len())
            .max(LRAction::Reduce(self.alts.len() as AltId - 1).to_string().len());
        let max_ntw = (num_states as VarId - 1).ilog10() as usize + 1;
        let t_str = (0..num_t_full as TokenId).map(|t| self.symbol(t).to_str_quote(symbol_table)).to_vec();
        let t_len = t_str.iter().enumerate().map(|(t, s)|
            s.len() // title
                .max(max_sws) // s15
                .max(if t + 1 < num_t_full { 1 } else { LRAction::Accept.to_string().len() }) // acc
        ).to_vec();
        let nt_str = (0..num_nt as VarId).map(|nt| Symbol::NT(nt).to_str(symbol_table)).to_vec();
        let nt_len = nt_str.iter().map(|s| s.len().max(max_ntw)).to_vec();
        lines.push(format!(
            "{:<w$} | {} | {}", "",
            (0..num_t_full).map(|t| format!("{:^w$}", t_str[t], w = t_len[t])).join(" "),
            (0..num_nt).map(|nt| format!("{:^w$}", nt_str[nt], w = nt_len[nt])).join(" "),
            w = max_sw));
        let line = format!(
            "{:-<w$}-+-{:-<t$}-+-{:-<nt$}", "", "", "",
            t = num_t_full + t_len.iter().sum::<usize>() - 1,
            nt = num_nt + nt_len.iter().sum::<usize>() - 1,
            w = max_sw);
        lines.push(line.clone());
        for s in 0..num_states {
            let action_s = (0..num_t_full).map(|t| format!("{:^w$}", action[s * num_t_full + t].to_string(), w = t_len[t])).join(" ");
            let goto_s = (0..num_nt).map(|nt| {
                if s < self.num_goto {
                    let val = goto[s * num_nt + nt];
                    if num_states > val as usize { format!("{val:^w$}", w = nt_len[nt]) } else { format!("{:^w$}", "-", w = nt_len[nt]) }
                } else {
                    format!("{:^w$}", "", w = nt_len[nt])
                }
            }).join(" ");
            lines.push(format!("{s:>w$} | {action_s} | {goto_s}", w = max_sw));
        }
        lines.push(line);
        lines
    }
}

// ---------------------------------------------------------------------------------------------

mod macros {
    #[macro_export]
    macro_rules! item {
        ($nt:expr, $a:expr , $b:expr) => { $crate::grammar::prs::lr::LRItem { alt_idx: $a, nt: $nt, pos: $b, prefix: None }};
        ($nt:expr, $a:expr) => { item!($nt, $a, 0) };
    }
}