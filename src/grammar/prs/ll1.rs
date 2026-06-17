// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashSet;
use std::marker::PhantomData;
use iter_index::IndexerIterator;
use lexigram_core::{AltId, CollectJoin, TokenId, VarId};
use lexigram_core::alt::{ruleflag, Alternative};
use lexigram_core::log::{BufLog, LogMsg, LogStatus, Logger};
use lexigram_core::parser::Symbol;
use crate::build::BuildFrom;
use crate::{grammar, indent_source, General, SymbolTable, LL1};
use crate::grammar::{calc_alt_first, ProdRuleSet, ProdRuleSetTables, SepInfo};

impl ProdRuleSet<LL1> {
    /// Creates the table for predictive top-down parsing.
    ///
    /// Returns:
    /// - `num_nt` = number of nonterminals
    /// - `num_t` = number of terminals (including the end symbol)
    /// - `alts`, the production alternatives: (VarId, Alternative) where the first value is the non-terminal index and the second one of its alts
    /// - the table of `num_nt * num_t` values, where `table[nt_index * num_nt + t_index]` gives the index of the production alternative for
    ///   the non-terminal index `nt_index` and the terminal index `t_index`. A value >= `alts.len()` stands for a syntax error.
    pub(crate) fn calc_table(&mut self, error_recovery: bool) -> LL1ParsingTable {
        fn add_table(table: &mut [Vec<AltId>], num_t: usize, nt_id: VarId, t_id: VarId, a_id: AltId) {
            let pos = nt_id as usize * num_t + t_id as usize;
            table[pos].push(a_id);
        }
        const VERBOSE: bool = false;
        if !self.log.has_no_errors() {
            return LL1ParsingTable::new();
        }
        if VERBOSE {
            fn print_first_or_follow(title: &str, tbl: Option<&SymbolTable>, table: &Vec<HashSet<Symbol>>) {
                let syms = table.iter().map(|hs| {
                    let mut f = hs.iter().cloned().to_vec();
                    f.sort();
                    f
                }).to_vec();
                println!(
                    "{title}\n{}",
                    syms.into_iter().index::<VarId>().filter(|(_, v)| !v.is_empty())
                        .map(|(s, f)| format!("- {} -> {}", Symbol::NT(s).to_str(tbl), f.into_iter().map(|s2| s2.to_str(tbl)).join(", "))).join("\n"));
            }
            print_first_or_follow("first:", self.get_symbol_table(), &self.first);
            print_first_or_follow("follow:", self.get_symbol_table(), &self.follow);
        }
        let mut alts = self.prules.iter().index()
            .flat_map(|(v, x)| x.iter().map(move |a| (v, a.clone())))
            .to_vec();
        let error_skip = alts.len() as AltId;   // table entry for syntactic error; recovery by skipping input symbol
        let error_pop = error_skip + 1;         // table entry for syntactic error; recovery by popping T or NT from stack
        let num_nt = self.num_nt;
        let num_t_full = self.num_t + 1;
        let end = (num_t_full - 1) as VarId; // index of end symbol
        let mut used_t = HashSet::<Symbol>::new();
        let mut table: Vec<Vec<AltId>> = vec![vec![]; num_nt * num_t_full];
        for (a_id, (nt_id, alt)) in alts.iter().index() {
            used_t.extend(alt.iter().filter(|s| s.is_t()));
            if VERBOSE { println!("- {a_id}: {} -> {}  => {}", Symbol::NT(*nt_id).to_str(self.get_symbol_table()),
                                  alt.to_str(self.get_symbol_table()),
                                  calc_alt_first(alt, &self.first).iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut has_end = false;
            let mut has_empty = false;
            for s in calc_alt_first(alt, &self.first) {
                match s {
                    Symbol::Empty => {
                        has_empty = true;
                        for s in &self.follow[*nt_id as usize] {
                            match s {
                                Symbol::T(t_id) => add_table(&mut table, num_t_full, *nt_id, *t_id, a_id),
                                Symbol::End     => add_table(&mut table, num_t_full, *nt_id, end, a_id),
                                _ => {}
                            }
                        }
                    }
                    Symbol::T(t_id) => {
                        add_table(&mut table, num_t_full, *nt_id, t_id, a_id);
                    }
                    Symbol::NT(_) => {}
                    Symbol::End => {
                        has_end = true;
                    }
                }
            }
            if has_empty && has_end {
                add_table(&mut table, num_t_full, *nt_id, end, end);
            }
        }
        // creates the table and removes ambiguities
        let mut final_table = Vec::<AltId>::new();
        for nt_id in 0..num_nt {
            for t_id in 0..num_t_full {
                let pos = nt_id * num_t_full + t_id;
                final_table.push(match table[pos].len() {
                    0 => {
                        if error_recovery {
                            let sym_t = if t_id < num_t_full - 1 { Symbol::T(t_id as TokenId) } else { Symbol::End };
                            if self.follow[nt_id].contains(&sym_t) || self.first[nt_id].contains(&sym_t) {
                                error_pop
                            } else {
                                error_skip
                            }
                        } else {
                            error_skip
                        }
                    },
                    1 => *table[pos].first().unwrap(),
                    _ => {
                        // we take the first item which isn't already in another position on the same NT row
                        let greedies = table[pos].iter().filter(|&a_id| alts[*a_id as usize].1.is_greedy()).cloned().to_vec();
                        if greedies.len() == 1 {
                            let chosen = greedies[0];
                            self.log.add_note(
                                format!("  - calc_table: expected ambiguity for NT '{}', T '{}': {} => <{}> is specified as greedy and has been chosen",
                                        Symbol::NT(nt_id as VarId).to_str(self.get_symbol_table()),
                                        if t_id < self.num_t { Symbol::T(t_id as VarId).to_str(self.get_symbol_table()) } else { "<EOF>".to_string() },
                                        table[pos].iter().map(|a_id|
                                            format!("<{}>", alts[*a_id as usize].1.to_str(self.get_symbol_table()))).join(" or "),
                                        alts[chosen as usize].1.to_str(self.get_symbol_table())
                                ));
                            table[pos] = greedies;
                            chosen
                        } else {
                            let row = (0..num_t_full).filter(|j| *j != t_id).flat_map(|j| &table[nt_id * num_t_full + j]).collect::<HashSet<_>>();
                            let chosen = *table[pos].iter().find(|a| !row.contains(a)).unwrap_or(&table[pos][0]);
                            self.log.add_warning(
                                format!("- calc_table: ambiguity for NT '{}', T '{}': {} => <{}> has been chosen",
                                        Symbol::NT(nt_id as VarId).to_str(self.get_symbol_table()),
                                        if t_id < self.num_t { Symbol::T(t_id as VarId).to_str(self.get_symbol_table()) } else { "<EOF>".to_string() },
                                        table[pos].iter().map(|a_id|
                                            format!("<{}>", alts[*a_id as usize].1.to_str(self.get_symbol_table()))).join(" or "),
                                        alts[chosen as usize].1.to_str(self.get_symbol_table())
                                ));
                            table[pos] = vec![chosen];
                            chosen
                        }
                    }
                });
            }
        }
        if !(0..num_t_full - 1).any(|t_id| (0..num_nt).any(|nt_id| final_table[nt_id * num_t_full + t_id] < error_skip)) {
            self.log.add_error("- calc_table: no terminal used in the table".to_string());
        }
        for (_, a) in &mut alts {
            a.flags &= !ruleflag::GREEDY;
        }
        let table = LL1ParsingTable { num_nt, num_t_full, alts, table: final_table, flags: self.flags.clone(), parent: self.parent.clone() };
        self.log.add_info("parsing table:");
        self.log.extend_messages(
            table.to_str(self.get_symbol_table()).into_iter()
                .map(LogMsg::Info)
        );
        table
    }

    pub fn make_parsing_table(&mut self, error_recovery: bool) -> LL1ParsingTable {
        self.log.add_note("- calculating parsing table...");
        self.calc_first();
        self.calc_follow();
        self.calc_table(error_recovery)
    }

    pub fn gen_tables_source_code(&self, indent: usize) -> String {
        assert!(self.first.is_empty(), "first & follow fields aren't empty");
        let st = self.symbol_table.as_ref().unwrap();
        let mut source = Vec::<String>::new();
        // "origin" preparation
        source.push(format!("static ORIGIN: [(Option<usize>, &[(GrNode, &[usize])]); {}] = [", self.origin.trees.len()));
        for t in &self.origin.trees {
            let tree_str = (0..t.len())
                .map(|i| format!("({}, &[{}])", t.get(i).gen_source_code(), t.children(i).iter().join(",")))
                .join(", ");
            source.push(format!("    ({:?}, &[{}]),", t.get_root(), tree_str));
        }
        source.push("];".to_string());
        source.push(format!("static MAP: [(VarId, (VarId, usize)); {}] = [", self.origin.map.len()));
        let mut sorted_map = self.origin.map.iter().to_vec();
        sorted_map.sort(); // we must sort it so that its output is reproducible
        source.extend(sorted_map.chunks(5)
            .map(|chk| format!("    {},", chk.iter().map(|(a, (c, d))| format!("({a}, ({c}, {d}))")).join(", "))));
        source.push("];".to_string());
        source.push("let origin = Origin::from_data(".to_string());
        source.push("    ORIGIN.into_iter().map(|(root, nodes)| GrTree::from((root, nodes.to_vec()))).collect(),".to_string());
        source.push("    HashMap::from(MAP));".to_string());
        // ProdRuleSetTables:
        source.push(String::new());
        source.push("let ll1_tables = ProdRuleSetTables::new(".to_string());
        source.push(format!("    {:?},", self.name));
        source.push("    vec![".to_string());
        source.extend(self.prules.iter().map(|prule| format!("        {},", grammar::prule_to_macro(prule))));
        source.push("    ],".to_string());
        source.push("    origin,".to_string());
        source.push(format!("    vec![{}],", st.get_terminals().map(|x| format!("{x:?}")).join(", ")));
        source.push(format!("    vec![{}],", st.get_nonterminals().map(|x| format!("{x:?}")).join(", ")));
        source.push(format!("    vec![{}],", self.flags.iter().join(", ")));
        source.push(format!("    vec![{}],", self.parent.iter().map(|p_maybe| format!("{p_maybe:?}")).join(", ")));
        source.push(format!("    {:?},", self.start));
        source.push(format!("    {:?},", self.options));
        source.push(format!("    hashmap![{}],", self.nt_conversion.iter().map(|(v, conv)| format!("{v} => {conv:?}")).join(", ")));
        let SepInfo::Nt(sep_nt) = &self.sep_info else { panic!() };
        source.push(format!("    vec![{}]", sep_nt.iter().map(|i| format!("{i:?}")).join(", ")));
        source.push(");".to_string());
        indent_source(vec![source], indent)
    }
}

impl BuildFrom<ProdRuleSetTables> for ProdRuleSet<LL1> {
    fn build_from(source: ProdRuleSetTables) -> Self {
        let mut symbol_table = SymbolTable::new();
        symbol_table.extend_terminals(source.t);
        symbol_table.extend_nonterminals(source.nt);
        ProdRuleSet {
            prules: source.prules,
            origin: source.origin,
            num_nt: symbol_table.get_num_nt(),
            num_t: symbol_table.get_num_t(),
            symbol_table: Some(symbol_table),
            flags: source.flags,
            parent: source.parent,
            start: source.start,
            name: source.name,
            nt_conversion: source.nt_conversion,
            log: BufLog::new(),
            options: source.options,
            first: Vec::new(),
            follow: Vec::new(),
            original_start: None,
            sep_info: SepInfo::Nt(source.sep_nt),
            _phantom: PhantomData,
        }
    }
}

impl BuildFrom<ProdRuleSet<General>> for ProdRuleSet<LL1> {
    fn build_from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.apply_sep_info_ll1();
            rules.remove_recursion();
            rules.left_factorize();
            rules.transfer_alt_flags();
            rules.check_flags();
            rules.log.add_note("final rule set:");
            rules.log.extend_messages(
                rules.prs_alt_origins_str().into_iter().map(LogMsg::Note)
            );
        }
        ProdRuleSet::<LL1> {
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
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct LL1ParsingTable {
    pub num_nt: usize,
    pub num_t_full: usize,               // includes the end $ symbol
    pub alts: Vec<(VarId, Alternative)>,
    pub table: Vec<AltId>,
    pub flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    pub parent: Vec<Option<VarId>>, // NT -> parent NT
}

impl LL1ParsingTable {
    pub fn new() -> Self {
        LL1ParsingTable { num_nt: 0, num_t_full: 0, alts: vec![], table: vec![], flags: vec![], parent: vec![] }
    }
}

impl Default for LL1ParsingTable {
    fn default() -> Self {
        Self::new()
    }
}

impl LL1ParsingTable {
    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> Vec<String> {
        let LL1ParsingTable { num_nt, num_t_full: num_t, alts, table, .. } = self;
        let error_skip = alts.len() as AltId;
        let error_pop = error_skip + 1;
        let str_nt = (0..*num_nt).map(|i| Symbol::NT(i as VarId).to_str(symbol_table)).to_vec();
        let max_nt_len = str_nt.iter().map(|s| s.len()).max().unwrap();
        let str_t = (0..*num_t).map(|j| if j + 1 < *num_t { Symbol::T(j as VarId).to_str_quote(symbol_table) } else { "$".to_string() }).to_vec();
        let t_len = str_t.iter().map(|s| s.len().max(3)).to_vec();
        let mut lines = vec![];
        lines.push(format!("{:<w$} | {}", "", (0..*num_t).map(|j| format!("{:^w$}", str_t[j], w = t_len[j])).join(" "), w = max_nt_len));
        lines.push(format!("{:-<w$}-+-{:-<t$}", "", "", w = max_nt_len, t = *num_t + t_len.iter().sum::<usize>()));
        for i in 0..*num_nt {
            let mut line = format!("{:<w$} |", str_nt[i], w = max_nt_len);
            for j in 0..*num_t {
                let value = table[i * num_t + j];
                if value < error_skip {
                    line.push_str(&format!(" {:^w$}", value, w = t_len[j]));
                } else {
                    line.push_str(&format!(" {:^w$}", if value == error_pop { "p" } else { "." }, w = t_len[j]));
                }
            }
            lines.push(line);
        }
        lines
    }

    pub fn print(&self, symbol_table: Option<&SymbolTable>, indent: usize) {
        let lines = self.to_str(symbol_table);
        for line in lines {
            println!("{:<1$}// {line}", "", indent);
        }
    }
}