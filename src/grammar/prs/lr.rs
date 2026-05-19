use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::marker::PhantomData;
use iter_index::IndexerIterator;
use lexigram_core::log::{LogStatus, Logger};
use lexigram_core::parser::Symbol;
use lexigram_core::{CollectJoin, TokenId, VarId};
use lexigram_core::alt::Alternative;
use crate::build::BuildFrom;
use crate::grammar::{ProdRule, ProdRuleSet};
use crate::{btreemap, item, prule, General, SymbolTable, LR};

trait LRItem {
    fn pos(&self) -> DotPos;
    fn alt_idx(&self) -> u16;
    fn prefix(&self) -> Option<&[Symbol]> { None }
}

/// Dot position in a production rule (alternative). The symbol after the dot is at [value as usize],
/// if it exists.
pub type DotPos = u16;

pub type StateId = u32;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct LR0Item {
    pub pos: DotPos,
    pub alt_idx: u16,
}

impl LRItem for LR0Item {
    fn pos(&self) -> DotPos {
        self.pos
    }

    fn alt_idx(&self) -> u16 {
        self.alt_idx
    }
}

impl<T> ProdRuleSet<T> {
    /// Adds goal nonterminal and production for extended grammar, required for LR parsing table.
    ///
    /// Returns the original starting nonterminal.
    fn add_goal_nt(&mut self) -> VarId {
        let orig_start = self.start.unwrap();
        let goal_prod = prule!(nt orig_start);
        self.prules.as_mut().unwrap().push(goal_prod);
        self.start = Some(self.num_nt as VarId);
        self.num_nt += 1;
        self.parent.push(None);
        self.flags.push(0);
        self.symbol_table.as_mut().map(|s| {
            let v = s.add_nonterminal("<goal>");
            assert_eq!(v, self.num_nt as VarId - 1);
        });
        orig_start
    }

    /// Removes extended nonterminal and production
    fn remove_goal_nt(&mut self, orig_start: VarId) {
        self.prules.as_mut().unwrap().pop().unwrap();
        self.start = Some(orig_start);
        self.num_nt -= 1;
        self.parent.pop();
        self.flags.pop();
        self.symbol_table.as_mut().map(|s| s.remove_nonterminal(self.num_nt as VarId));
    }

    fn remove_empty_symbols(&mut self) {
        for alt in self.prules.as_mut().unwrap().iter_mut().flat_map(|r| r.iter_mut().map(|p| &mut p.v)) {
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
    fn item_to_str<T: LRItem>(&self, item: &T) -> String {
        let (var_id, alt) = &self.alts[item.alt_idx() as usize];
        let left = alt.v[..item.pos() as usize].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" ");
        let right = alt.v[item.pos() as usize..].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" ");
        format!(
            "{} -> {left}{}•{}{right}{}",
            Symbol::NT(*var_id).to_str(self.get_symbol_table()),
            if !left.is_empty() { " " } else { "" },
            if !right.is_empty() { " " } else { "" },
            if let Some(p) = item.prefix() { format!(", {}", p.iter().map(|s| s.to_str_quote(self.get_symbol_table())).join("/")) } else { String::new() }
        )
    }

    fn items_to_str<T: LRItem>(&self, items: &[T]) -> String {
        items.iter().map(|i| format!("[{}]", self.item_to_str(i))).join(", ")
    }

    fn closure_lr0(&self, mut items: Vec<LR0Item>) -> Vec<LR0Item> {
        let mut set_items = HashSet::<LR0Item>::from_iter(items.clone());
        loop {
            let n = items.len();
            for idx_item in 0..n {
                let item = &items[idx_item];
                if let Some(Symbol::NT(nt)) = self.alts[item.alt_idx as usize].1.get(item.pos as usize) {
                    let &(start, end) = &self.nt_alts[*nt as usize];
                    for alt_id in start..end {
                        let new_item = item!(alt_id);
                        if !set_items.contains(&new_item) {
                            set_items.insert(new_item.clone());
                            items.push(new_item);
                        }
                    }
                }
            }
            if items.len() == n {
                break;
            }
        }
        items
    }

    fn goto_lr0(&self, items: &[LR0Item], x: &Symbol) -> Vec<LR0Item> {
        let mut s = vec![];
        for &LR0Item { alt_idx, pos } in items {
            if let Some(symbol) = self.alts[alt_idx as usize].1.get(pos as usize) {
                if symbol == x {
                    s.push(item!(alt_idx, pos + 1));
                }
            }
        }
        self.closure_lr0(s)
    }

    pub fn calc_states_lr0(&self) -> (Vec<Vec<LR0Item>>, Vec<BTreeMap<Symbol, StateId>>) {
        const VERBOSE: bool = false;

        let top_rule = self.nt_alts[self.start.unwrap() as usize].0;
        let mut states = vec![self.closure_lr0(vec![item!(top_rule)])];
        let mut set_states = HashMap::<Vec<LR0Item>, StateId>::from_iter(states.iter().cloned().index::<StateId>().map(|(i, v)| (v, i)));
        let mut gotos = vec![btreemap![]];
        loop {
            let n = states.len();
            for idx_state in 0..n {
                let state = &states[idx_state];
                let mut new_states = vec![]; // must split because of borrow checker limitation
                let symbols = state.iter()
                    .filter_map(|&LR0Item { alt_idx, pos }| self.alts[alt_idx as usize].1.get(pos as usize))
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
                            let new_state_id = states.len() + new_states.len();
                            if VERBOSE {
                                println!("| -> GOTO(items, {}) = {} => STATE = {new_state_id}", symbol.to_str(self.get_symbol_table()), self.items_to_str(&items));
                            }
                            gotos.push(btreemap![]);
                            gotos[idx_state].insert(symbol.clone(), new_state_id as StateId); // [from]: symbol => to
                            new_states.push(items.clone());
                            set_states.insert(items, new_state_id as StateId);
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
        assert!(states.len() < StateId::MAX as usize, "too many states ({})", states.len());
        (states, gotos)
    }

    fn calc_reverse_gotos(gotos: &[BTreeMap<Symbol, StateId>]) -> Vec<BTreeMap<Symbol, Vec<StateId>>> {
        let mut rev_gotos = vec![btreemap![]; gotos.len()];
        for (start, symb, dest) in gotos.iter().index::<StateId>().flat_map(|(start, g)| g.iter().map(move |(symb, dest)| (start, *symb, *dest))) {
            rev_gotos[dest as usize].entry(symb)
                .and_modify(|v: &mut Vec<StateId>| v.push(start))
                .or_insert_with(|| vec![start]);
        }
        rev_gotos
    }

    /// Makes an LALR parsing table using Bermudez and Logothetis' algorithm.
    /// Bermudez, Manuel. “Simple Computation of LALR(1) Lookahead Sets.” Information Processing Letters, 1989.
    /// Manuel E. Bermudez, George Logothetis, "Simple computation of LALR(1) lookahead sets."
    /// Information Processing Letters, Volume 31, Issue 5, 1989, pp. 233-238.
    /// doi:10.1016/0020-0190(89)90079-3
    pub fn make_parsing_table_lalr(&mut self, _error_recovery: bool) -> Result<LRParsingTable, ()> {
        const VERBOSE: bool = true;
        self.log.add_note("- calculating LALR parsing table...");
        let orig_start = self.add_goal_nt();
        self.remove_empty_symbols();
        self.calc_alts();
        let (states, gotos) = self.calc_states_lr0();
        let rev_gotos = Self::calc_reverse_gotos(&gotos);

        // nonterminals and terminals of G', the transition-graph grammar
        let mut symtab_p = SymbolTable::new();          // symbol table for G', for log/debug only
        let mut nts_p = vec![];                         // G' nonterminals
        let mut ts_p = vec![];                          // G' terminals
        let mut nt_to_nt_p = vec![vec![]; self.num_nt]; // nt_to_nt_p[nt] = vec![nt'0, nt'1, ...]
        let mut state_symb_p = vec![];                  // state_symb_p[state].get(symb) = Some(symb')
        for (state, g) in gotos.iter().index::<StateId>() {
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
            return Err(());
        }
        if num_t_p >= VarId::MAX as usize {
            self.log.add_error(format!("too many terminals in G' ({num_t_p})"));
            return Err(());
        }

        // productions of G'
        let mut prules = vec![ProdRule::new(); num_nt_p];
        for (nt, rule) in self.prules.as_ref().unwrap().iter().index::<VarId>() {
            for alt in rule.iter().map(|a| &a.v) {
                for nt_p in &nt_to_nt_p[nt as usize] {
                    let mut alt_p = vec![];
                    let mut state = nts_p[*nt_p as usize].0;
                    for symb in alt {
                        alt_p.push(state_symb_p[state as usize].get(symb).unwrap().clone());
                        state = *gotos[state as usize].get(symb).unwrap();
                    }
                    if alt_p.is_empty() {
                        alt_p.push(Symbol::Empty);
                    }
                    prules[*nt_p as usize].push(Alternative::new(alt_p));
                }
            }
        }
        if VERBOSE {
            self.print_alts();
            println!(
                "calc_states():{}",
                states.iter().enumerate()
                    .map(|(i, items)| format!("\nstate {i}:{}", items.iter().map(|i| format!("\n  - {}", self.item_to_str(i))).join(""))).join(""));
            println!(
                "gotos:{}",
                gotos.iter().enumerate()
                    .map(|(i, g)| format!("\n- {i}: {}", g.iter().map(|(s, t)| format!("{} → {t}", s.to_str_quote(self.get_symbol_table()))).join(", "))).join(""));
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
            prules: Some(prules),
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
            alts: vec![],
            nt_alts: vec![],
            first: vec![],
            follow: vec![],
            _phantom: Default::default(),
        };
        g_p.calc_first();
        g_p.calc_follow();

        if VERBOSE {
            g_p.print_alts();
            println!("Follow:{}", g_p.first_or_follow_to_str(&g_p.follow, "\n-"));
        }

        self.remove_goal_nt(orig_start);
        Ok(LRParsingTable { })
    }
}

impl BuildFrom<ProdRuleSet<General>> for ProdRuleSet<LR> {
    fn build_from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.remove_ambiguity();
            rules.transfer_alt_flags();
            rules.check_flags();
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
            alts: rules.alts,
            nt_alts: rules.nt_alts,
            first: rules.first,
            follow: rules.follow,
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct LRParsingTable {
}

// ---------------------------------------------------------------------------------------------

mod macros {
    #[macro_export]
    macro_rules! item {
        ($a:expr, $b:expr) => { $crate::grammar::prs::lr::LR0Item { alt_idx: $a, pos: $b }};
        ($a:expr) => { item!($a, 0) };
    }
}