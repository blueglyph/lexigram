use std::collections::{BTreeSet, HashMap, HashSet};
use std::marker::PhantomData;
use iter_index::IndexerIterator;
use lexigram_core::alt::Alternative;
use lexigram_core::log::{LogStatus, Logger};
use lexigram_core::parser::Symbol;
use lexigram_core::{CollectJoin, VarId};
use crate::build::BuildFrom;
use crate::{prule, General, SymbolTable, LR};
use crate::grammar::ProdRuleSet;

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
}

impl ProdRuleSet<LR> {
    pub fn make_parsing_table(&mut self, _error_recovery: bool) -> Vec<Vec<LRItem>> {
        self.log.add_note("- calculating parsing table...");
        let orig_start = self.add_goal_nt();
        let first: HashMap<Symbol, HashSet<Symbol>> = self.calc_first();
        let follow: HashMap<Symbol, HashSet<Symbol>> = self.calc_follow(&first);
        let mut nt_idx: VarId = 0;
        let nt_alts = self.prules.as_ref().unwrap().iter()
            .map(|p| {
                let len: VarId = p.len().try_into().expect("too many productions");
                let value = (nt_idx, nt_idx.checked_add(len).expect("too many productions"));
                nt_idx = value.1;
                value
            })
            .to_vec();
        let alts = self.prules.as_ref().unwrap().iter().index()
            .flat_map(|(v, x)| x.iter().map(move |a| (v, a.clone())))
            .to_vec();
        let table = LRParsingTable {
            num_nt: self.num_nt,
            num_t: self.num_t,
            alts,
            nt_alts,
            flags: self.flags.clone(),
            parent: self.parent.clone(),
            first,
            follow,
            start: self.start.unwrap(),
            orig_start,
            symbol_table: self.symbol_table.as_ref(),
        };
        let states = table.calc_states();
        self.remove_goal_nt(orig_start);
        states
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
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

/// Dot position in a production rule (alternative). The symbol after the dot is at [value as usize],
/// if it exists.
pub type DotPos = u16;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct LRItem {
    pub pos: DotPos,
    pub alt_idx: u16,
}

#[macro_export]
macro_rules! item {
    ($a:expr, $b:expr) => { LRItem { alt_idx: $a, pos: $b }};
    ($a:expr) => { LRItem { alt_idx: $a, pos: 0 }};
}

#[derive(Debug)]
pub struct LRParsingTable<'a> {
    pub num_nt: usize,
    pub num_t: usize,                   // includes the end $ symbol
    pub alts: Vec<(VarId, Alternative)>,
    pub nt_alts: Vec<(VarId, VarId)>,   // (first, last+1) in alts for each NT
    pub flags: Vec<u32>,                // NT -> flags (+ or * normalization)
    pub parent: Vec<Option<VarId>>,     // NT -> parent NT
    pub first: HashMap<Symbol, HashSet<Symbol>>,
    pub follow: HashMap<Symbol, HashSet<Symbol>>,
    pub start: VarId,                   // S' -> S
    pub orig_start: VarId,              // S
    symbol_table: Option<&'a SymbolTable>,
}

impl LRParsingTable<'_> {
    const VERBOSE: bool = true;

    fn item_to_str(&self, item: &LRItem) -> String {
        let (var_id, alt) = &self.alts[item.alt_idx as usize];
        let left = alt.v[..item.pos as usize].iter().map(|s| s.to_str(self.symbol_table)).join(" ");
        let right = alt.v[item.pos as usize..].iter().map(|s| s.to_str(self.symbol_table)).join(" ");
        format!(
            "{} -> {left}{}•{}{right}",
            Symbol::NT(*var_id).to_str(self.symbol_table),
            if !left.is_empty() { " " } else { "" },
            if !right.is_empty() { " " } else { "" })
    }

    fn items_to_str(&self, items: &[LRItem]) -> String {
        items.iter().map(|i| format!("[{}]", self.item_to_str(i))).join(", ")
    }

    fn closure(&self, items: Vec<LRItem>) -> Vec<LRItem> {
        let mut set_items = HashSet::<LRItem>::from_iter(items);
        loop {
            let n = set_items.len();
            let mut added = HashSet::<LRItem>::new();
            for item in &set_items {
                if let Some(Symbol::NT(nt)) = self.alts[item.alt_idx as usize].1.get(item.pos as usize) {
                    let &(start, end) = &self.nt_alts[*nt as usize];
                    for alt_id in start..end {
                        added.insert(item!(alt_id));
                    }
                }
            }
            set_items.extend(added);
            if set_items.len() == n {
                break;
            }
        }
        let mut new_items = Vec::<LRItem>::from_iter(set_items);
        new_items.sort();
        new_items
    }

    fn goto(&self, items: &[LRItem], x: &Symbol) -> Vec<LRItem> {
        let mut s = vec![];
        for &LRItem { alt_idx, pos } in items {
            if let Some(symbol) = self.alts[alt_idx as usize].1.get(pos as usize) {
                if symbol == x {
                    s.push(item!(alt_idx, pos + 1));
                }
            }
        }
        self.closure(s)
    }

    pub fn calc_states(&self) -> Vec<Vec<LRItem>> {
        let initial_items = self.closure(vec![item!(self.nt_alts[self.start as usize].0)]);
        let mut s = HashSet::<Vec<LRItem>>::from_iter([initial_items]);
        loop {
            let n = s.len();
            let mut added = HashSet::<Vec<LRItem>>::new();
            for items in &s {
                let symbols = items.into_iter()
                    .filter_map(|&LRItem { alt_idx, pos }| self.alts[alt_idx as usize].1.get(pos as usize))
                    .collect::<BTreeSet<_>>();
                if Self::VERBOSE {
                    println!("| items: {}", self.items_to_str(items));
                    println!("| -> symbols: {}", symbols.iter().map(|s| s.to_str(self.symbol_table)).join(", "));
                }
                for symbol in symbols {
                    let items = self.goto(items.as_slice(), symbol);
                    if Self::VERBOSE {
                        println!("| -> GOTO(items, {}) = {}", symbol.to_str(self.symbol_table), self.items_to_str(&items));
                    }
                    if !items.is_empty() {
                        added.insert(items);
                    }
                }
            }
            s.extend(added);
            if s.len() == n {
                break;
            }
        }
        let mut states = Vec::<Vec<LRItem>>::from_iter(s);
        states.sort();
        if Self::VERBOSE {
            println!(
                "calc_states():{}",
                states.iter().enumerate()
                    .map(|(i, items)| format!("\nstate {i}:{}", items.iter().map(|i| format!("\n  - {}", self.item_to_str(i))).join(""))).join(""))
        }
        states
    }
}
