use std::collections::{BTreeSet, HashMap, HashSet};
use std::marker::PhantomData;
use lexigram_core::log::{LogStatus, Logger};
use lexigram_core::parser::Symbol;
use lexigram_core::{CollectJoin, VarId};
use crate::build::BuildFrom;
use crate::grammar::ProdRuleSet;
use crate::{item, prule, General, LR};

trait LRItem {
    fn pos(&self) -> DotPos;
    fn alt_idx(&self) -> u16;
    fn prefix(&self) -> Option<&[Symbol]> { None }
}

/// Dot position in a production rule (alternative). The symbol after the dot is at [value as usize],
/// if it exists.
pub type DotPos = u16;

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

    fn first_or_follow_to_str(&self, set: &HashMap<Symbol, HashSet<Symbol>>, prefix: &str) -> String {
        let mut result = String::new();
        let mut keys = set.keys().to_vec();
        keys.sort();
        for s in keys {
            let mut values = set.get(s).unwrap().iter().to_vec();
            values.sort();
            result.push_str(&format!(
                "{prefix}{} -> {}",
                s.to_str(self.get_symbol_table()),
                values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")));
        }
        result
    }
}


impl ProdRuleSet<LR> {
    fn item_to_str<T: LRItem>(&self, item: &T) -> String {
        let (var_id, alt) = &self.alts[item.alt_idx() as usize];
        let left = alt.v[..item.pos() as usize].iter().map(|s| s.to_str(self.get_symbol_table())).join(" ");
        let right = alt.v[item.pos() as usize..].iter().map(|s| s.to_str(self.get_symbol_table())).join(" ");
        format!(
            "{} -> {left}{}•{}{right}{}",
            Symbol::NT(*var_id).to_str(self.get_symbol_table()),
            if !left.is_empty() { " " } else { "" },
            if !right.is_empty() { " " } else { "" },
            if let Some(p) = item.prefix() { format!(", {}", p.iter().map(|s| s.to_str(self.get_symbol_table())).join("/")) } else { String::new() }
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

    pub fn calc_states_lr0(&self) -> Vec<Vec<LR0Item>> {
        const VERBOSE: bool = true;

        let top_rule = self.nt_alts[self.start.unwrap() as usize].0;
        let mut states = vec![self.closure_lr0(vec![item!(top_rule)])];
        let mut set_states = HashSet::<Vec<LR0Item>>::from_iter(states.iter().cloned());
        loop {
            let n = states.len();
            for idx_state in 0..n {
                let state = &states[idx_state];
                let mut new_states = vec![]; // must split because of borrow checker limitation
                let symbols = state.iter()
                    .filter_map(|&LR0Item { alt_idx, pos }| self.alts[alt_idx as usize].1.get(pos as usize))
                    .collect::<BTreeSet<_>>();
                if VERBOSE {
                    println!("| items: {}", self.items_to_str(&state));
                    println!("| -> symbols: {}", symbols.iter().map(|s| s.to_str(self.get_symbol_table())).join(", "));
                }
                for symbol in symbols {
                    let items = self.goto_lr0(state.as_slice(), symbol);
                    if VERBOSE {
                        println!("| -> GOTO(items, {}) = {}", symbol.to_str(self.get_symbol_table()), self.items_to_str(&items));
                    }
                    if !items.is_empty() && !set_states.contains(&items) {
                        set_states.insert(items.clone());
                        new_states.push(items);
                    }
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
                    .map(|(i, items)| format!("\nstate {i}:{}", items.iter().map(|i| format!("\n  - {}", self.item_to_str(i))).join(""))).join(""))
        }
        states
    }

    pub fn make_parsing_table(&mut self, _error_recovery: bool) -> LRParsingTable {
        const VERBOSE: bool = true;
        self.log.add_note("- calculating parsing table...");
        let orig_start = self.add_goal_nt();
        self.calc_first();
        self.calc_follow();
        if VERBOSE {
            let first = self.first_or_follow_to_str(&self.first, "\n- ");
            println!("first:{first}");
            let follow = self.first_or_follow_to_str(&self.follow, "\n- ");
            println!("follow:{follow}");
        }
        self.calc_alts();
        let _states = self.calc_states_lr0();
        self.remove_goal_nt(orig_start);
        LRParsingTable { }
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

#[derive(Debug)]
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