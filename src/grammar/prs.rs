// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use super::*;
use crate::{columns_to_str, AltId, SymbolTable, VarId};
use lexigram_core::alt::{alt_to_rule_str, Alternative};
use lexigram_core::CollectJoin;

/// Stores a normalized production rule, where each alternative (e.g. `B C`) is stored in
/// a `Vec<GrNode>` and all the alternatives are stored in a `Vec`.
///
/// ## Example
/// `A -> B C | D | ε`
///
/// where the nonterminal indices A=0, B=1, C=2, D=3, is stored as:
///
/// `[[gnode!(nt 1), gnode!(nt 2)],[gnode!(nt 3)],[gnode!(e)]]`
///
/// (where the representation of vectors & type names has been simplified to square brackets).
pub type ProdRule = Vec<Alternative>;

pub fn prule_to_str(prule: &ProdRule, symbol_table: Option<&SymbolTable>) -> String {
    prule.iter().map(|alt| alt.to_str(symbol_table)).join(" | ")
}

pub fn prule_to_rule_str(nt: VarId, prule: &ProdRule, symbol_table: Option<&SymbolTable>) -> String {
    format!("{} -> {}", Symbol::NT(nt).to_str(symbol_table), prule.iter().map(|alt| alt.to_str(symbol_table)).join(" | "))

}

pub fn prule_to_macro(prule: &ProdRule) -> String {
    format!("prule!({})", prule.iter().map(|alt| alt.to_macro_item()).join("; "))
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum AltType { Independant, LeftAssoc, Prefix, RightAssoc, Suffix }

impl AltType {
    fn from(nt: &Symbol, alt: &Alternative) -> Self {
        let left = matches!(alt.first(), Some(var) if var == nt);
        let right = alt.len() > 1 && matches!(alt.last(), Some(var) if var == nt);
        match (left, right) {
            (false, false) => AltType::Independant,
            (false, true)  => AltType::Prefix,
            (true, false)  => AltType::Suffix,
            (true, true)   => if alt.flags & ruleflag::R_ASSOC != 0 { AltType::RightAssoc } else { AltType::LeftAssoc },
        }
    }
}

#[derive(Debug, Clone)]
struct AltInfo {
    #[allow(unused)]
    pred_priority: Option<AltId>,
    ivar: usize,
    ty: AltType
}

#[derive(Debug)]
pub struct LLParsingTable {
    pub num_nt: usize,
    pub num_t: usize,
    pub alts: Vec<(VarId, Alternative)>,
    pub table: Vec<AltId>,
    pub flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    pub parent: Vec<Option<VarId>>, // NT -> parent NT
}

impl LLParsingTable {
    pub fn new() -> Self {
        LLParsingTable { num_nt: 0, num_t: 0, alts: vec![], table: vec![], flags: vec![], parent: vec![] }
    }

    pub fn get_top_parent(&self, nt: VarId) -> VarId {
        let mut var = nt;
        while let Some(parent) = self.parent[var as usize] {
            var = parent;
        }
        var
    }
}

#[derive(Clone, Debug)]
pub struct ProdRuleSet<T> {
    pub(super) prules: Vec<ProdRule>,
    pub(crate) origin: Origin<VarId, FromPRS>,
    pub(super) num_nt: usize,
    pub(super) num_t: usize,
    pub(crate) symbol_table: Option<SymbolTable>,
    pub(super) flags: Vec<u32>,
    pub(super) parent: Vec<Option<VarId>>,
    pub(super) start: Option<VarId>,
    pub(crate) name: Option<String>,
    pub(crate) nt_conversion: HashMap<VarId, NTConversion>,
    pub(crate) log: BufLog,
    pub(super) _phantom: PhantomData<T>
}

impl<T> ProdRuleSet<T> {
    /// Returns the starting production rule.
    pub fn get_start(&self) -> Option<VarId> {
        self.start
    }

    /// Sets the starting production rule.
    pub fn set_start(&mut self, start: VarId) {
        self.start = Some(start);
    }

    pub fn get_name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    pub fn set_name(&mut self, name: Option<String>) {
        self.name = name;
    }

    /// Returns a variable ID that doesn't exist yet.
    pub fn get_next_available_var(&self) -> VarId {
        self.prules.len() as VarId   // we don't use self.num_nt for safety reason
    }

    /// Returns all the non-empty prules
    pub fn get_prules_iter(&self) -> impl Iterator<Item=(VarId, &ProdRule)> {
        self.prules.iter().index().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id, p)) })
    }

    pub fn get_prules_iter_mut(&mut self) -> impl Iterator<Item=(VarId, &mut ProdRule)> {
        self.prules.iter_mut().enumerate().filter_map(|(id, p)| if p.is_empty() { None } else { Some((id as VarId, p)) })
    }

    pub fn get_alts(&self) -> impl Iterator<Item=(VarId, &Alternative)> {
        self.prules.iter().enumerate()
            .flat_map(|(v, p)| p.iter().map(move |alt| (v as VarId, alt)))
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table);
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        self.symbol_table.as_ref()
    }

    pub fn get_num_nt(&self) -> usize {
        self.num_nt
    }

    pub fn get_num_t(&self) -> usize {
        self.num_t
    }

    /// Adds new flags to `flags[nt]` by or'ing them.
    /// If necessary, extends the `flags` array first.
    pub fn set_flags(&mut self, nt: VarId, new_flags: u32) {
        let nt = nt as usize;
        if nt >= self.flags.len() {
            self.flags.resize(nt + 1, 0);
        }
        self.flags[nt] |= new_flags;
    }

    pub fn get_flags(&self, nt: VarId) -> u32 {
        let nt = nt as usize;
        if nt < self.flags.len() {
            self.flags[nt]
        } else {
            0
        }
    }

    pub(super) fn set_parent(&mut self, child: VarId, parent: VarId) {
        let child = child as usize;
        if child >= self.parent.len() {
            self.parent.resize(child + 1, None);
        }
        self.parent[child] = Some(parent);
    }

    #[allow(unused)]
    #[cfg(test)] // we keep it here because we might use it later in logs
    pub(super) fn get_parent(&self, child: VarId) -> Option<VarId> {
        let child = child as usize;
        if child >= self.parent.len() {
            None
        } else {
            self.parent[child]
        }
    }

    fn get_top_parent(&self, nt: VarId) -> VarId {
        let mut var = nt;
        while let Some(parent) = self.parent[var as usize] {
            var = parent;
        }
        var
    }

    /// Calculates `num_t` and `num_nt` (done right after importing rules).
    /// - `num_t` is calculated on the basis of the higher symbol found in the production rules,
    /// so we can drop any unused symbol that is higher and keep the table width down. We can't
    /// compact the table by removing lower unused symbols, if any, because they are defined by
    /// the lexer.
    /// - `num_nt` is simply the number of production rules.
    pub(super) fn calc_num_symbols(&mut self) {
        self.num_nt = self.prules.len();
        self.num_t = self.prules.iter().map(|p|
            p.iter().map(|f|
                f.iter().filter_map(|s|
                    if let Symbol::T(v) = s { Some(*v + 1) } else { None }
                ).max().unwrap_or(0)
            ).max().unwrap_or(0)
        ).max().unwrap_or(0) as usize;
        self.symbol_table.as_mut().map(|st| st.downsize_num_t(self.num_t));
        self.flags.resize(self.num_nt, 0);
        self.parent.resize(self.num_nt, None);
    }

    /// Simplifies the productions by removing unnecessary empty symbols.
    pub(super) fn simplify(&mut self) {
        for p in &mut self.prules {
            let mut has_empty = false;
            let mut i = 0;
            while i < p.len() {
                let alt = p.get_mut(i).unwrap();
                let mut j = 0;
                while j < alt.len() {
                    if alt[j].is_empty() && (j > 0 || j + 1 < alt.len()) {
                        alt.v.remove(j);
                    } else {
                        j += 1;
                    }
                }
                if alt.len() == 1 && alt[0].is_empty() {
                    if has_empty {
                        p.remove(i);
                    } else {
                        has_empty = true;
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }
        }
    }

    fn check_num_nt_coherency(&mut self) {
        if let Some(n) = self.symbol_table.as_ref().and_then(|table| Some(table.get_num_nt())) {
            let num_nt = self.prules.len();
            if n != num_nt {
                self.log.add_error(format!("there are {num_nt} rules but the symbol table has {n} nonterminal symbols: dropping the table"));
                self.symbol_table = None;
            }
        }
    }

    /// Removes the unused nonterminals and renumbers everything accordingly.
    /// Note that we don't remove unused T symbols because it would create a coherency problem with the lexer.
    ///
    /// Returns the conversion `HashMap[old symbol => new symbol]` for nonterminals.
    fn cleanup_symbols(&mut self, keep: &mut HashSet<Symbol>) {
        const VERBOSE: bool = false;
        let mut nt_content = (0..self.num_nt).map(|v| if self.parent[v].is_none() { Some(v as VarId) } else { None }).to_vec();
        for (from, cnv) in self.nt_conversion.iter() {
            // right now, it only contains MovedTo items from the normalization
            let MovedTo(to) = cnv else { panic!("{cnv:?} unexpected") };
            nt_content[*from as usize] = None;
            nt_content[*to as usize] = Some(*from);
        }
        if VERBOSE {
            println!("Removing unused nonterminals:");
            let mut all_h = self.prules.iter().flat_map(|p| p.iter().map(|x| &x.v).flatten()).cloned().collect::<HashSet<_>>();
            all_h.extend((0..self.num_nt).map(|i| Symbol::NT(i as VarId)));
            let mut all = all_h.into_iter().collect::<Vec<_>>();
            all.sort();
            println!("  current NT symbols: {}", all.iter().filter_map(|s|
                if let Symbol::NT(_) = s { Some(format!("{}", s.to_str(self.get_symbol_table()))) } else { None }).join(", "));
            println!("  current  T symbols: {}", all.iter().filter_map(|s|
                if let Symbol::T(_) = s { Some(s.to_str(self.get_symbol_table())) } else { None }).join(" "));
            let mut used = keep.iter().collect::<Vec<_>>();
            used.sort();
            println!("  used NT symbols:    {}", used.iter().filter_map(|s| if let Symbol::NT(v) = s { Some((v, s)) } else { None })
                .enumerate().map(|(new_id, (id, s))| format!("{}({id} -> {new_id})", s.to_str(self.get_symbol_table()))).join(", "));
            println!("  used  T symbols:    {}", used.iter().filter_map(|s|
                if let Symbol::T(_) = s { Some(s.to_str(self.get_symbol_table())) } else { None }).join(" "));
            println!("  nt_conversion: {:?}", self.nt_conversion);
            println!("  nt_content: {}", nt_content.iter().enumerate()
                .filter_map(|(v, f)| if let Some(from) = f { Some(format!("{v}:{from}")) } else { None } )
                .join(", ")
            );
        }
        self.nt_conversion.clear();
        let new_num_nt = keep.iter().filter(|s| matches!(s, Symbol::NT(_))).count() as VarId;
        let mut new_v = new_num_nt;
        let mut conv = HashMap::<VarId, VarId>::new();
        for i in (0..self.num_nt).rev() {
            let v = i as VarId;
            let symbol = Symbol::NT(v);
            if !keep.contains(&symbol) {
                if VERBOSE { println!("- deleting {}", symbol.to_str(self.get_symbol_table())); }
                if self.parent[i].is_none() {
                    self.nt_conversion.insert(v, Removed);
                }
                nt_content.remove(i);
                self.prules.remove(i);
                self.start = self.start.map(|s| if s >= v { s - 1 } else { s });
                self.symbol_table.as_mut().map(|t| t.remove_nonterminal(v));
                self.flags.remove(i);
                self.parent.remove(i);
                if self.origin.trees.len() > i {
                    self.origin.trees.remove(i);
                }

            } else {
                new_v -= 1;
                conv.insert(v, new_v);
                if VERBOSE { println!("- {symbol:?} -> {:?}", Symbol::NT(new_v)); }
            }
        }
        for p in &mut self.prules {
            for f in p {
                for s in &mut f.v {
                    if let Symbol::NT(s_var) = s {
                        if let Some(new) = conv.get(s_var) {
                            *s = Symbol::NT(*new);
                        }
                    }
                }
                if let Some((ref mut var, _id)) = f.origin {
                    if let Some(new_var) = conv.get(&var) {
                        *var = *new_var;
                    }
                }
            }
        }
        for p in &mut self.parent {
            if let Some(parent) = p {
                if let Some(new) = conv.get(parent) {
                    *p = Some(*new);
                }
            }
        }
        for t in &mut self.origin.trees {
            for mut node in t.iter_depth_simple_mut() {
                match *node {
                    GrNode::Symbol(Symbol::NT(ref mut v)) => {
                        if let Some(new_v) = conv.get(&v) {
                            *v = *new_v;
                        }
                    }
                    _ => {}
                }
            }
        }
        let new_map = self.origin.map.iter()
            .map(|(v1, (v2, id))| (*conv.get(&v1).unwrap_or(&v1), (*conv.get(&v2).unwrap_or(&v2), *id)))
            .collect::<HashMap<_, _>>();
        // println!("old origin map: {:?}", self.origin.map);
        // println!("new origin map: {:?}", new_map);
        // println!("trees:\n{}", self.origin.trees.iter().enumerate().map(|(i, t)| format!("{i}:{}", grtree_to_str(t, None, None, None))).join("\n"));
        self.origin.map = new_map;
        keep.retain(|s| !matches!(s, Symbol::NT(_)));
        keep.extend((0..new_num_nt as VarId).map(|v| Symbol::NT(v)));
        self.num_nt = new_num_nt as usize;
        if VERBOSE {
            println!("-> nt_content: {}", nt_content.iter().enumerate()
                .filter_map(|(v, f)| if let Some(from) = f { Some(format!("{v}:{from}")) } else { None })
                .join(", ")
            );
        }
        for (to, f) in nt_content.into_iter().index() {
            if let Some(from) = f {
                if from != to {
                    self.nt_conversion.insert(from, MovedTo(to as VarId));
                } else {
                    if self.nt_conversion.get(&from) == Some(&Removed) {
                        self.nt_conversion.remove(&from);
                    }
                }
            }
        };
        if VERBOSE { println!("-> nt_conversion: {:?}", self.nt_conversion); }
    }

    pub fn calc_first(&mut self) -> HashMap<Symbol, HashSet<Symbol>> {
        const VERBOSE: bool = false;
        if self.start.is_none() {
            self.log.add_error("calc_first: start NT symbol not defined");
        }
        if self.num_nt == 0 {
            self.log.add_error("calc_first: no nonterminal in grammar".to_string());
        }
        if !self.log.has_no_errors() {
            return HashMap::new();
        }
        let mut symbols = HashSet::<Symbol>::new();
        let mut stack = vec![Symbol::NT(self.start.unwrap())];
        while let Some(sym) = stack.pop() {
            if !symbols.contains(&sym) {
                symbols.insert(sym);
                if let Symbol::NT(v) = sym {
                    stack.extend(self.prules[v as usize].iter().map(|x| &x.v).flatten());
                }
            }
        }
        if symbols.iter().filter(|s| matches!(s, Symbol::NT(_))).count() != self.num_nt {
            let nt_removed = (0..self.num_nt as VarId)
                // warnings about symbols that are not used but that have not been moved because of a */+ L-form:
                .filter(|v| !symbols.contains(&Symbol::NT(*v)) && !matches!(self.nt_conversion.get(v), Some(MovedTo(_))))
                .map(|v| Symbol::NT(v))
                .to_vec();
            if !nt_removed.is_empty() {
                self.log.add_warning(format!("calc_first: unused nonterminals: {}",
                                             nt_removed.into_iter().map(|s| format!("{:?} = {}", s, s.to_str(self.get_symbol_table()))).join(", ")));
            }
            self.cleanup_symbols(&mut symbols);
        }

        let unused_t = (0..self.num_t)
            .filter_map(|t_id| {
                let s = Symbol::T(t_id as VarId);
                if !symbols.contains(&s) { Some(format!("T({t_id}) = {}", s.to_str(self.get_symbol_table()))) } else { None }
            }).to_vec();
        if unused_t.len() > 0 {
            self.log.add_warning(format!("calc_first: unused terminals: {}", unused_t.join(", ")))
        }

        let mut first = symbols.into_iter().map(|sym| {
            match &sym {
                Symbol::T(_) | Symbol::Empty => (sym, hashset![sym]),
                Symbol::NT(_) => (sym, HashSet::new()),
                Symbol::End => panic!("found reserved symbol {sym:?} in production rules"),
            }
        }).collect::<HashMap<_, _>>();
        let mut change = true;
        let rules = (0..self.num_nt as VarId).filter(|var| first.contains_key(&Symbol::NT(*var))).to_vec();
        if VERBOSE { println!("rules: {}", rules.iter().map(|v| Symbol::NT(*v).to_str(self.symbol_table.as_ref())).join(", ")); }
        while change {
            change = false;
            for i in &rules {
                let prule = &self.prules[*i as usize];
                let symbol = Symbol::NT(*i as VarId);
                if VERBOSE { println!("- {} -> {}", symbol.to_str(self.symbol_table.as_ref()), prule_to_str(prule, self.symbol_table.as_ref())); }
                let num_items = first[&symbol].len();
                for alt in prule {
                    if VERBOSE { println!("  - {}", alt.to_str(self.symbol_table.as_ref())); }
                    assert!(alt.len() > 0, "empty alternative for {}: {}",
                            symbol.to_str(self.symbol_table.as_ref()), alt.to_str(self.symbol_table.as_ref()));
                    if VERBOSE {
                        print!("    [0] {}", alt[0].to_str(self.symbol_table.as_ref()));
                        println!(", first = {}", first[&alt[0]].iter().map(|s| s.to_str(self.symbol_table.as_ref())).join(", "));
                    }
                    let new = alt.calc_alt_first(&first);
                    let _n = first.get(&symbol).unwrap().len();
                    first.get_mut(&symbol).unwrap().extend(new);
                    if VERBOSE {
                        if first.get(&symbol).unwrap().len() > _n {
                            println!("    first[{}] -> {}", symbol.to_str(self.get_symbol_table()),
                                     first.get(&symbol).unwrap().iter().map(|s| s.to_str(self.get_symbol_table())).join(", "));
                        }
                    }
                }
                change |= first[&symbol].len() > num_items;
            }
            if VERBOSE && change { println!("---------------------------- again"); }
        }
        if self.num_t == 0 {
            self.log.add_error("calc_first: no terminal in grammar".to_string());
        }
        first
    }

    pub fn calc_follow(&self, first: &HashMap<Symbol, HashSet<Symbol>>) -> HashMap<Symbol, HashSet<Symbol>> {
        const VERBOSE: bool = false;
        assert!(self.start.is_some(), "start NT symbol not defined");
        if !self.log.has_no_errors() {
            return HashMap::new();
        }
        let mut follow = first.iter()
            .filter_map(|(s, _)| if matches!(s, Symbol::NT(_)) { Some((*s, HashSet::<Symbol>::new())) } else { None })
            .collect::<HashMap<_, _>>();
        follow.get_mut(&Symbol::NT(self.start.unwrap())).unwrap().insert(Symbol::End);
        let rules = (0..self.num_nt as VarId).filter(|var| follow.contains_key(&Symbol::NT(*var))).to_vec();
        let mut change = true;
        while change {
            change = false;
            for i in &rules {
                let prule = &self.prules[*i as usize];
                let symbol = Symbol::NT(*i as VarId);
                if VERBOSE { println!("- {} -> {}", symbol.to_str(self.symbol_table.as_ref()), prule_to_str(prule, self.symbol_table.as_ref())); }
                for alt in prule {
                    if VERBOSE { println!("  - {}", alt.to_str(self.symbol_table.as_ref())); }
                    let mut trail = follow.get(&symbol).unwrap().clone();
                    for sym_i in alt.iter().rev() {
                        if let Symbol::NT(_) = sym_i {
                            let num_items = follow.get(sym_i).unwrap().len();
                            follow.get_mut(sym_i).unwrap().extend(&trail);
                            if VERBOSE {
                                if follow.get(sym_i).unwrap().len() > num_items {
                                    println!("    follow[{}] -> {}", sym_i.to_str(self.get_symbol_table()),
                                             follow.get(sym_i).unwrap().iter().map(|s| s.to_str(self.get_symbol_table())).join(", "));
                                }
                            }
                            change |= follow.get(sym_i).unwrap().len() > num_items;
                            if first[sym_i].contains(&Symbol::Empty) {
                                trail.extend(first[sym_i].iter().filter(|s| *s != &Symbol::Empty));
                            } else {
                                trail.clear();
                                trail.extend(&first[sym_i]);
                            }
                        } else {
                            trail.clear();
                            trail.insert(*sym_i);
                        }
                    }
                }
            }
            if VERBOSE && change { println!("---------------------------- again"); }
        }
        follow
    }

    /// Eliminates recursion from production rules, removes potential ambiguity, and updates the symbol table if provided.
    /// ```eq
    /// A -> αi A | A βj | A γk A | δl
    /// ```
    /// becomes
    /// ```eq
    /// A[p] -> A[indep] ( βj | γk E[pk] )*
    /// ```
    /// then
    /// ```eq
    /// A[p]  -> A[indep] Ab[p]
    /// Ab[p] -> βj Ab[p] | γk A[var] Ab[p] | ε
    /// ```
    pub(super) fn remove_recursion(&mut self) {
        /// Maximum number of P/I alternatives that are distributed before creating a new nonterminal to hold them.
        /// They are never distributed in presence of a binary (L/R) because it would likely induce left factorization.
        const MAX_DISTRIB_LEN: Option<usize> = None; // always distributing makes for smaller tables
        const DONT_DISTRIB_IN_AMBIG: bool = true;   // E -> E * E | E + E | F  will keep F in an independent NT

        const VERBOSE: bool = false;

        self.log.add_note("removing left / binary recursion in grammar...");
        self.check_num_nt_coherency();
        if VERBOSE {
            println!("ORIGINAL:");
            self.print_rules(false, false);
        }
        let mut var_new = self.get_next_available_var() as usize;
        // we must take prules out because of the borrow checker and other &mut borrows we need later...
        let mut prules = take(&mut self.prules);
        let mut ambig_alt_id = 0;
        for var in 0..var_new {
            let prule = prules.get_mut(var).unwrap();
            let var = var as VarId;
            let symbol = Symbol::NT(var);
            let var_name = symbol.to_str(self.get_symbol_table());
            let mut extra_prods = Vec::<ProdRule>::new();
            if prule.iter().any(|p| !p.is_empty() && (p.first().unwrap() == &symbol)) {
                let orig_str = format!("{var_name} -> {}", prule_to_str(prule, self.get_symbol_table()));
                self.log.add_note(format!("- modifying: {orig_str}"));
                if VERBOSE { println!("processing: {orig_str}"); }

                let (indep, mut alts) : (Vec<_>, Vec<_>) = take(prule).into_iter()
                    .partition(|alt| alt.first().unwrap() != &symbol && alt.last().unwrap() != &symbol);
                alts.reverse();
                let mut prec_eq = alts.iter_mut()
                    .map(|f| {
                        let p = f.flags & ruleflag::PREC_EQ != 0;
                        f.flags &= !ruleflag::PREC_EQ;
                        p
                    }).to_vec();
                prec_eq.pop();
                prec_eq.insert(0, false);
                if indep.is_empty() {
                    self.log.add_error(format!("recursive rules must have at least one independent alternative: {} -> {}",
                                               Symbol::NT(var).to_str(self.get_symbol_table()),
                                               prule_to_str(prule, self.get_symbol_table())));
                    prule.extend(alts);
                    prule.extend(indep);
                    continue;
                }

                // below, the variable indices are mostly related to the variables that will be created to transform
                // the current rule. E[0] is the current `var`, E[1], ..., E[n-1] the children.

                let mut var_i = 0;        // index of E[i] variable used in a rule (... | α[i] E[i] | ...)
                let mut rule_var_i = 0;   // index of E[i] variable defined as a rule (E[i] -> ...)
                let mut var_alts: Vec<Vec<AltId>> = vec![vec![]]; // pr_rule[i] = alternatives present in E[i]
                let mut indep_alts = Vec::<AltId>::new();
                let mut pr_info = Vec::<AltInfo>::new();  // information on each alternative: type, priority, ...
                let mut has_ambig = false;

                for (i, alt) in alts.iter().index::<AltId>() {
                    let ty = AltType::from(&symbol, alt);
                    has_ambig |= ty == AltType::LeftAssoc || ty == AltType::RightAssoc;
                    var_i = match ty {
                        AltType::Independant => panic!("there can't be an independent alternative in `alts`"),
                        AltType::LeftAssoc => if prec_eq[i as usize] { var_i } else { var_i + 1 },
                        AltType::Prefix
                        | AltType::RightAssoc => if var_i > rule_var_i || prec_eq[i as usize] || var_alts[var_i].is_empty() { var_i } else { var_i + 1 },
                        AltType::Suffix => var_i,
                    };
                    let alt_info = AltInfo {
                        pred_priority: if ty == AltType::Prefix { None } else { Some(i) },
                        ivar: var_i,
                        ty
                    };
                    pr_info.push(alt_info);
                    let top_maybe = match ty {
                        AltType::Independant => panic!("there can't be an independent alternative in `alts`"),
                        AltType::LeftAssoc => Some(var_i - 1),    // uses alternative in rules of < priority
                        AltType::Prefix => None,                  // uses alternative in independent rule only
                        AltType::RightAssoc
                        | AltType::Suffix => Some(var_i),         // uses alternative in rules of <= priority
                    };
                    if let Some(top) = top_maybe {
                        rule_var_i = top;
                        var_alts.resize(1 + top, vec![]);
                        (0..=top).for_each(|v| var_alts[v].push(i));
                    } else {
                        indep_alts.push(i);
                    }
                    let var_alt_str = format!(
                        "[{i:2}] {:15}: {:10} => var_i = {var_i:2}, rule_var_i = {rule_var_i:2}, {{{}}}",
                        alt.to_str(self.get_symbol_table()),
                        format!("{ty:?}"), // "debug ignores width" bug https://github.com/rust-lang/rust/issues/55584
                        var_alts.iter().enumerate().map(|(i, va)| format!("{i}: {}", va.iter().join(","))).join("  "));
                    self.log.add_note(format!("  - {var_alt_str}"));
                    if VERBOSE { println!("- {var_alt_str}"); }
                };
                assert!(var_i <= rule_var_i + 1, "var_i = {var_i}, rule_var_i = {rule_var_i}");

                // (var, prime) for each rule except independent alternatives.
                // CAUTION! Includes the independent NT if last op is left-assoc

                let need_indep = indep.len() + indep_alts.len() > 1
                    && (MAX_DISTRIB_LEN.map(|max| indep.len() + indep_alts.len() > max).unwrap_or(false) || has_ambig || rule_var_i < var_i)
                    || DONT_DISTRIB_IN_AMBIG && has_ambig;
                let num_indep = if need_indep { 1 } else { 0 };
                let mut var_i_nt = Vec::<(VarId, VarId)>::with_capacity(var_i + 1);
                var_i_nt.push((var, var_new as VarId));
                var_i_nt.extend((0..var_i).map(|i| ((var_new + i*2 + 1) as VarId, (var_new + i*2 + 2) as VarId)));
                var_new += rule_var_i * 2 + 1 + num_indep;
                if VERBOSE { println!("adding {} variables (w/o independent alternatives), need_indep = {need_indep}, var_new: {} -> {var_new}",
                                      rule_var_i * 2 + 1 + num_indep, var_new - (rule_var_i * 2 + 1 + num_indep)); }
                let nt_indep_maybe = if need_indep { Some(var_new as VarId - 1) } else { None };
                if var_new > VarId::MAX as usize {
                    self.log.add_error(format!("too many nonterminals when expanding {var_name}: {var_new} > {}", VarId::MAX));
                    return;
                }

                // prepares the operation alternative parts, which will be assembled in each rule according to their priority
                // (the Ab[p] loop nonterminals will be added later since they're specific to each rule)
                let new_alts = alts.iter().zip(&pr_info).map(|(a, AltInfo { ivar, ty, .. })| {
                    let mut new_a: Alternative;
                    match ty {
                        AltType::LeftAssoc | AltType::RightAssoc => {
                            new_a = Alternative::new(a.v[1..a.len() - 1].to_owned());
                            if need_indep || *ivar <= rule_var_i {
                                new_a.v.push(Symbol::NT(var_i_nt[*ivar].0));
                            } else {
                                new_a.v.extend(&indep[0].v);
                            }
                            if ty == &AltType::RightAssoc {
                                new_a.flags = ruleflag::R_ASSOC;
                            }
                        }
                        AltType::Suffix => {
                            new_a = Alternative::new(a.v[1..a.len()].to_owned());
                        }
                        AltType::Prefix => {
                            new_a = Alternative::new(a.v[..a.len() - 1].to_owned());
                            new_a.v.push(Symbol::NT(var_i_nt[*ivar].0));
                        }
                        AltType::Independant => panic!("there can't be an independent alternative in `alts`"),
                    }
                    new_a.flags |= a.flags & ruleflag::ALTERNATIVE_INFO;
                    new_a.origin = a.origin;
                    new_a.ambig_alt_id = Some(ambig_alt_id);
                    ambig_alt_id += 1;
                    new_a
                }).to_vec();
                let mut used_sym = HashSet::<Symbol>::new();
                let mut prod_indep = new_alts.iter()
                    .zip(&pr_info)
                    .filter_map(|(nf, AltInfo { ty, .. })| if ty == &AltType::Prefix { Some(nf.clone()) } else { None })
                    .to_vec();
                // when the lowest priority alternative is P or R, all the alternatives
                // of the first variable rule will sprout an ambiguity:
                let greedy_prologue = pr_info[0].ty == AltType::Prefix && need_indep || pr_info[0].ty == AltType::RightAssoc;
                for (i, va) in var_alts.into_iter().enumerate() {
                    let (nt, nt_loop) = var_i_nt[i];
                    let prod_nt = if let Some(nt_indep) = nt_indep_maybe {
                        prule!(nt nt_indep, nt nt_loop)
                    } else {
                        // distributes the independent alternatives (only works if there are no L/R types in the original rule)
                        prod_indep.iter().cloned()
                            .chain(indep.iter().map(|a| {
                            let mut new_a = a.clone();
                            new_a.push(sym!(nt nt_loop));
                            new_a
                        })).collect()
                    };
                    let mut new_used_sym = Vec::<Symbol>::new();
                    let mut prod_nt_loop = va.iter().enumerate().rev().map(|(_, &a_id)| {
                        let mut f = new_alts[a_id as usize].clone();
                        f.v.push(Symbol::NT(nt_loop));
                        let sym = f.first().unwrap();
                        let is_used_sym = used_sym.contains(sym);
                        if !is_used_sym {
                            new_used_sym.push(*sym);
                        }
                        if is_used_sym || (i == 0 && greedy_prologue) {
                            f.flags |= ruleflag::GREEDY;
                        }
                        if !has_ambig && pr_info[a_id as usize].ty == AltType::Suffix {
                            self.set_flags(nt, ruleflag::PARENT_L_RECURSION);
                            self.set_flags(nt_loop, ruleflag::CHILD_L_RECURSION);
                        }
                        f
                    }).to_vec();
                    used_sym.extend(new_used_sym);
                    prod_nt_loop.push(alt!(e));
                    if i == 0 {
                        *prule = prod_nt;
                        extra_prods.push(prod_nt_loop);
                        self.symbol_table.as_mut().map(|t| {
                            assert_eq!(t.add_child_nonterminal(var), var_i_nt[0].1);
                        });
                    } else {
                        extra_prods.extend([prod_nt, prod_nt_loop]);
                        self.symbol_table.as_mut().map(|t| {
                            assert_eq!(t.add_child_nonterminal(var), var_i_nt[i].0);
                            assert_eq!(t.add_child_nonterminal(var), var_i_nt[i].1);
                        });
                    }
                    if has_ambig {
                        self.set_flags(nt, ruleflag::PARENT_L_RECURSION);
                        self.set_flags(nt_loop, ruleflag::CHILD_L_RECURSION);
                    }
                }
                if need_indep {
                    self.symbol_table.as_mut().map(|t| {
                        assert_eq!(t.add_child_nonterminal(var), nt_indep_maybe.unwrap());
                    });
                }
                if VERBOSE {
                    println!("new alternatives: {}", new_alts.iter().enumerate()
                        .filter_map(|(i, na)| if na.is_empty() { None } else { Some(format!("[{i}] {}", na.to_str(self.get_symbol_table()))) }).join(", "));
                    println!("vars: {}{}", var_i_nt.iter().take(rule_var_i + 1).enumerate().map(|(i, (v1, v2))|
                        format!("[{i}]:{},{}", Symbol::NT(*v1).to_str(self.get_symbol_table()),
                                Symbol::NT(*v2).to_str(self.get_symbol_table()))).join("  "),
                                if need_indep { format!("  [{}]:{}", var_i, Symbol::NT(var_i_nt[var_i].0).to_str(self.get_symbol_table() )) } else { String::new() });
                }
                if has_ambig {
                    self.set_flags(var, ruleflag::PARENT_AMBIGUITY);
                } else if !prod_indep.is_empty() && !need_indep {
                    self.set_flags(var, ruleflag::R_RECURSION);
                }
                for (nt, nt_prime) in var_i_nt.into_iter().take(rule_var_i + 1) {
                    if nt != var {
                        self.set_parent(nt, var);
                    }
                    self.set_parent(nt_prime, nt);
                }
                if let Some(nt_indep) = nt_indep_maybe {
                    if !has_ambig && prod_indep.iter().any(|p| p.last() == Some(&Symbol::NT(nt_indep)))
                        || has_ambig && !prod_indep.is_empty()
                    {
                        self.set_flags(nt_indep, ruleflag::R_RECURSION);
                    }
                    prod_indep.extend(indep);
                    self.set_parent(nt_indep, var);
                    extra_prods.push(prod_indep);
                }
                self.parent.resize(var_new, None);
                self.flags.resize(var_new, 0);
                self.log.add_note(format!("  => {var_name} -> {}", prule_to_str(prule, self.get_symbol_table())));
                for (v, p) in extra_prods.iter().index_start(prules.len() as VarId) {
                    self.log.add_note(
                        format!("     {} -> {}", Symbol::NT(v).to_str(self.get_symbol_table()), prule_to_str(p, self.get_symbol_table())));
                }
            } else if prule.iter().any(|p| !p.is_empty() && p.last().unwrap() == &symbol) {
                if self.get_flags(var) & ruleflag::CHILD_REPEAT == 0 {
                    self.set_flags(var, ruleflag::R_RECURSION);
                }
            }
            prules.extend(extra_prods);
            let incompatibility_flags = ruleflag::R_RECURSION | ruleflag::L_FORM | ruleflag::PARENT_L_RECURSION;
            let incompatibility_str = "<L> right recursivity + left recursivity";
            let flags = self.get_flags(var);
            if flags & incompatibility_flags == incompatibility_flags {
                let tree = &self.origin.trees[var as usize];
                self.log.add_error(format!(
                    "`{var_name} -> {}` has incompatible rule alternatives: {incompatibility_str}",
                    grtree_to_str(tree, None, None, None, self.get_symbol_table(), false)));
            }
        }
        if VERBOSE {
            println!("#prules: {}, #flags: {}, #parents:{}", prules.len(), self.flags.len(), self.parent.len());
            if let Some(ref mut table) = self.symbol_table {
                println!("table: {} ({})", table.get_nonterminals().join(", "), table.get_num_nt());
            }
        }
        self.prules = prules;
        self.num_nt = self.prules.len();
        if VERBOSE {
            println!("FINAL:");
            self.print_rules(false, false);
            println!("FLAGS:\n{}", self.flags.iter().index::<VarId>()
                .map(|(v, f)| format!("- ({v:2}) {}: {}", Symbol::NT(v).to_str(self.get_symbol_table()), ruleflag::to_string(*f).join(", "))).join("\n"));
            if let Some(table) = &self.symbol_table {
                println!("Symbol table:\n-NT: {}\n-T: {}",
                         table.get_nonterminals().enumerate().map(|(i, nt)| format!("{i}:{nt}")).join(", "),
                         table.get_terminals().enumerate()
                             .map(|(i, (t, txt))| format!("{i}:{t}{}", if let Some(st) = txt { format!(" ({st})") } else { String::new() })).join(", "))
            }
        }
    }

    /// Factorizes all the left symbols that are common to several alts by rejecting the non-common part
    /// to a new non-terminal. Updates the symbol table if provided.
    ///
    /// After the factorization, every child has an NT index greater than its parent, even if the
    /// factorization is performed several times on the same rule.
    ///
    /// Algorithm:
    /// - for each NT
    ///     - sort alts
    ///     - repeat as long as there are common starting symbols:
    ///         - take first group of >1 alts starting with the same symbol `α[0]`
    ///         - extract the number of starting symbols common to all alts of the group (1 or more): `α`
    ///         - create a new NT with the group, where `α` has been removed at the beginning
    pub fn left_factorize(&mut self) {
        fn similarity(a: &Alternative, b: &Alternative) -> usize {
            a.iter().zip(b.iter()).take_while(|(a, b)| a == b).count()
        }

        const VERBOSE: bool = false;
        self.log.add_note("removing left factorization...");
        let mut new_var = self.get_next_available_var();
        // we must take prules out because of the borrow checker and other &mut borrows we need later...
        let mut prules = take(&mut self.prules);
        let mut i = 0;
        while i < prules.len() {
            let prule = &mut prules[i];
            let var_flags = self.flags[i];
            let var = i as VarId;
            i += 1;
            if prule.len() < 2 {
                continue
            }
            let mut alts = prule.clone();
            let mut extra = Vec::<ProdRule>::new();
            let mut changed = false;
            alts.sort();
            if VERBOSE { println!("{var}: {} -> {}", Symbol::NT(var).to_str(self.get_symbol_table()), alts.iter().map(|f| f.to_str(self.get_symbol_table())).join(" | ")); }
            while alts.len() > 1 {
                let simi = alts.windows(2).enumerate()
                    .map(|(j, x)| (j, similarity(&x[0], &x[1])))
                    .skip_while(|(_, s)| *s == 0)
                    .take_while(|(_, s)| *s != 0)
                    .to_vec();
                if simi.is_empty() {
                    if VERBOSE { println!(" nothing to factorize"); }
                    break;
                }
                changed = true;
                let min = simi.iter().map(|(_, s)| *s).min().unwrap();
                let start = simi[0].0;
                let stop = start + simi.len();
                let mut factorized = Alternative::new(alts[start].v.iter().take(min).cloned().to_vec());
                let mut child = alts.drain(start..=stop).to_vec();
                if VERBOSE { println!("child:\n{}", child.iter().enumerate().map(|(i, c)| format!("- [{i}] = {}  org = {:?}", c.to_str(self.get_symbol_table()), c.origin)).join("\n")); }
                if child.iter().all(|f| f.is_greedy()) {
                    factorized.flags |= ruleflag::GREEDY;
                }
                for f in &mut child {
                    f.v.drain(0..min);
                }
                if child[0].v.is_empty() {
                    if var_flags & ruleflag::CHILD_REPEAT != 0 {
                        factorized.origin = child[0].origin;
                    }
                    child[0].v.push(Symbol::Empty);
                    let empty = child.remove(0);
                    child.push(empty);
                }
                let var_prime = new_var;
                new_var += 1;
                self.set_flags(var, ruleflag::PARENT_L_FACTOR);
                self.set_flags(var_prime, ruleflag::CHILD_L_FACT);
                let rep_l_form = ruleflag::CHILD_REPEAT | ruleflag::L_FORM;
                let top = if var_flags & rep_l_form == rep_l_form { var } else { self.get_top_parent(var) };
                self.symbol_table.as_mut().map(|table| assert_eq!(table.add_child_nonterminal(top), var_prime));
                self.set_parent(var_prime, var);
                let symbol_prime = Symbol::NT(var_prime);
                factorized.v.push(symbol_prime);
                alts.insert(start, factorized);
                if VERBOSE {
                    println!(" - similarity: {} => {}", simi.iter().map(|(j, s)| format!("{j}:{s}")).join(", "), min);
                    println!("   factorize: {}", child.iter().map(|a| a.to_str(self.get_symbol_table())).join(" | "));
                    println!("   left:      {}", alts.iter().map(|a| a.to_str(self.get_symbol_table())).join(" | "));
                }
                extra.push(child);
            }
            if changed {
                self.log.add_note(format!(
                    "- modifying: {} -> {}",
                    Symbol::NT(var).to_str(self.get_symbol_table()), prule_to_str(prule, self.get_symbol_table())));
                self.log.add_note(format!(
                    "  => {} -> {}",
                    Symbol::NT(var).to_str(self.get_symbol_table()), prule_to_str(&alts, self.get_symbol_table())));
                *prule = alts;
                let offset = prules.len() as VarId;
                for (v, p) in extra.iter().index_start(offset) {
                    self.log.add_note(format!(
                        "     {} -> {}",
                        Symbol::NT(v).to_str(self.get_symbol_table()), prule_to_str(p, self.get_symbol_table())));
                }
                prules.extend(extra);
            }
        }
        self.prules = prules;
        self.num_nt = self.prules.len();
    }

    pub(crate) fn remove_ambiguity(&self) {
        todo!()
    }

    /// Moves the flags of the mask from the alternatives to the NT flags
    fn transfer_alt_flags(&mut self) {
        // add other flags here if necessary:
        const FLAG_MASK: u32 = ruleflag::L_FORM;

        for (v, prule) in self.prules.iter_mut().enumerate() {
            let flags = prule.iter().fold(0, |acc, alt| acc | alt.flags) & FLAG_MASK;
            self.flags[v] |= flags;
            for alt in prule.iter_mut() {
                alt.flags &= !FLAG_MASK;
            }
        }
    }

    fn check_flags(&mut self) {
        const FLAG_CHECK_MASK: u32 = ruleflag::L_FORM | ruleflag::CHILD_REPEAT | ruleflag::R_RECURSION;
        for v in 0..self.num_nt {
            if self.flags[v] & FLAG_CHECK_MASK == ruleflag::L_FORM {
                // it's also fine to have L-form on a l-factor children of a right-recursive parent
                if self.flags[v] & ruleflag::CHILD_L_FACT == 0 || self.flags[self.parent[v].unwrap() as usize] & ruleflag::R_RECURSION == 0 {
                    self.log.add_error(format!("{} has an illegal flag L-Form (only used with +, *, or right recursion): {}",
                                               Symbol::NT(v as VarId).to_str(self.get_symbol_table()),
                                               ruleflag::to_string(self.flags[v]).join(" ")
                    ));
                }
            }
        }
    }

    /// Generates a side-by-side comparison between the production rules and the original rule
    /// each alternative represents (or which part of that rule).
    ///
    /// The output can be formatted with [`indent_source`].
    pub fn prs_alt_origins_str(&self, ansi: bool) -> Vec<String> {
        let mut cols = vec![vec!["ProdRuleSet".to_string(), "|".to_string(), "Original rules".to_string()]];
        cols.push(vec!["-----------".to_string(), "|".to_string(), "--------------".to_string()]);
        // adds the current alternatives
        cols.extend(self.get_prules_iter()
            .flat_map(|(v, prule)| prule.iter()
                .map(move |alt| vec![
                    alt_to_rule_str(v, &alt.v, self.get_symbol_table()),
                    "|".to_string(),
                    if let Some((vo, ido)) = alt.origin {
                        let tree = &self.origin.trees[vo as usize];
                        let emphasis = if tree.get_root() == Some(ido) { None } else { Some(ido) };
                        let orig_rule = grtree_to_str_custom(tree, None, emphasis, Some(vo), self.get_symbol_table(), false, ansi);
                        format!("{} -> {orig_rule}", Symbol::NT(vo).to_str(self.get_symbol_table()))
                    } else {
                        String::new()
                    }
                ])
            ));
        // adds child nonterminals that represent a part of the original nonterminals
        cols.extend((0..self.num_nt)
            .filter_map(|var|
                self.parent[var]
                    .and_then(|_| self.origin.map.get(&(var as VarId))
                        .and_then(|&(v, index)| Some((var as VarId, v, index)))))
            .map(|(var, v, index)| vec![
                Symbol::NT(var).to_str(self.get_symbol_table()),
                "|".to_string(),
                format!("{} -> {}",
                        Symbol::NT(v).to_str(self.get_symbol_table()),
                        grtree_to_str_custom(&self.origin.trees[v as usize], None, Some(index), None, self.get_symbol_table(), false, ansi))

            ]));
        columns_to_str(cols, None)
    }
}

impl<T> LogReader for ProdRuleSet<T> {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        &self.log
    }

    fn give_log(self) -> Self::Item {
        self.log
    }
}

impl<T> HasBuildErrorSource for ProdRuleSet<T> {
    const SOURCE: BuildErrorSource = BuildErrorSource::ProdRuleSet;
}

impl ProdRuleSet<General> {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            prules: Vec::with_capacity(capacity),
            origin: Origin::new(),
            num_nt: 0,
            num_t: 0,
            symbol_table: None,
            flags: Vec::with_capacity(capacity),
            parent: Vec::with_capacity(capacity),
            start: None,
            name: None,
            nt_conversion: HashMap::new(),
            log: BufLog::new(),
            _phantom: PhantomData
        }
    }
}

impl ProdRuleSet<LL1> {
    /// Creates the table for predictive top-down parsing.
    ///
    /// Returns:
    /// - `num_nt` = number of nonterminals
    /// - `num_t` = number of terminals (including the end symbol)
    /// - `alts`, the production alternatives: (VarId, Alternative) where the first value is the non-terminal index and the second one of its alts
    /// - the table of `num_nt * num_t` values, where `table[nt_index * num_nt + t_index]` gives the index of the production alternative for
    /// the non-terminal index `nt_index` and the terminal index `t_index`. A value >= `alts.len()` stands for a syntax error.
    pub(super) fn calc_table(&mut self, first: &HashMap<Symbol, HashSet<Symbol>>, follow: &HashMap<Symbol, HashSet<Symbol>>, error_recovery: bool) -> LLParsingTable {
        fn add_table(table: &mut Vec<Vec<AltId>>, num_t: usize, nt_id: VarId, t_id: VarId, a_id: AltId) {
            let pos = nt_id as usize * num_t + t_id as usize;
            table[pos].push(a_id);
        }
        const VERBOSE: bool = false;
        const DISABLE_FILTER: bool = false;
        if !self.log.has_no_errors() {
            return LLParsingTable::new();
        }
        let mut alts = self.prules.iter().index().filter(|(v, _)| DISABLE_FILTER || first.contains_key(&Symbol::NT(*v)))
            .flat_map(|(v, x)| x.iter().map(move |a| (v, a.clone()))).to_vec();
        let error_skip = alts.len() as AltId;   // table entry for syntactic error; recovery by skipping input symbol
        let error_pop = error_skip + 1;         // table entry for syntactic error; recovery by popping T or NT from stack
        let num_nt = self.num_nt;
        let num_t = self.num_t + 1;
        let end = (num_t - 1) as VarId; // index of end symbol
        let mut used_t = HashSet::<Symbol>::new();
        let mut table: Vec<Vec<AltId>> = vec![vec![]; num_nt * num_t];
        for (a_id, (nt_id, alt)) in alts.iter().index() {
            used_t.extend(alt.iter().filter(|s| s.is_t()));
            if VERBOSE { println!("- {a_id}: {} -> {}  => {}", Symbol::NT(*nt_id).to_str(self.get_symbol_table()),
                                  alt.to_str(self.get_symbol_table()),
                                  alt.calc_alt_first(first).iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut has_end = false;
            let mut has_empty = false;
            for s in alt.calc_alt_first(first) {
                match s {
                    Symbol::Empty => {
                        has_empty = true;
                        for s in &follow[&Symbol::NT(*nt_id)] {
                            match s {
                                Symbol::T(t_id) => add_table(&mut table, num_t, *nt_id, *t_id, a_id),
                                Symbol::End     => add_table(&mut table, num_t, *nt_id, end, a_id),
                                _ => {}
                            }
                        }
                    }
                    Symbol::T(t_id) => {
                        add_table(&mut table, num_t, *nt_id, t_id, a_id);
                    }
                    Symbol::NT(_) => {}
                    Symbol::End => {
                        has_end = true;
                    }
                }
            }
            if has_empty && has_end {
                add_table(&mut table, num_t, *nt_id, end, end);
            }
        }
        // creates the table and removes ambiguities
        let mut final_table = Vec::<AltId>::new();
        for nt_id in 0..num_nt {
            for t_id in 0..num_t {
                let pos = nt_id * num_t + t_id;
                final_table.push(match table[pos].len() {
                    0 => {
                        if error_recovery {
                            let sym_t = if t_id < num_t - 1 { Symbol::T(t_id as TokenId) } else { Symbol::End };
                            let sym_nt = Symbol::NT(nt_id as VarId);
                            if follow[&sym_nt].contains(&sym_t) || first[&sym_nt].contains(&sym_t) {
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
                                format!("- calc_table: expected ambiguity for NT '{}', T '{}': {} => <{}> is specified as greedy and has been chosen",
                                        Symbol::NT(nt_id as VarId).to_str(self.get_symbol_table()),
                                        if t_id < self.num_t { Symbol::T(t_id as VarId).to_str(self.get_symbol_table()) } else { "<EOF>".to_string() },
                                        table[pos].iter().map(|a_id|
                                            format!("<{}>", alts[*a_id as usize].1.to_str(self.get_symbol_table()))).join(" or "),
                                        alts[chosen as usize].1.to_str(self.get_symbol_table())
                                ));
                            table[pos] = greedies;
                            chosen
                        } else {
                            let row = (0..num_t).filter(|j| *j != t_id).flat_map(|j| &table[nt_id * num_t + j]).collect::<HashSet<_>>();
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
        if !(0..num_t - 1).any(|t_id| (0..num_nt).any(|nt_id| final_table[nt_id * num_t + t_id] < error_skip)) {
            self.log.add_error("- calc_table: no terminal used in the table".to_string());
        }
        for (_, a) in &mut alts {
            a.flags &= !ruleflag::GREEDY;
        }
        LLParsingTable { num_nt, num_t, alts, table: final_table, flags: self.flags.clone(), parent: self.parent.clone() }
    }

    pub fn make_parsing_table(&mut self, error_recovery: bool) -> LLParsingTable {
        self.log.add_note("calculating parsing table...");
        let first = self.calc_first();
        let follow = self.calc_follow(&first);
        self.calc_table(&first, &follow, error_recovery)
    }

    pub fn gen_tables_source_code(&self, indent: usize) -> String {
        let st = self.symbol_table.as_ref().unwrap();
        let mut source = Vec::<String>::new();
        // "origin" preparation
        source.push(format!("static ORIGIN: [(Option<usize>, &[(GrNode, &[usize])]); {}] = [", self.origin.trees.len()));
        for t in &self.origin.trees {
            let tree_str = (0..t.len()).into_iter()
                .map(|i| format!("({}, &[{}])", t.get(i).gen_source_code(), t.children(i).into_iter().join(",")))
                .join(", ");
            source.push(format!("    ({:?}, &[{}]),", t.get_root(), tree_str));
        }
        source.push("];".to_string());
        source.push(format!("static MAP: [(VarId, (VarId, usize)); {}] = [", self.origin.map.len()));
        let mut sorted_map = self.origin.map.iter().to_vec();
        sorted_map.sort(); // we must sort it so that its output is reproducible
        source.extend(sorted_map.chunks(5)
            .map(|chk| format!("    {},", chk.into_iter().map(|(a, (c, d))| format!("({a}, ({c}, {d}))")).join(", "))));
        source.push("];".to_string());
        source.push("let origin = Origin::from_data(".to_string());
        source.push("    ORIGIN.into_iter().map(|(root, nodes)| GrTree::from((root, nodes.to_vec()))).collect(),".to_string());
        source.push("    HashMap::from(MAP));".to_string());
        // ProdRuleSetTables:
        source.push(String::new());
        source.push("let ll1_tables = ProdRuleSetTables::new(".to_string());
        source.push(format!("    {:?},", self.name));
        source.push("    vec![".to_string());
        source.extend(self.prules.iter().map(|prule| format!("        {},", prule_to_macro(prule))));
        source.push("    ],".to_string());
        source.push("    origin,".to_string());
        source.push(format!("    vec![{}],", st.get_terminals().map(|x| format!("{x:?}")).join(", ")));
        source.push(format!("    vec![{}],", st.get_nonterminals().map(|x| format!("{x:?}")).join(", ")));
        source.push(format!("    vec![{}],", self.flags.iter().join(", ")));
        source.push(format!("    vec![{}],", self.parent.iter().map(|p_maybe| format!("{p_maybe:?}")).join(", ")));
        source.push(format!("    {:?},", self.start));
        source.push(format!("    hashmap![{}]", self.nt_conversion.iter().map(|(v, conv)| format!("{v} => {conv:?}")).join(", ")));
        source.push(");".to_string());
        indent_source(vec![source], indent)
    }
}

// ---------------------------------------------------------------------------------------------

pub struct ProdRuleSetTables {
    name: Option<String>,
    prules: Vec<ProdRule>,
    origin: Origin<VarId, FromPRS>,
    t: Vec<(String, Option<String>)>,   // terminal identifiers and optional representation
    nt: Vec<String>,                    // nt to nonterminal identifier
    flags: Vec<u32>,
    parent: Vec<Option<VarId>>,
    start: Option<VarId>,
    nt_conversion: HashMap<VarId, NTConversion>,
}

impl ProdRuleSetTables {
    pub fn new<T: Into<String>>(
        name: Option<T>,
        prules: Vec<ProdRule>,
        origin: Origin<VarId, FromPRS>,
        t: Vec<(T, Option<T>)>,
        nt: Vec<T>,
        flags: Vec<u32>,
        parent: Vec<Option<VarId>>,
        start: Option<VarId>,
        nt_conversion: HashMap<VarId, NTConversion>,
    ) -> Self {
        let t = t.into_iter().map(|(t, t_maybe)| (t.into(), t_maybe.map(|t| t.into()))).collect();
        let nt = nt.into_iter().map(|nt| nt.into()).collect();
        ProdRuleSetTables {
            name: name.map(|s| s.into()),
            prules,
            origin, t, nt, flags, parent, start, nt_conversion,
        }
    }

    pub fn get_name(&self) -> Option<&String> {
        self.name.as_ref()
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
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

impl BuildFrom<RuleTreeSet<Normalized>> for ProdRuleSet<General> {
    /// Builds a [`ProdRuleSet<General>`] from a [`RuleTreeSet<Normalized>`].
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn build_from(mut rules: RuleTreeSet<Normalized>) -> Self {
        fn children_to_vec(tree: &GrTree, parent_id: usize) -> Alternative {
            let mut flags: u32 = 0;
            let alt = tree.children(parent_id).iter()
                .map(|id| tree.get(*id))
                .filter(|node| {
                    match **node {
                        GrNode::RAssoc => {
                            flags |= ruleflag::R_ASSOC;
                            false
                        }
                        GrNode::LForm(_) => {
                            flags |= ruleflag::L_FORM;
                            false
                        }
                        GrNode::PrecEq => {
                            flags |= ruleflag::PREC_EQ;
                            false
                        }
                        GrNode::Greedy => {
                            flags |= ruleflag::GREEDY;
                            false
                        }
                        _ => true,
                    }
                })
                .map(|node| {
                match node {
                    GrNode::Symbol(s) => s.clone(),
                    x => panic!("unexpected symbol {x} under &")
                }
            }).to_vec();
            Alternative::new(alt).with_flags(flags)
        }
        let mut prules = Self::with_capacity(rules.trees.len());
        prules.start = rules.start;
        prules.symbol_table = rules.symbol_table;
        prules.flags = rules.flags;
        prules.parent = rules.parent;
        prules.nt_conversion = rules.nt_conversion;
        prules.log = rules.log;
        if !prules.log.has_no_errors() {
            // We handle the errors by transmitting the log to the next construct rather than returning a `Result` type.
            // This allows to cascade the transforms without getting a complicated error resolving system while preserving
            // the information about the errors easily.
            return prules;
        }
        prules.origin = Origin::<VarId, FromPRS>::from_trees_mut(&mut rules.origin.trees);
        for (var, tree) in rules.trees.iter().index() {
            if !tree.is_empty() {
                let root = tree.get_root().expect("tree {var} has no root");
                let root_sym = tree.get(root);
                let mut prule = match root_sym {
                    GrNode::Symbol(s) => {
                        let mut alt = Alternative::new(vec![s.clone()]);
                        if let Some(&(v, ch)) = rules.origin.map.get(&(var, root)) {
                            alt.origin = Some((v, ch));
                        }
                        vec![alt]
                    },
                    GrNode::Concat => {
                        let mut alt = children_to_vec(tree, root);
                        if let Some(&(v, ch)) = rules.origin.map.get(&(var, root)) {
                            alt.origin = Some((v, ch));
                        }
                        vec![alt]
                    },
                    GrNode::Or => tree.children(root).iter()
                        .map(|id| {
                            let child = tree.get(*id);
                            let mut alt = if let GrNode::Symbol(s) = child {
                                Alternative::new(vec![s.clone()])
                            } else {
                                assert_eq!(*child, GrNode::Concat, "unexpected symbol {child} under |");
                                children_to_vec(tree, *id)
                            };
                            if let Some(&(v, ch)) = rules.origin.map.get(&(var, *id)) {
                                alt.origin = Some((v, ch));
                            }
                            alt
                        }).to_vec(),
                    s => panic!("unexpected symbol {s} as root of normalized GrTree for NT {}", Symbol::NT(var).to_str(prules.get_symbol_table()))
                };
                if prule.iter().any(|f| f.flags & ruleflag::L_FORM != 0) {
                    // We keep the L flag on the child of +* normalization if it's intended only for that normalization.
                    // For example:
                    // - A -> (b <L>)+ A | c
                    //   - doesn't have an l-form right recursion
                    //   - has an l-form repetition of b
                    // - A -> <L> (b)+ A | c
                    //   - has an l-form right recursion
                    //   - doesn't have an l-form repetition of b
                    //
                    // while let Some(parent) = prules.get_parent(nt) {
                    //     nt = parent;
                    // }
                    prules.set_flags(var, ruleflag::L_FORM);
                    // not really necessary, but cleaner:
                    for a in prule.iter_mut() {
                        a.flags &= !ruleflag::L_FORM;
                    }
                }
                // the children NTs that represent a part of the original NT are stored in ProdRuleSet::origin
                // rather than in the alts themselves
                if prules.parent[var as usize].is_some() {
                    if let Some(&(v, index)) = rules.origin.map.get(&(var, root)) {
                        prules.origin.add(var, (v, index));
                    }
                }
                prules.prules.push(prule);
            } else {
                prules.prules.push(ProdRule::new()); // empty
            }
        }
        prules.calc_num_symbols();
        prules
    }
}

impl BuildFrom<RuleTreeSet<General>> for ProdRuleSet<General> {
    /// Builds a [`ProdRuleSet<General>`] from a [`RuleTreeSet<General>`].
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn build_from(rules: RuleTreeSet<General>) -> Self {
        let mut prs = ProdRuleSet::build_from(RuleTreeSet::<Normalized>::build_from(rules));
        if prs.log.has_no_errors() {
            prs.simplify();
        }
        prs
    }
}

impl BuildFrom<ProdRuleSet<General>> for ProdRuleSet<LL1> {
    fn build_from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.remove_recursion();
            rules.left_factorize();
            rules.transfer_alt_flags();
            rules.check_flags();
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
            _phantom: PhantomData,
        }
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
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting debug functions

impl<T> ProdRuleSet<T> {
    pub fn print_rules(&self, as_comment: bool, filter_empty_nt: bool) {
        let prefix = if as_comment { "            // " } else { "    " };
        println!("{prefix}{}",
                 self.get_prules_iter()
                     .filter(|(_, rule)| !filter_empty_nt || **rule != prule!(e))
                     .map(|(var, p)|
                         format!("({var}) {} -> {}",
                                 Symbol::NT(var).to_str(self.get_symbol_table()),
                                 prule_to_str(p, self.get_symbol_table())))
                     .join(&format!("\n{prefix}")));
    }

    pub fn print_alts(&self) {
        println!("Alternatives:\n{}",
                 self.get_alts().enumerate().map(|(id, (v, a))|
                     format!("    // - {id}: {} -> {}{}",
                             Symbol::NT(v).to_str(self.get_symbol_table()),
                             a.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                             if a.flags != 0 { format!("     {} ({})", ruleflag::to_string(a.flags).join(" | "), a.flags) } else { "".to_string() }
                     )
        ).join("\n"));
    }
}

impl LLParsingTable {
    pub fn print(&self, symbol_table: Option<&SymbolTable>, indent: usize) {
        let LLParsingTable { num_nt, num_t, alts, table, .. } = self;
        let error_skip = alts.len() as AltId;
        let error_pop = error_skip + 1;
        let str_nt = (0..*num_nt).map(|i| Symbol::NT(i as VarId).to_str(symbol_table)).to_vec();
        let max_nt_len = str_nt.iter().map(|s| s.len()).max().unwrap();
        let str_t = (0..*num_t).map(|j| if j + 1 < *num_t { Symbol::T(j as VarId).to_str(symbol_table) } else { "$".to_string() }).to_vec();
        let t_len = str_t.iter().map(|s| s.len().max(3)).to_vec();
        println!("{:<i$}// {:<w$} | {}", "", "", (0..*num_t).map(|j| format!("{:^w$}", str_t[j], w = t_len[j])).join(" "), w = max_nt_len, i = indent);
        println!("{:<i$}// {:-<w$}-+-{:-<t$}", "", "", "", w = max_nt_len, t = *num_t + t_len.iter().sum::<usize>(), i = indent);
        for i in 0..*num_nt {
            print!("{:<i$}// {:<w$} |", "", str_nt[i], w = max_nt_len, i = indent);
            for j in 0..*num_t {
                let value = table[i * num_t + j];
                if value < error_skip {
                    print!(" {:^w$}", value, w = t_len[j]);
                } else {
                    print!(" {:^w$}", if value == error_pop { "p" } else { "." }, w = t_len[j]);
                }
            }
            println!();
        }
    }
}
