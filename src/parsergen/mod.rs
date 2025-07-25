// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use iter_index::IndexerIterator;
use crate::grammar::{LLParsingTable, ProdRuleSet, ruleflag, RuleTreeSet, Symbol, VarId, FactorId, NTConversion, ProdFactor, factor_to_rule_str};
use crate::{CollectJoin, General, LL1, Normalized, SourceSpacer, SymbolTable, SymInfoTable, NameTransformer, NameFixer, columns_to_str, StructLibs, indent_source, FixedSymTable};
use crate::log::{BufLog, Logger};
use crate::parser::{OpCode, Parser};
use crate::segments::{Seg, Segments};

pub(crate) mod tests;

// ---------------------------------------------------------------------------------------------

pub(crate) fn symbol_to_code(s: &Symbol) -> String {
    match s {
        Symbol::Empty => "Symbol::Empty".to_string(),
        Symbol::T(t) => format!("Symbol::T({t})"),
        Symbol::NT(nt) => format!("Symbol::NT({nt})"),
        Symbol::End => "Symbol::End".to_string(),
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Empty => write!(f, "ε"),
            OpCode::T(t) => write!(f, ":{t}"),
            OpCode::NT(v) => write!(f, "►{v}"),
            OpCode::Loop(v) => write!(f, "●{v}"),
            OpCode::Exit(v) => write!(f, "◄{v}"),
            OpCode::End => write!(f, "$"),
        }
    }
}

impl OpCode {
    pub fn is_loop(&self) -> bool {
        matches!(self, OpCode::Loop(_))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, OpCode::Empty)
    }

    pub fn matches(&self, s: Symbol) -> bool {
        match self {
            OpCode::Empty => s == Symbol::Empty,
            OpCode::T(t) => s == Symbol::T(*t),
            OpCode::NT(v) => s == Symbol::NT(*v),
            OpCode::End => s == Symbol::End,
            OpCode::Loop(_) => false,
            OpCode::Exit(_) => false,
        }
    }

    pub fn to_str<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        if let Some(t) = symbol_table {
            match self {
                OpCode::Empty => "ε".to_string(),
                OpCode::T(v) => format!("{}{}", t.get_t_str(*v), if t.is_token_data(*v) { "!" } else { "" }),
                OpCode::NT(v) => format!("►{}", t.get_nt_name(*v)),
                OpCode::Loop(v) => format!("●{}", t.get_nt_name(*v)),
                OpCode::Exit(f) => format!("◄{f}"),
                OpCode::End => "$".to_string(),
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_ext(&self, symbol_table: Option<&SymbolTable>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(_) = symbol_table {
            if let OpCode::T(_) = self {
                result.push_str(&format!("({ext})"));
            }
        }
        result
    }
}

impl From<Symbol> for OpCode {
    fn from(value: Symbol) -> Self {
        match value {
            Symbol::Empty => OpCode::Empty,
            Symbol::T(t) => OpCode::T(t),
            Symbol::NT(v) => OpCode::NT(v),
            Symbol::End => OpCode::End,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
struct ItemInfo {
    name: String,
    sym: Symbol,            // NT(var) or T(token)
    owner: VarId,           // NT owning this item; for ex. owner = `A` for `sym = b` in `A -> a b+ c`
    index: Option<usize>    // when several identical symbols in the same factor: `A -> id := id ( id )`
}

#[allow(unused)]
impl ItemInfo {
    fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        format!("{} ({}{}, ◄{})",
                self.name,
                self.sym.to_str(symbol_table),
                if let Some(n) = self.index { format!(", [{n}]") } else { "".to_string() },
                Symbol::NT(self.owner).to_str(symbol_table))
    }
}

// ---------------------------------------------------------------------------------------------

/// Tables and parameters used to create a [`Parser`]. This type is used as a return object from the parser generator,
/// when the Parser must be created dynamically; for example, in tests or in situations where the grammar isn't
/// known in advance. In those situations, the ParserTables object must live as long as the parser it generates.
///
/// The Parser itself uses references to tables whenever possible because, in most situations, the tables are
/// static in generated source files. A few fields must still be created dynamically from (possibly) static
/// tables because they don't exist in static form.
pub struct ParserTables {
    num_nt: usize,
    num_t: usize,
    // parsing_table: LLParsingTable,
    factor_var: Vec<VarId>,
    factors: Vec<ProdFactor>,
    opcodes: Vec<Vec<OpCode>>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    table: Vec<FactorId>,
    symbol_table: FixedSymTable,
    start: VarId,
    include_factors: bool,
}

impl ParserTables {
    pub fn new(parsing_table: LLParsingTable, symbol_table: FixedSymTable, opcodes: Vec<Vec<OpCode>>, start: VarId, include_factors: bool) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        let num_nt = parsing_table.num_nt;
        let num_t = parsing_table.num_t;
        let flags = parsing_table.flags;
        let parent = parsing_table.parent;
        let table = parsing_table.table;
        let (factor_var, factors): (Vec<_>, Vec<_>) = parsing_table.factors.into_iter().unzip();
        ParserTables { num_nt, num_t, factor_var, factors, opcodes, flags, parent, table, symbol_table, start, include_factors }
    }

    pub fn make_parser(&self) -> Parser {
        Parser::new(
            self.num_nt,
            self.num_t,
            self.factor_var.as_slice(),
            if self.include_factors { self.factors.clone() } else { vec![] },
            self.opcodes.clone(),
            self.flags.as_slice(),
            self.parent.as_slice(),
            self.table.as_slice(),
            self.symbol_table.clone(),
            self.start,
        )
    }
}

impl From<ParserGen> for ParserTables {
    /// Creates a [`ParserTables`], from which a parser can be created dynamically with
    /// [`parser_table.make_parser()`](ParserTables::make_parser).
    fn from(parser_gen: ParserGen) -> Self {
        ParserTables::new(
            parser_gen.parsing_table,
            parser_gen.symbol_table.to_fixed_sym_table(),
            parser_gen.opcodes,
            parser_gen.start,
            parser_gen.include_factors
        )
    }
}
// ---------------------------------------------------------------------------------------------

pub static DEFAULT_LISTENER_NAME: &str = "Parser";

#[derive(Debug)]
pub struct ParserGen {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
    name: String,
    nt_value: Vec<bool>,
    /// `nt_parent[v]` is the vector of all variables having `v` has top parent (including `v` itself)
    nt_parent: Vec<Vec<VarId>>,
    var_factors: Vec<Vec<FactorId>>,
    original_factors: Vec<ProdFactor>,   // factors before transformation, for future reference
    item_ops: HashMap<FactorId, Vec<Symbol>>,
    opcodes: Vec<Vec<OpCode>>,
    start: VarId,
    nt_conversion: HashMap<VarId, NTConversion>,
    used_libs: StructLibs,
    nt_type: HashMap<VarId, String>,
    nt_extra_info: HashMap<VarId, (String, Vec<String>)>,
    log: BufLog,
    include_factors: bool,
}

impl ParserGen {
    /// Creates a [`ParserGen`] from a set of rules and gives it a specific name, which is used
    /// to name the user listener trait in the generated code.
    pub fn from_tree(tree: RuleTreeSet<General>, name: String) -> Self {
        let normalized = RuleTreeSet::<Normalized>::from(tree);
        let lr_rules = ProdRuleSet::from(normalized);
        Self::from_rules(lr_rules, name)
    }

    /// Creates a [`ParserGen`] from a set of production rules and gives it a specific name, which is used
    /// to name the user listener trait in the generated code.
    ///
    /// If [`rules`] already has a name, it is best to use the [From<ProdRuleSet<T>>](From<ProdRuleSet<T>>::from) trait.
    pub fn from_rules<T>(rules: ProdRuleSet<T>, name: String) -> Self where ProdRuleSet<LL1>: From<ProdRuleSet<T>> {
        let mut ll1_rules = ProdRuleSet::<LL1>::from(rules);
        assert_eq!(ll1_rules.get_log().num_errors(), 0);
        let parsing_table = ll1_rules.create_parsing_table(true);
        let num_nt = ll1_rules.get_num_nt();
        let start = ll1_rules.get_start().unwrap();
        let symbol_table = ll1_rules.give_symbol_table().expect(stringify!("symbol table is required to create a {}", std::any::type_name::<Self>()));
        let nt_conversion = ll1_rules.give_nt_conversion();
        let mut var_factors = vec![vec![]; num_nt];
        for (factor_id, (var_id, _)) in parsing_table.factors.iter().index() {
            var_factors[*var_id as usize].push(factor_id);
        }
        let original_factors = ll1_rules.give_original_factors();
        let mut nt_parent: Vec<Vec<VarId>> = vec![vec![]; num_nt];
        for var_id in 0..num_nt {
            let top_var_id = parsing_table.get_top_parent(var_id as VarId) as usize;
            nt_parent[top_var_id].push(var_id as VarId);
        }
        let mut builder = ParserGen {
            parsing_table,
            symbol_table,
            name,
            nt_value: vec![false; num_nt],
            nt_parent,
            var_factors,
            original_factors,
            item_ops: HashMap::new(),
            opcodes: Vec::new(),
            start,
            nt_conversion,
            used_libs: StructLibs::new(),
            nt_type: HashMap::new(),
            nt_extra_info: HashMap::new(),
            log: ll1_rules.log,
            include_factors: true,
        };
        builder.build_opcodes();
        builder
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        Some(&self.symbol_table)
    }

    #[inline]
    pub fn get_parsing_table(&self) -> &LLParsingTable {
        &self.parsing_table
    }

    #[inline]
    pub fn get_log(&self) -> &BufLog {
        &self.log
    }

    #[inline]
    pub fn add_lib(&mut self, lib: &str) {
        self.used_libs.add(lib);
    }

    #[inline]
    pub fn extend_libs<I: IntoIterator<Item=J>, J: Into<String>>(&mut self, libs: I) {
        self.used_libs.extend(libs);
    }

    #[inline]
    /// Declares the type of a non-terminal. The index of the NT, `org_var`, is the original index
    /// in the ruletree set, which is the index originally assigned when parsing the grammar file.
    pub fn add_nt_type<T: Into<String>>(&mut self, org_var: VarId, var_type: T) {
        let var = self.conv_nt(org_var).expect(&format!("var {org_var} doesn't exist"));
        self.nt_type.insert(var, var_type.into());
    }

    #[inline]
    pub fn get_nt_type(&self, v: VarId) -> &str {
        self.nt_type.get(&v).unwrap().as_str()
    }

    #[inline]
    /// NT source type and source code including doc comment and empty template type definition.
    ///
    /// ## Example:
    /// ```ignore
    /// let (type_src, src) = parger_gen.get_nt_extra_info(var_id);
    /// ```
    /// * `type_src` = "SynHeader"
    /// * `src` =
    ///     ```ignore
    ///     /// User-defined type for `header`
    ///     #[derive(Debug, PartialEq)]
    ///     pub struct SynHeader();
    ///     ```
    pub fn get_nt_extra_info(&self, nt: VarId) -> Option<&(String, Vec<String>)> {
        self.nt_extra_info.get(&nt)
    }

    #[inline]
    pub fn set_nt_has_value(&mut self, v: VarId, has_value: bool) {
        self.nt_value[v as usize] = has_value;
    }

    pub fn set_parents_have_value(&mut self) {
        for v in 0..self.get_symbol_table().unwrap().get_num_nt() as VarId {
            if self.get_nt_parent(v).is_none() {
                self.set_nt_has_value(v, true);
            } else {
            }
        }
    }

    #[inline]
    pub fn get_nt_parent(&self, v: VarId) -> Option<VarId> {
        self.parsing_table.parent[v as usize]
    }

    /// Include the factor definitions in the parser, for debugging purposes:
    /// allows to print out the factors in VERBOSE mode.
    pub fn set_include_factors(&mut self, include_factors: bool) {
        self.include_factors = include_factors;
    }

    #[allow(unused)]
    fn get_original_factor_str(&self, f_id: FactorId, symbol_table: Option<&SymbolTable>) -> Option<String> {
        let (var, f) = &self.parsing_table.factors[f_id as usize];
        f.get_original_factor_id().and_then(|orig_id| {
            let o_f = &self.original_factors[orig_id as usize];
            let parent = self.parsing_table.get_top_parent(*var);
            Some(format!("{} -> {}", Symbol::NT(parent).to_str(symbol_table), o_f.to_str(symbol_table)))
        })
    }

    /// Converts the original index of an NT to its current index.
    ///
    /// The original index is the NT's index of a general (non-normalized) ruletree set, as parsed from
    /// a grammar file. The current index may differ if NTs were removed during the analysis of the
    /// production rules or if <L> low-latency labels were declared.
    fn conv_nt(&self, org_var: VarId) -> Option<VarId> {
        match self.nt_conversion.get(&org_var) {
            None => if (org_var as usize) < self.parsing_table.num_nt { Some(org_var) } else { None },
            Some(NTConversion::MovedTo(new)) => Some(*new),
            Some(NTConversion::Removed) => None
        }
    }

    #[inline]
    fn nt_has_all_flags(&self, var: VarId, flags: u32) -> bool {
        self.parsing_table.flags[var as usize] & flags == flags
    }

    #[inline]
    fn nt_has_any_flags(&self, var: VarId, flags: u32) -> bool {
        self.parsing_table.flags[var as usize] & flags != 0
    }

    #[inline]
    fn sym_has_flags(&self, s: &Symbol, flags: u32) -> bool {
        if let Symbol::NT(nt) = s { self.nt_has_all_flags(*nt, flags) } else { false }
    }

    #[inline]
    fn sym_has_value(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::T(t) => self.symbol_table.is_token_data(*t),
            Symbol::NT(nt) => self.nt_value[*nt as usize],
            _ => false
        }
    }

    /// Expands all the parents of left factorization by replacing the NTs on the right that are CHILD_L_FACTOR
    /// with their factors. Proceeds until there are no more substitutions. Removes the empty symbols unless
    /// they're alone in a factor.
    ///
    /// Example:
    /// ```text
    /// - A -> a A_1
    /// - A -> e
    /// - A_1 -> b A_2
    /// - A_1 -> ε
    /// - A_2 -> c
    /// - A_2 -> d
    /// - A_2 -> ε
    ///
    /// expand_lfact(a b A_2) -> a b | a b c | a b d
    /// ```
    fn expand_lfact(&self, factors: &mut Vec<Vec<Symbol>>) {
        let mut change = true;
        while change {
            change = false;
            let mut extra = Vec::<Vec<Symbol>>::new();
            for f in &mut *factors {
                // we have to complicate in order to please the borrow checker:
                if matches!(f.last(), Some(Symbol::NT(v)) if self.parsing_table.flags[*v as usize] & ruleflag::CHILD_L_FACTOR != 0) {
                    let Symbol::NT(child) = f.pop().unwrap() else { panic!() };
                    let mut exp = self.var_factors[child as usize].iter().map(|child_f| {
                        let mut new = f.clone();
                        new.extend(self.parsing_table.factors[*child_f as usize].1.symbols());
                        new
                    }).to_vec();
                    *f = exp.pop().unwrap();
                    extra.extend(exp);
                    change = true;
                }
            }
            if change {
                factors.extend(extra);
            }
        }
        for f in &mut *factors {
            if matches!(f.last(), Some(Symbol::Empty)) && f.len() > 1 {
                f.pop();
            }
        }
        factors.sort();
    }

    /// Finds the factors, in the group parent, that reference the variable `nt`, directly or through intermediate children variables.
    ///
    /// Return `(nt_id, factor_id)` couples, where
    /// - `factor_id` is the factor in the group's parent
    /// - `nt_id` is the intermediate variable, in that factor, which depends on `nt`.
    fn get_top_factors(&self, nt: VarId) -> Vec<(VarId, FactorId)> {
        const VERBOSE: bool = false;
        let parent = self.parsing_table.get_top_parent(nt);
        let group_facts = self.get_group_factors(&self.nt_parent[parent as usize]);
        let mut result = Vec::<(VarId, FactorId)>::new();
        let mut new = vec![nt];
        let mut old = HashSet::<VarId>::new();
        let _symtable = self.get_symbol_table();
        if VERBOSE {
            println!("get_top_factors({nt}:{})", Symbol::NT(nt).to_str(_symtable));
            println!("group_facts = {group_facts:?}");
        }
        while !new.is_empty() {
            if VERBOSE {
                print!("new = [{}]", new.iter().map(|x| format!("{x}:{}", Symbol::NT(*x).to_str(_symtable))).join(", "));
                print!(", old = [{}]", old.iter().map(|x| format!("{x}:{}", Symbol::NT(*x).to_str(_symtable))).join(", "));
            }
            let goal = new.pop().unwrap();
            if VERBOSE { println!(" => testing {goal}:{}", Symbol::NT(goal).to_str(_symtable)); }
            for (v, f) in group_facts.iter().filter(|(v, _)| *v != goal && *v != nt) {
                if VERBOSE { print!("- {f}: {}: ", factor_to_rule_str(*v, self.parsing_table.factors[*f as usize].1.symbols(), _symtable)); }
                if old.contains(v) || new.contains(v) {
                    if VERBOSE { println!(" {} {}", if old.contains(v) { "already visited" } else { "" }, if new.contains(v) { "already in stack" } else { "" })}
                    continue
                }
                if self.parsing_table.factors[*f as usize].1.symbols().iter().any(|s| *s == Symbol::NT(goal)) {
                    if *v == parent {
                        if VERBOSE { println!("found top ({goal}, {f})"); }
                        result.push((goal, *f));
                        old.insert(goal);
                    } else {
                        if !old.contains(v) {
                            if VERBOSE { println!("found intermediate {v}"); }
                            new.push(*v);
                        }
                    }
                } else {
                    if VERBOSE { println!("/"); }
                }
            }
            old.insert(goal);
        }
        if VERBOSE { println!("=> result = {result:?}"); }
        result
    }

    fn full_factor_components<const VERBOSE: bool>(&self, f_id: FactorId, emphasis: Option<VarId>, quote: bool) -> (String, String) {
        // const VERBOSE: bool = true;
        if VERBOSE { println!("full_factor_components(f_id = {f_id}):"); }
        let (v_f, prodf) = &self.parsing_table.factors[f_id as usize];
        if let Some(id) = prodf.get_original_factor_id() {
            let parent_nt = self.parsing_table.get_top_parent(*v_f);
            let orig_f = &self.original_factors[id as usize];
            let mut pf = orig_f.iter().map(|s| {
                match s {
                    Symbol::NT(nt) if *nt != parent_nt && self.parsing_table.get_top_parent(*nt) == parent_nt => {
                        self.repeat_factor_str(&vec![*s], emphasis)
                    }
                    _ => s.to_str(self.get_symbol_table())
                }
            }).join(" ");
            let flags = orig_f.get_flags() & (ruleflag::L_FORM | ruleflag::R_ASSOC);
            if flags != 0 {
                pf.push_str(&format!(" <{}>", ruleflag::factor_info_to_string(flags).join(",")));
            }
            if VERBOSE { println!(" => ({}, {pf})", Symbol::NT(parent_nt).to_str(self.get_symbol_table())); }
            return (Symbol::NT(parent_nt).to_str(self.get_symbol_table()), pf);
        }
        let mut v_par_lf =  *v_f;
        let mut syms = prodf.symbols().iter().filter(|s| !s.is_empty()).cloned().to_vec();
        let mut left = *v_f;
        let lfact_str = if self.nt_has_all_flags(left, ruleflag::R_RECURSION | ruleflag::L_FORM) { " <L>" } else { "" };
        // if it's a child of left factorization, gathers the front symbols from the parents (going up)
        'up: while self.nt_has_all_flags(v_par_lf, ruleflag::CHILD_L_FACTOR) {
            let parent_v = self.parsing_table.parent[v_par_lf as usize].unwrap();
            for parent_f_id in &self.var_factors[parent_v as usize] {
                let (_, parent_pf) = &self.parsing_table.factors[*parent_f_id as usize];
                if let Some(idx) = parent_pf.iter().position(|sym| sym == &Symbol::NT(v_par_lf)) {
                    if VERBOSE { print!("  UN-FACT: {:?}: {:?} into {:?}", Symbol::NT(v_par_lf), syms, parent_pf.symbols()); }
                    let mut new_syms = parent_pf.symbols()[..idx].to_vec();
                    new_syms.extend(syms);
                    if VERBOSE { println!(" => {:?}", new_syms); }
                    syms = new_syms;
                    v_par_lf = parent_v;
                    left = v_par_lf;
                    continue 'up;
                }
            }
            panic!("factor not found");
        }
        if syms.is_empty() {
            syms.push(Symbol::Empty);
        }
        let mut facts = vec![syms];
        // expands any left factorization in the factor (going down)
        self.expand_lfact(&mut facts);
        // let  comment = String::new();
        let parent = self.parsing_table.get_top_parent(v_par_lf);

        if self.nt_has_any_flags(parent, ruleflag::PARENT_AMBIGUITY) {
            left = parent
        } else if self.nt_has_all_flags(v_par_lf, ruleflag::PARENT_L_RECURSION) {   // left recursion
            // initial value of left recursion loop
            left = parent;
            for f in facts.iter_mut() {
                if let Some(Symbol::NT(x)) = f.last() {
                    if self.nt_has_all_flags(*x, ruleflag::CHILD_L_RECURSION) {
                        f.pop();
                    }
                }
            }
        } else if self.nt_has_all_flags(v_par_lf, ruleflag::CHILD_L_RECURSION) {    // loop
            left = parent;
            for f in facts.iter_mut() {
                if !matches!(f.first(), Some(Symbol::Empty)) {
                    f.pop();
                    f.insert(0, Symbol::NT(left));
                }/* else {
                    comment.push_str(" (end of loop)");
                }*/
            }
        }
        let q = if quote { "`" } else { "" };
        let result = if self.nt_has_all_flags(v_par_lf, ruleflag::CHILD_REPEAT) {
            let is_empty = self.parsing_table.factors[f_id as usize].1.symbols().first() == Some(&Symbol::Empty);
            // let v_id = v_par_lf;
            let mut what_is_using_v: Vec<(VarId, FactorId)>;
            let mut fact_using_v: FactorId;
            let mut v_top = v_par_lf;
            loop {
                what_is_using_v = self.get_group_factors(&self.nt_parent[parent as usize]).iter()
                    // we exclude the repeat loop itself
                    // - in the case of *: *v != v_id (see RTS(26))
                    // - in the case of +: in any direct child factor, due to the left factorization of + (see RTS(16))
                    .filter(|(v, _)| *v != v_top && self.parsing_table.parent[*v as usize] != Some(v_top))
                    .filter_map(|(v, f)|
                        if self.parsing_table.factors[*f as usize].1.symbols().iter().any(|s| *s == Symbol::NT(v_top)) { Some((*v, *f)) } else { None })
                    .to_vec();
                fact_using_v = what_is_using_v[0].1;
                let nt_using_v = what_is_using_v[0].0;
                // if the child+* is used in another child+*, continue to go up in order to avoid a long chain of
                // ... iteration in ... in iteration ... etc.
                if !self.nt_has_all_flags(nt_using_v, ruleflag::CHILD_REPEAT) {
                    break;
                }
                v_top = nt_using_v;
            }
            // if there are several factors using v (see RTS(32)), we only show the first; the others are alternatives
            // of a left factorization in earlier symbols.
            let more_str = if what_is_using_v.len() > 1 { " | ..." } else { "" };
            let is_lform = self.nt_has_all_flags(v_par_lf, ruleflag::L_FORM);
            let emphasis_maybe = emphasis;
            if VERBOSE {
                println!("  full_factor_str({f_id}): C+*, v_par_lf={v_par_lf}, facts={}",
                         facts.iter().map(|f| f.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")).join(" | "));
                println!("  what_is_using_v ({}) = {what_is_using_v:?}", Symbol::NT(v_par_lf).to_str(self.get_symbol_table()));
                println!("  self.repeat_factor_str(&vec![{}], None) = '{}'",
                         Symbol::NT(v_par_lf).to_str(self.get_symbol_table()),
                         self.repeat_factor_str(&vec![Symbol::NT(v_par_lf)], emphasis_maybe));
                println!("  !!");
                println!("  self.full_factor_str::<false>({fact_using_v}, Some({}), false)) = '{}'",
                         Symbol::NT(v_par_lf).to_str(self.get_symbol_table()),
                         self.full_factor_str::<false>(fact_using_v, Some(v_par_lf as VarId), false));
            }
            (
                String::new(),
                if is_empty {
                    format!("end of {q}{}{q} {}s in {q}{}{more_str}{q}",
                            self.repeat_factor_str(&vec![Symbol::NT(v_par_lf)], emphasis_maybe),
                            if is_lform { "iteration" } else { "item" },
                            self.full_factor_str::<false>(fact_using_v, Some(v_par_lf as VarId), false))
                } else {
                    format!("{q}{}{q} {} in {q}{}{more_str}{q}",
                            self.repeat_factor_str(&vec![Symbol::NT(v_par_lf)], emphasis_maybe),
                            if is_lform { "iteration" } else { "item" },
                            self.full_factor_str::<false>(fact_using_v, Some(v_par_lf), false))
                }
            )
        } else if self.nt_has_all_flags(v_par_lf, ruleflag::PARENT_REPEAT) {
            if VERBOSE {
                println!("  full_factor_str({f_id}): P+*, v_par_lf={v_par_lf}, facts={}",
                         facts.iter().map(|f| f.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")).join(" | "));
            }
            (
                Symbol::NT(left).to_str(self.get_symbol_table()),
                format!("{}", facts.into_iter().map(|f| self.repeat_factor_str(&f, emphasis)).join(" | "))
            )
        } else {
            if VERBOSE {
                println!("  full_factor_str({f_id}): std, v_par_lf={v_par_lf}, facts={}",
                         facts.iter().map(|f| f.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")).join(" | "));
            }
            if self.nt_has_all_flags(v_par_lf, ruleflag::CHILD_L_RECURSION) && facts.len() == 1 && facts[0].first() == Some(&Symbol::Empty) {
                let lrec_facts = self.var_factors[v_par_lf as usize].iter()
                    .cloned()
                    .filter(|lrec_fid| self.parsing_table.factors[*lrec_fid as usize].1.first() != Some(&Symbol::Empty))
                    .to_vec();
                (
                    String::new(),
                    format!("end of iterations in {} -> {}",
                            Symbol::NT(parent).to_str(self.get_symbol_table()),
                            lrec_facts.into_iter().map(|lrec_fid| self.full_factor_components::<false>(lrec_fid, None, false).1).join(" | "))
                )
            } else {
                (
                    Symbol::NT(left).to_str(self.get_symbol_table()),
                    format!("{}{lfact_str}", facts.into_iter().map(|f| self.repeat_factor_str(&f, emphasis)).join(" | "))
                )
            }
        };
        if VERBOSE { println!("=> {result:?}"); }
        result
    }

    fn full_factor_str<const VERBOSE: bool>(&self, f_id: FactorId, emphasis: Option<VarId>, quote: bool) -> String {
        let (left, right) = self.full_factor_components::<VERBOSE>(f_id, emphasis, quote);
        if left.is_empty() {
            right
        } else {
            format!("{q}{left} -> {right}{q}", q = if quote { "`" } else { "" })
        }
    }

    fn repeat_factor_str(&self, f: &Vec<Symbol>, emphasis: Option<VarId>) -> String {
        // println!("repeat_factor_str({}, {emphasis:?})", f.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "));
        f.iter().map(|s| {
            if let Symbol::NT(v) = s {
                if self.nt_has_all_flags(*v, ruleflag::CHILD_REPEAT) {
                    let repeat_sym = if self.nt_has_all_flags(*v, ruleflag::REPEAT_PLUS) { '+' } else { '*' };
                    let is_lform = self.nt_has_all_flags(*v, ruleflag::L_FORM);
                    let mut fact = self.parsing_table.factors[self.var_factors[*v as usize][0] as usize].1.symbols().to_vec();
                    fact.pop(); // remove the loop NT
                    if is_lform {
                        let s = format!("{}({} <L>){}{}",
                                if emphasis == Some(*v) { " ► " } else { "" },
                                //self.repeat_factor_to_str(&fact, emphasis),
                                fact.into_iter().map(|s| {
                                    if self.sym_has_flags(&s, ruleflag::CHILD_REPEAT) {
                                        //format!("{{{}}}", s.to_str(self.get_symbol_table()))
                                        self.repeat_factor_str(&vec![s], emphasis)
                                    } else {
                                        s.to_str(self.get_symbol_table())
                                    }
                                }).join(" "),
                                repeat_sym,
                                if emphasis == Some(*v) { " ◄ " } else { "" });
                        // println!("= {s}");
                        s
                    } else {
                        format!("{}[{}]{}{}",
                                if emphasis == Some(*v) { " ► " } else { "" },
                                self.repeat_factor_str(&fact, emphasis),
                                repeat_sym,
                                if emphasis == Some(*v) { " ◄ " } else { "" })
                    }
                } else {
                    s.to_str(self.get_symbol_table())
                }
            } else {
                s.to_str(self.get_symbol_table())
            }
        }).join(" ")
    }

    fn build_opcodes(&mut self) {
        const VERBOSE: bool = false;
        for (factor_id, (var_id, factor)) in self.parsing_table.factors.iter().index() {
            if VERBOSE {
                println!("{}", factor.to_rule_str(*var_id, self.get_symbol_table(), 0));
            }
            let flags = self.parsing_table.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.parsing_table.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            if VERBOSE { println!("  - {}", new.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut opcode = Vec::<OpCode>::new();
            let parent = self.parsing_table.parent[*var_id as usize];
            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                let parent = parent.unwrap();
                // replaces Enter by Loop when going back to left-factorization parent, typically when coupled with + or *
                // (per construction, there can't be any factor going back to the grandparent or further up in a left factorization, so
                //  we don't check that)
                let parent_r_form_right_rec = self.parsing_table.flags[parent as usize] & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
                if new.get(0) == Some(&Symbol::NT(parent)) && !parent_r_form_right_rec {
                    opcode.push(OpCode::Loop(parent));
                    new.remove(0);
                }
            }
            let parent_lrec_no_lfact = flags & (ruleflag::PARENT_L_RECURSION | ruleflag::PARENT_L_FACTOR) == ruleflag::PARENT_L_RECURSION;
            if flags & ruleflag::PARENT_L_FACTOR == 0 ||
                parent_lrec_no_lfact ||
                new.iter().all(|s| if let Symbol::NT(ch) = s { !self.nt_has_all_flags(*ch, ruleflag::CHILD_L_FACTOR) } else { true })
            {
                // if it's not a parent of left factorization, or
                // if none of the NT in the factor is a child of left factorization, or
                // if it's the top parent of left recursion + left factorization,
                // => adds an Exit
                // (said otherwise: we don't want an exit in a parent or in the middle of a chain of left factorizations;
                //  the exit should be only at the end of left factorizations, or in factors that aren't left-factorized -
                //  except the special case of left recursion + left factorization, which needs the final exit)
                opcode.push(OpCode::Exit(factor_id)); // will be popped when this NT is completed
            }
            opcode.extend(new.into_iter().map(|s| OpCode::from(s)));
            let r_form_right_rec = flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
            if VERBOSE { println!("  r_form_right_rec = {r_form_right_rec} = {} || {}",
                                  flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0,
                                  flags & ruleflag::CHILD_L_FACTOR != 0 && self.parsing_table.flags[parent.unwrap() as usize] & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0); }
            if opcode.get(1).map(|op| op.matches(stack_sym)).unwrap_or(false) && !r_form_right_rec {
                // swaps Exit(self) when it's in 2nd position (only happens in [Loop(_), Exit(self), ...],
                // except right recursions that aren't left-form, because we let them unfold naturally (uses more stack)
                opcode.swap(0, 1);
                if VERBOSE { println!("  - swap 0, 1: {}", opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            } else if parent_lrec_no_lfact {
                if let Some(OpCode::NT(x)) = opcode.get(1) {
                    if self.nt_has_all_flags(*x, ruleflag::CHILD_L_RECURSION) {
                        // swaps Exit(self) and call to left recursive item so that the wrapper can issue an exit_NT
                        // with the correct context
                        opcode.swap(0, 1);
                        if VERBOSE { println!("  - swap 0, 1: {}", opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
                    }
                }
            } else if flags & ruleflag::CHILD_INDEPENDENT_AMBIGUITY != 0 && opcode.len() > 1 {
                // E_1: ◄4 ►E_2 ►E_1 abs  =>  ●E_2 ◄4 ●E_1 abs (where var_prime E_2 has child_amb flag)
                if let Some(OpCode::NT(var_prime)) = opcode.get(1) {
                    let vp = *var_prime; // to work around borrow checker
                    if self.nt_has_all_flags(vp, ruleflag::CHILD_AMBIGUITY) {
                        opcode.swap(0, 1);
                        opcode[0] = OpCode::Loop(vp);
                        if VERBOSE { println!("  - child indep ambig: {}", opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
                    }
                }
            }
            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                if opcode.len() >= 2 {
                    let fact_top = self.parsing_table.get_top_parent(*var_id);
                    if VERBOSE {
                        println!("  - check for initial exit swap: opcode = [{}], daddy = {}",
                                 opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                 Symbol::NT(fact_top).to_str(self.get_symbol_table()));
                    }
                    if self.parsing_table.flags[fact_top as usize] & ruleflag::PARENT_L_RECURSION != 0 &&
                        matches!(opcode[0], OpCode::Exit(_)) &&
                        matches!(opcode[1], OpCode::NT(v) if self.parsing_table.flags[v as usize] & ruleflag::CHILD_L_RECURSION != 0)
                    {
                        if VERBOSE {
                            println!("    swapping for initial exit_{}: {} <-> {}",
                                Symbol::NT(fact_top).to_str(self.get_symbol_table()).to_lowercase(),
                                opcode[0].to_str(self.get_symbol_table()),
                                opcode[1].to_str(self.get_symbol_table())
                            );
                        }
                        opcode.swap(0, 1);
                    }
                }
            }
            opcode.iter_mut().for_each(|o| {
                if let OpCode::NT(v) = o {
                    // replaces Enter by Loop when back to self,
                    // except right recursions that aren't left-form, because we let them unfold naturally (uses more stack)
                    if v == var_id && !r_form_right_rec {
                        *o = OpCode::Loop(*v)
                    }
                }
            });
            if VERBOSE { println!("  -> {}", opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            self.opcodes.push(opcode);
        }
    }

    fn get_group_factors(&self, g: &Vec<VarId>) -> Vec<(VarId, FactorId)> {
        g.iter().flat_map(|c|
            self.var_factors[*c as usize].iter().map(|f| (*c, *f))
        ).collect::<Vec<_>>()
    }

    /// Gathers all the alternatives in NT, and if some of them are parent_l_fact, searches the
    /// terminal child_l_fact instead. The result is the set of contexts that are used to
    /// call self.listener.exit_<NT>(ctx) for a right-rec, a left-rec parent, a left-rec child, ...
    fn gather_factors(&self, nt: VarId) -> Vec<FactorId> {
        const VERBOSE: bool = false;
        let mut alt = vec![];
        let mut explore = VecDeque::<VarId>::new();
        explore.push_back(nt);
        while !explore.is_empty() {
            let var = explore.pop_front().unwrap();
            if VERBOSE { println!("{var}: alt = {} | explore = {} | factors: {}",
                                  alt.iter().join(", "), explore.iter().join(", "),
                                  &self.var_factors[var as usize].iter().join(", ")); }
            for f in &self.var_factors[var as usize] {
                let (_, prodfactor) = &self.parsing_table.factors[*f as usize];
                if let Some(Symbol::NT(last)) = prodfactor.symbols().last() {
                    if self.nt_has_all_flags(*last, ruleflag::CHILD_L_FACTOR) {
                        // only one factor calls NT(last), so we won't push it twice in explore:
                        explore.push_back(*last);
                        continue;
                    }
                }
                alt.push(*f);
            }
            if VERBOSE { println!("  => alt = {} | explore = {}", alt.iter().join(", "), explore.iter().join(", ")); }
        }
        alt
    }

    pub(crate) fn build_item_ops(&mut self) {
        const VERBOSE: bool = false;
        let info = &self.parsing_table;
        let mut items = HashMap::<FactorId, Vec<Symbol>>::new();
        if VERBOSE {
            println!("Groups:");
            for g in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
                let group = self.get_group_factors(g);
                let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                println!("{}: {}, factors {}",
                         Symbol::NT(g[0]).to_str(self.get_symbol_table()),
                    ids.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "),
                    group.iter().map(|(_, f)| f.to_string()).join(", ")
                );
            }
        }
        // we proceed by var parent, then all factors in each parent/children group
        for g in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            // takes all the factors in the group (and their NT ID):
            let group = self.get_group_factors(g);
            let mut change = true;
            let g_top = g[0];
            let is_ambig = self.nt_has_all_flags(g_top, ruleflag::PARENT_AMBIGUITY);
            while change {
                change = false;
                let mut nt_used = HashSet::<VarId>::new();
                if VERBOSE {
                    let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                    println!("parent: {}, NT with value: {}",
                             Symbol::NT(g[0]).to_str(self.get_symbol_table()),
                             ids.into_iter().filter_map(|v|
                                 if self.nt_value[v as usize] { Some(Symbol::NT(v as VarId).to_str(self.get_symbol_table())) } else { None }
                             ).join(", "));
                }
                for (nt, factor_id) in &group {
                    let ambig_loop = is_ambig && self.nt_has_all_flags(*nt, ruleflag::CHILD_L_RECURSION);
                    items.insert(*factor_id, if ambig_loop { vec![Symbol::NT(g_top)] } else { vec![] });
                }
                for (var_id, factor_id) in &group {
                    let opcode = &self.opcodes[*factor_id as usize];
                    let (_, factor) = &info.factors[*factor_id as usize];
                    if VERBOSE {
                        print!("- {factor_id}: {} -> {}   [{}]",
                               Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                               factor.to_str(self.get_symbol_table()),
                               opcode.iter().map(|op| op.to_str(self.get_symbol_table())).join(" "));
                    }
                    let flags = info.flags[*var_id as usize];

                    // Default values are taken from opcodes. Loop(nt) is only taken if the parent is l-rec;
                    // we look at the parent's flags instead of the factor's because left factorization could
                    // displace the Loop(nt) to another non-l-rec child factor.
                    let mut values = self.opcodes[*factor_id as usize].iter().rev()
                        .filter_map(|s| {
                            let sym_maybe = match s {
                                OpCode::T(t) => Some(Symbol::T(*t)),
                                OpCode::NT(nt) => {
                                    let is_ambig_top = is_ambig && self.get_nt_parent(*nt) == Some(g_top)
                                        && !self.nt_has_any_flags(*nt, ruleflag::CHILD_L_RECURSION | ruleflag::CHILD_REPEAT);
                                    let var = if is_ambig_top { g_top } else { *nt };
                                    nt_used.insert(var);
                                    Some(Symbol::NT(var))
                                },
                                _ => {
                                    if VERBOSE { print!(" | {} dropped", s.to_str(self.get_symbol_table())); }
                                    None
                                }
                            };
                            sym_maybe.and_then(|s| if self.sym_has_value(&s) { Some(s) } else { None })
                        }).to_vec();
                    // Looks if a child_repeat has a value
                    if !values.is_empty() && self.parsing_table.parent[*var_id as usize].is_some() {
                        let mut top_nt = *var_id as usize;
                        while self.parsing_table.flags[top_nt] & ruleflag::CHILD_REPEAT == 0 {
                            if let Some(parent) = self.parsing_table.parent[top_nt] {
                                top_nt = parent as usize;
                            } else {
                                break;
                            }
                        }
                        // +* non-lform children have the same value as their parent, but +* lform children's "valueness" is independent from their parent's
                        if self.parsing_table.flags[top_nt] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {
                            if VERBOSE && !self.nt_value[top_nt] {
                                print!(" | {} is now valued {}",
                                       Symbol::NT(top_nt as VarId).to_str(self.get_symbol_table()),
                                       if nt_used.contains(&(top_nt as VarId)) { "and was used before" } else { "but wasn't used before" }
                                );
                            }
                            change |= !self.nt_value[top_nt] && nt_used.contains(&(top_nt as VarId));
                            self.nt_value[top_nt] = true;
                        }
                    }
                    if change {
                        // the nt_value of one item has been set.
                        if VERBOSE { println!("\nnt_value changed, redoing this group"); }
                        break;
                    }
                    // Loop NTs which carry values are kept on the stack, too
                    let parent_is_rrec_lfact = !is_ambig && self.nt_has_all_flags(g[0], ruleflag::R_RECURSION | ruleflag::PARENT_L_FACTOR);
                    if parent_is_rrec_lfact {
                        if self.nt_has_all_flags(*var_id, ruleflag::CHILD_L_FACTOR | ruleflag::L_FORM) {
                            if VERBOSE { print!(" child_rrec_lform_lfact"); }
                            items.get_mut(&factor_id).unwrap().insert(0, Symbol::NT(g[0]));
                        }
                    } else {
                        let sym_maybe = if flags & ruleflag::CHILD_REPEAT != 0 && (self.nt_value[*var_id as usize] || flags & ruleflag::L_FORM != 0) {
                            Some(Symbol::NT(*var_id))
                        } else if !is_ambig && flags & ruleflag::CHILD_L_RECURSION != 0 {
                            let parent = info.parent[*var_id as usize].unwrap();
                            Some(Symbol::NT(parent))
                        } else if !is_ambig && flags & (ruleflag::R_RECURSION | ruleflag::L_FORM) == ruleflag::R_RECURSION | ruleflag::L_FORM {
                            Some(Symbol::NT(*var_id))
                        } else {
                            None
                        };
                        if let Some(s) = sym_maybe {
                            if self.sym_has_value(&s) {
                                if VERBOSE { print!(" | loop => {}", s.to_str(self.get_symbol_table())); }
                                values.insert(0, s);
                            }
                        }
                    }
                    if VERBOSE { println!(" ==> [{}]", values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
                    if let Some(OpCode::NT(nt)) = opcode.get(0) {
                        // Take the values except the last NT
                        let backup = if matches!(values.last(), Some(Symbol::NT(x)) if x == nt) {
                            Some(values.pop().unwrap())
                        } else {
                            None
                        };
                        if nt != var_id && self.nt_has_all_flags(*nt, ruleflag::CHILD_L_RECURSION) {
                            if VERBOSE { println!("  CHILD_L_RECURSION"); }
                            // exit_<var_id>(context = values) before entering child loop
                            items.get_mut(&factor_id).unwrap().extend(values);
                            continue;
                        }
                        if flags & ruleflag::PARENT_L_FACTOR != 0 {
                            if VERBOSE {
                                println!("  PARENT_L_FACTOR: moving {} to child {}",
                                         values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                         Symbol::NT(*nt).to_str(self.get_symbol_table()));
                            }
                            // factorization reports all the values to the children
                            if let Some(pre) = items.get_mut(&factor_id) {
                                // pre-pends values that already exist for factor_id (and empties factor_id)
                                values.splice(0..0, std::mem::take(pre));
                            }
                            for f_id in self.var_factors[*nt as usize].iter() {
                                items.get_mut(f_id).unwrap().extend(values.clone());
                            }
                            continue;
                        }
                        if let Some(sym) = backup {
                            values.push(sym);
                        }
                    }
                    items.get_mut(&factor_id).unwrap().extend(values);
                }
            }
        }
        self.item_ops = items;
    }

    /// Calculates nt_name, nt_info, item_info
    ///
    /// * `nt_name[var]: (String, String, String)` contains (upper, lower, lower)-case unique identifiers for each parent NT (the first two
    ///                                            are changed if they're Rust identifiers)
    /// * `factor_info[factor]: Vec<Option<(VarId, String)>>` contains the enum variant names for each context (must be regrouped by VarId)
    /// * `item_info[factor]: Vec<ItemInfo>` contains the data available on the stacks when exiting the factor
    /// * `nt_repeat[var]: HashMap<VarId, Vec<ItemInfo>>` contains the data of + and * items
    ///
    /// For example:
    ///
    /// ```text
    /// // A -> (B c)* b | a; B -> b
    /// //
    /// //  0: A -> A_1 b     | ◄0 b! ►A_1    | A_1 b
    /// //  1: A -> a         | ◄1 a!         | a
    /// //  2: B -> b         | ◄2 b!         | b
    /// //  3: A_1 -> B c A_1 | ●A_1 ◄3 c! ►B | A_1 B c
    /// //  4: A_1 -> ε       | ◄4            |
    ///
    /// pub enum Ctx { A { a: SynA } }
    /// pub enum CtxA {
    ///     A1 { star: SynA1, b: String },
    ///     A2 { a: String },
    /// }
    /// pub enum CtxB {
    ///     B { b: String },
    /// }
    ///
    /// struct SynA1(Vec<SynA1Item>);
    /// struct SynA1Item { b: SynB, c: String }
    /// // User-defined: SynA, SynB
    ///
    /// nt_name = [("A", "a", "a"), ("B", "b", "b"), ("A1", "a1", "a1")]
    /// factor_info: [Some((0, "A1")), Some((0, "A2")), Some((1, "B")), None, None]
    /// item_info = [
    ///     [ItemInfo { name: "star", sym: NT(2), .. }, ItemInfo { name: "b", sym: T(1), .. }],
    ///     [ItemInfo { name: "a", sym: T(0), .. }],
    ///     [ItemInfo { name: "b", sym: T(1), .. }],
    ///     [ItemInfo { name: "star_it", sym: NT(2), .. }, ItemInfo { name: "b", sym: NT(1), .. }, ItemInfo { name: "c", sym: T(2), .. }],
    ///     []
    /// ]
    /// nt_repeat = {
    ///     2: [ItemInfo { name: "b", sym: NT(1), .. }, ItemInfo { name: "c", sym: T(2), .. }]
    /// }
    /// ```
    fn get_type_info(&self) -> (Vec<(String, String, String)>, Vec<Option<(VarId, String)>>, Vec<Vec<ItemInfo>>, HashMap<VarId, Vec<ItemInfo>>) {
        const VERBOSE: bool = false;

        let pinfo = &self.parsing_table;
        let mut nt_upper_fixer = NameFixer::new();
        let mut nt_lower_fixer = NameFixer::new();
        let mut nt_plower_fixer = NameFixer::new_empty(); // prefixed lowercase: don't worry about reserved words
        let nt_name: Vec<(String, String, String)> = (0..pinfo.num_nt).map(|v| {
            let name = self.symbol_table.get_nt_name(v as VarId);
            let nu = nt_upper_fixer.get_unique_name(name.to_camelcase());
            let nl = nt_lower_fixer.get_unique_name(nu.to_underscore_lowercase());
            let npl = nt_plower_fixer.get_unique_name(nu.to_underscore_lowercase());
            (nu, nl, npl)
        }).to_vec();

        let mut factor_info: Vec<Option<(VarId, String)>> = vec![None; pinfo.factors.len()];
        let mut nt_repeat = HashMap::<VarId, Vec<ItemInfo>>::new();
        let mut item_info: Vec<Vec<ItemInfo>> = vec![vec![]; pinfo.factors.len()];
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let is_ambig = self.nt_has_any_flags(group[0], ruleflag::PARENT_AMBIGUITY);
            let mut is_ambig_1st_child = is_ambig;
            let mut group_names = HashMap::<String, (usize, usize)>::new();
            for var in group {
                let nt = *var as usize;
                let nt_flags = pinfo.flags[nt];
                if is_ambig && (nt_flags & ruleflag::PARENT_L_RECURSION != 0 || (nt_flags & ruleflag::CHILD_L_RECURSION != 0 && !is_ambig_1st_child)) {
                    continue;
                }
                for &factor_id in &self.var_factors[nt] {
                    let i = factor_id as usize;
                    if is_ambig_1st_child && pinfo.factors[i].1.is_sym_empty() {
                        continue;
                    }
                    item_info[i] = if let Some(item_ops) = self.item_ops.get(&factor_id) {
                        // Adds a suffix to the names of different symbols that would otherwise collide in the same context option:
                        // - identical symbols are put in a vector (e.g. `id: [String; 2]`)
                        // - different symbols, which means T vs NT, must have different names (e.g. `NT(A)` becomes "a",
                        //   `T(a)` becomes "a", too => one is renamed to "a1" to avoid the collision: `{ a: SynA, a1: String }`)
                        let mut indices = HashMap::<Symbol, (String, Option<usize>)>::new();
                        let mut fixer = NameFixer::new();
                        let mut owner = pinfo.factors[i].0;
                        while let Some(parent) = pinfo.parent[owner as usize] {
                            if pinfo.flags[owner as usize] & ruleflag::CHILD_REPEAT != 0 {
                                // a child + * is owner
                                // - if <L>, it has its own public context and a user-defined return type
                                // - if not <L>, it has no context and a generator-defined return type (like Vec<String>)
                                // (we keep the loop for +, which has a left factorization, too)
                                break;
                            }
                            owner = parent;
                        }
                        let is_nt_repeat = pinfo.flags[owner as usize] & ruleflag::CHILD_REPEAT != 0;
                        for s in item_ops {
                            // if let Symbol::NT(v) = s {
                            //     if nt_name[*v as usize].is_none() {
                            //         let name = self.symbol_table.get_nt_name(*v);
                            //         nt_name[*v as usize] = Some((nt_upper_fixer.get_unique_name(name.to_camelcase()),
                            //                                      nt_lower_fixer.get_unique_name(name.to_underscore_lowercase()),
                            //                                      nt_plower_fixer.get_unique_name(name.to_underscore_lowercase())));
                            //     }
                            // }
                            if let Some((_, c)) = indices.get_mut(s) {
                                *c = Some(0);
                            } else {
                                let name = if let Symbol::NT(vs) = s {
                                    let flag = pinfo.flags[*vs as usize];
                                    if flag & ruleflag::CHILD_REPEAT != 0 {
                                        let inside_factor_id = self.var_factors[*vs as usize][0];
                                        let inside_factor = &pinfo.factors[inside_factor_id as usize].1;
                                        if false {
                                            // we don't use this any more
                                            let mut plus_name = inside_factor.symbols()[0].to_str(self.get_symbol_table()).to_underscore_lowercase();
                                            plus_name.push_str(if flag & ruleflag::REPEAT_PLUS != 0 { "_plus" } else { "_star" });
                                            plus_name
                                        } else {
                                            if is_nt_repeat && indices.is_empty() {
                                                // iterator variable in a + * loop (visible with <L>, for ex)
                                                if flag & ruleflag::REPEAT_PLUS != 0 { "plus_it".to_string() } else { "star_it".to_string() }
                                            } else {
                                                // reference to a + * result
                                                if flag & ruleflag::REPEAT_PLUS != 0 { "plus".to_string() } else { "star".to_string() }
                                            }
                                        }
                                    } else {
                                        nt_name[*vs as usize].clone().1
                                    }
                                } else {
                                    s.to_str(self.get_symbol_table()).to_lowercase()
                                };
                                indices.insert(*s, (fixer.get_unique_name(name), None));
                            }
                        }

                        // A parent of left factorization has no context, but we must check the factors that are the actual parents.
                        // The flag test is optional, but it serves to gate the more complex parental test.
                        let has_lfact_child = nt_flags & ruleflag::PARENT_L_FACTOR != 0 &&
                            pinfo.factors[i].1.symbols().iter().any(|s| matches!(s, &Symbol::NT(c) if pinfo.flags[c as usize] & ruleflag::CHILD_L_FACTOR != 0));

                        // (α)* doesn't call the listener for each α, unless it's l-form. We say it's a hidden child_repeat, and it doesn't need a context.
                        // The only children a child_repeat can have is due to left factorization in (α)+, so we check `owner` rather than `nt`.
                        let is_hidden_repeat_child = pinfo.flags[owner as usize] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT;

                        // (α <L>)+ have two similar factors with the same data on the stack, one that loops and the last iteration. We only keep one context
                        // because we use a flag to tell the listener when it's the last iteration (more convenient).
                        let is_duplicate = i > 0 && self.nt_has_all_flags(owner, ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) &&
                            factor_info[i - 1].as_ref().map(|fi| fi.0) == Some(owner);

                        let is_last_empty_iteration = (nt_flags & ruleflag::CHILD_L_RECURSION != 0 || self.nt_has_all_flags(*var, ruleflag::CHILD_REPEAT | ruleflag::L_FORM))
                            && self.is_factor_sym_empty(factor_id);

                        let has_context = !has_lfact_child && !is_hidden_repeat_child && !is_duplicate && !is_last_empty_iteration;
                        if VERBOSE {
                            println!("NT {nt}, factor {factor_id}: has_lfact_child = {has_lfact_child}, is_hidden_repeat_child = {is_hidden_repeat_child}, \
                                is_duplicate = {is_duplicate}, is_last_empty_iteration = {is_last_empty_iteration} => has_context = {has_context}");
                        }
                        if has_context {
                            let mut name = Symbol::NT(owner).to_str(self.get_symbol_table()).to_camelcase();
                            group_names.entry(name.clone())
                                .and_modify(|(last_i, number)| {
                                    if *number == 1 {
                                        NameFixer::add_number(&mut factor_info[*last_i].as_mut().unwrap().1, 1);
                                    }
                                    NameFixer::add_number(&mut name, *number + 1);
                                    factor_info[i] = Some((owner, name.clone()));
                                    *last_i = i;
                                    *number += 1;
                                })
                                .or_insert_with(|| {
                                    factor_info[i] = Some((owner, name));
                                    (i, 1)
                                });
                        }
                        if item_ops.is_empty() && nt_flags & ruleflag::CHILD_L_RECURSION != 0 {
                            // we put here the return context for the final exit of left recursive rule
                            if self.nt_value[owner as usize] {
                                vec![ItemInfo {
                                    name: nt_name[owner as usize].1.clone(),
                                    sym: Symbol::NT(owner),
                                    owner,
                                    index: None,
                                }]
                            } else {
                                vec![]
                            }
                        } else {
                            let mut infos = item_ops.into_iter().map(|s| {
                                let index = if let Some((_, Some(index))) = indices.get_mut(s) {
                                    let idx = *index;
                                    *index += 1;
                                    Some(idx)
                                } else {
                                    None
                                };
                                ItemInfo {
                                    name: indices[&s].0.clone(),
                                    sym: s.clone(),
                                    owner,
                                    index,
                                }
                            }).to_vec();
                            if self.nt_has_all_flags(owner, ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) {
                                // we add the flag telling the listener whether it's the last iteration or not
                                let last_name = fixer.get_unique_name("last_iteration".to_string());
                                infos.push(ItemInfo {
                                    name: last_name,
                                    sym: Symbol::Empty, // this marks the special flag variable
                                    owner,
                                    index: None,
                                });
                            };
                            if is_nt_repeat && infos.len() > 1 && !nt_repeat.contains_key(&owner) {
                                let iter = infos.iter().skip(1).cloned().to_vec();
                                if iter.len() > 1 || !iter[0].sym.is_t() {
                                    nt_repeat.insert(owner, iter);
                                }
                            }
                            infos
                        }
                    } else {
                        vec![]
                    };
                } // factor_id in var
                if is_ambig && nt_flags & ruleflag::CHILD_L_RECURSION != 0 {
                    is_ambig_1st_child = false;
                }
            } // var in group
        } // group

        if VERBOSE {
            println!("NT names: {}", nt_name.iter()
                .map(|(u, l, pl)| format!("{u}/{l}/{pl}"))
                .join(", "));
            println!("factor info:");
            for (factor_id, factor_names) in factor_info.iter().enumerate() {
                if let Some((v, name)) = factor_names {
                    println!("- factor {factor_id}, NT {v} {}, Ctx name: {name}", Symbol::NT(*v).to_str(self.get_symbol_table()));
                }
            }
            println!();
            println!("nt_name: {nt_name:?}");
            println!("factor_info: {factor_info:?}");
            println!("item_info:");
            for (i, item) in item_info.iter().enumerate().filter(|(_, item)| !item.is_empty()) {
                println!("- {i}: {{ {} }}", item.iter().map(|ii| format!("{} ({})", ii.name, ii.sym.to_str(self.get_symbol_table()))).join(", "));
            }
            // println!("item_info: {item_info:?}");
            println!("nt_repeat: {nt_repeat:?}");
        }
        (nt_name, factor_info, item_info, nt_repeat)
    }

    // Building the source code as we do below is not the most efficient, but it's done that way to
    // - be able to build only a part of the parser, and
    // - get the sources for the validation tests or print them / write them into a file.
    // The whole code isn't that big, so it's not a major issue.

    pub fn write_source_code(&mut self, file: Option<File>, indent: usize) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };
        let source = self.build_source_code(indent, true);
        out.write(source.as_bytes())?;
        // write!(out, "{source}");
        Ok(())
    }

    pub fn build_source_code(&mut self, indent: usize, wrapper: bool) -> String {
        let mut parts = vec![];
        let mut tmp_parts = vec![self.source_build_parser()];
        if wrapper {
            self.build_item_ops();
            tmp_parts.push(self.source_wrapper());
        }
        parts.push(self.source_use());
        parts.extend(tmp_parts);
        // Create source code:
        indent_source(parts, indent)
    }

    fn source_use(&self) -> Vec<String> {
        self.used_libs.build_source_code()
    }

    fn source_build_parser(&mut self) -> Vec<String> {
        let num_nt = self.symbol_table.get_num_nt();
        let num_t = self.symbol_table.get_num_t();
        for lib in [
            "lexigram_lib::grammar::ProdFactor",
            "lexigram_lib::grammar::Symbol",
            "lexigram_lib::grammar::VarId",
            "lexigram_lib::grammar::FactorId",
            "lexigram_lib::parser::OpCode",
            "lexigram_lib::parser::Parser",
            "lexigram_lib::FixedSymTable",
        ] {
            self.used_libs.add(lib);
        }

        let mut src = vec![
            format!("const PARSER_NUM_T: usize = {num_t};"),
            format!("const PARSER_NUM_NT: usize = {num_nt};"),
            format!("static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [{}];",
                     self.symbol_table.get_terminals().map(|(s, os)|
                         format!("(\"{s}\", {})", os.as_ref().map(|s| format!("Some(\"{s}\")")).unwrap_or("None".to_string()))).join(", ")),
            format!("static SYMBOLS_NT: [&str; PARSER_NUM_NT] = [{}];",
                     self.symbol_table.get_nonterminals().map(|s| format!("\"{s}\"")).join(", ")),
            format!("static FACTOR_VAR: [VarId; {}] = [{}];",
                     self.parsing_table.factors.len(),
                     self.parsing_table.factors.iter().map(|(v, _)| format!("{v}")).join(", ")),
        ];
        if self.include_factors {
            src.push(format!("static FACTORS: [&[Symbol]; {}] = [{}];",
                    self.parsing_table.factors.len(),
                    self.parsing_table.factors.iter().map(|(_, f)| format!("&[{}]", f.iter().map(|s| symbol_to_code(s)).join(", "))).join(", ")));
        }
        src.extend(vec![
            format!("static PARSING_TABLE: [FactorId; {}] = [{}];",
                     self.parsing_table.table.len(),
                     self.parsing_table.table.iter().map(|v| format!("{v}")).join(", ")),
            format!("static FLAGS: [u32; {}] = [{}];",
                     self.parsing_table.flags.len(), self.parsing_table.flags.iter().join(", ")),
            format!("static PARENT: [Option<VarId>; {}] = [{}];",
                     self.parsing_table.parent.len(), self.parsing_table.parent.iter().map(|p| if let Some(par) = p { format!("Some({par})") } else { format!("None") }).join(", ")),
            format!("static OPCODES: [&[OpCode]; {}] = [{}];", self.opcodes.len(),
                     self.opcodes.iter().map(|strip| format!("&[{}]", strip.into_iter().map(|op| format!("OpCode::{op:?}")).join(", "))).join(", ")),
            format!("static START_SYMBOL: VarId = {};\n", self.start),

            format!("pub fn build_parser() -> Parser<'static> {{"),
            format!("    let symbol_table = FixedSymTable::new("),
            format!("        SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),"),
            format!("        SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()"),
            format!("    );"),
            format!("    Parser::new("),
            format!("        PARSER_NUM_NT, PARSER_NUM_T + 1,"),
            format!("        &FACTOR_VAR,"),
            if self.include_factors{
                format!("        FACTORS.into_iter().map(|s| ProdFactor::new(s.to_vec())).collect(),")
            } else {
                format!("        Vec::new(),")
            },
            format!("        OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),"),
            format!("        &FLAGS,"),
            format!("        &PARENT,"),
            format!("        &PARSING_TABLE,"),
            format!("        symbol_table,"),
            format!("        START_SYMBOL"),
            format!("    )"),
            format!("}}"),
        ]);
        src
    }

    fn get_info_type(&self, infos: &Vec<ItemInfo>, info: &ItemInfo) -> String {
        let type_name_base = match info.sym {
            Symbol::T(_) => "String".to_string(),
            Symbol::NT(vs) => self.get_nt_type(vs).to_string(),
            Symbol::Empty => "bool".to_string(),
            _ => panic!("unexpected symbol {}", info.sym)
        };
        if info.index.is_some() {
            let nbr = infos.iter()
                .map(|nfo| if nfo.sym == info.sym { nfo.index.unwrap() } else { 0 })
                .max().unwrap() + 1;
            format!("[{type_name_base}; {nbr}]")
        } else {
            type_name_base
        }
    }

    /// Structure elements used in a context or in a +* child type
    fn source_infos(&self, infos: &Vec<ItemInfo>, add_pub: bool) -> String {
        let pub_str = if add_pub { "pub " } else { "" };
        infos.iter().filter_map(|info| {
            if info.index.is_none() || info.index == Some(0) {
                let type_name = self.get_info_type(&infos, &info);
                Some(format!("{pub_str}{}: {}", info.name, type_name))
            } else {
                None
            }
        }).join(", ")
    }

    fn is_factor_sym_empty(&self, f_id: FactorId) -> bool {
        self.parsing_table.factors[f_id as usize].1.is_sym_empty()
    }

    #[allow(unused)]
    fn source_wrapper(&mut self) -> Vec<String> {
        const VERBOSE: bool = false;
        const MATCH_COMMENTS_SHOW_DESCRIPTIVE_FACTORS: bool = false;

        self.used_libs.extend([
            "lexigram_lib::CollectJoin", "lexigram_lib::grammar::VarId", "lexigram_lib::parser::Call", "lexigram_lib::parser::ListenerWrapper",
            "lexigram_lib::grammar::FactorId", "lexigram_lib::log::Logger",
        ]);

        let (nt_name, factor_info, mut item_info, nt_repeat) = self.get_type_info();
        let pinfo = &self.parsing_table;

        let mut src = vec![];

        // Defines missing type names
        for (v, name) in nt_name.iter().enumerate().filter(|(v, _)| self.nt_value[*v]) {
            let v = v as VarId;
            if !self.nt_type.contains_key(&v) {
                self.nt_type.insert(v, format!("Syn{}", name.0));
            }
        }

        // Writes contexts
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let mut group_names = HashMap::<VarId, Vec<FactorId>>::new();
            // fetches the NT that have factor data
            for nt in group {
                let flags = pinfo.flags[*nt as usize];
                for &factor_id in &self.var_factors[*nt as usize] {
                    if let Some((owner, name)) = &factor_info[factor_id as usize] {
                        group_names.entry(*owner)
                            .and_modify(|v| v.push(factor_id))
                            .or_insert_with(|| vec![factor_id]);
                    }
                }
            }
            for &nt in group {
                if let Some(factors) = group_names.get(&nt) {
                    src.push(format!("#[derive(Debug)]"));
                    src.push(format!("pub enum Ctx{} {{", nt_name[nt as usize].0));
                    for &f_id in factors {
                        let (v, pf) = &self.parsing_table.factors[f_id as usize];
                        src.push(format!("    /// {}", self.full_factor_str::<false>(f_id, None, true)));
                        let ctx_content = self.source_infos(&item_info[f_id as usize], false);
                        let f_name = &factor_info[f_id as usize].as_ref().unwrap().1;
                        if ctx_content.is_empty() {
                            src.push(format!("    {f_name},", ))
                        } else {
                            src.push(format!("    {f_name} {{ {ctx_content} }},", ))
                        }
                    }
                    src.push(format!("}}"));
                }
            }
        }

        // Writes intermediate Syn types
        src.add_space();
        src.push("// NT types and user-defined type templates (copy elsewhere and uncomment when necessary):".to_string());
        src.add_space();
        let mut syns = Vec::<VarId>::new(); // list of valuable NTs
        for (v, names) in nt_name.iter().enumerate().filter(|(v, _)| self.nt_value[*v]) {
            let v = v as VarId;
            let (nu, nl, npl) = names;
            let nt_type = self.get_nt_type(v);
            if self.nt_has_all_flags(v, ruleflag::CHILD_REPEAT) {
                let parent = pinfo.get_top_parent(v);
                let tf = self.get_top_factors(v);
                let is_lform = self.nt_has_all_flags(v, ruleflag::L_FORM);
                let comment1 = tf.iter().map(|(var, f_id)| {
                    format!("{} in {}", if is_lform { "iteration" } else { "array" }, self.full_factor_str::<false>(*f_id, Some(v), true))
                }).join(", ");
                let comment2 = tf.iter().map(|(var, f_id)| {
                    format!("{} in {}", if is_lform { "iteration" } else { "item" }, self.full_factor_str::<false>(*f_id, Some(v), true))
                }).join(", ");
                let current = Symbol::NT(v).to_str(self.get_symbol_table());
                if let Some(infos) = nt_repeat.get(&(v)) {
                    if is_lform {
                        src.push(format!("// /// User-defined type for `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)));
                        src.push(format!("// #[derive(Debug, PartialEq)] pub struct {}();", self.get_nt_type(v)));
                        let extra_src = vec![
                            format!("/// User-defined type for `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)),
                            format!("#[derive(Debug, PartialEq)]"),
                            format!("pub struct {nt_type}();"),
                        ];
                        self.nt_extra_info.insert(v, (self.get_nt_type(v).to_string(), extra_src));
                    } else {
                        if infos.len() == 1 {
                            // single + * item; for ex. A -> (B)+
                            let info = &infos[0];
                            let type_name = self.get_info_type(&infos, &infos[0]);
                            src.push(format!("/// Computed `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)));
                            src.push(format!("#[derive(Debug, PartialEq)]"));
                            src.push(format!("pub struct {nt_type}(pub Vec<{type_name}>);", ));
                        } else {
                            // complex + * items; for ex. A -> (B b)+
                            src.push(format!("/// Computed `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)));
                            src.push(format!("#[derive(Debug, PartialEq)]"));
                            src.push(format!("pub struct {nt_type}(pub Vec<Syn{nu}Item>);"));
                            let mut fact = self.parsing_table.factors[self.var_factors[v as usize][0] as usize].1.symbols().to_vec();
                            fact.pop();
                            src.push(format!("/// `{}` {comment2}", self.repeat_factor_str(&fact, None)));
                            src.push(format!("#[derive(Debug, PartialEq)]"));
                            src.push(format!("pub struct Syn{nu}Item {{ {} }}", self.source_infos(&infos, true)));
                        }
                    }
                } else {
                    if is_lform {
                        src.push(format!("// /// User-defined type for `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)));
                        src.push(format!("// #[derive(Debug, PartialEq)] pub struct {}();", self.get_nt_type(v)));
                        let extra_src = vec![
                            format!("/// User-defined type for `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)),
                            format!("#[derive(Debug, PartialEq)]"),
                            format!("pub struct {nt_type}();"),
                        ];
                        self.nt_extra_info.insert(v, (self.get_nt_type(v).to_string(), extra_src));
                    } else {
                        // + * item is only a terminal
                        src.push(format!("/// Computed `{}` {comment1}", self.repeat_factor_str(&vec![Symbol::NT(v)], None)));
                        src.push(format!("#[derive(Debug, PartialEq)]"));
                        src.push(format!("pub struct {nt_type}(pub Vec<String>);"));
                    }
                }
            } else {
                src.push(format!("// /// User-defined type for `{}`", Symbol::NT(v).to_str(self.get_symbol_table())));
                src.push(format!("// #[derive(Debug, PartialEq)] pub struct {}();", self.get_nt_type(v)));
                let extra_src = vec![
                    format!("/// User-defined type for `{}`", Symbol::NT(v).to_str(self.get_symbol_table())),
                    format!("#[derive(Debug, PartialEq)]"),
                    format!("pub struct {}();", self.get_nt_type(v)),
                ];
                self.nt_extra_info.insert(v, (self.get_nt_type(v).to_string(), extra_src));
            }
            syns.push(v);
        }
        if !self.nt_value[self.start as usize] {
            let nu = &nt_name[self.start as usize].0;
            src.push(format!("/// Top non-terminal {nu} (has no value)"));
            src.push(format!("#[derive(Debug, PartialEq)]"));
            src.push(format!("pub struct Syn{nu}();"))
        }

        // Writes SynValue type and implementation
        if VERBOSE { println!("syns = {syns:?}"); }
        src.add_space();
        // SynValue type
        src.push(format!("#[derive(Debug)]"));
        src.push(format!("enum SynValue {{ {} }}",
                         syns.iter().map(|v| format!("{}({})", nt_name[*v as usize].0, self.get_nt_type(*v))).join(", ")));
        if !syns.is_empty() {
            // SynValue getters
            src.add_space();
            src.push("impl SynValue {".to_string());
            for v in &syns {
                let (nu, _, npl) = &nt_name[*v as usize];
                let nt_type = self.get_nt_type(*v);
                src.push(format!("    fn get_{npl}(self) -> {nt_type} {{"));
                if syns.len() == 1 {
                    src.push(format!("        let SynValue::{nu}(val) = self;"));
                    src.push(format!("        val"));
                } else {
                    src.push(format!("        if let SynValue::{nu}(val) = self {{ val }} else {{ panic!() }}"));
                }
                src.push(format!("    }}"));
            }
            src.push("}".to_string());
        }

        // Prepares the data for the following sections
        let mut src_init = Vec::<Vec<String>>::new();
        let mut src_exit = Vec::<Vec<String>>::new();
        let mut src_listener_decl = Vec::<String>::new();
        let mut src_wrapper_impl = Vec::<String>::new();
        let mut exit_fixer = NameFixer::new();

        // we proceed by var parent, then all factors in each parent/children group
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let parent_nt = group[0] as usize;
            let parent_flags = self.parsing_table.flags[parent_nt];
            let parent_has_value = self.nt_value[parent_nt];
            let mut exit_factor_done = HashSet::<VarId>::new();
            let mut init_nt_done = HashSet::<VarId>::new();
            if VERBOSE { println!("- GROUP {}, parent has {}value, parent flags: {}",
                                  group.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "),
                                  if parent_has_value { "" } else { "no " },
                                  ruleflag::to_string(parent_flags).join(" | ")); }
            let is_ambig = parent_flags & ruleflag::PARENT_AMBIGUITY != 0;
            let ambig_children = if is_ambig {
                group.iter().filter(|&v| self.nt_has_any_flags(*v, ruleflag::CHILD_L_RECURSION)).cloned().to_vec()
            } else {
                Vec::new()
            };
            let mut ambig_op_factors = BTreeMap::<FactorId, Vec<FactorId>>::new();
            for (id, f) in ambig_children.iter()        // id = operator priority/ID in ambig rule
                .flat_map(|v| self.gather_factors(*v))
                .filter_map(|f| self.parsing_table.factors[f as usize].1.get_original_factor_id().map(|id| (id, f)))
            {
                ambig_op_factors.entry(id).or_default().push(f);
            }
            if VERBOSE && is_ambig {
                println!("- ambig children vars: {}", ambig_children.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "));
                println!("  ambig op factors: {ambig_op_factors:?}");
            }
            for var in group {
                let sym_nt = Symbol::NT(*var);
                let nt = *var as usize;
                let flags = self.parsing_table.flags[nt];
                // the parents of left recursion are not useful in ambiguous rules (they just push / pop the same value):
                let is_ambig_1st_child =  is_ambig && flags & ruleflag::CHILD_L_RECURSION != 0 && ambig_children.get(0) == Some(var);
                // we only process the first variable of the left recursion; below we gather the factors of
                // the other variables of the same type (in ambiguous rules, they repeat the same operators)
                let is_ambig_redundant = is_ambig && flags & ruleflag::L_RECURSION != 0 && !is_ambig_1st_child;
                let has_value = self.nt_value[nt];
                let nt_comment = format!("// {}", sym_nt.to_str(self.get_symbol_table()));
                let is_parent = nt == parent_nt;
                let is_child_repeat_lform = self.nt_has_all_flags(*var, ruleflag::CHILD_REPEAT | ruleflag::L_FORM);
                if VERBOSE { println!("  - VAR {}, has {}value, flags: {}",
                                      sym_nt.to_str(self.get_symbol_table()),
                                      if has_value { "" } else { "no " },
                                      ruleflag::to_string(flags).join(" | ")); }

                // Call::Enter

                if self.parsing_table.parent[nt].is_none() {
                    let (nu, nl, npl) = &nt_name[nt];
                    init_nt_done.insert(*var);
                    if has_value && self.nt_has_all_flags(*var, ruleflag::R_RECURSION | ruleflag::L_FORM) {
                        src_wrapper_impl.push(String::new());
                        src_listener_decl.push(format!("    fn init_{npl}(&mut self) -> {};", self.get_nt_type(nt as VarId)));
                        src_init.push(vec![format!("                    {nt} => self.init_{nl}(),"), nt_comment]);
                        src_wrapper_impl.push(format!("    fn init_{npl}(&mut self) {{"));
                        src_wrapper_impl.push(format!("        let val = self.listener.init_{nl}();"));
                        src_wrapper_impl.push(format!("        self.stack.push(SynValue::{nu}(val));"));
                        src_wrapper_impl.push(format!("    }}"));
                    } else {
                        src_listener_decl.push(format!("    fn init_{npl}(&mut self) {{}}"));
                        src_init.push(vec![format!("                    {nt} => self.listener.init_{npl}(),"), nt_comment]);
                    }
                } else {
                    if flags & ruleflag::CHILD_REPEAT != 0 {
                        if has_value {
                            init_nt_done.insert(*var);
                            src_wrapper_impl.push(String::new());
                            let (nu, nl, npl) = &nt_name[nt];
                            src_init.push(vec![format!("                    {nt} => self.init_{npl}(),"), nt_comment]);
                            src_wrapper_impl.push(format!("    fn init_{npl}(&mut self) {{"));
                            if flags & ruleflag::L_FORM != 0 {
                                src_wrapper_impl.push(format!("        let val = self.listener.init_{npl}();"));
                                src_listener_decl.push(format!("    fn init_{npl}(&mut self) -> {};", self.get_nt_type(nt as VarId)));
                            } else {
                                src_wrapper_impl.push(format!("        let val = Syn{nu}(Vec::new());"));
                            }
                            src_wrapper_impl.push(format!("        self.stack.push(SynValue::{nu}(val));"));
                            src_wrapper_impl.push(format!("    }}"));
                        } else {
                            if flags & ruleflag::L_FORM != 0 {
                                init_nt_done.insert(*var);
                                let (nu, nl, npl) = &nt_name[nt];
                                src_init.push(vec![format!("                    {nt} => self.listener.init_{npl}(),"), nt_comment]);
                                src_listener_decl.push(format!("    fn init_{npl}(&mut self) {{}}"));
                            } else {
                                // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
                            }
                        }
                    } else if flags & (ruleflag::CHILD_L_RECURSION | ruleflag::CHILD_L_FACTOR) != 0 {
                        // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
                    } else {
                        // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
                    }
                }

                // Call::Exit

                fn make_match_choices(factors: &[FactorId], name: &str, flags: u32, no_method: bool, force_id: Option<FactorId>) -> (bool, Vec<String>) {
                    assert!(!factors.is_empty(), "factors cannot be empty");
                    // If + <L> child, the two factors are identical. We keep the two factors anyway because it's more coherent
                    // for the rest of the flow. At the end, when we generate the wrapper method, we'll discard the 2nd factor and use
                    // the `factor_id` parameter to determine whether it's the last iteration or not.
                    // We do discard the 2nd, empty factor immediately for a non-<L> * child because there's no associated context.
                    let discarded = if !no_method && flags & (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT { 1 } else { 0 };
                    // + children always have 2 left-factorized children with identical item_ops (one for the loop, one for the last iteration).
                    // There are two factors in the switch statement that call the wrapper method, but the factor_id is not required in non-<L> form because
                    // the data are the same and there's no context, hence the 2nd term of the following condition:
                    let is_factor_id = force_id.is_none() && factors.len() - discarded > 1 &&
                        flags & (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) != (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS);
                    let mut choices = Vec::<String>::new();
                    let force_id_str = force_id.map(|f| f.to_string()).unwrap_or_default();
                    if factors.len() - discarded == 1 {
                        if no_method {
                            choices.push(format!("                    {} => {{}}", factors[0]));
                        } else {
                            choices.push(format!("                    {} => self.{name}({force_id_str}),", factors[0], ));
                        }
                    } else {
                        choices.extend((0..factors.len() - 1 - discarded).map(|i| format!("                    {} |", factors[i])));
                        if no_method {
                            choices.push(format!("                    {} => {{}}", factors.last().unwrap()));
                        } else {
                            choices.push(format!("                    {} => self.{name}({}{force_id_str}),",
                                                 factors.last().unwrap(),
                                                 if is_factor_id { "factor_id" } else { "" }));
                        }
                    }
                    if discarded == 1 {
                        choices.push(format!("                    {} => {{}}", factors.last().unwrap()));
                    }
                    (is_factor_id, choices)
                }

                fn get_var_param(item: &ItemInfo, indices: &HashMap<Symbol, Vec<String>>, non_indices: &mut Vec<String>) -> Option<String> {
                    if let Some(index) = item.index {
                        if index == 0 {
                            Some(format!("{}: [{}]", item.name, indices[&item.sym].iter().rev().join(", ")))
                        } else {
                            None
                        }
                    } else {
                        let name = non_indices.pop().unwrap();
                        if name == item.name {
                            Some(name)
                        } else {
                            Some(format!("{}: {name}", item.name))
                        }
                    }
                }

                fn get_var_params(item_info: &Vec<ItemInfo>, skip: usize, indices: &HashMap<Symbol, Vec<String>>, non_indices: &mut Vec<String>) -> String {
                    item_info.iter().skip(skip).filter_map(|item| {
                        get_var_param(item, indices, non_indices)
                    }).join(", ")
                }

                // handles most rules except children of left factorization (already taken by self.gather_factors)
                if !is_ambig_redundant && flags & ruleflag::CHILD_L_FACTOR == 0 {
                    let (nu, nl, npl) = &nt_name[nt];
                    let (pnu, pnl, pnpl) = &nt_name[parent_nt];
                    if VERBOSE { println!("    {nu} (parent {pnu})"); }
                    let no_method = !has_value && flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT;
                    let (fnpl, fnu, fnt, f_valued) = if is_ambig_1st_child {
                        (pnpl, pnu, parent_nt, parent_has_value)    // parent_nt doesn't come through this code, so we must do it now
                    } else {
                        (npl, nu, nt, has_value)
                    };
                    if is_parent || (is_child_repeat_lform && !no_method) || is_ambig_1st_child {
                        if f_valued {
                            src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, _ctx: Ctx{fnu}) -> {};", self.get_nt_type(fnt as VarId)));
                        } else {
                            src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, _ctx: Ctx{fnu}) {{}}"));
                        }
                    }
                    let mut all_exit_factors = if is_ambig_1st_child {
                        ambig_op_factors.values().rev().map(|v| v[0]).to_vec()
                    } else {
                        self.gather_factors(nt as VarId)
                    };
                    let (last_it_factors, mut exit_factors) = all_exit_factors.into_iter()
                        .partition::<Vec<_>, _>(|f| /*factor_info[*f as usize].is_none() &&*/
                            (flags & ruleflag::CHILD_L_RECURSION != 0
                                || flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM | ruleflag::REPEAT_PLUS) == ruleflag::CHILD_REPEAT | ruleflag::L_FORM)
                            && self.is_factor_sym_empty(*f));
                    if VERBOSE {
                        println!("    no_method: {no_method}, exit factors: {}", exit_factors.iter().join(", "));
                        if !last_it_factors.is_empty() {
                            println!("    last_it_factors: {}", last_it_factors.iter().join(", "));
                        }
                    }
                    for f in &exit_factors {
                        exit_factor_done.insert(*f);
                    }
                    let inter_or_exit_name = if flags & ruleflag::PARENT_L_RECURSION != 0 { format!("inter_{npl}") } else { format!("exit_{npl}") };
                    let fn_name = exit_fixer.get_unique_name(inter_or_exit_name.clone());
                    let (is_factor_id, choices) = make_match_choices(&exit_factors, &fn_name, flags, no_method, None);
                    if VERBOSE { println!("    choices: {}", choices.iter().map(|s| s.trim()).join(" ")); }
                    let comments = exit_factors.iter().map(|f| {
                        let (v, pf) = &self.parsing_table.factors[*f as usize];
                        if MATCH_COMMENTS_SHOW_DESCRIPTIVE_FACTORS {
                            format!("// {}", self.full_factor_str::<false>(*f, None, false))
                        } else {
                            format!("// {}", pf.to_rule_str(*v, self.get_symbol_table(), self.parsing_table.flags[*v as usize]))
                        }
                    }).to_vec();
                    src_exit.extend(choices.into_iter().zip(comments).map(|(a, b)| vec![a, b]));
                    if is_ambig_1st_child {
                        for (f_id, dup_factors) in ambig_op_factors.values().rev().filter_map(|v| if v.len() > 1 { v.split_first() } else { None }) {
                            // note: is_factor_id must be true because we wouldn't get duplicate factors otherwise in an ambiguous rule
                            //       (it's duplicated to manage the priority between several factors, which are all in the first NT)
                            let (_, choices) = make_match_choices(dup_factors, &fn_name, 0, no_method, Some(*f_id));
                            let comments = dup_factors.iter()
                                .map(|f| {
                                    let (v, pf) = &pinfo.factors[*f as usize];
                                    format!("// {} (duplicate of {f_id})", pf.to_rule_str(*v, self.get_symbol_table(), 0))
                                }).to_vec();
                            src_exit.extend(choices.into_iter().zip(comments).map(|(a, b)| vec![a, b]));
                            for f in dup_factors {
                                exit_factor_done.insert(*f);
                            }
                        }
                    }
                    if !no_method {
                        src_wrapper_impl.push(String::new());
                        src_wrapper_impl.push(format!("    fn {fn_name}(&mut self{}) {{", if is_factor_id { ", factor_id: FactorId" } else { "" }));
                    }
                    if flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {
                        if exit_factors.len() > 2 {
                            self.log.add_error(format!("alternatives in * and + are not supported: in {}. {} has too many factors: {}",
                                                       Symbol::NT(parent_nt as VarId).to_str(self.get_symbol_table()),
                                                       sym_nt.to_str(self.get_symbol_table()),
                                                       exit_factors.iter().join(", ")));
                            continue;
                        }

                        if has_value {
                            let f = exit_factors[0];
                            if VERBOSE {
                                println!("    - FACTOR {f}: {} -> {}",
                                         sym_nt.to_str(self.get_symbol_table()),
                                         self.parsing_table.factors[f as usize].1.to_str(self.get_symbol_table()));
                            }
                            let mut var_fixer = NameFixer::new();
                            let mut indices = HashMap::<Symbol, Vec<String>>::new();
                            let mut non_indices = Vec::<String>::new();
                            for item in item_info[f as usize].iter().rev() {
                                let varname = if let Some(index) = item.index {
                                    let name = var_fixer.get_unique_name(format!("{}_{}", item.name, index + 1));
                                    indices.entry(item.sym).and_modify(|v| v.push(name.clone())).or_insert(vec![name.clone()]);
                                    name
                                } else {
                                    let name = item.name.clone();
                                    non_indices.push(name.clone());
                                    name
                                };
                                if let Symbol::NT(v) = item.sym {
                                    let mut_s = if /* !is_child_repeat_lform &&*/ v as usize == nt { "mut " } else { "" };
                                    src_wrapper_impl.push(format!("        let {mut_s}{varname} = self.stack.pop().unwrap().get_{}();",
                                                                  nt_name[v as usize].2));
                                } else {
                                    src_wrapper_impl.push(format!("        let {varname} = self.stack_t.pop().unwrap();"));
                                }
                            }
                            let var_name = non_indices.pop().unwrap();
                            let is_simple = item_info[f as usize].len() == 2 && item_info[f as usize][1].sym.is_t(); // Vec<String>
                            if is_simple {
                                src_wrapper_impl.push(format!("        {var_name}.0.push({});", &non_indices[0]));
                            } else {
                                if item_info[f as usize].len() == 2 {
                                    src_wrapper_impl.push(format!("        {var_name}.0.push({});",
                                                                  get_var_param(&item_info[f as usize][1], &indices, &mut non_indices).unwrap()));
                                } else {
                                    let params = get_var_params(&item_info[f as usize], 1, &indices, &mut non_indices);
                                    src_wrapper_impl.push(format!("        {var_name}.0.push(Syn{nu}Item {{ {params} }});"));
                                }
                            }
                            src_wrapper_impl.push(format!("        self.stack.push(SynValue::{nu}({var_name}));"));
                        }
                    } else {
                        // no_method is not expected here (only used in +* with no lform)
                        assert!(!no_method);
                        let has_last_flag = is_child_repeat_lform && flags & ruleflag::REPEAT_PLUS != 0; // special last flag
                        let last_factor_id = if has_last_flag {
                            exit_factors.pop() // removes the duplicate and only leaves one factor
                        } else {
                            None
                        };
                        let fnu = if is_child_repeat_lform { nu } else { pnu }; // +* <L> use the loop variable, the other factors use the parent
                        let fnpl = if is_child_repeat_lform { npl } else { pnpl }; // +* <L> use the loop variable, the other factors use the parent
                        let f_has_value = if is_child_repeat_lform { has_value } else { parent_has_value };
                        let is_single = has_last_flag || exit_factors.len() == 1;
                        let indent = if is_single { "        " } else { "                " };
                        if !is_single {
                            src_wrapper_impl.push(format!("        let ctx = match factor_id {{"));
                        }
                        for f in exit_factors {
                            if VERBOSE {
                                println!("    - FACTOR {f}: {} -> {}",
                                         Symbol::NT(*var).to_str(self.get_symbol_table()),
                                         self.parsing_table.factors[f as usize].1.to_str(self.get_symbol_table()));
                            }
                            let mut var_fixer = NameFixer::new();
                            let mut indices = HashMap::<Symbol, Vec<String>>::new();
                            let mut non_indices = Vec::<String>::new();
                            if !is_single {
                                src_wrapper_impl.push(format!("            {f} => {{"));
                            }
                            for item in item_info[f as usize].iter().rev() {
                                let varname = if let Some(index) = item.index {
                                    let name = var_fixer.get_unique_name(format!("{}_{}", item.name, index + 1));
                                    indices.entry(item.sym).and_modify(|v| v.push(name.clone())).or_insert(vec![name.clone()]);
                                    name
                                } else {
                                    let name = item.name.clone();
                                    non_indices.push(name.clone());
                                    name
                                };
                                if item.sym.is_empty() {
                                    assert!(has_last_flag);
                                    src_wrapper_impl.push(format!("{indent}let {varname} = factor_id == {};", last_factor_id.unwrap()));
                                } else if let Symbol::NT(v) = item.sym {
                                    src_wrapper_impl.push(format!("{indent}let {varname} = self.stack.pop().unwrap().get_{}();",
                                                                  nt_name[v as usize].2));
                                } else {
                                    src_wrapper_impl.push(format!("{indent}let {varname} = self.stack_t.pop().unwrap();"));
                                }
                            }
                            let ctx_params = get_var_params(&item_info[f as usize], 0, &indices, &mut non_indices);
                            let ctx = if ctx_params.is_empty() {
                                format!("Ctx{fnu}::{}", factor_info[f as usize].as_ref().unwrap().1)
                            } else {
                                format!("Ctx{fnu}::{} {{ {ctx_params} }}", factor_info[f as usize].as_ref().unwrap().1)
                            };
                            if is_single {
                                src_wrapper_impl.push(format!("        {}self.listener.exit_{fnpl}({ctx});", if f_has_value { "let val = " } else { "" }));
                                if f_has_value {
                                    src_wrapper_impl.push(format!("        self.stack.push(SynValue::{fnu}(val));"));
                                }
                            } else {
                                src_wrapper_impl.push(format!("{indent}{ctx}"));
                                src_wrapper_impl.push(format!("            }}"));
                            }
                        }
                        if !is_single {
                            src_wrapper_impl.push(format!("            _ => panic!(\"unexpected factor id {{factor_id}} in fn {fn_name}\")"));
                            src_wrapper_impl.push(format!("        }};"));
                            src_wrapper_impl.push(format!("        {}self.listener.exit_{fnpl}(ctx);", if f_has_value { "let val = " } else { "" }));
                            if f_has_value {
                                src_wrapper_impl.push(format!("        self.stack.push(SynValue::{fnu}(val));"));
                            }
                        }
                    }
                    if !no_method {
                        src_wrapper_impl.push(format!("    }}"));
                    }
                    for f in last_it_factors {
                        if let Some(info) = item_info[f as usize].get(0) {
                            if VERBOSE { println!("last_it_factors: {f}, info = {info:?}"); }
                            let (variant, _, fnname) = &nt_name[info.owner as usize];
                            let typ = self.get_nt_type(info.owner);
                            let varname = &info.name;
                            src_listener_decl.push(format!("    fn exitloop_{fnname}(&mut self, _{varname}: &mut {typ}) {{}}"));
                            let (v, pf) = &self.parsing_table.factors[f as usize];
                            let factor_str = if MATCH_COMMENTS_SHOW_DESCRIPTIVE_FACTORS {
                                self.full_factor_str::<false>(f, None, false)
                            } else {
                                pf.to_rule_str(*v, self.get_symbol_table(), self.parsing_table.flags[*v as usize])
                            };
                            src_exit.push(vec![format!("                    {f} => self.exitloop_{fnpl}(),"), format!("// {factor_str}")]);
                            exit_factor_done.insert(f);
                            src_wrapper_impl.push(String::new());
                            src_wrapper_impl.push(format!("    fn exitloop_{fnpl}(&mut self) {{"));
                            src_wrapper_impl.push(format!("        let SynValue::{variant}({varname}) = self.stack.last_mut().unwrap(){};",
                                                          if syns.len() > 1 { " else { panic!() }" } else { "" }));
                            src_wrapper_impl.push(format!("        self.listener.exitloop_{fnname}({varname});"));
                            src_wrapper_impl.push(format!("    }}"));
                        }
                    }
                }
            }
            for f in group.iter().flat_map(|v| &self.var_factors[*v as usize]).filter(|f| !exit_factor_done.contains(f)) {
                let is_called = self.opcodes[*f as usize].iter().any(|o| *o == OpCode::Exit(*f));
                let (v, pf) = &self.parsing_table.factors[*f as usize];
                let factor_str = if MATCH_COMMENTS_SHOW_DESCRIPTIVE_FACTORS {
                    self.full_factor_str::<false>(*f, None, false)
                } else {
                    pf.to_rule_str(*v, self.get_symbol_table(), self.parsing_table.flags[*v as usize])
                };
                let comment = format!("// {factor_str} ({})", if is_called { "not used" } else { "never called" });
                if is_called {
                    src_exit.push(vec![format!("                    {f} => {{}}"), comment]);
                } else {
                    src_exit.push(vec![format!("                 /* {f} */"), comment]);
                }
            }
            // adds unused init calls, using Segments to regroup them
            let mut seg_init = Segments::from_iter(
                group.into_iter()
                    .filter_map(|&v| if !init_nt_done.contains(&v) { Some(Seg(v as u32, v as u32)) } else { None })
            );
            seg_init.normalize();
            for seg in seg_init {
                let Seg(a, b) = seg;
                if a == b {
                    src_init.push(vec![format!("                    {a} => {{}}"), format!("// {}", Symbol::NT(a as VarId).to_str(self.get_symbol_table()))]);
                } else {
                    src_init.push(vec![
                        format!("                    {a}{}{b} => {{}}", if b == a + 1 { " | " } else { " ..= " }),
                        format!("// {}", (a..=b).map(|v| Symbol::NT(v as VarId).to_str(self.get_symbol_table())).join(", "))
                    ]);
                }
            }
        }

        // Writes the listener trait declaration
        src.add_space();
        src.push(format!("pub trait {}Listener {{", self.name));
        src.push(format!("    /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from"));
        src.push(format!("    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`."));
        src.push(format!("    fn check_abort_request(&self) -> bool {{ false }}"));
        src.push(format!("    fn get_mut_log(&mut self) -> &mut impl Logger;"));
        if self.nt_value[self.start as usize] {
            src.push(format!("    fn exit(&mut self, _{}: {}) {{}}", nt_name[self.start as usize].2, self.get_nt_type(self.start)));
        } else {
            src.push(format!("    fn exit(&mut self) {{}}"));
        }
        /*
                              fn init_a(&mut self) {}
                              fn exit_a(&mut self, _ctx: CtxA) -> SynA;
                              fn init_a_iter(&mut self) -> SynAIter;
                              fn exit_a_iter(&mut self, _ctx: CtxAIter) -> SynAIter;
        */
        src.extend(src_listener_decl);
        src.push(format!("}}"));

        // Writes the switch() function
        src.add_space();
        src.push(format!("pub struct Wrapper<T> {{"));
        src.push(format!("    verbose: bool,"));
        src.push(format!("    listener: T,"));
        src.push(format!("    stack: Vec<SynValue>,"));
        src.push(format!("    max_stack: usize,"));
        src.push(format!("    stack_t: Vec<String>,"));
        src.push(format!("}}"));
        src.push(format!(""));
        src.push(format!("impl<T: {}Listener> ListenerWrapper for Wrapper<T> {{", self.name));
        src.push(format!("    fn switch(&mut self, call: Call, nt: VarId, factor_id: FactorId, t_data: Option<Vec<String>>) {{"));
        src.push(format!("        if self.verbose {{"));
        src.push(format!("            println!(\"switch: call={{call:?}}, nt={{nt}}, factor={{factor_id}}, t_data={{t_data:?}}\");"));
        src.push(format!("        }}"));
        src.push(format!("        if let Some(mut t_data) = t_data {{"));
        src.push(format!("            self.stack_t.append(&mut t_data);"));
        src.push(format!("        }}"));
        src.push(format!("        match call {{"));
        src.push(format!("            Call::Enter => {{"));
        src.push(format!("                match nt {{"));
        /*
                                              0 => self.listener.init_a(),                // A
                                              1 => self.init_a_iter(),                    // AIter1
                                              2 => {}                                     // A_1
        */
        src.extend(columns_to_str(src_init, Some(vec![64, 0])));
        src.push(format!("                    _ => panic!(\"unexpected enter non-terminal id: {{nt}}\")"));
        src.push(format!("                }}"));
        src.push(format!("            }}"));
        src.push(format!("            Call::Loop => {{}}"));
        src.push(format!("            Call::Exit => {{"));
        src.push(format!("                match factor_id {{"));
        /*
                                              3 |                                         // A -> a a (b <L>)* c
                                              4 => self.exit_a(factor_id),                // A -> a c (b <L>)* c
                                              1 => self.exit_a_iter(),                    // (b <L>)* iteration in A -> a a  ► (b <L>)* ◄  c | ...
                                              2 => {}                                     // end of (b <L>)* iterations in A -> a a  ► (b <L>)* ◄  c | ...
                                           /* 0 */                                        // A -> a a (b <L>)* c | a c (b <L>)* c (never called)
        */
        src.extend(columns_to_str(src_exit, Some(vec![64, 0])));
        src.push(format!("                    _ => panic!(\"unexpected exit factor id: {{factor_id}}\")"));
        src.push(format!("                }}"));
        src.push(format!("            }}"));
        src.push(format!("            Call::End => {{"));
        if self.nt_value[self.start as usize] {
            src.push(format!("                self.exit();"));
        } else {
            src.push(format!("                self.listener.exit();"));
        }
        src.push(format!("            }}"));
        src.push(format!("        }}"));
        src.push(format!("        self.max_stack = std::cmp::max(self.max_stack, self.stack.len());"));
        src.push(format!("        if self.verbose {{"));
        src.push(format!("            println!(\"> stack_t:   {{}}\", self.stack_t.join(\", \"));"));
        src.push(format!("            println!(\"> stack:     {{}}\", self.stack.iter().map(|it| format!(\"{{it:?}}\")).join(\", \"));"));
        src.push(format!("        }}"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    fn check_abort_request(&self) -> bool {{"));
        src.push(format!("        self.listener.check_abort_request()"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    fn get_mut_log(&mut self) -> &mut impl Logger {{"));
        src.push(format!("        self.listener.get_mut_log()"));
        src.push(format!("    }}"));
        src.push(format!("}}"));

        src.add_space();
        src.push(format!("impl<T: {}Listener> Wrapper<T> {{", self.name));
        src.push(format!("    pub fn new(listener: T, verbose: bool) -> Self {{"));
        src.push(format!("        Wrapper {{ verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }}"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn get_listener(&self) -> &T {{"));
        src.push(format!("        &self.listener"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn get_mut_listener(&mut self) -> &mut T {{"));
        src.push(format!("        &mut self.listener"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn listener(self) -> T {{"));
        src.push(format!("        self.listener"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn set_verbose(&mut self, verbose: bool) {{"));
        src.push(format!("        self.verbose = verbose;"));
        src.push(format!("    }}"));
        if self.nt_value[self.start as usize] {
            src.push(format!(""));
            src.push(format!("    fn exit(&mut self) {{"));
            let (nu, nl, npl) = &nt_name[self.start as usize];
            src.push(format!("        let {nl} = self.stack.pop().unwrap().get_{npl}();"));
            src.push(format!("        self.listener.exit({nl});"));
            src.push(format!("    }}"));
        }
/*
                              impl<T: TestListener> ListenerWrapper<T> {
                                  fn exit(&mut self) {
                                      let a = self.stack.pop().unwrap().get_a();
                                      self.listener.exit(Ctx::A { a });
                                  }
                                  fn init_a_iter(&mut self) {
                                      let val = self.listener.init_a_iter();
                                      self.stack.push(SynValue::AIter(val));
                                  }
                                  fn exit_a_iter(&mut self) {
                                      let b = self.stack_t.pop().unwrap();
                                      let star_it = self.stack.pop().unwrap().get_a_iter();
                                      let val = self.listener.exit_a_iter(CtxAIter::Aiter1 { star_it, b });
                                      self.stack.push(SynValue::AIter(val));
                                  }
                                  // ...
                              }
*/
        src.extend(src_wrapper_impl);
        src.push(format!("}}"));

        src
    }
}

impl<T> From<ProdRuleSet<T>> for ParserGen where ProdRuleSet<LL1>: From<ProdRuleSet<T>> {
    /// Creates a [`ParserGen`] from a set of production rules.
    /// If the rule set has a name, it's transmitted to the parser generator to name the user
    /// listener trait in the generated code. If the rule set has no name, a default "Parser" name
    /// is used instead (unless the name is set with [`ParserGen::set_name()`].
    fn from(mut rules: ProdRuleSet<T>) -> Self {
        let name = rules.name.take().unwrap_or(DEFAULT_LISTENER_NAME.to_string());
        ParserGen::from_rules(rules, name)
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

pub fn print_items(builder: &ParserGen, indent: usize, show_symbols: bool) {
    let tbl = builder.get_symbol_table();
    let fields = (0..builder.parsing_table.factors.len())
        .filter_map(|f| {
            let f_id = f as FactorId;
            let (v, factor) = &builder.parsing_table.factors[f];
            let ops = &builder.opcodes[f];
            if let Some(it) = builder.item_ops.get(&f_id) {
                let mut cols = vec![];
                if show_symbols {
                    cols.push(format!("{f_id} => symbols![{}],", it.iter().map(|s| s.to_macro_item()).join(", ")));
                }
                cols.extend([
                    format!("// {f_id:2}: {} -> {}", Symbol::NT(*v).to_str(tbl), factor.iter().map(|s| s.to_str(tbl)).join(" ")),
                    format!("| {}", ops.into_iter().map(|s| s.to_str(tbl)).join(" ")),
                    format!("| {}", it.iter().map(|s| s.to_str(tbl)).join(" ")),
                ]);
                Some(cols)
            } else {
                None
            }
        }).to_vec();
    let widths = if show_symbols { vec![40, 0, 0, 0] } else { vec![16, 0, 0] };
    for l in columns_to_str(fields, Some(widths)) {
        println!("{:indent$}{l}", "", indent = indent)
    }
}

pub fn print_flags(builder: &ParserGen, indent: usize) {
    let tbl = builder.get_symbol_table();
    let prefix = format!("{:width$}//", "", width = indent);
    let nt_flags = builder.get_parsing_table().flags.iter().index().filter_map(|(nt, &f)|
        if f != 0 { Some(format!("{prefix}  - {}: {} ({})", Symbol::NT(nt).to_str(tbl), ruleflag::to_string(f).join(" | "), f)) } else { None }
    ).join("\n");
    let parents = builder.get_parsing_table().parent.iter().index().filter_map(|(c, &par)|
        if let Some(p) = par { Some(format!("{prefix}  - {} -> {}", Symbol::NT(c).to_str(tbl), Symbol::NT(p).to_str(tbl))) } else { None }
    ).join("\n");
    println!("{prefix} NT flags:\n{}", if nt_flags.is_empty() { format!("{prefix}  - (nothing)") } else { nt_flags });
    println!("{prefix} parents:\n{}", if parents.is_empty() { format!("{prefix}  - (nothing)") } else { parents });
}
