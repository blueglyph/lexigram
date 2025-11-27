// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use iter_index::IndexerIterator;
use crate::grammar::{LLParsingTable, ProdRuleSet, ruleflag, RuleTreeSet, Symbol, VarId, AltId, NTConversion, Alternative, grtree_to_str, GrTreeExt};
use crate::{CollectJoin, General, LL1, Normalized, SourceSpacer, SymbolTable, SymInfoTable, NameTransformer, NameFixer, columns_to_str, StructLibs, indent_source, FixedSymTable, HasBuildErrorSource, BuildError, BuildErrorSource};
use crate::grammar::origin::{FromPRS, Origin};
use crate::log::{BufLog, BuildFrom, LogMsg, LogReader, LogStatus, Logger, TryBuildFrom};
use crate::parser::{OpCode, Parser, SpanNbr};
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

    pub fn has_span(&self) -> bool {
        matches!(self, OpCode::T(_) | OpCode::NT(_))
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

    pub fn to_str_quote<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        if let Some(t) = symbol_table {
            match self {
                OpCode::T(v) => format!("{}{}", Symbol::T(*v).to_str_quote(symbol_table), if t.is_token_data(*v) { "!" } else { "" }),
                _ => self.to_str(symbol_table)
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_ext(&self, symbol_table: Option<&SymbolTable>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(t) = symbol_table {
            if let OpCode::T(tok) = self {
                if t.is_symbol_t_data(&Symbol::T(*tok)) {
                    result.push_str(&format!("({ext})"));
                }
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

#[derive(Clone, Debug, PartialEq)]
struct ItemInfo {
    name: String,
    sym: Symbol,            // NT(var) or T(token)
    owner: VarId,           // NT owning this item; for ex. owner = `A` for `sym = b` in `A -> a b+ c`
    index: Option<usize>    // when several identical symbols in the same alternative: `A -> id := id ( id )`
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
    alt_var: Vec<VarId>,
    alts: Vec<Alternative>,
    opcodes: Vec<Vec<OpCode>>,
    table: Vec<AltId>,
    symbol_table: FixedSymTable,
    start: VarId,
    include_alts: bool,
}

impl ParserTables {
    pub fn new(parsing_table: LLParsingTable, symbol_table: FixedSymTable, opcodes: Vec<Vec<OpCode>>, start: VarId, include_alts: bool) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        let num_nt = parsing_table.num_nt;
        let num_t = parsing_table.num_t;
        let table = parsing_table.table;
        let (factor_var, alts): (Vec<_>, Vec<_>) = parsing_table.alts.into_iter().unzip();
        ParserTables { num_nt, num_t, alt_var: factor_var, alts, opcodes, table, symbol_table, start, include_alts }
    }

    pub fn make_parser(&self) -> Parser<'_> {
        Parser::new(
            self.num_nt,
            self.num_t,
            self.alt_var.as_slice(),
            if self.include_alts { self.alts.clone() } else { vec![] },
            self.opcodes.clone(),
            self.table.as_slice(),
            self.symbol_table.clone(),
            self.start,
        )
    }
}

impl BuildFrom<ParserGen> for ParserTables {
    /// Creates a [`ParserTables`], from which a parser can be created dynamically with
    /// [`parser_table.make_parser()`](ParserTables::make_parser).
    fn build_from(parser_gen: ParserGen) -> Self {
        ParserTables::new(
            parser_gen.parsing_table,
            parser_gen.symbol_table.to_fixed_sym_table(),
            parser_gen.opcodes,
            parser_gen.start,
            parser_gen.include_alts
        )
    }
}

// not generated automatically since ParserTables isn't LogReader
impl TryBuildFrom<ParserGen> for ParserTables {
    type Error = BuildError;

    fn try_build_from(source: ParserGen) -> Result<Self, Self::Error> {
        if source.get_log().has_no_errors() {
            Ok(ParserTables::build_from(source))
        } else {
            Err(BuildError::new(source.give_log(), BuildErrorSource::ParserGen))
        }
    }
}

// ---------------------------------------------------------------------------------------------

pub static DEFAULT_LISTENER_NAME: &str = "Parser";

static FOLD_SPAN_CODE: [&str; 4] = [
    "        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();",
    "        let mut new_span = PosSpan::empty();",
    "        spans.iter().for_each(|span| new_span += span);",
    "        self.stack_span.push(new_span);",
];

fn count_span_nbr(opcode: &[OpCode]) -> SpanNbr {
    let count = opcode.iter().filter(|op| op.has_span()).count();
    count.try_into().expect(&format!("# span = {count} > {}", SpanNbr::MAX))
}

#[derive(Debug)]
pub struct ParserGen {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
    name: String,
    nt_value: Vec<bool>,
    /// `nt_parent[v]` is the vector of all variables having `v` has top parent (including `v` itself)
    nt_parent: Vec<Vec<VarId>>,
    var_alts: Vec<Vec<AltId>>,
    origin: Origin<VarId, FromPRS>,
    item_ops: HashMap<AltId, Vec<Symbol>>,
    opcodes: Vec<Vec<OpCode>>,
    /// generates code to give the location of nonterminals and tokens as extra parameters of listener methods
    gen_span_params: bool,
    span_nbrs: Vec<SpanNbr>,
    start: VarId,
    nt_conversion: HashMap<VarId, NTConversion>,
    used_libs: StructLibs,
    nt_type: HashMap<VarId, String>,
    nt_extra_info: HashMap<VarId, (String, Vec<String>)>,
    log: BufLog,
    include_alts: bool,
}

impl ParserGen {
    /// Creates a [`ParserGen`] from a set of rules and gives it a specific name, which is used
    /// to name the user listener trait in the generated code.
    pub fn build_from_tree(tree: RuleTreeSet<General>, name: String) -> Self {
        let normalized = RuleTreeSet::<Normalized>::build_from(tree);
        let lr_rules = ProdRuleSet::build_from(normalized);
        Self::build_from_rules(lr_rules, name)
    }

    /// Creates a [`ParserGen`] from a set of production rules and gives it a specific name, which is used
    /// to name the user listener trait in the generated code.
    ///
    /// If [`rules`] already has a name, it is best to use the [BuildFrom<ProdRuleSet<T>>](BuildFrom<ProdRuleSet<T>>::build_from) trait.
    pub fn build_from_rules<T>(rules: ProdRuleSet<T>, name: String) -> Self where ProdRuleSet<LL1>: BuildFrom<ProdRuleSet<T>> {
        let mut ll1_rules = ProdRuleSet::<LL1>::build_from(rules);
        assert_eq!(ll1_rules.get_log().num_errors(), 0);
        let parsing_table = ll1_rules.make_parsing_table(true);
        let num_nt = ll1_rules.get_num_nt();
        let start = ll1_rules.get_start().unwrap();
        let mut var_alts = vec![vec![]; num_nt];
        for (alt_id, (var_id, _)) in parsing_table.alts.iter().index() {
            var_alts[*var_id as usize].push(alt_id);
        }
        let mut nt_parent: Vec<Vec<VarId>> = vec![vec![]; num_nt];
        for var_id in 0..num_nt {
            let top_var_id = parsing_table.get_top_parent(var_id as VarId) as usize;
            nt_parent[top_var_id].push(var_id as VarId);
        }
        let ProdRuleSet { symbol_table, nt_conversion, origin, .. } = ll1_rules;
        let mut builder = ParserGen {
            parsing_table,
            symbol_table: symbol_table.expect(stringify!("symbol table is required to create a {}", std::any::type_name::<Self>())),
            name,
            nt_value: vec![false; num_nt],
            nt_parent,
            var_alts,
            origin,
            item_ops: HashMap::new(),
            opcodes: Vec::new(),
            gen_span_params: false,
            span_nbrs: Vec::new(),
            start,
            nt_conversion,
            used_libs: StructLibs::new(),
            nt_type: HashMap::new(),
            nt_extra_info: HashMap::new(),
            log: ll1_rules.log,
            include_alts: true,
        };
        builder.make_opcodes();
        builder.make_span_nbrs();
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

    /// Generates code to give the location of nonterminals and tokens as extra parameters of listener methods.
    pub fn set_gen_span_params(&mut self, gen_span_params: bool) {
        self.gen_span_params = gen_span_params;
    }

    #[inline]
    pub fn get_nt_parent(&self, v: VarId) -> Option<VarId> {
        self.parsing_table.parent[v as usize]
    }

    /// Include the definitions of the alternatives in the parser, for debugging purposes:
    /// allows to print out the alternatives in VERBOSE mode.
    pub fn set_include_alts(&mut self, include_alts: bool) {
        self.include_alts = include_alts;
    }

    #[cfg(test)] // we keep it here because we'll need it later for doc comments and logs
    fn get_original_alt_str(&self, a_id: AltId, symbol_table: Option<&SymbolTable>) -> Option<String> {
        let (_var, f) = &self.parsing_table.alts[a_id as usize];
        f.get_origin().and_then(|(o_v, o_id)| {
            Some(format!(
                "{} -> {}",
                Symbol::NT(o_v).to_str(symbol_table),
                grtree_to_str(self.origin.get_tree(o_v).unwrap(), Some(o_id), None, Some(o_v), symbol_table, false)
            ))
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

    #[allow(unused)]
    fn nt_has_all_flags(&self, var: VarId, flags: u32) -> bool {
        self.parsing_table.flags[var as usize] & flags == flags
    }

    #[allow(unused)]
    fn nt_has_any_flags(&self, var: VarId, flags: u32) -> bool {
        self.parsing_table.flags[var as usize] & flags != 0
    }

    #[allow(unused)]
    fn sym_has_flags(&self, s: &Symbol, flags: u32) -> bool {
        if let Symbol::NT(nt) = s { self.nt_has_all_flags(*nt, flags) } else { false }
    }

    #[allow(unused)]
    fn sym_has_value(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::T(t) => self.symbol_table.is_token_data(*t),
            Symbol::NT(nt) => self.nt_value[*nt as usize],
            _ => false
        }
    }

    fn full_alt_components(&self, a_id: AltId, emphasis: Option<VarId>) -> (String, String) {
        const VERBOSE: bool = false;
        if VERBOSE { println!("full_alt_components(a_id = {a_id}):"); }
        let &(mut v_a, ref alt) = &self.parsing_table.alts[a_id as usize];
        while self.parsing_table.flags[v_a as usize] & ruleflag::CHILD_L_FACT != 0 {
            v_a = *self.parsing_table.parent[v_a as usize].as_ref().unwrap();
        }
        let symtab = self.get_symbol_table();
        if let Some(v_emph) = emphasis {
            let parent_nt = self.parsing_table.get_top_parent(v_emph);
            if let Some((t_emph, id_emph)) = self.origin.get(v_emph) {
                return ((Symbol::NT(parent_nt).to_str(symtab)), grtree_to_str(t_emph, None, Some(id_emph), Some(parent_nt), symtab, true));
            } else {
                return (Symbol::NT(parent_nt).to_str(symtab), format!("<VAR {v_emph} NOT FOUND>"));
            }
        }
        if let Some((vo, id)) = alt.get_origin() {
            let t = self.origin.get_tree(vo).unwrap();
            let flags = self.parsing_table.flags[v_a as usize];
            if v_a != vo && flags & ruleflag::CHILD_REPEAT != 0 {
                // iteration in parent rule
                (
                    String::new(),
                    format!("`{}` {} in `{} -> {}`",
                            grtree_to_str(t, Some(id), None, Some(vo), symtab, true),
                            if flags & ruleflag::L_FORM != 0 { "iteration" } else { "item" },
                            Symbol::NT(vo).to_str(symtab),
                            grtree_to_str(t, None, Some(id), Some(vo), symtab, true))
                )
            } else {
                let root = Some(id);
                (Symbol::NT(vo).to_str(symtab), grtree_to_str(t, root, None, Some(vo), symtab, true))
            }
        } else {
            (Symbol::NT(v_a).to_str(symtab), format!("<alt {a_id} NOT FOUND>"))
        }
    }

    /// Representation of the original rule behind a context or a user variable
    fn full_alt_str(&self, a_id: AltId, emphasis: Option<VarId>, quote: bool) -> String {
        let (left, right) = self.full_alt_components(a_id, emphasis);
        if left.is_empty() {
            right
        } else {
            format!("{q}{left} -> {right}{q}", q = if quote { "`" } else { "" })
        }
    }

    fn make_opcodes(&mut self) {
        const VERBOSE: bool = false;
        for (alt_id, (var_id, alt)) in self.parsing_table.alts.iter().index() {
            if VERBOSE {
                println!("{alt_id}: {}", alt.to_rule_str(*var_id, self.get_symbol_table(), 0));
            }
            let flags = self.parsing_table.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.parsing_table.alts[alt_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            if VERBOSE { println!("  - {}", new.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut opcode = Vec::<OpCode>::new();
            let mut parent = self.parsing_table.parent[*var_id as usize];
            if flags & ruleflag::CHILD_L_FACT != 0 {
                while self.nt_has_all_flags(parent.unwrap(), ruleflag::CHILD_L_FACT) {
                    parent = self.parsing_table.parent[parent.unwrap() as usize];
                }
                let parent = parent.unwrap();
                // replaces Enter by Loop when going back to left-factorization parent, typically when coupled with + or *
                // (per construction, there can't be any alternative going back to the grandparent or further up in a left factorization, so
                //  we don't check that)
                let parent_r_form_right_rec = self.parsing_table.flags[parent as usize] & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
                if VERBOSE {
                    println!("  - child lfact, parent: {}, !parent_r_form_right_rec = !{parent_r_form_right_rec}, match = {}",
                             Symbol::NT(parent).to_str(self.get_symbol_table()),
                             new.get(0) == Some(&Symbol::NT(parent)));
                }
                if new.get(0) == Some(&Symbol::NT(parent)) && !parent_r_form_right_rec {
                    opcode.push(OpCode::Loop(parent));
                    new.remove(0);
                }
            }
            let parent_lrec_no_lfact = flags & (ruleflag::PARENT_L_RECURSION | ruleflag::PARENT_L_FACTOR) == ruleflag::PARENT_L_RECURSION;
            if flags & ruleflag::PARENT_L_FACTOR == 0 ||
                parent_lrec_no_lfact ||
                new.iter().all(|s| if let Symbol::NT(ch) = s { !self.nt_has_all_flags(*ch, ruleflag::CHILD_L_FACT) } else { true })
            {
                // if it's not a parent of left factorization, or
                // if none of the NT in the alternative is a child of left factorization, or
                // if it's the top parent of left recursion + left factorization,
                // => adds an Exit
                // (said otherwise: we don't want an exit in a parent or in the middle of a chain of left factorizations;
                //  the exit should be only at the end of left factorizations, or in alternatives that aren't left-factorized -
                //  except the special case of left recursion + left factorization, which needs the final exit)
                opcode.push(OpCode::Exit(alt_id)); // will be popped when this NT is completed
            }
            opcode.extend(new.into_iter().map(|s| OpCode::from(s)));
            let r_form_right_rec = flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
            if VERBOSE { println!("  - r_form_right_rec = {r_form_right_rec} = {} || {}",
                                  flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0,
                                  flags & ruleflag::CHILD_L_FACT != 0 && self.parsing_table.flags[parent.unwrap() as usize] & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0); }
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
            if flags & ruleflag::CHILD_L_FACT != 0 {
                if opcode.len() >= 2 {
                    if self.nt_has_all_flags(parent.unwrap(), ruleflag::R_RECURSION | ruleflag::L_FORM) {
                        if opcode[1] == OpCode::NT(parent.unwrap()) {
                            opcode.swap(0, 1);
                            opcode[0] = OpCode::Loop(parent.unwrap());
                        }
                    }
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

    fn make_span_nbrs(&mut self) {
        let mut span_nbrs = vec![0 as SpanNbr; self.parsing_table.alts.len()];
        for (alt_id, (var_id, _)) in self.parsing_table.alts.iter().enumerate() {
            let opcode = &self.opcodes[alt_id];
            let mut span_nbr = span_nbrs[alt_id] + count_span_nbr(&opcode);
            if self.nt_has_any_flags(*var_id, ruleflag::CHILD_REPEAT | ruleflag::CHILD_L_RECURSION) ||
                self.nt_has_all_flags(*var_id, ruleflag::R_RECURSION | ruleflag::L_FORM) {
                // there is a loop span
                span_nbr += 1;
            }
            if matches!(opcode.get(0), Some(OpCode::NT(nt)) if nt != var_id && self.parsing_table.flags[*nt as usize] & ruleflag::CHILD_L_RECURSION != 0) {
                // independent lrec term: the first NT doesn't count
                span_nbr -= 1;
            }
            // println!("### {} -> span = {span_nbr}: {}",
            //          opcode.iter().map(|o| o.to_string()).join(" "), opcode.iter().filter(|o| o.has_span()).map(|o| o.to_string()).join(" "));
            // println!("[{alt_id}]: {} + {} -> {span_nbr}", span_nbrs[alt_id], count_span_nbr(&opcode));
            if self.nt_has_all_flags(*var_id, ruleflag::PARENT_L_FACTOR) {
                if let Some(OpCode::NT(nt)) = opcode.get(0) {
                    span_nbr -= 1;
                    for a_id in self.var_alts[*nt as usize].iter() {
                        span_nbrs[*a_id as usize] += span_nbr;
                        // println!("- [{a_id}] += {span_nbr} -> {}", span_nbrs[*a_id as usize]);
                    }
                    // println!(" -> [{alt_id}] = 0");
                    span_nbr = 0;
                }
            }
            span_nbrs[alt_id] = span_nbr;
        }
        self.span_nbrs = span_nbrs;
    }

    fn get_group_alts(&self, g: &Vec<VarId>) -> Vec<(VarId, AltId)> {
        g.iter().flat_map(|c|
            self.var_alts[*c as usize].iter().map(|a| (*c, *a))
        ).collect::<Vec<_>>()
    }

    /// Gathers all the alternatives in NT, and if some of them are parent_l_fact, searches the
    /// terminal child_l_fact instead. The result is the set of contexts that are used to
    /// call self.listener.exit_<NT>(ctx) for a right-rec, a left-rec parent, a left-rec child, ...
    fn gather_alts(&self, nt: VarId) -> Vec<AltId> {
        const VERBOSE: bool = false;
        let mut alt = vec![];
        let mut explore = VecDeque::<VarId>::new();
        explore.push_back(nt);
        while !explore.is_empty() {
            let var = explore.pop_front().unwrap();
            if VERBOSE { println!("{var}: alt = {} | explore = {} | alts: {}",
                                  alt.iter().join(", "), explore.iter().join(", "),
                                  &self.var_alts[var as usize].iter().join(", ")); }
            for a in &self.var_alts[var as usize] {
                let (_, alter) = &self.parsing_table.alts[*a as usize];
                if let Some(Symbol::NT(last)) = alter.symbols().last() {
                    if self.nt_has_all_flags(*last, ruleflag::CHILD_L_FACT) {
                        // only one alternative calls NT(last), so we won't push it twice in explore:
                        explore.push_back(*last);
                        continue;
                    }
                }
                alt.push(*a);
            }
            if VERBOSE { println!("  => alt = {} | explore = {}", alt.iter().join(", "), explore.iter().join(", ")); }
        }
        alt
    }

    pub(crate) fn make_item_ops(&mut self) {
        const VERBOSE: bool = false;
        let info = &self.parsing_table;
        let mut items = HashMap::<AltId, Vec<Symbol>>::new();
        if VERBOSE {
            println!("Groups:");
            for g in self.nt_parent.iter().filter(|va| !va.is_empty()) {
                let group = self.get_group_alts(g);
                let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                println!("{}: {}, alts {}",
                         Symbol::NT(g[0]).to_str(self.get_symbol_table()),
                    ids.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "),
                    group.iter().map(|(_, a)| a.to_string()).join(", ")
                );
            }
        }
        // we proceed by var parent, then all alternatives in each parent/children group
        for g in self.nt_parent.iter().filter(|va| !va.is_empty()) {
            // takes all the alternatives in the group (and their NT ID):
            let group = self.get_group_alts(g);
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
                for (nt, alt_id) in &group {
                    let ambig_loop = is_ambig && self.nt_has_all_flags(*nt, ruleflag::CHILD_L_RECURSION);
                    items.insert(*alt_id, if ambig_loop { vec![Symbol::NT(g_top)] } else { vec![] });
                }
                for (var_id, alt_id) in &group {
                    let opcode = &self.opcodes[*alt_id as usize];
                    let (_, alt) = &info.alts[*alt_id as usize];
                    if VERBOSE {
                        print!("- {alt_id}: {} -> {}   [{}]",
                               Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                               alt.to_str(self.get_symbol_table()),
                               opcode.iter().map(|op| op.to_str(self.get_symbol_table())).join(" "));
                    }
                    let flags = info.flags[*var_id as usize];

                    // Default values are taken from opcodes. Loop(nt) is only taken if the parent is l-rec;
                    // we look at the parent's flags instead of the alternative's because left factorization could
                    // displace the Loop(nt) to another non-l-rec child alternative.
                    let mut values = self.opcodes[*alt_id as usize].iter().rev()
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
                        // +* non-lform children have the same value as their parent, but +* lform
                        // children's "valueness" is independent from their parent's
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
                        if flags & ruleflag::CHILD_L_FACT != 0 && self.nt_has_all_flags(g[0], ruleflag::L_FORM) {
                            assert!(!self.nt_has_all_flags(*var_id, ruleflag::CHILD_L_FACT | ruleflag::L_FORM), "this was useful after all");
                            if VERBOSE { print!(" child_rrec_lform_lfact"); }
                            items.get_mut(&alt_id).unwrap().insert(0, Symbol::NT(g[0]));
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
                    if VERBOSE {
                        println!(" ==> [{}] + [{}]",
                                 items.get(&alt_id).map(|v| v.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")).unwrap_or(String::new()),
                                 values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "));
                    }
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
                            items.get_mut(&alt_id).unwrap().extend(values);
                            continue;
                        }
                        if flags & ruleflag::PARENT_L_FACTOR != 0 {
                            if VERBOSE {
                                println!("  PARENT_L_FACTOR: moving {} to child {}",
                                         values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                         Symbol::NT(*nt).to_str(self.get_symbol_table()));
                            }
                            // factorization reports all the values to the children
                            if let Some(pre) = items.get_mut(&alt_id) {
                                // pre-pends values that already exist for alt_id (and empties alt_id)
                                values.splice(0..0, std::mem::take(pre));
                            }
                            for a_id in self.var_alts[*nt as usize].iter() {
                                items.get_mut(a_id).unwrap().extend(values.clone());
                            }
                            continue;
                        }
                        if let Some(sym) = backup {
                            values.push(sym);
                        }
                    }
                    items.get_mut(&alt_id).unwrap().extend(values);
                }
            }
        }
        self.item_ops = items;
    }

    fn sort_alt_ids(&self, top_nt: VarId, alts: &[AltId]) -> Vec<AltId> {
        const VERBOSE: bool = false;
        if VERBOSE {
            println!("  sorting {} alts {alts:?}", Symbol::NT(top_nt).to_str(self.get_symbol_table()));
            for &a_id in alts {
                let &(_nt, ref alt) = &self.parsing_table.alts[a_id as usize];
                if let Some((v, id)) = alt.origin {
                    let tree = &self.origin.trees[v as usize];
                    println!("    [{a_id}] id = {},{id} -> {}  <->  {}",
                             Symbol::NT(v).to_str(self.get_symbol_table()),
                             crate::grammar::grtree_to_str_ansi(tree, None, Some(id), Some(v), self.get_symbol_table(), false),
                             tree.to_str_index(None, self.get_symbol_table())
                    );
                    assert_eq!(v, top_nt, "v = {}, top_nt = {}", Symbol::NT(v).to_str(self.get_symbol_table()), Symbol::NT(top_nt).to_str(self.get_symbol_table()));
                }
            }
        }
        let mut sorted = vec![];
        let mut ids = alts.iter().filter_map(|&alt_id| self.parsing_table.alts[alt_id as usize].1.origin.map(|(_var, id)| (id, alt_id)))
            .collect::<HashMap<_, _>>();
        let tree = &self.origin.trees[top_nt as usize];
        for node in tree.iter_depth() {
            if let Some((_, alt_id)) = ids.remove_entry(&node.index) {
                sorted.push(alt_id);
            }
        }
        if VERBOSE { println!("    -> {sorted:?}"); }
        sorted
    }

    /// Calculates nt_name, nt_info, item_info
    ///
    /// * `nt_name[var]: (String, String, String)` contains (upper, lower, lower)-case unique identifiers for each parent NT (the first two
    ///                                            are changed if they're Rust identifiers)
    /// * `alt_info[alt]: Vec<Option<(VarId, String)>>` contains the enum variant names for each context (must be regrouped by VarId)
    /// * `item_info[alt]: Vec<ItemInfo>` contains the data available on the stacks when exiting the alternative
    /// * `child_repeat_endpoints[var]: HashMap<VarId, Vec<AltId>>` list of alts (several when the repeat child has several outcomes,
    ///                                 as in `a -> (A | B)+`), where each alt corresponds to the item_ops with the values on the stack.
    ///
    /// For example:
    ///
    /// ```text
    /// // a -> (b C | A D | A E E)* D | A; b -> B;
    /// //
    /// //  0: a -> a_1 D     | ◄0 D! ►a_1    | a_1 D
    /// //  1: a -> A         | ◄1 A!         | A
    /// //  2: b -> B         | ◄2 B!         | B
    /// //  3: a_1 -> A a_2   | ►a_2 A!       |
    /// //  4: a_1 -> b C a_1 | ●a_1 ◄4 C! ►b | a_1 b C
    /// //  5: a_1 -> ε       | ◄5            | a_1
    /// //  6: a_2 -> D a_1   | ●a_1 ◄6 D!    | a_1 A D
    /// //  7: a_2 -> E E a_1 | ●a_1 ◄7 E! E! | a_1 A E E
    ///
    /// pub enum CtxA {
    ///     A1 { star: SynA1, d: String },  // `a -> (b C | A D | A E E)* D`
    ///     A2 { a: String },               // `a -> A`
    /// }
    /// pub enum CtxB {
    ///     B { b: String },                // `b -> B`
    /// }
    /// pub struct SynA1(pub Vec<SynA1Item>);
    /// pub enum SynA1Item {
    ///     Ch1 { b: SynB, c: String },         // `b C` item in `a -> ( ►► b C ◄◄  | A D | A E E)* D | A`
    ///     Ch2 { a: String, d: String },       // `A D` item in `a -> (b C |  ►► A D ◄◄  | A E E)* D | A`
    ///     Ch3 { a: String, e: [String; 2] },  // `A E E` item in `a -> (b C | A D |  ►► A E E ◄◄ )* D | A`
    /// }
    /// // User-defined: SynA, SynB
    ///
    /// nt_name: [("A", "a", "a"), ("B", "b", "b"), ("A1", "a1", "a1"), ("A2", "a2", "a2")]
    /// alt_info: [Some((0, "A1")), Some((0, "A2")), Some((1, "B")), None, None, None, None, None]
    /// item_info:
    /// 0:  [ItemInfo { name: "star", sym: NT(2), owner: 0, index: None },
    ///      ItemInfo { name: "d", sym: T(2), owner: 0, index: None }],
    /// 1:  [ItemInfo { name: "a", sym: T(1), owner: 0, index: None }],
    /// 2:  [ItemInfo { name: "b", sym: T(4), owner: 1, index: None }],
    /// 3:  [],
    /// 4:  [ItemInfo { name: "b", sym: NT(1), owner: 2, index: None },
    ///      ItemInfo { name: "c", sym: T(0), owner: 2, index: None }],
    /// 5:  [],
    /// 6:  [ItemInfo { name: "a", sym: T(1), owner: 2, index: None },
    ///      ItemInfo { name: "d", sym: T(2), owner: 2, index: None }],
    /// 7:  [ItemInfo { name: "a", sym: T(1), owner: 2, index: None },
    ///      ItemInfo { name: "e", sym: T(3), owner: 2, index: Some(0) },
    ///      ItemInfo { name: "e", sym: T(3), owner: 2, index: Some(1) }]
    /// child_repeat_endpoints: {2: [4, 6, 7]}
    /// ```
    fn get_type_info(&self) -> (
        Vec<(String, String, String)>,
        Vec<Option<(VarId, String)>>,
        Vec<Vec<ItemInfo>>,
        HashMap<VarId, Vec<AltId>>
    ) {
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

        let mut alt_info: Vec<Option<(VarId, String)>> = vec![None; pinfo.alts.len()];
        let mut nt_repeat = HashMap::<VarId, Vec<ItemInfo>>::new();
        let mut item_info: Vec<Vec<ItemInfo>> = vec![vec![]; pinfo.alts.len()];
        let mut child_repeat_endpoints = HashMap::<VarId, Vec<AltId>>::new();
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let is_ambig = self.nt_has_any_flags(group[0], ruleflag::PARENT_AMBIGUITY);
            let mut is_ambig_1st_child = is_ambig;
            let mut alt_info_to_sort = HashMap::<VarId, Vec<AltId>>::new();
            for var in group {
                let nt = *var as usize;
                let nt_flags = pinfo.flags[nt];
                if is_ambig && (nt_flags & ruleflag::PARENT_L_RECURSION != 0 || (nt_flags & ruleflag::CHILD_L_RECURSION != 0 && !is_ambig_1st_child)) {
                    continue;
                }
                if nt_flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {
                    // collects the alt endpoints that correspond to each choice (one or several choices if | is used inside the repeat),
                    // each alt endpoint corresponding to the data in item_info
                    let is_plus = nt_flags & ruleflag::REPEAT_PLUS != 0;
                    let mut endpoints = self.gather_alts(*var);
                    if VERBOSE { println!("** {} endpoints: {endpoints:?} ", Symbol::NT(*var).to_str(self.get_symbol_table())); }
                    if is_plus {
                        // with +, alt endpoints come in couples: the first loops, the other exits, both having the same data
                        // (in each (id1, id2) couple, id2 == id1 + 1)
                        endpoints = endpoints.chunks(2).map(|slice| slice[0]).to_vec();
                    } else {
                        // with *, the endpoint corresponding to the exit has no data
                        //endpoints.retain(|e| self.item_ops[e].len() > 1);
                        endpoints.retain(|e| !pinfo.alts[*e as usize].1.is_sym_empty());
                    }
                    assert!(!endpoints.is_empty());
                    let endpoints = self.sort_alt_ids(group[0], &endpoints);
                    child_repeat_endpoints.insert(*var, endpoints);
                }
                for &alt_id in &self.var_alts[nt] {
                    let i = alt_id as usize;
                    if is_ambig_1st_child && pinfo.alts[i].1.is_sym_empty() {
                        continue;
                    }
                    item_info[i] = if let Some(item_ops) = self.item_ops.get(&alt_id) {
                        // Adds a suffix to the names of different symbols that would otherwise collide in the same context option:
                        // - identical symbols are put in a vector (e.g. `id: [String; 2]`)
                        // - different symbols, which means T vs NT, must have different names (e.g. `NT(A)` becomes "a",
                        //   `T(a)` becomes "a", too => one is renamed to "a1" to avoid the collision: `{ a: SynA, a1: String }`)
                        let mut indices = HashMap::<Symbol, (String, Option<usize>)>::new();
                        let mut fixer = NameFixer::new();
                        let mut owner = pinfo.alts[i].0;
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
                            if let Some((_, c)) = indices.get_mut(s) {
                                *c = Some(0);
                            } else {
                                let name = if let Symbol::NT(vs) = s {
                                    let flag = pinfo.flags[*vs as usize];
                                    if flag & ruleflag::CHILD_REPEAT != 0 {
                                        let inside_alt_id = self.var_alts[*vs as usize][0];
                                        let inside_alt = &pinfo.alts[inside_alt_id as usize].1;
                                        if false {
                                            // we don't use this any more
                                            let mut plus_name = inside_alt.symbols()[0].to_str(self.get_symbol_table()).to_underscore_lowercase();
                                            plus_name.push_str(if flag & ruleflag::REPEAT_PLUS != 0 { "_plus" } else { "_star" });
                                            plus_name
                                        } else {
                                            if is_nt_repeat && indices.is_empty() {
                                                // iterator variable in a + * loop (visible with <L>, for ex)
                                                if flag & ruleflag::REPEAT_PLUS != 0 { "plus_acc".to_string() } else { "star_acc".to_string() }
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

                        // A parent of left factorization has no context, but we must check the alternatives that are the actual parents.
                        // The flag test is optional, but it serves to gate the more complex parental test.
                        let has_lfact_child = nt_flags & ruleflag::PARENT_L_FACTOR != 0 &&
                            pinfo.alts[i].1.symbols().iter().any(|s| matches!(s, &Symbol::NT(c) if pinfo.flags[c as usize] & ruleflag::CHILD_L_FACT != 0));

                        // (α)* doesn't call the listener for each α, unless it's l-form. We say it's a hidden child_repeat, and it doesn't need a context.
                        // The only children a child_repeat can have is due to left factorization in (α)+, so we check `owner` rather than `nt`.
                        let is_hidden_repeat_child = pinfo.flags[owner as usize] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT;

                        // <alt> -> ε
                        let is_alt_sym_empty = self.is_alt_sym_empty(alt_id);

                        // (α <L>)+ have two similar alternatives with the same data on the stack, one that loops and the last iteration. We only
                        // keep one context because we use a flag to tell the listener when it's the last iteration (more convenient).
                        let is_duplicate = i > 0 && self.nt_has_all_flags(owner, ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) &&
                            is_alt_sym_empty;
                        // let is_duplicate = i > 0 && self.nt_has_all_flags(owner, ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) &&
                        //     alt_info[i - 1].as_ref().map(|fi| fi.0) == Some(owner);

                        let is_last_empty_iteration = (nt_flags & ruleflag::CHILD_L_RECURSION != 0
                            || self.nt_has_all_flags(*var, ruleflag::CHILD_REPEAT | ruleflag::L_FORM)) && is_alt_sym_empty;

                        let is_rep_child_no_lform = is_nt_repeat && pinfo.flags[owner as usize] & ruleflag::L_FORM == 0;

                        let has_context = !has_lfact_child && !is_hidden_repeat_child && !is_duplicate && !is_last_empty_iteration;
                        if VERBOSE {
                            println!("NT {nt}, alt {alt_id}: has_lfact_child = {has_lfact_child}, is_hidden_repeat_child = {is_hidden_repeat_child}, \
                                is_duplicate = {is_duplicate}, is_last_empty_iteration = {is_last_empty_iteration} => has_context = {has_context}");
                        }
                        if has_context {
                            alt_info_to_sort.entry(owner)
                                .and_modify(|v| v.push(alt_id))
                                .or_insert_with(|| vec![alt_id]);
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
                            let skip = if is_rep_child_no_lform { 1 } else { 0 };
                            let mut infos = item_ops.into_iter()
                                .skip(skip)
                                .map(|s| {
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
                            if is_nt_repeat && infos.len() > 0 && !nt_repeat.contains_key(&owner) {
                                nt_repeat.insert(owner, infos.clone());
                            }
                            infos
                        }
                    } else {
                        vec![]
                    };
                } // alt_id in var
                if is_ambig && nt_flags & ruleflag::CHILD_L_RECURSION != 0 {
                    is_ambig_1st_child = false;
                }
            } // var in group
            if VERBOSE { println!("alt_info_to_sort = {alt_info_to_sort:?}"); }
            for (owner, alts) in alt_info_to_sort {
                for (num, alt) in self.sort_alt_ids(group[0], &alts).into_iter().index_start(1) {
                    alt_info[alt as usize] = Some((owner, format!("V{num}")));
                }
            }
        } // group

        if VERBOSE {
            println!("NT names: {}", nt_name.iter()
                .map(|(u, l, pl)| format!("{u}/{l}/{pl}"))
                .join(", "));
            println!("alt info:");
            for (alt_id, alt_names) in alt_info.iter().enumerate() {
                if let Some((v, name)) = alt_names {
                    println!("- alt {alt_id}, NT {v} {}, Ctx name: {name}", Symbol::NT(*v).to_str(self.get_symbol_table()));
                }
            }
            println!();
            println!("nt_name: {nt_name:?}");
            println!("alt_info: {alt_info:?}");
            println!("item_info:");
            for (i, item) in item_info.iter().enumerate().filter(|(_, item)| !item.is_empty()) {
                println!("- {i}: {{ {} }}", item.iter()
                    .map(|ii| format!("{}{} ({})", ii.name, ii.index.map(|i| format!("[{i}]")).unwrap_or(String::new()), ii.sym.to_str(self.get_symbol_table())))
                    .join(", "));
            }
            println!("item_info: {item_info:?}");
            println!("child_repeat_endpoints: {child_repeat_endpoints:?}");
        }
        (nt_name, alt_info, item_info, child_repeat_endpoints)
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
        let source = self.gen_source_code(indent, true);
        out.write(source.as_bytes())?;
        // write!(out, "{source}");
        Ok(())
    }

    pub fn gen_source_code(&mut self, indent: usize, wrapper: bool) -> String {
        let mut parts = vec![];
        let mut tmp_parts = vec![self.source_build_parser()];
        if wrapper {
            self.make_item_ops();
            tmp_parts.push(self.source_wrapper());
        }
        parts.push(self.source_use());
        parts.extend(tmp_parts);
        // Create source code:
        indent_source(parts, indent)
    }

    pub fn try_gen_source_code(mut self, indent: usize, wrapper: bool) -> Result<(BufLog, String), BuildError> {
        let src = self.gen_source_code(indent, wrapper);
        if self.log.has_no_errors() {
            Ok((self.give_log(), src))
        } else {
            Err(BuildError::new(self.give_log(), BuildErrorSource::ParserGen))
        }
    }

    fn source_use(&self) -> Vec<String> {
        self.used_libs.gen_source_code()
    }

    fn source_build_parser(&mut self) -> Vec<String> {
        self.log.add_note("generating build_parser() source...");
        let num_nt = self.symbol_table.get_num_nt();
        let num_t = self.symbol_table.get_num_t();
        for lib in [
            "lexigram_lib::grammar::Alternative",
            "lexigram_lib::grammar::Symbol",
            "lexigram_lib::grammar::VarId",
            "lexigram_lib::grammar::AltId",
            "lexigram_lib::parser::OpCode",
            "lexigram_lib::parser::Parser",
            "lexigram_lib::FixedSymTable",
        ] {
            self.used_libs.add(lib);
        }

        self.log.add_note(format!("- creating symbol tables: {num_t} terminals, {num_nt} nonterminals"));
        let mut src = vec![
            format!("const PARSER_NUM_T: usize = {num_t};"),
            format!("const PARSER_NUM_NT: usize = {num_nt};"),
            format!("static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [{}];",
                     self.symbol_table.get_terminals().map(|(s, os)|
                         format!("(\"{s}\", {})", os.as_ref().map(|s| format!("Some(\"{s}\")")).unwrap_or("None".to_string()))).join(", ")),
            format!("static SYMBOLS_NT: [&str; PARSER_NUM_NT] = [{}];",
                     self.symbol_table.get_nonterminals().map(|s| format!("\"{s}\"")).join(", ")),
            format!("static ALT_VAR: [VarId; {}] = [{}];",
                    self.parsing_table.alts.len(),
                    self.parsing_table.alts.iter().map(|(v, _)| format!("{v}")).join(", ")),
        ];
        if self.include_alts {
            src.push(format!("static ALTERNATIVES: [&[Symbol]; {}] = [{}];",
                             self.parsing_table.alts.len(),
                             self.parsing_table.alts.iter().map(|(_, f)| format!("&[{}]", f.iter().map(|s| symbol_to_code(s)).join(", "))).join(", ")));
        }
        self.log.add_note(format!("- creating parsing tables: {} items, {} opcodes", self.parsing_table.table.len(), self.opcodes.len()));
        src.extend(vec![
            format!("static PARSING_TABLE: [AltId; {}] = [{}];",
                     self.parsing_table.table.len(),
                     self.parsing_table.table.iter().map(|v| format!("{v}")).join(", ")),
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
            format!("        &ALT_VAR,"),
            if self.include_alts {
                format!("        ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),")
            } else {
                format!("        Vec::new(),")
            },
            format!("        OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),"),
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

    fn is_alt_sym_empty(&self, a_id: AltId) -> bool {
        self.parsing_table.alts[a_id as usize].1.is_sym_empty()
    }

    /// Generates the match cases for the "Call::Exit" in the `switch` method.
    fn make_match_choices(&self, alts: &[AltId], name: &str, flags: u32, no_method: bool, force_id: Option<AltId>) -> (bool, Vec<String>) {
        assert!(!alts.is_empty(), "alts cannot be empty");
        // If + <L> child, the two alts are identical. We keep the two alts anyway because it's more coherent
        // for the rest of the flow. At the end, when we generate the wrapper method, we'll discard the 2nd alternative and use
        // the `alt_id` parameter to determine whether it's the last iteration or not.
        // We do discard the 2nd, empty alternative immediately for a non-<L> * child because there's no associated context.
        let discarded = if !no_method && flags & (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT { 1 } else { 0 };

        // + children always have 2*n left-factorized children, each couple with identical item_ops (one for the loop, one for the last iteration).
        // So in non-<L> +, we need more than 2 alts to need the alt_id parameter. In other cases, we need more than one
        // alt (after removing the possible discarded one) to require the alt_id parameter.
        let is_plus_no_lform = flags & (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) == (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS);
        let is_alt_id_threshold = if is_plus_no_lform { 2 } else { 1 };
        let is_alt_id = force_id.is_none() && alts.len() - discarded > is_alt_id_threshold;

        let mut choices = Vec::<String>::new();
        let force_id_str = force_id.map(|f| f.to_string()).unwrap_or_default();
        if alts.len() - discarded == 1 {
            if no_method {
                choices.push(format!("                    {} => {{}}", alts[0]));
            } else {
                choices.push(format!("                    {} => self.{name}({force_id_str}),", alts[0]));
            }
        } else {
            let last = alts.len() - 1 - discarded;
            choices.extend((0..last).map(|i| format!("                    {} |", alts[i])));
            if no_method {
                choices.push(format!("                    {} => {{}}", alts[last]));
            } else {
                choices.push(format!("                    {} => self.{name}({}{force_id_str}),",
                                     alts[last],
                                     if is_alt_id { "alt_id" } else { "" }));
            }
        }
        if discarded == 1 {
            choices.push(format!("                    {} => {{}}", alts.last().unwrap()));
        }
        (is_alt_id, choices)
    }

    /// Generates a string with either `"{common}"` or `"({span_code}, {common})"`, where `span_code` is
    /// created by the closure. We use a closure because it's executed only if necessary, which
    /// avoids accessing data that might not be available when the span code is not generated.
    fn gen_match_item<F: FnOnce() -> String>(&self, common: String, span_only: F) -> String {
        if self.gen_span_params {
            let span_code = span_only();
            format!("({span_code}, {common})")
        } else {
            common
        }
    }

    /// Generates the wrapper source code.
    fn source_wrapper(&mut self) -> Vec<String> {
        const VERBOSE: bool = false;
        const MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS: bool = false;

        // DO NOT RETURN FROM THIS METHOD EXCEPT AT THE END

        let mut log = std::mem::take(&mut self.log); // work-around for borrow checker (`let nt_type = self.get_nt_type(v)`: immutable borrow, etc)
        log.add_note("generating wrapper source...");
        self.used_libs.extend([
            "lexigram_lib::CollectJoin", "lexigram_lib::grammar::VarId", "lexigram_lib::parser::Call", "lexigram_lib::parser::ListenerWrapper",
            "lexigram_lib::grammar::AltId", "lexigram_lib::log::Logger",
        ]);
        if self.gen_span_params {
            self.used_libs.add("lexigram_lib::lexer::PosSpan");
        }

        let (nt_name, alt_info, item_info, child_repeat_endpoints) = self.get_type_info();
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
        log.add_note(format!("- Contexts used in {}Listener trait:", self.name));
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let mut group_names = HashMap::<VarId, Vec<AltId>>::new();
            // fetches the NT that have alt data
            for nt in group {
                for &alt_id in &self.var_alts[*nt as usize] {
                    if let Some((owner, _name)) = &alt_info[alt_id as usize] {
                        group_names.entry(*owner)
                            .and_modify(|v| v.push(alt_id))
                            .or_insert_with(|| vec![alt_id]);
                    }
                }
            }
            if VERBOSE {
                println!("group {}", group.iter().map(|nt| Symbol::NT(*nt).to_str(self.get_symbol_table())).join(" "));
            }
            for &nt in group {
                if let Some(alts) = group_names.get(&nt) {
                    if VERBOSE {
                        if let Some(gn) = group_names.get(&nt) {
                            println!("- {}: alts = {}", Symbol::NT(nt).to_str(self.get_symbol_table()), gn.iter().map(|a| a.to_string()).join(", "));
                            let sorted = self.sort_alt_ids(group[0], gn);
                            println!("     sorted alts: {sorted:?}");
                        }
                    }
                    log.add_note(format!("  - Ctx{}:", nt_name[nt as usize].0));
                    src.push(format!("#[derive(Debug)]"));
                    src.push(format!("pub enum Ctx{} {{", nt_name[nt as usize].0));
                    if VERBOSE { println!("  context Ctx{}:", nt_name[nt as usize].0); }
                    for a_id in self.sort_alt_ids(group[0], alts) {
                        let comment = self.full_alt_str(a_id, None, true);
                        log.add_note(format!("    /// {comment}"));
                        src.push(format!("    /// {comment}"));
                        if VERBOSE { println!("      /// {comment}"); }
                        let ctx_content = self.source_infos(&item_info[a_id as usize], false);
                        let a_name = &alt_info[a_id as usize].as_ref().unwrap().1;
                        let ctx_item = if ctx_content.is_empty() {
                            if VERBOSE { println!("      {a_name},"); }
                            format!("    {a_name},", )
                        } else {
                            if VERBOSE { println!("      {a_name} {{ {ctx_content} }},"); }
                            format!("    {a_name} {{ {ctx_content} }},", )
                        };
                        log.add_note(ctx_item.clone());
                        src.push(ctx_item);
                    }
                    src.push(format!("}}"));
                }
            }
        }

        // Writes intermediate Syn types
        src.add_space();
        log.add_note("- NT types and user-defined type templates:");
        src.push("// NT types and user-defined type templates (copy elsewhere and uncomment when necessary):".to_string());
        src.add_space();
        let mut syns = Vec::<VarId>::new(); // list of valuable NTs
        for (v, names) in nt_name.iter().enumerate().filter(|(v, _)| self.nt_value[*v]) {
            let v = v as VarId;
            let (nu, _nl, _npl) = names;
            let nt_type = self.get_nt_type(v);
            if self.nt_has_all_flags(v, ruleflag::CHILD_REPEAT) {
                let is_lform = self.nt_has_all_flags(v, ruleflag::L_FORM);
                let first_alt = self.var_alts[v as usize][0];
                let (t, var_oid) = self.origin.get(v).unwrap();
                if is_lform {
                    let astr = format!("/// User-defined type for {}", self.full_alt_str(first_alt, None, true));
                    let user_def_type = vec![
                        format!("// {astr}"),
                        format!("// #[derive(Debug, PartialEq)] pub struct {}();", self.get_nt_type(v)),
                    ];
                    log.extend_messages(user_def_type.iter().map(|s| LogMsg::Note(s[3..].to_string())));
                    src.extend(user_def_type);
                    let extra_src = vec![
                        astr,
                        format!("#[derive(Debug, PartialEq)]"),
                        format!("pub struct {nt_type}();"),
                    ];
                    self.nt_extra_info.insert(v, (self.get_nt_type(v).to_string(), extra_src));
                } else {
                    let top_parent = self.parsing_table.get_top_parent(v);
                    src.push(format!("/// Computed `{}` array in `{} -> {}`",
                                     grtree_to_str(t, Some(var_oid), None, Some(top_parent), self.get_symbol_table(), true),
                                     Symbol::NT(top_parent).to_str(self.get_symbol_table()),
                                     grtree_to_str(t, None, Some(var_oid), Some(top_parent), self.get_symbol_table(), true),
                    ));
                    let endpoints = child_repeat_endpoints.get(&v).unwrap();
                    if endpoints.len() > 1 {
                        // several possibilities; for ex. a -> (A | B)+  => Vec of enum type
                        src.push(format!("#[derive(Debug, PartialEq)]"));
                        src.push(format!("pub struct {nt_type}(pub Vec<Syn{nu}Item>);"));
                        src.push(format!("#[derive(Debug, PartialEq)]"));
                        src.push(format!("pub enum Syn{nu}Item {{"));
                        for (i, &a_id) in endpoints.into_iter().index_start(1) {
                            src.push(format!("    /// {}", self.full_alt_str(a_id, None, true)));
                            src.push(format!("    V{i} {{ {} }},", self.source_infos(&item_info[a_id as usize], false)));
                        }
                        src.push(format!("}}"));
                    } else {
                        // single possibility; for ex. a -> (A B)+  => struct
                        let a_id = endpoints[0];
                        let infos = &item_info[a_id as usize];
                        if infos.len() == 1 {
                            // single repeat item; for ex. A -> B+  => type directly as Vec<type>
                            let type_name = self.get_info_type(&infos, &infos[0]);
                            src.push(format!("#[derive(Debug, PartialEq)]"));
                            src.push(format!("pub struct {nt_type}(pub Vec<{type_name}>);", ));
                        } else {
                            // several repeat items; for ex. A -> (B b)+  => intermediate struct type for Vec
                            src.push(format!("#[derive(Debug, PartialEq)]"));
                            src.push(format!("pub struct {nt_type}(pub Vec<Syn{nu}Item>);"));
                            src.push(format!("/// {}", self.full_alt_str(first_alt, None, false)));
                            src.push(format!("#[derive(Debug, PartialEq)]"));
                            src.push(format!("pub struct Syn{nu}Item {{ {} }}", self.source_infos(&infos, true)));
                        }
                    }
                }
            } else {
                let user_def_type = vec![
                    format!("// /// User-defined type for `{}`", Symbol::NT(v).to_str(self.get_symbol_table())),
                    format!("// #[derive(Debug, PartialEq)] pub struct {}();", self.get_nt_type(v)),
                ];
                log.extend_messages(user_def_type.iter().map(|s| LogMsg::Note(s[3..].to_string())));
                src.extend(user_def_type);
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
        let mut span_init = HashSet::<VarId>::new();

        // we proceed by var parent, then all alts in each parent/children group
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let parent_nt = group[0] as usize;
            let parent_flags = self.parsing_table.flags[parent_nt];
            let parent_has_value = self.nt_value[parent_nt];
            let mut exit_alt_done = HashSet::<VarId>::new();
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
            let mut ambig_op_alts = BTreeMap::<AltId, Vec<AltId>>::new();
            for (id, f) in ambig_children.iter()        // id = operator priority/ID in ambig rule
                .flat_map(|v| self.gather_alts(*v))
                .filter_map(|f| self.parsing_table.alts[f as usize].1.get_ambig_alt_id().map(|id| (id, f)))
            {
                ambig_op_alts.entry(id).or_default().push(f);
            }
            if VERBOSE && is_ambig {
                println!("- ambig children vars: {}", ambig_children.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "));
                println!("  ambig op alts: {ambig_op_alts:?}");
            }
            for var in group {
                let sym_nt = Symbol::NT(*var);
                let nt = *var as usize;
                let flags = self.parsing_table.flags[nt];
                // the parents of left recursion are not useful in ambiguous rules (they just push / pop the same value):
                let is_ambig_1st_child =  is_ambig && flags & ruleflag::CHILD_L_RECURSION != 0 && ambig_children.get(0) == Some(var);
                // we only process the first variable of the left recursion; below we gather the alts of
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
                        span_init.insert(*var);
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
                        span_init.insert(*var);
                        if has_value {
                            init_nt_done.insert(*var);
                            src_wrapper_impl.push(String::new());
                            let (nu, _nl, npl) = &nt_name[nt];
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
                                let (_nu, _nl, npl) = &nt_name[nt];
                                src_init.push(vec![format!("                    {nt} => self.listener.init_{npl}(),"), nt_comment]);
                                src_listener_decl.push(format!("    fn init_{npl}(&mut self) {{}}"));
                            } else {
                                // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
                            }
                        }
                    } else if flags & (ruleflag::CHILD_L_RECURSION | ruleflag::CHILD_L_FACT) != 0 {
                        // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
                    } else {
                        // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
                    }
                }

                // Call::Exit

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

                fn get_var_params(item_info: &[ItemInfo], skip: usize, indices: &HashMap<Symbol, Vec<String>>, non_indices: &mut Vec<String>) -> String {
                    item_info.iter().skip(skip).filter_map(|item| {
                        get_var_param(item, indices, non_indices)
                    }).join(", ")
                }

                // handles most rules except children of left factorization (already taken by self.gather_alts)
                if !is_ambig_redundant && flags & ruleflag::CHILD_L_FACT == 0 {
                    let (nu, _nl, npl) = &nt_name[nt];
                    let (pnu, _pnl, pnpl) = &nt_name[parent_nt];
                    if VERBOSE { println!("    {nu} (parent {pnu})"); }
                    let no_method = !has_value && flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT;
                    let (fnpl, fnu, fnt, f_valued) = if is_ambig_1st_child {
                        (pnpl, pnu, parent_nt, parent_has_value)    // parent_nt doesn't come through this code, so we must do it now
                    } else {
                        (npl, nu, nt, has_value)
                    };
                    if is_parent || (is_child_repeat_lform && !no_method) || is_ambig_1st_child {
                        let extra_param = if self.gen_span_params { ", spans: Vec<PosSpan>" } else { "" };
                        if f_valued {
                            src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, ctx: Ctx{fnu}{extra_param}) -> {};", self.get_nt_type(fnt as VarId)));
                        } else {
                            src_listener_decl.push(format!("    #[allow(unused)]"));
                            src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, ctx: Ctx{fnu}{extra_param}) {{}}"));
                        }
                    }
                    let all_exit_alts = if is_ambig_1st_child {
                        ambig_op_alts.values().rev().map(|v| v[0]).to_vec()
                    } else {
                        self.gather_alts(nt as VarId)
                    };
                    let (last_it_alts, exit_alts) = all_exit_alts.into_iter()
                        .partition::<Vec<_>, _>(|f| /*alt_info[*f as usize].is_none() &&*/
                            (flags & ruleflag::CHILD_L_RECURSION != 0
                                || flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM | ruleflag::REPEAT_PLUS) == ruleflag::CHILD_REPEAT | ruleflag::L_FORM)
                            && self.is_alt_sym_empty(*f));
                    if VERBOSE {
                        println!("    no_method: {no_method}, exit alts: {}", exit_alts.iter().join(", "));
                        if !last_it_alts.is_empty() {
                            println!("    last_it_alts: {}", last_it_alts.iter().join(", "));
                        }
                    }
                    for f in &exit_alts {
                        exit_alt_done.insert(*f);
                    }
                    let inter_or_exit_name = if flags & ruleflag::PARENT_L_RECURSION != 0 { format!("inter_{npl}") } else { format!("exit_{npl}") };
                    let fn_name = exit_fixer.get_unique_name(inter_or_exit_name.clone());
                    let (is_alt_id, choices) = self.make_match_choices(&exit_alts, &fn_name, flags, no_method, None);
                    if VERBOSE { println!("    choices: {}", choices.iter().map(|s| s.trim()).join(" ")); }
                    let comments = exit_alts.iter().map(|f| {
                        let (v, pf) = &self.parsing_table.alts[*f as usize];
                        if MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS {
                            format!("// {}", self.full_alt_str(*f, None, false))
                        } else {
                            format!("// {}", pf.to_rule_str(*v, self.get_symbol_table(), self.parsing_table.flags[*v as usize]))
                        }
                    }).to_vec();
                    src_exit.extend(choices.into_iter().zip(comments).map(|(a, b)| vec![a, b]));
                    if is_ambig_1st_child {
                        for (a_id, dup_alts) in ambig_op_alts.values().rev().filter_map(|v| if v.len() > 1 { v.split_first() } else { None }) {
                            // note: is_alt_id must be true because we wouldn't get duplicate alternatives otherwise in an ambiguous rule
                            //       (it's duplicated to manage the priority between several alternatives, which are all in the first NT)
                            let (_, choices) = self.make_match_choices(dup_alts, &fn_name, 0, no_method, Some(*a_id));
                            let comments = dup_alts.iter()
                                .map(|a| {
                                    let (v, alt) = &pinfo.alts[*a as usize];
                                    format!("// {} (duplicate of {a_id})", alt.to_rule_str(*v, self.get_symbol_table(), 0))
                                }).to_vec();
                            src_exit.extend(choices.into_iter().zip(comments).map(|(a, b)| vec![a, b]));
                            for a in dup_alts {
                                exit_alt_done.insert(*a);
                            }
                        }
                    }
                    if !no_method {
                        src_wrapper_impl.push(String::new());
                        src_wrapper_impl.push(format!("    fn {fn_name}(&mut self{}) {{", if is_alt_id { ", alt_id: AltId" } else { "" }));
                    }
                    if flags & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {

                        fn source_lets(infos: &[ItemInfo], nt_name: &[(String, String, String)], indent: &str) -> (Vec<String>, String) {
                            let mut src_let = vec![];
                            let mut var_fixer = NameFixer::new();
                            let mut indices = HashMap::<Symbol, Vec<String>>::new();
                            let mut non_indices = Vec::<String>::new();
                            for item in infos.iter().rev() {
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
                                    src_let.push(format!("{indent}        let {varname} = self.stack.pop().unwrap().get_{}();", nt_name[v as usize].2));
                                } else {
                                    src_let.push(format!("{indent}        let {varname} = self.stack_t.pop().unwrap();"));
                                }
                            }

                            let is_simple = infos.len() == 1 && infos[0].sym.is_t(); // Vec<String>
                            let src_struct = if is_simple {
                                non_indices[0].clone()
                            } else {
                                if infos.len() == 1 {
                                    get_var_param(&infos[0], &indices, &mut non_indices).unwrap()
                                } else {
                                    get_var_params(&infos, 0, &indices, &mut non_indices)
                                }
                            };
                            (src_let, src_struct)
                        }

                        if has_value {
                            let endpoints = child_repeat_endpoints.get(var).unwrap();
                            let is_plus = flags & ruleflag::REPEAT_PLUS != 0;
                            let vec_name = if is_plus { "plus_acc" } else { "star_acc" };
                            let val_name = if endpoints.len() > 1 {
                                // several possibilities; for ex. a -> (A | B)+  => Vec of enum type
                                src_wrapper_impl.push(format!("        let {} = match alt_id {{", self.gen_match_item("val".to_string(), || "n".to_string())));
                                for (i, &a_id) in endpoints.into_iter().index_start(1) {
                                    let infos = &item_info[a_id as usize];
                                    src_wrapper_impl.push(format!("            {a_id}{} => {{", if is_plus { format!(" | {}", a_id + 1) } else { String::new() }));
                                    let (src_let, src_struct) = source_lets(infos, &nt_name, "        ");
                                    src_wrapper_impl.extend(src_let);
                                    let return_value = self.gen_match_item(
                                        format!("Syn{nu}Item::V{i} {{ {} }}", src_struct),
                                        || self.span_nbrs[a_id as usize].to_string());
                                    src_wrapper_impl.push(format!("                {return_value}"));
                                    src_wrapper_impl.push(format!("            }}"));
                                }
                                src_wrapper_impl.push(format!("            _ => panic!(\"unexpected alt id {{alt_id}} in fn {fn_name}\"),"));
                                src_wrapper_impl.push(format!("        }};"));
                                "val".to_string()
                            } else {
                                // single possibility; for ex. a -> (A B)+  => struct
                                let a_id = endpoints[0];
                                let infos = &item_info[a_id as usize];
                                let (src_let, src_struct) = source_lets(infos, &nt_name, "");
                                src_wrapper_impl.extend(src_let);
                                if self.gen_span_params {
                                    src_wrapper_impl.push(format!("        let n = {};", self.span_nbrs[a_id as usize]));
                                }
                                if infos.len() == 1 {
                                    // single repeat item; for ex. A -> B+  => type directly as Vec<type>
                                    let val_name = infos[0].name.clone();
                                    val_name
                                } else {
                                    // several repeat items; for ex. A -> (B b)+  => intermediate struct type for Vec
                                    src_wrapper_impl.push(format!("        let val = Syn{nu}Item {{ {} }};", src_struct));
                                    "val".to_string()
                                }
                            };
                            src_wrapper_impl.push(format!("        let Some(SynValue::{nu}(Syn{nu}({vec_name}))) = self.stack.last_mut() else {{"));
                            src_wrapper_impl.push(format!("            panic!(\"unexpected Syn{nu} item on wrapper stack\");"));
                            src_wrapper_impl.push(format!("        }};"));
                            src_wrapper_impl.push(format!("        {vec_name}.push({val_name});"));
                            if self.gen_span_params {
                                // "        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();"
                                // "        let mut new_span = PosSpan::empty();"
                                // "        spans.iter().for_each(|span| new_span += span);"
                                // "        self.stack_span.push(new_span);"
                                src_wrapper_impl.extend(FOLD_SPAN_CODE.into_iter().map(|s| s.to_string()).to_vec());
                            }
                        }
                    } else {
                        assert!(!no_method, "no_method is not expected here (only used in +* with no lform)");
                        let has_last_flag = is_child_repeat_lform && flags & ruleflag::REPEAT_PLUS != 0; // special last flag
                        let (mut last_alt_ids, exit_alts): (Vec<AltId>, Vec<AltId>) = exit_alts.into_iter().partition(|i| alt_info[*i as usize].is_none());
                        let fnu = if is_child_repeat_lform { nu } else { pnu }; // +* <L> use the loop variable, the other alternatives use the parent
                        let fnpl = if is_child_repeat_lform { npl } else { pnpl }; // +* <L> use the loop variable, the other alternatives use the parent
                        let a_has_value = if is_child_repeat_lform { has_value } else { parent_has_value };
                        let is_single = exit_alts.len() == 1;
                        let indent = if is_single { "        " } else { "                " };
                        if !is_single {
                            if self.gen_span_params {
                                src_wrapper_impl.push(format!("        let (n, ctx) = match alt_id {{"));
                            } else {
                                src_wrapper_impl.push(format!("        let ctx = match alt_id {{"));
                            }
                        }
                        if VERBOSE { println!("    exit_alts -> {exit_alts:?}, last_alt_id -> {last_alt_ids:?}"); }
                        for a in exit_alts {
                            if VERBOSE {
                                println!("    - ALTERNATIVE {a}: {} -> {}",
                                         Symbol::NT(*var).to_str(self.get_symbol_table()),
                                         self.parsing_table.alts[a as usize].1.to_str(self.get_symbol_table()));
                            }
                            let mut var_fixer = NameFixer::new();
                            let mut indices = HashMap::<Symbol, Vec<String>>::new();
                            let mut non_indices = Vec::<String>::new();
                            let last_alt_id_maybe = if last_alt_ids.is_empty() { None } else { Some(last_alt_ids.remove(0)) };
                            if !is_single {
                                let last_alt_choice = if let Some(last_alt_id) = last_alt_id_maybe { format!(" | {last_alt_id}") } else { String::new() };
                                src_wrapper_impl.push(format!("            {a}{last_alt_choice} => {{", ));
                            }
                            for item in item_info[a as usize].iter().rev() {
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
                                    src_wrapper_impl.push(format!("{indent}let {varname} = alt_id == {};", last_alt_id_maybe.unwrap()));
                                } else if let Symbol::NT(v) = item.sym {
                                    src_wrapper_impl.push(format!("{indent}let {varname} = self.stack.pop().unwrap().get_{}();",
                                                                  nt_name[v as usize].2));
                                } else {
                                    src_wrapper_impl.push(format!("{indent}let {varname} = self.stack_t.pop().unwrap();"));
                                }
                            }
                            let ctx_params = get_var_params(&item_info[a as usize], 0, &indices, &mut non_indices);
                            let ctx = if ctx_params.is_empty() {
                                format!("Ctx{fnu}::{}", alt_info[a as usize].as_ref().unwrap().1)
                            } else {
                                format!("Ctx{fnu}::{} {{ {ctx_params} }}", alt_info[a as usize].as_ref().unwrap().1)
                            };
                            if is_single {
                                src_wrapper_impl.push(format!("        let ctx = {ctx};"));
                                if self.gen_span_params {
                                    src_wrapper_impl.push(format!("        let n = {};", self.span_nbrs[a as usize]));
                                    src_wrapper_impl.extend(FOLD_SPAN_CODE.into_iter().map(|s| s.to_string()).to_vec());

                                }
                                src_wrapper_impl.push(format!(
                                    "        {}self.listener.exit_{fnpl}(ctx{});",
                                    if a_has_value { "let val = " } else { "" },
                                    if self.gen_span_params { ", spans" } else { "" }));
                                if a_has_value {
                                    src_wrapper_impl.push(format!("        self.stack.push(SynValue::{fnu}(val));"));
                                }
                            } else {
                                let ctx_value = self.gen_match_item(ctx, || self.span_nbrs[a as usize].to_string());
                                src_wrapper_impl.push(format!("{indent}{ctx_value}"));
                                src_wrapper_impl.push(format!("            }}"));
                            }
                        }
                        if !is_single {
                            src_wrapper_impl.push(format!("            _ => panic!(\"unexpected alt id {{alt_id}} in fn {fn_name}\")"));
                            src_wrapper_impl.push(format!("        }};"));
                            if self.gen_span_params {
                                src_wrapper_impl.extend(FOLD_SPAN_CODE.into_iter().map(|s| s.to_string()).to_vec());
                            }
                            src_wrapper_impl.push(format!(
                                "        {}self.listener.exit_{fnpl}(ctx{});",
                                if a_has_value { "let val = " } else { "" },
                                if self.gen_span_params { ", spans" } else { "" }));
                            if a_has_value {
                                src_wrapper_impl.push(format!("        self.stack.push(SynValue::{fnu}(val));"));
                            }
                        }
                    }
                    if !no_method {
                        src_wrapper_impl.push(format!("    }}"));
                    }
                    for a in last_it_alts {
                        if let Some(info) = item_info[a as usize].get(0) {
                            if VERBOSE { println!("last_it_alts: {a}, info = {info:?}"); }
                            let (variant, _, fnname) = &nt_name[info.owner as usize];
                            let typ = self.get_nt_type(info.owner);
                            let varname = &info.name;
                            src_listener_decl.push(format!("    #[allow(unused)]"));
                            src_listener_decl.push(format!("    fn exitloop_{fnname}(&mut self, {varname}: &mut {typ}) {{}}"));
                            let (v, pf) = &self.parsing_table.alts[a as usize];
                            let alt_str = if MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS {
                                self.full_alt_str(a, None, false)
                            } else {
                                pf.to_rule_str(*v, self.get_symbol_table(), self.parsing_table.flags[*v as usize])
                            };
                            src_exit.push(vec![format!("                    {a} => self.exitloop_{fnpl}(),"), format!("// {alt_str}")]);
                            exit_alt_done.insert(a);
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
            for a in group.iter().flat_map(|v| &self.var_alts[*v as usize]).filter(|a| !exit_alt_done.contains(a)) {
                let is_called = self.opcodes[*a as usize].iter().any(|o| *o == OpCode::Exit(*a));
                let (v, alt) = &self.parsing_table.alts[*a as usize];
                let alt_str = if MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS {
                    self.full_alt_str(*a, None, false)
                } else {
                    alt.to_rule_str(*v, self.get_symbol_table(), self.parsing_table.flags[*v as usize])
                };
                let comment = format!("// {alt_str} ({})", if is_called { "not used" } else { "never called" });
                if is_called {
                    src_exit.push(vec![format!("                    {a} => {{}}"), comment]);
                } else {
                    src_exit.push(vec![format!("                 /* {a} */"), comment]);
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
        let extra_param = if self.gen_span_params { ", span: PosSpan" } else { "" };
        if self.nt_value[self.start as usize] || self.gen_span_params {
            src.push(format!("    #[allow(unused)]"));
        }
        if self.nt_value[self.start as usize] {
            src.push(format!("    fn exit(&mut self, {}: {}{extra_param}) {{}}", nt_name[self.start as usize].2, self.get_nt_type(self.start)));
        } else {
            src.push(format!("    fn exit(&mut self{extra_param}) {{}}"));
        }
        /*
                              fn init_a(&mut self) {}
                              fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
                              fn init_a_iter(&mut self) -> SynAIter;
                              #[allow(unused)]
                              fn exit_a_iter(&mut self, ctx: CtxAIter) -> SynAIter {};
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
        if self.gen_span_params {
            src.push(format!("    stack_span: Vec<PosSpan>,"));
        }
        src.push(format!("}}"));
        src.push(format!(""));
        src.push(format!("impl<T: {}Listener> ListenerWrapper for Wrapper<T> {{", self.name));
        src.push(format!("    fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {{"));
        src.push(format!("        if self.verbose {{"));
        src.push(format!("            println!(\"switch: call={{call:?}}, nt={{nt}}, alt={{alt_id}}, t_data={{t_data:?}}\");"));
        src.push(format!("        }}"));
        src.push(format!("        if let Some(mut t_data) = t_data {{"));
        src.push(format!("            self.stack_t.append(&mut t_data);"));
        src.push(format!("        }}"));
        src.push(format!("        match call {{"));
        src.push(format!("            Call::Enter => {{"));
        if self.gen_span_params {
            // adds span accumulator inits, using Segments to regroup them
            let mut seg_span = Segments::from_iter(span_init.into_iter().map(|v| Seg(v as u32, v as u32)));
            seg_span.normalize();
            let pattern = seg_span.into_iter().map(|Seg(a, b)| {
                if a == b {
                    a.to_string()
                } else if b == a + 1 {
                    format!("{a} | {b}")
                } else {
                    format!("{a} ..= {b}")
                }
            }).join(" | ");
            if !pattern.is_empty() {
                src.push(format!("                if matches!(nt, {pattern}) {{"));
                src.push(format!("                    self.stack_span.push(PosSpan::empty());"));
                src.push(format!("                }}"));
            }
        }
        src.push(format!("                match nt {{"));
        /*
                                              0 => self.listener.init_a(),                // A
                                              1 => self.init_a_iter(),                    // AIter1
                                              2 => {}                                     // A_1
        */
        src.extend(columns_to_str(src_init, Some(vec![64, 0])));
        src.push(format!("                    _ => panic!(\"unexpected enter nonterminal id: {{nt}}\")"));
        src.push(format!("                }}"));
        src.push(format!("            }}"));
        src.push(format!("            Call::Loop => {{}}"));
        src.push(format!("            Call::Exit => {{"));
        src.push(format!("                match alt_id {{"));
        /*
                                              3 |                                         // A -> a a (b <L>)* c
                                              4 => self.exit_a(alt_id),                   // A -> a c (b <L>)* c
                                              1 => self.exit_a_iter(),                    // (b <L>)* iteration in A -> a a  ► (b <L>)* ◄  c | ...
                                              2 => {}                                     // end of (b <L>)* iterations in A -> a a  ► (b <L>)* ◄  c | ...
                                           /* 0 */                                        // A -> a a (b <L>)* c | a c (b <L>)* c (never called)
        */
        src.extend(columns_to_str(src_exit, Some(vec![64, 0])));
        src.push(format!("                    _ => panic!(\"unexpected exit alternative id: {{alt_id}}\")"));
        src.push(format!("                }}"));
        src.push(format!("            }}"));
        src.push(format!("            Call::End => {{"));
        if self.nt_value[self.start as usize] {
            src.push(format!("                self.exit();"));
        } else {
            if self.gen_span_params {
                src.push(format!("                let span = self.stack_span.pop().unwrap();"));
                src.push(format!("                self.listener.exit(span);"));
            } else {
                src.push(format!("                self.listener.exit();"));
            }
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
        if self.gen_span_params {
            src.push(format!(""));
            src.push(format!("    fn push_span(&mut self, span: PosSpan) {{"));
            src.push(format!("        self.stack_span.push(span);"));
            src.push(format!("    }}"));
        }
        src.push(format!(""));
        src.push(format!("    fn is_stack_empty(&self) -> bool {{"));
        src.push(format!("        self.stack.is_empty()"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    fn is_stack_t_empty(&self) -> bool {{"));
        src.push(format!("        self.stack_t.is_empty()"));
        src.push(format!("    }}"));
        if self.gen_span_params {
            src.push(format!(""));
            src.push(format!("    fn is_stack_span_empty(&self) -> bool {{"));
            src.push(format!("        self.stack_span.is_empty()"));
            src.push(format!("    }}"));
        }
        src.push(format!("}}"));

        src.add_space();
        src.push(format!("impl<T: {}Listener> Wrapper<T> {{", self.name));
        src.push(format!("    pub fn new(listener: T, verbose: bool) -> Self {{"));
        src.push(format!(
            "        Wrapper {{ verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new(){} }}",
            if self.gen_span_params { ", stack_span: Vec::new()" } else { "" }
        ));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn get_listener(&self) -> &T {{"));
        src.push(format!("        &self.listener"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn get_listener_mut(&mut self) -> &mut T {{"));
        src.push(format!("        &mut self.listener"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn give_listener(self) -> T {{"));
        src.push(format!("        self.listener"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn set_verbose(&mut self, verbose: bool) {{"));
        src.push(format!("        self.verbose = verbose;"));
        src.push(format!("    }}"));
        if self.nt_value[self.start as usize] {
            src.push(format!(""));
            src.push(format!("    fn exit(&mut self) {{"));
            let (_nu, nl, npl) = &nt_name[self.start as usize];
            src.push(format!("        let {nl} = self.stack.pop().unwrap().get_{npl}();"));
            if self.gen_span_params {
                src.push(format!("        let span = self.stack_span.pop().unwrap();"));
            }
            src.push(format!("        self.listener.exit({nl}{});", if self.gen_span_params { ", span" } else { "" }));
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
                                      let star_acc = self.stack.pop().unwrap().get_a_iter();
                                      let val = self.listener.exit_a_iter(CtxAIter::Aiter1 { star_acc, b });
                                      self.stack.push(SynValue::AIter(val));
                                  }
                                  // ...
                              }
*/
        src.extend(src_wrapper_impl);
        src.push(format!("}}"));
        self.log = log;

        src
    }
}

impl LogReader for ParserGen {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        &self.log
    }

    fn give_log(self) -> Self::Item {
        self.log
    }
}

impl HasBuildErrorSource for ParserGen {
    const SOURCE: BuildErrorSource = BuildErrorSource::ParserGen;
}

impl<T> BuildFrom<ProdRuleSet<T>> for ParserGen where ProdRuleSet<LL1>: BuildFrom<ProdRuleSet<T>> {
    /// Creates a [`ParserGen`] from a set of production rules.
    ///
    /// If the rule set has a name, it's transmitted to the parser generator to name the user
    /// listener trait in the generated code. If the rule set has no name, a default "Parser" name
    /// is used instead (unless the name is set with [`ParserGen::set_name()`].
    fn build_from(mut rules: ProdRuleSet<T>) -> Self {
        let name = rules.name.take().unwrap_or(DEFAULT_LISTENER_NAME.to_string());
        ParserGen::build_from_rules(rules, name)
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

pub fn print_items(builder: &ParserGen, indent: usize, show_symbols: bool, show_span: bool) {
    let tbl = builder.get_symbol_table();
    let fields = (0..builder.parsing_table.alts.len())
        .filter_map(|a| {
            let a_id = a as AltId;
            let (v, alt) = &builder.parsing_table.alts[a];
            let ops = &builder.opcodes[a];
            if let Some(it) = builder.item_ops.get(&a_id) {
                let mut cols = vec![];
                if show_symbols {
                    let symbols = format!("symbols![{}]", it.iter().map(|s| s.to_macro_item()).join(", "));
                    let value = if show_span {
                        assert!(builder.gen_span_params, "ParserGen is not configured for spans");
                        format!("({}, {symbols})", builder.span_nbrs[a_id as usize])
                    } else {
                        symbols
                    };
                    cols.push(format!("{a_id} => {value},"));
                }
                cols.extend([
                    format!("// {a_id:2}: {} -> {}", Symbol::NT(*v).to_str(tbl), alt.iter().map(|s| s.to_str_quote(tbl)).join(" ")),
                    format!("| {}", ops.into_iter().map(|s| s.to_str_quote(tbl)).join(" ")),
                    format!("| {}", &builder.span_nbrs[a_id as usize]),
                    format!("| {}", it.iter().map(|s| s.to_str(tbl)).join(" ")),
                ]);
                Some(cols)
            } else {
                None
            }
        }).to_vec();
    let widths = if show_symbols { vec![40, 0, 0, 0, 0] } else { vec![16, 0, 0, 0] };
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
    if !nt_flags.is_empty() {
        println!("{prefix} NT flags:\n{nt_flags}");
    }
    if !parents.is_empty() {
        println!("{prefix} parents:\n{parents}");
    }
}
