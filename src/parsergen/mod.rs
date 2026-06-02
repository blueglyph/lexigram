// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use iter_index::IndexerIterator;
use vectree::VecTree;
use lexigram_core::log::LogMsg;
use lexigram_core::{CharLen, TokenId};
use lexigram_core::alt::Alternative;
use lexigram_core::parser::lr_parser::{LRAction, LRStateId};
use crate::grammar::{grtree_to_str, GrTreeExt, NTConversion, ProdRuleSet};
use crate::{columns_to_str, indent_source, AltId, NameFixer, NameTransformer, SourceSpacer, StructLibs, SymbolTable, VarId, LL1, LR};
use crate::fixed_sym_table::{SymInfoTable};
use crate::alt::ruleflag;
use crate::build::{BuildError, BuildErrorSource, BuildFrom, HasBuildErrorSource};
use crate::CollectJoin;
use crate::grammar::ll1::LL1ParsingTable;
use crate::grammar::origin::{FromPRS, Origin};
use crate::lexergen::LexigramCrate;
use crate::log::{BufLog, LogReader, LogStatus, Logger};
use crate::parser::{OpCode, Symbol};
use crate::segments::Segments;
use crate::segmap::Seg;

pub mod lr;
pub mod ll1;
pub(crate) mod tests;

pub use ll1::LLParserTables;
use crate::parsergen::lr::alts_to_alt_nt_len;
// ---------------------------------------------------------------------------------------------

pub(crate) fn symbol_to_code(s: &Symbol) -> String {
    match s {
        Symbol::Empty => "Symbol::Empty".to_string(),
        Symbol::T(t) => format!("Symbol::T({t})"),
        Symbol::NT(nt) => format!("Symbol::NT({nt})"),
        Symbol::End => "Symbol::End".to_string(),
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

/// Determines which nonterminals have a value, so a dedicated type and field in contexts:
/// * [`None`](NTValue::None): No nonterminal has a value
/// * [`Parents`](NTValue::Parents): Only the top nonterminal parents have a value
/// * [`Default`](NTValue::Default): The top nonterminal parents and the children of `(<L> )+*` have a value
/// * [`SetIds(Vec<VarId>)`](NTValue::SetIds): The nonterminals that have a value is set explicitly by ID
/// * [`SetNames(Vec<String>)`](NTValue::SetNames): The nonterminals that have a value is set explicitly by name.
///   The names "`<default>`" and "`<parents>`" can be used to set all the nonterminals of the corresponding class.
///   Individual nonterminals can be preceded by a "-" to indicate they don't hold a value.
#[derive(Clone, PartialEq, Default, Debug)]
pub enum NTValue {
    /// No nonterminal has a value
    None,
    /// Only the top nonterminal parents have a value
    Parents,
    /// The top nonterminal parents and the children of `(<L> )+*` have a value
    #[default]
    Default,
    /// The set of nonterminals that have a value is set explicitly by ID
    SetIds(Vec<VarId>),
    /// The set of nonterminals that have a value is set explicitly by name. Individual names preceded by a "-" don't have a value.
    SetNames(Vec<String>),
}

impl NTValue {
    /// Only the top nonterminal parents have a value (can be used in [NTValue::SetNames])
    pub const DEFAULT: &str = "<default>";
    /// The top nonterminal parents and the children of `(<L> )+*` have a value (can be used in [NTValue::SetNames])
    pub const PARENTS: &str = "<parents>";

    pub fn is_none(&self) -> bool {
        matches!(self, NTValue::None)
    }

    pub fn is_parents(&self) -> bool {
        matches!(self, NTValue::Parents)
    }

    pub fn is_default(&self) -> bool {
        matches!(self, NTValue::Default)
    }

    pub fn is_ids(&self) -> bool {
        matches!(self, NTValue::SetIds(_))
    }

    pub fn is_names(&self) -> bool {
        matches!(self, NTValue::SetNames(_))
    }
}

// ---------------------------------------------------------------------------------------------

pub static DEFAULT_LISTENER_NAME: &str = "Parser";

pub type SpanNbr = u16;

fn count_span_nbr(opcode: &[OpCode]) -> SpanNbr {
    let count = opcode.iter().filter(|op| op.has_span()).count();
    count.try_into().unwrap_or_else(|_| panic!("# span = {count} > {}", SpanNbr::MAX))
}

struct SourceInputContext<'a> {
    parent_has_value        : bool,
    parent_nt               : usize,
    syns                    : &'a Vec<VarId>,
    ambig_op_alts           : &'a BTreeMap<AltId, Vec<AltId>>,
}

struct SourceState<'a> {
    init_nt_done            : &'a mut HashSet<VarId>,
    span_init               : &'a mut HashSet<VarId>,
    nt_contexts             : &'a mut Vec<Option<Vec<AltId>>>,
    exit_alt_done           : &'a mut HashSet<VarId>,
    exit_fixer              : &'a mut NameFixer,
}

struct WrapperSources {
    src                     : Vec<String>,
    src_listener_decl       : Vec<String>,
    src_skel                : Vec<String>,
    src_types               : Vec<String>,
    src_init                : Vec<Vec<String>>,
    src_exit                : Vec<Vec<String>>,
    src_wrapper_impl        : Vec<String>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParserType {
    LL1,
    LALR,
}

#[derive(Clone, Debug)]
pub struct ParserGenOptions {
    pub nt_value: NTValue,
    pub include_alts: bool,
    pub headers: Vec<String>,
    pub used_libs: StructLibs,
    /// generates the wrapper source code
    pub gen_wrapper: bool,
    /// generates code to give the location of nonterminals and tokens as extra parameters of listener methods
    pub gen_span_params: bool,
    pub gen_token_enums: bool,
    pub lib_crate: LexigramCrate,
    /// source code indentation of the wrapper, in number of space characters
    pub indent: usize,
    /// source code indentation of the template for the user types
    pub types_indent: usize,
    /// source code indentation of the template for the listener implementation
    pub listener_indent: usize,
    /// parser type: LL1 or LALR
    pub parser_type: ParserType,
}

#[derive(Debug)]
pub struct ParserGen {
    // common parsing table fields:
    num_nt: usize,
    num_t_full: usize,               // includes the end $ symbol
    alts: Vec<(VarId, Alternative)>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    // LL1-specific parsing table fields:
    table: Vec<AltId>,
    // LR-specific parsing table fields:
    #[allow(unused)]
    num_states: usize,
    #[allow(unused)]
    action: Vec<LRAction>,
    #[allow(unused)]
    goto: Vec<LRStateId>,

    symbol_table: SymbolTable,
    terminal_hooks: Vec<TokenId>,
    name: String,
    options: ParserGenOptions,
    nt_values: Vec<bool>,
    /// `nt_parent[v]` is the vector of all variables having `v` has top parent (including `v` itself)
    nt_parent: Vec<Vec<VarId>>,
    var_alts: Vec<Vec<AltId>>,
    origin: Origin<VarId, FromPRS>,
    item_ops: Vec<Vec<Symbol>>,
    opcodes: Vec<Vec<OpCode>>,
    init_opcodes: Vec<OpCode>,
    nt_name: Vec<(String, String, String)>,
    alt_info: Vec<Option<(VarId, String)>>,
    item_info: Vec<Vec<ItemInfo>>,
    child_repeat_endpoints: HashMap<VarId, Vec<AltId>>,
    /// generates the parser source code
    gen_parser: bool,
    span_nbrs: Vec<SpanNbr>,
    /// Number of span values taken from a PARENT_REPEAT in init() to extract the first item of a SEP_LIST
    span_nbrs_sep_list: HashMap<AltId, SpanNbr>,
    start: VarId,
    nt_conversion: HashMap<VarId, NTConversion>,
    nt_type: HashMap<VarId, String>,
    log: BufLog,
}

impl ParserGen {
    /// Creates a [ParserGen] from a set of production rules and gives it a specific name, which is used
    /// to name the user listener trait in the generated code.
    ///
    /// If `rules` already has a name, it is best to use the
    /// [`BuildFrom<ProdRuleSet<T>>`](BuildFrom<ProdRuleSet<T>>::build_from) trait.
    pub fn build_from_rules_ll1<T>(mut rules: ProdRuleSet<T>, name: String) -> Self
    where
        ProdRuleSet<LL1>: BuildFrom<ProdRuleSet<T>>,
    {
        rules.log.add_note("building parser gen from rules...");
        let mut ll1_rules = ProdRuleSet::<LL1>::build_from(rules);
        assert_eq!(ll1_rules.get_log().num_errors(), 0);
        let parsing_table = ll1_rules.make_parsing_table(true);
        let num_nt = ll1_rules.get_num_nt();
        let start = ll1_rules.get_start().unwrap();
        let mut var_alts = vec![vec![]; num_nt];
        for (alt_id, (var_id, _)) in parsing_table.alts.iter().index() {
            var_alts[*var_id as usize].push(alt_id);
        }
        let nt_parent: Vec<Vec<VarId>> = vec![vec![]; num_nt];
        let ProdRuleSet { symbol_table, nt_conversion, origin, .. } = ll1_rules;
        let mut builder = ParserGen {
            num_nt: parsing_table.num_nt,
            num_t_full: parsing_table.num_t_full,
            alts: parsing_table.alts,
            flags: parsing_table.flags,
            parent: parsing_table.parent,
            table: parsing_table.table,
            num_states: 0,
            action: vec![],
            goto: vec![],
            symbol_table: symbol_table.expect(stringify!("symbol table is required to create a {}", std::any::type_name::<Self>())),
            name,
            options: ParserGenOptions::default(),
            nt_values: vec![false; num_nt],
            nt_parent,
            var_alts,
            origin,
            terminal_hooks: Vec::new(),
            item_ops: Vec::new(),
            opcodes: Vec::new(),
            init_opcodes: Vec::new(),
            nt_name: Vec::new(),
            alt_info: Vec::new(),
            item_info: Vec::new(),
            child_repeat_endpoints: HashMap::new(),
            gen_parser: true,
            span_nbrs: Vec::new(),
            span_nbrs_sep_list: HashMap::new(),
            start,
            nt_conversion,
            nt_type: HashMap::new(),
            log: ll1_rules.log,
        };
        builder.post_build_from_rules(ParserType::LL1);
        builder
    }

    /// Creates a [ParserGen] from a set of production rules and gives it a specific name, which is used
    /// to name the user listener trait in the generated code.
    ///
    /// If `rules` already has a name, it is best to use the
    /// [`BuildFrom<ProdRuleSet<T>>`](BuildFrom<ProdRuleSet<T>>::build_from) trait.
    pub fn build_from_rules_lr<T>(mut rules: ProdRuleSet<T>, name: String) -> Self
    where
        ProdRuleSet<LR>: BuildFrom<ProdRuleSet<T>>,
    {
        rules.log.add_note("building parser gen from rules...");
        let mut lr_rules = ProdRuleSet::<LR>::build_from(rules);
        assert_eq!(lr_rules.get_log().num_errors(), 0);
        let parsing_table = lr_rules.make_parsing_table_lalr();
        let num_nt = lr_rules.get_num_nt();
        let mut var_alts = vec![vec![]; num_nt];
        for (alt_id, (var_id, _)) in parsing_table.alts.iter().index() {
            var_alts[*var_id as usize].push(alt_id);
        }
        let nt_parent: Vec<Vec<VarId>> = vec![vec![]; num_nt];
        let ProdRuleSet { symbol_table, nt_conversion, origin, .. } = lr_rules;

        let mut builder = ParserGen {
            num_nt: parsing_table.num_nt,
            num_t_full: parsing_table.num_t_full,
            alts: parsing_table.alts,
            flags: parsing_table.flags,
            parent: parsing_table.parent,
            table: vec![],
            num_states: parsing_table.num_states,
            action: parsing_table.action,
            goto: parsing_table.goto,
            symbol_table: symbol_table.expect(stringify!("symbol table is required to create a {}", std::any::type_name::<Self>())),
            name,
            options: ParserGenOptions::default(),
            nt_values: vec![false; num_nt],
            nt_parent,
            var_alts,
            origin,
            terminal_hooks: Vec::new(),
            item_ops: Vec::new(),
            opcodes: Vec::new(),
            init_opcodes: Vec::new(),
            nt_name: Vec::new(),
            alt_info: Vec::new(),
            item_info: Vec::new(),
            child_repeat_endpoints: HashMap::new(),
            gen_parser: true,
            span_nbrs: Vec::new(),
            span_nbrs_sep_list: HashMap::new(),
            start: 0,
            nt_conversion,
            nt_type: HashMap::new(),
            log: lr_rules.log,
        };
        builder.post_build_from_rules(ParserType::LALR);
        builder
    }

    fn post_build_from_rules(&mut self, parser_type: ParserType) {
        self.options.parser_type = parser_type;
        for var_id in 0..self.num_nt {
            let top_var_id = self.get_top_parent(var_id as VarId) as usize;
            self.nt_parent[top_var_id].push(var_id as VarId);
        }
        self.apply_options();
        self.calc_opcodes();
        self.calc_span_nbrs();
    }

    pub fn set_options(&mut self, options: ParserGenOptions) {
        assert_eq!(options.parser_type, self.options.parser_type, "incompatible parser type");
        self.options = options;
        self.apply_options();
    }

    /// Sets internal values accordingly to the options
    fn apply_options(&mut self) {
        self.apply_nt_value();
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
    pub fn set_terminal_hooks(&mut self, terminal_hooks: Vec<TokenId>) {
        if !terminal_hooks.is_empty() {
            self.options.gen_token_enums = true;
        }
        self.terminal_hooks = terminal_hooks;
        self.add_opcode_hooks();
    }

    #[inline]
    pub fn add_header<T: Into<String>>(&mut self, header: T) {
        self.options.headers.push(header.into());
    }

    #[inline]
    pub fn extend_headers<I: IntoIterator<Item=T>, T: Into<String>>(&mut self, headers: I) {
        self.options.headers.extend(headers.into_iter().map(|s| s.into()));
    }

    #[inline]
    pub fn add_lib<T: Into<String>>(&mut self, lib:T) {
        self.options.used_libs.add(lib);
    }

    #[inline]
    pub fn extend_libs<I: IntoIterator<Item=T>, T: Into<String>>(&mut self, libs: I) {
        self.options.used_libs.extend(libs);
    }

    #[inline]
    /// Declares the type of a non-terminal. The index of the NT, `org_var`, is the original index
    /// in the ruletree set, which is the index originally assigned when parsing the grammar file.
    pub fn add_nt_type<T: Into<String>>(&mut self, org_var: VarId, var_type: T) {
        let var = self.conv_nt(org_var).unwrap_or_else(|| panic!("var {org_var} doesn't exist"));
        self.nt_type.insert(var, var_type.into());
    }

    #[inline]
    pub fn get_nt_type(&self, v: VarId) -> &str {
        self.nt_type.get(&v).unwrap().as_str()
    }

    /// Sets which nonterminals have a value, from [nt_value](NTValue).
    pub fn set_nt_value(&mut self, nt_value: NTValue) {
        self.options.nt_value = nt_value;
        self.apply_nt_value();
    }

    pub fn make_ll1_parsing_table(&self) -> LL1ParsingTable {
        LL1ParsingTable {
            num_nt: self.num_nt,
            num_t_full: self.num_t_full,
            alts: self.alts.clone(),
            table: self.table.clone(),
            flags: self.flags.clone(),
            parent: self.parent.clone()
        }
    }

    fn apply_nt_value(&mut self) {
        let num_nt = self.get_symbol_table().unwrap().get_num_nt() as VarId;
        let mut stack = vec![&self.options.nt_value];
        let mut neg_stack = vec![];
        self.nt_values.fill(false);
        while let Some(nt_value) = stack.pop() {
            match nt_value {
                NTValue::None => {}
                NTValue::Parents => {
                    for v in 0..num_nt {
                        if self.get_nt_parent(v).is_none() {
                            self.nt_values[v as usize] = true;
                        }
                    }
                }
                NTValue::Default => {
                    for v in 0..num_nt {
                        if self.get_nt_parent(v).is_none() || self.nt_has_all_flags(v, ruleflag::CHILD_REPEAT | ruleflag::L_FORM) {
                            self.nt_values[v as usize] = true;
                        }
                    }
                }
                NTValue::SetIds(ids) => {
                    for v in ids {
                        if *v < num_nt {
                            self.nt_values[*v as usize] = true;
                        } else {
                            self.log.add_error(format!("setting value of NT #{v}, which doesn't exist"));
                        }
                    }
                }
                NTValue::SetNames(names) => {
                    let name_to_id = self.symbol_table.get_nonterminals().index::<VarId>()
                        .map(|(v, name)| (name.as_str(), v))
                        .collect::<HashMap<&str, VarId>>();
                    for name in names {
                        match name.as_str() {
                            NTValue::DEFAULT => stack.push(&NTValue::Default),
                            NTValue::PARENTS => stack.push(&NTValue::Parents),
                            mut nt_name => {
                                let add = if !nt_name.starts_with('-') {
                                    true
                                } else {
                                    nt_name = &nt_name[1..];
                                    false
                                };
                                if let Some(v) = name_to_id.get(nt_name) {
                                    if add {
                                        self.nt_values[*v as usize] = true;
                                    } else {
                                        neg_stack.push(*v);
                                    }
                                } else {
                                    self.log.add_error(format!("setting value of NT '{name}', which doesn't exist"));
                                }
                            }
                        }
                    }
                }
            }
        }
        for v in neg_stack {
            self.nt_values[v as usize] = false;
        }
    }

    #[inline]
    pub fn set_nt_has_value(&mut self, v: VarId, has_value: bool) {
        self.nt_values[v as usize] = has_value;
    }

    /// Generates the parser source code if `gen_parser` is `true`. This option is `true` by default.
    pub fn set_gen_parser(&mut self, gen_parser: bool) {
        self.gen_parser = gen_parser;
    }

    /// Generates the wrapper source code if `gen_parser` is `true`. This option is `true` by default.
    pub fn set_gen_wrapper(&mut self, gen_wrapper: bool) {
        self.options.gen_wrapper = gen_wrapper;
    }

    /// Sets the source code indentation. This option is 0 by default.
    pub fn set_indent(&mut self, indent: usize) {
        self.options.indent = indent;
    }

    /// Sets the source code indentation of the template for the user types.
    /// This option is 0 by default.
    pub fn set_types_indent(&mut self, indent: usize) {
        self.options.types_indent = indent;
    }

    /// Sets the source code indentation of the template for the listener implementation.
    /// This option is 0 by default.
    pub fn set_listener_indent(&mut self, indent: usize) {
        self.options.listener_indent = indent;
    }

    /// Sets the source code indentation for the wrapper, the template for the user types and the
    /// template for the listener implementation.
    pub fn set_indents(&mut self, wrapper: usize, types: usize, listner: usize) {
        self.options.indent = wrapper;
        self.options.types_indent = types;
        self.options.listener_indent = listner;
    }

    /// Generates code to give the location of nonterminals and tokens as extra parameters of listener methods.
    pub fn set_gen_span_params(&mut self, gen_span_params: bool) {
        self.options.gen_span_params = gen_span_params;
    }

    /// Generates enums for the terminal and nonterminal values. They may be helpful in the optional
    /// listener trait methods like `hook()` and `intercept_token()` when they are used.
    ///
    /// # Example
    ///
    /// ```ignore
    /// #[derive(Clone, Copy, PartialEq, Debug)]
    /// #[repr(u16)]
    /// pub enum Term {
    ///     #[doc = "','"]        Comma = 0,
    ///     #[doc = "';'"]        SemiColon = 1,
    /// }
    ///
    /// #[derive(Clone, Copy, PartialEq, Debug)]
    /// #[repr(u16)]
    /// pub enum NTerm {
    ///     #[doc = "`program`"]                   Program = 0,
    ///     #[doc = "`stmt_i`, parent: `program`"] StmtI = 1,
    /// }
    /// ```
    pub fn set_gen_token_enums(&mut self, gen_token_enums: bool) {
        self.options.gen_token_enums = gen_token_enums;
    }

    #[inline]
    pub fn get_nt_parent(&self, v: VarId) -> Option<VarId> {
        self.parent[v as usize]
    }

    #[inline]
    pub fn get_top_parent(&self, nt: VarId) -> VarId {
        let mut var = nt;
        while let Some(parent) = self.parent[var as usize] {
            var = parent;
        }
        var
    }

    /// Include the definitions of the alternatives in the parser, for debugging purposes:
    /// allows to print out the alternatives in VERBOSE mode.
    pub fn set_include_alts(&mut self, include_alts: bool) {
        self.options.include_alts = include_alts;
    }

    #[inline]
    pub fn use_full_lib(&mut self, use_full_lib: bool) {
        self.options.lib_crate = if use_full_lib { LexigramCrate::Full } else { LexigramCrate::Core };
    }

    #[inline]
    pub fn set_crate(&mut self, lcrate: LexigramCrate) {
        self.options.lib_crate = lcrate;
    }

    #[cfg(test)] // we keep it here because we'll need it later for doc comments and logs
    fn get_original_alt_str(&self, a_id: AltId, symbol_table: Option<&SymbolTable>) -> Option<String> {
        let (_var, f) = &self.alts[a_id as usize];
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
            None => if (org_var as usize) < self.num_nt { Some(org_var) } else { None },
            Some(NTConversion::MovedTo(new)) => Some(*new),
            Some(NTConversion::Removed) => None
        }
    }

    #[allow(unused)]
    fn nt_has_all_flags(&self, var: VarId, flags: u32) -> bool {
        self.flags[var as usize] & flags == flags
    }

    #[allow(unused)]
    fn nt_has_any_flags(&self, var: VarId, flags: u32) -> bool {
        self.flags[var as usize] & flags != 0
    }

    #[allow(unused)]
    fn sym_has_flags(&self, s: &Symbol, flags: u32) -> bool {
        if let Symbol::NT(nt) = s { self.nt_has_all_flags(*nt, flags) } else { false }
    }

    #[allow(unused)]
    fn sym_has_value(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::T(t) => self.symbol_table.is_token_data(*t),
            Symbol::NT(nt) => self.nt_values[*nt as usize],
            _ => false
        }
    }

    fn full_alt_components(&self, a_id: AltId, emphasis: Option<VarId>) -> (String, String) {
        const VERBOSE: bool = false;
        if VERBOSE { println!("full_alt_components(a_id = {a_id}):"); }
        let &(mut v_a, ref alt) = &self.alts[a_id as usize];
        while self.flags[v_a as usize] & ruleflag::CHILD_L_FACT != 0 {
            v_a = *self.parent[v_a as usize].as_ref().unwrap();
        }
        let symtab = self.get_symbol_table();
        if let Some(v_emph) = emphasis {
            let parent_nt = self.get_top_parent(v_emph);
            if let Some((t_emph, id_emph)) = self.origin.get(v_emph) {
                return ((Symbol::NT(parent_nt).to_str(symtab)), grtree_to_str(t_emph, None, Some(id_emph), Some(parent_nt), symtab, true));
            } else {
                return (Symbol::NT(parent_nt).to_str(symtab), format!("<VAR {v_emph} NOT FOUND>"));
            }
        }
        if let Some((vo, id)) = alt.get_origin() {
            let t = self.origin.get_tree(vo).unwrap();
            let flags = self.flags[v_a as usize];
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

    fn calc_opcodes(&mut self) {
        const VERBOSE: bool = false;
        self.log.add_note("- making opcodes...");
        if self.options.parser_type == ParserType::LALR {
            self.opcodes = self.alts.iter()
                .map(|(nt, Alternative { v, .. })| {
                    let mut ops = v.iter().filter_map(|s| if !s.is_empty() { Some(OpCode::from(s.clone())) } else { None })
                        .rev()
                        .to_vec();
                    if self.flags[*nt as usize] & ruleflag::CHILD_REPEAT != 0 {
                        let op = ops.last_mut();
                        if matches!(op, Some(OpCode::NT(nt2)) if nt == nt2) {
                            *op.unwrap() = OpCode::Loop(*nt);
                        }
                    }
                    ops
                })
                .to_vec();
            return;
        }
        self.opcodes.clear();
        self.init_opcodes = vec![OpCode::End, OpCode::NT(self.start)];
        for (alt_id, (var_id, alt)) in self.alts.iter().index() {
            if VERBOSE {
                println!("{alt_id}: {}", alt.to_rule_str(*var_id, self.get_symbol_table(), 0));
            }
            let flags = self.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.alts[alt_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            if VERBOSE { println!("  - {}", new.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut opcode = Vec::<OpCode>::new();
            let mut parent = self.parent[*var_id as usize];
            if flags & ruleflag::CHILD_L_FACT != 0 {
                while self.nt_has_all_flags(parent.unwrap(), ruleflag::CHILD_L_FACT) {
                    parent = self.parent[parent.unwrap() as usize];
                }
                let parent = parent.unwrap();
                // replaces Enter by Loop when going back to left-factorization parent, typically when coupled with + or *
                // (per construction, there can't be any alternative going back to the grandparent or further up in a left factorization, so
                //  we don't check that)
                let parent_r_form_right_rec = self.flags[parent as usize] & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
                if VERBOSE {
                    println!("  - child lfact, parent: {}, !parent_r_form_right_rec = !{parent_r_form_right_rec}, match = {}",
                             Symbol::NT(parent).to_str(self.get_symbol_table()),
                             new.first() == Some(&Symbol::NT(parent)));
                }
                if new.first() == Some(&Symbol::NT(parent)) && !parent_r_form_right_rec {
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
            opcode.extend(new.into_iter().map(OpCode::from));
            let r_form_right_rec = flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
            if VERBOSE { println!("  - r_form_right_rec = {r_form_right_rec} = {} || {}",
                                  flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0,
                                  flags & ruleflag::CHILD_L_FACT != 0 && self.flags[parent.unwrap() as usize] & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0); }
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
            if flags & ruleflag::CHILD_L_FACT != 0 && opcode.len() >= 2 {
                if self.nt_has_all_flags(parent.unwrap(), ruleflag::R_RECURSION | ruleflag::L_FORM)
                    && opcode[1] == OpCode::NT(parent.unwrap())
                {
                    opcode.swap(0, 1);
                    opcode[0] = OpCode::Loop(parent.unwrap());
                }
                let fact_top = self.get_top_parent(*var_id);
                if VERBOSE {
                    println!("  - check for initial exit swap: opcode = [{}], daddy = {}",
                             opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                             Symbol::NT(fact_top).to_str(self.get_symbol_table()));
                }
                if self.flags[fact_top as usize] & ruleflag::PARENT_L_RECURSION != 0 &&
                    matches!(opcode[0], OpCode::Exit(_)) &&
                    matches!(opcode[1], OpCode::NT(v) if self.flags[v as usize] & ruleflag::CHILD_L_RECURSION != 0)
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

    /// Adds hook opcodes that must be popped before the corresponding terminal is processed,
    /// and possibly modified. For example, an `Id` terminal must be changed into `Type`
    /// when that id has been previously declared as type in the parsed text.
    ///
    /// ```text
    ///          | Num Id  Type  ,   ;  typedef let  =  print  -   +   $
    /// ---------+--------------------------------------------------------
    /// program  |  .   .   0    .   .     0     0   .    0    .   .   p
    /// decl_i   |  .   .   1    .   .     1     2   .    2    .   .   .
    /// inst_i   |  .   .   .    .   .     .     3   .    3    .   .   p
    /// decl     |  .   .   4    .   .     5     p   .    p    .   .   .
    /// […]
    ///
    ///  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i
    ///  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl
    ///  2: decl_i -> ε                   | ◄2
    ///  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst
    ///  4: decl -> Type Id decl_1 ";"    | ◄4 ";" ►decl_1 Id! Type!
    ///  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef"
    /// […]
    /// ```
    /// - create set of NT that need hooked terminal to make a decision in the table:
    ///
    ///     `deps = { ►program, ►decl_i, ►decl }`
    ///
    /// - hook required at init if start NT in deps:
    ///
    ///     `init stack = [end ►program hook]`
    ///
    /// - hook required after each deps item that isn't in last opcode position:
    ///     ```text
    ///     1: decl_i -> decl decl_i         | ●decl_i hook ◄1 ►decl hook
    ///                                                ^^^^
    ///     ```
    /// - hook required after each hooked terminal that isn't in last opcode position:
    ///     ```text
    ///     5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! hook "typedef"
    ///                                                         ^^^^
    ///     ```
    fn add_opcode_hooks(&mut self) {
        const VERBOSE: bool = false;
        if self.options.parser_type != ParserType::LL1 { return }
        self.log.add_note("- adding hooks into opcodes...");
        let hooks: HashSet<TokenId> = self.terminal_hooks.iter().cloned().collect();
        let num_nt = self.num_nt;
        let num_t = self.num_t_full;
        let err = self.alts.len() as AltId;
        if VERBOSE {
            self.make_ll1_parsing_table().print(self.get_symbol_table(), 0);
            println!("num_nt = {num_nt}\nnum_t = {num_t}\ntable: {}", self.table.len());
        }
        if VERBOSE { println!("hooks: {}", self.terminal_hooks.iter().map(|t| self.symbol_table.get_t_name(*t)).join(", ")); }
        let deps: HashSet<VarId> = (0..num_nt as VarId)
            .filter(|&nt| hooks.iter().any(|&t| self.table[nt as usize * num_t + t as usize] < err))
            .collect();
        if VERBOSE { println!("deps = {deps:?} = {}", deps.iter().map(|nt| self.symbol_table.get_nt_name(*nt)).join(", ")); }

        // hook required when parser starts?
        if deps.contains(&self.start) {
            self.init_opcodes = vec![OpCode::End, OpCode::NT(self.start), OpCode::Hook];
        }
        let mut changed = false;
        for opcodes in self.opcodes.iter_mut() {
            let mut new = vec![];
            let n = opcodes.len();
            for op in &opcodes[..n - 1] {
                new.push(*op);
                match op {
                    OpCode::T(t) if hooks.contains(t) => {
                        new.push(OpCode::Hook);
                    }
                    OpCode::NT(nt) | OpCode::Loop(nt) if deps.contains(nt) => {
                        new.push(OpCode::Hook);
                    }
                    _ => {}
                }
            }
            if new.len() + 1 > n {
                new.push(opcodes[n - 1]);
                *opcodes = new;
                changed = true;
            }
        }
        if VERBOSE && changed {
            println!("new opcodes:");
            let mut cols = vec![];
            let tbl = self.get_symbol_table();
            for (i, (opcodes, (nt, alt))) in self.opcodes.iter().zip(&self.alts).enumerate() {
                cols.push(vec![
                    i.to_string(),
                    format!("{} -> ", Symbol::NT(*nt).to_str(tbl)),
                    alt.to_str(tbl),
                    opcodes.iter().map(|op| op.to_str(tbl)).join(" "),
                ]);
            }
            println!("{}", indent_source(vec![columns_to_str(cols, None)], 4))
        }
    }

    fn calc_span_nbrs(&mut self) {
        self.log.add_note("- making spans...");
        let mut span_nbrs = vec![0 as SpanNbr; self.alts.len()];
        for (alt_id, (var_id, _)) in self.alts.iter().enumerate() {
            let opcode = &self.opcodes[alt_id];
            let mut span_nbr = span_nbrs[alt_id] + count_span_nbr(opcode);
            if self.nt_has_any_flags(*var_id, ruleflag::CHILD_REPEAT | ruleflag::CHILD_L_RECURSION) ||
                self.nt_has_all_flags(*var_id, ruleflag::R_RECURSION | ruleflag::L_FORM) {
                // there is a loop span
                span_nbr += 1;
            }
            if matches!(opcode.first(), Some(OpCode::NT(nt)) if nt != var_id && self.flags[*nt as usize] & ruleflag::CHILD_L_RECURSION != 0) {
                // independent lrec term: the first NT doesn't count
                span_nbr -= 1;
            }
            // println!("### {} -> span = {span_nbr}: {}",
            //          opcode.iter().map(|o| o.to_string()).join(" "), opcode.iter().filter(|o| o.has_span()).map(|o| o.to_string()).join(" "));
            // println!("[{alt_id}]: {} + {} -> {span_nbr}", span_nbrs[alt_id], count_span_nbr(&opcode));
            if self.nt_has_all_flags(*var_id, ruleflag::PARENT_L_FACTOR) {
                if let Some(OpCode::NT(nt)) = opcode.first() {
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

    fn get_group_alts(&self, g: &[VarId]) -> Vec<(VarId, AltId)> {
        g.iter().flat_map(|c|
            self.var_alts[*c as usize].iter().map(|a| (*c, *a))
        ).collect::<Vec<_>>()
    }

    /// Gathers all the alternatives of a nonterminal, and if some of them are parent_l_fact, searches the
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
                let (_, alter) = &self.alts[*a as usize];
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

    fn calc_nt_value(&mut self) {
        const VERBOSE: bool = false;
        self.log.add_note("- calculating nonterminals' value...");
        // we proceed by var parent, then all alternatives in each parent/children group
        for g in self.nt_parent.iter().filter(|va| !va.is_empty()) {
            // takes all the alternatives in the group (and their NT ID):
            let group = self.get_group_alts(g);
            let mut re_evaluate = true;
            let g_top = g[0];
            let is_ambig = self.nt_has_all_flags(g_top, ruleflag::PARENT_AMBIGUITY);
            while re_evaluate {
                re_evaluate = false;
                let mut nt_used = HashSet::<VarId>::new();
                if VERBOSE {
                    let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                    println!("parent: {}, NT with value: {}",
                             Symbol::NT(g[0]).to_str(self.get_symbol_table()),
                             ids.into_iter().filter_map(|v|
                                 if self.nt_values[v as usize] { Some(Symbol::NT(v as VarId).to_str(self.get_symbol_table())) } else { None }
                             ).join(", "));
                }
                for (var_id, alt_id) in &group {
                    // Default values are taken from opcodes.
                    let mut has_value = false;
                    for s in &self.opcodes[*alt_id as usize] {
                        match s {
                            OpCode::T(t) =>
                                has_value |= self.symbol_table.is_token_data(*t),
                            OpCode::NT(nt) => {
                                let is_ambig_top = is_ambig && self.get_nt_parent(*nt) == Some(g_top)
                                    && !self.nt_has_any_flags(*nt, ruleflag::CHILD_L_RECURSION | ruleflag::CHILD_REPEAT);
                                let var = if is_ambig_top { g_top } else { *nt };
                                nt_used.insert(var);
                                has_value |= self.nt_values[var as usize]
                            },
                            _ => {}
                        }
                    }
                    // Looks if a child_repeat has a value
                    if has_value && self.parent[*var_id as usize].is_some() {
                        // If it's a child of left factorization, we need to find the original nonterminal with the flags:
                        let mut child_nt = *var_id as usize;
                        while self.flags[child_nt] & ruleflag::CHILD_REPEAT == 0 {
                            if let Some(parent) = self.parent[child_nt] {
                                child_nt = parent as usize;
                            } else {
                                break;
                            }
                        }
                        // +* non-lform children have the same value as their parent, but +* lform
                        // children's value is independent of their parent's
                        if self.flags[child_nt] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {
                            if VERBOSE && !self.nt_values[child_nt] {
                                print!(" | {} is now valued {}",
                                       Symbol::NT(child_nt as VarId).to_str(self.get_symbol_table()),
                                       if nt_used.contains(&(child_nt as VarId)) { "and was used before" } else { "but wasn't used before" }
                                );
                            }
                            re_evaluate |= !self.nt_values[child_nt] && nt_used.contains(&(child_nt as VarId));
                            self.nt_values[child_nt] = true;
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn calc_item_ops(&mut self) {
        const VERBOSE: bool = false;
        self.calc_nt_value();
        self.log.add_note("- making item ops...");
        let mut items = vec![Vec::<Symbol>::new(); self.alts.len()];
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
        let mut alts_to_revisit = HashSet::<AltId>::new();
        // we proceed by var parent, then all alternatives in each parent/children group
        for g in self.nt_parent.iter().filter(|va| !va.is_empty()) {
            // takes all the alternatives in the group (and their NT ID):
            let group = self.get_group_alts(g);
            let g_top = g[0];
            let is_ambig = self.nt_has_all_flags(g_top, ruleflag::PARENT_AMBIGUITY);
            if VERBOSE {
                let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                println!("parent: {}, NT with value: {}",
                         Symbol::NT(g[0]).to_str(self.get_symbol_table()),
                         ids.into_iter().filter_map(|v|
                             if self.nt_values[v as usize] { Some(Symbol::NT(v as VarId).to_str(self.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            let g_top_has_value = self.nt_values[g_top as usize];
            for (var_id, alt_id) in &group {
                let ambig_loop_value = g_top_has_value && is_ambig && self.nt_has_all_flags(*var_id, ruleflag::CHILD_L_RECURSION);
                items[*alt_id as usize] = if ambig_loop_value { vec![Symbol::NT(g_top)] } else { vec![] };
            }
            for (var_id, alt_id) in &group {
                let opcode = &self.opcodes[*alt_id as usize];
                let (_, alt) = &self.alts[*alt_id as usize];
                if VERBOSE {
                    print!("- {alt_id}: {} -> {}   [{}]",
                           Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                           alt.to_str(self.get_symbol_table()),
                           opcode.iter().map(|op| op.to_str(self.get_symbol_table())).join(" "));
                }
                let flags = self.flags[*var_id as usize];

                // Default values are taken from opcodes. Loop(nt) is only taken if the parent is l-rec;
                // we look at the parent's flags instead of the alternative's because left factorization could
                // displace the Loop(nt) to another non-l-rec child alternative.
                let mut has_sep_list_child_without_value = false;
                let mut values = self.opcodes[*alt_id as usize].iter().rev()
                    .filter_map(|s| {
                        let sym_maybe = match s {
                            OpCode::T(t) => Some(Symbol::T(*t)),
                            OpCode::NT(nt) => {
                                let is_ambig_top = is_ambig && self.get_nt_parent(*nt) == Some(g_top)
                                    && !self.nt_has_any_flags(*nt, ruleflag::CHILD_L_RECURSION | ruleflag::CHILD_REPEAT);
                                let var = if is_ambig_top { g_top } else { *nt };
                                Some(Symbol::NT(var))
                            },
                            _ => {
                                if VERBOSE { print!(" | {} dropped", s.to_str(self.get_symbol_table())); }
                                None
                            }
                        };
                        sym_maybe.and_then(|s| {
                            const REP_MASK: u32 = ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM;
                            const CHILD_STAR: u32 = ruleflag::CHILD_REPEAT | ruleflag::L_FORM;
                            let has_value = self.sym_has_value(&s);
                            if has_value
                                // for now, leaves child* nonterminals used in another nonterminal (the parent),
                                // because they're necessary in check_sep_list() to locate the first sep_list item:
                                || matches!(s, Symbol::NT(v) if v != *var_id && self.flags[v as usize] & REP_MASK == CHILD_STAR)
                            {
                                if !has_value {
                                    has_sep_list_child_without_value = true;
                                }
                                Some(s)
                            } else {
                                None
                            }
                        })
                    }).to_vec();
                //
                if has_sep_list_child_without_value {
                    // there's a child* nonterminal in `values` that has no value and should be removed later
                    alts_to_revisit.insert(*alt_id);
                }
                // Loop NTs which carry values are kept on the stack, too
                let parent_is_rrec_lfact = !is_ambig && self.nt_has_all_flags(g[0], ruleflag::R_RECURSION | ruleflag::PARENT_L_FACTOR);
                if parent_is_rrec_lfact {
                    if flags & ruleflag::CHILD_L_FACT != 0 && self.nt_has_all_flags(g[0], ruleflag::L_FORM) {
                        assert!(!self.nt_has_all_flags(*var_id, ruleflag::CHILD_L_FACT | ruleflag::L_FORM), "this was useful after all");
                        if VERBOSE { print!(" child_rrec_lform_lfact"); }
                        items[*alt_id as usize].insert(0, Symbol::NT(g[0]));
                    }
                } else {
                    let sym_maybe = if flags & ruleflag::CHILD_REPEAT != 0 && (self.nt_values[*var_id as usize] || flags & ruleflag::L_FORM != 0) {
                        Some(Symbol::NT(*var_id))
                    } else if !is_ambig && flags & ruleflag::CHILD_L_RECURSION != 0 {
                        let parent = self.parent[*var_id as usize].unwrap();
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
                             items[*alt_id as usize].iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                             values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "));
                }
                if let Some(OpCode::NT(nt)) = opcode.first() {
                    // Take the values except the last NT
                    let backup = if matches!(values.last(), Some(Symbol::NT(x)) if x == nt) {
                        Some(values.pop().unwrap())
                    } else {
                        None
                    };
                    if nt != var_id && self.nt_has_all_flags(*nt, ruleflag::CHILD_L_RECURSION) {
                        if VERBOSE { println!("  CHILD_L_RECURSION"); }
                        // exit_<var_id>(context = values) before entering child loop
                        items[*alt_id as usize].extend(values);
                        continue;
                    }
                    if flags & ruleflag::PARENT_L_FACTOR != 0 {
                        if VERBOSE {
                            println!("  PARENT_L_FACTOR: moving {} to child {}",
                                     values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                     Symbol::NT(*nt).to_str(self.get_symbol_table()));
                        }
                        // factorization reports all the values to the children
                        let pre = &mut items[*alt_id as usize];
                        if !pre.is_empty() {
                            // pre-pends values that already exist for alt_id (and empties alt_id)
                            values.splice(0..0, std::mem::take(pre));
                        }
                        for a_id in self.var_alts[*nt as usize].iter() {
                            items[*a_id as usize].extend(values.clone());
                            // Can an lfact parent contain a child*? No, because child* are transformed in the
                            // RTS, while lfact is a late-PRS transformation. If an lfact parent included a
                            // child*, like in `parent -> x* A | x* B`, the x* of each lfact alt would be
                            // different: something like `parent -> parent_1 A | parent_2 B`.
                            // There wouldn't be an lfact => we don't need to add a_id to alts_to_revisit
                        }
                        continue;
                    }
                    if let Some(sym) = backup {
                        values.push(sym);
                    }
                }
                items[*alt_id as usize].extend(values);
            } // WARNING: loop has `continue` jumps
        }

        // adds sep_list flags to * with token-separated lists, and removes the corresponding
        // items that will be taken in an init method or added in first position in the array
        self.check_sep_list(&mut items);

        // removes the child* nonterminals with no value that had been left earlier for check_sep_list()
        for alt_id in alts_to_revisit {
            items[alt_id as usize].retain(|s| self.sym_has_value(s));
        }
        self.item_ops = items;

        self.log.add_note(
            format!(
                "NT with value: {}",
                self.nt_values.iter().index()
                    .filter(|&(_, val)| *val)
                    .map(|(var, _)| Symbol::NT(var).to_str(self.get_symbol_table()))
                    .join(", ")));
    }

    /// Detects and transforms token-separated item lists of the form α (β α)*
    /// - α having valuable T/NT
    /// - β having no value (typ. fixed terminals)
    fn check_sep_list(&mut self, items: &mut [Vec<Symbol>]) {
        // a -> Id "(" Id ":" type ("," Id ":" type)* ")"
        //
        // alt | rule alternative                 | items
        // ----+----------------------------------+---------------
        //  0  |  a -> Id "(" Id ":" type a_1 ")" | Id Id type a_1
        //  1  |  type -> Id                      | Id
        //  2  |  a_1 -> "," Id ":" type a_1      | a_1 Id type
        //  3  |  a_1 -> ε                        | a_1
        //
        // - c_alt = 2 is child_*, pattern in `items` = [Id type] (a_1 is skipped),
        //   - pattern_len = 2
        //   - find NT in group that's not a_1 and that has a_1 in one of its alt
        //     => p_var = a, p_alt_id = 0
        //   - find position of a_1 in p_alt; check if there's something on the left (reject if not)
        //     and takes the position of the left symbol:

        //          0   1  2   3    4          0  1   2  3   4    5
        //       2: "," Id ":" type a_1     0: Id "(" Id ":" type a_1 ")"
        //                     ^^^^ c_pos (init) = 3         ^^^^ p_pos0 = p_pos (init) = 4
        //
        //   - counts how many symbols match on the left, and
        //     for each symbol that has a value, pops one from pattern,
        //     until pattern is empty of c_pos/p_pos is 0
        //
        //          0   1  2   3    4          0  1   2  3   4    5
        //       2: "," Id ":" type a_1     0: Id "(" Id ":" type a_1 ")"
        //          !=  <----------               !=  <---------- span_nbr = 3 (Id ":" type)
        //
        //   - if pattern is empty, we have a potential match, but we must still verify if
        //     all the parents alternatives that use a_1 also include the pattern. For example,
        //     `a -> Id? ("," Id)*` would be transformed as `a -> Id ("," Id)* | ("," Id)*`, so
        //     we can't apply sep_list because the child* shouldn't always expect a first Id
        //     on the stack.
        //     - find all the positions of a_1 in self.gather(a)
        //     - check that, for each of the a_1 found, the pattern precedes it: Id [Id type] a_1 => OK
        //     - for each p_alt and postion (here, p_alt = 0 and pos = 3),
        //       - remove [pos - pattern_len..pos] from items[p_alt] -> [3 - 2..3] = [1..3] => [Id a_1] is left
        const VERBOSE: bool = false;
        if VERBOSE {
            let log = std::mem::take(&mut self.log);
            self.item_ops = items.iter().cloned().to_vec();
            self.log_nt_info();
            self.log_alt_info();
            println!("{}", self.log);
            self.item_ops.clear();
            self.log = log;
        }
        self.log.add_note("- determining sep_list nonterminals...");
        if VERBOSE { println!("check_sep_list:"); }
        // takes one group at a time
        for (top_nt, g) in self.nt_parent.iter().enumerate().filter(|va| !va.1.is_empty()) {
            // takes the potential child_*
            let candidate_children = g.iter()
                .filter_map(|&var| {
                    let alts = &self.var_alts[var as usize];
                    let flags = self.flags[var as usize];
                    // takes only len() == 2 to reject complex cases like a -> A B C (B C | D)*
                    if alts.len() == 2 && flags & (ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS) == ruleflag::CHILD_REPEAT {
                        Some((var, alts[0] as usize, flags))
                    } else {
                        None
                    }
                })
                .to_vec();  // to avoid borrow checker issue with &mut self later
            for &(c_var, c_alt_id, _c_flags) in &candidate_children {
                let has_value = self.nt_values[c_var as usize];
                let skip_loop_nt = if has_value { 1 } else { 0 }; // the loop NT that's put in front when it has a value
                let mut pattern = items[c_alt_id].iter().skip(skip_loop_nt).cloned().to_vec();
                if VERBOSE {
                    println!(
                        "? {} {c_alt_id}: pattern = {}",
                        Symbol::NT(c_var).to_str(self.get_symbol_table()),
                        pattern.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
                if !pattern.is_empty() {
                    let pattern_len = pattern.len();
                    let pattern_copy = pattern.clone();
                    let c_sym = Symbol::NT(c_var);
                    // finds the parent's alt that includes this child_+*
                    let (p_var, _p_alt_id, p_alt, mut p_pos) = self.nt_parent[top_nt].iter()
                        .flat_map(|&p_var| &self.var_alts[p_var as usize])
                        .filter_map(|&p_alt_id| {
                            let (p_var, p_alt) = &self.alts[p_alt_id as usize];
                            if *p_var != c_var {
                                p_alt.v.iter().position(|s| s == &c_sym).map(|p_pos| (*p_var, p_alt_id as usize, p_alt, p_pos))
                            } else {
                                None
                            }
                        })
                        .next()
                        .unwrap_or_else(|| panic!("NT {c_var} alt {c_alt_id} should have a parent's alt that includes it"));
                    if p_pos > 0 {
                        // verifies if enough symbols match
                        p_pos -= 1; // easier to skip the child nonterminal, since it may or may not have a value
                        let c_alt = &self.alts[c_alt_id].1.v;
                        let mut c_pos = c_alt.len() - 2; // safe: there's at least another symbol before `c_sym` (per design)
                        let p_pos0 = p_pos;
                        let mut span_nbr = 0;
                        while !pattern.is_empty() {
                            if p_alt[p_pos] == c_alt[c_pos] {
                                span_nbr += 1;
                                if self.sym_has_value(&c_alt[c_pos]) {
                                    pattern.pop();
                                }
                                if c_pos == 0 || p_pos == 0 {
                                    break;
                                }
                                c_pos -= 1;
                                p_pos -= 1;
                            } else {
                                break;
                            }
                        }
                        if pattern.is_empty() {
                            let exit_alts = self.gather_alts(p_var);
                            // check that all the items that include c_var have the whole pattern to avoid cases like
                            // a -> V? ("," V) => a -> V ("," V) | ("," V) => a -> V a_1 | a_1
                            // where one branch doesn't have the initial V
                            // let mut whole_pattern = items[c_alt_id].iter().skip(skip_loop_nt).cloned().to_vec();
                            // whole_pattern.push(c_sym);
                            let mut found_pos = vec![];
                            let all_match = exit_alts.into_iter().all(|a| {
                                let a_items = &items[a as usize];
                                if let Some(p) = a_items.iter().position(|s| *s == c_sym) {
                                    // c_var is in items, but does it have the pattern too?
                                    if p >= pattern_len && a_items[p - pattern_len..p] == pattern_copy {
                                        found_pos.push((a as usize, p));
                                        true
                                    } else {
                                        // c_var is there, but the pattern isn't, we can't do the modification for this child*
                                        false
                                    }
                                } else {
                                    true
                                }
                            });
                            if all_match {
                                if VERBOSE {
                                    println!("- match:");
                                    println!("  c[{c_alt_id}]: {}    items: {}",
                                             c_alt.iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" "),
                                             items[c_alt_id].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" "));
                                }
                                for (p_alt_id, pos) in found_pos {
                                    if VERBOSE {
                                        println!("  p[{p_alt_id}]: {}    items: {}",
                                                 p_alt.iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" "),
                                                 items[p_alt_id].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" "));
                                        println!(
                                            "    c_alt_id = {c_alt_id}, p_alt_id = {p_alt_id}, p_pos0 = {p_pos0}, span_nbr = {span_nbr}, pos = {pos} => remove  [{}..{}]",
                                            pos - pattern_len, pos);
                                    }
                                    self.span_nbrs[p_alt_id] -= span_nbr as SpanNbr;
                                    self.span_nbrs_sep_list.insert(c_alt_id as AltId, span_nbr as SpanNbr);
                                    items[p_alt_id].drain(pos - pattern_len..pos);
                                    if VERBOSE {
                                        println!("    => p items: {}", items[p_alt_id].iter().map(|s| s.to_str_quote(self.get_symbol_table())).join(" "));
                                    }
                                    self.flags[c_var as usize] |= ruleflag::SEP_LIST;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn sort_alt_ids(&self, top_nt: VarId, alts: &[AltId]) -> Vec<AltId> {
        const VERBOSE: bool = false;
        if VERBOSE {
            println!("  sorting {} alts {alts:?}", Symbol::NT(top_nt).to_str(self.get_symbol_table()));
            for &a_id in alts {
                let &(_nt, ref alt) = &self.alts[a_id as usize];
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
        let mut ids = alts.iter().filter_map(|&alt_id| self.alts[alt_id as usize].1.origin.map(|(_var, id)| (id, alt_id)))
            .collect::<HashMap<_, _>>();
        let tree = &self.origin.trees[top_nt as usize];
        for node in tree.iter_post_depth() {
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
    ///   are changed if they're Rust identifiers)
    /// * `alt_info[alt]: Vec<Option<(VarId, String)>>` contains the enum variant names for each context (must be regrouped by VarId)
    /// * `item_info[alt]: Vec<ItemInfo>` contains the data available on the stacks when exiting the alternative
    /// * `child_repeat_endpoints[var]: HashMap<VarId, Vec<AltId>>` list of alts (several when the repeat child has several outcomes,
    ///   as in `a -> (A | B)+`), where each alt corresponds to the item_ops with the values on the stack.
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
    fn get_type_info(&mut self) {
        const VERBOSE: bool = false;

        self.log.add_note("- determining item_info...");
        let mut nt_upper_fixer = NameFixer::new();
        let mut nt_lower_fixer = NameFixer::new();
        let mut nt_plower_fixer = NameFixer::new_empty(); // prefixed lowercase: don't worry about reserved words
        let nt_name: Vec<(String, String, String)> = (0..self.num_nt).map(|v| {
            let name = self.symbol_table.get_nt_name(v as VarId);
            let nu = nt_upper_fixer.get_unique_name(name.to_camelcase());
            let nl = nt_lower_fixer.get_unique_name(nu.to_underscore_lowercase());
            let npl = nt_plower_fixer.get_unique_name(nu.to_underscore_lowercase());
            (nu, nl, npl)
        }).to_vec();

        let mut alt_info: Vec<Option<(VarId, String)>> = vec![None; self.alts.len()];
        let mut nt_repeat = HashMap::<VarId, Vec<ItemInfo>>::new();
        let mut item_info: Vec<Vec<ItemInfo>> = vec![vec![]; self.alts.len()];
        let mut child_repeat_endpoints = HashMap::<VarId, Vec<AltId>>::new();
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let is_ambig = self.nt_has_any_flags(group[0], ruleflag::PARENT_AMBIGUITY);
            let mut is_ambig_1st_child = is_ambig;
            let mut alt_info_to_sort = HashMap::<VarId, Vec<AltId>>::new();
            for var in group {
                let nt = *var as usize;
                let nt_flags = self.flags[nt];
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
                        endpoints.retain(|e| !self.alts[*e as usize].1.is_sym_empty());
                    }
                    assert!(!endpoints.is_empty());
                    let endpoints = self.sort_alt_ids(group[0], &endpoints);
                    child_repeat_endpoints.insert(*var, endpoints);
                }
                for &alt_id in &self.var_alts[nt] {
                    let i = alt_id as usize;
                    if is_ambig_1st_child && self.alts[i].1.is_sym_empty() {
                        continue;
                    }
                    let item_ops = &self.item_ops[alt_id as usize];
                    // Adds a suffix to the names of different symbols that would otherwise collide in the same context option:
                    // - identical symbols are put in a vector (e.g. `id: [String; 2]`)
                    // - different symbols, which means T vs NT, must have different names (e.g. `NT(A)` becomes "a",
                    //   `T(a)` becomes "a", too => one is renamed to "a1" to avoid the collision: `{ a: SynA, a1: String }`)
                    let mut indices = HashMap::<Symbol, (String, Option<usize>)>::new();
                    let mut fixer = NameFixer::new();
                    let mut owner = self.alts[i].0;
                    while let Some(parent) = self.parent[owner as usize] {
                        if self.flags[owner as usize] & ruleflag::CHILD_REPEAT != 0 {
                            // a child + * is owner
                            // - if <L>, it has its own public context and a user-defined return type
                            // - if not <L>, it has no context and a generator-defined return type (like Vec<String>)
                            // (we keep the loop for +, which has a left factorization, too)
                            break;
                        }
                        owner = parent;
                    }
                    let is_nt_child_repeat = self.flags[owner as usize] & ruleflag::CHILD_REPEAT != 0;
                    for s in item_ops {
                        if let Some((_, c)) = indices.get_mut(s) {
                            *c = Some(0);
                        } else {
                            let name = if let Symbol::NT(vs) = s {
                                let flag = self.flags[*vs as usize];
                                if flag & ruleflag::CHILD_REPEAT != 0 {
                                    let inside_alt_id = self.var_alts[*vs as usize][0];
                                    let inside_alt = &self.alts[inside_alt_id as usize].1;
                                    if false {
                                        // we don't use this any more
                                        let mut plus_name = inside_alt.symbols()[0].to_str(self.get_symbol_table()).to_underscore_lowercase();
                                        plus_name.push_str(if flag & ruleflag::REPEAT_PLUS != 0 { "_plus" } else { "_star" });
                                        plus_name
                                    } else if is_nt_child_repeat && indices.is_empty() {
                                        // iterator variable in a + * loop (visible with <L>, for ex)
                                        if flag & ruleflag::REPEAT_PLUS != 0 { "plus_acc".to_string() } else { "star_acc".to_string() }
                                    } else {
                                        // reference to a + * result
                                        if flag & ruleflag::REPEAT_PLUS != 0 { "plus".to_string() } else { "star".to_string() }
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
                        self.alts[i].1.symbols().iter().any(|s| matches!(s, &Symbol::NT(c) if self.flags[c as usize] & ruleflag::CHILD_L_FACT != 0));

                    // (α)* doesn't call the listener for each α, unless it's l-form. We say it's a hidden child_repeat, and it doesn't need a context.
                    // The only children a child_repeat can have is due to left factorization in (α)+, so we check `owner` rather than `nt`.
                    let is_hidden_repeat_child = self.flags[owner as usize] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT;

                    // <alt> -> ε
                    let is_alt_sym_empty = self.is_alt_sym_empty(alt_id);

                    // (α <L>)+ have two similar alternatives with the same data on the stack, one that loops and the last iteration. We only
                    // keep one context because we use a flag to tell the listener when it's the last iteration (more convenient).
                    let is_duplicate = i > 0 && self.nt_has_all_flags(owner, ruleflag::CHILD_REPEAT | ruleflag::REPEAT_PLUS | ruleflag::L_FORM) &&
                        is_alt_sym_empty;

                    let is_last_empty_iteration = (nt_flags & ruleflag::CHILD_L_RECURSION != 0
                        || self.nt_has_all_flags(*var, ruleflag::CHILD_REPEAT | ruleflag::L_FORM)) && is_alt_sym_empty;

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
                    let has_owner_value = self.nt_values[owner as usize];
                    item_info[i] = if item_ops.is_empty() && nt_flags & ruleflag::CHILD_L_RECURSION != 0 {
                        // we put here the return context for the final exit of left recursive rule
                        if has_owner_value {
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
                        let is_rrec_lform = self.nt_has_all_flags(owner, ruleflag::R_RECURSION | ruleflag::L_FORM);
                        let skip = if (is_nt_child_repeat || is_rrec_lform) && has_owner_value { 1 } else { 0 };
                        let mut infos = item_ops.iter()
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
                                    name: indices[s].0.clone(),
                                    sym: *s,
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
                        if is_nt_child_repeat && !infos.is_empty() && !nt_repeat.contains_key(&owner) {
                            nt_repeat.insert(owner, infos.clone());
                        }
                        infos
                    }
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
        self.nt_name = nt_name;
        self.alt_info = alt_info;
        self.item_info = item_info;
        self.child_repeat_endpoints = child_repeat_endpoints;
    }

    /// Generates the source code of the wrapper and the accompanying templates. Returns
    /// * the indented source of the wrapper
    /// * the indented source of the template for the user types
    /// * the indented source of the template for the listener implementation
    pub fn gen_source_code(&mut self) -> (String, String, String) {
        self.log.add_note("generating source code...");
        if !self.log.has_no_errors() {
            return (String::new(), String::new(), String::new());
        }
        // Building the source code as we do below is not the most efficient, but it's done that way to
        // - be able to build only a part of the parser, and
        // - get the sources for the validation tests or print them / write them into a file.
        // The whole code isn't that big, so it's not a major issue.
        let mut parts = vec![];
        if !self.options.headers.is_empty() {
            parts.push(self.options.headers.clone());
        }
        let mut tmp_parts = if self.gen_parser {
            vec![self.source_build_parser()]
        } else {
            vec![]
        };
        let (src_types, src_listener) = if self.options.gen_wrapper {
            self.calc_item_ops();
            let (src_wrapper, src_types, src_listener) = self.source_wrapper();
            tmp_parts.push(src_wrapper);
            (
                indent_source(vec![src_types], self.options.types_indent),
                indent_source(vec![src_listener], self.options.listener_indent)
            )
        } else {
            (String::new(), String::new())
        };
        self.log_nt_info();
        self.log_alt_info();
        parts.push(self.source_use());
        parts.extend(tmp_parts);
        // Create source code:
        (indent_source(parts, self.options.indent), src_types, src_listener)
    }

    pub fn try_gen_source_code(mut self) -> Result<(BufLog, String, String, String), BuildError> {
        let (src, src_types, src_listener) = self.gen_source_code();
        if self.log.has_no_errors() {
            Ok((self.give_log(), src, src_types, src_listener))
        } else {
            Err(BuildError::new(self.give_log(), BuildErrorSource::ParserGen))
        }
    }

    fn source_use(&self) -> Vec<String> {
        self.options.used_libs.gen_source_code()
    }

    fn source_build_parser(&mut self) -> Vec<String> {
        match self.options.parser_type {
            ParserType::LL1 => self.source_build_parser_ll1(),
            ParserType::LALR => self.source_build_parser_lalr(),
        }
    }

    fn source_build_parser_ll1(&mut self) -> Vec<String> {
        static BASE_PARSER_LIBS: [&str; 5] = [
            "::VarId",
            "::AltId",
            "::parser::OpCode",
            "::parser::LLParser",
            "::fixed_sym_table::FixedSymTable",
        ];
        static ALT_PARSER_LIBS: [&str; 2] = [
            "::alt::Alternative",
            "::parser::Symbol",
        ];

        self.log.add_note("generating LL1 build_parser source...");
        let num_nt = self.symbol_table.get_num_nt();
        let num_t = self.symbol_table.get_num_t();
        self.options.used_libs.extend(BASE_PARSER_LIBS.into_iter().map(|s| format!("{}{s}", self.options.lib_crate)));
        self.log.add_note(format!("- creating symbol tables: {num_t} terminals, {num_nt} nonterminals"));
        let mut src = vec![
            format!("const PARSER_NUM_T: usize = {num_t};"),
            format!("const PARSER_NUM_NT: usize = {num_nt};"),
            format!("static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [{}];",
                     self.symbol_table.get_terminals().map(|(s, os)|
                         format!("(\"{s}\", {})", os.as_ref().map(|s| format!("Some({s:?})")).unwrap_or("None".to_string()))).join(", ")),
            format!("static SYMBOLS_NT: [&str; PARSER_NUM_NT] = [{}];",
                     self.symbol_table.get_nonterminals().map(|s| format!("{s:?}")).join(", ")),
            format!("static ALT_VAR: [VarId; {}] = [{}];",
                    self.alts.len(),
                    self.alts.iter().map(|(v, _)| format!("{v}")).join(", ")),
        ];
        if self.options.include_alts {
            self.options.used_libs.extend(ALT_PARSER_LIBS.into_iter().map(|s| format!("{}{s}", self.options.lib_crate)));
            src.push(format!("static ALTERNATIVES: [&[Symbol]; {}] = [{}];",
                             self.alts.len(),
                             self.alts.iter().map(|(_, f)| format!("&[{}]", f.iter().map(symbol_to_code).join(", "))).join(", ")));
        }
        self.log.add_note(format!("- creating parsing tables: {} items, {} opcodes", self.table.len(), self.opcodes.len()));
        src.extend(vec![
            format!(
                "static PARSING_TABLE: [AltId; {}] = [{}];",
                self.table.len(),
                self.table.iter().map(|v| format!("{v}")).join(", ")),
            format!(
                "static OPCODES: [&[OpCode]; {}] = [{}];",
                self.opcodes.len(),
                self.opcodes.iter().map(|strip| format!("&[{}]", strip.iter().map(|op| format!("OpCode::{op:?}")).join(", "))).join(", ")),
            format!(
                "static INIT_OPCODES: [OpCode; {}] = [{}];",
                self.init_opcodes.len(),
                self.init_opcodes.iter().map(|op| format!("OpCode::{op:?}")).join(", ")),
            format!("static START_SYMBOL: VarId = {};\n", self.start),
        ]);
        if self.options.gen_token_enums {
            src.add_space();
            src.push("#[derive(Clone, Copy, PartialEq, Debug)]".to_string());
            src.push("#[repr(u16)]".to_string());
            src.push("pub enum Term {".to_string());
            let cols = self.symbol_table.get_terminals().enumerate()
                .map(|(t, (s, s_opt))| vec![
                    // format!("    #[doc=\"{:?}\"]", if let Some(so) = s_opt { format!("{so:?}") } else { String::new() }),
                    // if let Some(so) = s_opt { format!("    #[doc = \"'{so}'\"]") } else { String::new() },
                    format!("    #[doc = \"{}\"]", if let Some(so) = s_opt { format!("'{so}'") } else { "(variable)".to_string() }),
                    format!("{s} = {t},", )])
                .to_vec();
            src.extend(columns_to_str(cols, Some(vec![16, 0])));
            src.push("}\n".to_string());
            src.push("#[derive(Clone, Copy, PartialEq, Debug)]".to_string());
            src.push("#[repr(u16)]".to_string());
            src.push("pub enum NTerm {".to_string());
            let cols = self.symbol_table.get_nonterminals().index()
                .map(|(t, s)| vec![
                    format!(
                        "    #[doc = \"`{s}`{}\"]",
                        if let Some(p) = self.get_nt_parent(t) {
                            format!(", parent: `{}`", Symbol::NT(p).to_str(self.get_symbol_table()))
                        } else {
                            String::new()
                        }),
                    format!("{} = {t},", s.to_camelcase())])
                .to_vec();
            src.extend(columns_to_str(cols, Some(vec![16, 0])));
            src.push("}\n".to_string());
            src.push("pub fn get_term_name(t: TokenId) -> (&'static str, Option<&'static str>) {".to_string());
            src.push("    SYMBOLS_T[t as usize]".to_string());
            src.push("}\n".to_string());
        }
        src.extend(vec![
            "pub fn build_parser() -> LLParser<'static> {{".to_string(),
            "    let symbol_table = FixedSymTable::new(".to_string(),
            "        SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),".to_string(),
            "        SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()".to_string(),
            "    );".to_string(),
            "    LLParser::new(".to_string(),
            "        PARSER_NUM_NT, PARSER_NUM_T + 1,".to_string(),
            "        &ALT_VAR,".to_string(),
            if self.options.include_alts {
                "        ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),".to_string()
            } else {
                "        Vec::new(),".to_string()
            },
            "        OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),".to_string(),
            "        INIT_OPCODES.to_vec(),".to_string(),
            "        &PARSING_TABLE,".to_string(),
            "        symbol_table,".to_string(),
            "        START_SYMBOL".to_string(),
            "    )".to_string(),
            "}}".to_string(),
        ]);
        src
    }

    fn source_build_parser_lalr(&mut self) -> Vec<String> {
        static BASE_PARSER_LIBS: [&str; 6] = [
            "::VarId",
            "::LALR",
            "::fixed_sym_table::FixedSymTable",
            "::parser::lr_parser::LRAction",
            "::parser::lr_parser::LRParser",
            "::parser::lr_parser::LRStateId",
        ];
        self.log.add_note("generating LALR build_parser source...");
        let num_nt_table = self.symbol_table.get_num_nt();
        let num_t_table = self.symbol_table.get_num_t(); // includes <$> and <empty>
        self.options.used_libs.extend(BASE_PARSER_LIBS.into_iter().map(|s| format!("{}{s}", self.options.lib_crate)));
        self.log.add_note(format!(
            "- creating parsor tables: {num_t_table} terminals (including $ and empty), {num_nt_table} nonterminals, {} actions, {} gotos, {} productions",
            self.action.len(), self.goto.len(), self.alts.len()));
        let alt_nt_len = alts_to_alt_nt_len(&self.alts, &self.symbol_table);
        vec![
            format!("static NUM_NT: usize = {};", self.num_nt),
            format!("static NUM_T_FULL: usize = {};", self.num_t_full),
            format!("static ACTION: [LRAction; {}] = [{}];", self.action.len(), self.action.iter().map(|a| format!("LRAction::{a:?}")).join(", ")),
            format!("static GOTO: [LRStateId; {}] = {:?};", self.goto.len(), self.goto),
            format!("static ALT_NT_LEN: [(VarId, u16, u16); {}] = {:?};", alt_nt_len.len(), alt_nt_len),
            format!("static SYMBOL_TABLE_T: [(&str, Option<&str>); {num_t_table}] = {:?};", self.symbol_table.get_terminals().to_vec()),
            format!("static SYMBOL_TABLE_NT: [&str; {num_nt_table}] = {:?};", self.symbol_table.get_nonterminals().to_vec()),
            String::new(),
            "pub fn build_parser(n: u32) -> LRParser<'static, LALR> {".to_string(),
            "    LRParser::new(".to_string(),
            "        NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,".to_string(),
            "        FixedSymTable::new(".to_string(),
            "            SYMBOL_TABLE_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),".to_string(),
            "            SYMBOL_TABLE_NT.into_iter().map(|s| s.to_string()).collect()".to_string(),
            "        )".to_string(),
            "    )".to_string(),
            "}".to_string(),
        ]
    }

    fn get_info_type(&self, infos: &[ItemInfo], info: &ItemInfo) -> String {
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
    fn source_infos(&self, infos: &[ItemInfo], add_pub: bool, add_type: bool) -> String {
        let pub_str = if add_pub { "pub " } else { "" };
        infos.iter()
            .filter_map(|info| {
                if info.index.is_none() || info.index == Some(0) {
                    let type_name = if add_type {
                        format!(": {}", self.get_info_type(infos, info))
                    } else {
                        String::new()
                    };
                    Some(format!("{pub_str}{}{type_name}", info.name))
                } else {
                    None
                }
        }).join(", ")
    }

    fn is_alt_sym_empty(&self, a_id: AltId) -> bool {
        self.alts[a_id as usize].1.is_sym_empty()
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
        if self.options.gen_span_params {
            let span_code = span_only();
            format!("({span_code}, {common})")
        } else {
            common
        }
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

    fn get_var_params(item_info: &[ItemInfo], skip: usize, indices: &HashMap<Symbol, Vec<String>>, non_indices: &mut Vec<String>) -> String {
        item_info.iter().skip(skip).filter_map(|item| {
            Self::get_var_param(item, indices, non_indices)
        }).join(", ")
    }

    fn source_lets(infos: &[ItemInfo], nt_name: &[(String, String, String)], indent: &str, last_alt_id_maybe: Option<AltId>) -> (Vec<String>, String) {
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
            if item.sym.is_empty() {
                src_let.push(format!("{indent}let {varname} = alt_id == {};", last_alt_id_maybe.unwrap()));
            } else if let Symbol::NT(v) = item.sym {
                src_let.push(format!("{indent}let {varname} = self.stack.pop().unwrap().get_{}();", nt_name[v as usize].2));
            } else {
                src_let.push(format!("{indent}let {varname} = self.stack_t.pop().unwrap();"));
            }
        }
        let src_struct = Self::get_var_params(infos, 0, &indices, &mut non_indices);
        (src_let, src_struct)
    }

    fn source_update_span(n: &str) -> Vec<String> {
        vec![
            format!("        let spans = self.stack_span.drain(self.stack_span.len() - {n} ..).collect::<Vec<_>>();"),
            "        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));".to_string(),
        ]
    }

    /// Source code of the first part of exit_* method for children of *+,
    /// which takes the values of the NT and T items from the stack at each iteration.
    /// The same code is generated for init_* methods in the case of token-separated items,
    /// when the accumulator is initialized with the first items.
    fn source_child_repeat_lets(
        &self,
        endpoints: &[AltId],
        item_info: &[Vec<ItemInfo>],
        is_plus: bool,
        nt_name: &[(String, String, String)],
        fn_name: &str,
        nu: &str,
        is_init: bool,
    ) -> (Vec<String>, String)
    {
        // ex:
        //     let type1 = self.stack.pop().unwrap().get_type();
        //     let id = self.stack_t.pop().unwrap();
        //     let val = SynA1Item { id, type1 };
        //     let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
        //     self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        //
        let mut src_val = vec![];
        let val_name = if endpoints.len() > 1 {
            // several possibilities; for ex. a -> (A | B)+  => Vec of enum type
            src_val.push(format!("        let {} = match alt_id {{", self.gen_match_item("val".to_string(), || "n".to_string())));
            for (i, &a_id) in endpoints.iter().index_start(1) {
                let infos = &item_info[a_id as usize];
                src_val.push(format!("            {a_id}{} => {{", if is_plus { format!(" | {}", a_id + 1) } else { String::new() }));
                let (src_let, src_struct) = Self::source_lets(infos, nt_name, "                ", None);
                src_val.extend(src_let);
                let return_value = self.gen_match_item(
                    format!("Syn{nu}Item::V{i} {{ {} }}", src_struct),
                    || self.span_nbrs[a_id as usize].to_string());
                src_val.push(format!("                {return_value}"));
                src_val.push("            }".to_string());
            }
            src_val.push(format!("            _ => panic!(\"unexpected alt id {{alt_id}} in fn {fn_name}\"),"));
            src_val.push("        };".to_string());
            if self.options.gen_span_params {
                src_val.extend(Self::source_update_span("n"));
            }
            "val".to_string()
        } else {
            // single possibility; for ex. a -> (A B)+  => struct
            let a_id = endpoints[0];
            if self.options.gen_span_params {
                let span_nbr = if is_init {
                    *self.span_nbrs_sep_list.get(&a_id).unwrap()
                } else {
                    self.span_nbrs[a_id as usize]
                };
                src_val.extend(Self::source_update_span(&span_nbr.to_string()));
            }
            let infos = &item_info[a_id as usize];
            let (src_let, src_struct) = Self::source_lets(infos, nt_name, "        ", None);
            src_val.extend(src_let);
            if infos.len() == 1 {
                // single repeat item; for ex. A -> B+  => type directly as Vec<type>
                infos[0].name.clone()
            } else {
                // several repeat items; for ex. A -> (B b)+  => intermediate struct type for Vec
                src_val.push(format!("        let val = Syn{nu}Item {{ {} }};", src_struct));
                "val".to_string()
            }
        };
        (src_val, val_name)
    }

    /// Generates the wrapper source code and returns
    /// * the wrapper source code
    /// * the template for the user-defined types (Syn*)
    /// * the template for the listener implementation
    fn source_wrapper(&mut self) -> (Vec<String>, Vec<String>, Vec<String>) {
        const VERBOSE: bool = false;
        const MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS: bool = false;

        static PARSER_LIBS: [&str; 9] = [
            "::VarId", "::parser::Call", "::parser::ListenerWrapper",
            "::AltId", "::log::Logger", "::TokenId", "::lexer::PosSpan",
            "::parser::Terminate", "::log::LogMsg"
        ];

        self.log.add_note("generating wrapper source...");
        self.options.used_libs.extend(PARSER_LIBS.into_iter().map(|s| format!("{}{s}", self.options.lib_crate)));

        self.get_type_info();

        // defines missing type names
        for (v, name) in self.nt_name.iter().enumerate().filter(|(v, _)| self.nt_values[*v]) {
            let v = v as VarId;
            self.nt_type.entry(v).or_insert_with(|| format!("Syn{}", name.0));
        }

        let mut src = vec![];

        // writes contexts
        let mut nt_contexts = self.source_wrapper_ctx::<VERBOSE>(&mut src);

        // writes intermediate Syn types
        let (src_types, syns) = self.source_wrapper_types::<VERBOSE>(&mut src);

        // prepares the data for the following sections
        let mut exit_fixer = NameFixer::new();
        let mut span_init = HashSet::<VarId>::new();
        let src_skel = vec![
            format!("// {:-<80}", ""),
            format!("// Template for the user implementation of {}Listener", self.name),
            String::new(),
            "struct Listener {".to_string(),
            "    log: BufLog,".to_string(),
            "}".to_string(),
            String::new(),
            "#[allow(unused)]".to_string(),
            format!("impl {}Listener for Listener {{", self.name),
            "    fn get_log_mut(&mut self) -> &mut impl Logger {".to_string(),
            "        &mut self.log".to_string(),
            "    }".to_string(),
            String::new(),
        ];
        let mut sources = WrapperSources {
            src,
            src_listener_decl: vec![],
            src_skel,
            src_types,
            src_init: vec![],
            src_exit: vec![],
            src_wrapper_impl: vec![],
        };

        // we proceed by var parent, then all alts in each parent/children group
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let parent_nt = group[0] as usize;
            let parent_flags = self.flags[parent_nt];
            let parent_has_value = self.nt_values[parent_nt];
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
                .filter_map(|f| self.alts[f as usize].1.get_ambig_alt_id().map(|id| (id, f)))
            {
                ambig_op_alts.entry(id).or_default().push(f);
            }
            if VERBOSE && is_ambig {
                println!("- ambig children vars: {}", ambig_children.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "));
                println!("  ambig op alts: {ambig_op_alts:?}");
            }

            // contexts used in source_wrapper_init and source_wrapper_exit methods:
            let in_ctx = SourceInputContext {
                parent_has_value,
                parent_nt,
                syns: &syns,
                ambig_op_alts: &ambig_op_alts,
            };
            let mut state = SourceState {
                init_nt_done: &mut init_nt_done,
                span_init: &mut span_init,
                nt_contexts: &mut nt_contexts,
                exit_alt_done: &mut exit_alt_done,
                exit_fixer: &mut exit_fixer,
            };

            for var in group {
                let nt = *var as usize;
                let flags = self.flags[nt];
                // the parents of left recursion are not useful in ambiguous rules (they just push / pop the same value):
                let is_ambig_1st_child =  is_ambig && flags & ruleflag::CHILD_L_RECURSION != 0 && ambig_children.first() == Some(var);
                // we only process the first variable of the left recursion; below we gather the alts of
                // the other variables of the same type (in ambiguous rules, they repeat the same operators)
                let is_ambig_redundant = is_ambig && flags & ruleflag::L_RECURSION != 0 && !is_ambig_1st_child;
                let has_value = self.nt_values[nt];

                // Call::Enter
                self.source_wrapper_init::<VERBOSE>(
                    &in_ctx,
                    *var,
                    flags,
                    has_value,
                    is_ambig_1st_child,
                    &mut state,
                    &mut sources
                );

                // Call::Exit
                self.source_wrapper_exit::<VERBOSE>(
                    &in_ctx,
                    *var,
                    flags,
                    has_value,
                    is_ambig_1st_child,
                    is_ambig_redundant,
                    &mut state,
                    &mut sources
                );
            }

            // completes the unused nt entries in the match's Call::Exit => { ... }
            for a in group.iter().flat_map(|v| &self.var_alts[*v as usize]).filter(|a| !exit_alt_done.contains(a)) {
                let is_called = self.opcodes[*a as usize].contains(&OpCode::Exit(*a));
                let (v, alt) = &self.alts[*a as usize];
                let alt_str = if MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS {
                    self.full_alt_str(*a, None, false)
                } else {
                    alt.to_rule_str(*v, self.get_symbol_table(), self.flags[*v as usize])
                };
                let comment = format!("// {alt_str} ({})", if is_called { "not used" } else { "never called" });
                if is_called {
                    sources.src_exit.push(vec![format!("                    {a} => {{}}"), comment]);
                } else {
                    sources.src_exit.push(vec![format!("                 /* {a} */"), comment]);
                }
            }
            // adds unused init calls, using Segments to regroup them
            let mut seg_init = Segments::from_iter(
                group.iter()
                    .filter_map(|&v| if !init_nt_done.contains(&v) { Some(Seg(v as u32, v as u32)) } else { None })
            );
            seg_init.normalize();
            for seg in seg_init {
                let Seg(a, b) = seg;
                if a == b {
                    sources.src_init.push(vec![format!("                    {a} => {{}}"), format!("// {}", Symbol::NT(a as VarId).to_str(self.get_symbol_table()))]);
                } else {
                    sources.src_init.push(vec![
                        format!("                    {a}{}{b} => {{}}", if b == a + 1 { " | " } else { " ..= " }),
                        format!("// {}", (a..=b).map(|v| Symbol::NT(v as VarId).to_str(self.get_symbol_table())).join(", "))
                    ]);
                }
            }
        }

        self.source_wrapper_finalize(span_init, sources)
    }

    /// Adds the wrapper source code related to the Ctx types
    fn source_wrapper_ctx<const VERBOSE: bool>(&self, src: &mut Vec<String>) -> Vec<Option<Vec<AltId>>> {
        let mut nt_contexts: Vec<Option<Vec<AltId>>> = vec![None; self.num_nt];
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            let mut group_names = HashMap::<VarId, Vec<AltId>>::new();
            // fetches the NT that have alt data
            for nt in group {
                for &alt_id in &self.var_alts[*nt as usize] {
                    if let Some((owner, _name)) = &self.alt_info[alt_id as usize] {
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
                    let flags = self.flags[nt as usize];
                    if VERBOSE {
                        print!("- {}: flags {}", Symbol::NT(nt).to_str(self.get_symbol_table()), ruleflag::to_string(flags).join(" "));
                        if let Some(gn) = group_names.get(&nt) {
                            println!(", alts = {}", gn.iter().map(|a| a.to_string()).join(", "));
                            let sorted = self.sort_alt_ids(group[0], gn);
                            println!("     sorted alts: {sorted:?}");
                        } else {
                            println!();
                        }
                    }
                    if flags & (ruleflag::SEP_LIST | ruleflag::L_FORM) == ruleflag::SEP_LIST | ruleflag::L_FORM {
                        src.push("#[derive(Debug)]".to_string());
                        src.push(format!("pub enum InitCtx{} {{", self.nt_name[nt as usize].0));
                        let a_id = self.var_alts[nt as usize][0];
                        let comment = format!(
                            "value of `{}` before {}",
                            self.item_ops[a_id as usize][1..].iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                            self.full_alt_components(a_id, None).1
                        );
                        let ctx_content = self.source_infos(&self.item_info[a_id as usize], false, true);
                        src.push(format!("    /// {comment}"));
                        let a_name = &self.alt_info[a_id as usize].as_ref().unwrap().1;
                        let ctx_item = if ctx_content.is_empty() {
                            if VERBOSE { println!("      {a_name},"); }
                            format!("    {a_name},", )
                        } else {
                            if VERBOSE { println!("      {a_name} {{ {ctx_content} }},"); }
                            format!("    {a_name} {{ {ctx_content} }},", )
                        };
                        src.push(ctx_item);
                        src.push("}".to_string());
                    }
                    src.push("#[derive(Debug)]".to_string());
                    src.push(format!("pub enum Ctx{} {{", self.nt_name[nt as usize].0));
                    if VERBOSE { println!("  context Ctx{}:", self.nt_name[nt as usize].0); }
                    let alts = self.sort_alt_ids(group[0], alts);
                    nt_contexts[nt as usize] = Some(alts.clone());
                    for a_id in alts {
                        let comment = self.full_alt_str(a_id, None, true);
                        src.push(format!("    /// {comment}"));
                        if VERBOSE { println!("      /// {comment}"); }
                        let ctx_content = self.source_infos(&self.item_info[a_id as usize], false, true);
                        let a_name = &self.alt_info[a_id as usize].as_ref().unwrap().1;
                        let ctx_item = if ctx_content.is_empty() {
                            if VERBOSE { println!("      {a_name},"); }
                            format!("    {a_name},", )
                        } else {
                            if VERBOSE { println!("      {a_name} {{ {ctx_content} }},"); }
                            format!("    {a_name} {{ {ctx_content} }},", )
                        };
                        src.push(ctx_item);
                    }
                    src.push("}".to_string());
                }
            }
        }
        nt_contexts
    }

    /// Adds the wrapper source code related to the user types
    fn source_wrapper_types<const VERBOSE: bool>(&self, src: &mut Vec<String>) -> (Vec<String>, Vec<VarId>) {
        static TYPE_DERIVE: &str = "#[derive(Debug, PartialEq)]";

        let mut src_types = vec![
            format!("// {:-<80}", ""),
            "// Template for the user-defined types:".to_string(),
        ];
        src.add_space();
        let mut syns = Vec::<VarId>::new(); // list of valuable NTs
        for (v, names) in self.nt_name.iter().enumerate().filter(|(v, _)| self.nt_values[*v]) {
            let v = v as VarId;
            let (nu, _nl, _npl) = names;
            let nt_type = self.get_nt_type(v);
            if self.nt_has_all_flags(v, ruleflag::CHILD_REPEAT) {
                let is_lform = self.nt_has_all_flags(v, ruleflag::L_FORM);
                let first_alt = self.var_alts[v as usize][0];
                let (t, var_oid) = self.origin.get(v).unwrap();
                if is_lform {
                    let astr = format!("/// User-defined type for {}", self.full_alt_str(first_alt, None, true));
                    src_types.push(String::new());
                    src_types.push(astr.clone());
                    src_types.push(TYPE_DERIVE.to_string());
                    src_types.push(format!("pub struct {}();", self.get_nt_type(v)));
                } else {
                    let top_parent = self.get_top_parent(v);
                    src.push(format!("/// Computed `{}` array in `{} -> {}`",
                                     grtree_to_str(t, Some(var_oid), None, Some(top_parent), self.get_symbol_table(), true),
                                     Symbol::NT(top_parent).to_str(self.get_symbol_table()),
                                     grtree_to_str(t, None, Some(var_oid), Some(top_parent), self.get_symbol_table(), true),
                    ));
                    let endpoints = self.child_repeat_endpoints.get(&v).unwrap();
                    if endpoints.len() > 1 {
                        // several possibilities; for ex. a -> (A | B)+  => Vec of enum type
                        src.push("#[derive(Debug, PartialEq)]".to_string());
                        src.push(format!("pub struct {nt_type}(pub Vec<Syn{nu}Item>);"));
                        src.push("#[derive(Debug, PartialEq)]".to_string());
                        src.push(format!("pub enum Syn{nu}Item {{"));
                        for (i, &a_id) in endpoints.iter().index_start(1) {
                            src.push(format!("    /// {}", self.full_alt_str(a_id, None, true)));
                            src.push(format!("    V{i} {{ {} }},", self.source_infos(&self.item_info[a_id as usize], false, true)));
                        }
                        src.push("}".to_string());
                    } else {
                        // single possibility; for ex. a -> (A B)+  => struct
                        let a_id = endpoints[0];
                        let infos = &self.item_info[a_id as usize];
                        if infos.len() == 1 {
                            // single repeat item; for ex. A -> B+  => type directly as Vec<type>
                            let type_name = self.get_info_type(infos, &infos[0]);
                            src.push("#[derive(Debug, PartialEq)]".to_string());
                            src.push(format!("pub struct {nt_type}(pub Vec<{type_name}>);", ));
                        } else {
                            // several repeat items; for ex. A -> (B b)+  => intermediate struct type for Vec
                            src.push("#[derive(Debug, PartialEq)]".to_string());
                            src.push(format!("pub struct {nt_type}(pub Vec<Syn{nu}Item>);"));
                            src.push(format!("/// {}", self.full_alt_str(first_alt, None, false)));
                            src.push("#[derive(Debug, PartialEq)]".to_string());
                            src.push(format!("pub struct Syn{nu}Item {{ {} }}", self.source_infos(infos, true, true)));
                        }
                    }
                }
            } else {
                src_types.push(String::new());
                src_types.push(format!("/// User-defined type for `{}`", Symbol::NT(v).to_str(self.get_symbol_table())));
                src_types.push(TYPE_DERIVE.to_string());
                src_types.push(format!("pub struct {}();", self.get_nt_type(v)));
            }
            syns.push(v);
        }
        if !self.nt_values[self.start as usize] {
            let nu = &self.nt_name[self.start as usize].0;
            src.push(format!("/// Top non-terminal {nu} (has no value)"));
            src.push("#[derive(Debug, PartialEq)]".to_string());
            src.push(format!("pub struct Syn{nu}();"))
        }

        // Writes EnumSynValue type and implementation
        if VERBOSE { println!("syns = {syns:?}"); }
        src.add_space();
        // EnumSynValue type
        src.push("#[derive(Debug)]".to_string());
        src.push(format!("enum EnumSynValue {{ {} }}",
                         syns.iter().map(|v| format!("{}({})", self.nt_name[*v as usize].0, self.get_nt_type(*v))).join(", ")));
        if !syns.is_empty() {
            // EnumSynValue getters
            src.add_space();
            src.push("impl EnumSynValue {".to_string());
            for v in &syns {
                let (nu, _, npl) = &self.nt_name[*v as usize];
                let nt_type = self.get_nt_type(*v);
                src.push(format!("    fn get_{npl}(self) -> {nt_type} {{"));
                if syns.len() == 1 {
                    src.push(format!("        let EnumSynValue::{nu}(val) = self;"));
                    src.push("        val".to_string());
                } else {
                    src.push(format!("        if let EnumSynValue::{nu}(val) = self {{ val }} else {{ panic!() }}"));
                }
                src.push("    }".to_string());
            }
            src.push("}".to_string());
        }
        (src_types, syns)
    }

    /// Adds the wrapper source code related to Call::Enter
    fn source_wrapper_init<const VERBOSE: bool>(
        &self,
        ctx                 : &SourceInputContext,
        var                 : VarId,
        flags               : u32,
        has_value           : bool,
        is_ambig_1st_child  : bool,
        state               : &mut SourceState,
        sources             : &mut WrapperSources
    ) {
        let &SourceInputContext { ambig_op_alts, .. } = ctx;
        let SourceState { init_nt_done, span_init, .. } = state;
        let WrapperSources { src_listener_decl, src_skel, src_init, src_wrapper_impl, .. } = sources;
        let nt = var as usize;
        let sym_nt = Symbol::NT(var);
        let nt_comment = format!("// {}", sym_nt.to_str(self.get_symbol_table()));
        let is_sep_list = flags & ruleflag::SEP_LIST != 0;
        let is_lform = flags & ruleflag::L_FORM != 0;
        let is_rrec_lform = is_lform && flags & ruleflag::R_RECURSION != 0;
        let is_plus = flags & ruleflag::REPEAT_PLUS != 0;
        let (nu, nl, npl) = &self.nt_name[nt];
        if VERBOSE { println!("  - VAR {}, has {}value, flags: {}",
                              sym_nt.to_str(self.get_symbol_table()),
                              if has_value { "" } else { "no " },
                              ruleflag::to_string(flags).join(" | ")); }

        let mut has_skel_init = false;
        let init_fn_name = format!("init_{npl}");
        if self.parent[nt].is_none() {
            init_nt_done.insert(var);
            if is_rrec_lform {
                span_init.insert(var);
            }
            if is_rrec_lform && has_value {
                src_wrapper_impl.push(String::new());
                src_listener_decl.push(format!("    fn {init_fn_name}(&mut self) -> {};", self.get_nt_type(nt as VarId)));
                src_skel.push(format!("    fn {init_fn_name}(&mut self) -> {} {{", self.get_nt_type(nt as VarId)));
                has_skel_init = true;
                src_init.push(vec![format!("                    {nt} => self.init_{nl}(),"), nt_comment]);
                src_wrapper_impl.push(format!("    fn {init_fn_name}(&mut self) {{"));
                src_wrapper_impl.push(format!("        let val = self.listener.init_{nl}();"));
                src_wrapper_impl.push(format!("        self.stack.push(EnumSynValue::{nu}(val));"));
                src_wrapper_impl.push("    }".to_string());
            } else {
                src_listener_decl.push(format!("    fn {init_fn_name}(&mut self) {{}}"));
                src_init.push(vec![format!("                    {nt} => self.listener.{init_fn_name}(),"), nt_comment]);
            }
        } else if flags & ruleflag::CHILD_REPEAT != 0 {
            if !is_sep_list {
                span_init.insert(var);
            }
            if has_value || is_sep_list {
                init_nt_done.insert(var);
                src_wrapper_impl.push(String::new());
                src_init.push(vec![format!("                    {nt} => self.{init_fn_name}(),"), nt_comment]);
                src_wrapper_impl.push(format!("    fn {init_fn_name}(&mut self) {{"));
                if is_lform {
                    if is_sep_list {
                        let all_exit_alts = if is_ambig_1st_child {
                            ambig_op_alts.values().rev().map(|v| v[0]).to_vec()
                        } else {
                            self.gather_alts(nt as VarId)
                        };
                        let exit_alts = all_exit_alts.into_iter()
                            .filter(|f|
                                (flags & ruleflag::CHILD_L_RECURSION == 0
                                    && flags & (ruleflag::CHILD_REPEAT_LFORM | ruleflag::REPEAT_PLUS) != ruleflag::CHILD_REPEAT_LFORM)
                                || !self.is_alt_sym_empty(*f)
                            );
                        let (mut last_alt_ids, exit_info_alts): (Vec<AltId>, Vec<AltId>) = exit_alts.into_iter()
                            .partition(|i| self.alt_info[*i as usize].is_none());
                        let last_alt_id_maybe = if last_alt_ids.is_empty() { None } else { Some(last_alt_ids.remove(0)) };
                        let a = exit_info_alts[0];
                        let indent = "        ";
                        let (src_let, ctx_params) = Self::source_lets(&self.item_info[a as usize], &self.nt_name, indent, last_alt_id_maybe);
                        src_wrapper_impl.extend(src_let);
                        let ctx = if ctx_params.is_empty() {
                            format!("InitCtx{nu}::{}", self.alt_info[a as usize].as_ref().unwrap().1)
                        } else {
                            format!("InitCtx{nu}::{} {{ {ctx_params} }}", self.alt_info[a as usize].as_ref().unwrap().1)
                        };
                        src_wrapper_impl.push(format!("        let ctx = {ctx};"));
                        if self.options.gen_span_params {
                            src_wrapper_impl.extend(Self::source_update_span(&self.span_nbrs_sep_list[&a].to_string()));
                        }
                        src_wrapper_impl.push(format!(
                            "        {}self.listener.{init_fn_name}(ctx{});",
                            if has_value { "let val = " } else { "" },
                            if self.options.gen_span_params { ", spans" } else { "" }));
                        let ret = if has_value {
                            format!("-> {};", self.get_nt_type(nt as VarId))
                        } else {
                            src_listener_decl.push("    #[allow(unused_variables)]".to_string());
                            "{}".to_string()
                        };
                        src_listener_decl.push(format!(
                            "    fn {init_fn_name}(&mut self, ctx: InitCtx{nu}{}) {ret}",
                            if self.options.gen_span_params { ", spans: Vec<PosSpan>" } else { "" }));

                        // skeleton (listener template)
                        let ret = if has_value { format!(" -> {}", self.get_nt_type(nt as VarId)) } else { String::new() };
                        src_skel.push(format!(
                            "    fn {init_fn_name}(&mut self, ctx: InitCtx{nu}{}){ret} {{",
                            if self.options.gen_span_params { ", spans: Vec<PosSpan>" } else { "" }));
                        let a_id = self.var_alts[nt][0];
                        let a_info = &self.item_info[a_id as usize];
                        if !a_info.is_empty() {
                            let comment = format!(
                                "value of `{}` before {}",
                                self.item_ops[a_id as usize][1..].iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                self.full_alt_components(a_id, None).1
                            );
                            let ctx_content = a_info.iter().map(|i| i.name.clone()).join(", ");
                            let a_name = &self.alt_info[a_id as usize].as_ref().unwrap().1;
                            src_skel.push(format!("        // {comment}"));
                            src_skel.push(format!("        let InitCtx{nu}::{a_name} {{ {ctx_content} }} = ctx;"));
                        }
                        has_skel_init = true;
                    } else {
                        src_wrapper_impl.push(format!("        let val = self.listener.{init_fn_name}();"));
                        src_listener_decl.push(format!("    fn {init_fn_name}(&mut self) -> {};", self.get_nt_type(nt as VarId)));
                        src_skel.push(format!("    fn {init_fn_name}(&mut self) -> {} {{", self.get_nt_type(nt as VarId)));
                        has_skel_init = true;
                    }
                    if has_value {
                        src_wrapper_impl.push(format!("        self.stack.push(EnumSynValue::{nu}(val));"));
                    }
                } else if is_sep_list {
                    // fetch values from stack to init the list with the first value that was outside the repetition:
                    // first α in α (β α)*
                    let endpoints = self.child_repeat_endpoints.get(&var).unwrap();
                    let (src_val, val_name) = self.source_child_repeat_lets(endpoints, &self.item_info, is_plus, &self.nt_name, &init_fn_name, nu, true);
                    src_wrapper_impl.extend(src_val);
                    src_wrapper_impl.push(format!("        self.stack.push(EnumSynValue::{nu}(Syn{nu}(vec![{val_name}])));"));
                } else {
                    src_wrapper_impl.push(format!("        let val = Syn{nu}(Vec::new());"));
                    src_wrapper_impl.push(format!("        self.stack.push(EnumSynValue::{nu}(val));"));
                }
                src_wrapper_impl.push("    }".to_string());
            } else if is_lform {
                init_nt_done.insert(var);
                src_init.push(vec![format!("                    {nt} => self.listener.{init_fn_name}(),"), nt_comment]);
                src_listener_decl.push(format!("    fn {init_fn_name}(&mut self) {{}}"));
            } else {
                // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
            }
        } else {
            // src_init.push(vec![format!("                    {nt} => {{}}"), nt_comment]);
        }
        if has_skel_init {
            if has_value {
                src_skel.push(format!("        {}()", self.get_nt_type(nt as VarId)));
            }
            src_skel.push("    }".to_string());
            src_skel.push(String::new());
        }
    }

    /// Adds the wrapper source code related to Call::Exit
    fn source_wrapper_exit<const VERBOSE: bool>(
        &self,
        ctx                 : &SourceInputContext,
        var                 : VarId,
        flags               : u32,
        has_value           : bool,
        is_ambig_1st_child  : bool,
        is_ambig_redundant  : bool,
        state               : &mut SourceState,
        sources             : &mut WrapperSources
    ) {
        const MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS: bool = false;

        let &SourceInputContext {
            parent_has_value, parent_nt, syns, ambig_op_alts
        } = ctx;
        let SourceState { nt_contexts, exit_alt_done, exit_fixer, .. } = state;
        let WrapperSources { src_listener_decl, src_skel, src_exit, src_wrapper_impl, .. } = sources;
        let nt = var as usize;
        let is_plus = flags & ruleflag::REPEAT_PLUS != 0;
        let is_parent = nt == parent_nt;
        let is_child_repeat_lform = self.nt_has_all_flags(var, ruleflag::CHILD_REPEAT_LFORM);
        let (nu, _nl, npl) = &self.nt_name[nt];

        // handles most rules except children of left factorization (already taken by self.gather_alts)
        if !is_ambig_redundant && flags & ruleflag::CHILD_L_FACT == 0 {
            let mut has_skel_exit = false;
            let mut has_skel_exit_return = false;
            let (pnu, _pnl, pnpl) = &self.nt_name[parent_nt];
            if VERBOSE { println!("    {nu} (parent {pnu})"); }
            let no_method = !has_value && flags & ruleflag::CHILD_REPEAT_LFORM == ruleflag::CHILD_REPEAT;
            let is_rrec_lform = self.nt_has_all_flags(var, ruleflag::R_RECURSION | ruleflag::L_FORM);
            let (fnpl, fnu, fnt, f_valued) = if is_ambig_1st_child {
                (pnpl, pnu, parent_nt, parent_has_value)    // parent_nt doesn't come through this code, so we must do it now
            } else {
                (npl, nu, nt, has_value)
            };
            if is_parent || (is_child_repeat_lform && !no_method) || is_ambig_1st_child {
                let extra_param = if self.options.gen_span_params { ", spans: Vec<PosSpan>" } else { "" };
                if f_valued {
                    let nt_type = self.get_nt_type(fnt as VarId);
                    if is_rrec_lform || (is_child_repeat_lform) {
                        src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, acc: &mut {nt_type}, ctx: Ctx{fnu}{extra_param});"));
                        src_skel.push(format!("    fn exit_{fnpl}(&mut self, acc: &mut {nt_type}, ctx: Ctx{fnu}{extra_param}) {{"));
                    } else {
                        src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, ctx: Ctx{fnu}{extra_param}) -> {nt_type};"));
                        src_skel.push(format!("    fn exit_{fnpl}(&mut self, ctx: Ctx{fnu}{extra_param}) -> {nt_type} {{"));
                        has_skel_exit_return = true;
                    }
                } else {
                    src_listener_decl.push("    #[allow(unused_variables)]".to_string());
                    src_listener_decl.push(format!("    fn exit_{fnpl}(&mut self, ctx: Ctx{fnu}{extra_param}) {{}}"));
                    src_skel.push(format!("    fn exit_{fnpl}(&mut self, ctx: Ctx{fnu}{extra_param}) {{"));
                }
                has_skel_exit = true;
            }
            let all_exit_alts = if is_ambig_1st_child {
                ambig_op_alts.values().rev().map(|v| v[0]).to_vec()
            } else {
                self.gather_alts(nt as VarId)
            };
            let (last_it_alts, exit_alts) = all_exit_alts.into_iter()
                .partition::<Vec<_>, _>(|f|
                    (flags & ruleflag::CHILD_L_RECURSION != 0
                        || flags & (ruleflag::CHILD_REPEAT_LFORM | ruleflag::REPEAT_PLUS) == ruleflag::CHILD_REPEAT_LFORM)
                    && self.is_alt_sym_empty(*f));
            if VERBOSE {
                println!("    no_method: {no_method}, exit alts: {}", exit_alts.iter().join(", "));
                if !last_it_alts.is_empty() {
                    println!("    last_it_alts: {}", last_it_alts.iter().join(", "));
                }
            }

            // skeleton (listener template)
            if has_skel_exit {
                if let Some(alts) = &nt_contexts[fnt] {
                    let mut skel_ctx = vec![];
                    for &a_id in alts {
                        if let Some((_, variant)) = self.alt_info[a_id as usize].as_ref() {
                            let comment = self.full_alt_str(a_id, None, false);
                            let fields = self.source_infos(&self.item_info[a_id as usize], false, false);
                            let ctx_content = if fields.is_empty() {
                                String::new()
                            } else {
                                format!(" {{ {fields} }}")
                            };
                            skel_ctx.push((comment, variant, ctx_content));
                        }
                    }
                    match skel_ctx.len() {
                        0 => {}
                        1 => {
                            let (comment, variant, ctx_content) = skel_ctx.pop().unwrap();
                            src_skel.push(format!("        // {comment}"));
                            src_skel.push(format!("        let Ctx{fnu}::{variant}{ctx_content} = ctx;"));
                        }
                        _ => {
                            src_skel.push("        match ctx {".to_string());
                            for (comment, variant, ctx_content) in skel_ctx {
                                src_skel.push(format!("            // {comment}"));
                                src_skel.push(format!("            Ctx{fnu}::{variant}{ctx_content} => {{}}"));
                            }
                            src_skel.push("        }".to_string());
                        }
                    }
                    if has_skel_exit_return {
                        src_skel.push(format!("        {}()", self.get_nt_type(fnt as VarId)));
                    }
                    src_skel.push("    }".to_string());
                    src_skel.push(String::new());
                } else {
                    panic!("no alts for NT {fnpl} [{fnt}]");
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
                let (v, pf) = &self.alts[*f as usize];
                if MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS {
                    format!("// {}", self.full_alt_str(*f, None, false))
                } else {
                    format!("// {}", pf.to_rule_str(*v, self.get_symbol_table(), self.flags[*v as usize]))
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
                            let (v, alt) = &self.alts[*a as usize];
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
            if flags & ruleflag::CHILD_REPEAT_LFORM == ruleflag::CHILD_REPEAT {
                if has_value {
                    let endpoints = self.child_repeat_endpoints.get(&var).unwrap();
                    let (src_val, val_name) = self.source_child_repeat_lets(endpoints, &self.item_info, is_plus, &self.nt_name, &fn_name, nu, false);
                    src_wrapper_impl.extend(src_val);
                    let vec_name = if is_plus { "plus_acc" } else { "star_acc" };
                    src_wrapper_impl.push(format!("        let Some(EnumSynValue::{nu}(Syn{nu}({vec_name}))) = self.stack.last_mut() else {{"));
                    src_wrapper_impl.push(format!("            panic!(\"expected Syn{nu} item on wrapper stack\");"));
                    src_wrapper_impl.push("        };".to_string());
                    src_wrapper_impl.push(format!("        {vec_name}.push({val_name});"));
                }
            } else {
                assert!(!no_method, "no_method is not expected here (only used in +* with no lform)");
                let (mut last_alt_ids, exit_info_alts): (Vec<AltId>, Vec<AltId>) = exit_alts.into_iter()
                    .partition(|i| self.alt_info[*i as usize].is_none());
                let fnu = if is_child_repeat_lform { nu } else { pnu }; // +* <L> use the loop variable, the other alternatives use the parent
                let fnpl = if is_child_repeat_lform { npl } else { pnpl }; // +* <L> use the loop variable, the other alternatives use the parent
                let a_has_value = if is_child_repeat_lform { has_value } else { parent_has_value };
                let is_single = exit_info_alts.len() == 1;
                let indent = if is_single { "        " } else { "                " };
                if !is_single {
                    if self.options.gen_span_params {
                        src_wrapper_impl.push("        let (n, ctx) = match alt_id {".to_string());
                    } else {
                        src_wrapper_impl.push("        let ctx = match alt_id {".to_string());
                    }
                }
                if VERBOSE { println!("    exit_alts -> {exit_info_alts:?}, last_alt_id -> {last_alt_ids:?}"); }
                let spans_param = if self.options.gen_span_params { ", spans" } else { "" };
                for a in exit_info_alts {
                    if VERBOSE {
                        println!("    - ALTERNATIVE {a}: {} -> {}",
                                 Symbol::NT(var).to_str(self.get_symbol_table()),
                                 self.alts[a as usize].1.to_str(self.get_symbol_table()));
                    }
                    let last_alt_id_maybe = if last_alt_ids.is_empty() { None } else { Some(last_alt_ids.remove(0)) };
                    if !is_single {
                        let last_alt_choice = if let Some(last_alt_id) = last_alt_id_maybe { format!(" | {last_alt_id}") } else { String::new() };
                        src_wrapper_impl.push(format!("            {a}{last_alt_choice} => {{", ));
                    }
                    let (src_let, ctx_params) = Self::source_lets(&self.item_info[a as usize], &self.nt_name, indent, last_alt_id_maybe);
                    src_wrapper_impl.extend(src_let);
                    let ctx = if ctx_params.is_empty() {
                        format!("Ctx{fnu}::{}", self.alt_info[a as usize].as_ref().unwrap().1)
                    } else {
                        format!("Ctx{fnu}::{} {{ {ctx_params} }}", self.alt_info[a as usize].as_ref().unwrap().1)
                    };
                    if is_single {
                        src_wrapper_impl.push(format!("        let ctx = {ctx};"));
                        if self.options.gen_span_params {
                            src_wrapper_impl.extend(Self::source_update_span(&self.span_nbrs[a as usize].to_string()));

                        }
                    } else {
                        let ctx_value = self.gen_match_item(ctx, || self.span_nbrs[a as usize].to_string());
                        src_wrapper_impl.push(format!("{indent}{ctx_value}"));
                        src_wrapper_impl.push("            }".to_string());
                    }
                }
                if !is_single {
                    src_wrapper_impl.push(format!("            _ => panic!(\"unexpected alt id {{alt_id}} in fn {fn_name}\")"));
                    src_wrapper_impl.push("        };".to_string());
                    if self.options.gen_span_params {
                        src_wrapper_impl.extend(Self::source_update_span("n"));
                    }
                }
                if (is_rrec_lform | is_child_repeat_lform) && f_valued {
                    src_wrapper_impl.push(
                        format!("        let Some(EnumSynValue::{fnu}(acc)) = self.stack.last_mut() else {{ panic!() }};"));
                    src_wrapper_impl.push(
                        format!("        self.listener.exit_{fnpl}(acc, ctx{spans_param});"));
                } else {
                    src_wrapper_impl.push(format!(
                        "        {}self.listener.exit_{fnpl}(ctx{spans_param});",
                        if a_has_value { "let val = " } else { "" }));
                    if a_has_value {
                        src_wrapper_impl.push(format!("        self.stack.push(EnumSynValue::{fnu}(val));"));
                    }
                }
            }
            if !no_method {
                src_wrapper_impl.push("    }".to_string());
            }
            for a in last_it_alts {
                assert_eq!(flags, self.flags[nt]);
                // optional exitloop_<NT> used by lrec and child_* <L> to post-process the accumulator
                // (rrec <L> and child_+ <L> don't need it because the context indicates the end alternative)
                let owner_maybe = if flags & ruleflag::CHILD_REPEAT_LFORM == ruleflag::CHILD_REPEAT_LFORM {
                    Some(var)
                } else if flags & ruleflag::CHILD_L_RECURSION != 0 {
                    self.parent[nt]
                } else {
                    None
                };
                if let Some(owner) = owner_maybe {
                    if self.nt_values[owner as usize] {
                        let (variant, _, fnname) = &self.nt_name[owner as usize];
                        let typ = self.get_nt_type(owner);
                        let varname = if is_child_repeat_lform { "acc" } else { fnname };
                        if VERBOSE { println!("    exitloop{fnname}({varname}) owner = {}", Symbol::NT(owner).to_str(self.get_symbol_table())); }
                        src_listener_decl.push("    #[allow(unused_variables)]".to_string());
                        src_listener_decl.push(format!("    fn exitloop_{fnname}(&mut self, {varname}: &mut {typ}) {{}}"));
                        let (v, pf) = &self.alts[a as usize];
                        let alt_str = if MATCH_COMMENTS_SHOW_DESCRIPTIVE_ALTS {
                            self.full_alt_str(a, None, false)
                        } else {
                            pf.to_rule_str(*v, self.get_symbol_table(), self.flags[*v as usize])
                        };
                        src_exit.push(vec![format!("                    {a} => self.exitloop_{fnpl}(),"), format!("// {alt_str}")]);
                        exit_alt_done.insert(a);
                        src_wrapper_impl.push(String::new());
                        src_wrapper_impl.push(format!("    fn exitloop_{fnpl}(&mut self) {{"));
                        src_wrapper_impl.push(format!("        let EnumSynValue::{variant}({varname}) = self.stack.last_mut().unwrap(){};",
                                                      if syns.len() > 1 { " else { panic!() }" } else { "" }));
                        src_wrapper_impl.push(format!("        self.listener.exitloop_{fnname}({varname});"));
                        src_wrapper_impl.push("    }".to_string());
                    }
                }
            }
        }
    }

    fn source_wrapper_finalize(&mut self, span_init: HashSet<VarId>, sources: WrapperSources) -> (Vec<String>, Vec<String>, Vec<String>) {
        let WrapperSources { mut src, src_listener_decl, mut src_skel, mut src_types, src_init, src_exit, src_wrapper_impl } = sources;

        // Writes the listener trait declaration
        src.add_space();
        src.push(format!("pub trait {}Listener {{", self.name));
        src.push("    /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from".to_string());
        src.push("    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.".to_string());
        src.push("    fn check_abort_request(&self) -> Terminate { Terminate::None }".to_string());
        src.push("    fn get_log_mut(&mut self) -> &mut impl Logger;".to_string());
        src.push("    #[allow(unused_variables)]".to_string());
        src.push("    fn handle_msg(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {".to_string());
        src.push("        self.get_log_mut().add(msg);".to_string());
        src.push("    }".to_string());
        let extra_span = if self.options.gen_span_params { ", span: PosSpan" } else { "" };
        let extra_ref_span = if self.options.gen_span_params { ", span: &PosSpan" } else { "" };
        if !self.terminal_hooks.is_empty() {
            src.push("    #[allow(unused_variables)]".to_string());
            src.push(format!("    fn hook(&mut self, token: TokenId, text: &str{extra_ref_span}) -> TokenId {{ token }}"));
        }
        src.push("    #[allow(unused_variables)]".to_string());
        src.push(format!("    fn intercept_token(&mut self, token: TokenId, text: &str{extra_ref_span}) -> TokenId {{ token }}"));
        if self.nt_values[self.start as usize] || self.options.gen_span_params {
            src.push("    #[allow(unused_variables)]".to_string());
        }
        if self.nt_values[self.start as usize] {
            src.push(format!("    fn exit(&mut self, {}: {}{extra_span}) {{}}", self.nt_name[self.start as usize].2, self.get_nt_type(self.start)));
        } else {
            src.push(format!("    fn exit(&mut self{extra_span}) {{}}"));
        }
        src.push("    #[allow(unused_variables)]".to_string());
        src.push("    fn abort(&mut self, terminate: Terminate) {}".to_string());
        /*
                              fn init_a(&mut self) {}
                              fn exit_a(&mut self, ctx: CtxA, spans: Vec<PosSpan>) -> SynA;
                              fn init_a_iter(&mut self) -> SynAIter;
                              #[allow(unused_variables)]
                              fn exit_a_iter(&mut self, ctx: CtxAIter) -> SynAIter {};
        */
        src.extend(src_listener_decl);
        src.push("}".to_string());

        // Writes the switch() function
        src.add_space();
        src.push("pub struct Wrapper<T> {".to_string());
        src.push("    verbose: bool,".to_string());
        src.push("    listener: T,".to_string());
        src.push("    stack: Vec<EnumSynValue>,".to_string());
        src.push("    max_stack: usize,".to_string());
        src.push("    stack_t: Vec<String>,".to_string());
        if self.options.gen_span_params {
            src.push("    stack_span: Vec<PosSpan>,".to_string());
        }
        src.push("}".to_string());
        src.push(String::new());
        src.push(format!("impl<T: {}Listener> ListenerWrapper for Wrapper<T> {{", self.name));
        src.push("    fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {".to_string());
        src.push("        if self.verbose {".to_string());
        src.push("            println!(\"switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}\");".to_string());
        src.push("        }".to_string());
        src.push("        if let Some(mut t_data) = t_data {".to_string());
        src.push("            self.stack_t.append(&mut t_data);".to_string());
        src.push("        }".to_string());
        src.push("        match call {".to_string());
        src.push("            Call::Enter => {".to_string());
        if self.options.gen_span_params {
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
                src.push("                    self.stack_span.push(PosSpan::empty());".to_string());
                src.push("                }".to_string());
            }
        }
        src.push("                match nt {".to_string());
        /*
                                              0 => self.listener.init_a(),                // A
                                              1 => self.init_a_iter(),                    // AIter1
                                              2 => {}                                     // A_1
        */
        src.extend(columns_to_str(src_init, Some(vec![64, 0])));
        src.push("                    _ => panic!(\"unexpected enter nonterminal id: {nt}\")".to_string());
        src.push("                }".to_string());
        src.push("            }".to_string());
        src.push("            Call::Loop => {}".to_string());
        src.push("            Call::Exit => {".to_string());
        src.push("                match alt_id {".to_string());
        /*
                                              3 |                                         // A -> a a (b <L>)* c
                                              4 => self.exit_a(alt_id),                   // A -> a c (b <L>)* c
                                              1 => self.exit_a_iter(),                    // (b <L>)* iteration in A -> a a  ► (b <L>)* ◄  c | ...
                                              2 => {}                                     // end of (b <L>)* iterations in A -> a a  ► (b <L>)* ◄  c | ...
                                           /* 0 */                                        // A -> a a (b <L>)* c | a c (b <L>)* c (never called)
        */
        src.extend(columns_to_str(src_exit, Some(vec![64, 0])));
        src.push("                    _ => panic!(\"unexpected exit alternative id: {alt_id}\")".to_string());
        src.push("                }".to_string());
        src.push("            }".to_string());
        src.push("            Call::End(terminate) => {".to_string());
        src.push("                match terminate {".to_string());
        src.push("                    Terminate::None => {".to_string());
        let mut args = vec![];
        let (_nu, _nl, npl) = &self.nt_name[self.start as usize];
        if self.nt_values[self.start as usize] {
            src.push(format!("                        let val = self.stack.pop().unwrap().get_{npl}();"));
            args.push("val");
        }
        if self.options.gen_span_params {
            src.push("                        let span = self.stack_span.pop().unwrap();".to_string());
            args.push("span");
        }
        src.push(format!("                        self.listener.exit({});", args.join(", ")));
        src.push("                    }".to_string());
        src.push("                    Terminate::Abort | Terminate::Conclude => self.listener.abort(terminate),".to_string());
        src.push("                }".to_string());
        src.push("            }".to_string());
        src.push("        }".to_string());
        src.push("        self.max_stack = std::cmp::max(self.max_stack, self.stack.len());".to_string());
        src.push("        if self.verbose {".to_string());
        src.push("            println!(\"> stack_t:   {}\", self.stack_t.join(\", \"));".to_string());
        src.push("            println!(\"> stack:     {}\", self.stack.iter().map(|it| format!(\"{it:?}\")).collect::<Vec<_>>().join(\", \"));".to_string());
        src.push("        }".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    fn check_abort_request(&self) -> Terminate {".to_string());
        src.push("        self.listener.check_abort_request()".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    fn abort(&mut self) {".to_string());
        src.push("        self.stack.clear();".to_string());
        if self.options.gen_span_params {
            src.push("        self.stack_span.clear();".to_string());
        }
        src.push("        self.stack_t.clear();".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    fn get_log_mut(&mut self) -> &mut impl Logger {".to_string());
        src.push("        self.listener.get_log_mut()".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    fn report(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {".to_string());
        src.push("        self.listener.handle_msg(span_opt, msg);".to_string());
        src.push("    }".to_string());
        if self.options.gen_span_params {
            src.push(String::new());
            src.push("    fn push_span(&mut self, span: PosSpan) {".to_string());
            src.push("        self.stack_span.push(span);".to_string());
            src.push("    }".to_string());
        }
        src.push(String::new());
        src.push("    fn is_stack_empty(&self) -> bool {".to_string());
        src.push("        self.stack.is_empty()".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    fn is_stack_t_empty(&self) -> bool {".to_string());
        src.push("        self.stack_t.is_empty()".to_string());
        src.push("    }".to_string());
        if self.options.gen_span_params {
            src.add_space();
            src.push("    fn is_stack_span_empty(&self) -> bool {".to_string());
            src.push("        self.stack_span.is_empty()".to_string());
            src.push("    }".to_string());
        }
        let unused_span = if self.options.gen_span_params { "" } else { "_" };
        let extra_span_arg = if self.options.gen_span_params { ", span" } else { "" };
        if !self.terminal_hooks.is_empty() {
            src.add_space();
            src.push(format!("    fn hook(&mut self, token: TokenId, text: &str, {unused_span}span: &PosSpan) -> TokenId {{"));
            src.push(format!("        self.listener.hook(token, text{extra_span_arg})"));
            src.push("    }".to_string());
        }
        src.add_space();
        src.push(format!("    fn intercept_token(&mut self, token: TokenId, text: &str, {unused_span}span: &PosSpan) -> TokenId {{"));
        src.push(format!("        self.listener.intercept_token(token, text{extra_span_arg})"));
        src.push("    }".to_string());
        src.push("}".to_string());

        src.add_space();
        src.push(format!("impl<T: {}Listener> Wrapper<T> {{", self.name));
        src.push("    pub fn new(listener: T, verbose: bool) -> Self {".to_string());
        src.push(format!(
            "        Wrapper {{ verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new(){} }}",
            if self.options.gen_span_params { ", stack_span: Vec::new()" } else { "" }
        ));
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    pub fn get_listener(&self) -> &T {".to_string());
        src.push("        &self.listener".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    pub fn get_listener_mut(&mut self) -> &mut T {".to_string());
        src.push("        &mut self.listener".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    pub fn give_listener(self) -> T {".to_string());
        src.push("        self.listener".to_string());
        src.push("    }".to_string());
        src.push(String::new());
        src.push("    pub fn set_verbose(&mut self, verbose: bool) {".to_string());
        src.push("        self.verbose = verbose;".to_string());
        src.push("    }".to_string());
/*
                              impl<T: TestListener> ListenerWrapper<T> {
                                  fn exit(&mut self) {
                                      let a = self.stack.pop().unwrap().get_a();
                                      self.listener.exit(Ctx::A { a });
                                  }
                                  fn init_a_iter(&mut self) {
                                      let val = self.listener.init_a_iter();
                                      self.stack.push(EnumSynValue::AIter(val));
                                  }
                                  fn exit_a_iter(&mut self) {
                                      let b = self.stack_t.pop().unwrap();
                                      let star_acc = self.stack.pop().unwrap().get_a_iter();
                                      let val = self.listener.exit_a_iter(CtxAIter::Aiter1 { star_acc, b });
                                      self.stack.push(EnumSynValue::AIter(val));
                                  }
                                  // ...
                              }
*/
        src.extend(src_wrapper_impl);
        src.push("}".to_string());

        // templates
        src_types.extend(vec![
            String::new(),
            format!("// {:-<80}", ""),
        ]);
        if let Some(line) = src_skel.last() {
            if line.is_empty() {
                src_skel.pop();
            }
        }
        src_skel.extend(vec![
            "}".to_string(),
            String::new(),
            format!("// {:-<80}", ""),
        ]);
        self.log.add_info(format!("Template for the user types:\n\n{}\n", src_types.join("\n")));
        self.log.add_info(format!("Template for the listener implementation:\n\n{}\n", src_skel.join("\n")));

        (src, src_types, src_skel)
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
        ParserGen::build_from_rules_ll1(rules, name)
    }
}

impl Default for ParserGenOptions {
    fn default() -> Self {
        ParserGenOptions {
            nt_value: NTValue::Default,
            include_alts: false,
            headers: vec![],
            used_libs: StructLibs::new(),
            gen_wrapper: true,
            gen_span_params: false,
            gen_token_enums: false,
            lib_crate: LexigramCrate::Core,
            indent: 0,
            types_indent: 0,
            listener_indent: 0,
            parser_type: ParserType::LL1,
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

impl ParserGen {

    pub fn get_nt_tree(&self) -> VecTree<VarId> {
        let mut tree = VecTree::new();
        let root = tree.add_root(0);
        let mut idx = HashMap::new();
        for group in self.nt_parent.iter().filter(|vf| !vf.is_empty()) {
            idx.clear();
            // some parents are later in the group, so we can't do this:
            // for &c in group {
            //     let idx_parent = self.parsing_table
            //         .parent[c as usize]
            //         .map(|v| idx.get(&v).unwrap())
            //         .unwrap_or(&root);
            //     let idx_c = tree.add(Some(*idx_parent), c);
            //     idx.insert(c, idx_c);
            // }
            let tree_ids = tree.add_iter(None, group.iter().cloned()).to_vec();
            idx.extend(group.iter().zip(tree_ids));
            for &child in group.iter() {
                tree.attach_child(
                    self.parent[child as usize]
                        .map(|p| idx[&p])
                        .unwrap_or(root),
                    idx[&child]);
            }
        }
        tree
    }

    pub fn get_indented_nt(&self) -> Vec<(VarId, String)>{
        let tree = self.get_nt_tree();
        let mut indented = vec![];
        let mut indent = vec![];
        for node in tree.iter_pre_depth_simple().skip(1) {
            let depth = node.depth as usize;
            if indent.len() < depth {
                indent.push((1..depth).map(|i| if i & 1 == 0 { "  " } else { ". " }).join(""));
            }
            indented.push((*node, format!("{}{}", &indent[depth - 1], Symbol::NT(*node).to_str(self.get_symbol_table()))));
        }
        indented
    }

    pub fn nt_info_str(&self) -> Vec<String> {
        let indented = self.get_indented_nt();
        let mut cols = vec![
            vec!["  NT".to_string(), "  name".to_string(), " val".to_string(), /*"  parent".to_string(),*/ "  flags".to_string(), String::new()]];
        for (v, line) in indented {
            let nt = v as usize;
            // let parent = self.parent[nt].map(|p| Symbol::NT(p).to_str(self.get_symbol_table())).unwrap_or_else(||String::new());
            cols.push(vec![
                format!("| {v:3}"),
                format!("| {line}"),
                if self.nt_values[nt] { "| y".to_string() } else { "|".to_string() },
                // format!("| {parent}"),
                format!("| {}", ruleflag::to_string(self.flags[nt]).join(", ")),
                "|".to_string(),
            ]);
        }
        let mut txt = columns_to_str(cols, Some(vec![3, 5, 0, /*0,*/ 0, 0]));
        if let Some(max) = txt.get(1).map(|s| s.charlen()) {
            let sep = format!("+{:-<1$}+", "", max - 2);
            txt.insert(1, sep.clone());
            txt.push(sep);
        }
        txt
    }

    pub fn log_nt_info(&mut self) {
        let mut txt = self.nt_info_str();
        txt.push(String::new());
        self.log.add_info("nonterminal information:");
        self.log.extend_messages(txt.into_iter().map(LogMsg::Info));
    }

    pub fn alt_info_str(&self) -> Vec<String> {
        let indented = self.get_indented_nt();
        let mut cols = vec![
            vec!["  NT".to_string(), "  alt".to_string(), "  opcodes".to_string(), " spans".to_string(), "  item_ops".to_string(), String::new()]];
        for (v, line) in indented {
            let nt = v as usize;
            for &alt_id in &self.var_alts[nt] {
                let a_id = alt_id as usize;
                let alt = &self.alts[a_id].1;
                let opcodes = self.opcodes[a_id].iter().map(|o| o.to_str_quote(self.get_symbol_table())).join(" ");
                let item_ops = self.item_ops.get(a_id)
                    .map(|ops| ops.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "))
                    .unwrap_or_else(|| "-".to_string());
                cols.push(vec![
                    format!("| {v:3}"),
                    format!("| {alt_id:4}: {line} -> {}", alt.to_str(self.get_symbol_table())),
                    format!("| {opcodes}"),
                    format!("| {}{}",
                        &self.span_nbrs[a_id],
                        if let Some(ispan) = self.span_nbrs_sep_list.get(&alt_id) { format!(", {ispan}") } else { String::new() }),
                    format!("| {item_ops}"),
                    "|".to_string(),
                ]);
            }
        }
        let mut txt = columns_to_str(cols, Some(vec![3, 5, 0, 0, 0, 0]));
        if let Some(max) = txt.get(1).map(|s| s.charlen()) {
            let sep = format!("+{:-<1$}+", "", max - 2);
            txt.insert(1, sep.clone());
            txt.push(sep);
        }
        txt
    }

    pub fn log_alt_info(&mut self) {
        let mut txt = self.alt_info_str();
        txt.push("legend: ►nt = enter nonterminal nt, ◄0 = exit alt, ●nt = loop nonterminal, Xyz! = variable terminal, \"…\" = fixed terminal, ▲ = hook".to_string());
        txt.push(String::new());
        self.log.add_note("rule alternatives:");
        self.log.extend_messages(txt.into_iter().map(LogMsg::Info));
    }

    pub fn print_items(&self, indent: usize, show_symbols: bool, show_span: bool) {
        let tbl = self.get_symbol_table();
        let fields = (0..self.alts.len())
            .map(|a| {
                let a_id = a as AltId;
                let (v, alt) = &self.alts[a];
                let ops = &self.opcodes[a];
                let it = &self.item_ops[a_id as usize];
                let mut cols = vec![];
                if show_symbols {
                    let symbols = format!("symbols![{}]", it.iter().map(|s| s.to_macro_item()).join(", "));
                    let value = if show_span {
                        assert!(self.options.gen_span_params, "ParserGen is not configured for spans");
                        format!("({}, {symbols})", self.span_nbrs[a_id as usize])
                    } else {
                        symbols
                    };
                    cols.push(format!("{a_id} => {value},"));
                }
                cols.extend([
                    format!("// {a_id:2}: {} -> {}", Symbol::NT(*v).to_str(tbl), alt.iter().map(|s| s.to_str_quote(tbl)).join(" ")),
                    format!("| {}", ops.iter().map(|s| s.to_str_quote(tbl)).join(" ")),
                    format!(
                        "| {}{}",
                        &self.span_nbrs[a_id as usize],
                        if let Some(ispan) = self.span_nbrs_sep_list.get(&a_id) { format!(", {ispan}") } else { String::new() }),
                    format!("| {}", it.iter().map(|s| s.to_str(tbl)).join(" ")),
                ]);
                cols
            }).to_vec();
        let widths = if show_symbols { vec![40, 0, 0, 0, 0] } else { vec![16, 0, 0, 0, 0] };
        for l in columns_to_str(fields, Some(widths)) {
            println!("{:indent$}{l}", "", indent = indent)
        }
    }

    pub fn print_flags(&self, indent: usize) {
        let tbl: Option<&SymbolTable> = self.get_symbol_table();
        let prefix = format!("{:width$}//", "", width = indent);
        let nt_flags = self.flags.iter().index().filter_map(|(nt, &f)|
            if f != 0 { Some(format!("{prefix}  - {}: {} ({})", Symbol::NT(nt).to_str(tbl), ruleflag::to_string(f).join(" | "), f)) } else { None }
        ).join("\n");
        let parents = self.parent.iter().index().filter_map(|(c, &par)|
            par.map(|p| format!("{prefix}  - {} -> {}", Symbol::NT(c).to_str(tbl), Symbol::NT(p).to_str(tbl)))
        ).join("\n");
        if !nt_flags.is_empty() {
            println!("{prefix} NT flags:\n{nt_flags}");
        }
        if !parents.is_empty() {
            println!("{prefix} parents:\n{parents}");
        }
    }
}
