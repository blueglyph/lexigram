// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashMap;
use iter_index::IndexerIterator;
use lexigram_core::alt::Alternative;
use lexigram_core::parser::{OpCode, Symbol};
use lexigram_core::{AltId, CollectJoin, VarId, LL1};
use lexigram_core::fixed_sym_table::FixedSymTable;
use lexigram_core::log::{LogReader, LogStatus, Logger};
use lexigram_core::parser::ll1::LLParser;
use crate::build::{BuildError, BuildErrorSource, BuildFrom, TryBuildFrom};
use crate::{columns_to_str, NameTransformer, SourceSpacer};
use crate::grammar::ll1::LL1ParsingTable;
use crate::grammar::{ProdRuleSet, SepInfo};
use crate::parsergen::{symbol_to_code, ParserGen, ParserGenOptions, ParserType};

impl ParserGen {
    /// Creates a [ParserGen] from a set of LL(1) production rules.
    ///
    /// `rules` must contain a name, which is used to name the user listener trait in the generated code.
    pub fn build_from_rules_ll1<T>(mut rules: ProdRuleSet<T>) -> Self
    where
        ProdRuleSet<LL1>: BuildFrom<ProdRuleSet<T>>,
    {
        rules.log.add_note("building parser gen from rules...");
        let name = rules.get_name()
            .and_then(|n| Some(n.clone()))
            .unwrap_or_else(|| {
                rules.log.add_error("The rules didn't specify a name for the parser, using Test as replacement");
                "Test".to_string()
            });
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
        let ProdRuleSet { symbol_table, nt_conversion, origin, sep_info, .. } = ll1_rules;
        let SepInfo::Nt(sep_nt) = sep_info else { panic!("unprocessed ProdRuleSet<LL1>") };
        let mut builder = ParserGen {
            num_nt: parsing_table.num_nt,
            num_t_full: parsing_table.num_t_full,
            alts: parsing_table.alts,
            flags: parsing_table.flags,
            parent: parsing_table.parent,
            sep_info: SepInfo::Nt(sep_nt),
            table: parsing_table.table,
            num_states: 0,
            action: vec![],
            goto: vec![],
            init_hook: false,
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

    pub(super) fn source_build_parser_ll1(&mut self) -> Vec<String> {
        static BASE_PARSER_LIBS: [&str; 5] = [
            "::VarId",
            "::AltId",
            "::parser::OpCode",
            "::parser::ll1::LLParser",
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
}

impl<T> BuildFrom<ProdRuleSet<T>> for ParserGen where ProdRuleSet<LL1>: BuildFrom<ProdRuleSet<T>> {
    /// Creates a [`ParserGen`] from a set of production rules.
    ///
    /// If the rule set has a name, it's transmitted to the parser generator to name the user
    /// listener trait in the generated code. If the rule set has no name, a default "Parser" name
    /// is used instead (unless the name is set with [`ParserGen::set_name()`].
    fn build_from(rules: ProdRuleSet<T>) -> Self {
        ParserGen::build_from_rules_ll1(rules)
    }
}

// ---------------------------------------------------------------------------------------------

/// Tables and parameters used to create a [`LLParser`]. This type is used as a return object from the parser generator,
/// when the LLParser must be created dynamically; for example, in tests or in situations where the grammar isn't
/// known in advance. In those situations, the LLParserTables object must live as long as the parser it generates.
///
/// The LLParser itself uses references to tables whenever possible because, in most situations, the tables are
/// static in generated source files. A few fields must still be created dynamically from (possibly) static
/// tables because they don't exist in static form.
pub struct LLParserTables {
    num_nt: usize,
    num_t_full: usize,
    alt_var: Vec<VarId>,
    alts: Vec<Alternative>,
    opcodes: Vec<Vec<OpCode>>,
    init_opcodes: Vec<OpCode>,
    table: Vec<AltId>,
    symbol_table: FixedSymTable,
    start: VarId,
    include_alts: bool,
}

impl LLParserTables {
    pub fn new(
        parsing_table: LL1ParsingTable,
        symbol_table: FixedSymTable,
        opcodes: Vec<Vec<OpCode>>,
        init_opcodes: Vec<OpCode>,
        start: VarId,
        include_alts: bool
    ) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        let num_nt = parsing_table.num_nt;
        let num_t_full = parsing_table.num_t_full;
        let table = parsing_table.table;
        let (factor_var, alts): (Vec<_>, Vec<_>) = parsing_table.alts.into_iter().unzip();
        LLParserTables { num_nt, num_t_full, alt_var: factor_var, alts, opcodes, init_opcodes, table, symbol_table, start, include_alts }
    }

    pub fn get_symbol_table(&self) -> &FixedSymTable {
        &self.symbol_table
    }
    
    pub fn make_parser(&self) -> LLParser<'_> {
        LLParser::new(
            self.num_nt,
            self.num_t_full,
            self.alt_var.as_slice(),
            if self.include_alts { self.alts.clone() } else { vec![] },
            self.opcodes.clone(),
            self.init_opcodes.clone(),
            self.table.as_slice(),
            self.symbol_table.clone(),
            self.start,
        )
    }
}

impl BuildFrom<ParserGen> for LLParserTables {
    /// Creates a [`LLParserTables`], from which a parser can be created dynamically with
    /// [`parser_table.make_parser()`](LLParserTables::make_parser).
    fn build_from(mut parser_gen: ParserGen) -> Self {
        parser_gen.pre_calc_data();
        if !parser_gen.has_no_errors() {
            panic!("creation of LL parser tables failed:{}", parser_gen.log);
        }
        let parsing_table = LL1ParsingTable {
            num_nt: parser_gen.num_nt,
            num_t_full: parser_gen.num_t_full,
            alts: parser_gen.alts,
            table: parser_gen.table,
            flags: parser_gen.flags,
            parent: parser_gen.parent,
        };
        LLParserTables::new(
            parsing_table,
            parser_gen.symbol_table.to_fixed_sym_table(),
            parser_gen.opcodes,
            parser_gen.init_opcodes,
            parser_gen.start,
            parser_gen.options.include_alts
        )
    }
}

// not generated automatically since LLParserTables isn't LogReader
impl TryBuildFrom<ParserGen> for LLParserTables {
    type Error = BuildError;

    fn try_build_from(source: ParserGen) -> Result<Self, Self::Error> {
        if source.get_log().has_no_errors() {
            Ok(LLParserTables::build_from(source))
        } else {
            Err(BuildError::new(source.give_log(), BuildErrorSource::ParserGen))
        }
    }
}
