// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashMap;
use std::marker::PhantomData;
use iter_index::IndexerIterator;
use lexigram_core::fixed_sym_table::{FixedSymTable, SymInfoTable};
use lexigram_core::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_core::parser::lr::{LRAction, LRParser, LRStateId};
use lexigram_core::{CollectJoin, VarId};
use lexigram_core::alt::Alternative;
use lexigram_core::parser::Symbol;
use crate::build::BuildFrom;
use crate::grammar::{ProdRuleSet, SepInfo};
use crate::{SymbolTable, LALR, LR, SourceSpacer};
use crate::parsergen::{ParserGen, ParserGenOptions, ParserType};
use crate::adaptors::FlagLastIterator;

impl ParserGen {
    /// Creates a [ParserGen] from a set of LR production rules.
    ///
    /// `rules` must contain a name, which is used to name the user listener trait in the generated code.
    pub fn build_lalr_from_rules_lr<T>(mut rules: ProdRuleSet<T>) -> Self
    where
        ProdRuleSet<LR>: BuildFrom<ProdRuleSet<T>>,
    {
        rules.log.add_note("building parser gen from rules...");
        let name = rules.get_name()
            .and_then(|n| Some(n.clone()))
            .unwrap_or_else(|| {
                rules.log.add_error("The rules didn't specify a name for the parser, using Test as replacement");
                "Test".to_string()
            });
        let mut lr_rules = ProdRuleSet::<LR>::build_from(rules);
        assert_eq!(lr_rules.get_log().num_errors(), 0);
        let parsing_table = lr_rules.make_parsing_table_lalr(true);
        let num_nt = lr_rules.get_num_nt();
        let mut var_alts = vec![vec![]; num_nt];
        for (alt_id, (var_id, _)) in parsing_table.alts.iter().index() {
            var_alts[*var_id as usize].push(alt_id);
        }
        let nt_parent: Vec<Vec<VarId>> = vec![vec![]; num_nt];
        let ProdRuleSet { symbol_table, nt_conversion, origin, sep_info, terminal_hooks, .. } = lr_rules;
        let SepInfo::Nt(sep_nt) = sep_info else { panic!("unprocessed ProdRuleSet<LL1>") };
        let mut builder = ParserGen {
            num_nt: parsing_table.num_nt,
            num_t_full: parsing_table.num_t_full,
            alts: parsing_table.alts,
            flags: parsing_table.flags,
            parent: parsing_table.parent,
            sep_info: SepInfo::Nt(sep_nt),
            table: vec![],
            num_states: parsing_table.num_states,
            action: parsing_table.action,
            goto: parsing_table.goto,
            state_symbol: parsing_table.state_symbol,
            init_hook: parsing_table.init_hook,
            symbol_table: symbol_table.expect(stringify!("symbol table is required to create a {}", std::any::type_name::<Self>())),
            name,
            options: ParserGenOptions::default(),
            nt_values: vec![false; num_nt],
            nt_parent,
            var_alts,
            origin,
            terminal_hooks,         // the LR transformation has already been done, but just for the sake of coherency
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

    pub(super) fn source_build_parser_lalr(&mut self) -> Vec<String> {
        static BASE_PARSER_LIBS: [&str; 7] = [
            "::VarId",
            "::LALR",
            "::fixed_sym_table::FixedSymTable",
            "::parser::lr::LRParser",
            "::parser::lr::LRStateId",
            "::parser::lr::LRAction",
            "::parser::Symbol",
        ];
        static BASE_PARSER_LRACTION_LIBS : [&str; 5] = [
            "::parser::lr::LRAction::Error as LRE",
            "::parser::lr::LRAction::Shift as LRS",
            "::parser::lr::LRAction::ShiftHook as LRSH",
            "::parser::lr::LRAction::Reduce as LRR",
            "::parser::lr::LRAction::Accept as LRA",
        ];
        self.log.add_note("generating LALR build_parser source...");
        let mut action_used = [false, false, false, false, false];
        let actions = self.action.chunks(ACTION_CHUNK)
            .flag_first_last()
            .map(|(_, is_last, actions)|
                format!(
                    "    {}{}",
                    actions.into_iter().map(|a| {
                        let (str, id) = match a {
                            LRAction::Error => ("LRE".to_string(), 0),
                            LRAction::Shift(s) => (format!("LRS({s})"), 1),
                            LRAction::ShiftHook(s) => (format!("LRSH({s})"), 2),
                            LRAction::Reduce(r) => (format!("LRR({r})"), 3),
                            LRAction::Accept => ("LRA".to_string(), 4),
                        };
                        action_used[id] = true;
                        str
                    }).join(","),
                    if is_last { "];" } else { "," }))
            .to_vec();
        let num_nt_table = self.symbol_table.get_num_nt();
        let num_t_table = self.symbol_table.get_num_t(); // includes <$> and <empty>
        self.options.used_libs.extend(
            BASE_PARSER_LIBS.into_iter().map(|s| format!("{}{s}", self.options.lib_crate))
        );
        self.options.used_libs.extend(
            BASE_PARSER_LRACTION_LIBS.into_iter().enumerate()
                .filter_map(|(i, s)| if action_used[i] { Some(format!("{}{s}", self.options.lib_crate)) } else { None } )
        );
        self.log.add_note(format!(
            "- creating parsor tables: {num_t_table} terminals (including $ and empty), {num_nt_table} nonterminals, {} actions, {} gotos, {} productions",
            self.action.len(), self.goto.len(), self.alts.len()));
        let alt_nt_len = alts_to_alt_nt_len(&self.alts, &self.symbol_table);
        let mut src = vec![
            format!("static NUM_NT: usize = {};", self.num_nt),
            format!("static NUM_T_FULL: usize = {};", self.num_t_full),
            format!("static ACTION: [LRAction; {}] = [", self.action.len()),
        ];
        const ACTION_CHUNK: usize = 35;
        assert!(!self.action.is_empty(), "action table is empty");
        src.extend(actions);
        const GOTO_CHUNK: usize = 40;
        assert!(!self.goto.is_empty(), "goto table is empty");
        src.push(format!("static GOTO: [LRStateId; {}] = [", self.goto.len()));
        src.extend(
            self.goto.chunks(GOTO_CHUNK)
                .flag_first_last()
                .map(|(_, is_last, gotos)|
                    format!(
                        "    {}{}",
                        gotos.into_iter().map(LRStateId::to_string).join(","),
                        if is_last { "];" } else { "," })));
        const ALT_CHUNK: usize = 25;
        assert!(!alt_nt_len.is_empty(), "alt_nt_len table is empty");
        src.push(format!("static ALT_NT_LEN: [(VarId, u16, u16); {}] = [", alt_nt_len.len()));
        src.extend(
            alt_nt_len.chunks(ALT_CHUNK)
                .flag_first_last()
                .map(|(_, is_last, terminals)|
                    format!("    {}{}", terminals.iter().map(|v| format!("{v:?}")).join(","), if is_last { "];" } else { "," })));
        const NT_CHUNK: usize = 20;
        assert!(self.symbol_table.get_num_nt() > 0, "terminal table is empty");
        src.push(format!("static SYMBOLS_NT: [&str; {num_nt_table}] = ["));
        let mut it = self.symbol_table.get_nonterminals();
        src.extend(
            (0..(self.symbol_table.get_num_nt() + NT_CHUNK - 1) / NT_CHUNK)
                .flag_first_last()
                .map(|(_, is_last, _)|
                    format!("    {}{}", (0..NT_CHUNK).filter_map(|_| it.next()).map(|v| format!("{v:?}")).join(","), if is_last { "];" } else { "," })));
        src.push(String::new());
        src.extend(self.source_token_enums());
        src.add_space();

        // NT values for error recovery
        if self.options.has_lr_error_recovery() {
            const NT_VALUE_CHUNK: usize = 20;
            assert!(self.symbol_table.get_num_nt() > 0, "terminal table is empty");
            src.push(format!("static NT_VALUE: [bool; {}] = [", self.nt_values.len()));
            src.extend(
                self.nt_values.chunks(NT_VALUE_CHUNK)
                    .flag_first_last()
                    .map(|(_, is_last, values)|
                        format!("    {}{}", values.iter().map(|&v| format!("{v:?}")).join(","), if is_last { "];" } else { "," })));
            const STATE_SYMBOL_CHUNK: usize = 25;
            assert!(!self.state_symbol.is_empty(), "state_symbol is empty");
            src.push(format!("static STATE_SYMBOL: [Symbol; {}] = [", self.state_symbol.len()));
            src.extend(
                self.state_symbol.chunks(STATE_SYMBOL_CHUNK)
                    .flag_first_last()
                    .map(|(_, is_last, symbols)|
                        format!("    {}{}", symbols.iter().map(|s| format!("Symbol::{s:?}")).join(","), if is_last { "];" } else { "," })));
            src.push(String::new());
        }

        src.extend([
            "pub fn build_parser() -> LRParser<'static, LALR> {".to_string(),
            "    LRParser::new(".to_string(),
            "        NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,".to_string(),
            "        FixedSymTable::new(".to_string(),
            "            SYMBOLS_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),".to_string(),
            "            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()".to_string(),
            "        ),".to_string(),
            format!("        {},", self.init_hook),
            "        &STATE_SYMBOL,".to_string(),
            "        &NT_VALUE".to_string(),
            "    )".to_string(),
            "}".to_string(),
        ]);
        src
    }
}

// ---------------------------------------------------------------------------------------------

/// Tables and parameters used to create a [`LRParser`]. This type is used as a return object from the parser generator,
/// when the LRParser must be created dynamically; for example, in tests or in situations where the grammar isn't
/// known in advance. In those situations, the LRParserTables object must live as long as the parser it generates.
///
/// The LRParser itself uses references to tables whenever possible because, in most situations, the tables are
/// static in generated source files. A few fields must still be created dynamically from (possibly) static
/// tables because they don't exist in static form.
#[derive(Default, Debug)]
pub struct LRParserTables<T> {
    num_nt: usize,
    num_t_full: usize,                  // includes the end symbol
    action: Vec<LRAction>,
    goto: Vec<LRStateId>,
    alt_nt_len: Vec<(VarId, u16, u16)>, // alt_id -> (nt, # symbols in alt, # terminals in alt)
    symbol_table: FixedSymTable,        // must include terminals <$> and <empty> at the end
    init_hook: bool,
    state_symbol: Vec<Symbol>,
    nt_value: Vec<bool>,
    log: BufLog,
    _phantom: PhantomData<T>,
}

impl<T> LRParserTables<T> {
    pub fn new(
        num_nt: usize,
        num_t_full: usize,
        action: Vec<LRAction>,
        goto: Vec<LRStateId>,
        alt_nt_len: Vec<(VarId, u16, u16)>,
        symbol_table: FixedSymTable,
        init_hook: bool,
        state_symbol: Vec<Symbol>,
        nt_value: Vec<bool>,
        log: Option<BufLog>
    ) -> Self {
        let log = log.unwrap_or_else(|| BufLog::new());
        LRParserTables { num_nt, num_t_full, action, goto, alt_nt_len, symbol_table, init_hook, log, state_symbol, nt_value, _phantom: PhantomData }
    }

    pub fn make_parser(&self) -> LRParser<'_, T> {
        LRParser::new(
            self.num_nt,
            self.num_t_full,
            &self.action,
            &self.goto,
            &self.alt_nt_len,
            self.symbol_table.clone(),
            self.init_hook,
            &self.state_symbol,
            &self.nt_value
        )
    }

    pub fn get_log(&self) -> &BufLog {
        &self.log
    }
}

// ---------------------------------------------------------------------------------------------

pub fn alts_to_alt_nt_len(alts: &Vec<(VarId, Alternative)>, symtable: &SymbolTable) -> Vec<(VarId, u16, u16)> {
    alts.into_iter()
        .enumerate()
        .map(|(i, (nt, alt))| (
            // nonterminal index:
            *nt,
            // number of symbols in production alternative:
            u16::try_from(if alt.is_sym_empty() { 0 } else { alt.len() })
                .expect(&format!("alt[{i}] too long:\n{}", alt.to_str(Some(symtable)))),
            // number of (variable) terminals containing data:
            alt.iter().filter(|s| symtable.is_symbol_t_data(s)).count() as u16
        ))
        .to_vec()
}

impl BuildFrom<ProdRuleSet<LR>> for LRParserTables<LALR> {
    fn build_from(mut source: ProdRuleSet<LR>) -> Self {
        let table = source.make_parsing_table_lalr(true);
        if !source.has_no_errors() {
            panic!("creation of LALR parsing table failed:{}", source.log);
        }
        let Some(mut symtable) = source.symbol_table.clone() else {
            panic!("no symbol table in ProdRuleSet<LR>");
        };
        symtable.add_terminal("<$>", Some("<$>"));
        symtable.add_terminal("<empty>", Some("<empty>"));
        LRParserTables::new(
            table.num_nt,
            table.num_t_full,
            table.action,
            table.goto,
            alts_to_alt_nt_len(&table.alts, &symtable),
            symtable.to_fixed_sym_table(),
            table.init_hook,
            table.state_symbol,
            vec![false; source.get_num_nt()],
            Some(source.log)
        )
    }
}
