// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]
#![allow(non_camel_case_types)]

#[allow(unused)]
pub(crate) mod listener1 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener1]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{LLParser, OpCode, Symbol}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Not", Some("!")), ("Sub", Some("-")), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1"];
    static ALT_VAR: [VarId; 4] = [0, 0, 1, 1];
    static ALTERNATIVES: [&[Symbol]; 4] = [&[Symbol::T(1), Symbol::NT(0)], &[Symbol::T(2), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(1)], &[Symbol::Empty]];
    static PARSING_TABLE: [AltId; 8] = [4, 0, 1, 5, 2, 4, 4, 3];
    static OPCODES: [&[OpCode]; 4] = [&[OpCode::Exit(0), OpCode::NT(0), OpCode::T(1)], &[OpCode::NT(1), OpCode::Exit(1), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::T(0)], &[OpCode::Exit(3)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener1]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener2 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener2]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{LLParser, OpCode, Symbol}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Sub", Some("-")), ("Mul", Some("*")), ("Div", Some("/")), ("Add", Some("+")), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1", "e_2", "e_3", "e_4"];
    static ALT_VAR: [VarId; 12] = [0, 1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 4];
    static ALTERNATIVES: [&[Symbol]; 12] = [&[Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(2), Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(3), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(2), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(1), Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(2), Symbol::NT(4), Symbol::NT(3)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(4)], &[Symbol::T(4)]];
    static PARSING_TABLE: [AltId; 30] = [0, 12, 12, 12, 0, 13, 4, 1, 2, 3, 12, 5, 6, 13, 13, 13, 6, 13, 9, 7, 8, 9, 12, 9, 10, 13, 13, 13, 11, 13];
    static OPCODES: [&[OpCode]; 12] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(4)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(4), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(4), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(2), OpCode::T(3)], &[OpCode::Loop(1), OpCode::Exit(4), OpCode::NT(2), OpCode::T(0)], &[OpCode::Exit(5)], &[OpCode::NT(3), OpCode::Exit(6), OpCode::NT(4)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(4), OpCode::T(1)], &[OpCode::Loop(3), OpCode::Exit(8), OpCode::NT(4), OpCode::T(2)], &[OpCode::Exit(9)], &[OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11), OpCode::T(4)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener2]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener3 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener3]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{LLParser, OpCode, Symbol}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Sub", Some("-")), ("Mul", Some("*")), ("Div", Some("/")), ("Add", Some("+")), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1", "e_2", "e_3", "e_4"];
    static ALT_VAR: [VarId; 12] = [0, 1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 4];
    static ALTERNATIVES: [&[Symbol]; 12] = [&[Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(3), Symbol::NT(0), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(0), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(3)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(3)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(4)], &[Symbol::T(4)]];
    static PARSING_TABLE: [AltId; 30] = [0, 13, 13, 13, 0, 13, 4, 1, 2, 3, 12, 5, 6, 13, 13, 13, 6, 13, 9, 7, 8, 9, 12, 9, 10, 13, 13, 13, 11, 13];
    static OPCODES: [&[OpCode]; 12] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(4)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(2), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(0), OpCode::T(3)], &[OpCode::Loop(1), OpCode::Exit(4), OpCode::NT(0), OpCode::T(0)], &[OpCode::Exit(5)], &[OpCode::NT(3), OpCode::Exit(6), OpCode::NT(4)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(3), OpCode::Exit(8), OpCode::NT(2), OpCode::T(2)], &[OpCode::Exit(9)], &[OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11), OpCode::T(4)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener3]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener4 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener4]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{LLParser, OpCode, Symbol}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Sub", Some("-")), ("Mul", Some("*")), ("Div", Some("/")), ("Add", Some("+")), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1", "e_2", "e_3", "e_4"];
    static ALT_VAR: [VarId; 12] = [0, 1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 4];
    static ALTERNATIVES: [&[Symbol]; 12] = [&[Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(3), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(2), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(3)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(3)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(4)], &[Symbol::T(4)]];
    static PARSING_TABLE: [AltId; 30] = [0, 12, 12, 12, 0, 13, 4, 1, 2, 3, 12, 5, 6, 13, 13, 13, 6, 13, 9, 7, 8, 9, 12, 9, 10, 13, 13, 13, 11, 13];
    static OPCODES: [&[OpCode]; 12] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(4)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(2), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(2), OpCode::T(3)], &[OpCode::Loop(1), OpCode::Exit(4), OpCode::NT(2), OpCode::T(0)], &[OpCode::Exit(5)], &[OpCode::NT(3), OpCode::Exit(6), OpCode::NT(4)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(3), OpCode::Exit(8), OpCode::NT(2), OpCode::T(2)], &[OpCode::Exit(9)], &[OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11), OpCode::T(4)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener4]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener5 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener5]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{LLParser, OpCode, Symbol}};

    const PARSER_NUM_T: usize = 2;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Num", None), ("Exp", Some("^"))];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["expr", "expr_1"];
    static ALT_VAR: [VarId; 3] = [0, 1, 1];
    static ALTERNATIVES: [&[Symbol]; 3] = [&[Symbol::T(0), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(0)], &[Symbol::Empty]];
    static PARSING_TABLE: [AltId; 6] = [0, 3, 4, 3, 1, 2];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::NT(1), OpCode::T(0)], &[OpCode::Loop(0), OpCode::Exit(1), OpCode::T(1)], &[OpCode::Exit(2)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener5]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod lr_parser_601 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener6]

    use lexigram_lib::{LALR, VarId, fixed_sym_table::FixedSymTable, parser::lr_parser::{LRAction, LRParser, LRStateId}};

    static NUM_NT: usize = 1;
    static NUM_T_FULL: usize = 5;
    static ACTION: [LRAction; 40] = [LRAction::Error, LRAction::Error, LRAction::Shift(1), LRAction::Shift(2), LRAction::Error, LRAction::Reduce(2), LRAction::Reduce(2), LRAction::Error, LRAction::Error, LRAction::Reduce(2), LRAction::Reduce(3), LRAction::Reduce(3), LRAction::Error, LRAction::Error, LRAction::Reduce(3), LRAction::Shift(4), LRAction::Shift(5), LRAction::Error, LRAction::Error, LRAction::Accept, LRAction::Error, LRAction::Error, LRAction::Shift(1), LRAction::Shift(2), LRAction::Error, LRAction::Error, LRAction::Error, LRAction::Shift(1), LRAction::Shift(2), LRAction::Error, LRAction::Reduce(0), LRAction::Reduce(0), LRAction::Error, LRAction::Error, LRAction::Reduce(0), LRAction::Shift(4), LRAction::Reduce(1), LRAction::Error, LRAction::Error, LRAction::Reduce(1)];
    static GOTO: [LRStateId; 8] = [3, 8, 8, 8, 6, 7, 8, 8];
    static ALT_NT_LEN: [(VarId, u16, u16); 5] = [(0, 3, 0), (0, 3, 0), (0, 1, 1), (0, 1, 1), (1, 1, 0)];
    static SYMBOL_TABLE_T: [(&str, Option<&str>); 4] = [("Mul", Some("*")), ("Add", Some("+")), ("Num", None), ("Id", None)];
    static SYMBOL_TABLE_NT: [&str; 2] = ["e", "<goal>"];

    pub fn build_parser(n: u32) -> LRParser<'static, LALR> {
        LRParser::new(
            NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,
            FixedSymTable::new(
                SYMBOL_TABLE_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),
                SYMBOL_TABLE_NT.into_iter().map(|s| s.to_string()).collect()
            )
        )
    }

    // [write_source_code_for_integration_listener6]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod lr_parser_2000 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener7]

    use lexigram_lib::{LALR, VarId, fixed_sym_table::FixedSymTable, parser::lr_parser::{LRAction, LRParser, LRStateId}};

    static NUM_NT: usize = 5;
    static NUM_T_FULL: usize = 3;
    static ACTION: [LRAction; 42] = [LRAction::Shift(1), LRAction::Shift(2), LRAction::Error, LRAction::Shift(4), LRAction::Error, LRAction::Error, LRAction::Shift(7), LRAction::Error, LRAction::Error, LRAction::Error, LRAction::Error, LRAction::Accept, LRAction::Reduce(4), LRAction::Shift(9), LRAction::Error, LRAction::Shift(10), LRAction::Error, LRAction::Error, LRAction::Reduce(6), LRAction::Reduce(6), LRAction::Error, LRAction::Error, LRAction::Reduce(4), LRAction::Error, LRAction::Error, LRAction::Shift(13), LRAction::Error, LRAction::Error, LRAction::Error, LRAction::Reduce(1), LRAction::Error, LRAction::Error, LRAction::Reduce(0), LRAction::Reduce(3), LRAction::Reduce(3), LRAction::Error, LRAction::Reduce(5), LRAction::Reduce(5), LRAction::Error, LRAction::Error, LRAction::Error, LRAction::Reduce(2)];
    static GOTO: [LRStateId; 70] = [3, 14, 14, 14, 14, 14, 5, 6, 14, 14, 14, 8, 6, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 11, 12, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14];
    static ALT_NT_LEN: [(VarId, u16, u16); 8] = [(0, 3, 0), (0, 3, 0), (0, 3, 0), (1, 2, 0), (2, 1, 0), (3, 1, 0), (4, 0, 0), (5, 1, 0)];
    static SYMBOL_TABLE_T: [(&str, Option<&str>); 2] = [("A", Some("a")), ("B", Some("b"))];
    static SYMBOL_TABLE_NT: [&str; 6] = ["s", "a", "b", "c", "d", "<goal>"];

    pub fn build_parser(n: u32) -> LRParser<'static, LALR> {
        LRParser::new(
            NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,
            FixedSymTable::new(
                SYMBOL_TABLE_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),
                SYMBOL_TABLE_NT.into_iter().map(|s| s.to_string()).collect()
            )
        )
    }

    // [write_source_code_for_integration_listener7]
    // -------------------------------------------------------------------------
}