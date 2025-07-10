// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]
#![allow(non_camel_case_types)]

#[allow(unused)]
mod listener1 {

    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener1]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 8;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("SUB", Some("-")), ("ADD", Some("+")), ("DIV", Some("/")), ("MUL", Some("*")), ("LPAREN", Some("(")), ("RPAREN", Some(")")), ("N", None), ("I", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "T", "F", "E_1", "T_1"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 11] = [(0, &[Symbol::NT(1), Symbol::NT(3)]), (1, &[Symbol::NT(2), Symbol::NT(4)]), (2, &[Symbol::T(4), Symbol::NT(0), Symbol::T(5)]), (2, &[Symbol::T(6)]), (2, &[Symbol::T(7)]), (3, &[Symbol::T(0), Symbol::NT(1), Symbol::NT(3)]), (3, &[Symbol::T(1), Symbol::NT(1), Symbol::NT(3)]), (3, &[Symbol::Empty]), (4, &[Symbol::T(2), Symbol::NT(2), Symbol::NT(4)]), (4, &[Symbol::T(3), Symbol::NT(2), Symbol::NT(4)]), (4, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 45] = [11, 11, 11, 11, 0, 12, 0, 0, 12, 12, 12, 11, 11, 1, 12, 1, 1, 12, 12, 12, 12, 12, 2, 12, 3, 4, 12, 5, 6, 11, 11, 11, 7, 11, 11, 7, 10, 10, 8, 9, 11, 10, 11, 11, 10];
    static FLAGS: [u32; 5] = [512, 512, 0, 4, 4];
    static PARENT: [Option<VarId>; 5] = [None, None, None, Some(0), Some(1)];
    static OPCODES: [&[OpCode]; 11] = [&[OpCode::NT(3), OpCode::Exit(0), OpCode::NT(1)], &[OpCode::NT(4), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2), OpCode::T(5), OpCode::NT(0), OpCode::T(4)], &[OpCode::Exit(3), OpCode::T(6)], &[OpCode::Exit(4), OpCode::T(7)], &[OpCode::Loop(3), OpCode::Exit(5), OpCode::NT(1), OpCode::T(0)], &[OpCode::Loop(3), OpCode::Exit(6), OpCode::NT(1), OpCode::T(1)], &[OpCode::Exit(7)], &[OpCode::Loop(4), OpCode::Exit(8), OpCode::NT(2), OpCode::T(2)], &[OpCode::Loop(4), OpCode::Exit(9), OpCode::NT(2), OpCode::T(3)], &[OpCode::Exit(10)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener1]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener2 {
    use std::collections::HashMap;
    use std::str::FromStr;
    use iter_index::IndexerIterator;
    use lexigram::dfa::TokenId;
    use lexigram::parser::{Call, ListenerWrapper};
    use lexigram::CollectJoin;
    use lexigram::lexer::CaretCol;
    use lexigram::log::{BufLog, Logger};

    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener2]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 10;
    const PARSER_NUM_NT: usize = 8;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("ABS", Some("abs")), ("NEG", Some("-")), ("EXP", Some("^")), ("MUL", Some("*")), ("ADD", Some("+")), ("LPAREN", Some("(")), ("RPAREN", Some(")")), ("NUM", None), ("ID", None), ("PRIME", Some("'"))];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "F", "E_1", "E_2", "E_3", "E_4", "E_5", "E_6"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 21] = [(0, &[Symbol::NT(7), Symbol::NT(2)]), (1, &[Symbol::T(5), Symbol::NT(0), Symbol::T(6)]), (1, &[Symbol::T(7)]), (1, &[Symbol::T(8)]), (2, &[Symbol::T(2), Symbol::NT(7), Symbol::NT(2)]), (2, &[Symbol::T(9), Symbol::NT(2)]), (2, &[Symbol::T(3), Symbol::NT(5), Symbol::NT(2)]), (2, &[Symbol::T(4), Symbol::NT(3), Symbol::NT(2)]), (2, &[Symbol::Empty]), (3, &[Symbol::NT(7), Symbol::NT(4)]), (4, &[Symbol::T(2), Symbol::NT(7), Symbol::NT(4)]), (4, &[Symbol::T(9), Symbol::NT(4)]), (4, &[Symbol::T(3), Symbol::NT(5), Symbol::NT(4)]), (4, &[Symbol::Empty]), (5, &[Symbol::NT(7), Symbol::NT(6)]), (6, &[Symbol::T(2), Symbol::NT(7), Symbol::NT(6)]), (6, &[Symbol::T(9), Symbol::NT(6)]), (6, &[Symbol::Empty]), (7, &[Symbol::T(1), Symbol::NT(3)]), (7, &[Symbol::T(0), Symbol::NT(7)]), (7, &[Symbol::NT(1)])];
    static PARSING_TABLE: [FactorId; 88] = [0, 0, 21, 21, 21, 0, 22, 0, 0, 21, 22, 21, 21, 22, 22, 22, 1, 22, 2, 3, 22, 22, 21, 21, 4, 6, 7, 21, 8, 21, 21, 5, 8, 9, 9, 22, 22, 22, 9, 22, 9, 9, 22, 22, 21, 21, 10, 12, 13, 21, 13, 21, 21, 11, 13, 14, 14, 22, 22, 22, 14, 22, 14, 14, 22, 22, 21, 21, 15, 17, 17, 21, 17, 21, 21, 16, 17, 19, 18, 22, 22, 22, 20, 22, 20, 20, 22, 22];
    static FLAGS: [u32; 8] = [1536, 0, 4, 512, 4, 512, 4, 2];
    static PARENT: [Option<VarId>; 8] = [None, None, Some(0), Some(0), Some(3), Some(0), Some(5), Some(0)];
    static OPCODES: [&[OpCode]; 21] = [&[OpCode::NT(2), OpCode::Exit(0), OpCode::NT(7)], &[OpCode::Exit(1), OpCode::T(6), OpCode::NT(0), OpCode::T(5)], &[OpCode::Exit(2), OpCode::T(7)], &[OpCode::Exit(3), OpCode::T(8)], &[OpCode::Loop(2), OpCode::Exit(4), OpCode::NT(7), OpCode::T(2)], &[OpCode::Loop(2), OpCode::Exit(5), OpCode::T(9)], &[OpCode::Loop(2), OpCode::Exit(6), OpCode::NT(5), OpCode::T(3)], &[OpCode::Loop(2), OpCode::Exit(7), OpCode::NT(3), OpCode::T(4)], &[OpCode::Exit(8)], &[OpCode::NT(4), OpCode::Exit(9), OpCode::NT(7)], &[OpCode::Loop(4), OpCode::Exit(10), OpCode::NT(7), OpCode::T(2)], &[OpCode::Loop(4), OpCode::Exit(11), OpCode::T(9)], &[OpCode::Loop(4), OpCode::Exit(12), OpCode::NT(5), OpCode::T(3)], &[OpCode::Exit(13)], &[OpCode::NT(6), OpCode::Exit(14), OpCode::NT(7)], &[OpCode::Loop(6), OpCode::Exit(15), OpCode::NT(7), OpCode::T(2)], &[OpCode::Loop(6), OpCode::Exit(16), OpCode::T(9)], &[OpCode::Exit(17)], &[OpCode::Exit(18), OpCode::NT(3), OpCode::T(1)], &[OpCode::Exit(19), OpCode::NT(7), OpCode::T(0)], &[OpCode::Exit(20), OpCode::NT(1)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener2]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener3 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener3]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 6;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("struct", Some("struct")), ("{", Some("{")), ("}", Some("}")), (":", Some(":")), (";", Some(";")), ("id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["STRUCT", "LIST"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 3] = [(0, &[Symbol::T(0), Symbol::T(5), Symbol::T(1), Symbol::NT(1)]), (1, &[Symbol::T(5), Symbol::T(3), Symbol::T(5), Symbol::T(4), Symbol::NT(1)]), (1, &[Symbol::T(2)])];
    static PARSING_TABLE: [FactorId; 14] = [0, 3, 3, 3, 3, 3, 4, 3, 3, 2, 3, 3, 1, 4];
    static FLAGS: [u32; 2] = [0, 2];
    static PARENT: [Option<VarId>; 2] = [None, None];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::Exit(0), OpCode::NT(1), OpCode::T(1), OpCode::T(5), OpCode::T(0)], &[OpCode::Exit(1), OpCode::NT(1), OpCode::T(4), OpCode::T(5), OpCode::T(3), OpCode::T(5)], &[OpCode::Exit(2), OpCode::T(2)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener3]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener4 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener4]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 6;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("struct", Some("struct")), ("{", Some("{")), ("}", Some("}")), (":", Some(":")), (";", Some(";")), ("id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["STRUCT", "LIST"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 3] = [(0, &[Symbol::T(0), Symbol::T(5), Symbol::T(1), Symbol::NT(1)]), (1, &[Symbol::T(5), Symbol::T(3), Symbol::T(5), Symbol::T(4), Symbol::NT(1)]), (1, &[Symbol::T(2)])];
    static PARSING_TABLE: [FactorId; 14] = [0, 3, 3, 3, 3, 3, 4, 3, 3, 2, 3, 3, 1, 4];
    static FLAGS: [u32; 2] = [0, 130];
    static PARENT: [Option<VarId>; 2] = [None, None];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::Exit(0), OpCode::NT(1), OpCode::T(1), OpCode::T(5), OpCode::T(0)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::T(4), OpCode::T(5), OpCode::T(3), OpCode::T(5)], &[OpCode::Exit(2), OpCode::T(2)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener4]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener5 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener5]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 2;
    const PARSER_NUM_NT: usize = 3;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [(".", Some(".")), ("id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "F", "E_1"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 4] = [(0, &[Symbol::NT(1), Symbol::NT(2)]), (1, &[Symbol::T(1)]), (2, &[Symbol::T(0), Symbol::T(1), Symbol::NT(2)]), (2, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 9] = [4, 0, 5, 5, 1, 5, 2, 4, 3];
    static FLAGS: [u32; 3] = [512, 0, 4];
    static PARENT: [Option<VarId>; 3] = [None, None, Some(0)];
    static OPCODES: [&[OpCode]; 4] = [&[OpCode::NT(2), OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::T(1)], &[OpCode::Loop(2), OpCode::Exit(2), OpCode::T(1), OpCode::T(0)], &[OpCode::Exit(3)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener5]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener6 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener6]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 4;
    const PARSER_NUM_NT: usize = 4;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [(".", Some(".")), ("id", None), ("(", Some("(")), (")", Some(")"))];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "F", "E_1", "E_2"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 6] = [(0, &[Symbol::NT(1), Symbol::NT(2)]), (1, &[Symbol::T(1)]), (2, &[Symbol::T(0), Symbol::T(1), Symbol::NT(3)]), (2, &[Symbol::Empty]), (3, &[Symbol::T(2), Symbol::T(3), Symbol::NT(2)]), (3, &[Symbol::NT(2)])];
    static PARSING_TABLE: [FactorId; 20] = [6, 0, 6, 6, 7, 7, 1, 6, 6, 7, 2, 6, 6, 6, 3, 5, 6, 4, 6, 5];
    static FLAGS: [u32; 4] = [512, 0, 36, 64];
    static PARENT: [Option<VarId>; 4] = [None, None, Some(0), Some(2)];
    static OPCODES: [&[OpCode]; 6] = [&[OpCode::NT(2), OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::T(1)], &[OpCode::NT(3), OpCode::T(1), OpCode::T(0)], &[OpCode::Exit(3)], &[OpCode::Loop(2), OpCode::Exit(4), OpCode::T(3), OpCode::T(2)], &[OpCode::Loop(2), OpCode::Exit(5)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener6]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener7 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener7]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("a", None), ("b", None), ("c", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["A", "A_1"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 3] = [(0, &[Symbol::T(0), Symbol::NT(1), Symbol::T(2)]), (1, &[Symbol::T(1), Symbol::NT(1)]), (1, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 8] = [0, 3, 3, 4, 3, 1, 2, 3];
    static FLAGS: [u32; 2] = [2048, 1];
    static PARENT: [Option<VarId>; 2] = [None, Some(0)];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::Exit(0), OpCode::T(2), OpCode::NT(1), OpCode::T(0)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::T(1)], &[OpCode::Exit(2)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener7]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener8 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener8]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("a", None), ("b", None), ("c", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["A", "AIter1"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 3] = [(0, &[Symbol::T(0), Symbol::NT(1), Symbol::T(2)]), (1, &[Symbol::T(1), Symbol::NT(1)]), (1, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 8] = [0, 3, 3, 4, 3, 1, 2, 3];
    static FLAGS: [u32; 2] = [2048, 129];
    static PARENT: [Option<VarId>; 2] = [None, Some(0)];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::Exit(0), OpCode::T(2), OpCode::NT(1), OpCode::T(0)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::T(1)], &[OpCode::Exit(2)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener8]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener9 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener9]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 4;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("a", None), ("b", None), ("c", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["A", "A_1", "A_2", "A_3"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 6] = [(0, &[Symbol::T(0), Symbol::NT(2)]), (1, &[Symbol::T(2), Symbol::NT(3)]), (2, &[Symbol::NT(1), Symbol::T(1), Symbol::NT(2)]), (2, &[Symbol::Empty]), (3, &[Symbol::NT(1)]), (3, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 16] = [0, 6, 6, 7, 6, 7, 1, 6, 6, 6, 2, 3, 6, 5, 4, 6];
    static FLAGS: [u32; 4] = [6656, 4129, 4, 64];
    static PARENT: [Option<VarId>; 4] = [None, Some(0), Some(0), Some(1)];
    static OPCODES: [&[OpCode]; 6] = [&[OpCode::NT(2), OpCode::Exit(0), OpCode::T(0)], &[OpCode::NT(3), OpCode::T(2)], &[OpCode::Loop(2), OpCode::Exit(2), OpCode::T(1), OpCode::NT(1)], &[OpCode::Exit(3)], &[OpCode::Loop(1), OpCode::Exit(4)], &[OpCode::Exit(5)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener9]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener10 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener10]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 3;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("a", None), ("b", None), ("c", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["A", "A_1", "A_2"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 4] = [(0, &[Symbol::T(0), Symbol::NT(1), Symbol::T(2)]), (1, &[Symbol::T(1), Symbol::NT(2)]), (2, &[Symbol::NT(1)]), (2, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 12] = [0, 4, 4, 5, 4, 1, 5, 4, 4, 2, 3, 4];
    static FLAGS: [u32; 3] = [6144, 4129, 64];
    static PARENT: [Option<VarId>; 3] = [None, Some(0), Some(1)];
    static OPCODES: [&[OpCode]; 4] = [&[OpCode::Exit(0), OpCode::T(2), OpCode::NT(1), OpCode::T(0)], &[OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2)], &[OpCode::Exit(3)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener10]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener11 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener11]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 4;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("a", None), ("b", None), ("c", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["A", "B", "A_1", "A_2"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 5] = [(0, &[Symbol::T(0), Symbol::NT(2), Symbol::T(2)]), (1, &[Symbol::T(1)]), (2, &[Symbol::NT(1), Symbol::NT(3)]), (3, &[Symbol::NT(2)]), (3, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 16] = [0, 5, 5, 6, 5, 1, 6, 5, 5, 2, 6, 5, 5, 3, 4, 5];
    static FLAGS: [u32; 4] = [6144, 0, 4129, 64];
    static PARENT: [Option<VarId>; 4] = [None, None, Some(0), Some(2)];
    static OPCODES: [&[OpCode]; 5] = [&[OpCode::Exit(0), OpCode::T(2), OpCode::NT(2), OpCode::T(0)], &[OpCode::Exit(1), OpCode::T(1)], &[OpCode::NT(3), OpCode::NT(1)], &[OpCode::Loop(2), OpCode::Exit(3)], &[OpCode::Exit(4)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener11]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener12 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener12]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 4;
    const PARSER_NUM_NT: usize = 3;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("a", None), ("b", None), ("c", None), ("d", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["A", "A_1", "A_2"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 5] = [(0, &[Symbol::T(1), Symbol::NT(2)]), (1, &[Symbol::T(0), Symbol::NT(1)]), (1, &[Symbol::Empty]), (2, &[Symbol::T(2), Symbol::NT(1)]), (2, &[Symbol::T(3), Symbol::NT(1)])];
    static PARSING_TABLE: [FactorId; 15] = [5, 0, 5, 5, 6, 1, 5, 5, 5, 2, 5, 5, 3, 4, 6];
    static FLAGS: [u32; 3] = [544, 4, 64];
    static PARENT: [Option<VarId>; 3] = [None, Some(0), Some(0)];
    static OPCODES: [&[OpCode]; 5] = [&[OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::T(0)], &[OpCode::Exit(2)], &[OpCode::NT(1), OpCode::Exit(3), OpCode::T(2)], &[OpCode::NT(1), OpCode::Exit(4), OpCode::T(3)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener12]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
mod listener13 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener13]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 3;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [(".", Some(".")), ("id", None), ("num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "F", "E_1"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 5] = [(0, &[Symbol::NT(1), Symbol::NT(2)]), (0, &[Symbol::T(2), Symbol::NT(2)]), (1, &[Symbol::T(1)]), (2, &[Symbol::T(0), Symbol::T(1), Symbol::NT(2)]), (2, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 12] = [5, 0, 1, 6, 6, 2, 5, 6, 3, 5, 5, 4];
    static FLAGS: [u32; 3] = [512, 0, 4];
    static PARENT: [Option<VarId>; 3] = [None, None, Some(0)];
    static OPCODES: [&[OpCode]; 5] = [&[OpCode::NT(2), OpCode::Exit(0), OpCode::NT(1)], &[OpCode::NT(2), OpCode::Exit(1), OpCode::T(2)], &[OpCode::Exit(2), OpCode::T(1)], &[OpCode::Loop(2), OpCode::Exit(3), OpCode::T(1), OpCode::T(0)], &[OpCode::Exit(4)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener13]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener14 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener14]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 7;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("EXP", Some("^")), ("MUL", Some("*")), ("NEG", Some("-")), ("ADD", Some("+")), ("ID", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "E_1", "E_2", "E_3", "E_4", "E_5", "E_6"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 14] = [(0, &[Symbol::NT(6), Symbol::NT(1)]), (1, &[Symbol::T(0), Symbol::NT(4), Symbol::NT(1)]), (1, &[Symbol::T(1), Symbol::NT(4), Symbol::NT(1)]), (1, &[Symbol::T(3), Symbol::NT(2), Symbol::NT(1)]), (1, &[Symbol::Empty]), (2, &[Symbol::NT(6), Symbol::NT(3)]), (3, &[Symbol::T(0), Symbol::NT(4), Symbol::NT(3)]), (3, &[Symbol::T(1), Symbol::NT(4), Symbol::NT(3)]), (3, &[Symbol::Empty]), (4, &[Symbol::NT(6), Symbol::NT(5)]), (5, &[Symbol::T(0), Symbol::NT(4), Symbol::NT(5)]), (5, &[Symbol::Empty]), (6, &[Symbol::T(2), Symbol::NT(2)]), (6, &[Symbol::T(4)])];
    static PARSING_TABLE: [FactorId; 42] = [14, 14, 0, 14, 0, 15, 1, 2, 14, 3, 14, 4, 15, 15, 5, 15, 5, 15, 6, 7, 14, 8, 14, 8, 15, 15, 9, 15, 9, 15, 10, 11, 14, 11, 14, 11, 15, 15, 12, 15, 13, 15];
    static FLAGS: [u32; 7] = [1536, 4, 512, 4, 512, 4, 2];
    static PARENT: [Option<VarId>; 7] = [None, Some(0), Some(0), Some(2), Some(0), Some(4), Some(0)];
    static OPCODES: [&[OpCode]; 14] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(6)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(4), OpCode::T(0)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(4), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(2), OpCode::T(3)], &[OpCode::Exit(4)], &[OpCode::NT(3), OpCode::Exit(5), OpCode::NT(6)], &[OpCode::Loop(3), OpCode::Exit(6), OpCode::NT(4), OpCode::T(0)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(4), OpCode::T(1)], &[OpCode::Exit(8)], &[OpCode::NT(5), OpCode::Exit(9), OpCode::NT(6)], &[OpCode::Loop(5), OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11)], &[OpCode::Exit(12), OpCode::NT(2), OpCode::T(2)], &[OpCode::Exit(13), OpCode::T(4)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener14]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener15 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener15]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 8;
    const PARSER_NUM_NT: usize = 8;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("EXP", Some("^")), ("MUL", Some("*")), ("NEG", Some("-")), ("ADD", Some("+")), ("ID", None), ("NUM", None), ("LPAREN", Some("(")), ("RPAREN", Some(")"))];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "F", "E_1", "E_2", "E_3", "E_4", "E_5", "E_6"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 17] = [(0, &[Symbol::NT(7), Symbol::NT(2)]), (1, &[Symbol::T(4)]), (1, &[Symbol::T(5)]), (1, &[Symbol::T(6), Symbol::NT(0), Symbol::T(7)]), (2, &[Symbol::T(0), Symbol::NT(5), Symbol::NT(2)]), (2, &[Symbol::T(1), Symbol::NT(3), Symbol::NT(2)]), (2, &[Symbol::T(3), Symbol::NT(3), Symbol::NT(2)]), (2, &[Symbol::Empty]), (3, &[Symbol::NT(7), Symbol::NT(4)]), (4, &[Symbol::T(0), Symbol::NT(5), Symbol::NT(4)]), (4, &[Symbol::T(1), Symbol::NT(3), Symbol::NT(4)]), (4, &[Symbol::Empty]), (5, &[Symbol::NT(7), Symbol::NT(6)]), (6, &[Symbol::T(0), Symbol::NT(5), Symbol::NT(6)]), (6, &[Symbol::Empty]), (7, &[Symbol::T(2), Symbol::NT(3)]), (7, &[Symbol::NT(1)])];
    static PARSING_TABLE: [FactorId; 72] = [17, 17, 0, 17, 0, 0, 0, 18, 18, 18, 18, 17, 18, 1, 2, 3, 18, 18, 4, 5, 17, 6, 17, 17, 17, 7, 7, 18, 18, 8, 18, 8, 8, 8, 18, 18, 9, 10, 17, 11, 17, 17, 17, 11, 11, 18, 18, 12, 18, 12, 12, 12, 18, 18, 13, 14, 17, 14, 17, 17, 17, 14, 14, 18, 18, 15, 18, 16, 16, 16, 18, 18];
    static FLAGS: [u32; 8] = [1536, 0, 4, 512, 4, 512, 4, 2];
    static PARENT: [Option<VarId>; 8] = [None, None, Some(0), Some(0), Some(3), Some(0), Some(5), Some(0)];
    static OPCODES: [&[OpCode]; 17] = [&[OpCode::NT(2), OpCode::Exit(0), OpCode::NT(7)], &[OpCode::Exit(1), OpCode::T(4)], &[OpCode::Exit(2), OpCode::T(5)], &[OpCode::Exit(3), OpCode::T(7), OpCode::NT(0), OpCode::T(6)], &[OpCode::Loop(2), OpCode::Exit(4), OpCode::NT(5), OpCode::T(0)], &[OpCode::Loop(2), OpCode::Exit(5), OpCode::NT(3), OpCode::T(1)], &[OpCode::Loop(2), OpCode::Exit(6), OpCode::NT(3), OpCode::T(3)], &[OpCode::Exit(7)], &[OpCode::NT(4), OpCode::Exit(8), OpCode::NT(7)], &[OpCode::Loop(4), OpCode::Exit(9), OpCode::NT(5), OpCode::T(0)], &[OpCode::Loop(4), OpCode::Exit(10), OpCode::NT(3), OpCode::T(1)], &[OpCode::Exit(11)], &[OpCode::NT(6), OpCode::Exit(12), OpCode::NT(7)], &[OpCode::Loop(6), OpCode::Exit(13), OpCode::NT(5), OpCode::T(0)], &[OpCode::Exit(14)], &[OpCode::Exit(15), OpCode::NT(3), OpCode::T(2)], &[OpCode::Exit(16), OpCode::NT(1)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener15]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener16 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener16]

    use lexigram::{FixedSymTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("ADD", Some("+")), ("SUB", Some("-")), ("ZERO", Some("0"))];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["E", "E_1"];
    static PARSING_FACTORS: [(VarId, &[Symbol]); 4] = [(0, &[Symbol::T(1), Symbol::NT(0)]), (0, &[Symbol::T(2), Symbol::NT(1)]), (1, &[Symbol::T(0), Symbol::NT(1)]), (1, &[Symbol::Empty])];
    static PARSING_TABLE: [FactorId; 8] = [4, 0, 1, 5, 2, 4, 4, 3];
    static FLAGS: [u32; 2] = [514, 4];
    static PARENT: [Option<VarId>; 2] = [None, Some(0)];
    static OPCODES: [&[OpCode]; 4] = [&[OpCode::Exit(0), OpCode::NT(0), OpCode::T(1)], &[OpCode::NT(1), OpCode::Exit(1), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::T(0)], &[OpCode::Exit(3)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    // [write_source_code_for_integration_listener16]
    // -------------------------------------------------------------------------
}