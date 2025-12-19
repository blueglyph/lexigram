// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

/*!
```
// ---------------------------------------------------------------------------------------------
// [lexer_lexicon]

lexicon Tst;

Equal                   : '=';
LSbracket               : '{';
RSbracket               : '}';
Semi                    : ';';
Print                   : 'print';
Call                    : 'call';
Id                      : [a-z][a-z0-9]*;
Num                     : [0-9]+;

SkipWhiteSpace          : [ \n\r\t]+                -> skip;

// [lexer_lexicon]
// ---------------------------------------------------------------------------------------------
// [parser_grammar]

grammar Tst;

script:
    LSbracket instruction* RSbracket
;
instruction:
    Id Equal Num Semi
|   Print Id Semi
|   Call Id Semi
;

// [parser_grammar]
// ---------------------------------------------------------------------------------------------
```
*/

#![cfg(test)]
#[cfg(feature = "generated_tests")]

mod lexer {
    // [lexer_source]

    // This code is generated from lexigram\src\tests.rs
    // and corresponds to the lexicon above between tags [lexer_lexicon]

    use std::collections::HashMap;
    use std::io::Read;
    use crate::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use crate::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 15;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 1;
    const NBR_STATES: StateId = 17;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         15,  15,  15,  15,  15,  15,  15,  15,  15,   0,   0,  15,  15,   0,  15,  15,   // 0-15
         15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,   // 16-31
          0,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,   // 32-47
          1,   1,   1,   1,   1,   1,   1,   1,   1,   1,  15,   2,  15,   3,  15,  15,   // 48-63
         15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,   // 64-79
         15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,   // 80-95
         15,   9,   4,   5,   4,   4,   4,   4,   4,  11,   4,   4,  14,   4,  12,   4,   // 96-111
          6,   4,  10,   4,  13,   4,   4,   4,   4,   4,   4,   7,  15,   8,  15,  15,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 0] = [
    ];
    static TERMINAL_TABLE: [Terminal;16] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 256] = [
          1,   2,   3,   4,   5,   6,   7,   8,   9,   5,   5,   5,   5,   5,   5, // state 0
          1,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17, // state 1 <skip>
         17,   2,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17, // state 2 <end:7>
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17, // state 3 <end:3>
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17, // state 4 <end:0>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,   5,   5,   5, // state 5 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,  14,   5,   5,   5,   5,   5, // state 6 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,  10,   5,   5,   5,   5, // state 7 <end:6>
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17, // state 8 <end:1>
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17, // state 9 <end:2>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,  11,   5,   5,   5, // state 10 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,  12,   5,   5, // state 11 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,   5,  13,   5, // state 12 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,   5,   5,   5, // state 13 <end:4>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,   5,   5,  15, // state 14 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,   5,   5,  16, // state 15 <end:6>
         17,   5,  17,  17,   5,   5,   5,  17,  17,   5,   5,   5,   5,   5,   5, // state 16 <end:5>
         17 // error group in [nbr_state * nbr_group + nbr_group]
    ];

    pub fn build_lexer<R: Read>() -> Lexer<'static, R> {
        Lexer::new(
            // parameters
            NBR_GROUPS,
            INITIAL_STATE,
            FIRST_END_STATE,
            NBR_STATES,
            // tables
            &ASCII_TO_GROUP,
            HashMap::<char, GroupId>::from(UTF8_TO_GROUP),
            SegMap::<GroupId>::from(SEG_TO_GROUP),
            &STATE_TABLE,
            &TERMINAL_TABLE,
        )
    }

    // [lexer_source]
}

mod parser {
    // [parser_source]

    // This code is generated with lexigram version 0.8.0 from lexigram\src\tests.rs
    // and corresponds to the grammar above between tags [parser_grammar]

    use crate::{AltId, VarId, fixed_sym_table::FixedSymTable, parser::{OpCode, Parser}};

    const PARSER_NUM_T: usize = 8;
    const PARSER_NUM_NT: usize = 3;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Equal", Some("=")), ("LSbracket", Some("{")), ("RSbracket", Some("}")), ("Semi", Some(";")), ("Print", Some("print")), ("Call", Some("call")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["script", "instruction", "script_1"];
    static ALT_VAR: [VarId; 6] = [0, 1, 1, 1, 2, 2];
    static PARSING_TABLE: [AltId; 27] = [6, 0, 6, 6, 6, 6, 6, 6, 7, 6, 6, 7, 6, 2, 3, 1, 6, 6, 6, 6, 5, 6, 4, 4, 4, 6, 6];
    static OPCODES: [&[OpCode]; 6] = [&[OpCode::Exit(0), OpCode::T(2), OpCode::NT(2), OpCode::T(1)], &[OpCode::Exit(1), OpCode::T(3), OpCode::T(7), OpCode::T(0), OpCode::T(6)], &[OpCode::Exit(2), OpCode::T(3), OpCode::T(6), OpCode::T(4)], &[OpCode::Exit(3), OpCode::T(3), OpCode::T(6), OpCode::T(5)], &[OpCode::Loop(2), OpCode::Exit(4), OpCode::NT(1)], &[OpCode::Exit(5)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            Vec::new(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }

    // [parser_source]
}

// mod tok {
//     use crate::TokenId;
//
//     pub const EQUAL      : TokenId = 0;     // '='
//     pub const LSBRACKET  : TokenId = 1;     // '{'
//     pub const RSBRACKET  : TokenId = 2;     // '}'
//     pub const SEMI       : TokenId = 3;     // ';'
//     pub const PRINT      : TokenId = 4;     // 'print'
//     pub const CALL       : TokenId = 5;     // 'call'
//     pub const ID         : TokenId = 6;     // [a-z][a-z0-9]*
//     pub const NUM        : TokenId = 7;     // [0-9]+
// }

mod wrapper {
    use crate::parser::{Call, ListenerWrapper, Symbol};
    use crate::{AltId, VarId};
    use crate::fixed_sym_table::FixedSymTable;
    use crate::lexer::PosSpan;
    use crate::log::{BufLog, Logger};

    pub struct Wrapper {
        log: BufLog,
        symtable: Option<FixedSymTable>,
    }

    impl Wrapper {
        pub fn new(symtable: Option<FixedSymTable>) -> Self {
            Wrapper {
                log: BufLog::new(),
                symtable,
            }
        }
    }

    impl ListenerWrapper for Wrapper {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            // nt alt  rule
            // ----------------------------------------
            // 0   0   script -> "{" script_1 "}"
            // 2   4   script_1 -> instruction script_1
            // 2   5   script_1 -> ε
            // 1   1   instruction -> Id "=" Num ";"
            // 1   2   instruction -> "print" Id ";"
            // 1   3   instruction -> "call" Id ";"
            //
            println!("- switch(call={call:?}, nt={}, alt_id={alt_id}, t_data={t_data:?}", Symbol::NT(nt).to_str(self.symtable.as_ref()));
            match call {
                Call::Enter => {
                    match nt {
                        0 => {} // script
                        1 => {} // instruction
                        2 => {} // script_1
                        _ => panic!("unexpected nt {nt} in Enter"),
                    }
                }
                Call::Loop => {
                    match nt {
                        2 => {} // script_1
                        _ => panic!("unexpected nt {nt} in Loop"),
                    }
                }
                Call::Exit => {
                    match alt_id {
                        0 => { //  script -> "{" script_1 "}"
                        }
                        4 => { //  script_1 -> instruction script_1
                        }
                        5 => { //  script_1 -> ε
                        }
                        1 => { //  instruction -> Id "=" Num ";"
                        }
                        2 => { //  instruction -> "print" Id ";"
                        }
                        3 => { //  instruction -> "call" Id ";"
                        }
                        _ => panic!("unexpected alt_id {alt_id} in Exit"),
                    }
                }
                Call::End => {
                    assert_eq!(alt_id, 0, "unexpected alt_id {alt_id} in End");
                }
            }
        }

        fn check_abort_request(&self) -> bool {
            false
        }

        fn abort(&mut self) {
            panic!("abort");
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn push_span(&mut self, span: PosSpan) {
            println!("- push(span={span})");
        }
    }
}

mod simple {
    use std::io::Cursor;
    use crate::char_reader::CharReader;
    use crate::lexer::TokenSpliterator;
    use super::lexer::*;
    use super::parser::*;
    use super::wrapper::*;

    #[test]
    fn calls() {
        let mut lexer = build_lexer();
        let mut parser = build_parser();
        let symtable = parser.get_symbol_table().cloned();
        let mut wrapper = Wrapper::new(symtable);

        let text = "{ a = 10; print a; call exit; }";
        let stream = CharReader::new(Cursor::new(text));
        lexer.attach_stream(stream);
        let token_stream = lexer.tokens().keep_channel0();
        match parser.parse_stream(&mut wrapper, token_stream) {
            Ok(_) => {
                println!("parsing successful")
            }
            Err(e) => {
                panic!("parsing failed:\n{e}")
            }
        }
    }
}

// use crate::log::{BufLog, BuildFrom, LogStatus, Logger};
// use crate::parser::{ListenerWrapper, OpCode, Parser, Symbol};
// use lexigram_lib::LL1;
// use lexigram_lib::grammar::ProdRuleSet;
// use lexigram_lib::grammar::tests::old_build_rts_prs::T;
// use lexigram_lib::grammar::tests::old_build_rts_prs::build_prs;
// use lexigram_lib::parsergen::{ParserGen, ParserTables};

// ---------------------------------------------------------------------------------------------

// #[test]
// fn parser_parse_stream() {
//
//     struct Stub(BufLog);
//     impl ListenerWrapper for Stub {
//         fn get_mut_log(&mut self) -> &mut impl Logger {
//             &mut self.0
//         }
//     }
//
//     let tests = vec![
//         (4, 0, vec![
//             ("I*I", true),
//             ("(N)", true),
//             ("((N))", true),
//         ]),
//         (5, 0, vec![
//             ("++;;", true),
//             ("--+;;", true),
//             ("+-;;", false),
//             ("++;;-", false),
//             ("++;-", false),
//             ("-", false),
//         ]),
//         (8, 0, vec![ // ambiguous grammar but that should work
//             ("b a b a b", true),
//             ("b", true),
//         ]),
//         (16, 0, vec![ // A -> B A | b ; B -> a
//             ("aaab", true),
//         ]),
//         (17, 0, vec![
//             ("(((a)))", true),
//             ("((a)", false),
//             ("((a)))", false),
//         ]),
//         (18, 0, vec![
//             ("a", true),
//             ("", false),
//             ("aa", false),
//         ]),
//         (19, 0, vec![
//             ("a", true),
//             ("", true),
//             ("aa", false),
//         ]),
//     ];
//     const VERBOSE: bool = false;
//     for (test_id, (ll_id, start, sequences)) in tests.into_iter().enumerate() {
//         if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id}/{start}", ""); }
//         let mut ll1 = ProdRuleSet::<LL1>::build_from(build_prs(ll_id, false));
//         ll1.set_start(start);
//         let symbols = (0..ll1.get_num_t() as TokenId)
//             .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
//             .collect::<HashMap<_, _>>();
//         let parser_tables = ParserTables::build_from(ParserGen::build_from_rules(ll1, "Test".to_string()));
//         let mut parser = parser_tables.make_parser();
//         for (input, expected_success) in sequences {
//             if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
//             let stream = input.chars().into_iter().index_start::<CaretCol>(1).filter_map(|(i, c)| {
//                 if c.is_ascii_whitespace() {
//                     None
//                 } else {
//                     let c_str = c.to_string();
//                     if let Some(s) = symbols.get(&c_str) {
//                         // println!("stream: '{}' -> sym!({})", c, symbol_to_macro(s));
//                         let pos = Pos(1, i);
//                         Some((*s, c_str, PosSpan::new(pos, pos)))
//                     } else {
//                         panic!("unrecognized test input '{c}' in test {test_id}/{ll_id}/{start}, input {input}");
//                     }
//                 }
//             });
//             let mut listener = Stub(BufLog::new());
//             let success = match parser.parse_stream(&mut listener, stream) {
//                 Ok(_) => {
//                     if VERBOSE { println!("parsing completed successfully"); }
//                     true
//                 }
//                 Err(e) => {
//                     if VERBOSE { println!("parsing failed: {e}"); }
//                     false
//                 }
//             };
//             if VERBOSE {
//                 let msg = listener.0.get_messages().map(|s| format!("- {s:?}")).join("\n");
//                 if !msg.is_empty() {
//                     println!("Messages:\n{msg}");
//                 }
//             }
//             assert_eq!(success, expected_success, "test {test_id}/{ll_id}/{start} failed for input {input}");
//         }
//     }
// }
//
// #[test]
// fn parser_parse_stream_id() {
//
//     struct Stub(BufLog);
//     impl ListenerWrapper for Stub {
//         fn get_mut_log(&mut self) -> &mut impl Logger {
//             &mut self.0
//         }
//     }
//
//     let tests = vec![
//         (T::RTS(9), 0, 2, 999, vec![
//             ("var a , b ,", None),
//         ]),
//         (T::RTS(23), 0, 999, 999, vec![
//             // A -> a (b)+ c
//             ("a b b c", None),
//         ]),
//         (T::RTS(27), 0, 999, 999, vec![
//             // A -> a (b)+ c
//             ("a b b c", None),
//         ]),
//         (T::PRS(20), 0, 5, 999, vec![
//             ("struct test1 { a : int ; b : string ; c : bool ; }", None),
//             ("struct test2 { a : int ; b : string ; c : bool }", Some(vec![
//                 "syntax error: found input '}' instead of ';', line 1, col 15"
//             ])),
//         ]),
//         (T::PRS(33), 0, 999, 999, vec![
//             // A -> A a | b c | b d
//             ("b c a a", None),
//             ("b d a a", None),
//             ("b c", None),
//             ("b d", None),
//         ]),
//         (T::PRS(43), 0, 7, 6, vec![
//             // BATCH -> GROUP ';' BATCH <L> | ε
//             // GROUP -> '[' EXPR ']' | '(' EXPR ')'
//             // EXPR -> FACTOR '*' FACTOR;
//             // FACTOR -> id | int | '(' EXPR ')';
//             ("[ 1 * 2 ] ;", None),
//             ("[ ( 1 * 2 * 3 ] ;", Some(vec![
//                 "syntax error: found input '*' instead of ')', line 1, col 6"
//             ])),
//             ("[ 1 * 2 ; [ ( 3 * 4 ) * ] ; [ 5 * 6 ] ;", Some(vec![
//                 "syntax error: found input ';' instead of ']', line 1, col 5",
//                 "syntax error: found input ']' instead of '(', 'id', 'int' while parsing '►FACTOR', line 1, col 13"
//             ])),
//         ]),
//         (T::PRS(51), 0, 8, 7, vec![
//             // E -> 'abs' E | E '^' E | E '*' E | '-' E | E '+' E | F;
//             // F -> ( E ) | NUM | ID
//             ("1 ^ 2", None),
//             ("1 + 2 * 3 + 4 ^ 5 * 6 + 7 ^ 8", None),
//             ("2 ' ^ 3", None),
//             ("- 4 * 3", None),
//             ("3 * - 4", None),
//             ("( 1 + 2 ) * ( 3 + - abs i * - 5 + 6 ) ^ 2", None)
//         ]),
//         (T::PRS(61), 0, 99, 99, vec![
//             ("- 0 +", None),
//             ("- - 1 + +", None),
//             ("- 0", None),
//             ("1 +", None),
//             ("0", None),
//             ("1", None),
//         ]),
//         (T::PRS(63), 0, 4, 99, vec![
//             ("a * b", None),
//             ("a + b", None),
//             ("- a", None),
//             ("a * b + c", None),
//             ("a + b * c", None),
//             ("- a * b", None),
//             ("a * - b", None),
//         ]),
//         (T::PRS(100), 0, 999, 999, vec![
//             ("c a c a c b b", None),
//             ("c a c b a c b", None),
//         ]),
//     ];
//     const VERBOSE: bool = false;
//     for (test_id, (ll_id, start, id_id, num_id, sequences)) in tests.into_iter().enumerate() {
//         if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id:?}/{start}", ""); }
//         let ll1 = ll_id.build_prs(test_id, start, false);
//         let symbols = (0..ll1.get_num_t() as TokenId)
//             .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
//             .collect::<HashMap<_, _>>();
//         let parser_tables = ParserTables::build_from(ParserGen::build_from_rules(ll1, "Test".to_string()));
//         let mut parser = parser_tables.make_parser();
//         for (input, expected_errors) in sequences {
//             if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
//             let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
//                 let pos = Pos(1, i);
//                 let pos_span = PosSpan::new(pos, pos);
//                 if let Some(s) = symbols.get(w) {
//                     (*s, w.to_string(), pos_span)
//                 } else {
//                     if w.chars().next().unwrap().is_ascii_digit() {
//                         (num_id, w.to_string(), pos_span)
//                     } else {
//                         (id_id, w.to_string(), pos_span)
//                     }
//                 }
//             });
//             let mut listener = Stub(BufLog::new());
//             let errors = match parser.parse_stream(&mut listener, stream) {
//                 Ok(_) => {
//                     if VERBOSE { println!("parsing completed successfully"); }
//                     None
//                 }
//                 Err(e) => {
//                     if VERBOSE { println!("parsing failed: {e}"); }
//                     Some(listener.0.get_errors().map(|s| s.as_str()).to_vec())
//                 }
//             };
//             if VERBOSE {
//                 let msg = listener.0.get_messages().map(|s| format!("- {s:?}")).join("\n");
//                 if !msg.is_empty() {
//                     println!("Messages:\n{msg}");
//                 }
//             }
//             assert_eq!(errors, expected_errors, "test {test_id}/{ll_id:?}/{start} failed for input {input}");
//         }
//     }
// }
