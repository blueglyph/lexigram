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

    // This code is generated from lexi-gram\src\tests.rs
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

    // This code is generated with lexigram version 0.8.3 from lexi-gram\src\tests.rs
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

const VERBOSE: bool = false;

mod wrapper {
    use std::collections::HashMap;
    use std::str::FromStr;
    use crate::parser::{Call, ListenerWrapper};
    use crate::{AltId, CollectJoin, VarId};
    use crate::fixed_sym_table::FixedSymTable;
    use crate::lexer::PosSpan;
    use crate::log::{BufLog, Logger};
    use crate::parser::tests::VERBOSE;

    pub type Id = u32;
    pub type Num = i32;

    #[derive(Clone, Copy, PartialEq, Debug)]
    pub enum Inst {
        Let(Id, Num),
        Print(Id),
        Call(Id),
    }

    #[derive(Debug)]
    pub struct Wrapper {
        pub log: BufLog,
        pub symtable: Option<FixedSymTable>,
        pub script: Option<Vec<Inst>>,
        pub curr_insts: Option<Vec<Inst>>,
        pub curr_inst: Option<Inst>,
        pub ids: HashMap<String, Id>,
        pub stack_span: Vec<PosSpan>,
        pub loop_init: Option<bool>,
        pub parsed_span: Option<PosSpan>,
        pub span_reprs: Vec<String>
    }

    impl Wrapper {
        pub fn new(symtable: Option<FixedSymTable>) -> Self {
            Wrapper {
                log: BufLog::new(),
                symtable,
                script: None,
                curr_insts: None,
                curr_inst: None,
                ids: HashMap::new(),
                stack_span: Vec::new(),
                loop_init: None,
                parsed_span: None,
                span_reprs: Vec::new(),
            }
        }

        fn collect_spans(&mut self, n: usize) -> (PosSpan, String) {
            let mut total = PosSpan::empty();
            if VERBOSE { print!("- collect {n} spans (stack has {}) -> ", self.stack_span.len()) }
            let repr = self.stack_span.drain(self.stack_span.len() - n..)
                .map(|span| {
                    total += &span;
                    span.to_string()
                })
                .join(", ");
            if VERBOSE { println!("{repr} = {total}"); }
            (total, repr)
        }
    }

    impl ListenerWrapper for Wrapper {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, mut t_data: Option<Vec<String>>) {
            // nt alt  rule
            // ----------------------------------------
            // 0   0   script -> "{" script_1 "}"
            // 2   4   script_1 -> instruction script_1
            // 2   5   script_1 -> ε
            // 1   1   instruction -> Id "=" Num ";"
            // 1   2   instruction -> "print" Id ";"
            // 1   3   instruction -> "call" Id ";"
            //
            if VERBOSE {
                println!(
                    "- switch(call={call:?}, nt={}, alt_id={alt_id}, t_data={t_data:?}",
                    crate::parser::Symbol::NT(nt).to_str(self.symtable.as_ref()));
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => {  // script
                            assert_eq!(self.curr_insts, None, "unexpected curr_insts in Enter(script)");
                            assert_eq!(self.script, None, "unexpected script in Enter(script)");
                        }
                        1 => {  // instruction
                            assert_eq!(self.curr_inst, None, "unexpected curr_inst in Enter(instruction)");
                            self.curr_inst = None
                        },
                        2 => {  // script_1
                            assert_eq!(self.curr_insts, None, "unexpected curr_insts in Enter(script_1)");
                            assert_eq!(self.loop_init, None, "unexpected init_loop in Enter(script_1)");
                            self.curr_insts = Some(Vec::new());
                            self.loop_init = Some(true);
                            self.stack_span.push(PosSpan::empty());
                        },
                        _ => panic!("unexpected nt {nt} in Enter"),
                    }
                }
                Call::Loop => {
                    match nt {
                        2 => {
                            assert_eq!(self.loop_init, Some(false), "unexpected loop_init in Loop(script_1)");
                            self.loop_init = Some(true);
                        } // script_1
                        _ => panic!("unexpected nt {nt} in Loop"),
                    }
                }
                Call::Exit => {
                    match alt_id {
                        0 => { //  script -> "{" script_1 "}"
                            assert_eq!(self.loop_init, None, "unexpected loop_init in Exit(script)");
                            let script = self.curr_insts.take().unwrap();
                            self.script = Some(script);
                            let (final_span, _) = self.collect_spans(3);
                            self.parsed_span = Some(final_span);
                            assert_eq!(self.stack_span.len(), 0, "stack: {}", self.stack_span.iter().map(|s| s.to_string()).join(", "));
                        }
                        4 => { //  script_1 -> instruction script_1
                            assert_eq!(self.loop_init, Some(true), "unexpected loop_init in Exit(script_1)");
                            let inst = self.curr_inst.take().unwrap();
                            self.curr_insts.as_mut().unwrap().push(inst);
                            self.loop_init = Some(false);
                            let (new_span, _) = self.collect_spans(2);
                            self.stack_span.push(new_span);
                        }
                        5 => { //  script_1 -> ε
                            assert_eq!(self.loop_init, Some(true), "unexpected loop_init in Exit(script_1)");
                            self.loop_init = None;
                        }
                        1 => { //  instruction -> Id "=" Num ";"
                            let num_s = t_data.as_mut().unwrap().pop().unwrap();
                            let id_s = t_data.as_mut().unwrap().pop().unwrap();
                            let next_id = self.ids.len();
                            let id = self.ids.entry(id_s)
                                .or_insert_with(|| u32::try_from(next_id).unwrap());
                            self.curr_inst = Some(Inst::Let(*id, i32::from_str(&num_s).expect(&format!("can't convert {num_s} to i32"))));
                            let (new_span, repr) = self.collect_spans(4);
                            self.span_reprs.push(repr);
                            self.stack_span.push(new_span);
                        }
                        2 => { //  instruction -> "print" Id ";"
                            let id_s = t_data.as_mut().unwrap().pop().unwrap();
                            let next_id = self.ids.len();
                            let id = self.ids.entry(id_s)
                                .or_insert_with(|| u32::try_from(next_id).unwrap());
                            self.curr_inst = Some(Inst::Print(*id));
                            let (new_span, repr) = self.collect_spans(3);
                            self.span_reprs.push(repr);
                            self.stack_span.push(new_span);
                        }
                        3 => { //  instruction -> "call" Id ";"
                            let id_s = t_data.as_mut().unwrap().pop().unwrap();
                            let next_id = self.ids.len();
                            let id = self.ids.entry(id_s)
                                .or_insert_with(|| u32::try_from(next_id).unwrap());
                            self.curr_inst = Some(Inst::Call(*id));
                            let (new_span, repr) = self.collect_spans(3);
                            self.span_reprs.push(repr);
                            self.stack_span.push(new_span);
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
            if VERBOSE { println!("- push(span={span})"); }
            self.stack_span.push(span);
        }
    }
}

mod simple {
    use std::io::Cursor;
    use crate::char_reader::CharReader;
    use crate::CollectJoin;
    use crate::lexer::TokenSpliterator;
    use super::VERBOSE;
    use super::lexer::*;
    use super::parser::*;
    use super::wrapper::*;

    #[test]
    fn calls() {
        let mut lexer = build_lexer();
        let mut parser = build_parser();
        let mut symtable = parser.get_symbol_table().cloned();

        let tests = vec![
            (
                "{  }",
                vec![],
                vec![],
                "1:1-4",
            ),
            (
                "{ a = 10; print a; call exit; }",
                vec![Inst::Let(0, 10), Inst::Print(0), Inst::Call(1)],
                vec!["1:3, 1:5, 1:7-8, 1:9", "1:11-15, 1:17, 1:18", "1:20-23, 1:25-28, 1:29"],
                "1:1-31",
            ),
            (
                " { \n  a = 10;\n  bt = 5; \n  print a;\n  call bt ;\n call exit;\n} ",
                vec![Inst::Let(0, 10), Inst::Let(1, 5), Inst::Print(0), Inst::Call(1), Inst::Call(2)],
                vec!["2:3, 2:5, 2:7-8, 2:9", "3:3-4, 3:6, 3:8, 3:9", "4:3-7, 4:9, 4:10", "5:3-6, 5:8-9, 5:11", "6:2-5, 6:7-10, 6:11"],
                "1:2-7:1",
            ),
        ];
        for (id, (text, expected_insts, expected_reprs, expected_span)) in tests.into_iter().enumerate() {
            let stream = CharReader::new(Cursor::new(text));
            lexer.attach_stream(stream);
            let token_stream = lexer.tokens().keep_channel0();
            let mut wrapper = Wrapper::new(symtable);
            match parser.parse_stream(&mut wrapper, token_stream) {
                Ok(_) => {
                    if VERBOSE {
                        println!("parsing successful");
                        println!("{:?}", wrapper.script.as_ref().unwrap());
                        println!("repr:\n{}", wrapper.span_reprs.iter().map(|r| format!("- {r}")).join("\n"));
                    }
                    assert_eq!(wrapper.script.as_ref().unwrap(), &expected_insts, "test {id} failed: script");
                    assert_eq!(wrapper.span_reprs, expected_reprs, "test {id} failed: reprs");
                    assert_eq!(wrapper.parsed_span.as_ref().unwrap().to_string(), expected_span, "test {id} failed: span");
                }
                Err(e) => {
                    panic!("test {id} failed: parsing\n{e}");
                }
            }
            symtable = wrapper.symtable.take();
        }
    }
}
