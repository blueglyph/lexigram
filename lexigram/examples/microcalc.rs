// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::io::Cursor;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexer::{Lexer, TokenSpliterator};
use lexigram_lib::log::{BufLog, LogStatus, Logger};
use lexigram_lib::parser::Parser;
use crate::listener_types::*;
use crate::microcalc_lexer::build_lexer;
use crate::microcalc_parser::*;

const VERBOSE_WRAPPER: bool = false;

static TXT1: &str = r#"
def const() { return 12; }
def a(x) { let y = 2*x; return y; }
def main() { let value = a(const()); print value; return value; }
"#;

fn main() {
    println!("{:=<80}\n{TXT1}\n{0:=<80}", "");
    match MCalc::parse_text(TXT1.to_string()) {
        Ok(log) => println!("parsing successful\n{log}"),
        Err(log) => println!("errors during parsing:\n{log}"),
    }
}

// -------------------------------------------------------------------------
// minimalist parser, top level

pub struct MCalc<'l, 'p> {
    lexer: Lexer<'l, Cursor<String>>,
    parser: Parser<'p>,
    wrapper: Wrapper<MCalcListener>,
}

impl MCalc<'_, '_> {
    pub fn parse_text(text: String) -> Result<BufLog, BufLog> {
        let mut mcalc = MCalc::new();
        mcalc.parse(text)
    }

    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        let wrapper = Wrapper::new(MCalcListener::new(), VERBOSE_WRAPPER);
        MCalc { lexer, parser, wrapper }
    }

    pub fn parse(&mut self, text: String) -> Result<BufLog, BufLog> {
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
            panic!("unexpected channel {ch} while parsing a file, line {line} col {col}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(&mut self.wrapper, tokens) {
            self.wrapper.get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let log = std::mem::take(&mut self.wrapper.get_listener_mut().log);
        if log.has_no_errors() {
            Ok(log)
        } else {
            Err(log)
        }
    }
}

// listener implementation

struct MCalcListener {
    log: BufLog,
}

impl MCalcListener {
    fn new() -> Self {
        MCalcListener { log: BufLog::new() }
    }
}

impl MicroCalcListener for MCalcListener {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit_program(&mut self, _ctx: CtxProgram) -> SynProgram {
        SynProgram()
    }

    fn exit_function(&mut self, _ctx: CtxFunction) -> SynFunction {
        SynFunction()
    }

    fn exit_fun_params(&mut self, _ctx: CtxFunParams) -> SynFunParams {
        SynFunParams()
    }

    fn exit_instruction(&mut self, _ctx: CtxInstruction) -> SynInstruction {
        SynInstruction()
    }

    fn exit_expr(&mut self, _ctx: CtxExpr) -> SynExpr {
        SynExpr()
    }

    fn exit_fun_args(&mut self, _ctx: CtxFunArgs) -> SynFunArgs {
        SynFunArgs()
    }
}

// -------------------------------------------------------------------------
// User types used in the listener interface:
// (initially copied/uncommented from the generated parser code)

pub mod listener_types {
    /// User-defined type for `program`
    #[derive(Debug, PartialEq)] pub struct SynProgram();
    /// User-defined type for `function`
    #[derive(Debug, PartialEq)] pub struct SynFunction();
    /// User-defined type for `fun_params`
    #[derive(Debug, PartialEq)] pub struct SynFunParams();
    /// User-defined type for `instruction`
    #[derive(Debug, PartialEq)] pub struct SynInstruction();
    /// User-defined type for `expr`
    #[derive(Debug, PartialEq)] pub struct SynExpr();
    /// User-defined type for `fun_args`
    #[derive(Debug, PartialEq)] pub struct SynFunArgs();
}

// -------------------------------------------------------------------------

pub mod microcalc_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [microcalc_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_lib::dfa::{StateId, Terminal, ActionOption, ModeOption};
    use lexigram_lib::lexer::Lexer;
    use lexigram_lib::lexergen::GroupId;
    use lexigram_lib::segments::{Seg, SegMap};

    const NBR_GROUPS: u32 = 32;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 6;
    const NBR_STATES: StateId = 41;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         24,  24,  24,  24,  24,  24,  24,  24,  24,   0,  26,  24,  24,  26,  24,  24,   // 0-15
         24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,   // 16-31
          0,  24,  24,  24,  24,  24,  24,  24,   1,   2,   3,   4,   5,   6,  24,   7,   // 32-47
          8,   9,  22,  22,  22,  22,  22,  22,  22,  22,  24,  10,  24,  11,  24,  24,   // 48-63
         24,  20,  20,  20,  20,  20,  20,  23,  23,  23,  23,  23,  23,  23,  23,  23,   // 64-79
         23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  24,  24,  24,  24,  21,   // 80-95
         24,  20,  12,  20,  13,  25,  27,  23,  23,  29,  23,  23,  14,  23,  30,  23,   // 96-111
         15,  23,  16,  23,  28,  31,  23,  23,  19,  23,  23,  17,  24,  18,  24,  24,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 24),
        (Seg(57344, 1114111), 24),
    ];
    static TERMINAL_TABLE: [Terminal;35] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 1313] = [
          6,   7,   8,   9,  10,  11,  12,  13,   1,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  17,  17,  41,  14,  17,  41,  17,   6,  17,  17,  17,  17,  17, // state 0
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,   4,  41,  41,  41,  41,  41,  41,   5,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 1
          2,   2,   2,   3,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 2
          2,   2,   2,   3,   2,   2,   2,  38,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 3
         41,  41,  41,  41,  41,  41,  41,  41,  41,  39,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 4
         41,  41,  41,  41,  41,  41,  41,  41,  41,  40,  41,  41,  40,  40,  41,  41,  41,  41,  41,  41,  40,  41,  40,  41,  41,  40,  41,  40,  41,  41,  41,  41, // state 5
          6,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,   6,  41,  41,  41,  41,  41, // state 6 <skip>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 7 <end:6>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 8 <end:8>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 9 <end:4>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 10 <end:0>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 11 <end:1>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 12 <end:10>
         41,  41,  41,   2,  41,  41,  41,  24,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 13 <end:2>
         41,  41,  41,  41,  41,  41,  41,  41,  14,  14,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  14,  14,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 14 <end:16>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 15 <end:9>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 16 <end:3>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 17 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  25,  41,  17,  17,  17,  17,  17, // state 18 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  27,  41,  17,  17,  17,  17,  17, // state 19 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  29,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 20 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  33,  41,  17,  17,  17,  17,  17, // state 21 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 22 <end:5>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 23 <end:7>
         24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  41,  24,  24,  24,  24,  24, // state 24 <skip>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  26,  17,  17,  17,  17, // state 25 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 26 <end:11>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  28,  17,  17,  17, // state 27 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 28 <end:12>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  30,  17,  17, // state 29 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  31,  17, // state 30 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  32,  17,  17,  17, // state 31 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 32 <end:13>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  34,  17,  17,  17, // state 33 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  35, // state 34 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  36,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 35 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  37,  17, // state 36 <end:15>
         41,  41,  41,  41,  41,  41,  41,  41,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  41,  17,  17,  17,  17,  17,  41,  17,  41,  17,  17,  17,  17,  17, // state 37 <end:14>
         41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 38 <skip>
         41,  41,  41,  41,  41,  41,  41,  41,  39,  39,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41,  39,  41,  41,  41,  41,  41,  41,  41,  41,  41,  41, // state 39 <end:16>
         41,  41,  41,  41,  41,  41,  41,  41,  40,  40,  41,  41,  40,  40,  41,  41,  41,  41,  41,  41,  40,  40,  40,  41,  41,  40,  41,  40,  41,  41,  41,  41, // state 40 <end:16>
         41 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [microcalc_lexer]
}

// -------------------------------------------------------------------------

pub mod microcalc_parser {
    // Generated code, don't modify manually anything between the tags below

    // [microcalc_parser]

    use lexigram_lib::{CollectJoin, FixedSymTable, grammar::{AltId, Alternative, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 17;
    const PARSER_NUM_NT: usize = 17;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Comma", Some(",")), ("Div", Some("/")), ("Equal", Some("=")), ("Mul", Some("*")), ("Lbracket", Some("{")), ("Lpar", Some("(")), ("Rbracket", Some("}")), ("Rpar", Some(")")), ("Semi", Some(";")), ("Sub", Some("-")), ("Def", Some("def")), ("Let", Some("let")), ("Print", Some("print")), ("Return", Some("return")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "function", "fun_params", "instruction", "expr", "fun_args", "program_1", "function_1", "fun_params_1", "fun_args_1", "expr_1", "expr_2", "expr_3", "expr_4", "program_2", "function_2", "expr_5"];
    static ALT_VAR: [VarId; 35] = [0, 1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 7, 8, 8, 9, 9, 10, 10, 10, 10, 10, 11, 12, 12, 12, 13, 13, 13, 13, 14, 14, 15, 15, 16, 16];
    static ALTERNATIVES: [&[Symbol]; 35] = [&[Symbol::NT(6)], &[Symbol::T(11), Symbol::T(15), Symbol::T(6), Symbol::NT(2), Symbol::T(8), Symbol::T(5), Symbol::NT(7), Symbol::T(7)], &[Symbol::T(15), Symbol::NT(8)], &[Symbol::Empty], &[Symbol::T(12), Symbol::T(15), Symbol::T(3), Symbol::NT(4), Symbol::T(9)], &[Symbol::T(14), Symbol::NT(4), Symbol::T(9)], &[Symbol::T(13), Symbol::NT(4), Symbol::T(9)], &[Symbol::NT(13), Symbol::NT(10)], &[Symbol::NT(4), Symbol::NT(9)], &[Symbol::Empty], &[Symbol::NT(1), Symbol::NT(14)], &[Symbol::NT(3), Symbol::NT(15)], &[Symbol::T(1), Symbol::T(15), Symbol::NT(8)], &[Symbol::Empty], &[Symbol::T(1), Symbol::NT(4), Symbol::NT(9)], &[Symbol::Empty], &[Symbol::T(4), Symbol::NT(13), Symbol::NT(10)], &[Symbol::T(2), Symbol::NT(13), Symbol::NT(10)], &[Symbol::T(0), Symbol::NT(11), Symbol::NT(10)], &[Symbol::T(10), Symbol::NT(11), Symbol::NT(10)], &[Symbol::Empty], &[Symbol::NT(13), Symbol::NT(12)], &[Symbol::T(4), Symbol::NT(13), Symbol::NT(12)], &[Symbol::T(2), Symbol::NT(13), Symbol::NT(12)], &[Symbol::Empty], &[Symbol::T(6), Symbol::NT(4), Symbol::T(8)], &[Symbol::T(10), Symbol::NT(13)], &[Symbol::T(15), Symbol::NT(16)], &[Symbol::T(16)], &[Symbol::NT(6)], &[Symbol::Empty], &[Symbol::NT(7)], &[Symbol::Empty], &[Symbol::T(6), Symbol::NT(5), Symbol::T(8)], &[Symbol::Empty]];
    static PARSING_TABLE: [AltId; 306] = [35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 0, 35, 35, 35, 35, 35, 36, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 1, 35, 35, 35, 35, 35, 36, 35, 35, 35, 35, 35, 35, 35, 35, 3, 35, 35, 35, 35, 35, 35, 2, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36, 35, 35, 35, 35, 4, 6, 5, 35, 35, 35, 35, 36, 35, 35, 35, 35, 7, 35, 36, 36, 7, 35, 35, 35, 35, 7, 7, 35, 35, 35, 35, 35, 35, 35, 8, 35, 9, 35, 8, 35, 35, 35, 35, 8, 8, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 10, 35, 35, 35, 35, 35, 36, 35, 35, 35, 35, 35, 35, 35, 36, 35, 35, 35, 35, 11, 11, 11, 35, 35, 35, 35, 12, 35, 35, 35, 35, 35, 35, 13, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 14, 35, 35, 35, 35, 35, 35, 15, 35, 35, 35, 35, 35, 35, 35, 35, 35, 18, 20, 17, 35, 16, 35, 35, 35, 20, 20, 19, 35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 35, 36, 35, 21, 35, 36, 36, 21, 35, 35, 35, 35, 21, 21, 35, 24, 24, 23, 35, 22, 35, 35, 35, 24, 24, 24, 35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 35, 36, 35, 25, 35, 36, 36, 26, 35, 35, 35, 35, 27, 28, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 29, 35, 35, 35, 35, 35, 30, 35, 35, 35, 35, 35, 35, 35, 32, 35, 35, 35, 35, 31, 31, 31, 35, 35, 35, 34, 34, 34, 35, 34, 35, 33, 35, 34, 34, 34, 35, 35, 35, 35, 35, 35, 35];
    static OPCODES: [&[OpCode]; 35] = [&[OpCode::Exit(0), OpCode::NT(6)], &[OpCode::Exit(1), OpCode::T(7), OpCode::NT(7), OpCode::T(5), OpCode::T(8), OpCode::NT(2), OpCode::T(6), OpCode::T(15), OpCode::T(11)], &[OpCode::Exit(2), OpCode::NT(8), OpCode::T(15)], &[OpCode::Exit(3)], &[OpCode::Exit(4), OpCode::T(9), OpCode::NT(4), OpCode::T(3), OpCode::T(15), OpCode::T(12)], &[OpCode::Exit(5), OpCode::T(9), OpCode::NT(4), OpCode::T(14)], &[OpCode::Exit(6), OpCode::T(9), OpCode::NT(4), OpCode::T(13)], &[OpCode::NT(10), OpCode::Exit(7), OpCode::NT(13)], &[OpCode::Exit(8), OpCode::NT(9), OpCode::NT(4)], &[OpCode::Exit(9)], &[OpCode::NT(14), OpCode::NT(1)], &[OpCode::NT(15), OpCode::NT(3)], &[OpCode::Loop(8), OpCode::Exit(12), OpCode::T(15), OpCode::T(1)], &[OpCode::Exit(13)], &[OpCode::Loop(9), OpCode::Exit(14), OpCode::NT(4), OpCode::T(1)], &[OpCode::Exit(15)], &[OpCode::Loop(10), OpCode::Exit(16), OpCode::NT(13), OpCode::T(4)], &[OpCode::Loop(10), OpCode::Exit(17), OpCode::NT(13), OpCode::T(2)], &[OpCode::Loop(10), OpCode::Exit(18), OpCode::NT(11), OpCode::T(0)], &[OpCode::Loop(10), OpCode::Exit(19), OpCode::NT(11), OpCode::T(10)], &[OpCode::Exit(20)], &[OpCode::NT(12), OpCode::Exit(21), OpCode::NT(13)], &[OpCode::Loop(12), OpCode::Exit(22), OpCode::NT(13), OpCode::T(4)], &[OpCode::Loop(12), OpCode::Exit(23), OpCode::NT(13), OpCode::T(2)], &[OpCode::Exit(24)], &[OpCode::Exit(25), OpCode::T(8), OpCode::NT(4), OpCode::T(6)], &[OpCode::Exit(26), OpCode::NT(13), OpCode::T(10)], &[OpCode::NT(16), OpCode::T(15)], &[OpCode::Exit(28), OpCode::T(16)], &[OpCode::Loop(6), OpCode::Exit(29)], &[OpCode::Exit(30)], &[OpCode::Loop(7), OpCode::Exit(31)], &[OpCode::Exit(32)], &[OpCode::Exit(33), OpCode::T(8), OpCode::NT(5), OpCode::T(6)], &[OpCode::Exit(34)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }

    #[derive(Debug)]
    pub enum CtxProgram {
        /// `program -> function+`
        Program { plus: SynProgram1 },
    }
    #[derive(Debug)]
    pub enum CtxFunction {
        /// `function -> "def" Id "(" fun_params ")" "{" instruction+ "}"`
        Function { id: String, fun_params: SynFunParams, plus: SynFunction1 },
    }
    #[derive(Debug)]
    pub enum CtxFunParams {
        /// `fun_params -> Id ("," Id)*`
        FunParams1 { id: String, star: SynFunParams1 },
        /// `fun_params -> ε`
        FunParams2,
    }
    #[derive(Debug)]
    pub enum CtxInstruction {
        /// `instruction -> "let" Id "=" expr ";"`
        Instruction1 { id: String, expr: SynExpr },
        /// `instruction -> "return" expr ";"`
        Instruction2 { expr: SynExpr },
        /// `instruction -> "print" expr ";"`
        Instruction3 { expr: SynExpr },
    }
    #[derive(Debug)]
    pub enum CtxExpr {
        /// `expr -> expr "*" expr`
        Expr1 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "/" expr`
        Expr2 { expr: [SynExpr; 2] },
        /// `expr -> expr "+" expr`
        Expr3 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "-" expr`
        Expr4 { expr: [SynExpr; 2] },
        /// `expr -> "(" expr ")"`
        Expr5 { expr: SynExpr },
        /// `expr -> "-" expr`
        Expr6 { expr: SynExpr },
        /// `expr -> Num`
        Expr7 { num: String },
        /// `expr -> Id "(" fun_args ")"`
        Expr8 { id: String, fun_args: SynFunArgs },
        /// `expr -> Id`
        Expr9 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxFunArgs {
        /// `fun_args -> expr ("," expr)*`
        FunArgs1 { expr: SynExpr, star: SynFunArgs1 },
        /// `fun_args -> ε`
        FunArgs2,
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `program`
    // #[derive(Debug, PartialEq)] pub struct SynProgram();
    // /// User-defined type for `function`
    // #[derive(Debug, PartialEq)] pub struct SynFunction();
    // /// User-defined type for `fun_params`
    // #[derive(Debug, PartialEq)] pub struct SynFunParams();
    // /// User-defined type for `instruction`
    // #[derive(Debug, PartialEq)] pub struct SynInstruction();
    // /// User-defined type for `expr`
    // #[derive(Debug, PartialEq)] pub struct SynExpr();
    // /// User-defined type for `fun_args`
    // #[derive(Debug, PartialEq)] pub struct SynFunArgs();
    /// Computed `function+` array in `program ->  ►► function+ ◄◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynProgram1(pub Vec<SynFunction>);
    /// Computed `instruction+` array in `function -> "def" Id "(" fun_params ")" "{"  ►► instruction+ ◄◄  "}"`
    #[derive(Debug, PartialEq)]
    pub struct SynFunction1(pub Vec<SynInstruction>);
    /// Computed `("," Id)*` array in `fun_params -> Id  ►► ("," Id)* ◄◄  | ε`
    #[derive(Debug, PartialEq)]
    pub struct SynFunParams1(pub Vec<String>);
    /// Computed `("," expr)*` array in `fun_args -> expr  ►► ("," expr)* ◄◄  | ε`
    #[derive(Debug, PartialEq)]
    pub struct SynFunArgs1(pub Vec<SynExpr>);

    #[derive(Debug)]
    enum SynValue { Program(SynProgram), Function(SynFunction), FunParams(SynFunParams), Instruction(SynInstruction), Expr(SynExpr), FunArgs(SynFunArgs), Program1(SynProgram1), Function1(SynFunction1), FunParams1(SynFunParams1), FunArgs1(SynFunArgs1) }

    impl SynValue {
        fn get_program(self) -> SynProgram {
            if let SynValue::Program(val) = self { val } else { panic!() }
        }
        fn get_function(self) -> SynFunction {
            if let SynValue::Function(val) = self { val } else { panic!() }
        }
        fn get_fun_params(self) -> SynFunParams {
            if let SynValue::FunParams(val) = self { val } else { panic!() }
        }
        fn get_instruction(self) -> SynInstruction {
            if let SynValue::Instruction(val) = self { val } else { panic!() }
        }
        fn get_expr(self) -> SynExpr {
            if let SynValue::Expr(val) = self { val } else { panic!() }
        }
        fn get_fun_args(self) -> SynFunArgs {
            if let SynValue::FunArgs(val) = self { val } else { panic!() }
        }
        fn get_program1(self) -> SynProgram1 {
            if let SynValue::Program1(val) = self { val } else { panic!() }
        }
        fn get_function1(self) -> SynFunction1 {
            if let SynValue::Function1(val) = self { val } else { panic!() }
        }
        fn get_fun_params1(self) -> SynFunParams1 {
            if let SynValue::FunParams1(val) = self { val } else { panic!() }
        }
        fn get_fun_args1(self) -> SynFunArgs1 {
            if let SynValue::FunArgs1(val) = self { val } else { panic!() }
        }
    }

    pub trait MicroCalcListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        fn exit(&mut self, _program: SynProgram) {}
        fn init_program(&mut self) {}
        fn exit_program(&mut self, _ctx: CtxProgram) -> SynProgram;
        fn init_function(&mut self) {}
        fn exit_function(&mut self, _ctx: CtxFunction) -> SynFunction;
        fn init_fun_params(&mut self) {}
        fn exit_fun_params(&mut self, _ctx: CtxFunParams) -> SynFunParams;
        fn init_instruction(&mut self) {}
        fn exit_instruction(&mut self, _ctx: CtxInstruction) -> SynInstruction;
        fn init_expr(&mut self) {}
        fn exit_expr(&mut self, _ctx: CtxExpr) -> SynExpr;
        fn init_fun_args(&mut self) {}
        fn exit_fun_args(&mut self, _ctx: CtxFunArgs) -> SynFunArgs;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: MicroCalcListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_program(),          // program
                        6 => self.init_program1(),                  // program_1
                        14 => {}                                    // program_2
                        1 => self.listener.init_function(),         // function
                        7 => self.init_function1(),                 // function_1
                        15 => {}                                    // function_2
                        2 => self.listener.init_fun_params(),       // fun_params
                        8 => self.init_fun_params1(),               // fun_params_1
                        3 => self.listener.init_instruction(),      // instruction
                        4 => self.listener.init_expr(),             // expr
                        10 ..= 13 => {}                             // expr_1, expr_2, expr_3, expr_4
                        16 => {}                                    // expr_5
                        5 => self.listener.init_fun_args(),         // fun_args
                        9 => self.init_fun_args1(),                 // fun_args_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_program(),                   // program -> program_1
                        29 |                                        // program_2 -> program_1
                        30 => self.exit_program1(),                 // program_2 -> ε
                     /* 10 */                                       // program_1 -> function program_2 (never called)
                        1 => self.exit_function(),                  // function -> "def" Id "(" fun_params ")" "{" function_1 "}"
                        31 |                                        // function_2 -> function_1
                        32 => self.exit_function1(),                // function_2 -> ε
                     /* 11 */                                       // function_1 -> instruction function_2 (never called)
                        2 |                                         // fun_params -> Id fun_params_1
                        3 => self.exit_fun_params(alt_id),          // fun_params -> ε
                        12 => self.exit_fun_params1(),              // fun_params_1 -> "," Id fun_params_1
                        13 => {}                                    // fun_params_1 -> ε
                        4 |                                         // instruction -> "let" Id "=" expr ";"
                        5 |                                         // instruction -> "return" expr ";"
                        6 => self.exit_instruction(alt_id),         // instruction -> "print" expr ";"
                        16 |                                        // expr_1 -> "*" expr_4 expr_1
                        17 |                                        // expr_1 -> "/" expr_4 expr_1
                        18 |                                        // expr_1 -> "+" expr_2 expr_1
                        19 => self.exit_expr1(alt_id),              // expr_1 -> "-" expr_2 expr_1
                        22 => self.exit_expr1(16),                  // expr_3 -> "*" expr_4 expr_3 (duplicate of 16)
                        23 => self.exit_expr1(17),                  // expr_3 -> "/" expr_4 expr_3 (duplicate of 17)
                        25 |                                        // expr_4 -> "(" expr ")"
                        26 |                                        // expr_4 -> "-" expr_4
                        28 |                                        // expr_4 -> Num
                        33 |                                        // expr_5 -> "(" fun_args ")"
                        34 => self.exit_expr4(alt_id),              // expr_5 -> ε
                        7 => {}                                     // expr -> expr_4 expr_1 (not used)
                        20 => {}                                    // expr_1 -> ε (not used)
                        21 => {}                                    // expr_2 -> expr_4 expr_3 (not used)
                        24 => {}                                    // expr_3 -> ε (not used)
                     /* 27 */                                       // expr_4 -> Id expr_5 (never called)
                        8 |                                         // fun_args -> expr fun_args_1
                        9 => self.exit_fun_args(alt_id),            // fun_args -> ε
                        14 => self.exit_fun_args1(),                // fun_args_1 -> "," expr fun_args_1
                        15 => {}                                    // fun_args_1 -> ε
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
            }
        }

        fn check_abort_request(&self) -> bool {
            self.listener.check_abort_request()
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            self.listener.get_mut_log()
        }
    }

    impl<T: MicroCalcListener> Wrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn get_listener(&self) -> &T {
            &self.listener
        }

        pub fn get_listener_mut(&mut self) -> &mut T {
            &mut self.listener
        }

        pub fn give_listener(self) -> T {
            self.listener
        }

        pub fn set_verbose(&mut self, verbose: bool) {
            self.verbose = verbose;
        }

        fn exit(&mut self) {
            let program = self.stack.pop().unwrap().get_program();
            self.listener.exit(program);
        }

        fn exit_program(&mut self) {
            let plus = self.stack.pop().unwrap().get_program1();
            let val = self.listener.exit_program(CtxProgram::Program { plus });
            self.stack.push(SynValue::Program(val));
        }

        fn init_program1(&mut self) {
            let val = SynProgram1(Vec::new());
            self.stack.push(SynValue::Program1(val));
        }

        fn exit_program1(&mut self) {
            let function = self.stack.pop().unwrap().get_function();
            let mut plus_it = self.stack.pop().unwrap().get_program1();
            plus_it.0.push(function);
            self.stack.push(SynValue::Program1(plus_it));
        }

        fn exit_function(&mut self) {
            let plus = self.stack.pop().unwrap().get_function1();
            let fun_params = self.stack.pop().unwrap().get_fun_params();
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_function(CtxFunction::Function { id, fun_params, plus });
            self.stack.push(SynValue::Function(val));
        }

        fn init_function1(&mut self) {
            let val = SynFunction1(Vec::new());
            self.stack.push(SynValue::Function1(val));
        }

        fn exit_function1(&mut self) {
            let instruction = self.stack.pop().unwrap().get_instruction();
            let mut plus_it = self.stack.pop().unwrap().get_function1();
            plus_it.0.push(instruction);
            self.stack.push(SynValue::Function1(plus_it));
        }

        fn exit_fun_params(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                2 => {
                    let star = self.stack.pop().unwrap().get_fun_params1();
                    let id = self.stack_t.pop().unwrap();
                    CtxFunParams::FunParams1 { id, star }
                }
                3 => {
                    CtxFunParams::FunParams2
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_fun_params")
            };
            let val = self.listener.exit_fun_params(ctx);
            self.stack.push(SynValue::FunParams(val));
        }

        fn init_fun_params1(&mut self) {
            let val = SynFunParams1(Vec::new());
            self.stack.push(SynValue::FunParams1(val));
        }

        fn exit_fun_params1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let mut star_it = self.stack.pop().unwrap().get_fun_params1();
            star_it.0.push(id);
            self.stack.push(SynValue::FunParams1(star_it));
        }

        fn exit_instruction(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                4 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    CtxInstruction::Instruction1 { id, expr }
                }
                5 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::Instruction2 { expr }
                }
                6 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::Instruction3 { expr }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_instruction")
            };
            let val = self.listener.exit_instruction(ctx);
            self.stack.push(SynValue::Instruction(val));
        }

        fn exit_expr1(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                16 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::Expr1 { expr: [expr_1, expr_2] }
                }
                17 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::Expr2 { expr: [expr_1, expr_2] }
                }
                18 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::Expr3 { expr: [expr_1, expr_2] }
                }
                19 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::Expr4 { expr: [expr_1, expr_2] }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr1")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }

        fn exit_expr4(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                25 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::Expr5 { expr }
                }
                26 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::Expr6 { expr }
                }
                28 => {
                    let num = self.stack_t.pop().unwrap();
                    CtxExpr::Expr7 { num }
                }
                33 => {
                    let fun_args = self.stack.pop().unwrap().get_fun_args();
                    let id = self.stack_t.pop().unwrap();
                    CtxExpr::Expr8 { id, fun_args }
                }
                34 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxExpr::Expr9 { id }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr4")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }

        fn exit_fun_args(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                8 => {
                    let star = self.stack.pop().unwrap().get_fun_args1();
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxFunArgs::FunArgs1 { expr, star }
                }
                9 => {
                    CtxFunArgs::FunArgs2
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_fun_args")
            };
            let val = self.listener.exit_fun_args(ctx);
            self.stack.push(SynValue::FunArgs(val));
        }

        fn init_fun_args1(&mut self) {
            let val = SynFunArgs1(Vec::new());
            self.stack.push(SynValue::FunArgs1(val));
        }

        fn exit_fun_args1(&mut self) {
            let expr = self.stack.pop().unwrap().get_expr();
            let mut star_it = self.stack.pop().unwrap().get_fun_args1();
            star_it.0.push(expr);
            self.stack.push(SynValue::FunArgs1(star_it));
        }
    }

    // [microcalc_parser]
}

// -------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gen_microcalc() {
        main();
    }
}