// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Parser;
use crate::listener_types::*;
use crate::microcalc_lexer::build_lexer;
use crate::microcalc_parser::*;

const VERBOSE_WRAPPER: bool = false;

static TXT1: &str = r#"
def gcd(a, b) {
    while a != b {
        if a > b
            a = a - b;
        else
            b = b - a;
    }
    return a;
}
def const() { return 12; }
def main() { let value = gcd(9, const()); print value; return value; }
"#;

static TXT2: &str = r#"
def main() {
    let x = 1;
    let y = 2;
    let z = 0;
    if x == 1
        if y == 1
            z = 1;
        else
            z = 2;
    print z;
    return z;
}
"#;

fn main() {
    for txt in &[TXT1, TXT2] {
        println!("{:=<80}\n{txt}\n{0:-<80}", "");
        match MCalc::parse_text(txt.to_string()) {
            Ok(log) => println!("parsing successful\n{log}"),
            Err(log) => println!("errors during parsing:\n{log}"),
        }
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
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
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

    fn exit_block(&mut self, _ctx: CtxBlock) -> SynBlock {
        SynBlock()
    }

    fn exit_instruction(&mut self, ctx: CtxInstruction) -> SynInstruction {
        match ctx {
            // instruction -> "let" Id "=" expr ";"
            CtxInstruction::V1 { .. } => {}
            // instruction -> Id "=" expr ";"
            CtxInstruction::V2 { .. } => {}
            // instruction -> "return" expr ";"
            CtxInstruction::V3 { .. } => {}
            // instruction -> "print" expr ";"
            CtxInstruction::V4 { .. } => {}
            // instruction -> Id "(" fun_args ")" ";"
            CtxInstruction::V5 { .. } => {}
            // instruction -> "if" expr instruction "else" instruction
            CtxInstruction::V6 { .. } => {}
            // instruction -> "if" expr instruction
            CtxInstruction::V7 { .. } => {}
            // instruction -> "while" expr block
            CtxInstruction::V8 { .. } => {}
            // instruction -> block
            CtxInstruction::V9 { .. } => {}
        }
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
    /// User-defined type for `block`
    #[derive(Debug, PartialEq)] pub struct SynBlock();
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
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 39;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 5;
    const NBR_STATES: StateId = 60;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         29,  29,  29,  29,  29,  29,  29,  29,  29,   0,  34,  29,  29,  34,  29,  29,   // 0-15
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,   // 16-31
          0,   1,  29,  29,  29,  29,  29,  29,   2,   3,   4,   5,   6,   7,  29,   8,   // 32-47
          9,  10,  27,  27,  27,  27,  27,  27,  27,  27,  29,  11,  12,  13,  14,  29,   // 48-63
         29,  30,  30,  30,  30,  30,  30,  31,  31,  31,  31,  31,  31,  31,  31,  31,   // 64-79
         31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  29,  29,  29,  16,  26,   // 80-95
         29,  30,  15,  30,  17,  18,  32,  31,  33,  19,  31,  31,  20,  31,  37,  31,   // 96-111
         21,  31,  22,  35,  36,  38,  31,  23,  28,  31,  31,  24,  29,  25,  29,  29,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 29),
        (Seg(57344, 1114111), 29),
    ];
    static TERMINAL_TABLE: [Terminal;55] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 2341] = [
          5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  60,  15,  20,  60,  20,  20,  20,  20,   5,  20,  20,  20,  20, // state 0
          1,   1,   1,   1,   2,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
          1,   1,   1,   1,   2,   1,   1,   1,  57,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 2
         60,  60,  60,  60,  60,  60,  60,  60,  60,  58,  58,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 3
         60,  60,  60,  60,  60,  60,  60,  60,  60,  59,  59,  60,  60,  60,  60,  59,  60,  59,  59,  60,  60,  60,  60,  60,  60,  60,  60,  59,  60,  60,  59,  60,  59,  60,  60,  60,  60,  60,  60, // state 4
          5,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,   5,  60,  60,  60,  60, // state 5 <skip>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  32,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 6 <end:6>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 7 <end:14>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 8 <end:16>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 9 <end:12>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 10 <end:7>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 11 <end:8>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 12 <end:18>
         60,  60,  60,  60,   1,  60,  60,  60,  35,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 13 <end:9>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  15,  15,  60,  60,  60,  60,   3,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  15,  15,   4,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 14 <end:27>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  15,  15,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  15,  15,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 15 <end:27>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 16 <end:17>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  33,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 17 <end:2>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  31,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 18 <end:10>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  34,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 19 <end:3>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 20 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 21 <end:11>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  36,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 22 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  38,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 23 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  41,  20,  60,  20,  20,  20,  20, // state 24 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  42,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 25 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  44,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 26 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  48,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 27 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  53,  60,  20,  20,  20,  20, // state 28 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 29 <end:13>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 30 <end:15>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 31 <end:0>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 32 <end:1>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 33 <end:4>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 34 <end:5>
         35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  60,  35,  35,  35,  35, // state 35 <skip>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  37,  20,  60,  20,  20,  20,  20, // state 36 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 37 <end:19>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  39,  20,  20,  20, // state 38 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  40,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 39 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 40 <end:20>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 41 <end:21>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  43,  20,  20, // state 42 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 43 <end:22>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  45,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 44 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  46,  20, // state 45 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  47,  20,  20, // state 46 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 47 <end:23>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  49,  20,  20, // state 48 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  50, // state 49 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  51,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 50 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  52,  20, // state 51 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 52 <end:24>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  54,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 53 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  55,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 54 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  56,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 55 <end:26>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  20,  20,  60,  60,  60,  60,  20,  60,  20,  20,  20,  20,  20,  20,  20,  60,  60,  20,  20,  20,  60,  20,  20,  20,  20,  60,  20,  20,  20,  20, // state 56 <end:25>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 57 <skip>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  58,  58,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  58,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60,  60, // state 58 <end:27>
         60,  60,  60,  60,  60,  60,  60,  60,  60,  59,  59,  60,  60,  60,  60,  59,  60,  59,  59,  60,  60,  60,  60,  60,  60,  60,  59,  59,  60,  60,  59,  60,  59,  60,  60,  60,  60,  60,  60, // state 59 <end:27>
         60 // error group in [nbr_state * nbr_group + nbr_group]
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

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 28;
    const PARSER_NUM_NT: usize = 23;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Eq", Some("==")), ("Ne", Some("!=")), ("Lt", Some("<")), ("Gt", Some(">")), ("Le", Some("<=")), ("Ge", Some(">=")), ("Not", Some("!")), ("Add", Some("+")), ("Comma", Some(",")), ("Div", Some("/")), ("Equal", Some("=")), ("Exp", Some("^")), ("Mul", Some("*")), ("Lbracket", Some("{")), ("Lpar", Some("(")), ("Rbracket", Some("}")), ("Rpar", Some(")")), ("Semi", Some(";")), ("Sub", Some("-")), ("Def", Some("def")), ("Else", Some("else")), ("If", Some("if")), ("Let", Some("let")), ("Print", Some("print")), ("Return", Some("return")), ("While", Some("while")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "function", "fun_params", "block", "instruction", "expr", "fun_args", "program_1", "fun_params_1", "block_1", "fun_args_1", "expr_1", "expr_2", "expr_3", "expr_4", "expr_5", "expr_6", "expr_7", "expr_8", "instruction_1", "instruction_2", "program_2", "expr_9"];
    static ALT_VAR: [VarId; 62] = [0, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4, 5, 6, 6, 7, 8, 8, 9, 9, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 13, 13, 13, 13, 13, 13, 14, 15, 15, 15, 15, 16, 17, 17, 18, 18, 18, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22];
    static PARSING_TABLE: [AltId; 667] = [62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 0, 62, 62, 62, 62, 62, 62, 62, 62, 63, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 1, 62, 62, 62, 62, 62, 62, 62, 62, 63, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 3, 62, 62, 62, 62, 62, 62, 62, 62, 62, 2, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 4, 62, 63, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63, 62, 63, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 11, 62, 63, 62, 62, 62, 63, 63, 5, 6, 7, 8, 9, 10, 62, 63, 62, 62, 62, 62, 62, 62, 12, 62, 63, 62, 62, 62, 62, 63, 12, 62, 63, 63, 12, 62, 62, 63, 63, 63, 63, 63, 12, 12, 62, 62, 62, 62, 62, 62, 62, 13, 62, 62, 62, 62, 62, 62, 62, 13, 62, 14, 62, 13, 62, 62, 62, 62, 62, 62, 62, 13, 13, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 15, 62, 62, 62, 62, 62, 62, 62, 62, 63, 62, 62, 62, 62, 62, 62, 62, 62, 16, 62, 62, 62, 62, 62, 62, 62, 17, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 18, 62, 19, 62, 62, 62, 62, 62, 18, 18, 18, 18, 18, 18, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 20, 62, 62, 62, 62, 62, 62, 62, 21, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 27, 28, 29, 30, 31, 32, 62, 25, 33, 24, 62, 22, 23, 33, 62, 62, 33, 33, 26, 62, 62, 33, 33, 33, 33, 33, 33, 62, 62, 63, 63, 63, 63, 63, 63, 34, 63, 63, 63, 62, 63, 63, 63, 34, 62, 63, 63, 34, 62, 62, 63, 63, 63, 63, 63, 34, 34, 62, 40, 40, 40, 40, 40, 40, 62, 38, 40, 37, 62, 35, 36, 40, 62, 62, 40, 40, 39, 62, 62, 40, 40, 40, 40, 40, 40, 62, 62, 63, 63, 63, 63, 63, 63, 41, 63, 63, 63, 62, 63, 63, 63, 41, 62, 63, 63, 41, 62, 62, 63, 63, 63, 63, 63, 41, 41, 62, 45, 45, 45, 45, 45, 45, 62, 45, 45, 44, 62, 42, 43, 45, 62, 62, 45, 45, 45, 62, 62, 45, 45, 45, 45, 45, 45, 62, 62, 63, 63, 63, 63, 63, 63, 46, 63, 63, 63, 62, 63, 63, 63, 46, 62, 63, 63, 46, 62, 62, 63, 63, 63, 63, 63, 46, 46, 62, 48, 48, 48, 48, 48, 48, 62, 48, 48, 48, 62, 47, 48, 48, 62, 62, 48, 48, 48, 62, 62, 48, 48, 48, 48, 48, 48, 62, 62, 63, 63, 63, 63, 63, 63, 49, 63, 63, 63, 62, 63, 63, 63, 50, 62, 63, 63, 51, 62, 62, 63, 63, 63, 63, 63, 52, 53, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 55, 62, 55, 62, 62, 62, 55, 54, 55, 55, 55, 55, 55, 55, 62, 55, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 56, 62, 62, 63, 57, 63, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63, 62, 63, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 58, 62, 62, 62, 62, 62, 62, 62, 62, 59, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 62, 61, 61, 61, 60, 62, 61, 61, 61, 62, 62, 61, 61, 61, 61, 61, 61, 62, 62];
    static OPCODES: [&[OpCode]; 62] = [&[OpCode::Exit(0), OpCode::NT(7)], &[OpCode::Exit(1), OpCode::NT(4), OpCode::T(16), OpCode::NT(2), OpCode::T(14), OpCode::T(26), OpCode::T(19)], &[OpCode::Exit(2), OpCode::NT(8), OpCode::T(26)], &[OpCode::Exit(3)], &[OpCode::Exit(4), OpCode::T(15), OpCode::NT(9), OpCode::T(13)], &[OpCode::NT(19), OpCode::NT(4), OpCode::NT(5), OpCode::T(21)], &[OpCode::Exit(6), OpCode::T(17), OpCode::NT(5), OpCode::T(10), OpCode::T(26), OpCode::T(22)], &[OpCode::Exit(7), OpCode::T(17), OpCode::NT(5), OpCode::T(23)], &[OpCode::Exit(8), OpCode::T(17), OpCode::NT(5), OpCode::T(24)], &[OpCode::Exit(9), OpCode::NT(3), OpCode::NT(5), OpCode::T(25)], &[OpCode::NT(20), OpCode::T(26)], &[OpCode::Exit(11), OpCode::NT(3)], &[OpCode::NT(11), OpCode::Exit(12), OpCode::NT(18)], &[OpCode::Exit(13), OpCode::NT(10), OpCode::NT(5)], &[OpCode::Exit(14)], &[OpCode::NT(21), OpCode::NT(1)], &[OpCode::Loop(8), OpCode::Exit(16), OpCode::T(26), OpCode::T(8)], &[OpCode::Exit(17)], &[OpCode::Loop(9), OpCode::Exit(18), OpCode::NT(4)], &[OpCode::Exit(19)], &[OpCode::Loop(10), OpCode::Exit(20), OpCode::NT(5), OpCode::T(8)], &[OpCode::Exit(21)], &[OpCode::Loop(11), OpCode::Exit(22), OpCode::NT(16), OpCode::T(11)], &[OpCode::Loop(11), OpCode::Exit(23), OpCode::NT(16), OpCode::T(12)], &[OpCode::Loop(11), OpCode::Exit(24), OpCode::NT(16), OpCode::T(9)], &[OpCode::Loop(11), OpCode::Exit(25), OpCode::NT(14), OpCode::T(7)], &[OpCode::Loop(11), OpCode::Exit(26), OpCode::NT(14), OpCode::T(18)], &[OpCode::Loop(11), OpCode::Exit(27), OpCode::NT(12), OpCode::T(0)], &[OpCode::Loop(11), OpCode::Exit(28), OpCode::NT(12), OpCode::T(1)], &[OpCode::Loop(11), OpCode::Exit(29), OpCode::NT(12), OpCode::T(2)], &[OpCode::Loop(11), OpCode::Exit(30), OpCode::NT(12), OpCode::T(3)], &[OpCode::Loop(11), OpCode::Exit(31), OpCode::NT(12), OpCode::T(4)], &[OpCode::Loop(11), OpCode::Exit(32), OpCode::NT(12), OpCode::T(5)], &[OpCode::Exit(33)], &[OpCode::NT(13), OpCode::Exit(34), OpCode::NT(18)], &[OpCode::Loop(13), OpCode::Exit(35), OpCode::NT(16), OpCode::T(11)], &[OpCode::Loop(13), OpCode::Exit(36), OpCode::NT(16), OpCode::T(12)], &[OpCode::Loop(13), OpCode::Exit(37), OpCode::NT(16), OpCode::T(9)], &[OpCode::Loop(13), OpCode::Exit(38), OpCode::NT(14), OpCode::T(7)], &[OpCode::Loop(13), OpCode::Exit(39), OpCode::NT(14), OpCode::T(18)], &[OpCode::Exit(40)], &[OpCode::NT(15), OpCode::Exit(41), OpCode::NT(18)], &[OpCode::Loop(15), OpCode::Exit(42), OpCode::NT(16), OpCode::T(11)], &[OpCode::Loop(15), OpCode::Exit(43), OpCode::NT(16), OpCode::T(12)], &[OpCode::Loop(15), OpCode::Exit(44), OpCode::NT(16), OpCode::T(9)], &[OpCode::Exit(45)], &[OpCode::NT(17), OpCode::Exit(46), OpCode::NT(18)], &[OpCode::Loop(17), OpCode::Exit(47), OpCode::NT(16), OpCode::T(11)], &[OpCode::Exit(48)], &[OpCode::Exit(49), OpCode::NT(12), OpCode::T(6)], &[OpCode::Exit(50), OpCode::T(16), OpCode::NT(5), OpCode::T(14)], &[OpCode::Exit(51), OpCode::NT(18), OpCode::T(18)], &[OpCode::NT(22), OpCode::T(26)], &[OpCode::Exit(53), OpCode::T(27)], &[OpCode::Exit(54), OpCode::NT(4), OpCode::T(20)], &[OpCode::Exit(55)], &[OpCode::Exit(56), OpCode::T(17), OpCode::NT(5), OpCode::T(10)], &[OpCode::Exit(57), OpCode::T(17), OpCode::T(16), OpCode::NT(6), OpCode::T(14)], &[OpCode::Loop(7), OpCode::Exit(58)], &[OpCode::Exit(59)], &[OpCode::Exit(60), OpCode::T(16), OpCode::NT(6), OpCode::T(14)], &[OpCode::Exit(61)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            Vec::new(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    #[derive(Debug)]
    pub enum CtxProgram {
        /// `program -> function+`
        V1 { plus: SynProgram1 },
    }
    #[derive(Debug)]
    pub enum CtxFunction {
        /// `function -> "def" Id "(" fun_params ")" instruction`
        V1 { id: String, fun_params: SynFunParams, instruction: SynInstruction },
    }
    #[derive(Debug)]
    pub enum CtxFunParams {
        /// `fun_params -> Id ("," Id)*`
        V1 { star: SynFunParams1 },
        /// `fun_params -> ε`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxBlock {
        /// `block -> "{" instruction* "}"`
        V1 { star: SynBlock1 },
    }
    #[derive(Debug)]
    pub enum CtxInstruction {
        /// `instruction -> "let" Id "=" expr ";"`
        V1 { id: String, expr: SynExpr },
        /// `instruction -> Id "=" expr ";"`
        V2 { id: String, expr: SynExpr },
        /// `instruction -> "return" expr ";"`
        V3 { expr: SynExpr },
        /// `instruction -> "print" expr ";"`
        V4 { expr: SynExpr },
        /// `instruction -> Id "(" fun_args ")" ";"`
        V5 { id: String, fun_args: SynFunArgs },
        /// `instruction -> "if" expr instruction <G> "else" instruction`
        V6 { expr: SynExpr, instruction: [SynInstruction; 2] },
        /// `instruction -> "if" expr instruction`
        V7 { expr: SynExpr, instruction: SynInstruction },
        /// `instruction -> "while" expr block`
        V8 { expr: SynExpr, block: SynBlock },
        /// `instruction -> block`
        V9 { block: SynBlock },
    }
    #[derive(Debug)]
    pub enum CtxExpr {
        /// `expr -> "-" expr`
        V1 { expr: SynExpr },
        /// `expr -> expr <R> "^" expr`
        V2 { expr: [SynExpr; 2] },
        /// `expr -> expr "*" expr`
        V3 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "/" expr`
        V4 { expr: [SynExpr; 2] },
        /// `expr -> expr "+" expr`
        V5 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "-" expr`
        V6 { expr: [SynExpr; 2] },
        /// `expr -> "!" expr`
        V7 { expr: SynExpr },
        /// `expr -> expr "==" expr`
        V8 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "!=" expr`
        V9 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "<" expr`
        V10 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> ">" expr`
        V11 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "<=" expr`
        V12 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> ">=" expr`
        V13 { expr: [SynExpr; 2] },
        /// `expr -> "(" expr ")"`
        V14 { expr: SynExpr },
        /// `expr -> Id "(" fun_args ")"`
        V15 { id: String, fun_args: SynFunArgs },
        /// `expr -> Id`
        V16 { id: String },
        /// `expr -> Num`
        V17 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxFunArgs {
        /// `fun_args -> expr ("," expr)*`
        V1 { star: SynFunArgs1 },
        /// `fun_args -> ε`
        V2,
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `program`
    // #[derive(Debug, PartialEq)] pub struct SynProgram();
    // /// User-defined type for `function`
    // #[derive(Debug, PartialEq)] pub struct SynFunction();
    // /// User-defined type for `fun_params`
    // #[derive(Debug, PartialEq)] pub struct SynFunParams();
    // /// User-defined type for `block`
    // #[derive(Debug, PartialEq)] pub struct SynBlock();
    // /// User-defined type for `instruction`
    // #[derive(Debug, PartialEq)] pub struct SynInstruction();
    // /// User-defined type for `expr`
    // #[derive(Debug, PartialEq)] pub struct SynExpr();
    // /// User-defined type for `fun_args`
    // #[derive(Debug, PartialEq)] pub struct SynFunArgs();
    /// Computed `function+` array in `program ->  ►► function+ ◄◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynProgram1(pub Vec<SynFunction>);
    /// Computed `("," Id)*` array in `fun_params -> Id  ►► ("," Id)* ◄◄  | ε`
    #[derive(Debug, PartialEq)]
    pub struct SynFunParams1(pub Vec<String>);
    /// Computed `instruction*` array in `block -> "{"  ►► instruction* ◄◄  "}"`
    #[derive(Debug, PartialEq)]
    pub struct SynBlock1(pub Vec<SynInstruction>);
    /// Computed `("," expr)*` array in `fun_args -> expr  ►► ("," expr)* ◄◄  | ε`
    #[derive(Debug, PartialEq)]
    pub struct SynFunArgs1(pub Vec<SynExpr>);

    #[derive(Debug)]
    enum SynValue { Program(SynProgram), Function(SynFunction), FunParams(SynFunParams), Block(SynBlock), Instruction(SynInstruction), Expr(SynExpr), FunArgs(SynFunArgs), Program1(SynProgram1), FunParams1(SynFunParams1), Block1(SynBlock1), FunArgs1(SynFunArgs1) }

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
        fn get_block(self) -> SynBlock {
            if let SynValue::Block(val) = self { val } else { panic!() }
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
        fn get_fun_params1(self) -> SynFunParams1 {
            if let SynValue::FunParams1(val) = self { val } else { panic!() }
        }
        fn get_block1(self) -> SynBlock1 {
            if let SynValue::Block1(val) = self { val } else { panic!() }
        }
        fn get_fun_args1(self) -> SynFunArgs1 {
            if let SynValue::FunArgs1(val) = self { val } else { panic!() }
        }
    }

    pub trait MicroCalcListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, program: SynProgram) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_program(&mut self) {}
        fn exit_program(&mut self, ctx: CtxProgram) -> SynProgram;
        fn init_function(&mut self) {}
        fn exit_function(&mut self, ctx: CtxFunction) -> SynFunction;
        fn init_fun_params(&mut self) {}
        fn exit_fun_params(&mut self, ctx: CtxFunParams) -> SynFunParams;
        fn init_block(&mut self) {}
        fn exit_block(&mut self, ctx: CtxBlock) -> SynBlock;
        fn init_instruction(&mut self) {}
        fn exit_instruction(&mut self, ctx: CtxInstruction) -> SynInstruction;
        fn init_expr(&mut self) {}
        fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
        fn init_fun_args(&mut self) {}
        fn exit_fun_args(&mut self, ctx: CtxFunArgs) -> SynFunArgs;
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
                        7 => self.init_program1(),                  // program_1
                        21 => {}                                    // program_2
                        1 => self.listener.init_function(),         // function
                        2 => self.listener.init_fun_params(),       // fun_params
                        8 => self.init_fun_params1(),               // fun_params_1
                        3 => self.listener.init_block(),            // block
                        9 => self.init_block1(),                    // block_1
                        4 => self.listener.init_instruction(),      // instruction
                        19 | 20 => {}                               // instruction_1, instruction_2
                        5 => self.listener.init_expr(),             // expr
                        11 ..= 18 => {}                             // expr_1, expr_2, expr_3, expr_4, expr_5, expr_6, expr_7, expr_8
                        22 => {}                                    // expr_9
                        6 => self.listener.init_fun_args(),         // fun_args
                        10 => self.init_fun_args1(),                // fun_args_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_program(),                   // program -> program_1
                        58 |                                        // program_2 -> program_1
                        59 => self.exit_program1(),                 // program_2 -> ε
                     /* 15 */                                       // program_1 -> function program_2 (never called)
                        1 => self.exit_function(),                  // function -> "def" Id "(" fun_params ")" instruction
                        2 |                                         // fun_params -> Id fun_params_1
                        3 => self.exit_fun_params(alt_id),          // fun_params -> ε
                        16 => self.exit_fun_params1(),              // fun_params_1 -> "," Id fun_params_1
                        17 => {}                                    // fun_params_1 -> ε
                        4 => self.exit_block(),                     // block -> "{" block_1 "}"
                        18 => self.exit_block1(),                   // block_1 -> instruction block_1
                        19 => {}                                    // block_1 -> ε
                        6 |                                         // instruction -> "let" Id "=" expr ";"
                        7 |                                         // instruction -> "print" expr ";"
                        8 |                                         // instruction -> "return" expr ";"
                        9 |                                         // instruction -> "while" expr block
                        11 |                                        // instruction -> block
                        54 |                                        // instruction_1 -> "else" instruction
                        55 |                                        // instruction_1 -> ε
                        56 |                                        // instruction_2 -> "=" expr ";"
                        57 => self.exit_instruction(alt_id),        // instruction_2 -> "(" fun_args ")" ";"
                     /* 5 */                                        // instruction -> "if" expr instruction instruction_1 (never called)
                     /* 10 */                                       // instruction -> Id instruction_2 (never called)
                        22 |                                        // expr_1 -> <R> "^" expr_6 expr_1
                        23 |                                        // expr_1 -> "*" expr_6 expr_1
                        24 |                                        // expr_1 -> "/" expr_6 expr_1
                        25 |                                        // expr_1 -> "+" expr_4 expr_1
                        26 |                                        // expr_1 -> "-" expr_4 expr_1
                        27 |                                        // expr_1 -> "==" expr_2 expr_1
                        28 |                                        // expr_1 -> "!=" expr_2 expr_1
                        29 |                                        // expr_1 -> "<" expr_2 expr_1
                        30 |                                        // expr_1 -> ">" expr_2 expr_1
                        31 |                                        // expr_1 -> "<=" expr_2 expr_1
                        32 => self.exit_expr1(alt_id),              // expr_1 -> ">=" expr_2 expr_1
                        35 |                                        // expr_3 -> <R> "^" expr_6 expr_3 (duplicate of 22)
                        42 |                                        // expr_5 -> <R> "^" expr_6 expr_5 (duplicate of 22)
                        47 => self.exit_expr1(22),                  // expr_7 -> <R> "^" expr_6 expr_7 (duplicate of 22)
                        36 |                                        // expr_3 -> "*" expr_6 expr_3 (duplicate of 23)
                        43 => self.exit_expr1(23),                  // expr_5 -> "*" expr_6 expr_5 (duplicate of 23)
                        37 |                                        // expr_3 -> "/" expr_6 expr_3 (duplicate of 24)
                        44 => self.exit_expr1(24),                  // expr_5 -> "/" expr_6 expr_5 (duplicate of 24)
                        38 => self.exit_expr1(25),                  // expr_3 -> "+" expr_4 expr_3 (duplicate of 25)
                        39 => self.exit_expr1(26),                  // expr_3 -> "-" expr_4 expr_3 (duplicate of 26)
                        49 |                                        // expr_8 -> "!" expr_2
                        50 |                                        // expr_8 -> "(" expr ")"
                        51 |                                        // expr_8 -> "-" expr_8
                        53 |                                        // expr_8 -> Num
                        60 |                                        // expr_9 -> "(" fun_args ")"
                        61 => self.exit_expr8(alt_id),              // expr_9 -> ε
                        12 => {}                                    // expr -> expr_8 expr_1 (not used)
                        33 => {}                                    // expr_1 -> ε (not used)
                        34 => {}                                    // expr_2 -> expr_8 expr_3 (not used)
                        40 => {}                                    // expr_3 -> ε (not used)
                        41 => {}                                    // expr_4 -> expr_8 expr_5 (not used)
                        45 => {}                                    // expr_5 -> ε (not used)
                        46 => {}                                    // expr_6 -> expr_8 expr_7 (not used)
                        48 => {}                                    // expr_7 -> ε (not used)
                     /* 52 */                                       // expr_8 -> Id expr_9 (never called)
                        13 |                                        // fun_args -> expr fun_args_1
                        14 => self.exit_fun_args(alt_id),           // fun_args -> ε
                        20 => self.exit_fun_args1(),                // fun_args_1 -> "," expr fun_args_1
                        21 => {}                                    // fun_args_1 -> ε
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let val = self.stack.pop().unwrap().get_program();
                            self.listener.exit(val);
                        }
                        Terminate::Abort | Terminate::Conclude => self.listener.abort(terminate),
                    }
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).collect::<Vec<_>>().join(", "));
            }
        }

        fn check_abort_request(&self) -> Terminate {
            self.listener.check_abort_request()
        }

        fn abort(&mut self) {
            self.stack.clear();
            self.stack_t.clear();
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            self.listener.get_mut_log()
        }

        fn is_stack_empty(&self) -> bool {
            self.stack.is_empty()
        }

        fn is_stack_t_empty(&self) -> bool {
            self.stack_t.is_empty()
        }

        fn intercept_token(&mut self, token: TokenId, text: &str, _span: &PosSpan) -> TokenId {
            self.listener.intercept_token(token, text)
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

        fn exit_program(&mut self) {
            let plus = self.stack.pop().unwrap().get_program1();
            let ctx = CtxProgram::V1 { plus };
            let val = self.listener.exit_program(ctx);
            self.stack.push(SynValue::Program(val));
        }

        fn init_program1(&mut self) {
            let val = SynProgram1(Vec::new());
            self.stack.push(SynValue::Program1(val));
        }

        fn exit_program1(&mut self) {
            let function = self.stack.pop().unwrap().get_function();
            let Some(SynValue::Program1(SynProgram1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynProgram1 item on wrapper stack");
            };
            plus_acc.push(function);
        }

        fn exit_function(&mut self) {
            let instruction = self.stack.pop().unwrap().get_instruction();
            let fun_params = self.stack.pop().unwrap().get_fun_params();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxFunction::V1 { id, fun_params, instruction };
            let val = self.listener.exit_function(ctx);
            self.stack.push(SynValue::Function(val));
        }

        fn exit_fun_params(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                2 => {
                    let star = self.stack.pop().unwrap().get_fun_params1();
                    CtxFunParams::V1 { star }
                }
                3 => {
                    CtxFunParams::V2
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_fun_params")
            };
            let val = self.listener.exit_fun_params(ctx);
            self.stack.push(SynValue::FunParams(val));
        }

        fn init_fun_params1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            self.stack.push(SynValue::FunParams1(SynFunParams1(vec![id])));
        }

        fn exit_fun_params1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let Some(SynValue::FunParams1(SynFunParams1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynFunParams1 item on wrapper stack");
            };
            star_acc.push(id);
        }

        fn exit_block(&mut self) {
            let star = self.stack.pop().unwrap().get_block1();
            let ctx = CtxBlock::V1 { star };
            let val = self.listener.exit_block(ctx);
            self.stack.push(SynValue::Block(val));
        }

        fn init_block1(&mut self) {
            let val = SynBlock1(Vec::new());
            self.stack.push(SynValue::Block1(val));
        }

        fn exit_block1(&mut self) {
            let instruction = self.stack.pop().unwrap().get_instruction();
            let Some(SynValue::Block1(SynBlock1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynBlock1 item on wrapper stack");
            };
            star_acc.push(instruction);
        }

        fn exit_instruction(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                6 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    CtxInstruction::V1 { id, expr }
                }
                7 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::V4 { expr }
                }
                8 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::V3 { expr }
                }
                9 => {
                    let block = self.stack.pop().unwrap().get_block();
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::V8 { expr, block }
                }
                11 => {
                    let block = self.stack.pop().unwrap().get_block();
                    CtxInstruction::V9 { block }
                }
                54 => {
                    let instruction_2 = self.stack.pop().unwrap().get_instruction();
                    let instruction_1 = self.stack.pop().unwrap().get_instruction();
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::V6 { expr, instruction: [instruction_1, instruction_2] }
                }
                55 => {
                    let instruction = self.stack.pop().unwrap().get_instruction();
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInstruction::V7 { expr, instruction }
                }
                56 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    CtxInstruction::V2 { id, expr }
                }
                57 => {
                    let fun_args = self.stack.pop().unwrap().get_fun_args();
                    let id = self.stack_t.pop().unwrap();
                    CtxInstruction::V5 { id, fun_args }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_instruction")
            };
            let val = self.listener.exit_instruction(ctx);
            self.stack.push(SynValue::Instruction(val));
        }

        fn exit_expr1(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                22 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V2 { expr: [expr_1, expr_2] }
                }
                23 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V3 { expr: [expr_1, expr_2] }
                }
                24 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V4 { expr: [expr_1, expr_2] }
                }
                25 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V5 { expr: [expr_1, expr_2] }
                }
                26 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V6 { expr: [expr_1, expr_2] }
                }
                27 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V8 { expr: [expr_1, expr_2] }
                }
                28 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V9 { expr: [expr_1, expr_2] }
                }
                29 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V10 { expr: [expr_1, expr_2] }
                }
                30 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V11 { expr: [expr_1, expr_2] }
                }
                31 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V12 { expr: [expr_1, expr_2] }
                }
                32 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V13 { expr: [expr_1, expr_2] }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr1")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }

        fn exit_expr8(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                49 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V7 { expr }
                }
                50 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V14 { expr }
                }
                51 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V1 { expr }
                }
                53 => {
                    let num = self.stack_t.pop().unwrap();
                    CtxExpr::V17 { num }
                }
                60 => {
                    let fun_args = self.stack.pop().unwrap().get_fun_args();
                    let id = self.stack_t.pop().unwrap();
                    CtxExpr::V15 { id, fun_args }
                }
                61 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxExpr::V16 { id }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr8")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }

        fn exit_fun_args(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                13 => {
                    let star = self.stack.pop().unwrap().get_fun_args1();
                    CtxFunArgs::V1 { star }
                }
                14 => {
                    CtxFunArgs::V2
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_fun_args")
            };
            let val = self.listener.exit_fun_args(ctx);
            self.stack.push(SynValue::FunArgs(val));
        }

        fn init_fun_args1(&mut self) {
            let expr = self.stack.pop().unwrap().get_expr();
            self.stack.push(SynValue::FunArgs1(SynFunArgs1(vec![expr])));
        }

        fn exit_fun_args1(&mut self) {
            let expr = self.stack.pop().unwrap().get_expr();
            let Some(SynValue::FunArgs1(SynFunArgs1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynFunArgs1 item on wrapper stack");
            };
            star_acc.push(expr);
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