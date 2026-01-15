// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

// ---------------------------------------------------------------------------------------------
// lexicon (automatically generated)
// ---------------------------------------------------------------------------------------------

pub const LEXICON_TAG_CONTENT: &str = r#"
// [test3_lexicon_tag]

lexicon Test1;

fragment DecInteger     : [1-9] ( '_' | [0-9])*;

Add                     : '+';
Div                     : '/';
Equal                   : '=';
Mul                     : '*';
Lpar                    : '(';
Rpar                    : ')';
Semi                    : ';';
Shl                     : '<<';
Shr                     : '>>';
Sub                     : '-';

Let                     : 'let';
Print                   : 'print';

SComment                : '/*' .*? '*/'             -> skip;
SLineComment            : '//' ~[\r\n]*             -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;
Num                     : DecInteger;

// [test3_lexicon_tag]
"#;

// ---------------------------------------------------------------------------------------------
// grammar (automatically generated)
// ---------------------------------------------------------------------------------------------

pub const GRAMMAR_TAG_CONTENT: &str = r#"
// [test3_grammar_tag]

grammar Test1;

program:
    (<L=inst> instruction)*
;

instruction:
    Let Id Equal expr Semi
|   Print expr Semi
;

expr:
    Sub expr
|   expr (Mul | <P> Div) expr
|   expr (Add | <P> Sub) expr
|   expr (Shl | <P> Shr) expr
|   Lpar expr Rpar
|   Id
|   Num
;

// [test3_grammar_tag]
"#;

// ---------------------------------------------------------------------------------------------
// lexer (automatically generated)
// ---------------------------------------------------------------------------------------------

// [test3_lexer_tag]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
use lexigram_lib::segmap::{GroupId, Seg, SegMap};

const NBR_GROUPS: u32 = 23;
const INITIAL_STATE: StateId = 0;
const FIRST_END_STATE: StateId = 5;
const NBR_STATES: StateId = 28;
static ASCII_TO_GROUP: [GroupId; 128] = [
     16,  16,  16,  16,  16,  16,  16,  16,  16,   0,  19,  16,  16,  19,  16,  16,   // 0-15
     16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,   // 16-31
      0,  16,  16,  16,  16,  16,  16,  16,   1,   2,   3,   4,  16,   5,  16,   6,   // 32-47
     15,   7,   7,   7,   7,   7,   7,   7,   7,   7,  16,   8,   9,  10,  11,  16,   // 48-63
     16,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,   // 64-79
     12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  16,  16,  16,  16,  15,   // 80-95
     16,  12,  12,  12,  12,  17,  12,  12,  12,  21,  12,  12,  13,  12,  22,  12,   // 96-111
     14,  12,  18,  12,  20,  12,  12,  12,  12,  12,  12,  16,  16,  16,  16,  16,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 16),
    (Seg(57344, 1114111), 16),
];
static TERMINAL_TABLE: [Terminal;23] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [StateId; 645] = [
      5,   6,   7,   8,   9,  10,  11,  12,  13,   1,  14,   2,  15,  16,  17,  28,  28,  15,  15,   5,  15,  15,  15, // state 0
     28,  28,  28,  28,  28,  28,  28,  28,  28,  19,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 1
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  20,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 2
      3,   3,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 3
      3,   3,   3,   4,   3,   3,  27,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 4
      5,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,   5,  28,  28,  28, // state 5 <skip>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 6 <end:4>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 7 <end:5>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 8 <end:3>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 9 <end:0>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 10 <end:9>
     28,  28,  28,   3,  28,  28,  18,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 11 <end:1>
     28,  28,  28,  28,  28,  28,  28,  12,  28,  28,  28,  28,  28,  28,  28,  12,  28,  28,  28,  28,  28,  28,  28, // state 12 <end:13>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 13 <end:6>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 14 <end:2>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  15, // state 15 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  21,  15,  28,  15,  15,  15, // state 16 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  23,  28,  15,  15,  15, // state 17 <end:12>
     18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  28,  18,  18,  18, // state 18 <skip>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 19 <end:7>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 20 <end:8>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  22,  15,  15, // state 21 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  15, // state 22 <end:10>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  24,  15, // state 23 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  25, // state 24 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  26,  15,  15, // state 25 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  15, // state 26 <end:11>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 27 <skip>
     28 // error group in [nbr_state * nbr_group + nbr_group]
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

// [test3_lexer_tag]

// ---------------------------------------------------------------------------------------------
// parser (automatically generated)
// ---------------------------------------------------------------------------------------------

// [test3_parser_tag]

use lexigram_lib::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
use super::listener_types::test1::*;

const PARSER_NUM_T: usize = 14;
const PARSER_NUM_NT: usize = 10;
static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Div", Some("/")), ("Equal", Some("=")), ("Mul", Some("*")), ("Lpar", Some("(")), ("Rpar", Some(")")), ("Semi", Some(";")), ("Shl", Some("<<")), ("Shr", Some(">>")), ("Sub", Some("-")), ("Let", Some("let")), ("Print", Some("print")), ("Id", None), ("Num", None)];
static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "inst", "instruction", "expr", "expr_1", "expr_2", "expr_3", "expr_4", "expr_5", "expr_6"];
static ALT_VAR: [VarId; 27] = [0, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4, 5, 6, 6, 6, 6, 6, 7, 8, 8, 8, 9, 9, 9, 9];
static PARSING_TABLE: [AltId; 150] = [27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 0, 0, 27, 27, 0, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 1, 1, 27, 27, 2, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 3, 4, 27, 27, 28, 27, 27, 27, 27, 5, 28, 28, 27, 27, 5, 27, 27, 5, 5, 27, 8, 7, 27, 6, 27, 12, 12, 10, 11, 9, 27, 27, 27, 27, 27, 28, 28, 27, 28, 13, 28, 28, 28, 28, 13, 27, 27, 13, 13, 27, 16, 15, 27, 14, 27, 18, 18, 18, 18, 17, 27, 27, 27, 27, 27, 28, 28, 27, 28, 19, 28, 28, 28, 28, 19, 27, 27, 19, 19, 27, 22, 21, 27, 20, 27, 22, 22, 22, 22, 22, 27, 27, 27, 27, 27, 28, 28, 27, 28, 24, 28, 28, 28, 28, 23, 27, 27, 25, 26, 27];
static OPCODES: [&[OpCode]; 27] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::T(6), OpCode::NT(3), OpCode::T(2), OpCode::T(12), OpCode::T(10)], &[OpCode::Exit(4), OpCode::T(6), OpCode::NT(3), OpCode::T(11)], &[OpCode::NT(4), OpCode::Exit(5), OpCode::NT(9)], &[OpCode::Loop(4), OpCode::Exit(6), OpCode::NT(9), OpCode::T(3)], &[OpCode::Loop(4), OpCode::Exit(7), OpCode::NT(9), OpCode::T(1)], &[OpCode::Loop(4), OpCode::Exit(8), OpCode::NT(7), OpCode::T(0)], &[OpCode::Loop(4), OpCode::Exit(9), OpCode::NT(7), OpCode::T(9)], &[OpCode::Loop(4), OpCode::Exit(10), OpCode::NT(5), OpCode::T(7)], &[OpCode::Loop(4), OpCode::Exit(11), OpCode::NT(5), OpCode::T(8)], &[OpCode::Exit(12)], &[OpCode::NT(6), OpCode::Exit(13), OpCode::NT(9)], &[OpCode::Loop(6), OpCode::Exit(14), OpCode::NT(9), OpCode::T(3)], &[OpCode::Loop(6), OpCode::Exit(15), OpCode::NT(9), OpCode::T(1)], &[OpCode::Loop(6), OpCode::Exit(16), OpCode::NT(7), OpCode::T(0)], &[OpCode::Loop(6), OpCode::Exit(17), OpCode::NT(7), OpCode::T(9)], &[OpCode::Exit(18)], &[OpCode::NT(8), OpCode::Exit(19), OpCode::NT(9)], &[OpCode::Loop(8), OpCode::Exit(20), OpCode::NT(9), OpCode::T(3)], &[OpCode::Loop(8), OpCode::Exit(21), OpCode::NT(9), OpCode::T(1)], &[OpCode::Exit(22)], &[OpCode::Exit(23), OpCode::NT(9), OpCode::T(9)], &[OpCode::Exit(24), OpCode::T(5), OpCode::NT(3), OpCode::T(4)], &[OpCode::Exit(25), OpCode::T(12)], &[OpCode::Exit(26), OpCode::T(13)]];
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
    /// `program -> (<L> instruction)*`
    V1,
}
#[derive(Debug)]
pub enum CtxInst {
    /// `<L> instruction` iteration in `program -> ( ►► <L> instruction ◄◄ )*`
    V1 { instruction: SynInstruction },
}
#[derive(Debug)]
pub enum CtxInstruction {
    /// `instruction -> "let" Id "=" expr ";"`
    V1 { id: String, expr: SynExpr },
    /// `instruction -> "print" expr ";"`
    V2 { expr: SynExpr },
}
#[derive(Debug)]
pub enum CtxExpr {
    /// `expr -> "-" expr`
    V1 { expr: SynExpr },
    /// `expr -> expr "*" expr`
    V2 { expr: [SynExpr; 2] },
    /// `expr -> expr <P> "/" expr`
    V3 { expr: [SynExpr; 2] },
    /// `expr -> expr "+" expr`
    V4 { expr: [SynExpr; 2] },
    /// `expr -> expr <P> "-" expr`
    V5 { expr: [SynExpr; 2] },
    /// `expr -> expr "<<" expr`
    V6 { expr: [SynExpr; 2] },
    /// `expr -> expr <P> ">>" expr`
    V7 { expr: [SynExpr; 2] },
    /// `expr -> "(" expr ")"`
    V8 { expr: SynExpr },
    /// `expr -> Id`
    V9 { id: String },
    /// `expr -> Num`
    V10 { num: String },
}

// NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

// /// User-defined type for `program`
// #[derive(Debug, PartialEq)] pub struct SynProgram();
// /// User-defined type for `instruction`
// #[derive(Debug, PartialEq)] pub struct SynInstruction();
// /// User-defined type for `expr`
// #[derive(Debug, PartialEq)] pub struct SynExpr();

#[derive(Debug)]
enum SynValue { Program(SynProgram), Instruction(SynInstruction), Expr(SynExpr) }

impl SynValue {
    fn get_program(self) -> SynProgram {
        if let SynValue::Program(val) = self { val } else { panic!() }
    }
    fn get_instruction(self) -> SynInstruction {
        if let SynValue::Instruction(val) = self { val } else { panic!() }
    }
    fn get_expr(self) -> SynExpr {
        if let SynValue::Expr(val) = self { val } else { panic!() }
    }
}

pub trait Test1Listener {
    /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
    fn check_abort_request(&self) -> Terminate { Terminate::None }
    fn get_mut_log(&mut self) -> &mut impl Logger;
    #[allow(unused_variables)]
    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
    #[allow(unused_variables)]
    fn exit(&mut self, program: SynProgram, span: PosSpan) {}
    #[allow(unused_variables)]
    fn abort(&mut self, terminate: Terminate) {}
    fn init_program(&mut self) {}
    fn exit_program(&mut self, ctx: CtxProgram, spans: Vec<PosSpan>) -> SynProgram;
    fn init_inst(&mut self) {}
    #[allow(unused_variables)]
    fn exit_inst(&mut self, ctx: CtxInst, spans: Vec<PosSpan>) {}
    fn init_instruction(&mut self) {}
    fn exit_instruction(&mut self, ctx: CtxInstruction, spans: Vec<PosSpan>) -> SynInstruction;
    fn init_expr(&mut self) {}
    fn exit_expr(&mut self, ctx: CtxExpr, spans: Vec<PosSpan>) -> SynExpr;
}

pub struct Wrapper<T> {
    verbose: bool,
    listener: T,
    stack: Vec<SynValue>,
    max_stack: usize,
    stack_t: Vec<String>,
    stack_span: Vec<PosSpan>,
}

impl<T: Test1Listener> ListenerWrapper for Wrapper<T> {
    fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
        if self.verbose {
            println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
        }
        if let Some(mut t_data) = t_data {
            self.stack_t.append(&mut t_data);
        }
        match call {
            Call::Enter => {
                if matches!(nt, 1) {
                    self.stack_span.push(PosSpan::empty());
                }
                match nt {
                    0 => self.listener.init_program(),          // program
                    1 => self.listener.init_inst(),             // inst
                    2 => self.listener.init_instruction(),      // instruction
                    3 => self.listener.init_expr(),             // expr
                    4 ..= 9 => {}                               // expr_1, expr_2, expr_3, expr_4, expr_5, expr_6
                    _ => panic!("unexpected enter nonterminal id: {nt}")
                }
            }
            Call::Loop => {}
            Call::Exit => {
                match alt_id {
                    0 => self.exit_program(),                   // program -> inst
                    1 => self.exit_inst(),                      // inst -> <L> instruction inst
                    2 => {}                                     // inst -> <L> ε (not used)
                    3 |                                         // instruction -> "let" Id "=" expr ";"
                    4 => self.exit_instruction(alt_id),         // instruction -> "print" expr ";"
                    6 |                                         // expr_1 -> "*" expr_6 expr_1
                    7 |                                         // expr_1 -> "/" expr_6 expr_1
                    8 |                                         // expr_1 -> "+" expr_4 expr_1
                    9 |                                         // expr_1 -> "-" expr_4 expr_1
                    10 |                                        // expr_1 -> "<<" expr_2 expr_1
                    11 => self.exit_expr1(alt_id),              // expr_1 -> ">>" expr_2 expr_1
                    14 |                                        // expr_3 -> "*" expr_6 expr_3 (duplicate of 6)
                    20 => self.exit_expr1(6),                   // expr_5 -> "*" expr_6 expr_5 (duplicate of 6)
                    15 |                                        // expr_3 -> "/" expr_6 expr_3 (duplicate of 7)
                    21 => self.exit_expr1(7),                   // expr_5 -> "/" expr_6 expr_5 (duplicate of 7)
                    16 => self.exit_expr1(8),                   // expr_3 -> "+" expr_4 expr_3 (duplicate of 8)
                    17 => self.exit_expr1(9),                   // expr_3 -> "-" expr_4 expr_3 (duplicate of 9)
                    23 |                                        // expr_6 -> "-" expr_6
                    24 |                                        // expr_6 -> "(" expr ")"
                    25 |                                        // expr_6 -> Id
                    26 => self.exit_expr6(alt_id),              // expr_6 -> Num
                    5 => {}                                     // expr -> expr_6 expr_1 (not used)
                    12 => {}                                    // expr_1 -> ε (not used)
                    13 => {}                                    // expr_2 -> expr_6 expr_3 (not used)
                    18 => {}                                    // expr_3 -> ε (not used)
                    19 => {}                                    // expr_4 -> expr_6 expr_5 (not used)
                    22 => {}                                    // expr_5 -> ε (not used)
                    _ => panic!("unexpected exit alternative id: {alt_id}")
                }
            }
            Call::End(terminate) => {
                match terminate {
                    Terminate::None => {
                        let val = self.stack.pop().unwrap().get_program();
                        let span = self.stack_span.pop().unwrap();
                        self.listener.exit(val, span);
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
        self.stack_span.clear();
        self.stack_t.clear();
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        self.listener.get_mut_log()
    }

    fn push_span(&mut self, span: PosSpan) {
        self.stack_span.push(span);
    }

    fn is_stack_empty(&self) -> bool {
        self.stack.is_empty()
    }

    fn is_stack_t_empty(&self) -> bool {
        self.stack_t.is_empty()
    }

    fn is_stack_span_empty(&self) -> bool {
        self.stack_span.is_empty()
    }

    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        self.listener.intercept_token(token, text, span)
    }
}

impl<T: Test1Listener> Wrapper<T> {
    pub fn new(listener: T, verbose: bool) -> Self {
        Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new(), stack_span: Vec::new() }
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
        let ctx = CtxProgram::V1;
        let n = 1;
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        let mut new_span = PosSpan::empty();
        spans.iter().for_each(|span| new_span += span);
        self.stack_span.push(new_span);
        let val = self.listener.exit_program(ctx, spans);
        self.stack.push(SynValue::Program(val));
    }

    fn exit_inst(&mut self) {
        let instruction = self.stack.pop().unwrap().get_instruction();
        let ctx = CtxInst::V1 { instruction };
        let n = 2;
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        let mut new_span = PosSpan::empty();
        spans.iter().for_each(|span| new_span += span);
        self.stack_span.push(new_span);
        self.listener.exit_inst(ctx, spans);
    }

    fn exit_instruction(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            3 => {
                let expr = self.stack.pop().unwrap().get_expr();
                let id = self.stack_t.pop().unwrap();
                (5, CtxInstruction::V1 { id, expr })
            }
            4 => {
                let expr = self.stack.pop().unwrap().get_expr();
                (3, CtxInstruction::V2 { expr })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_instruction")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        let mut new_span = PosSpan::empty();
        spans.iter().for_each(|span| new_span += span);
        self.stack_span.push(new_span);
        let val = self.listener.exit_instruction(ctx, spans);
        self.stack.push(SynValue::Instruction(val));
    }

    fn exit_expr1(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            6 => {
                let expr_2 = self.stack.pop().unwrap().get_expr();
                let expr_1 = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V2 { expr: [expr_1, expr_2] })
            }
            7 => {
                let expr_2 = self.stack.pop().unwrap().get_expr();
                let expr_1 = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V3 { expr: [expr_1, expr_2] })
            }
            8 => {
                let expr_2 = self.stack.pop().unwrap().get_expr();
                let expr_1 = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V4 { expr: [expr_1, expr_2] })
            }
            9 => {
                let expr_2 = self.stack.pop().unwrap().get_expr();
                let expr_1 = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V5 { expr: [expr_1, expr_2] })
            }
            10 => {
                let expr_2 = self.stack.pop().unwrap().get_expr();
                let expr_1 = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V6 { expr: [expr_1, expr_2] })
            }
            11 => {
                let expr_2 = self.stack.pop().unwrap().get_expr();
                let expr_1 = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V7 { expr: [expr_1, expr_2] })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_expr1")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        let mut new_span = PosSpan::empty();
        spans.iter().for_each(|span| new_span += span);
        self.stack_span.push(new_span);
        let val = self.listener.exit_expr(ctx, spans);
        self.stack.push(SynValue::Expr(val));
    }

    fn exit_expr6(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            23 => {
                let expr = self.stack.pop().unwrap().get_expr();
                (2, CtxExpr::V1 { expr })
            }
            24 => {
                let expr = self.stack.pop().unwrap().get_expr();
                (3, CtxExpr::V8 { expr })
            }
            25 => {
                let id = self.stack_t.pop().unwrap();
                (1, CtxExpr::V9 { id })
            }
            26 => {
                let num = self.stack_t.pop().unwrap();
                (1, CtxExpr::V10 { num })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_expr6")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        let mut new_span = PosSpan::empty();
        spans.iter().for_each(|span| new_span += span);
        self.stack_span.push(new_span);
        let val = self.listener.exit_expr(ctx, spans);
        self.stack.push(SynValue::Expr(val));
    }
}

// [test3_parser_tag]

// ---------------------------------------------------------------------------------------------
