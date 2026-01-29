// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
#![allow(unused)]

// [lexiparser]

use lexigram_lib::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
use lexiparser_types::*;

const PARSER_NUM_T: usize = 34;
const PARSER_NUM_NT: usize = 34;
static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Dot", Some(".")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Minus", Some("-")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("More", Some("more")), ("Skip", Some("skip")), ("Type", Some("type")), ("Channel", Some("channel")), ("Hook", Some("hook")), ("Id", None), ("CharLit", None), ("StrLit", None), ("FixedSet", None), ("LSbracket", Some("[")), ("RSbracket", Some("]")), ("SetChar", None)];
static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "file_item", "header", "declaration", "option", "rule", "opt_str_lit", "rule_fragment_name", "rule_terminal_name", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "char_set", "char_set_one", "file_1", "option_1", "actions_1", "alt_items_1", "alt_item_1", "char_set_1", "rule_1", "rule_2", "repeat_item_1", "item_1", "char_set_one_1", "alt_item_2", "char_set_2", "rule_3", "repeat_item_2", "repeat_item_3"];
static ALT_VAR: [VarId; 71] = [0, 0, 1, 1, 1, 2, 3, 4, 5, 5, 5, 6, 6, 7, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10, 11, 12, 13, 14, 15, 15, 15, 15, 15, 15, 16, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 23, 24, 24, 25, 25, 26, 26, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 33];
static PARSING_TABLE: [AltId; 1190] = [71, 71, 71, 71, 71, 71, 1, 71, 71, 71, 71, 71, 71, 71, 71, 71, 1, 1, 0, 1, 71, 71, 71, 71, 71, 71, 71, 1, 71, 71, 71, 71, 71, 71, 1, 71, 71, 71, 71, 71, 71, 4, 71, 71, 71, 71, 71, 71, 71, 71, 71, 2, 4, 71, 3, 71, 71, 71, 71, 71, 71, 71, 4, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 5, 72, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 71, 6, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 7, 72, 71, 72, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 8, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 9, 71, 72, 71, 71, 71, 71, 71, 71, 71, 10, 71, 71, 71, 71, 71, 71, 72, 12, 11, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 12, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 13, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 14, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 15, 15, 15, 15, 15, 15, 15, 15, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 16, 18, 17, 20, 19, 21, 22, 23, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 24, 71, 71, 24, 24, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 24, 24, 24, 24, 24, 71, 71, 71, 72, 71, 71, 25, 71, 71, 25, 25, 71, 71, 71, 71, 71, 72, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 25, 25, 25, 25, 25, 71, 71, 71, 72, 71, 71, 26, 71, 71, 26, 26, 71, 71, 72, 71, 71, 72, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 26, 26, 26, 26, 26, 71, 71, 71, 72, 71, 71, 27, 71, 71, 27, 27, 71, 71, 72, 71, 71, 72, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 27, 27, 27, 27, 27, 71, 71, 71, 72, 71, 71, 33, 71, 71, 28, 29, 71, 72, 72, 72, 71, 72, 72, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 30, 31, 32, 33, 33, 71, 71, 71, 72, 71, 71, 35, 71, 71, 72, 72, 71, 72, 72, 72, 71, 72, 72, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 36, 34, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 37, 71, 72, 38, 71, 71, 71, 71, 71, 71, 71, 39, 71, 71, 71, 71, 71, 71, 71, 71, 71, 39, 39, 71, 39, 71, 71, 71, 71, 71, 71, 71, 39, 71, 71, 71, 71, 71, 71, 40, 71, 71, 41, 71, 71, 71, 71, 71, 71, 71, 71, 71, 42, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 43, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 44, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 46, 71, 71, 71, 71, 71, 71, 71, 71, 71, 45, 71, 71, 46, 46, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 47, 71, 71, 47, 47, 71, 71, 72, 71, 71, 72, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 47, 47, 47, 47, 47, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 48, 71, 72, 48, 71, 49, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 50, 71, 72, 72, 71, 72, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 71, 51, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 71, 72, 71, 71, 71, 71, 71, 71, 71, 52, 71, 71, 71, 71, 71, 71, 72, 56, 71, 71, 56, 71, 71, 56, 56, 71, 53, 56, 54, 71, 56, 56, 55, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 56, 56, 56, 56, 56, 71, 71, 71, 58, 71, 71, 58, 57, 71, 58, 58, 71, 58, 58, 58, 71, 58, 58, 58, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 58, 58, 58, 58, 58, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 59, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 60, 71, 60, 60, 71, 62, 71, 71, 61, 71, 71, 61, 61, 71, 71, 62, 71, 71, 62, 62, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 61, 61, 61, 61, 61, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 63, 71, 64, 63, 71, 65, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 71, 66, 71, 72, 72, 71, 72, 71, 71, 71, 71, 71, 71, 71, 72, 71, 71, 71, 71, 71, 71, 72, 68, 71, 71, 68, 71, 71, 68, 68, 71, 71, 68, 67, 71, 68, 68, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 68, 68, 68, 68, 68, 71, 71, 71, 70, 71, 71, 70, 71, 71, 70, 70, 71, 71, 70, 69, 71, 70, 70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 70, 70, 70, 70, 70, 71, 71, 71];
static OPCODES: [&[OpCode]; 71] = [&[OpCode::Exit(0), OpCode::NT(18), OpCode::NT(2)], &[OpCode::Exit(1), OpCode::NT(18)], &[OpCode::Exit(2), OpCode::NT(4)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(14), OpCode::T(27), OpCode::T(18)], &[OpCode::Exit(6), OpCode::T(14), OpCode::T(27), OpCode::T(19)], &[OpCode::Exit(7), OpCode::T(12), OpCode::NT(19), OpCode::T(27), OpCode::T(5), OpCode::T(16)], &[OpCode::NT(24), OpCode::NT(6), OpCode::T(13), OpCode::NT(8), OpCode::T(6)], &[OpCode::Exit(9), OpCode::T(14), OpCode::NT(11), OpCode::T(1), OpCode::NT(7)], &[OpCode::NT(25), OpCode::NT(8)], &[OpCode::Exit(11), OpCode::T(29), OpCode::T(1)], &[OpCode::Exit(12)], &[OpCode::Exit(13), OpCode::T(27), OpCode::T(17)], &[OpCode::Exit(14), OpCode::T(27)], &[OpCode::Exit(15), OpCode::NT(20), OpCode::NT(10)], &[OpCode::Exit(16), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(19)], &[OpCode::Exit(17), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(21)], &[OpCode::Exit(18), OpCode::T(20)], &[OpCode::Exit(19), OpCode::T(23)], &[OpCode::Exit(20), OpCode::T(22)], &[OpCode::Exit(21), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(24)], &[OpCode::Exit(22), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(25)], &[OpCode::Exit(23), OpCode::T(26)], &[OpCode::Exit(24), OpCode::NT(12)], &[OpCode::Exit(25), OpCode::NT(21), OpCode::NT(13)], &[OpCode::Exit(26), OpCode::NT(22)], &[OpCode::NT(26), OpCode::NT(15)], &[OpCode::Exit(28), OpCode::T(13), OpCode::NT(12), OpCode::T(6)], &[OpCode::Exit(29), OpCode::NT(15), OpCode::T(7)], &[OpCode::Exit(30), OpCode::T(27)], &[OpCode::NT(27), OpCode::T(28)], &[OpCode::Exit(32), OpCode::T(29)], &[OpCode::Exit(33), OpCode::NT(16)], &[OpCode::Exit(34), OpCode::T(32), OpCode::NT(23), OpCode::T(31)], &[OpCode::Exit(35), OpCode::T(3)], &[OpCode::Exit(36), OpCode::T(30)], &[OpCode::Exit(37), OpCode::T(30)], &[OpCode::NT(28), OpCode::T(33)], &[OpCode::Loop(18), OpCode::Exit(39), OpCode::NT(1)], &[OpCode::Exit(40)], &[OpCode::Loop(19), OpCode::Exit(41), OpCode::T(27), OpCode::T(2)], &[OpCode::Exit(42)], &[OpCode::Loop(20), OpCode::Exit(43), OpCode::NT(10), OpCode::T(2)], &[OpCode::Exit(44)], &[OpCode::Loop(21), OpCode::Exit(45), OpCode::NT(13), OpCode::T(10)], &[OpCode::Exit(46)], &[OpCode::NT(29), OpCode::NT(14)], &[OpCode::NT(30), OpCode::NT(17)], &[OpCode::Exit(49), OpCode::T(14), OpCode::T(26), OpCode::T(0)], &[OpCode::Exit(50), OpCode::T(14)], &[OpCode::NT(31), OpCode::NT(11), OpCode::T(1)], &[OpCode::Exit(52), OpCode::T(14), OpCode::T(27)], &[OpCode::NT(32), OpCode::T(9)], &[OpCode::Exit(54), OpCode::T(11)], &[OpCode::NT(33), OpCode::T(15)], &[OpCode::Exit(56)], &[OpCode::Exit(57), OpCode::T(28), OpCode::T(4)], &[OpCode::Exit(58)], &[OpCode::Exit(59), OpCode::T(33), OpCode::T(8)], &[OpCode::Exit(60)], &[OpCode::Loop(22), OpCode::Exit(61)], &[OpCode::Exit(62)], &[OpCode::Loop(23), OpCode::Exit(63)], &[OpCode::Exit(64)], &[OpCode::Exit(65), OpCode::T(14), OpCode::NT(9), OpCode::T(0)], &[OpCode::Exit(66), OpCode::T(14)], &[OpCode::Exit(67), OpCode::T(11)], &[OpCode::Exit(68)], &[OpCode::Exit(69), OpCode::T(11)], &[OpCode::Exit(70)]];
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
pub enum CtxFile {
    /// `file -> header file_item*`
    V1 { header: SynHeader, star: SynFile1 },
    /// `file -> file_item*`
    V2 { star: SynFile1 },
}
#[derive(Debug)]
pub enum CtxFileItem {
    /// `file_item -> option`
    V1 { option: SynOption },
    /// `file_item -> declaration`
    V2 { declaration: SynDeclaration },
    /// `file_item -> rule`
    V3 { rule: SynRule },
}
#[derive(Debug)]
pub enum CtxHeader {
    /// `header -> "lexicon" Id ";"`
    V1 { id: String },
}
#[derive(Debug)]
pub enum CtxDeclaration {
    /// `declaration -> "mode" Id ";"`
    V1 { id: String },
}
#[derive(Debug)]
pub enum CtxOption {
    /// `option -> "channels" "{" Id ("," Id)* "}"`
    V1 { star: SynOption1 },
}
#[derive(Debug)]
pub enum CtxRule {
    /// `rule -> rule_fragment_name ":" match ";"`
    V1 { rule_fragment_name: SynRuleFragmentName, match1: SynMatch },
    /// `rule -> rule_terminal_name ":" match "->" actions ";"`
    V2 { rule_terminal_name: SynRuleTerminalName, match1: SynMatch, actions: SynActions },
    /// `rule -> rule_terminal_name ":" match ";"`
    V3 { rule_terminal_name: SynRuleTerminalName, match1: SynMatch },
    /// `rule -> "(" rule_terminal_name ")" opt_str_lit "->" "hook" ";"`
    V4 { rule_terminal_name: SynRuleTerminalName, opt_str_lit: SynOptStrLit },
    /// `rule -> "(" rule_terminal_name ")" opt_str_lit ";"`
    V5 { rule_terminal_name: SynRuleTerminalName, opt_str_lit: SynOptStrLit },
    /// `rule -> rule_terminal_name Id ";"`
    V6 { rule_terminal_name: SynRuleTerminalName, id: String },
}
#[derive(Debug)]
pub enum CtxOptStrLit {
    /// `opt_str_lit -> ":" StrLit`
    V1 { strlit: String },
    /// `opt_str_lit -> ε`
    V2,
}
#[derive(Debug)]
pub enum CtxRuleFragmentName {
    /// `rule_fragment_name -> "fragment" Id`
    V1 { id: String },
}
#[derive(Debug)]
pub enum CtxRuleTerminalName {
    /// `rule_terminal_name -> Id`
    V1 { id: String },
}
#[derive(Debug)]
pub enum CtxActions {
    /// `actions -> action ("," action)*`
    V1 { star: SynActions1 },
}
#[derive(Debug)]
pub enum CtxAction {
    /// `action -> "mode" "(" Id ")"`
    V1 { id: String },
    /// `action -> "push" "(" Id ")"`
    V2 { id: String },
    /// `action -> "pop"`
    V3,
    /// `action -> "skip"`
    V4,
    /// `action -> "more"`
    V5,
    /// `action -> "type" "(" Id ")"`
    V6 { id: String },
    /// `action -> "channel" "(" Id ")"`
    V7 { id: String },
    /// `action -> "hook"`
    V8,
}
#[derive(Debug)]
pub enum CtxMatch {
    /// `match -> alt_items`
    V1 { alt_items: SynAltItems },
}
#[derive(Debug)]
pub enum CtxAltItems {
    /// `alt_items -> alt_item ("|" alt_item)*`
    V1 { star: SynAltItems1 },
}
#[derive(Debug)]
pub enum CtxAltItem {
    /// `alt_item -> repeat_item+`
    V1 { plus: SynAltItem1 },
}
#[derive(Debug)]
pub enum CtxRepeatItem {
    /// `repeat_item -> item "*" "?"`
    V1 { item: SynItem },
    /// `repeat_item -> item "*"`
    V2 { item: SynItem },
    /// `repeat_item -> item "+" "?"`
    V3 { item: SynItem },
    /// `repeat_item -> item "+"`
    V4 { item: SynItem },
    /// `repeat_item -> item "?"`
    V5 { item: SynItem },
    /// `repeat_item -> item`
    V6 { item: SynItem },
}
#[derive(Debug)]
pub enum CtxItem {
    /// `item -> Id`
    V1 { id: String },
    /// `item -> CharLit ".." CharLit`
    V2 { charlit: [String; 2] },
    /// `item -> CharLit`
    V3 { charlit: String },
    /// `item -> StrLit`
    V4 { strlit: String },
    /// `item -> char_set`
    V5 { char_set: SynCharSet },
    /// `item -> "(" alt_items ")"`
    V6 { alt_items: SynAltItems },
    /// `item -> "~" item`
    V7 { item: SynItem },
}
#[derive(Debug)]
pub enum CtxCharSet {
    /// `char_set -> "[" char_set_one+ "]"`
    V1 { plus: SynCharSet1 },
    /// `char_set -> "."`
    V2,
    /// `char_set -> FixedSet`
    V3 { fixedset: String },
}
#[derive(Debug)]
pub enum CtxCharSetOne {
    /// `char_set_one -> SetChar "-" SetChar`
    V1 { setchar: [String; 2] },
    /// `char_set_one -> SetChar`
    V2 { setchar: String },
    /// `char_set_one -> FixedSet`
    V3 { fixedset: String },
}

// NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

// /// User-defined type for `file`
// #[derive(Debug, PartialEq)] pub struct SynFile();
// /// User-defined type for `file_item`
// #[derive(Debug, PartialEq)] pub struct SynFileItem();
// /// User-defined type for `header`
// #[derive(Debug, PartialEq)] pub struct SynHeader();
// /// User-defined type for `declaration`
// #[derive(Debug, PartialEq)] pub struct SynDeclaration();
// /// User-defined type for `option`
// #[derive(Debug, PartialEq)] pub struct SynOption();
// /// User-defined type for `rule`
// #[derive(Debug, PartialEq)] pub struct SynRule();
// /// User-defined type for `opt_str_lit`
// #[derive(Debug, PartialEq)] pub struct SynOptStrLit();
// /// User-defined type for `rule_fragment_name`
// #[derive(Debug, PartialEq)] pub struct SynRuleFragmentName();
// /// User-defined type for `rule_terminal_name`
// #[derive(Debug, PartialEq)] pub struct SynRuleTerminalName();
// /// User-defined type for `actions`
// #[derive(Debug, PartialEq)] pub struct SynActions();
// /// User-defined type for `action`
// #[derive(Debug, PartialEq)] pub struct SynAction();
// /// User-defined type for `match`
// #[derive(Debug, PartialEq)] pub struct SynMatch();
// /// User-defined type for `alt_items`
// #[derive(Debug, PartialEq)] pub struct SynAltItems();
// /// User-defined type for `alt_item`
// #[derive(Debug, PartialEq)] pub struct SynAltItem();
// /// User-defined type for `repeat_item`
// #[derive(Debug, PartialEq)] pub struct SynRepeatItem();
// /// User-defined type for `item`
// #[derive(Debug, PartialEq)] pub struct SynItem();
// /// User-defined type for `char_set`
// #[derive(Debug, PartialEq)] pub struct SynCharSet();
// /// User-defined type for `char_set_one`
// #[derive(Debug, PartialEq)] pub struct SynCharSetOne();
/// Computed `file_item*` array in `file -> header  ►► file_item* ◄◄  |  ►► file_item* ◄◄ `
#[derive(Debug, PartialEq)]
pub struct SynFile1(pub Vec<SynFileItem>);
/// Computed `("," Id)*` array in `option -> "channels" "{" Id  ►► ("," Id)* ◄◄  "}"`
#[derive(Debug, PartialEq)]
pub struct SynOption1(pub Vec<String>);
/// Computed `("," action)*` array in `actions -> action  ►► ("," action)* ◄◄ `
#[derive(Debug, PartialEq)]
pub struct SynActions1(pub Vec<SynAction>);
/// Computed `("|" alt_item)*` array in `alt_items -> alt_item  ►► ("|" alt_item)* ◄◄ `
#[derive(Debug, PartialEq)]
pub struct SynAltItems1(pub Vec<SynAltItem>);
/// Computed `repeat_item+` array in `alt_item ->  ►► repeat_item+ ◄◄ `
#[derive(Debug, PartialEq)]
pub struct SynAltItem1(pub Vec<SynRepeatItem>);
/// Computed `char_set_one+` array in `char_set -> "["  ►► char_set_one+ ◄◄  "]" | "." | FixedSet`
#[derive(Debug, PartialEq)]
pub struct SynCharSet1(pub Vec<SynCharSetOne>);

#[derive(Debug)]
enum SynValue { File(SynFile), FileItem(SynFileItem), Header(SynHeader), Declaration(SynDeclaration), Option(SynOption), Rule(SynRule), OptStrLit(SynOptStrLit), RuleFragmentName(SynRuleFragmentName), RuleTerminalName(SynRuleTerminalName), Actions(SynActions), Action(SynAction), Match(SynMatch), AltItems(SynAltItems), AltItem(SynAltItem), RepeatItem(SynRepeatItem), Item(SynItem), CharSet(SynCharSet), CharSetOne(SynCharSetOne), File1(SynFile1), Option1(SynOption1), Actions1(SynActions1), AltItems1(SynAltItems1), AltItem1(SynAltItem1), CharSet1(SynCharSet1) }

impl SynValue {
    fn get_file(self) -> SynFile {
        if let SynValue::File(val) = self { val } else { panic!() }
    }
    fn get_file_item(self) -> SynFileItem {
        if let SynValue::FileItem(val) = self { val } else { panic!() }
    }
    fn get_header(self) -> SynHeader {
        if let SynValue::Header(val) = self { val } else { panic!() }
    }
    fn get_declaration(self) -> SynDeclaration {
        if let SynValue::Declaration(val) = self { val } else { panic!() }
    }
    fn get_option(self) -> SynOption {
        if let SynValue::Option(val) = self { val } else { panic!() }
    }
    fn get_rule(self) -> SynRule {
        if let SynValue::Rule(val) = self { val } else { panic!() }
    }
    fn get_opt_str_lit(self) -> SynOptStrLit {
        if let SynValue::OptStrLit(val) = self { val } else { panic!() }
    }
    fn get_rule_fragment_name(self) -> SynRuleFragmentName {
        if let SynValue::RuleFragmentName(val) = self { val } else { panic!() }
    }
    fn get_rule_terminal_name(self) -> SynRuleTerminalName {
        if let SynValue::RuleTerminalName(val) = self { val } else { panic!() }
    }
    fn get_actions(self) -> SynActions {
        if let SynValue::Actions(val) = self { val } else { panic!() }
    }
    fn get_action(self) -> SynAction {
        if let SynValue::Action(val) = self { val } else { panic!() }
    }
    fn get_match(self) -> SynMatch {
        if let SynValue::Match(val) = self { val } else { panic!() }
    }
    fn get_alt_items(self) -> SynAltItems {
        if let SynValue::AltItems(val) = self { val } else { panic!() }
    }
    fn get_alt_item(self) -> SynAltItem {
        if let SynValue::AltItem(val) = self { val } else { panic!() }
    }
    fn get_repeat_item(self) -> SynRepeatItem {
        if let SynValue::RepeatItem(val) = self { val } else { panic!() }
    }
    fn get_item(self) -> SynItem {
        if let SynValue::Item(val) = self { val } else { panic!() }
    }
    fn get_char_set(self) -> SynCharSet {
        if let SynValue::CharSet(val) = self { val } else { panic!() }
    }
    fn get_char_set_one(self) -> SynCharSetOne {
        if let SynValue::CharSetOne(val) = self { val } else { panic!() }
    }
    fn get_file1(self) -> SynFile1 {
        if let SynValue::File1(val) = self { val } else { panic!() }
    }
    fn get_option1(self) -> SynOption1 {
        if let SynValue::Option1(val) = self { val } else { panic!() }
    }
    fn get_actions1(self) -> SynActions1 {
        if let SynValue::Actions1(val) = self { val } else { panic!() }
    }
    fn get_alt_items1(self) -> SynAltItems1 {
        if let SynValue::AltItems1(val) = self { val } else { panic!() }
    }
    fn get_alt_item1(self) -> SynAltItem1 {
        if let SynValue::AltItem1(val) = self { val } else { panic!() }
    }
    fn get_char_set1(self) -> SynCharSet1 {
        if let SynValue::CharSet1(val) = self { val } else { panic!() }
    }
}

pub trait LexiParserListener {
    /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
    fn check_abort_request(&self) -> Terminate { Terminate::None }
    fn get_mut_log(&mut self) -> &mut impl Logger;
    #[allow(unused_variables)]
    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
    #[allow(unused_variables)]
    fn exit(&mut self, file: SynFile, span: PosSpan) {}
    #[allow(unused_variables)]
    fn abort(&mut self, terminate: Terminate) {}
    fn init_file(&mut self) {}
    fn exit_file(&mut self, ctx: CtxFile, spans: Vec<PosSpan>) -> SynFile;
    fn init_file_item(&mut self) {}
    fn exit_file_item(&mut self, ctx: CtxFileItem, spans: Vec<PosSpan>) -> SynFileItem;
    fn init_header(&mut self) {}
    fn exit_header(&mut self, ctx: CtxHeader, spans: Vec<PosSpan>) -> SynHeader;
    fn init_declaration(&mut self) {}
    fn exit_declaration(&mut self, ctx: CtxDeclaration, spans: Vec<PosSpan>) -> SynDeclaration;
    fn init_option(&mut self) {}
    fn exit_option(&mut self, ctx: CtxOption, spans: Vec<PosSpan>) -> SynOption;
    fn init_rule(&mut self) {}
    fn exit_rule(&mut self, ctx: CtxRule, spans: Vec<PosSpan>) -> SynRule;
    fn init_opt_str_lit(&mut self) {}
    fn exit_opt_str_lit(&mut self, ctx: CtxOptStrLit, spans: Vec<PosSpan>) -> SynOptStrLit;
    fn init_rule_fragment_name(&mut self) {}
    fn exit_rule_fragment_name(&mut self, ctx: CtxRuleFragmentName, spans: Vec<PosSpan>) -> SynRuleFragmentName;
    fn init_rule_terminal_name(&mut self) {}
    fn exit_rule_terminal_name(&mut self, ctx: CtxRuleTerminalName, spans: Vec<PosSpan>) -> SynRuleTerminalName;
    fn init_actions(&mut self) {}
    fn exit_actions(&mut self, ctx: CtxActions, spans: Vec<PosSpan>) -> SynActions;
    fn init_action(&mut self) {}
    fn exit_action(&mut self, ctx: CtxAction, spans: Vec<PosSpan>) -> SynAction;
    fn init_match(&mut self) {}
    fn exit_match(&mut self, ctx: CtxMatch, spans: Vec<PosSpan>) -> SynMatch;
    fn init_alt_items(&mut self) {}
    fn exit_alt_items(&mut self, ctx: CtxAltItems, spans: Vec<PosSpan>) -> SynAltItems;
    fn init_alt_item(&mut self) {}
    fn exit_alt_item(&mut self, ctx: CtxAltItem, spans: Vec<PosSpan>) -> SynAltItem;
    fn init_repeat_item(&mut self) {}
    fn exit_repeat_item(&mut self, ctx: CtxRepeatItem, spans: Vec<PosSpan>) -> SynRepeatItem;
    fn init_item(&mut self) {}
    fn exit_item(&mut self, ctx: CtxItem, spans: Vec<PosSpan>) -> SynItem;
    fn init_char_set(&mut self) {}
    fn exit_char_set(&mut self, ctx: CtxCharSet, spans: Vec<PosSpan>) -> SynCharSet;
    fn init_char_set_one(&mut self) {}
    fn exit_char_set_one(&mut self, ctx: CtxCharSetOne, spans: Vec<PosSpan>) -> SynCharSetOne;
}

pub struct Wrapper<T> {
    verbose: bool,
    listener: T,
    stack: Vec<SynValue>,
    max_stack: usize,
    stack_t: Vec<String>,
    stack_span: Vec<PosSpan>,
}

impl<T: LexiParserListener> ListenerWrapper for Wrapper<T> {
    fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
        if self.verbose {
            println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
        }
        if let Some(mut t_data) = t_data {
            self.stack_t.append(&mut t_data);
        }
        match call {
            Call::Enter => {
                if matches!(nt, 18 | 22 | 23) {
                    self.stack_span.push(PosSpan::empty());
                }
                match nt {
                    0 => self.listener.init_file(),               // file
                    18 => self.init_file1(),                      // file_1
                    1 => self.listener.init_file_item(),          // file_item
                    2 => self.listener.init_header(),             // header
                    3 => self.listener.init_declaration(),        // declaration
                    4 => self.listener.init_option(),             // option
                    19 => self.init_option1(),                    // option_1
                    5 => self.listener.init_rule(),               // rule
                    24 | 25 => {}                                 // rule_1, rule_2
                    31 => {}                                      // rule_3
                    6 => self.listener.init_opt_str_lit(),        // opt_str_lit
                    7 => self.listener.init_rule_fragment_name(), // rule_fragment_name
                    8 => self.listener.init_rule_terminal_name(), // rule_terminal_name
                    9 => self.listener.init_actions(),            // actions
                    20 => self.init_actions1(),                   // actions_1
                    10 => self.listener.init_action(),            // action
                    11 => self.listener.init_match(),             // match
                    12 => self.listener.init_alt_items(),         // alt_items
                    21 => self.init_alt_items1(),                 // alt_items_1
                    13 => self.listener.init_alt_item(),          // alt_item
                    22 => self.init_alt_item1(),                  // alt_item_1
                    29 => {}                                      // alt_item_2
                    14 => self.listener.init_repeat_item(),       // repeat_item
                    26 => {}                                      // repeat_item_1
                    32 | 33 => {}                                 // repeat_item_2, repeat_item_3
                    15 => self.listener.init_item(),              // item
                    27 => {}                                      // item_1
                    16 => self.listener.init_char_set(),          // char_set
                    23 => self.init_char_set1(),                  // char_set_1
                    30 => {}                                      // char_set_2
                    17 => self.listener.init_char_set_one(),      // char_set_one
                    28 => {}                                      // char_set_one_1
                    _ => panic!("unexpected enter nonterminal id: {nt}")
                }
            }
            Call::Loop => {}
            Call::Exit => {
                match alt_id {
                    0 |                                         // file -> header file_1
                    1 => self.exit_file(alt_id),                // file -> file_1
                    39 => self.exit_file1(),                    // file_1 -> file_item file_1
                    40 => {}                                    // file_1 -> ε
                    2 |                                         // file_item -> option
                    3 |                                         // file_item -> declaration
                    4 => self.exit_file_item(alt_id),           // file_item -> rule
                    5 => self.exit_header(),                    // header -> "lexicon" Id ";"
                    6 => self.exit_declaration(),               // declaration -> "mode" Id ";"
                    7 => self.exit_option(),                    // option -> "channels" "{" Id option_1 "}"
                    41 => self.exit_option1(),                  // option_1 -> "," Id option_1
                    42 => {}                                    // option_1 -> ε
                    9 |                                         // rule -> rule_fragment_name ":" match ";"
                    49 |                                        // rule_1 -> "->" "hook" ";"
                    50 |                                        // rule_1 -> ";"
                    52 |                                        // rule_2 -> Id ";"
                    65 |                                        // rule_3 -> "->" actions ";"
                    66 => self.exit_rule(alt_id),               // rule_3 -> ";"
                 /* 8 */                                        // rule -> "(" rule_terminal_name ")" opt_str_lit rule_1 (never called)
                 /* 10 */                                       // rule -> rule_terminal_name rule_2 (never called)
                 /* 51 */                                       // rule_2 -> ":" match rule_3 (never called)
                    11 |                                        // opt_str_lit -> ":" StrLit
                    12 => self.exit_opt_str_lit(alt_id),        // opt_str_lit -> ε
                    13 => self.exit_rule_fragment_name(),       // rule_fragment_name -> "fragment" Id
                    14 => self.exit_rule_terminal_name(),       // rule_terminal_name -> Id
                    15 => self.exit_actions(),                  // actions -> action actions_1
                    43 => self.exit_actions1(),                 // actions_1 -> "," action actions_1
                    44 => {}                                    // actions_1 -> ε
                    16 |                                        // action -> "mode" "(" Id ")"
                    17 |                                        // action -> "push" "(" Id ")"
                    18 |                                        // action -> "pop"
                    19 |                                        // action -> "skip"
                    20 |                                        // action -> "more"
                    21 |                                        // action -> "type" "(" Id ")"
                    22 |                                        // action -> "channel" "(" Id ")"
                    23 => self.exit_action(alt_id),             // action -> "hook"
                    24 => self.exit_match(),                    // match -> alt_items
                    25 => self.exit_alt_items(),                // alt_items -> alt_item alt_items_1
                    45 => self.exit_alt_items1(),               // alt_items_1 -> "|" alt_item alt_items_1
                    46 => {}                                    // alt_items_1 -> ε
                    26 => self.exit_alt_item(),                 // alt_item -> alt_item_1
                    61 |                                        // alt_item_2 -> alt_item_1
                    62 => self.exit_alt_item1(),                // alt_item_2 -> ε
                 /* 47 */                                       // alt_item_1 -> repeat_item alt_item_2 (never called)
                    54 |                                        // repeat_item_1 -> "?"
                    56 |                                        // repeat_item_1 -> ε
                    67 |                                        // repeat_item_2 -> "?"
                    68 |                                        // repeat_item_2 -> ε
                    69 |                                        // repeat_item_3 -> "?"
                    70 => self.exit_repeat_item(alt_id),        // repeat_item_3 -> ε
                 /* 27 */                                       // repeat_item -> item repeat_item_1 (never called)
                 /* 53 */                                       // repeat_item_1 -> "+" repeat_item_2 (never called)
                 /* 55 */                                       // repeat_item_1 -> "*" repeat_item_3 (never called)
                    28 |                                        // item -> "(" alt_items ")"
                    29 |                                        // item -> "~" item
                    30 |                                        // item -> Id
                    32 |                                        // item -> StrLit
                    33 |                                        // item -> char_set
                    57 |                                        // item_1 -> ".." CharLit
                    58 => self.exit_item(alt_id),               // item_1 -> ε
                 /* 31 */                                       // item -> CharLit item_1 (never called)
                    34 |                                        // char_set -> "[" char_set_1 "]"
                    35 |                                        // char_set -> "."
                    36 => self.exit_char_set(alt_id),           // char_set -> FixedSet
                    63 |                                        // char_set_2 -> char_set_1
                    64 => self.exit_char_set1(),                // char_set_2 -> ε
                 /* 48 */                                       // char_set_1 -> char_set_one char_set_2 (never called)
                    37 |                                        // char_set_one -> FixedSet
                    59 |                                        // char_set_one_1 -> "-" SetChar
                    60 => self.exit_char_set_one(alt_id),       // char_set_one_1 -> ε
                 /* 38 */                                       // char_set_one -> SetChar char_set_one_1 (never called)
                    _ => panic!("unexpected exit alternative id: {alt_id}")
                }
            }
            Call::End(terminate) => {
                match terminate {
                    Terminate::None => {
                        let val = self.stack.pop().unwrap().get_file();
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

impl<T: LexiParserListener> Wrapper<T> {
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

    fn exit_file(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            0 => {
                let star = self.stack.pop().unwrap().get_file1();
                let header = self.stack.pop().unwrap().get_header();
                (2, CtxFile::V1 { header, star })
            }
            1 => {
                let star = self.stack.pop().unwrap().get_file1();
                (1, CtxFile::V2 { star })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_file")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_file(ctx, spans);
        self.stack.push(SynValue::File(val));
    }

    fn init_file1(&mut self) {
        let val = SynFile1(Vec::new());
        self.stack.push(SynValue::File1(val));
    }

    fn exit_file1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let file_item = self.stack.pop().unwrap().get_file_item();
        let Some(SynValue::File1(SynFile1(star_acc))) = self.stack.last_mut() else {
            panic!("expected SynFile1 item on wrapper stack");
        };
        star_acc.push(file_item);
    }

    fn exit_file_item(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            2 => {
                let option = self.stack.pop().unwrap().get_option();
                (1, CtxFileItem::V1 { option })
            }
            3 => {
                let declaration = self.stack.pop().unwrap().get_declaration();
                (1, CtxFileItem::V2 { declaration })
            }
            4 => {
                let rule = self.stack.pop().unwrap().get_rule();
                (1, CtxFileItem::V3 { rule })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_file_item")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_file_item(ctx, spans);
        self.stack.push(SynValue::FileItem(val));
    }

    fn exit_header(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let ctx = CtxHeader::V1 { id };
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_header(ctx, spans);
        self.stack.push(SynValue::Header(val));
    }

    fn exit_declaration(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let ctx = CtxDeclaration::V1 { id };
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_declaration(ctx, spans);
        self.stack.push(SynValue::Declaration(val));
    }

    fn exit_option(&mut self) {
        let star = self.stack.pop().unwrap().get_option1();
        let ctx = CtxOption::V1 { star };
        let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_option(ctx, spans);
        self.stack.push(SynValue::Option(val));
    }

    fn init_option1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let id = self.stack_t.pop().unwrap();
        self.stack.push(SynValue::Option1(SynOption1(vec![id])));
    }

    fn exit_option1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let id = self.stack_t.pop().unwrap();
        let Some(SynValue::Option1(SynOption1(star_acc))) = self.stack.last_mut() else {
            panic!("expected SynOption1 item on wrapper stack");
        };
        star_acc.push(id);
    }

    fn exit_rule(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            9 => {
                let match1 = self.stack.pop().unwrap().get_match();
                let rule_fragment_name = self.stack.pop().unwrap().get_rule_fragment_name();
                (4, CtxRule::V1 { rule_fragment_name, match1 })
            }
            49 => {
                let opt_str_lit = self.stack.pop().unwrap().get_opt_str_lit();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                (7, CtxRule::V4 { rule_terminal_name, opt_str_lit })
            }
            50 => {
                let opt_str_lit = self.stack.pop().unwrap().get_opt_str_lit();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                (5, CtxRule::V5 { rule_terminal_name, opt_str_lit })
            }
            52 => {
                let id = self.stack_t.pop().unwrap();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                (3, CtxRule::V6 { rule_terminal_name, id })
            }
            65 => {
                let actions = self.stack.pop().unwrap().get_actions();
                let match1 = self.stack.pop().unwrap().get_match();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                (6, CtxRule::V2 { rule_terminal_name, match1, actions })
            }
            66 => {
                let match1 = self.stack.pop().unwrap().get_match();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                (4, CtxRule::V3 { rule_terminal_name, match1 })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_rule")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rule(ctx, spans);
        self.stack.push(SynValue::Rule(val));
    }

    fn exit_opt_str_lit(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            11 => {
                let strlit = self.stack_t.pop().unwrap();
                (2, CtxOptStrLit::V1 { strlit })
            }
            12 => {
                (0, CtxOptStrLit::V2)
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_opt_str_lit")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_opt_str_lit(ctx, spans);
        self.stack.push(SynValue::OptStrLit(val));
    }

    fn exit_rule_fragment_name(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let ctx = CtxRuleFragmentName::V1 { id };
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rule_fragment_name(ctx, spans);
        self.stack.push(SynValue::RuleFragmentName(val));
    }

    fn exit_rule_terminal_name(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let ctx = CtxRuleTerminalName::V1 { id };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_rule_terminal_name(ctx, spans);
        self.stack.push(SynValue::RuleTerminalName(val));
    }

    fn exit_actions(&mut self) {
        let star = self.stack.pop().unwrap().get_actions1();
        let ctx = CtxActions::V1 { star };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_actions(ctx, spans);
        self.stack.push(SynValue::Actions(val));
    }

    fn init_actions1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let action = self.stack.pop().unwrap().get_action();
        self.stack.push(SynValue::Actions1(SynActions1(vec![action])));
    }

    fn exit_actions1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let action = self.stack.pop().unwrap().get_action();
        let Some(SynValue::Actions1(SynActions1(star_acc))) = self.stack.last_mut() else {
            panic!("expected SynActions1 item on wrapper stack");
        };
        star_acc.push(action);
    }

    fn exit_action(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            16 => {
                let id = self.stack_t.pop().unwrap();
                (4, CtxAction::V1 { id })
            }
            17 => {
                let id = self.stack_t.pop().unwrap();
                (4, CtxAction::V2 { id })
            }
            18 => {
                (1, CtxAction::V3)
            }
            19 => {
                (1, CtxAction::V4)
            }
            20 => {
                (1, CtxAction::V5)
            }
            21 => {
                let id = self.stack_t.pop().unwrap();
                (4, CtxAction::V6 { id })
            }
            22 => {
                let id = self.stack_t.pop().unwrap();
                (4, CtxAction::V7 { id })
            }
            23 => {
                (1, CtxAction::V8)
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_action")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_action(ctx, spans);
        self.stack.push(SynValue::Action(val));
    }

    fn exit_match(&mut self) {
        let alt_items = self.stack.pop().unwrap().get_alt_items();
        let ctx = CtxMatch::V1 { alt_items };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_match(ctx, spans);
        self.stack.push(SynValue::Match(val));
    }

    fn exit_alt_items(&mut self) {
        let star = self.stack.pop().unwrap().get_alt_items1();
        let ctx = CtxAltItems::V1 { star };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_alt_items(ctx, spans);
        self.stack.push(SynValue::AltItems(val));
    }

    fn init_alt_items1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let alt_item = self.stack.pop().unwrap().get_alt_item();
        self.stack.push(SynValue::AltItems1(SynAltItems1(vec![alt_item])));
    }

    fn exit_alt_items1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let alt_item = self.stack.pop().unwrap().get_alt_item();
        let Some(SynValue::AltItems1(SynAltItems1(star_acc))) = self.stack.last_mut() else {
            panic!("expected SynAltItems1 item on wrapper stack");
        };
        star_acc.push(alt_item);
    }

    fn exit_alt_item(&mut self) {
        let plus = self.stack.pop().unwrap().get_alt_item1();
        let ctx = CtxAltItem::V1 { plus };
        let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_alt_item(ctx, spans);
        self.stack.push(SynValue::AltItem(val));
    }

    fn init_alt_item1(&mut self) {
        let val = SynAltItem1(Vec::new());
        self.stack.push(SynValue::AltItem1(val));
    }

    fn exit_alt_item1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let repeat_item = self.stack.pop().unwrap().get_repeat_item();
        let Some(SynValue::AltItem1(SynAltItem1(plus_acc))) = self.stack.last_mut() else {
            panic!("expected SynAltItem1 item on wrapper stack");
        };
        plus_acc.push(repeat_item);
    }

    fn exit_repeat_item(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            54 => {
                let item = self.stack.pop().unwrap().get_item();
                (2, CtxRepeatItem::V5 { item })
            }
            56 => {
                let item = self.stack.pop().unwrap().get_item();
                (1, CtxRepeatItem::V6 { item })
            }
            67 => {
                let item = self.stack.pop().unwrap().get_item();
                (3, CtxRepeatItem::V3 { item })
            }
            68 => {
                let item = self.stack.pop().unwrap().get_item();
                (2, CtxRepeatItem::V4 { item })
            }
            69 => {
                let item = self.stack.pop().unwrap().get_item();
                (3, CtxRepeatItem::V1 { item })
            }
            70 => {
                let item = self.stack.pop().unwrap().get_item();
                (2, CtxRepeatItem::V2 { item })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_repeat_item")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_repeat_item(ctx, spans);
        self.stack.push(SynValue::RepeatItem(val));
    }

    fn exit_item(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            28 => {
                let alt_items = self.stack.pop().unwrap().get_alt_items();
                (3, CtxItem::V6 { alt_items })
            }
            29 => {
                let item = self.stack.pop().unwrap().get_item();
                (2, CtxItem::V7 { item })
            }
            30 => {
                let id = self.stack_t.pop().unwrap();
                (1, CtxItem::V1 { id })
            }
            32 => {
                let strlit = self.stack_t.pop().unwrap();
                (1, CtxItem::V4 { strlit })
            }
            33 => {
                let char_set = self.stack.pop().unwrap().get_char_set();
                (1, CtxItem::V5 { char_set })
            }
            57 => {
                let charlit_2 = self.stack_t.pop().unwrap();
                let charlit_1 = self.stack_t.pop().unwrap();
                (3, CtxItem::V2 { charlit: [charlit_1, charlit_2] })
            }
            58 => {
                let charlit = self.stack_t.pop().unwrap();
                (1, CtxItem::V3 { charlit })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_item")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_item(ctx, spans);
        self.stack.push(SynValue::Item(val));
    }

    fn exit_char_set(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            34 => {
                let plus = self.stack.pop().unwrap().get_char_set1();
                (3, CtxCharSet::V1 { plus })
            }
            35 => {
                (1, CtxCharSet::V2)
            }
            36 => {
                let fixedset = self.stack_t.pop().unwrap();
                (1, CtxCharSet::V3 { fixedset })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_char_set")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_char_set(ctx, spans);
        self.stack.push(SynValue::CharSet(val));
    }

    fn init_char_set1(&mut self) {
        let val = SynCharSet1(Vec::new());
        self.stack.push(SynValue::CharSet1(val));
    }

    fn exit_char_set1(&mut self) {
        let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let char_set_one = self.stack.pop().unwrap().get_char_set_one();
        let Some(SynValue::CharSet1(SynCharSet1(plus_acc))) = self.stack.last_mut() else {
            panic!("expected SynCharSet1 item on wrapper stack");
        };
        plus_acc.push(char_set_one);
    }

    fn exit_char_set_one(&mut self, alt_id: AltId) {
        let (n, ctx) = match alt_id {
            37 => {
                let fixedset = self.stack_t.pop().unwrap();
                (1, CtxCharSetOne::V3 { fixedset })
            }
            59 => {
                let setchar_2 = self.stack_t.pop().unwrap();
                let setchar_1 = self.stack_t.pop().unwrap();
                (3, CtxCharSetOne::V1 { setchar: [setchar_1, setchar_2] })
            }
            60 => {
                let setchar = self.stack_t.pop().unwrap();
                (1, CtxCharSetOne::V2 { setchar })
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_char_set_one")
        };
        let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
        self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        let val = self.listener.exit_char_set_one(ctx, spans);
        self.stack.push(SynValue::CharSetOne(val));
    }
}

// [lexiparser]
// -------------------------------------------------------------------------

pub(crate) mod lexiparser_types {
    use lexigram_lib::segments::Segments;
    use crate::lexi::listener::LexAction;

    /// SynFile: User-defined type for `file`
    #[derive(Debug, PartialEq)] pub struct SynFile();
    /// SynFileItem: User-defined type for `file_item`
    #[derive(Debug, PartialEq)] pub struct SynFileItem();
    /// SynHeader: User-defined type for `header`
    #[derive(Debug, PartialEq)] pub struct SynHeader();
    /// SynDeclaration: User-defined type for `declaration`
    #[derive(Debug, PartialEq)] pub struct SynDeclaration();
    /// SynOption: User-defined type for `option`
    #[derive(Debug, PartialEq)] pub struct SynOption();
    /// SynRule: User-defined type for `rule`
    #[derive(Debug, PartialEq)] pub struct SynRule();
    /// User-defined type for `opt_str_lit`
    #[derive(Debug, PartialEq)] pub struct SynOptStrLit(pub Option<String>);
    /// User-defined type for `rule_fragment_name`
    #[derive(Debug, PartialEq)] pub struct SynRuleFragmentName(pub String);
    /// User-defined type for `rule_terminal_name`
    #[derive(Debug, PartialEq)] pub struct SynRuleTerminalName(pub String);
    /// SynActions: User-defined type for `actions`
    #[derive(Debug, PartialEq)] pub struct SynActions(pub LexAction);
    /// SynAction: User-defined type for `action`
    #[derive(Debug, PartialEq)] pub struct SynAction(pub LexAction);
    /// SynMatch: User-defined type for `match`
    #[derive(Debug, PartialEq)] pub struct SynMatch(pub Option<String>);
    /// SynAltItems: User-defined type for `alt_items`
    #[derive(Debug, PartialEq)] pub struct SynAltItems(pub (usize, Option<String>));
    /// SynAltItem: User-defined type for `alt_item`
    #[derive(Debug, PartialEq)] pub struct SynAltItem(pub (usize, Option<String>));
    /// SynRepeatItem: User-defined type for `repeat_item`
    #[derive(Debug, PartialEq)] pub struct SynRepeatItem(pub (usize, Option<String>));
    /// SynItem: User-defined type for `item`
    #[derive(Debug, PartialEq)] pub struct SynItem(pub (usize, Option<String>));
    /// User-defined type for `char_set`
    #[derive(Debug, PartialEq)] pub struct SynCharSet(pub Segments);
    /// User-defined type for `char_set_one`
    #[derive(Debug, PartialEq)] pub struct SynCharSetOne(pub Segments);
}
