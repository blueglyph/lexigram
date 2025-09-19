// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
#![allow(unused)]

// [lexiparser]

use lexigram_lib::{CollectJoin, FixedSymTable, grammar::{AltId, Alternative, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
use lexiparser_types::*;

const PARSER_NUM_T: usize = 33;
const PARSER_NUM_NT: usize = 31;
static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Dot", Some(".")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Minus", Some("-")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("More", Some("more")), ("Skip", Some("skip")), ("Type", Some("type")), ("Channel", Some("channel")), ("Id", None), ("CharLit", None), ("StrLit", None), ("FixedSet", None), ("LSbracket", Some("[")), ("RSbracket", Some("]")), ("SetChar", None)];
static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "file_item", "header", "declaration", "option", "rule", "rule_fragment_name", "rule_terminal_name", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "char_set", "char_set_one", "file_1", "option_1", "actions_1", "alt_items_1", "alt_item_1", "char_set_1", "rule_1", "repeat_item_1", "item_1", "char_set_one_1", "alt_item_2", "char_set_2", "repeat_item_2", "repeat_item_3"];
static ALT_VAR: [VarId; 63] = [0, 0, 1, 1, 1, 2, 3, 4, 5, 5, 6, 7, 8, 9, 9, 9, 9, 9, 9, 9, 10, 11, 12, 13, 14, 14, 14, 14, 14, 14, 15, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 22, 23, 23, 24, 24, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30];
static ALTERNATIVES: [&[Symbol]; 63] = [&[Symbol::NT(2), Symbol::NT(17)], &[Symbol::NT(17)], &[Symbol::NT(4)], &[Symbol::NT(3)], &[Symbol::NT(5)], &[Symbol::T(18), Symbol::T(26), Symbol::T(14)], &[Symbol::T(19), Symbol::T(26), Symbol::T(14)], &[Symbol::T(16), Symbol::T(5), Symbol::T(26), Symbol::NT(18), Symbol::T(12)], &[Symbol::NT(6), Symbol::T(1), Symbol::NT(10), Symbol::T(14)], &[Symbol::NT(7), Symbol::T(1), Symbol::NT(10), Symbol::NT(23)], &[Symbol::T(17), Symbol::T(26)], &[Symbol::T(26)], &[Symbol::NT(9), Symbol::NT(19)], &[Symbol::T(19), Symbol::T(6), Symbol::T(26), Symbol::T(13)], &[Symbol::T(21), Symbol::T(6), Symbol::T(26), Symbol::T(13)], &[Symbol::T(20)], &[Symbol::T(23)], &[Symbol::T(22)], &[Symbol::T(24), Symbol::T(6), Symbol::T(26), Symbol::T(13)], &[Symbol::T(25), Symbol::T(6), Symbol::T(26), Symbol::T(13)], &[Symbol::NT(11)], &[Symbol::NT(12), Symbol::NT(20)], &[Symbol::NT(21)], &[Symbol::NT(14), Symbol::NT(24)], &[Symbol::T(6), Symbol::NT(11), Symbol::T(13)], &[Symbol::T(7), Symbol::NT(14)], &[Symbol::T(26)], &[Symbol::T(27), Symbol::NT(25)], &[Symbol::T(28)], &[Symbol::NT(15)], &[Symbol::T(30), Symbol::NT(22), Symbol::T(31)], &[Symbol::T(3)], &[Symbol::T(29)], &[Symbol::T(29)], &[Symbol::T(32), Symbol::NT(26)], &[Symbol::NT(1), Symbol::NT(17)], &[Symbol::Empty], &[Symbol::T(2), Symbol::T(26), Symbol::NT(18)], &[Symbol::Empty], &[Symbol::T(2), Symbol::NT(9), Symbol::NT(19)], &[Symbol::Empty], &[Symbol::T(10), Symbol::NT(12), Symbol::NT(20)], &[Symbol::Empty], &[Symbol::NT(13), Symbol::NT(27)], &[Symbol::NT(16), Symbol::NT(28)], &[Symbol::T(0), Symbol::NT(8), Symbol::T(14)], &[Symbol::T(14)], &[Symbol::T(9), Symbol::NT(29)], &[Symbol::T(11)], &[Symbol::T(15), Symbol::NT(30)], &[Symbol::Empty], &[Symbol::T(4), Symbol::T(27)], &[Symbol::Empty], &[Symbol::T(8), Symbol::T(32)], &[Symbol::Empty], &[Symbol::NT(21)], &[Symbol::Empty], &[Symbol::NT(22)], &[Symbol::Empty], &[Symbol::T(11)], &[Symbol::Empty], &[Symbol::T(11)], &[Symbol::Empty]];
static PARSING_TABLE: [AltId; 1054] = [63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 1, 1, 0, 1, 63, 63, 63, 63, 63, 63, 1, 63, 63, 63, 63, 63, 63, 1, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 2, 4, 63, 3, 63, 63, 63, 63, 63, 63, 4, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 64, 5, 64, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 64, 63, 6, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 7, 64, 63, 64, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 8, 63, 64, 63, 63, 63, 63, 63, 63, 9, 63, 63, 63, 63, 63, 63, 64, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 10, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 11, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 12, 12, 12, 12, 12, 12, 12, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 13, 15, 14, 17, 16, 18, 19, 63, 63, 63, 63, 63, 63, 63, 63, 64, 63, 63, 20, 63, 63, 20, 20, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 20, 20, 20, 20, 20, 63, 63, 63, 64, 63, 63, 21, 63, 63, 21, 21, 63, 63, 63, 63, 63, 64, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 21, 21, 21, 21, 21, 63, 63, 63, 64, 63, 63, 22, 63, 63, 22, 22, 63, 63, 64, 63, 63, 64, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 22, 22, 22, 22, 22, 63, 63, 63, 64, 63, 63, 23, 63, 63, 23, 23, 63, 63, 64, 63, 63, 64, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 23, 23, 23, 23, 23, 63, 63, 63, 64, 63, 63, 29, 63, 63, 24, 25, 63, 64, 64, 64, 63, 64, 64, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 26, 27, 28, 29, 29, 63, 63, 63, 64, 63, 63, 31, 63, 63, 64, 64, 63, 64, 64, 64, 63, 64, 64, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 64, 64, 32, 30, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 33, 63, 64, 34, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 35, 35, 63, 35, 63, 63, 63, 63, 63, 63, 35, 63, 63, 63, 63, 63, 63, 36, 63, 63, 37, 63, 63, 63, 63, 63, 63, 63, 63, 63, 38, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 39, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 40, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 42, 63, 63, 63, 63, 63, 63, 63, 63, 63, 41, 63, 63, 42, 42, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 64, 63, 63, 43, 63, 63, 43, 43, 63, 63, 64, 63, 63, 64, 64, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 43, 43, 43, 43, 43, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 44, 63, 64, 44, 63, 45, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 46, 63, 64, 64, 63, 64, 63, 63, 63, 63, 63, 63, 64, 63, 63, 63, 63, 63, 63, 64, 50, 63, 63, 50, 63, 63, 50, 50, 63, 47, 50, 48, 63, 50, 50, 49, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 50, 50, 50, 50, 50, 63, 63, 63, 52, 63, 63, 52, 51, 63, 52, 52, 63, 52, 52, 52, 63, 52, 52, 52, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 52, 52, 52, 52, 52, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 53, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 54, 63, 54, 54, 63, 56, 63, 63, 55, 63, 63, 55, 55, 63, 63, 56, 63, 63, 56, 56, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 55, 55, 55, 55, 55, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 57, 63, 58, 57, 63, 60, 63, 63, 60, 63, 63, 60, 60, 63, 63, 60, 59, 63, 60, 60, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 60, 60, 60, 60, 60, 63, 63, 63, 62, 63, 63, 62, 63, 63, 62, 62, 63, 63, 62, 61, 63, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 62, 62, 62, 62, 63, 63, 63];
static OPCODES: [&[OpCode]; 63] = [&[OpCode::Exit(0), OpCode::NT(17), OpCode::NT(2)], &[OpCode::Exit(1), OpCode::NT(17)], &[OpCode::Exit(2), OpCode::NT(4)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(14), OpCode::T(26), OpCode::T(18)], &[OpCode::Exit(6), OpCode::T(14), OpCode::T(26), OpCode::T(19)], &[OpCode::Exit(7), OpCode::T(12), OpCode::NT(18), OpCode::T(26), OpCode::T(5), OpCode::T(16)], &[OpCode::Exit(8), OpCode::T(14), OpCode::NT(10), OpCode::T(1), OpCode::NT(6)], &[OpCode::NT(23), OpCode::NT(10), OpCode::T(1), OpCode::NT(7)], &[OpCode::Exit(10), OpCode::T(26), OpCode::T(17)], &[OpCode::Exit(11), OpCode::T(26)], &[OpCode::Exit(12), OpCode::NT(19), OpCode::NT(9)], &[OpCode::Exit(13), OpCode::T(13), OpCode::T(26), OpCode::T(6), OpCode::T(19)], &[OpCode::Exit(14), OpCode::T(13), OpCode::T(26), OpCode::T(6), OpCode::T(21)], &[OpCode::Exit(15), OpCode::T(20)], &[OpCode::Exit(16), OpCode::T(23)], &[OpCode::Exit(17), OpCode::T(22)], &[OpCode::Exit(18), OpCode::T(13), OpCode::T(26), OpCode::T(6), OpCode::T(24)], &[OpCode::Exit(19), OpCode::T(13), OpCode::T(26), OpCode::T(6), OpCode::T(25)], &[OpCode::Exit(20), OpCode::NT(11)], &[OpCode::Exit(21), OpCode::NT(20), OpCode::NT(12)], &[OpCode::Exit(22), OpCode::NT(21)], &[OpCode::NT(24), OpCode::NT(14)], &[OpCode::Exit(24), OpCode::T(13), OpCode::NT(11), OpCode::T(6)], &[OpCode::Exit(25), OpCode::NT(14), OpCode::T(7)], &[OpCode::Exit(26), OpCode::T(26)], &[OpCode::NT(25), OpCode::T(27)], &[OpCode::Exit(28), OpCode::T(28)], &[OpCode::Exit(29), OpCode::NT(15)], &[OpCode::Exit(30), OpCode::T(31), OpCode::NT(22), OpCode::T(30)], &[OpCode::Exit(31), OpCode::T(3)], &[OpCode::Exit(32), OpCode::T(29)], &[OpCode::Exit(33), OpCode::T(29)], &[OpCode::NT(26), OpCode::T(32)], &[OpCode::Loop(17), OpCode::Exit(35), OpCode::NT(1)], &[OpCode::Exit(36)], &[OpCode::Loop(18), OpCode::Exit(37), OpCode::T(26), OpCode::T(2)], &[OpCode::Exit(38)], &[OpCode::Loop(19), OpCode::Exit(39), OpCode::NT(9), OpCode::T(2)], &[OpCode::Exit(40)], &[OpCode::Loop(20), OpCode::Exit(41), OpCode::NT(12), OpCode::T(10)], &[OpCode::Exit(42)], &[OpCode::NT(27), OpCode::NT(13)], &[OpCode::NT(28), OpCode::NT(16)], &[OpCode::Exit(45), OpCode::T(14), OpCode::NT(8), OpCode::T(0)], &[OpCode::Exit(46), OpCode::T(14)], &[OpCode::NT(29), OpCode::T(9)], &[OpCode::Exit(48), OpCode::T(11)], &[OpCode::NT(30), OpCode::T(15)], &[OpCode::Exit(50)], &[OpCode::Exit(51), OpCode::T(27), OpCode::T(4)], &[OpCode::Exit(52)], &[OpCode::Exit(53), OpCode::T(32), OpCode::T(8)], &[OpCode::Exit(54)], &[OpCode::Loop(21), OpCode::Exit(55)], &[OpCode::Exit(56)], &[OpCode::Loop(22), OpCode::Exit(57)], &[OpCode::Exit(58)], &[OpCode::Exit(59), OpCode::T(11)], &[OpCode::Exit(60)], &[OpCode::Exit(61), OpCode::T(11)], &[OpCode::Exit(62)]];
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
pub enum CtxFile {
    /// `file -> header file_item*`
    File1 { header: SynHeader, star: SynFile1 },
    /// `file -> file_item*`
    File2 { star: SynFile1 },
}
#[derive(Debug)]
pub enum CtxFileItem {
    /// `file_item -> option`
    FileItem1 { option: SynOption },
    /// `file_item -> declaration`
    FileItem2 { declaration: SynDeclaration },
    /// `file_item -> rule`
    FileItem3 { rule: SynRule },
}
#[derive(Debug)]
pub enum CtxHeader {
    /// `header -> "lexicon" Id ";"`
    Header { id: String },
}
#[derive(Debug)]
pub enum CtxDeclaration {
    /// `declaration -> "mode" Id ";"`
    Declaration { id: String },
}
#[derive(Debug)]
pub enum CtxOption {
    /// `option -> "channels" "{" Id ("," Id)* "}"`
    Option { id: String, star: SynOption1 },
}
#[derive(Debug)]
pub enum CtxRule {
    /// `rule -> rule_fragment_name ":" match ";"`
    Rule1 { rule_fragment_name: SynRuleFragmentName, match1: SynMatch },
    /// `rule -> rule_terminal_name ":" match "->" actions ";"`
    Rule2 { rule_terminal_name: SynRuleTerminalName, match1: SynMatch, actions: SynActions },
    /// `rule -> rule_terminal_name ":" match ";"`
    Rule3 { rule_terminal_name: SynRuleTerminalName, match1: SynMatch },
}
#[derive(Debug)]
pub enum CtxRuleFragmentName {
    /// `rule_fragment_name -> "fragment" Id`
    RuleFragmentName { id: String },
}
#[derive(Debug)]
pub enum CtxRuleTerminalName {
    /// `rule_terminal_name -> Id`
    RuleTerminalName { id: String },
}
#[derive(Debug)]
pub enum CtxActions {
    /// `actions -> action ("," action)*`
    Actions { action: SynAction, star: SynActions1 },
}
#[derive(Debug)]
pub enum CtxAction {
    /// `action -> "mode" "(" Id ")"`
    Action1 { id: String },
    /// `action -> "push" "(" Id ")"`
    Action2 { id: String },
    /// `action -> "pop"`
    Action3,
    /// `action -> "skip"`
    Action4,
    /// `action -> "more"`
    Action5,
    /// `action -> "type" "(" Id ")"`
    Action6 { id: String },
    /// `action -> "channel" "(" Id ")"`
    Action7 { id: String },
}
#[derive(Debug)]
pub enum CtxMatch {
    /// `match -> alt_items`
    Match { alt_items: SynAltItems },
}
#[derive(Debug)]
pub enum CtxAltItems {
    /// `alt_items -> alt_item ("|" alt_item)*`
    AltItems { alt_item: SynAltItem, star: SynAltItems1 },
}
#[derive(Debug)]
pub enum CtxAltItem {
    /// `alt_item -> repeat_item+`
    AltItem { plus: SynAltItem1 },
}
#[derive(Debug)]
pub enum CtxRepeatItem {
    /// `repeat_item -> item "?"`
    RepeatItem1 { item: SynItem },
    /// `repeat_item -> item`
    RepeatItem2 { item: SynItem },
    /// `repeat_item -> item "+" "?"`
    RepeatItem3 { item: SynItem },
    /// `repeat_item -> item "+"`
    RepeatItem4 { item: SynItem },
    /// `repeat_item -> item "*" "?"`
    RepeatItem5 { item: SynItem },
    /// `repeat_item -> item "*"`
    RepeatItem6 { item: SynItem },
}
#[derive(Debug)]
pub enum CtxItem {
    /// `item -> "(" alt_items ")"`
    Item1 { alt_items: SynAltItems },
    /// `item -> "~" item`
    Item2 { item: SynItem },
    /// `item -> Id`
    Item3 { id: String },
    /// `item -> StrLit`
    Item4 { strlit: String },
    /// `item -> char_set`
    Item5 { char_set: SynCharSet },
    /// `item -> CharLit ".." CharLit`
    Item6 { charlit: [String; 2] },
    /// `item -> CharLit`
    Item7 { charlit: String },
}
#[derive(Debug)]
pub enum CtxCharSet {
    /// `char_set -> "[" char_set_one+ "]"`
    CharSet1 { plus: SynCharSet1 },
    /// `char_set -> "."`
    CharSet2,
    /// `char_set -> FixedSet`
    CharSet3 { fixedset: String },
}
#[derive(Debug)]
pub enum CtxCharSetOne {
    /// `char_set_one -> FixedSet`
    CharSetOne1 { fixedset: String },
    /// `char_set_one -> SetChar "-" SetChar`
    CharSetOne2 { setchar: [String; 2] },
    /// `char_set_one -> SetChar`
    CharSetOne3 { setchar: String },
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
enum SynValue { File(SynFile), FileItem(SynFileItem), Header(SynHeader), Declaration(SynDeclaration), Option(SynOption), Rule(SynRule), RuleFragmentName(SynRuleFragmentName), RuleTerminalName(SynRuleTerminalName), Actions(SynActions), Action(SynAction), Match(SynMatch), AltItems(SynAltItems), AltItem(SynAltItem), RepeatItem(SynRepeatItem), Item(SynItem), CharSet(SynCharSet), CharSetOne(SynCharSetOne), File1(SynFile1), Option1(SynOption1), Actions1(SynActions1), AltItems1(SynAltItems1), AltItem1(SynAltItem1), CharSet1(SynCharSet1) }

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
    fn check_abort_request(&self) -> bool { false }
    fn get_mut_log(&mut self) -> &mut impl Logger;
    fn exit(&mut self, _file: SynFile) {}
    fn init_file(&mut self) {}
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile;
    fn init_file_item(&mut self) {}
    fn exit_file_item(&mut self, _ctx: CtxFileItem) -> SynFileItem;
    fn init_header(&mut self) {}
    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader;
    fn init_declaration(&mut self) {}
    fn exit_declaration(&mut self, _ctx: CtxDeclaration) -> SynDeclaration;
    fn init_option(&mut self) {}
    fn exit_option(&mut self, _ctx: CtxOption) -> SynOption;
    fn init_rule(&mut self) {}
    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule;
    fn init_rule_fragment_name(&mut self) {}
    fn exit_rule_fragment_name(&mut self, _ctx: CtxRuleFragmentName) -> SynRuleFragmentName;
    fn init_rule_terminal_name(&mut self) {}
    fn exit_rule_terminal_name(&mut self, _ctx: CtxRuleTerminalName) -> SynRuleTerminalName;
    fn init_actions(&mut self) {}
    fn exit_actions(&mut self, _ctx: CtxActions) -> SynActions;
    fn init_action(&mut self) {}
    fn exit_action(&mut self, _ctx: CtxAction) -> SynAction;
    fn init_match(&mut self) {}
    fn exit_match(&mut self, _ctx: CtxMatch) -> SynMatch;
    fn init_alt_items(&mut self) {}
    fn exit_alt_items(&mut self, _ctx: CtxAltItems) -> SynAltItems;
    fn init_alt_item(&mut self) {}
    fn exit_alt_item(&mut self, _ctx: CtxAltItem) -> SynAltItem;
    fn init_repeat_item(&mut self) {}
    fn exit_repeat_item(&mut self, _ctx: CtxRepeatItem) -> SynRepeatItem;
    fn init_item(&mut self) {}
    fn exit_item(&mut self, _ctx: CtxItem) -> SynItem;
    fn init_char_set(&mut self) {}
    fn exit_char_set(&mut self, _ctx: CtxCharSet) -> SynCharSet;
    fn init_char_set_one(&mut self) {}
    fn exit_char_set_one(&mut self, _ctx: CtxCharSetOne) -> SynCharSetOne;
}

pub struct Wrapper<T> {
    verbose: bool,
    listener: T,
    stack: Vec<SynValue>,
    max_stack: usize,
    stack_t: Vec<String>,
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
                match nt {
                    0 => self.listener.init_file(),               // file
                    17 => self.init_file1(),                      // file_1
                    1 => self.listener.init_file_item(),          // file_item
                    2 => self.listener.init_header(),             // header
                    3 => self.listener.init_declaration(),        // declaration
                    4 => self.listener.init_option(),             // option
                    18 => self.init_option1(),                    // option_1
                    5 => self.listener.init_rule(),               // rule
                    23 => {}                                      // rule_1
                    6 => self.listener.init_rule_fragment_name(), // rule_fragment_name
                    7 => self.listener.init_rule_terminal_name(), // rule_terminal_name
                    8 => self.listener.init_actions(),            // actions
                    19 => self.init_actions1(),                   // actions_1
                    9 => self.listener.init_action(),             // action
                    10 => self.listener.init_match(),             // match
                    11 => self.listener.init_alt_items(),         // alt_items
                    20 => self.init_alt_items1(),                 // alt_items_1
                    12 => self.listener.init_alt_item(),          // alt_item
                    21 => self.init_alt_item1(),                  // alt_item_1
                    27 => {}                                      // alt_item_2
                    13 => self.listener.init_repeat_item(),       // repeat_item
                    24 => {}                                      // repeat_item_1
                    29 | 30 => {}                                 // repeat_item_2, repeat_item_3
                    14 => self.listener.init_item(),              // item
                    25 => {}                                      // item_1
                    15 => self.listener.init_char_set(),          // char_set
                    22 => self.init_char_set1(),                  // char_set_1
                    28 => {}                                      // char_set_2
                    16 => self.listener.init_char_set_one(),      // char_set_one
                    26 => {}                                      // char_set_one_1
                    _ => panic!("unexpected enter nonterminal id: {nt}")
                }
            }
            Call::Loop => {}
            Call::Exit => {
                match alt_id {
                    0 |                                         // file -> header file_1
                    1 => self.exit_file(alt_id),                // file -> file_1
                    35 => self.exit_file1(),                    // file_1 -> file_item file_1
                    36 => {}                                    // file_1 -> ε
                    2 |                                         // file_item -> option
                    3 |                                         // file_item -> declaration
                    4 => self.exit_file_item(alt_id),           // file_item -> rule
                    5 => self.exit_header(),                    // header -> "lexicon" Id ";"
                    6 => self.exit_declaration(),               // declaration -> "mode" Id ";"
                    7 => self.exit_option(),                    // option -> "channels" "{" Id option_1 "}"
                    37 => self.exit_option1(),                  // option_1 -> "," Id option_1
                    38 => {}                                    // option_1 -> ε
                    8 |                                         // rule -> rule_fragment_name ":" match ";"
                    45 |                                        // rule_1 -> "->" actions ";"
                    46 => self.exit_rule(alt_id),               // rule_1 -> ";"
                 /* 9 */                                        // rule -> rule_terminal_name ":" match rule_1 (never called)
                    10 => self.exit_rule_fragment_name(),       // rule_fragment_name -> "fragment" Id
                    11 => self.exit_rule_terminal_name(),       // rule_terminal_name -> Id
                    12 => self.exit_actions(),                  // actions -> action actions_1
                    39 => self.exit_actions1(),                 // actions_1 -> "," action actions_1
                    40 => {}                                    // actions_1 -> ε
                    13 |                                        // action -> "mode" "(" Id ")"
                    14 |                                        // action -> "push" "(" Id ")"
                    15 |                                        // action -> "pop"
                    16 |                                        // action -> "skip"
                    17 |                                        // action -> "more"
                    18 |                                        // action -> "type" "(" Id ")"
                    19 => self.exit_action(alt_id),             // action -> "channel" "(" Id ")"
                    20 => self.exit_match(),                    // match -> alt_items
                    21 => self.exit_alt_items(),                // alt_items -> alt_item alt_items_1
                    41 => self.exit_alt_items1(),               // alt_items_1 -> "|" alt_item alt_items_1
                    42 => {}                                    // alt_items_1 -> ε
                    22 => self.exit_alt_item(),                 // alt_item -> alt_item_1
                    55 |                                        // alt_item_2 -> alt_item_1
                    56 => self.exit_alt_item1(),                // alt_item_2 -> ε
                 /* 43 */                                       // alt_item_1 -> repeat_item alt_item_2 (never called)
                    48 |                                        // repeat_item_1 -> "?"
                    50 |                                        // repeat_item_1 -> ε
                    59 |                                        // repeat_item_2 -> "?"
                    60 |                                        // repeat_item_2 -> ε
                    61 |                                        // repeat_item_3 -> "?"
                    62 => self.exit_repeat_item(alt_id),        // repeat_item_3 -> ε
                 /* 23 */                                       // repeat_item -> item repeat_item_1 (never called)
                 /* 47 */                                       // repeat_item_1 -> "+" repeat_item_2 (never called)
                 /* 49 */                                       // repeat_item_1 -> "*" repeat_item_3 (never called)
                    24 |                                        // item -> "(" alt_items ")"
                    25 |                                        // item -> "~" item
                    26 |                                        // item -> Id
                    28 |                                        // item -> StrLit
                    29 |                                        // item -> char_set
                    51 |                                        // item_1 -> ".." CharLit
                    52 => self.exit_item(alt_id),               // item_1 -> ε
                 /* 27 */                                       // item -> CharLit item_1 (never called)
                    30 |                                        // char_set -> "[" char_set_1 "]"
                    31 |                                        // char_set -> "."
                    32 => self.exit_char_set(alt_id),           // char_set -> FixedSet
                    57 |                                        // char_set_2 -> char_set_1
                    58 => self.exit_char_set1(),                // char_set_2 -> ε
                 /* 44 */                                       // char_set_1 -> char_set_one char_set_2 (never called)
                    33 |                                        // char_set_one -> FixedSet
                    53 |                                        // char_set_one_1 -> "-" SetChar
                    54 => self.exit_char_set_one(alt_id),       // char_set_one_1 -> ε
                 /* 34 */                                       // char_set_one -> SetChar char_set_one_1 (never called)
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

impl<T: LexiParserListener> Wrapper<T> {
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
        let file = self.stack.pop().unwrap().get_file();
        self.listener.exit(file);
    }

    fn exit_file(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            0 => {
                let star = self.stack.pop().unwrap().get_file1();
                let header = self.stack.pop().unwrap().get_header();
                CtxFile::File1 { header, star }
            }
            1 => {
                let star = self.stack.pop().unwrap().get_file1();
                CtxFile::File2 { star }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_file")
        };
        let val = self.listener.exit_file(ctx);
        self.stack.push(SynValue::File(val));
    }

    fn init_file1(&mut self) {
        let val = SynFile1(Vec::new());
        self.stack.push(SynValue::File1(val));
    }

    fn exit_file1(&mut self) {
        let file_item = self.stack.pop().unwrap().get_file_item();
        let mut star_it = self.stack.pop().unwrap().get_file1();
        star_it.0.push(file_item);
        self.stack.push(SynValue::File1(star_it));
    }

    fn exit_file_item(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            2 => {
                let option = self.stack.pop().unwrap().get_option();
                CtxFileItem::FileItem1 { option }
            }
            3 => {
                let declaration = self.stack.pop().unwrap().get_declaration();
                CtxFileItem::FileItem2 { declaration }
            }
            4 => {
                let rule = self.stack.pop().unwrap().get_rule();
                CtxFileItem::FileItem3 { rule }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_file_item")
        };
        let val = self.listener.exit_file_item(ctx);
        self.stack.push(SynValue::FileItem(val));
    }

    fn exit_header(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_header(CtxHeader::Header { id });
        self.stack.push(SynValue::Header(val));
    }

    fn exit_declaration(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_declaration(CtxDeclaration::Declaration { id });
        self.stack.push(SynValue::Declaration(val));
    }

    fn exit_option(&mut self) {
        let star = self.stack.pop().unwrap().get_option1();
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_option(CtxOption::Option { id, star });
        self.stack.push(SynValue::Option(val));
    }

    fn init_option1(&mut self) {
        let val = SynOption1(Vec::new());
        self.stack.push(SynValue::Option1(val));
    }

    fn exit_option1(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let mut star_it = self.stack.pop().unwrap().get_option1();
        star_it.0.push(id);
        self.stack.push(SynValue::Option1(star_it));
    }

    fn exit_rule(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            8 => {
                let match1 = self.stack.pop().unwrap().get_match();
                let rule_fragment_name = self.stack.pop().unwrap().get_rule_fragment_name();
                CtxRule::Rule1 { rule_fragment_name, match1 }
            }
            45 => {
                let actions = self.stack.pop().unwrap().get_actions();
                let match1 = self.stack.pop().unwrap().get_match();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                CtxRule::Rule2 { rule_terminal_name, match1, actions }
            }
            46 => {
                let match1 = self.stack.pop().unwrap().get_match();
                let rule_terminal_name = self.stack.pop().unwrap().get_rule_terminal_name();
                CtxRule::Rule3 { rule_terminal_name, match1 }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_rule")
        };
        let val = self.listener.exit_rule(ctx);
        self.stack.push(SynValue::Rule(val));
    }

    fn exit_rule_fragment_name(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_rule_fragment_name(CtxRuleFragmentName::RuleFragmentName { id });
        self.stack.push(SynValue::RuleFragmentName(val));
    }

    fn exit_rule_terminal_name(&mut self) {
        let id = self.stack_t.pop().unwrap();
        let val = self.listener.exit_rule_terminal_name(CtxRuleTerminalName::RuleTerminalName { id });
        self.stack.push(SynValue::RuleTerminalName(val));
    }

    fn exit_actions(&mut self) {
        let star = self.stack.pop().unwrap().get_actions1();
        let action = self.stack.pop().unwrap().get_action();
        let val = self.listener.exit_actions(CtxActions::Actions { action, star });
        self.stack.push(SynValue::Actions(val));
    }

    fn init_actions1(&mut self) {
        let val = SynActions1(Vec::new());
        self.stack.push(SynValue::Actions1(val));
    }

    fn exit_actions1(&mut self) {
        let action = self.stack.pop().unwrap().get_action();
        let mut star_it = self.stack.pop().unwrap().get_actions1();
        star_it.0.push(action);
        self.stack.push(SynValue::Actions1(star_it));
    }

    fn exit_action(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            13 => {
                let id = self.stack_t.pop().unwrap();
                CtxAction::Action1 { id }
            }
            14 => {
                let id = self.stack_t.pop().unwrap();
                CtxAction::Action2 { id }
            }
            15 => {
                CtxAction::Action3
            }
            16 => {
                CtxAction::Action4
            }
            17 => {
                CtxAction::Action5
            }
            18 => {
                let id = self.stack_t.pop().unwrap();
                CtxAction::Action6 { id }
            }
            19 => {
                let id = self.stack_t.pop().unwrap();
                CtxAction::Action7 { id }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_action")
        };
        let val = self.listener.exit_action(ctx);
        self.stack.push(SynValue::Action(val));
    }

    fn exit_match(&mut self) {
        let alt_items = self.stack.pop().unwrap().get_alt_items();
        let val = self.listener.exit_match(CtxMatch::Match { alt_items });
        self.stack.push(SynValue::Match(val));
    }

    fn exit_alt_items(&mut self) {
        let star = self.stack.pop().unwrap().get_alt_items1();
        let alt_item = self.stack.pop().unwrap().get_alt_item();
        let val = self.listener.exit_alt_items(CtxAltItems::AltItems { alt_item, star });
        self.stack.push(SynValue::AltItems(val));
    }

    fn init_alt_items1(&mut self) {
        let val = SynAltItems1(Vec::new());
        self.stack.push(SynValue::AltItems1(val));
    }

    fn exit_alt_items1(&mut self) {
        let alt_item = self.stack.pop().unwrap().get_alt_item();
        let mut star_it = self.stack.pop().unwrap().get_alt_items1();
        star_it.0.push(alt_item);
        self.stack.push(SynValue::AltItems1(star_it));
    }

    fn exit_alt_item(&mut self) {
        let plus = self.stack.pop().unwrap().get_alt_item1();
        let val = self.listener.exit_alt_item(CtxAltItem::AltItem { plus });
        self.stack.push(SynValue::AltItem(val));
    }

    fn init_alt_item1(&mut self) {
        let val = SynAltItem1(Vec::new());
        self.stack.push(SynValue::AltItem1(val));
    }

    fn exit_alt_item1(&mut self) {
        let repeat_item = self.stack.pop().unwrap().get_repeat_item();
        let mut plus_it = self.stack.pop().unwrap().get_alt_item1();
        plus_it.0.push(repeat_item);
        self.stack.push(SynValue::AltItem1(plus_it));
    }

    fn exit_repeat_item(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            48 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxRepeatItem::RepeatItem1 { item }
            }
            50 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxRepeatItem::RepeatItem2 { item }
            }
            59 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxRepeatItem::RepeatItem3 { item }
            }
            60 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxRepeatItem::RepeatItem4 { item }
            }
            61 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxRepeatItem::RepeatItem5 { item }
            }
            62 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxRepeatItem::RepeatItem6 { item }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_repeat_item")
        };
        let val = self.listener.exit_repeat_item(ctx);
        self.stack.push(SynValue::RepeatItem(val));
    }

    fn exit_item(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            24 => {
                let alt_items = self.stack.pop().unwrap().get_alt_items();
                CtxItem::Item1 { alt_items }
            }
            25 => {
                let item = self.stack.pop().unwrap().get_item();
                CtxItem::Item2 { item }
            }
            26 => {
                let id = self.stack_t.pop().unwrap();
                CtxItem::Item3 { id }
            }
            28 => {
                let strlit = self.stack_t.pop().unwrap();
                CtxItem::Item4 { strlit }
            }
            29 => {
                let char_set = self.stack.pop().unwrap().get_char_set();
                CtxItem::Item5 { char_set }
            }
            51 => {
                let charlit_2 = self.stack_t.pop().unwrap();
                let charlit_1 = self.stack_t.pop().unwrap();
                CtxItem::Item6 { charlit: [charlit_1, charlit_2] }
            }
            52 => {
                let charlit = self.stack_t.pop().unwrap();
                CtxItem::Item7 { charlit }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_item")
        };
        let val = self.listener.exit_item(ctx);
        self.stack.push(SynValue::Item(val));
    }

    fn exit_char_set(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            30 => {
                let plus = self.stack.pop().unwrap().get_char_set1();
                CtxCharSet::CharSet1 { plus }
            }
            31 => {
                CtxCharSet::CharSet2
            }
            32 => {
                let fixedset = self.stack_t.pop().unwrap();
                CtxCharSet::CharSet3 { fixedset }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_char_set")
        };
        let val = self.listener.exit_char_set(ctx);
        self.stack.push(SynValue::CharSet(val));
    }

    fn init_char_set1(&mut self) {
        let val = SynCharSet1(Vec::new());
        self.stack.push(SynValue::CharSet1(val));
    }

    fn exit_char_set1(&mut self) {
        let char_set_one = self.stack.pop().unwrap().get_char_set_one();
        let mut plus_it = self.stack.pop().unwrap().get_char_set1();
        plus_it.0.push(char_set_one);
        self.stack.push(SynValue::CharSet1(plus_it));
    }

    fn exit_char_set_one(&mut self, alt_id: AltId) {
        let ctx = match alt_id {
            33 => {
                let fixedset = self.stack_t.pop().unwrap();
                CtxCharSetOne::CharSetOne1 { fixedset }
            }
            53 => {
                let setchar_2 = self.stack_t.pop().unwrap();
                let setchar_1 = self.stack_t.pop().unwrap();
                CtxCharSetOne::CharSetOne2 { setchar: [setchar_1, setchar_2] }
            }
            54 => {
                let setchar = self.stack_t.pop().unwrap();
                CtxCharSetOne::CharSetOne3 { setchar }
            }
            _ => panic!("unexpected alt id {alt_id} in fn exit_char_set_one")
        };
        let val = self.listener.exit_char_set_one(ctx);
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
