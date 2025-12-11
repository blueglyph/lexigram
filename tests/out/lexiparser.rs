// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.


pub(crate) mod lexiparser {
    #![allow(unused)]
    // -------------------------------------------------------------------------
    // [lexiparser]

    use lexigram_lib::{AltId, FixedSymTable, VarId, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::lexiparser_types::*;

    const PARSER_NUM_T: usize = 34;
    const PARSER_NUM_NT: usize = 29;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Dot", Some(".")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Minus", Some("-")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("More", Some("more")), ("Skip", Some("skip")), ("Type", Some("type")), ("Channel", Some("channel")), ("SymEof", Some("EOF")), ("Id", None), ("CharLit", None), ("StrLit", None), ("FixedSet", None), ("LSbracket", Some("[")), ("RSbracket", Some("]")), ("SetChar", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "file_item", "header", "declaration", "option", "rule", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "char_set", "char_set_one", "file_1", "option_1", "actions_1", "alt_items_1", "alt_item_1", "char_set_1", "rule_1", "repeat_item_1", "item_1", "char_set_one_1", "alt_item_2", "char_set_2", "repeat_item_2", "repeat_item_3"];
    static ALT_VAR: [VarId; 61] = [0, 0, 1, 1, 1, 2, 3, 4, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 8, 9, 10, 11, 12, 12, 12, 12, 12, 12, 13, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 20, 21, 21, 22, 22, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28];
    static PARSING_TABLE: [AltId; 1015] = [61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 1, 1, 0, 1, 61, 61, 61, 61, 61, 61, 61, 1, 61, 61, 61, 61, 61, 61, 1, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 2, 4, 61, 3, 61, 61, 61, 61, 61, 61, 61, 4, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 62, 5, 62, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 62, 61, 6, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 7, 62, 61, 62, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 8, 61, 62, 61, 61, 61, 61, 61, 61, 61, 9, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 10, 10, 10, 10, 10, 10, 10, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 11, 13, 12, 15, 14, 16, 17, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 18, 61, 61, 18, 18, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 18, 18, 18, 18, 18, 61, 61, 61, 62, 61, 61, 19, 61, 61, 19, 19, 61, 61, 61, 61, 61, 62, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 19, 19, 19, 19, 19, 61, 61, 61, 62, 61, 61, 20, 61, 61, 20, 20, 61, 61, 62, 61, 61, 62, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 20, 20, 20, 20, 20, 61, 61, 61, 62, 61, 61, 21, 61, 61, 21, 21, 61, 61, 62, 61, 61, 62, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 21, 21, 21, 21, 21, 61, 61, 61, 62, 61, 61, 27, 61, 61, 22, 23, 61, 62, 62, 62, 61, 62, 62, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 24, 25, 26, 27, 27, 61, 61, 61, 62, 61, 61, 29, 61, 61, 62, 62, 61, 62, 62, 62, 61, 62, 62, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 62, 62, 30, 28, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 31, 61, 62, 32, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 33, 33, 61, 33, 61, 61, 61, 61, 61, 61, 61, 33, 61, 61, 61, 61, 61, 61, 34, 61, 61, 35, 61, 61, 61, 61, 61, 61, 61, 61, 61, 36, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 37, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 38, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 40, 61, 61, 61, 61, 61, 61, 61, 61, 61, 39, 61, 61, 40, 40, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 41, 61, 61, 41, 41, 61, 61, 62, 61, 61, 62, 62, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 41, 41, 41, 41, 41, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 42, 61, 62, 42, 61, 43, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 44, 61, 62, 62, 61, 62, 61, 61, 61, 61, 61, 61, 61, 62, 61, 61, 61, 61, 61, 61, 62, 48, 61, 61, 48, 61, 61, 48, 48, 61, 45, 48, 46, 61, 48, 48, 47, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 48, 48, 48, 48, 48, 61, 61, 61, 50, 61, 61, 50, 49, 61, 50, 50, 61, 50, 50, 50, 61, 50, 50, 50, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 50, 50, 50, 50, 50, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 51, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 52, 61, 52, 52, 61, 54, 61, 61, 53, 61, 61, 53, 53, 61, 61, 54, 61, 61, 54, 54, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 53, 53, 53, 53, 53, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 55, 61, 56, 55, 61, 58, 61, 61, 58, 61, 61, 58, 58, 61, 61, 58, 57, 61, 58, 58, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 58, 58, 58, 58, 58, 61, 61, 61, 60, 61, 61, 60, 61, 61, 60, 60, 61, 61, 60, 59, 61, 60, 60, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 60, 60, 60, 60, 60, 61, 61, 61];
    static OPCODES: [&[OpCode]; 61] = [&[OpCode::Exit(0), OpCode::NT(15), OpCode::NT(2)], &[OpCode::Exit(1), OpCode::NT(15)], &[OpCode::Exit(2), OpCode::NT(4)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(14), OpCode::T(27), OpCode::T(18)], &[OpCode::Exit(6), OpCode::T(14), OpCode::T(27), OpCode::T(19)], &[OpCode::Exit(7), OpCode::T(12), OpCode::NT(16), OpCode::T(27), OpCode::T(5), OpCode::T(16)], &[OpCode::Exit(8), OpCode::T(14), OpCode::NT(8), OpCode::T(1), OpCode::T(27), OpCode::T(17)], &[OpCode::NT(21), OpCode::NT(8), OpCode::T(1), OpCode::T(27)], &[OpCode::Exit(10), OpCode::NT(17), OpCode::NT(7)], &[OpCode::Exit(11), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(19)], &[OpCode::Exit(12), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(21)], &[OpCode::Exit(13), OpCode::T(20)], &[OpCode::Exit(14), OpCode::T(23)], &[OpCode::Exit(15), OpCode::T(22)], &[OpCode::Exit(16), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(24)], &[OpCode::Exit(17), OpCode::T(13), OpCode::T(27), OpCode::T(6), OpCode::T(25)], &[OpCode::Exit(18), OpCode::NT(9)], &[OpCode::Exit(19), OpCode::NT(18), OpCode::NT(10)], &[OpCode::Exit(20), OpCode::NT(19)], &[OpCode::NT(22), OpCode::NT(12)], &[OpCode::Exit(22), OpCode::T(13), OpCode::NT(9), OpCode::T(6)], &[OpCode::Exit(23), OpCode::NT(12), OpCode::T(7)], &[OpCode::Exit(24), OpCode::T(27)], &[OpCode::NT(23), OpCode::T(28)], &[OpCode::Exit(26), OpCode::T(29)], &[OpCode::Exit(27), OpCode::NT(13)], &[OpCode::Exit(28), OpCode::T(32), OpCode::NT(20), OpCode::T(31)], &[OpCode::Exit(29), OpCode::T(3)], &[OpCode::Exit(30), OpCode::T(30)], &[OpCode::Exit(31), OpCode::T(30)], &[OpCode::NT(24), OpCode::T(33)], &[OpCode::Loop(15), OpCode::Exit(33), OpCode::NT(1)], &[OpCode::Exit(34)], &[OpCode::Loop(16), OpCode::Exit(35), OpCode::T(27), OpCode::T(2)], &[OpCode::Exit(36)], &[OpCode::Loop(17), OpCode::Exit(37), OpCode::NT(7), OpCode::T(2)], &[OpCode::Exit(38)], &[OpCode::Loop(18), OpCode::Exit(39), OpCode::NT(10), OpCode::T(10)], &[OpCode::Exit(40)], &[OpCode::NT(25), OpCode::NT(11)], &[OpCode::NT(26), OpCode::NT(14)], &[OpCode::Exit(43), OpCode::T(14), OpCode::NT(6), OpCode::T(0)], &[OpCode::Exit(44), OpCode::T(14)], &[OpCode::NT(27), OpCode::T(9)], &[OpCode::Exit(46), OpCode::T(11)], &[OpCode::NT(28), OpCode::T(15)], &[OpCode::Exit(48)], &[OpCode::Exit(49), OpCode::T(28), OpCode::T(4)], &[OpCode::Exit(50)], &[OpCode::Exit(51), OpCode::T(33), OpCode::T(8)], &[OpCode::Exit(52)], &[OpCode::Loop(19), OpCode::Exit(53)], &[OpCode::Exit(54)], &[OpCode::Loop(20), OpCode::Exit(55)], &[OpCode::Exit(56)], &[OpCode::Exit(57), OpCode::T(11)], &[OpCode::Exit(58)], &[OpCode::Exit(59), OpCode::T(11)], &[OpCode::Exit(60)]];
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
        V1 { id: String, star: SynOption1 },
    }
    #[derive(Debug)]
    pub enum CtxRule {
        /// `rule -> "fragment" Id ":" match ";"`
        V1 { id: String, match1: SynMatch },
        /// `rule -> Id ":" match "->" actions ";"`
        V2 { id: String, match1: SynMatch, actions: SynActions },
        /// `rule -> Id ":" match ";"`
        V3 { id: String, match1: SynMatch },
    }
    #[derive(Debug)]
    pub enum CtxActions {
        /// `actions -> action ("," action)*`
        V1 { action: SynAction, star: SynActions1 },
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
    }
    #[derive(Debug)]
    pub enum CtxMatch {
        /// `match -> alt_items`
        V1 { alt_items: SynAltItems },
    }
    #[derive(Debug)]
    pub enum CtxAltItems {
        /// `alt_items -> alt_item ("|" alt_item)*`
        V1 { alt_item: SynAltItem, star: SynAltItems1 },
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
    enum SynValue { File(SynFile), FileItem(SynFileItem), Header(SynHeader), Declaration(SynDeclaration), Option(SynOption), Rule(SynRule), Actions(SynActions), Action(SynAction), Match(SynMatch), AltItems(SynAltItems), AltItem(SynAltItem), RepeatItem(SynRepeatItem), Item(SynItem), CharSet(SynCharSet), CharSetOne(SynCharSetOne), File1(SynFile1), Option1(SynOption1), Actions1(SynActions1), AltItems1(SynAltItems1), AltItem1(SynAltItem1), CharSet1(SynCharSet1) }

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
        #[allow(unused)]
        fn exit(&mut self, file: SynFile) {}
        fn init_file(&mut self) {}
        fn exit_file(&mut self, ctx: CtxFile) -> SynFile;
        fn init_file_item(&mut self) {}
        fn exit_file_item(&mut self, ctx: CtxFileItem) -> SynFileItem;
        fn init_header(&mut self) {}
        fn exit_header(&mut self, ctx: CtxHeader) -> SynHeader;
        fn init_declaration(&mut self) {}
        fn exit_declaration(&mut self, ctx: CtxDeclaration) -> SynDeclaration;
        fn init_option(&mut self) {}
        fn exit_option(&mut self, ctx: CtxOption) -> SynOption;
        fn init_rule(&mut self) {}
        fn exit_rule(&mut self, ctx: CtxRule) -> SynRule;
        fn init_actions(&mut self) {}
        fn exit_actions(&mut self, ctx: CtxActions) -> SynActions;
        fn init_action(&mut self) {}
        fn exit_action(&mut self, ctx: CtxAction) -> SynAction;
        fn init_match(&mut self) {}
        fn exit_match(&mut self, ctx: CtxMatch) -> SynMatch;
        fn init_alt_items(&mut self) {}
        fn exit_alt_items(&mut self, ctx: CtxAltItems) -> SynAltItems;
        fn init_alt_item(&mut self) {}
        fn exit_alt_item(&mut self, ctx: CtxAltItem) -> SynAltItem;
        fn init_repeat_item(&mut self) {}
        fn exit_repeat_item(&mut self, ctx: CtxRepeatItem) -> SynRepeatItem;
        fn init_item(&mut self) {}
        fn exit_item(&mut self, ctx: CtxItem) -> SynItem;
        fn init_char_set(&mut self) {}
        fn exit_char_set(&mut self, ctx: CtxCharSet) -> SynCharSet;
        fn init_char_set_one(&mut self) {}
        fn exit_char_set_one(&mut self, ctx: CtxCharSetOne) -> SynCharSetOne;
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
                        0 => self.listener.init_file(),             // file
                        15 => self.init_file1(),                    // file_1
                        1 => self.listener.init_file_item(),        // file_item
                        2 => self.listener.init_header(),           // header
                        3 => self.listener.init_declaration(),      // declaration
                        4 => self.listener.init_option(),           // option
                        16 => self.init_option1(),                  // option_1
                        5 => self.listener.init_rule(),             // rule
                        21 => {}                                    // rule_1
                        6 => self.listener.init_actions(),          // actions
                        17 => self.init_actions1(),                 // actions_1
                        7 => self.listener.init_action(),           // action
                        8 => self.listener.init_match(),            // match
                        9 => self.listener.init_alt_items(),        // alt_items
                        18 => self.init_alt_items1(),               // alt_items_1
                        10 => self.listener.init_alt_item(),        // alt_item
                        19 => self.init_alt_item1(),                // alt_item_1
                        25 => {}                                    // alt_item_2
                        11 => self.listener.init_repeat_item(),     // repeat_item
                        22 => {}                                    // repeat_item_1
                        27 | 28 => {}                               // repeat_item_2, repeat_item_3
                        12 => self.listener.init_item(),            // item
                        23 => {}                                    // item_1
                        13 => self.listener.init_char_set(),        // char_set
                        20 => self.init_char_set1(),                // char_set_1
                        26 => {}                                    // char_set_2
                        14 => self.listener.init_char_set_one(),    // char_set_one
                        24 => {}                                    // char_set_one_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 |                                         // file -> header file_1
                        1 => self.exit_file(alt_id),                // file -> file_1
                        33 => self.exit_file1(),                    // file_1 -> file_item file_1
                        34 => {}                                    // file_1 -> ε
                        2 |                                         // file_item -> option
                        3 |                                         // file_item -> declaration
                        4 => self.exit_file_item(alt_id),           // file_item -> rule
                        5 => self.exit_header(),                    // header -> "lexicon" Id ";"
                        6 => self.exit_declaration(),               // declaration -> "mode" Id ";"
                        7 => self.exit_option(),                    // option -> "channels" "{" Id option_1 "}"
                        35 => self.exit_option1(),                  // option_1 -> "," Id option_1
                        36 => {}                                    // option_1 -> ε
                        8 |                                         // rule -> "fragment" Id ":" match ";"
                        43 |                                        // rule_1 -> "->" actions ";"
                        44 => self.exit_rule(alt_id),               // rule_1 -> ";"
                     /* 9 */                                        // rule -> Id ":" match rule_1 (never called)
                        10 => self.exit_actions(),                  // actions -> action actions_1
                        37 => self.exit_actions1(),                 // actions_1 -> "," action actions_1
                        38 => {}                                    // actions_1 -> ε
                        11 |                                        // action -> "mode" "(" Id ")"
                        12 |                                        // action -> "push" "(" Id ")"
                        13 |                                        // action -> "pop"
                        14 |                                        // action -> "skip"
                        15 |                                        // action -> "more"
                        16 |                                        // action -> "type" "(" Id ")"
                        17 => self.exit_action(alt_id),             // action -> "channel" "(" Id ")"
                        18 => self.exit_match(),                    // match -> alt_items
                        19 => self.exit_alt_items(),                // alt_items -> alt_item alt_items_1
                        39 => self.exit_alt_items1(),               // alt_items_1 -> "|" alt_item alt_items_1
                        40 => {}                                    // alt_items_1 -> ε
                        20 => self.exit_alt_item(),                 // alt_item -> alt_item_1
                        53 |                                        // alt_item_2 -> alt_item_1
                        54 => self.exit_alt_item1(),                // alt_item_2 -> ε
                     /* 41 */                                       // alt_item_1 -> repeat_item alt_item_2 (never called)
                        46 |                                        // repeat_item_1 -> "?"
                        48 |                                        // repeat_item_1 -> ε
                        57 |                                        // repeat_item_2 -> "?"
                        58 |                                        // repeat_item_2 -> ε
                        59 |                                        // repeat_item_3 -> "?"
                        60 => self.exit_repeat_item(alt_id),        // repeat_item_3 -> ε
                     /* 21 */                                       // repeat_item -> item repeat_item_1 (never called)
                     /* 45 */                                       // repeat_item_1 -> "+" repeat_item_2 (never called)
                     /* 47 */                                       // repeat_item_1 -> "*" repeat_item_3 (never called)
                        22 |                                        // item -> "(" alt_items ")"
                        23 |                                        // item -> "~" item
                        24 |                                        // item -> Id
                        26 |                                        // item -> StrLit
                        27 |                                        // item -> char_set
                        49 |                                        // item_1 -> ".." CharLit
                        50 => self.exit_item(alt_id),               // item_1 -> ε
                     /* 25 */                                       // item -> CharLit item_1 (never called)
                        28 |                                        // char_set -> "[" char_set_1 "]"
                        29 |                                        // char_set -> "."
                        30 => self.exit_char_set(alt_id),           // char_set -> FixedSet
                        55 |                                        // char_set_2 -> char_set_1
                        56 => self.exit_char_set1(),                // char_set_2 -> ε
                     /* 42 */                                       // char_set_1 -> char_set_one char_set_2 (never called)
                        31 |                                        // char_set_one -> FixedSet
                        51 |                                        // char_set_one_1 -> "-" SetChar
                        52 => self.exit_char_set_one(alt_id),       // char_set_one_1 -> ε
                     /* 32 */                                       // char_set_one -> SetChar char_set_one_1 (never called)
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
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).collect::<Vec<_>>().join(", "));
            }
        }

        fn check_abort_request(&self) -> bool {
            self.listener.check_abort_request()
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
                    CtxFile::V1 { header, star }
                }
                1 => {
                    let star = self.stack.pop().unwrap().get_file1();
                    CtxFile::V2 { star }
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
            let Some(SynValue::File1(SynFile1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynFile1 item on wrapper stack");
            };
            star_acc.push(file_item);
        }

        fn exit_file_item(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                2 => {
                    let option = self.stack.pop().unwrap().get_option();
                    CtxFileItem::V1 { option }
                }
                3 => {
                    let declaration = self.stack.pop().unwrap().get_declaration();
                    CtxFileItem::V2 { declaration }
                }
                4 => {
                    let rule = self.stack.pop().unwrap().get_rule();
                    CtxFileItem::V3 { rule }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_file_item")
            };
            let val = self.listener.exit_file_item(ctx);
            self.stack.push(SynValue::FileItem(val));
        }

        fn exit_header(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxHeader::V1 { id };
            let val = self.listener.exit_header(ctx);
            self.stack.push(SynValue::Header(val));
        }

        fn exit_declaration(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxDeclaration::V1 { id };
            let val = self.listener.exit_declaration(ctx);
            self.stack.push(SynValue::Declaration(val));
        }

        fn exit_option(&mut self) {
            let star = self.stack.pop().unwrap().get_option1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxOption::V1 { id, star };
            let val = self.listener.exit_option(ctx);
            self.stack.push(SynValue::Option(val));
        }

        fn init_option1(&mut self) {
            let val = SynOption1(Vec::new());
            self.stack.push(SynValue::Option1(val));
        }

        fn exit_option1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let Some(SynValue::Option1(SynOption1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynOption1 item on wrapper stack");
            };
            star_acc.push(id);
        }

        fn exit_rule(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                8 => {
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::V1 { id, match1 }
                }
                43 => {
                    let actions = self.stack.pop().unwrap().get_actions();
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::V2 { id, match1, actions }
                }
                44 => {
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::V3 { id, match1 }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_rule")
            };
            let val = self.listener.exit_rule(ctx);
            self.stack.push(SynValue::Rule(val));
        }

        fn exit_actions(&mut self) {
            let star = self.stack.pop().unwrap().get_actions1();
            let action = self.stack.pop().unwrap().get_action();
            let ctx = CtxActions::V1 { action, star };
            let val = self.listener.exit_actions(ctx);
            self.stack.push(SynValue::Actions(val));
        }

        fn init_actions1(&mut self) {
            let val = SynActions1(Vec::new());
            self.stack.push(SynValue::Actions1(val));
        }

        fn exit_actions1(&mut self) {
            let action = self.stack.pop().unwrap().get_action();
            let Some(SynValue::Actions1(SynActions1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynActions1 item on wrapper stack");
            };
            star_acc.push(action);
        }

        fn exit_action(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                11 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::V1 { id }
                }
                12 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::V2 { id }
                }
                13 => {
                    CtxAction::V3
                }
                14 => {
                    CtxAction::V4
                }
                15 => {
                    CtxAction::V5
                }
                16 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::V6 { id }
                }
                17 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::V7 { id }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_action")
            };
            let val = self.listener.exit_action(ctx);
            self.stack.push(SynValue::Action(val));
        }

        fn exit_match(&mut self) {
            let alt_items = self.stack.pop().unwrap().get_alt_items();
            let ctx = CtxMatch::V1 { alt_items };
            let val = self.listener.exit_match(ctx);
            self.stack.push(SynValue::Match(val));
        }

        fn exit_alt_items(&mut self) {
            let star = self.stack.pop().unwrap().get_alt_items1();
            let alt_item = self.stack.pop().unwrap().get_alt_item();
            let ctx = CtxAltItems::V1 { alt_item, star };
            let val = self.listener.exit_alt_items(ctx);
            self.stack.push(SynValue::AltItems(val));
        }

        fn init_alt_items1(&mut self) {
            let val = SynAltItems1(Vec::new());
            self.stack.push(SynValue::AltItems1(val));
        }

        fn exit_alt_items1(&mut self) {
            let alt_item = self.stack.pop().unwrap().get_alt_item();
            let Some(SynValue::AltItems1(SynAltItems1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynAltItems1 item on wrapper stack");
            };
            star_acc.push(alt_item);
        }

        fn exit_alt_item(&mut self) {
            let plus = self.stack.pop().unwrap().get_alt_item1();
            let ctx = CtxAltItem::V1 { plus };
            let val = self.listener.exit_alt_item(ctx);
            self.stack.push(SynValue::AltItem(val));
        }

        fn init_alt_item1(&mut self) {
            let val = SynAltItem1(Vec::new());
            self.stack.push(SynValue::AltItem1(val));
        }

        fn exit_alt_item1(&mut self) {
            let repeat_item = self.stack.pop().unwrap().get_repeat_item();
            let Some(SynValue::AltItem1(SynAltItem1(plus_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynAltItem1 item on wrapper stack");
            };
            plus_acc.push(repeat_item);
        }

        fn exit_repeat_item(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                46 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::V5 { item }
                }
                48 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::V6 { item }
                }
                57 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::V3 { item }
                }
                58 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::V4 { item }
                }
                59 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::V1 { item }
                }
                60 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::V2 { item }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_repeat_item")
            };
            let val = self.listener.exit_repeat_item(ctx);
            self.stack.push(SynValue::RepeatItem(val));
        }

        fn exit_item(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                22 => {
                    let alt_items = self.stack.pop().unwrap().get_alt_items();
                    CtxItem::V6 { alt_items }
                }
                23 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxItem::V7 { item }
                }
                24 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxItem::V1 { id }
                }
                26 => {
                    let strlit = self.stack_t.pop().unwrap();
                    CtxItem::V4 { strlit }
                }
                27 => {
                    let char_set = self.stack.pop().unwrap().get_char_set();
                    CtxItem::V5 { char_set }
                }
                49 => {
                    let charlit_2 = self.stack_t.pop().unwrap();
                    let charlit_1 = self.stack_t.pop().unwrap();
                    CtxItem::V2 { charlit: [charlit_1, charlit_2] }
                }
                50 => {
                    let charlit = self.stack_t.pop().unwrap();
                    CtxItem::V3 { charlit }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_item")
            };
            let val = self.listener.exit_item(ctx);
            self.stack.push(SynValue::Item(val));
        }

        fn exit_char_set(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                28 => {
                    let plus = self.stack.pop().unwrap().get_char_set1();
                    CtxCharSet::V1 { plus }
                }
                29 => {
                    CtxCharSet::V2
                }
                30 => {
                    let fixedset = self.stack_t.pop().unwrap();
                    CtxCharSet::V3 { fixedset }
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
            let Some(SynValue::CharSet1(SynCharSet1(plus_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynCharSet1 item on wrapper stack");
            };
            plus_acc.push(char_set_one);
        }

        fn exit_char_set_one(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                31 => {
                    let fixedset = self.stack_t.pop().unwrap();
                    CtxCharSetOne::V3 { fixedset }
                }
                51 => {
                    let setchar_2 = self.stack_t.pop().unwrap();
                    let setchar_1 = self.stack_t.pop().unwrap();
                    CtxCharSetOne::V1 { setchar: [setchar_1, setchar_2] }
                }
                52 => {
                    let setchar = self.stack_t.pop().unwrap();
                    CtxCharSetOne::V2 { setchar }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_char_set_one")
            };
            let val = self.listener.exit_char_set_one(ctx);
            self.stack.push(SynValue::CharSetOne(val));
        }
    }

    // [lexiparser]
    // -------------------------------------------------------------------------
}

pub(crate) mod lexiparser_types {
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
    /// SynActions: User-defined type for `actions`
    #[derive(Debug, PartialEq)] pub struct SynActions();
    /// SynAction: User-defined type for `action`
    #[derive(Debug, PartialEq)] pub struct SynAction();
    /// SynMatch: User-defined type for `match`
    #[derive(Debug, PartialEq)] pub struct SynMatch();
    /// SynAltItems: User-defined type for `alt_items`
    #[derive(Debug, PartialEq)] pub struct SynAltItems();
    /// SynAltItem: User-defined type for `alt_item`
    #[derive(Debug, PartialEq)] pub struct SynAltItem();
    /// SynRepeatItem: User-defined type for `repeat_item`
    #[derive(Debug, PartialEq)] pub struct SynRepeatItem();
    /// SynItem: User-defined type for `item`
    #[derive(Debug, PartialEq)] pub struct SynItem();
    /// User-defined type for `char_set`
    #[derive(Debug, PartialEq)] pub struct SynCharSet();
    /// User-defined type for `char_set_one`
    #[derive(Debug, PartialEq)] pub struct SynCharSetOne();
}
