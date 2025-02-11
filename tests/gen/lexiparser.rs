#![allow(unused)]

pub(crate) mod lexiparser {
    // -------------------------------------------------------------------------
    // [lexiparser]

    use rlexer::{CollectJoin, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{Call, Listener, OpCode, Parser}, symbol_table::SymbolTable};
    use super::lexiparser_types::*;

    const PARSER_NUM_T: usize = 27;
    const PARSER_NUM_NT: usize = 25;
    const SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("Return", Some("return")), ("Skip", Some("skip")), ("SymEof", Some("EOF")), ("Id", None), ("CharLit", None), ("CharSet", None), ("StrLit", None)];
    const SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "file_item", "header", "declaration", "option", "rule", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "file_1", "option_1", "actions_1", "alt_item_1", "alt_items_1", "repeat_item_1", "rule_1", "repeat_item_2", "item_1", "alt_item_2", "repeat_item_3", "repeat_item_4"];
    const SYMBOLS_NAMES: [(&str, VarId); 12] = [("actions_1", 15), ("alt_item_1", 16), ("alt_item_2", 22), ("alt_items_1", 17), ("file_1", 13), ("item_1", 21), ("option_1", 14), ("repeat_item_1", 18), ("repeat_item_2", 20), ("repeat_item_3", 23), ("repeat_item_4", 24), ("rule_1", 19)];
    const PARSING_FACTORS: [(VarId, &[Symbol]); 50] = [(0, &[Symbol::NT(2), Symbol::NT(13)]), (0, &[Symbol::NT(13)]), (1, &[Symbol::NT(4)]), (1, &[Symbol::NT(3)]), (1, &[Symbol::NT(5)]), (2, &[Symbol::T(16), Symbol::T(23), Symbol::T(12)]), (3, &[Symbol::T(17), Symbol::T(23), Symbol::T(12)]), (4, &[Symbol::T(14), Symbol::T(4), Symbol::T(23), Symbol::NT(14), Symbol::T(10)]), (5, &[Symbol::T(15), Symbol::T(23), Symbol::T(1), Symbol::NT(8), Symbol::T(12)]), (5, &[Symbol::T(23), Symbol::T(1), Symbol::NT(8), Symbol::NT(19)]), (6, &[Symbol::NT(7), Symbol::NT(15)]), (7, &[Symbol::T(19), Symbol::T(5), Symbol::T(23), Symbol::T(11)]), (7, &[Symbol::T(18)]), (7, &[Symbol::T(21)]), (7, &[Symbol::T(20)]), (8, &[Symbol::NT(9)]), (9, &[Symbol::NT(10), Symbol::NT(17)]), (10, &[Symbol::NT(16)]), (11, &[Symbol::NT(12), Symbol::NT(20)]), (12, &[Symbol::T(5), Symbol::NT(9), Symbol::T(11)]), (12, &[Symbol::T(6), Symbol::NT(12)]), (12, &[Symbol::T(22)]), (12, &[Symbol::T(23)]), (12, &[Symbol::T(24), Symbol::NT(21)]), (12, &[Symbol::T(25)]), (12, &[Symbol::T(26)]), (13, &[Symbol::NT(1), Symbol::NT(13)]), (13, &[Symbol::Empty]), (14, &[Symbol::T(2), Symbol::T(23), Symbol::NT(14)]), (14, &[Symbol::Empty]), (15, &[Symbol::T(2), Symbol::NT(7), Symbol::NT(15)]), (15, &[Symbol::Empty]), (16, &[Symbol::NT(11), Symbol::NT(22)]), (17, &[Symbol::T(8), Symbol::NT(10), Symbol::NT(17)]), (17, &[Symbol::Empty]), (18, &[Symbol::T(7), Symbol::NT(23)]), (18, &[Symbol::T(13), Symbol::NT(24)]), (18, &[Symbol::Empty]), (19, &[Symbol::T(0), Symbol::NT(6), Symbol::T(12)]), (19, &[Symbol::T(12)]), (20, &[Symbol::T(9), Symbol::NT(18)]), (20, &[Symbol::NT(18)]), (21, &[Symbol::T(3), Symbol::T(24)]), (21, &[Symbol::Empty]), (22, &[Symbol::NT(16)]), (22, &[Symbol::Empty]), (23, &[Symbol::T(9), Symbol::NT(18)]), (23, &[Symbol::NT(18)]), (24, &[Symbol::T(9), Symbol::NT(18)]), (24, &[Symbol::NT(18)])];
    const PARSING_TABLE: [FactorId; 700] = [50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 1, 1, 0, 1, 50, 50, 50, 50, 50, 1, 50, 50, 50, 1, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 2, 4, 50, 3, 50, 50, 50, 50, 50, 4, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 5, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 6, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 7, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 8, 50, 50, 50, 50, 50, 50, 50, 9, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 10, 10, 10, 10, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 12, 11, 14, 13, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 15, 15, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 15, 15, 15, 15, 15, 50, 50, 50, 50, 50, 50, 16, 16, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 16, 16, 16, 16, 16, 50, 50, 50, 50, 50, 50, 17, 17, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 17, 17, 17, 17, 17, 50, 50, 50, 50, 50, 50, 18, 18, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 18, 18, 18, 18, 18, 50, 50, 50, 50, 50, 50, 19, 20, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 21, 22, 23, 24, 25, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 26, 26, 50, 26, 50, 50, 50, 50, 50, 26, 50, 50, 50, 27, 50, 50, 28, 50, 50, 50, 50, 50, 50, 50, 29, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 30, 50, 50, 50, 50, 50, 50, 50, 50, 50, 31, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 32, 32, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 32, 32, 32, 32, 32, 50, 34, 50, 50, 50, 50, 50, 50, 50, 33, 50, 50, 34, 34, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 37, 50, 50, 50, 50, 37, 37, 35, 37, 50, 50, 37, 37, 36, 50, 50, 50, 50, 50, 50, 50, 50, 37, 37, 37, 37, 37, 50, 38, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 39, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 41, 50, 50, 50, 50, 41, 41, 41, 41, 40, 50, 41, 41, 41, 50, 50, 50, 50, 50, 50, 50, 50, 41, 41, 41, 41, 41, 50, 43, 50, 50, 42, 50, 43, 43, 43, 43, 43, 50, 43, 43, 43, 50, 50, 50, 50, 50, 50, 50, 50, 43, 43, 43, 43, 43, 50, 45, 50, 50, 50, 50, 44, 44, 50, 45, 50, 50, 45, 45, 50, 50, 50, 50, 50, 50, 50, 50, 50, 44, 44, 44, 44, 44, 50, 47, 50, 50, 50, 50, 47, 47, 47, 47, 46, 50, 47, 47, 47, 50, 50, 50, 50, 50, 50, 50, 50, 47, 47, 47, 47, 47, 50, 49, 50, 50, 50, 50, 49, 49, 49, 49, 48, 50, 49, 49, 49, 50, 50, 50, 50, 50, 50, 50, 50, 49, 49, 49, 49, 49, 50];
    const FLAGS: [u32; 25] = [2048, 0, 0, 0, 2048, 32, 2048, 0, 0, 512, 6144, 544, 34, 1, 1, 1, 4129, 4, 36, 64, 64, 64, 64, 64, 64];
    const PARENT: [Option<VarId>; 25] = [None, None, None, None, None, None, None, None, None, None, None, None, None, Some(0), Some(4), Some(6), Some(10), Some(9), Some(11), Some(5), Some(11), Some(12), Some(16), Some(18), Some(18)];
    const OPCODES: [&[OpCode]; 50] = [&[OpCode::Exit(0), OpCode::NT(13), OpCode::NT(2)], &[OpCode::Exit(1), OpCode::NT(13)], &[OpCode::Exit(2), OpCode::NT(4)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(12), OpCode::T(23), OpCode::T(16)], &[OpCode::Exit(6), OpCode::T(12), OpCode::T(23), OpCode::T(17)], &[OpCode::Exit(7), OpCode::T(10), OpCode::NT(14), OpCode::T(23), OpCode::T(4), OpCode::T(14)], &[OpCode::Exit(8), OpCode::T(12), OpCode::NT(8), OpCode::T(1), OpCode::T(23), OpCode::T(15)], &[OpCode::NT(19), OpCode::NT(8), OpCode::T(1), OpCode::T(23)], &[OpCode::Exit(10), OpCode::NT(15), OpCode::NT(7)], &[OpCode::Exit(11), OpCode::T(11), OpCode::T(23), OpCode::T(5), OpCode::T(19)], &[OpCode::Exit(12), OpCode::T(18)], &[OpCode::Exit(13), OpCode::T(21)], &[OpCode::Exit(14), OpCode::T(20)], &[OpCode::Exit(15), OpCode::NT(9)], &[OpCode::NT(17), OpCode::Exit(16), OpCode::NT(10)], &[OpCode::Exit(17), OpCode::NT(16)], &[OpCode::NT(20), OpCode::NT(12)], &[OpCode::Exit(19), OpCode::T(11), OpCode::NT(9), OpCode::T(5)], &[OpCode::Exit(20), OpCode::NT(12), OpCode::T(6)], &[OpCode::Exit(21), OpCode::T(22)], &[OpCode::Exit(22), OpCode::T(23)], &[OpCode::NT(21), OpCode::T(24)], &[OpCode::Exit(24), OpCode::T(25)], &[OpCode::Exit(25), OpCode::T(26)], &[OpCode::Loop(13), OpCode::Exit(26), OpCode::NT(1)], &[OpCode::Exit(27)], &[OpCode::Loop(14), OpCode::Exit(28), OpCode::T(23), OpCode::T(2)], &[OpCode::Exit(29)], &[OpCode::Loop(15), OpCode::Exit(30), OpCode::NT(7), OpCode::T(2)], &[OpCode::Exit(31)], &[OpCode::NT(22), OpCode::NT(11)], &[OpCode::Loop(17), OpCode::Exit(33), OpCode::NT(10), OpCode::T(8)], &[OpCode::Exit(34)], &[OpCode::NT(23), OpCode::T(7)], &[OpCode::NT(24), OpCode::T(13)], &[OpCode::Exit(37)], &[OpCode::Exit(38), OpCode::T(12), OpCode::NT(6), OpCode::T(0)], &[OpCode::Exit(39), OpCode::T(12)], &[OpCode::NT(18), OpCode::Exit(40), OpCode::T(9)], &[OpCode::NT(18), OpCode::Exit(41)], &[OpCode::Exit(42), OpCode::T(24), OpCode::T(3)], &[OpCode::Exit(43)], &[OpCode::Loop(16), OpCode::Exit(44)], &[OpCode::Exit(45)], &[OpCode::Loop(18), OpCode::Exit(46), OpCode::T(9)], &[OpCode::Loop(18), OpCode::Exit(47)], &[OpCode::Loop(18), OpCode::Exit(48), OpCode::T(9)], &[OpCode::Loop(18), OpCode::Exit(49)]];
    const START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = SymbolTable::new();
        symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));
        symbol_table.extend_non_terminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));
        symbol_table.extend_names(SYMBOLS_NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = rlexer::grammar::LLParsingTable {
            num_nt: PARSER_NUM_NT,
            num_t: PARSER_NUM_T + 1,
            factors,
            table,
            flags: FLAGS.into(),
            parent: PARENT.into(),
        };
        Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)
    }

    #[derive(Debug)]
    pub enum CtxFile {
        /// `file -> header [file_item]*`
        File1 { header: SynHeader, star: SynFile1 },
        /// `file -> [file_item]*`
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
        /// `header -> lexicon Id ;`
        Header { id: String },
    }
    #[derive(Debug)]
    pub enum CtxDeclaration {
        /// `declaration -> mode Id ;`
        Declaration { id: String },
    }
    #[derive(Debug)]
    pub enum CtxOption {
        /// `option -> channels { Id [, Id]* }`
        Option { id: String, star: SynOption1 },
    }
    #[derive(Debug)]
    pub enum CtxRule {
        /// `rule -> fragment Id : match ;`
        Rule1 { id: String, match1: SynMatch },
        /// `rule -> Id : match -> actions ;`
        Rule2 { id: String, match1: SynMatch, actions: SynActions },
        /// `rule -> Id : match ;`
        Rule3 { id: String, match1: SynMatch },
    }
    #[derive(Debug)]
    pub enum CtxActions {
        /// `actions -> action [, action]*`
        Actions { action: SynAction, star: SynActions1 },
    }
    #[derive(Debug)]
    pub enum CtxAction {
        /// `action -> push ( Id )`
        Action1 { id: String },
        /// `action -> pop`
        Action2,
        /// `action -> skip`
        Action3,
        /// `action -> return`
        Action4,
    }
    #[derive(Debug)]
    pub enum CtxMatch {
        /// `match -> alt_items`
        Match { alt_items: SynAltItems },
    }
    #[derive(Debug)]
    pub enum CtxAltItems {
        /// `alt_items -> alt_item`
        AltItems1 { alt_item: SynAltItem },
        /// `alt_items -> alt_items | alt_item`
        AltItems2 { alt_items: SynAltItems, alt_item: SynAltItem },
        /// end of iterations in alt_items -> alt_items | alt_item
        AltItems3 { alt_items: SynAltItems },
    }
    #[derive(Debug)]
    pub enum CtxAltItem {
        /// `alt_item -> [repeat_item]+`
        AltItem { plus: SynAltItem1 },
    }
    #[derive(Debug)]
    pub enum CtxRepeatItem {
        /// end of iterations in repeat_item -> repeat_item + ? | repeat_item + | repeat_item * ? | repeat_item *
        RepeatItem1 { repeat_item: SynRepeatItem },
        /// `repeat_item -> item ?`
        RepeatItem2 { item: SynItem },
        /// `repeat_item -> item`
        RepeatItem3 { item: SynItem },
        /// `repeat_item -> repeat_item + ?`
        RepeatItem4 { repeat_item: SynRepeatItem },
        /// `repeat_item -> repeat_item +`
        RepeatItem5 { repeat_item: SynRepeatItem },
        /// `repeat_item -> repeat_item * ?`
        RepeatItem6 { repeat_item: SynRepeatItem },
        /// `repeat_item -> repeat_item *`
        RepeatItem7 { repeat_item: SynRepeatItem },
    }
    #[derive(Debug)]
    pub enum CtxItem {
        /// `item -> ( alt_items )`
        Item1 { alt_items: SynAltItems },
        /// `item -> ~ item`
        Item2 { item: SynItem },
        /// `item -> EOF`
        Item3,
        /// `item -> Id`
        Item4 { id: String },
        /// `item -> CharSet`
        Item5 { charset: String },
        /// `item -> StrLit`
        Item6 { strlit: String },
        /// `item -> CharLit .. CharLit`
        Item7 { charlit: [String; 2] },
        /// `item -> CharLit`
        Item8 { charlit: String },
    }

    // NT types:
    // SynFile: User-defined type for `file`
    // SynFileItem: User-defined type for `file_item`
    // SynHeader: User-defined type for `header`
    // SynDeclaration: User-defined type for `declaration`
    // SynOption: User-defined type for `option`
    // SynRule: User-defined type for `rule`
    // SynActions: User-defined type for `actions`
    // SynAction: User-defined type for `action`
    // SynMatch: User-defined type for `match`
    // SynAltItems: User-defined type for `alt_items`
    // SynAltItem: User-defined type for `alt_item`
    // SynRepeatItem: User-defined type for `repeat_item`
    // SynItem: User-defined type for `item`
    /// Computed `[file_item]*` array in `file -> header  ► [file_item]* ◄ `, array in `file ->  ► [file_item]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynFile1(Vec<SynFile1Item>);
    /// `file_item` item in `file -> header  ► [file_item]* ◄ `, item in `file ->  ► [file_item]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynFile1Item { file_item: SynFileItem }
    /// Computed `[, Id]*` array in `option -> channels { Id  ► [, Id]* ◄  }`
    #[derive(Debug, PartialEq)]
    pub struct SynOption1(Vec<String>);
    /// Computed `[, action]*` array in `actions -> action  ► [, action]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynActions1(Vec<SynActions1Item>);
    /// `, action` item in `actions -> action  ► [, action]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynActions1Item { action: SynAction }
    /// Computed `[repeat_item]+` array in `alt_item ->  ► [repeat_item]+ ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynAltItem1(Vec<SynAltItem1Item>);
    /// `repeat_item` item in `alt_item ->  ► [repeat_item]+ ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynAltItem1Item { repeat_item: SynRepeatItem }

    #[derive(Debug)]
    enum SynValue { File(SynFile), FileItem(SynFileItem), Header(SynHeader), Declaration(SynDeclaration), Option(SynOption), Rule(SynRule), Actions(SynActions), Action(SynAction), Match(SynMatch), AltItems(SynAltItems), AltItem(SynAltItem), RepeatItem(SynRepeatItem), Item(SynItem), File1(SynFile1), Option1(SynOption1), Actions1(SynActions1), AltItem1(SynAltItem1) }

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
        fn get_file1(self) -> SynFile1 {
            if let SynValue::File1(val) = self { val } else { panic!() }
        }
        fn get_option1(self) -> SynOption1 {
            if let SynValue::Option1(val) = self { val } else { panic!() }
        }
        fn get_actions1(self) -> SynActions1 {
            if let SynValue::Actions1(val) = self { val } else { panic!() }
        }
        fn get_alt_item1(self) -> SynAltItem1 {
            if let SynValue::AltItem1(val) = self { val } else { panic!() }
        }
    }

    pub trait LexiParserListener {
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
    }

    pub struct ListenerWrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: LexiParserListener> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: FactorId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, factor={factor_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_file(),             // file
                        13 => self.init_file1(),                    // file_1
                        1 => self.listener.init_file_item(),        // file_item
                        2 => self.listener.init_header(),           // header
                        3 => self.listener.init_declaration(),      // declaration
                        4 => self.listener.init_option(),           // option
                        14 => self.init_option1(),                  // option_1
                        5 => self.listener.init_rule(),             // rule
                        19 => {}                                    // rule_1
                        6 => self.listener.init_actions(),          // actions
                        15 => self.init_actions1(),                 // actions_1
                        7 => self.listener.init_action(),           // action
                        8 => self.listener.init_match(),            // match
                        9 => self.listener.init_alt_items(),        // alt_items
                        17 => {}                                    // alt_items_1
                        10 => self.listener.init_alt_item(),        // alt_item
                        16 => self.init_alt_item1(),                // alt_item_1
                        22 => {}                                    // alt_item_2
                        11 => self.listener.init_repeat_item(),     // repeat_item
                        18 => {}                                    // repeat_item_1
                        20 => {}                                    // repeat_item_2
                        23 => {}                                    // repeat_item_3
                        24 => {}                                    // repeat_item_4
                        12 => self.listener.init_item(),            // item
                        21 => {}                                    // item_1
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 |                                         // file -> header [file_item]*
                        1 => self.exit_file(factor_id),             // file -> [file_item]*
                        26 => self.exit_file1(),                    // [file_item]* item in file -> header  ► [file_item]* ◄  | ...
                        27 => {}                                    // end of [file_item]* items in file -> header  ► [file_item]* ◄  | ...
                        2 |                                         // file_item -> option
                        3 |                                         // file_item -> declaration
                        4 => self.exit_file_item(factor_id),        // file_item -> rule
                        5 => self.exit_header(),                    // header -> lexicon Id ;
                        6 => self.exit_declaration(),               // declaration -> mode Id ;
                        7 => self.exit_option(),                    // option -> channels { Id [, Id]* }
                        28 => self.exit_option1(),                  // [, Id]* item in option -> channels { Id  ► [, Id]* ◄  }
                        29 => {}                                    // end of [, Id]* items in option -> channels { Id  ► [, Id]* ◄  }
                        8 |                                         // rule -> fragment Id : match ;
                        38 |                                        // rule -> Id : match -> actions ;
                        39 => self.exit_rule(factor_id),            // rule -> Id : match ;
                     /* 9 */                                        // rule -> Id : match -> actions ; | Id : match ; (never called)
                        10 => self.exit_actions(),                  // actions -> action [, action]*
                        30 => self.exit_actions1(),                 // [, action]* item in actions -> action  ► [, action]* ◄
                        31 => {}                                    // end of [, action]* items in actions -> action  ► [, action]* ◄
                        11 |                                        // action -> push ( Id )
                        12 |                                        // action -> pop
                        13 |                                        // action -> skip
                        14 => self.exit_action(factor_id),          // action -> return
                        15 => self.exit_match(),                    // match -> alt_items
                        16 => self.init_alt_items(),                // alt_items -> alt_item
                        33 |                                        // alt_items -> alt_items | alt_item
                        34 => self.exit_alt_items1(factor_id),      // end of iterations in alt_items -> alt_items | alt_item
                        17 => self.exit_alt_item(),                 // alt_item -> [repeat_item]+
                        44 |                                        // [repeat_item]+ item in alt_item ->  ► [repeat_item]+ ◄
                        45 => self.exit_alt_item1(),                // end of [repeat_item]+ items in alt_item ->  ► [repeat_item]+ ◄
                     /* 32 */                                       // [repeat_item]+ item in alt_item ->  ► [repeat_item]+ ◄  (never called)
                        40 |                                        // repeat_item -> item ?
                        41 => self.init_repeat_item(factor_id),     // repeat_item -> item
                        37 |                                        // end of iterations in repeat_item -> repeat_item + ? | repeat_item + | repeat_item * ? | repeat_item *
                        46 |                                        // repeat_item -> repeat_item + ?
                        47 |                                        // repeat_item -> repeat_item +
                        48 |                                        // repeat_item -> repeat_item * ?
                        49 => self.exit_repeat_item1(factor_id),    // repeat_item -> repeat_item *
                     /* 18 */                                       // repeat_item -> item ? | item (never called)
                     /* 35 */                                       // repeat_item -> repeat_item + ? | repeat_item + (never called)
                     /* 36 */                                       // repeat_item -> repeat_item * ? | repeat_item * (never called)
                        19 |                                        // item -> ( alt_items )
                        20 |                                        // item -> ~ item
                        21 |                                        // item -> EOF
                        22 |                                        // item -> Id
                        24 |                                        // item -> CharSet
                        25 |                                        // item -> StrLit
                        42 |                                        // item -> CharLit .. CharLit
                        43 => self.exit_item(factor_id),            // item -> CharLit
                     /* 23 */                                       // item -> CharLit | CharLit .. CharLit (never called)
                        _ => panic!("unexpected exit factor id: {factor_id}")
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
    }

    impl<T: LexiParserListener> ListenerWrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            ListenerWrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }

        pub fn set_verbose(&mut self, verbose: bool) {
            self.verbose = verbose;
        }

        fn exit(&mut self) {
            let file = self.stack.pop().unwrap().get_file();
            self.listener.exit(file);
        }

        fn exit_file(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                0 => {
                    let star = self.stack.pop().unwrap().get_file1();
                    let header = self.stack.pop().unwrap().get_header();
                    CtxFile::File1 { header, star }
                }
                1 => {
                    let star = self.stack.pop().unwrap().get_file1();
                    CtxFile::File2 { star }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_file")
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
            star_it.0.push(SynFile1Item { file_item });
            self.stack.push(SynValue::File1(star_it));
        }

        fn exit_file_item(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
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
                _ => panic!("unexpected factor id {factor_id} in fn exit_file_item")
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

        fn exit_rule(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                8 => {
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::Rule1 { id, match1 }
                }
                38 => {
                    let actions = self.stack.pop().unwrap().get_actions();
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::Rule2 { id, match1, actions }
                }
                39 => {
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::Rule3 { id, match1 }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_rule")
            };
            let val = self.listener.exit_rule(ctx);
            self.stack.push(SynValue::Rule(val));
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
            star_it.0.push(SynActions1Item { action });
            self.stack.push(SynValue::Actions1(star_it));
        }

        fn exit_action(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                11 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::Action1 { id }
                }
                12 => {
                    CtxAction::Action2
                }
                13 => {
                    CtxAction::Action3
                }
                14 => {
                    CtxAction::Action4
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_action")
            };
            let val = self.listener.exit_action(ctx);
            self.stack.push(SynValue::Action(val));
        }

        fn exit_match(&mut self) {
            let alt_items = self.stack.pop().unwrap().get_alt_items();
            let val = self.listener.exit_match(CtxMatch::Match { alt_items });
            self.stack.push(SynValue::Match(val));
        }

        fn init_alt_items(&mut self) {
            let alt_item = self.stack.pop().unwrap().get_alt_item();
            let val = self.listener.exit_alt_items(CtxAltItems::AltItems1 { alt_item });
            self.stack.push(SynValue::AltItems(val));
        }

        fn exit_alt_items1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                33 => {
                    let alt_item = self.stack.pop().unwrap().get_alt_item();
                    let alt_items = self.stack.pop().unwrap().get_alt_items();
                    CtxAltItems::AltItems2 { alt_items, alt_item }
                }
                34 => {
                    let alt_items = self.stack.pop().unwrap().get_alt_items();
                    CtxAltItems::AltItems3 { alt_items }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_alt_items1")
            };
            let val = self.listener.exit_alt_items(ctx);
            self.stack.push(SynValue::AltItems(val));
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
            plus_it.0.push(SynAltItem1Item { repeat_item });
            self.stack.push(SynValue::AltItem1(plus_it));
        }

        fn init_repeat_item(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                40 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::RepeatItem2 { item }
                }
                41 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::RepeatItem3 { item }
                }
                _ => panic!("unexpected factor id {factor_id} in fn init_repeat_item")
            };
            let val = self.listener.exit_repeat_item(ctx);
            self.stack.push(SynValue::RepeatItem(val));
        }

        fn exit_repeat_item1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                37 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem1 { repeat_item }
                }
                46 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem4 { repeat_item }
                }
                47 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem5 { repeat_item }
                }
                48 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem6 { repeat_item }
                }
                49 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem7 { repeat_item }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_repeat_item1")
            };
            let val = self.listener.exit_repeat_item(ctx);
            self.stack.push(SynValue::RepeatItem(val));
        }

        fn exit_item(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                19 => {
                    let alt_items = self.stack.pop().unwrap().get_alt_items();
                    CtxItem::Item1 { alt_items }
                }
                20 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxItem::Item2 { item }
                }
                21 => {
                    CtxItem::Item3
                }
                22 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxItem::Item4 { id }
                }
                24 => {
                    let charset = self.stack_t.pop().unwrap();
                    CtxItem::Item5 { charset }
                }
                25 => {
                    let strlit = self.stack_t.pop().unwrap();
                    CtxItem::Item6 { strlit }
                }
                42 => {
                    let charlit_2 = self.stack_t.pop().unwrap();
                    let charlit_1 = self.stack_t.pop().unwrap();
                    CtxItem::Item7 { charlit: [charlit_1, charlit_2] }
                }
                43 => {
                    let charlit = self.stack_t.pop().unwrap();
                    CtxItem::Item8 { charlit }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_item")
            };
            let val = self.listener.exit_item(ctx);
            self.stack.push(SynValue::Item(val));
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
}
