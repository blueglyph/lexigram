#![allow(unused)]

pub(crate) mod lexiparser {
    // -------------------------------------------------------------------------
    // [lexiparser]

    use rlexer::{CollectJoin, grammar::{FactorId, ProdFactor, Symbol, VarId}, parser::{Call, Listener, OpCode, Parser}, symbol_table::SymbolTable};
    use super::lexiparser_types::*;

    const PARSER_NUM_T: usize = 29;
    const PARSER_NUM_NT: usize = 25;
    const SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("More", Some("more")), ("Skip", Some("skip")), ("Type", Some("type")), ("Channel", Some("channel")), ("SymEof", Some("EOF")), ("Id", None), ("CharLit", None), ("CharSet", None), ("StrLit", None)];
    const SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "file_item", "header", "declaration", "option", "rule", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "file_1", "option_1", "actions_1", "alt_item_1", "alt_items_1", "repeat_item_1", "rule_1", "repeat_item_2", "item_1", "alt_item_2", "repeat_item_3", "repeat_item_4"];
    const SYMBOLS_NAMES: [(&str, VarId); 12] = [("actions_1", 15), ("alt_item_1", 16), ("alt_item_2", 22), ("alt_items_1", 17), ("file_1", 13), ("item_1", 21), ("option_1", 14), ("repeat_item_1", 18), ("repeat_item_2", 20), ("repeat_item_3", 23), ("repeat_item_4", 24), ("rule_1", 19)];
    const PARSING_FACTORS: [(VarId, &[Symbol]); 52] = [(0, &[Symbol::NT(2), Symbol::NT(13)]), (0, &[Symbol::NT(13)]), (1, &[Symbol::NT(4)]), (1, &[Symbol::NT(3)]), (1, &[Symbol::NT(5)]), (2, &[Symbol::T(16), Symbol::T(25), Symbol::T(12)]), (3, &[Symbol::T(17), Symbol::T(25), Symbol::T(12)]), (4, &[Symbol::T(14), Symbol::T(4), Symbol::T(25), Symbol::NT(14), Symbol::T(10)]), (5, &[Symbol::T(15), Symbol::T(25), Symbol::T(1), Symbol::NT(8), Symbol::T(12)]), (5, &[Symbol::T(25), Symbol::T(1), Symbol::NT(8), Symbol::NT(19)]), (6, &[Symbol::NT(7), Symbol::NT(15)]), (7, &[Symbol::T(17), Symbol::T(5), Symbol::T(25), Symbol::T(11)]), (7, &[Symbol::T(19), Symbol::T(5), Symbol::T(25), Symbol::T(11)]), (7, &[Symbol::T(18)]), (7, &[Symbol::T(21)]), (7, &[Symbol::T(20)]), (7, &[Symbol::T(22), Symbol::T(5), Symbol::T(25), Symbol::T(11)]), (7, &[Symbol::T(23), Symbol::T(5), Symbol::T(25), Symbol::T(11)]), (8, &[Symbol::NT(9)]), (9, &[Symbol::NT(10), Symbol::NT(17)]), (10, &[Symbol::NT(16)]), (11, &[Symbol::NT(12), Symbol::NT(20)]), (12, &[Symbol::T(5), Symbol::NT(9), Symbol::T(11)]), (12, &[Symbol::T(6), Symbol::NT(12)]), (12, &[Symbol::T(25)]), (12, &[Symbol::T(26), Symbol::NT(21)]), (12, &[Symbol::T(27)]), (12, &[Symbol::T(28)]), (13, &[Symbol::NT(1), Symbol::NT(13)]), (13, &[Symbol::Empty]), (14, &[Symbol::T(2), Symbol::T(25), Symbol::NT(14)]), (14, &[Symbol::Empty]), (15, &[Symbol::T(2), Symbol::NT(7), Symbol::NT(15)]), (15, &[Symbol::Empty]), (16, &[Symbol::NT(11), Symbol::NT(22)]), (17, &[Symbol::T(8), Symbol::NT(10), Symbol::NT(17)]), (17, &[Symbol::Empty]), (18, &[Symbol::T(7), Symbol::NT(23)]), (18, &[Symbol::T(13), Symbol::NT(24)]), (18, &[Symbol::Empty]), (19, &[Symbol::T(0), Symbol::NT(6), Symbol::T(12)]), (19, &[Symbol::T(12)]), (20, &[Symbol::T(9), Symbol::NT(18)]), (20, &[Symbol::NT(18)]), (21, &[Symbol::T(3), Symbol::T(26)]), (21, &[Symbol::Empty]), (22, &[Symbol::NT(16)]), (22, &[Symbol::Empty]), (23, &[Symbol::T(9), Symbol::NT(18)]), (23, &[Symbol::NT(18)]), (24, &[Symbol::T(9), Symbol::NT(18)]), (24, &[Symbol::NT(18)])];
    const PARSING_TABLE: [FactorId; 750] = [52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 1, 1, 0, 1, 52, 52, 52, 52, 52, 52, 52, 1, 52, 52, 52, 1, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 2, 4, 52, 3, 52, 52, 52, 52, 52, 52, 52, 4, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 5, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 6, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 7, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 8, 52, 52, 52, 52, 52, 52, 52, 52, 52, 9, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 10, 10, 10, 10, 10, 10, 10, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 11, 13, 12, 15, 14, 16, 17, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 18, 18, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 18, 18, 18, 18, 52, 52, 52, 52, 52, 52, 19, 19, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 19, 19, 19, 19, 52, 52, 52, 52, 52, 52, 20, 20, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 20, 20, 20, 20, 52, 52, 52, 52, 52, 52, 21, 21, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 21, 21, 21, 21, 52, 52, 52, 52, 52, 52, 22, 23, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 24, 25, 26, 27, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 28, 28, 52, 28, 52, 52, 52, 52, 52, 52, 52, 28, 52, 52, 52, 29, 52, 52, 30, 52, 52, 52, 52, 52, 52, 52, 31, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 32, 52, 52, 52, 52, 52, 52, 52, 52, 52, 33, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 34, 34, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 34, 34, 34, 34, 52, 36, 52, 52, 52, 52, 52, 52, 52, 35, 52, 52, 36, 36, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 39, 52, 52, 52, 52, 39, 39, 37, 39, 52, 52, 39, 39, 38, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 39, 39, 39, 39, 52, 40, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 41, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 43, 52, 52, 52, 52, 43, 43, 43, 43, 42, 52, 43, 43, 43, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 43, 43, 43, 43, 52, 45, 52, 52, 44, 52, 45, 45, 45, 45, 45, 52, 45, 45, 45, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 45, 45, 45, 45, 52, 47, 52, 52, 52, 52, 46, 46, 52, 47, 52, 52, 47, 47, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 46, 46, 46, 46, 52, 49, 52, 52, 52, 52, 49, 49, 49, 49, 48, 52, 49, 49, 49, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 49, 49, 49, 49, 52, 51, 52, 52, 52, 52, 51, 51, 51, 51, 50, 52, 51, 51, 51, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 51, 51, 51, 51, 52];
    const FLAGS: [u32; 25] = [2048, 0, 0, 0, 2048, 32, 2048, 0, 0, 512, 6144, 544, 34, 1, 1, 1, 4129, 4, 36, 64, 64, 64, 64, 64, 64];
    const PARENT: [Option<VarId>; 25] = [None, None, None, None, None, None, None, None, None, None, None, None, None, Some(0), Some(4), Some(6), Some(10), Some(9), Some(11), Some(5), Some(11), Some(12), Some(16), Some(18), Some(18)];
    const OPCODES: [&[OpCode]; 52] = [&[OpCode::Exit(0), OpCode::NT(13), OpCode::NT(2)], &[OpCode::Exit(1), OpCode::NT(13)], &[OpCode::Exit(2), OpCode::NT(4)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(12), OpCode::T(25), OpCode::T(16)], &[OpCode::Exit(6), OpCode::T(12), OpCode::T(25), OpCode::T(17)], &[OpCode::Exit(7), OpCode::T(10), OpCode::NT(14), OpCode::T(25), OpCode::T(4), OpCode::T(14)], &[OpCode::Exit(8), OpCode::T(12), OpCode::NT(8), OpCode::T(1), OpCode::T(25), OpCode::T(15)], &[OpCode::NT(19), OpCode::NT(8), OpCode::T(1), OpCode::T(25)], &[OpCode::Exit(10), OpCode::NT(15), OpCode::NT(7)], &[OpCode::Exit(11), OpCode::T(11), OpCode::T(25), OpCode::T(5), OpCode::T(17)], &[OpCode::Exit(12), OpCode::T(11), OpCode::T(25), OpCode::T(5), OpCode::T(19)], &[OpCode::Exit(13), OpCode::T(18)], &[OpCode::Exit(14), OpCode::T(21)], &[OpCode::Exit(15), OpCode::T(20)], &[OpCode::Exit(16), OpCode::T(11), OpCode::T(25), OpCode::T(5), OpCode::T(22)], &[OpCode::Exit(17), OpCode::T(11), OpCode::T(25), OpCode::T(5), OpCode::T(23)], &[OpCode::Exit(18), OpCode::NT(9)], &[OpCode::NT(17), OpCode::Exit(19), OpCode::NT(10)], &[OpCode::Exit(20), OpCode::NT(16)], &[OpCode::NT(20), OpCode::NT(12)], &[OpCode::Exit(22), OpCode::T(11), OpCode::NT(9), OpCode::T(5)], &[OpCode::Exit(23), OpCode::NT(12), OpCode::T(6)], &[OpCode::Exit(24), OpCode::T(25)], &[OpCode::NT(21), OpCode::T(26)], &[OpCode::Exit(26), OpCode::T(27)], &[OpCode::Exit(27), OpCode::T(28)], &[OpCode::Loop(13), OpCode::Exit(28), OpCode::NT(1)], &[OpCode::Exit(29)], &[OpCode::Loop(14), OpCode::Exit(30), OpCode::T(25), OpCode::T(2)], &[OpCode::Exit(31)], &[OpCode::Loop(15), OpCode::Exit(32), OpCode::NT(7), OpCode::T(2)], &[OpCode::Exit(33)], &[OpCode::NT(22), OpCode::NT(11)], &[OpCode::Loop(17), OpCode::Exit(35), OpCode::NT(10), OpCode::T(8)], &[OpCode::Exit(36)], &[OpCode::NT(23), OpCode::T(7)], &[OpCode::NT(24), OpCode::T(13)], &[OpCode::Exit(39)], &[OpCode::Exit(40), OpCode::T(12), OpCode::NT(6), OpCode::T(0)], &[OpCode::Exit(41), OpCode::T(12)], &[OpCode::NT(18), OpCode::Exit(42), OpCode::T(9)], &[OpCode::NT(18), OpCode::Exit(43)], &[OpCode::Exit(44), OpCode::T(26), OpCode::T(3)], &[OpCode::Exit(45)], &[OpCode::Loop(16), OpCode::Exit(46)], &[OpCode::Exit(47)], &[OpCode::Loop(18), OpCode::Exit(48), OpCode::T(9)], &[OpCode::Loop(18), OpCode::Exit(49)], &[OpCode::Loop(18), OpCode::Exit(50), OpCode::T(9)], &[OpCode::Loop(18), OpCode::Exit(51)]];
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
        /// `action -> mode ( Id )`
        Action1 { id: String },
        /// `action -> push ( Id )`
        Action2 { id: String },
        /// `action -> pop`
        Action3,
        /// `action -> skip`
        Action4,
        /// `action -> more`
        Action5,
        /// `action -> type ( Id )`
        Action6 { id: String },
        /// `action -> channel ( Id )`
        Action7 { id: String },
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
        /// `item -> Id`
        Item3 { id: String },
        /// `item -> CharSet`
        Item4 { charset: String },
        /// `item -> StrLit`
        Item5 { strlit: String },
        /// `item -> CharLit .. CharLit`
        Item6 { charlit: [String; 2] },
        /// `item -> CharLit`
        Item7 { charlit: String },
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
    /// Computed `[file_item]*` array in `file -> header  ► [file_item]* ◄ `, array in `file ->  ► [file_item]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynFile1(pub Vec<SynFileItem>);
    /// Computed `[, Id]*` array in `option -> channels { Id  ► [, Id]* ◄  }`
    #[derive(Debug, PartialEq)]
    pub struct SynOption1(pub Vec<String>);
    /// Computed `[, action]*` array in `actions -> action  ► [, action]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynActions1(pub Vec<SynAction>);
    /// Computed `[repeat_item]+` array in `alt_item ->  ► [repeat_item]+ ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynAltItem1(pub Vec<SynRepeatItem>);

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
                        28 => self.exit_file1(),                    // [file_item]* item in file -> header  ► [file_item]* ◄  | ...
                        29 => {}                                    // end of [file_item]* items in file -> header  ► [file_item]* ◄  | ...
                        2 |                                         // file_item -> option
                        3 |                                         // file_item -> declaration
                        4 => self.exit_file_item(factor_id),        // file_item -> rule
                        5 => self.exit_header(),                    // header -> lexicon Id ;
                        6 => self.exit_declaration(),               // declaration -> mode Id ;
                        7 => self.exit_option(),                    // option -> channels { Id [, Id]* }
                        30 => self.exit_option1(),                  // [, Id]* item in option -> channels { Id  ► [, Id]* ◄  }
                        31 => {}                                    // end of [, Id]* items in option -> channels { Id  ► [, Id]* ◄  }
                        8 |                                         // rule -> fragment Id : match ;
                        40 |                                        // rule -> Id : match -> actions ;
                        41 => self.exit_rule(factor_id),            // rule -> Id : match ;
                     /* 9 */                                        // rule -> Id : match -> actions ; | Id : match ; (never called)
                        10 => self.exit_actions(),                  // actions -> action [, action]*
                        32 => self.exit_actions1(),                 // [, action]* item in actions -> action  ► [, action]* ◄
                        33 => {}                                    // end of [, action]* items in actions -> action  ► [, action]* ◄
                        11 |                                        // action -> mode ( Id )
                        12 |                                        // action -> push ( Id )
                        13 |                                        // action -> pop
                        14 |                                        // action -> skip
                        15 |                                        // action -> more
                        16 |                                        // action -> type ( Id )
                        17 => self.exit_action(factor_id),          // action -> channel ( Id )
                        18 => self.exit_match(),                    // match -> alt_items
                        19 => self.init_alt_items(),                // alt_items -> alt_item
                        35 |                                        // alt_items -> alt_items | alt_item
                        36 => self.exit_alt_items1(factor_id),      // end of iterations in alt_items -> alt_items | alt_item
                        20 => self.exit_alt_item(),                 // alt_item -> [repeat_item]+
                        46 |                                        // [repeat_item]+ item in alt_item ->  ► [repeat_item]+ ◄
                        47 => self.exit_alt_item1(),                // end of [repeat_item]+ items in alt_item ->  ► [repeat_item]+ ◄
                     /* 34 */                                       // [repeat_item]+ item in alt_item ->  ► [repeat_item]+ ◄  (never called)
                        42 |                                        // repeat_item -> item ?
                        43 => self.init_repeat_item(factor_id),     // repeat_item -> item
                        39 |                                        // end of iterations in repeat_item -> repeat_item + ? | repeat_item + | repeat_item * ? | repeat_item *
                        48 |                                        // repeat_item -> repeat_item + ?
                        49 |                                        // repeat_item -> repeat_item +
                        50 |                                        // repeat_item -> repeat_item * ?
                        51 => self.exit_repeat_item1(factor_id),    // repeat_item -> repeat_item *
                     /* 21 */                                       // repeat_item -> item ? | item (never called)
                     /* 37 */                                       // repeat_item -> repeat_item + ? | repeat_item + (never called)
                     /* 38 */                                       // repeat_item -> repeat_item * ? | repeat_item * (never called)
                        22 |                                        // item -> ( alt_items )
                        23 |                                        // item -> ~ item
                        24 |                                        // item -> Id
                        26 |                                        // item -> CharSet
                        27 |                                        // item -> StrLit
                        44 |                                        // item -> CharLit .. CharLit
                        45 => self.exit_item(factor_id),            // item -> CharLit
                     /* 25 */                                       // item -> CharLit | CharLit .. CharLit (never called)
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
            star_it.0.push(file_item);
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
                40 => {
                    let actions = self.stack.pop().unwrap().get_actions();
                    let match1 = self.stack.pop().unwrap().get_match();
                    let id = self.stack_t.pop().unwrap();
                    CtxRule::Rule2 { id, match1, actions }
                }
                41 => {
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
            star_it.0.push(action);
            self.stack.push(SynValue::Actions1(star_it));
        }

        fn exit_action(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                11 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::Action1 { id }
                }
                12 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::Action2 { id }
                }
                13 => {
                    CtxAction::Action3
                }
                14 => {
                    CtxAction::Action4
                }
                15 => {
                    CtxAction::Action5
                }
                16 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::Action6 { id }
                }
                17 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxAction::Action7 { id }
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
                35 => {
                    let alt_item = self.stack.pop().unwrap().get_alt_item();
                    let alt_items = self.stack.pop().unwrap().get_alt_items();
                    CtxAltItems::AltItems2 { alt_items, alt_item }
                }
                36 => {
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
            plus_it.0.push(repeat_item);
            self.stack.push(SynValue::AltItem1(plus_it));
        }

        fn init_repeat_item(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                42 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxRepeatItem::RepeatItem2 { item }
                }
                43 => {
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
                39 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem1 { repeat_item }
                }
                48 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem4 { repeat_item }
                }
                49 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem5 { repeat_item }
                }
                50 => {
                    let repeat_item = self.stack.pop().unwrap().get_repeat_item();
                    CtxRepeatItem::RepeatItem6 { repeat_item }
                }
                51 => {
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
                22 => {
                    let alt_items = self.stack.pop().unwrap().get_alt_items();
                    CtxItem::Item1 { alt_items }
                }
                23 => {
                    let item = self.stack.pop().unwrap().get_item();
                    CtxItem::Item2 { item }
                }
                24 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxItem::Item3 { id }
                }
                26 => {
                    let charset = self.stack_t.pop().unwrap();
                    CtxItem::Item4 { charset }
                }
                27 => {
                    let strlit = self.stack_t.pop().unwrap();
                    CtxItem::Item5 { strlit }
                }
                44 => {
                    let charlit_2 = self.stack_t.pop().unwrap();
                    let charlit_1 = self.stack_t.pop().unwrap();
                    CtxItem::Item6 { charlit: [charlit_1, charlit_2] }
                }
                45 => {
                    let charlit = self.stack_t.pop().unwrap();
                    CtxItem::Item7 { charlit }
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
    use crate::lexi::LexAction;

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
    #[derive(Debug, PartialEq)] pub struct SynActions(pub LexAction);
    /// SynAction: User-defined type for `action`
    #[derive(Debug, PartialEq)] pub struct SynAction(pub LexAction);
    /// SynMatch: User-defined type for `match`
    #[derive(Debug, PartialEq)] pub struct SynMatch();
    /// SynAltItems: User-defined type for `alt_items`
    #[derive(Debug, PartialEq)] pub struct SynAltItems(pub usize);
    /// SynAltItem: User-defined type for `alt_item`
    #[derive(Debug, PartialEq)] pub struct SynAltItem(pub usize);
    /// SynRepeatItem: User-defined type for `repeat_item`
    #[derive(Debug, PartialEq)] pub struct SynRepeatItem(pub usize);
    /// SynItem: User-defined type for `item`
    #[derive(Debug, PartialEq)] pub struct SynItem(pub usize);
}
