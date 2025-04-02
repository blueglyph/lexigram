// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

pub(crate) mod gramparser {
    // -------------------------------------------------------------------------
    // [gramparser]

    use lexigram::{CollectJoin, grammar::{FactorId, ProdFactor, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}, symbol_table::SymbolTable};
    use super::gramparser_types::*;

    const PARSER_NUM_T: usize = 13;
    const PARSER_NUM_NT: usize = 13;
    const SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Id", None)];
    const SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "header", "rules", "rule", "prod", "prod_factor", "prod_term", "term_item", "prod_factor_1", "rules_1", "prod_1", "file_1", "prod_term_1"];
    const SYMBOLS_NAMES: [(&str, VarId); 5] = [("file_1", 11), ("prod_1", 10), ("prod_factor_1", 8), ("prod_term_1", 12), ("rules_1", 9)];
    const PARSING_FACTORS: [(VarId, &[Symbol]); 23] = [(0, &[Symbol::NT(1), Symbol::NT(2), Symbol::NT(11)]), (1, &[Symbol::T(8), Symbol::T(12), Symbol::T(6)]), (2, &[Symbol::NT(3), Symbol::NT(9)]), (3, &[Symbol::T(12), Symbol::T(0), Symbol::NT(4), Symbol::T(6)]), (4, &[Symbol::NT(5), Symbol::NT(10)]), (5, &[Symbol::NT(8)]), (6, &[Symbol::NT(7), Symbol::NT(12)]), (7, &[Symbol::T(12)]), (7, &[Symbol::T(10)]), (7, &[Symbol::T(11)]), (7, &[Symbol::T(1), Symbol::NT(4), Symbol::T(5)]), (8, &[Symbol::NT(6), Symbol::NT(8)]), (8, &[Symbol::Empty]), (9, &[Symbol::NT(3), Symbol::NT(9)]), (9, &[Symbol::Empty]), (10, &[Symbol::T(2), Symbol::NT(5), Symbol::NT(10)]), (10, &[Symbol::Empty]), (11, &[Symbol::T(9)]), (11, &[Symbol::Empty]), (12, &[Symbol::T(3)]), (12, &[Symbol::T(4)]), (12, &[Symbol::T(7)]), (12, &[Symbol::Empty])];
    const PARSING_TABLE: [FactorId; 182] = [23, 23, 23, 23, 23, 23, 23, 23, 0, 23, 23, 23, 23, 24, 23, 23, 23, 23, 23, 23, 23, 23, 1, 23, 23, 23, 24, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 23, 23, 2, 24, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 23, 23, 3, 24, 23, 4, 4, 23, 23, 4, 4, 23, 23, 23, 4, 4, 4, 23, 23, 5, 5, 23, 23, 5, 5, 23, 23, 23, 5, 5, 5, 23, 23, 6, 24, 23, 23, 24, 24, 23, 23, 23, 6, 6, 6, 23, 23, 10, 24, 24, 24, 24, 24, 24, 23, 23, 8, 9, 7, 23, 23, 11, 12, 23, 23, 12, 12, 23, 23, 23, 11, 11, 11, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 14, 23, 23, 13, 14, 23, 23, 15, 23, 23, 16, 16, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 17, 23, 23, 23, 18, 23, 22, 22, 19, 20, 22, 22, 21, 23, 23, 22, 22, 22, 23];
    const FLAGS: [u32; 13] = [32, 0, 512, 0, 512, 2048, 32, 0, 1, 4, 4, 64, 64];
    const PARENT: [Option<VarId>; 13] = [None, None, None, None, None, None, None, None, Some(5), Some(2), Some(4), Some(0), Some(6)];
    const OPCODES: [&[OpCode]; 23] = [&[OpCode::NT(11), OpCode::NT(2), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::T(6), OpCode::T(12), OpCode::T(8)], &[OpCode::NT(9), OpCode::Exit(2), OpCode::NT(3)], &[OpCode::Exit(3), OpCode::T(6), OpCode::NT(4), OpCode::T(0), OpCode::T(12)], &[OpCode::NT(10), OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::NT(8)], &[OpCode::NT(12), OpCode::NT(7)], &[OpCode::Exit(7), OpCode::T(12)], &[OpCode::Exit(8), OpCode::T(10)], &[OpCode::Exit(9), OpCode::T(11)], &[OpCode::Exit(10), OpCode::T(5), OpCode::NT(4), OpCode::T(1)], &[OpCode::Loop(8), OpCode::Exit(11), OpCode::NT(6)], &[OpCode::Exit(12)], &[OpCode::Loop(9), OpCode::Exit(13), OpCode::NT(3)], &[OpCode::Exit(14)], &[OpCode::Loop(10), OpCode::Exit(15), OpCode::NT(5), OpCode::T(2)], &[OpCode::Exit(16)], &[OpCode::Exit(17), OpCode::T(9)], &[OpCode::Exit(18)], &[OpCode::Exit(19), OpCode::T(3)], &[OpCode::Exit(20), OpCode::T(4)], &[OpCode::Exit(21), OpCode::T(7)], &[OpCode::Exit(22)]];
    const START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = SymbolTable::new();
        symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));
        symbol_table.extend_non_terminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));
        symbol_table.extend_names(SYMBOLS_NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));
        let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();
        let table: Vec<FactorId> = PARSING_TABLE.into();
        let parsing_table = lexigram::grammar::LLParsingTable {
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
        /// `file -> header rules EOF`
        File1 { header: SynHeader, rules: SynRules },
        /// `file -> header rules`
        File2 { header: SynHeader, rules: SynRules },
    }
    #[derive(Debug)]
    pub enum CtxHeader {
        /// `header -> grammar Id ;`
        Header { id: String },
    }
    #[derive(Debug)]
    pub enum CtxRules {
        /// `rules -> rule`
        Rules1 { rule: SynRule },
        /// `rules -> rules rule`
        Rules2 { rules: SynRules, rule: SynRule },
        /// end of iterations in rules -> rules rule
        Rules3 { rules: SynRules },
    }
    #[derive(Debug)]
    pub enum CtxRule {
        /// `rule -> Id : prod ;`
        Rule { id: String, prod: SynProd },
    }
    #[derive(Debug)]
    pub enum CtxProd {
        /// `prod -> prod_factor`
        Prod1 { prod_factor: SynProdFactor },
        /// `prod -> prod | prod_factor`
        Prod2 { prod: SynProd, prod_factor: SynProdFactor },
        /// end of iterations in prod -> prod | prod_factor
        Prod3 { prod: SynProd },
    }
    #[derive(Debug)]
    pub enum CtxProdFactor {
        /// `prod_factor -> [prod_term]*`
        ProdFactor { star: SynProdFactor1 },
    }
    #[derive(Debug)]
    pub enum CtxProdTerm {
        /// `prod_term -> term_item +`
        ProdTerm1 { term_item: SynTermItem },
        /// `prod_term -> term_item ?`
        ProdTerm2 { term_item: SynTermItem },
        /// `prod_term -> term_item *`
        ProdTerm3 { term_item: SynTermItem },
        /// `prod_term -> term_item`
        ProdTerm4 { term_item: SynTermItem },
    }
    #[derive(Debug)]
    pub enum CtxTermItem {
        /// `term_item -> Id`
        TermItem1 { id: String },
        /// `term_item -> Lform`
        TermItem2 { lform: String },
        /// `term_item -> <R>`
        TermItem3,
        /// `term_item -> ( prod )`
        TermItem4 { prod: SynProd },
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `file`
    // #[derive(Debug, PartialEq)] pub struct SynFile();
    // /// User-defined type for `header`
    // #[derive(Debug, PartialEq)] pub struct SynHeader();
    // /// User-defined type for `rules`
    // #[derive(Debug, PartialEq)] pub struct SynRules();
    // /// User-defined type for `rule`
    // #[derive(Debug, PartialEq)] pub struct SynRule();
    // /// User-defined type for `prod`
    // #[derive(Debug, PartialEq)] pub struct SynProd();
    // /// User-defined type for `prod_factor`
    // #[derive(Debug, PartialEq)] pub struct SynProdFactor();
    // /// User-defined type for `prod_term`
    // #[derive(Debug, PartialEq)] pub struct SynProdTerm();
    // /// User-defined type for `term_item`
    // #[derive(Debug, PartialEq)] pub struct SynTermItem();
    /// Computed `[prod_term]*` array in `prod_factor ->  ► [prod_term]* ◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynProdFactor1(pub Vec<SynProdTerm>);

    #[derive(Debug)]
    enum SynValue { File(SynFile), Header(SynHeader), Rules(SynRules), Rule(SynRule), Prod(SynProd), ProdFactor(SynProdFactor), ProdTerm(SynProdTerm), TermItem(SynTermItem), ProdFactor1(SynProdFactor1) }

    impl SynValue {
        fn get_file(self) -> SynFile {
            if let SynValue::File(val) = self { val } else { panic!() }
        }
        fn get_header(self) -> SynHeader {
            if let SynValue::Header(val) = self { val } else { panic!() }
        }
        fn get_rules(self) -> SynRules {
            if let SynValue::Rules(val) = self { val } else { panic!() }
        }
        fn get_rule(self) -> SynRule {
            if let SynValue::Rule(val) = self { val } else { panic!() }
        }
        fn get_prod(self) -> SynProd {
            if let SynValue::Prod(val) = self { val } else { panic!() }
        }
        fn get_prod_factor(self) -> SynProdFactor {
            if let SynValue::ProdFactor(val) = self { val } else { panic!() }
        }
        fn get_prod_term(self) -> SynProdTerm {
            if let SynValue::ProdTerm(val) = self { val } else { panic!() }
        }
        fn get_term_item(self) -> SynTermItem {
            if let SynValue::TermItem(val) = self { val } else { panic!() }
        }
        fn get_prod_factor1(self) -> SynProdFactor1 {
            if let SynValue::ProdFactor1(val) = self { val } else { panic!() }
        }
    }

    pub trait GramParserListener {
        fn get_mut_log(&mut self) -> &mut impl Logger;
        fn exit(&mut self, _file: SynFile) {}
        fn init_file(&mut self) {}
        fn exit_file(&mut self, _ctx: CtxFile) -> SynFile;
        fn init_header(&mut self) {}
        fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader;
        fn init_rules(&mut self) {}
        fn exit_rules(&mut self, _ctx: CtxRules) -> SynRules;
        fn init_rule(&mut self) {}
        fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule;
        fn init_prod(&mut self) {}
        fn exit_prod(&mut self, _ctx: CtxProd) -> SynProd;
        fn init_prod_factor(&mut self) {}
        fn exit_prod_factor(&mut self, _ctx: CtxProdFactor) -> SynProdFactor;
        fn init_prod_term(&mut self) {}
        fn exit_prod_term(&mut self, _ctx: CtxProdTerm) -> SynProdTerm;
        fn init_term_item(&mut self) {}
        fn exit_term_item(&mut self, _ctx: CtxTermItem) -> SynTermItem;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: GramParserListener> ListenerWrapper for Wrapper<T> {
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
                        11 => {}                                    // file_1
                        1 => self.listener.init_header(),           // header
                        2 => self.listener.init_rules(),            // rules
                        9 => {}                                     // rules_1
                        3 => self.listener.init_rule(),             // rule
                        4 => self.listener.init_prod(),             // prod
                        10 => {}                                    // prod_1
                        5 => self.listener.init_prod_factor(),      // prod_factor
                        8 => self.init_prod_factor1(),              // prod_factor_1
                        6 => self.listener.init_prod_term(),        // prod_term
                        12 => {}                                    // prod_term_1
                        7 => self.listener.init_term_item(),        // term_item
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        17 |                                        // file -> header rules EOF
                        18 => self.exit_file(factor_id),            // file -> header rules
                     /* 0 */                                        // file -> header rules | header rules EOF (never called)
                        1 => self.exit_header(),                    // header -> grammar Id ;
                        2 => self.init_rules(),                     // rules -> rule
                        13 |                                        // rules -> rules rule
                        14 => self.exit_rules1(factor_id),          // end of iterations in rules -> rules rule
                        3 => self.exit_rule(),                      // rule -> Id : prod ;
                        4 => self.init_prod(),                      // prod -> prod_factor
                        15 |                                        // prod -> prod | prod_factor
                        16 => self.exit_prod1(factor_id),           // end of iterations in prod -> prod | prod_factor
                        5 => self.exit_prod_factor(),               // prod_factor -> [prod_term]*
                        11 => self.exit_prod_factor1(),             // [prod_term]* item in prod_factor ->  ► [prod_term]* ◄
                        12 => {}                                    // end of [prod_term]* items in prod_factor ->  ► [prod_term]* ◄
                        19 |                                        // prod_term -> term_item +
                        20 |                                        // prod_term -> term_item ?
                        21 |                                        // prod_term -> term_item *
                        22 => self.exit_prod_term(factor_id),       // prod_term -> term_item
                     /* 6 */                                        // prod_term -> term_item | term_item + | term_item ? | term_item * (never called)
                        7 |                                         // term_item -> Id
                        8 |                                         // term_item -> Lform
                        9 |                                         // term_item -> <R>
                        10 => self.exit_term_item(factor_id),       // term_item -> ( prod )
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

        fn get_mut_log(&mut self) -> &mut impl Logger {
            self.listener.get_mut_log()
        }
    }

    impl<T: GramParserListener> Wrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn get_listener(&self) -> &T {
            &self.listener
        }

        pub fn get_mut_listener(&mut self) -> &mut T {
            &mut self.listener
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
                17 => {
                    let rules = self.stack.pop().unwrap().get_rules();
                    let header = self.stack.pop().unwrap().get_header();
                    CtxFile::File1 { header, rules }
                }
                18 => {
                    let rules = self.stack.pop().unwrap().get_rules();
                    let header = self.stack.pop().unwrap().get_header();
                    CtxFile::File2 { header, rules }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_file")
            };
            let val = self.listener.exit_file(ctx);
            self.stack.push(SynValue::File(val));
        }

        fn exit_header(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_header(CtxHeader::Header { id });
            self.stack.push(SynValue::Header(val));
        }

        fn init_rules(&mut self) {
            let rule = self.stack.pop().unwrap().get_rule();
            let val = self.listener.exit_rules(CtxRules::Rules1 { rule });
            self.stack.push(SynValue::Rules(val));
        }

        fn exit_rules1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                13 => {
                    let rule = self.stack.pop().unwrap().get_rule();
                    let rules = self.stack.pop().unwrap().get_rules();
                    CtxRules::Rules2 { rules, rule }
                }
                14 => {
                    let rules = self.stack.pop().unwrap().get_rules();
                    CtxRules::Rules3 { rules }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_rules1")
            };
            let val = self.listener.exit_rules(ctx);
            self.stack.push(SynValue::Rules(val));
        }

        fn exit_rule(&mut self) {
            let prod = self.stack.pop().unwrap().get_prod();
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_rule(CtxRule::Rule { id, prod });
            self.stack.push(SynValue::Rule(val));
        }

        fn init_prod(&mut self) {
            let prod_factor = self.stack.pop().unwrap().get_prod_factor();
            let val = self.listener.exit_prod(CtxProd::Prod1 { prod_factor });
            self.stack.push(SynValue::Prod(val));
        }

        fn exit_prod1(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                15 => {
                    let prod_factor = self.stack.pop().unwrap().get_prod_factor();
                    let prod = self.stack.pop().unwrap().get_prod();
                    CtxProd::Prod2 { prod, prod_factor }
                }
                16 => {
                    let prod = self.stack.pop().unwrap().get_prod();
                    CtxProd::Prod3 { prod }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_prod1")
            };
            let val = self.listener.exit_prod(ctx);
            self.stack.push(SynValue::Prod(val));
        }

        fn exit_prod_factor(&mut self) {
            let star = self.stack.pop().unwrap().get_prod_factor1();
            let val = self.listener.exit_prod_factor(CtxProdFactor::ProdFactor { star });
            self.stack.push(SynValue::ProdFactor(val));
        }

        fn init_prod_factor1(&mut self) {
            let val = SynProdFactor1(Vec::new());
            self.stack.push(SynValue::ProdFactor1(val));
        }

        fn exit_prod_factor1(&mut self) {
            let prod_term = self.stack.pop().unwrap().get_prod_term();
            let mut star_it = self.stack.pop().unwrap().get_prod_factor1();
            star_it.0.push(prod_term);
            self.stack.push(SynValue::ProdFactor1(star_it));
        }

        fn exit_prod_term(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                19 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm1 { term_item }
                }
                20 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm2 { term_item }
                }
                21 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm3 { term_item }
                }
                22 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm4 { term_item }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_prod_term")
            };
            let val = self.listener.exit_prod_term(ctx);
            self.stack.push(SynValue::ProdTerm(val));
        }

        fn exit_term_item(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                7 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxTermItem::TermItem1 { id }
                }
                8 => {
                    let lform = self.stack_t.pop().unwrap();
                    CtxTermItem::TermItem2 { lform }
                }
                9 => {
                    CtxTermItem::TermItem3
                }
                10 => {
                    let prod = self.stack.pop().unwrap().get_prod();
                    CtxTermItem::TermItem4 { prod }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_term_item")
            };
            let val = self.listener.exit_term_item(ctx);
            self.stack.push(SynValue::TermItem(val));
        }
    }

    // [gramparser]
    // -------------------------------------------------------------------------
}

pub(crate) mod gramparser_types {

    /// User-defined type for `file`
    #[derive(Debug, PartialEq)] pub struct SynFile();
    /// User-defined type for `header`
    #[derive(Debug, PartialEq)] pub struct SynHeader();
    /// User-defined type for `rules`
    #[derive(Debug, PartialEq)] pub struct SynRules();
    /// User-defined type for `rule`
    #[derive(Debug, PartialEq)] pub struct SynRule();
    /// User-defined type for `prod`
    #[derive(Debug, PartialEq)] pub struct SynProd();
    /// User-defined type for `prod_factor`
    #[derive(Debug, PartialEq)] pub struct SynProdFactor();
    /// User-defined type for `prod_term`
    #[derive(Debug, PartialEq)] pub struct SynProdTerm();
    /// User-defined type for `term_item`
    #[derive(Debug, PartialEq)] pub struct SynTermItem();
}
