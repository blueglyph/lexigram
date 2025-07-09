// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

pub(crate) mod gramparser {
    // -------------------------------------------------------------------------
    // [gramparser]

    use lexigram::{CollectJoin, SymbolTable, grammar::{FactorId, ProdFactor, Symbol, VarId}, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::gramparser_types::*;

    const PARSER_NUM_T: usize = 13;
    const PARSER_NUM_NT: usize = 14;
    const SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Id", None)];
    const SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["file", "header", "rules", "rule", "rule_name", "prod", "prod_factor", "prod_term", "term_item", "prod_factor_1", "rules_1", "prod_1", "rule_1", "prod_term_1"];
    const PARSING_FACTORS: [(VarId, &[Symbol]); 24] = [(0, &[Symbol::NT(1), Symbol::NT(2)]), (1, &[Symbol::T(8), Symbol::T(12), Symbol::T(6)]), (2, &[Symbol::NT(3), Symbol::NT(10)]), (3, &[Symbol::NT(4), Symbol::T(0), Symbol::NT(5), Symbol::NT(12)]), (4, &[Symbol::T(12)]), (5, &[Symbol::NT(6), Symbol::NT(11)]), (6, &[Symbol::NT(9)]), (7, &[Symbol::NT(8), Symbol::NT(13)]), (8, &[Symbol::T(12)]), (8, &[Symbol::T(10)]), (8, &[Symbol::T(11)]), (8, &[Symbol::T(1), Symbol::NT(5), Symbol::T(5)]), (9, &[Symbol::NT(7), Symbol::NT(9)]), (9, &[Symbol::Empty]), (10, &[Symbol::NT(3), Symbol::NT(10)]), (10, &[Symbol::Empty]), (11, &[Symbol::T(2), Symbol::NT(6), Symbol::NT(11)]), (11, &[Symbol::Empty]), (12, &[Symbol::T(6)]), (12, &[Symbol::T(9), Symbol::T(6)]), (13, &[Symbol::T(3)]), (13, &[Symbol::T(4)]), (13, &[Symbol::T(7)]), (13, &[Symbol::Empty])];
    const PARSING_TABLE: [FactorId; 196] = [24, 24, 24, 24, 24, 24, 24, 24, 0, 24, 24, 24, 24, 25, 24, 24, 24, 24, 24, 24, 24, 24, 1, 24, 24, 24, 25, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 2, 25, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 3, 25, 25, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 4, 24, 24, 5, 5, 24, 24, 5, 5, 24, 24, 5, 5, 5, 5, 24, 24, 6, 6, 24, 24, 6, 6, 24, 24, 6, 6, 6, 6, 24, 24, 7, 25, 24, 24, 25, 25, 24, 24, 25, 7, 7, 7, 24, 24, 11, 25, 25, 25, 25, 25, 25, 24, 25, 9, 10, 8, 24, 24, 12, 13, 24, 24, 13, 13, 24, 24, 13, 12, 12, 12, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 14, 15, 24, 24, 16, 24, 24, 17, 17, 24, 24, 17, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 18, 24, 24, 19, 24, 24, 25, 25, 24, 23, 23, 20, 21, 23, 23, 22, 24, 23, 23, 23, 23, 24];
    const FLAGS: [u32; 14] = [0, 0, 512, 32, 0, 512, 2048, 32, 0, 1, 4, 4, 64, 64];
    const PARENT: [Option<VarId>; 14] = [None, None, None, None, None, None, None, None, None, Some(6), Some(2), Some(5), Some(3), Some(7)];
    const OPCODES: [&[OpCode]; 24] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::T(6), OpCode::T(12), OpCode::T(8)], &[OpCode::NT(10), OpCode::Exit(2), OpCode::NT(3)], &[OpCode::NT(12), OpCode::NT(5), OpCode::T(0), OpCode::NT(4)], &[OpCode::Exit(4), OpCode::T(12)], &[OpCode::NT(11), OpCode::Exit(5), OpCode::NT(6)], &[OpCode::Exit(6), OpCode::NT(9)], &[OpCode::NT(13), OpCode::NT(8)], &[OpCode::Exit(8), OpCode::T(12)], &[OpCode::Exit(9), OpCode::T(10)], &[OpCode::Exit(10), OpCode::T(11)], &[OpCode::Exit(11), OpCode::T(5), OpCode::NT(5), OpCode::T(1)], &[OpCode::Loop(9), OpCode::Exit(12), OpCode::NT(7)], &[OpCode::Exit(13)], &[OpCode::Loop(10), OpCode::Exit(14), OpCode::NT(3)], &[OpCode::Exit(15)], &[OpCode::Loop(11), OpCode::Exit(16), OpCode::NT(6), OpCode::T(2)], &[OpCode::Exit(17)], &[OpCode::Exit(18), OpCode::T(6)], &[OpCode::Exit(19), OpCode::T(6), OpCode::T(9)], &[OpCode::Exit(20), OpCode::T(3)], &[OpCode::Exit(21), OpCode::T(4)], &[OpCode::Exit(22), OpCode::T(7)], &[OpCode::Exit(23)]];
    const START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser {
        let mut symbol_table = SymbolTable::new();
        symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));
        symbol_table.extend_nonterminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));
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
        /// `file -> header rules`
        File { header: SynHeader, rules: SynRules },
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
    }
    #[derive(Debug)]
    pub enum CtxRule {
        /// `rule -> rule_name : prod ;`
        Rule1 { rule_name: SynRuleName, prod: SynProd },
        /// `rule -> rule_name : prod EOF ;`
        Rule2 { rule_name: SynRuleName, prod: SynProd },
    }
    #[derive(Debug)]
    pub enum CtxRuleName {
        /// `rule_name -> Id`
        RuleName { id: String },
    }
    #[derive(Debug)]
    pub enum CtxProd {
        /// `prod -> prod_factor`
        Prod1 { prod_factor: SynProdFactor },
        /// `prod -> prod | prod_factor`
        Prod2 { prod: SynProd, prod_factor: SynProdFactor },
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
    // /// User-defined type for `rule_name`
    // #[derive(Debug, PartialEq)] pub struct SynRuleName();
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
    enum SynValue { File(SynFile), Header(SynHeader), Rules(SynRules), Rule(SynRule), RuleName(SynRuleName), Prod(SynProd), ProdFactor(SynProdFactor), ProdTerm(SynProdTerm), TermItem(SynTermItem), ProdFactor1(SynProdFactor1) }

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
        fn get_rule_name(self) -> SynRuleName {
            if let SynValue::RuleName(val) = self { val } else { panic!() }
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
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        fn exit(&mut self, _file: SynFile) {}
        fn init_file(&mut self) {}
        fn exit_file(&mut self, _ctx: CtxFile) -> SynFile;
        fn init_header(&mut self) {}
        fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader;
        fn init_rules(&mut self) {}
        fn exit_rules(&mut self, _ctx: CtxRules) -> SynRules;
        fn exitloop_rules(&mut self, _rules: &mut SynRules) {}
        fn init_rule(&mut self) {}
        fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule;
        fn init_rule_name(&mut self) {}
        fn exit_rule_name(&mut self, _ctx: CtxRuleName) -> SynRuleName;
        fn init_prod(&mut self) {}
        fn exit_prod(&mut self, _ctx: CtxProd) -> SynProd;
        fn exitloop_prod(&mut self, _prod: &mut SynProd) {}
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
                        1 => self.listener.init_header(),           // header
                        2 => self.listener.init_rules(),            // rules
                        10 => {}                                    // rules_1
                        3 => self.listener.init_rule(),             // rule
                        12 => {}                                    // rule_1
                        4 => self.listener.init_rule_name(),        // rule_name
                        5 => self.listener.init_prod(),             // prod
                        11 => {}                                    // prod_1
                        6 => self.listener.init_prod_factor(),      // prod_factor
                        9 => self.init_prod_factor1(),              // prod_factor_1
                        7 => self.listener.init_prod_term(),        // prod_term
                        13 => {}                                    // prod_term_1
                        8 => self.listener.init_term_item(),        // term_item
                        _ => panic!("unexpected enter non-terminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match factor_id {
                        0 => self.exit_file(),                      // file -> header rules
                        1 => self.exit_header(),                    // header -> grammar Id ;
                        2 => self.inter_rules(),                    // rules -> rule rules_1
                        14 => self.exit_rules1(),                   // rules_1 -> rule rules_1
                        15 => self.exitloop_rules1(),               // rules_1 -> ε
                        18 |                                        // rule_1 -> ;
                        19 => self.exit_rule(factor_id),            // rule_1 -> EOF ;
                     /* 3 */                                        // rule -> rule_name : prod rule_1 (never called)
                        4 => self.exit_rule_name(),                 // rule_name -> Id
                        5 => self.inter_prod(),                     // prod -> prod_factor prod_1
                        16 => self.exit_prod1(),                    // prod_1 -> | prod_factor prod_1
                        17 => self.exitloop_prod1(),                // prod_1 -> ε
                        6 => self.exit_prod_factor(),               // prod_factor -> prod_factor_1
                        12 => self.exit_prod_factor1(),             // prod_factor_1 -> prod_term prod_factor_1
                        13 => {}                                    // prod_factor_1 -> ε
                        20 |                                        // prod_term_1 -> +
                        21 |                                        // prod_term_1 -> ?
                        22 |                                        // prod_term_1 -> *
                        23 => self.exit_prod_term(factor_id),       // prod_term_1 -> ε
                     /* 7 */                                        // prod_term -> term_item prod_term_1 (never called)
                        8 |                                         // term_item -> Id
                        9 |                                         // term_item -> Lform
                        10 |                                        // term_item -> <R>
                        11 => self.exit_term_item(factor_id),       // term_item -> ( prod )
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

        fn check_abort_request(&self) -> bool {
            self.listener.check_abort_request()
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

        fn exit_file(&mut self) {
            let rules = self.stack.pop().unwrap().get_rules();
            let header = self.stack.pop().unwrap().get_header();
            let val = self.listener.exit_file(CtxFile::File { header, rules });
            self.stack.push(SynValue::File(val));
        }

        fn exit_header(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_header(CtxHeader::Header { id });
            self.stack.push(SynValue::Header(val));
        }

        fn inter_rules(&mut self) {
            let rule = self.stack.pop().unwrap().get_rule();
            let val = self.listener.exit_rules(CtxRules::Rules1 { rule });
            self.stack.push(SynValue::Rules(val));
        }

        fn exit_rules1(&mut self) {
            let rule = self.stack.pop().unwrap().get_rule();
            let rules = self.stack.pop().unwrap().get_rules();
            let val = self.listener.exit_rules(CtxRules::Rules2 { rules, rule });
            self.stack.push(SynValue::Rules(val));
        }

        fn exitloop_rules1(&mut self) {
            let SynValue::Rules(rules) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_rules(rules);
        }

        fn exit_rule(&mut self, factor_id: FactorId) {
            let ctx = match factor_id {
                18 => {
                    let prod = self.stack.pop().unwrap().get_prod();
                    let rule_name = self.stack.pop().unwrap().get_rule_name();
                    CtxRule::Rule1 { rule_name, prod }
                }
                19 => {
                    let prod = self.stack.pop().unwrap().get_prod();
                    let rule_name = self.stack.pop().unwrap().get_rule_name();
                    CtxRule::Rule2 { rule_name, prod }
                }
                _ => panic!("unexpected factor id {factor_id} in fn exit_rule")
            };
            let val = self.listener.exit_rule(ctx);
            self.stack.push(SynValue::Rule(val));
        }

        fn exit_rule_name(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let val = self.listener.exit_rule_name(CtxRuleName::RuleName { id });
            self.stack.push(SynValue::RuleName(val));
        }

        fn inter_prod(&mut self) {
            let prod_factor = self.stack.pop().unwrap().get_prod_factor();
            let val = self.listener.exit_prod(CtxProd::Prod1 { prod_factor });
            self.stack.push(SynValue::Prod(val));
        }

        fn exit_prod1(&mut self) {
            let prod_factor = self.stack.pop().unwrap().get_prod_factor();
            let prod = self.stack.pop().unwrap().get_prod();
            let val = self.listener.exit_prod(CtxProd::Prod2 { prod, prod_factor });
            self.stack.push(SynValue::Prod(val));
        }

        fn exitloop_prod1(&mut self) {
            let SynValue::Prod(prod) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_prod(prod);
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
                20 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm1 { term_item }
                }
                21 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm2 { term_item }
                }
                22 => {
                    let term_item = self.stack.pop().unwrap().get_term_item();
                    CtxProdTerm::ProdTerm3 { term_item }
                }
                23 => {
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
                8 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxTermItem::TermItem1 { id }
                }
                9 => {
                    let lform = self.stack_t.pop().unwrap();
                    CtxTermItem::TermItem2 { lform }
                }
                10 => {
                    CtxTermItem::TermItem3
                }
                11 => {
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
    /// User-defined type for `rule_name`
    #[derive(Debug, PartialEq)] pub struct SynRuleName(pub String);
    /// User-defined type for `prod`
    #[derive(Debug, PartialEq)] pub struct SynProd(pub usize);
    /// User-defined type for `prod_factor`
    #[derive(Debug, PartialEq)] pub struct SynProdFactor(pub usize);
    /// User-defined type for `prod_term`
    #[derive(Debug, PartialEq)] pub struct SynProdTerm(pub usize);
    /// User-defined type for `term_item`
    #[derive(Debug, PartialEq)] pub struct SynTermItem(pub usize);
}
