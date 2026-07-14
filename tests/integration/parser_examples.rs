// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]
#![allow(non_camel_case_types)]

#[allow(unused)]
pub(crate) mod listener1 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener1]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{OpCode, Symbol, ll1::LLParser}};

    const PARSER_NUM_T: usize = 3;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Not", Some("!")), ("Sub", Some("-")), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1"];
    static ALT_VAR: [VarId; 4] = [0, 0, 1, 1];
    static ALTERNATIVES: [&[Symbol]; 4] = [&[Symbol::T(1), Symbol::NT(0)], &[Symbol::T(2), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(1)], &[Symbol::Empty]];
    static PARSING_TABLE: [AltId; 8] = [4, 0, 1, 5, 2, 4, 4, 3];
    static OPCODES: [&[OpCode]; 4] = [&[OpCode::Exit(0), OpCode::NT(0), OpCode::T(1)], &[OpCode::NT(1), OpCode::Exit(1), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::T(0)], &[OpCode::Exit(3)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener1]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener2 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener2]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{OpCode, Symbol, ll1::LLParser}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Sub", Some("-")), ("Mul", Some("*")), ("Div", Some("/")), ("Add", Some("+")), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1", "e_2", "e_3", "e_4"];
    static ALT_VAR: [VarId; 12] = [0, 1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 4];
    static ALTERNATIVES: [&[Symbol]; 12] = [&[Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(2), Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(3), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(2), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(1), Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(2), Symbol::NT(4), Symbol::NT(3)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(4)], &[Symbol::T(4)]];
    static PARSING_TABLE: [AltId; 30] = [0, 12, 12, 12, 0, 13, 4, 1, 2, 3, 12, 5, 6, 13, 13, 13, 6, 13, 9, 7, 8, 9, 12, 9, 10, 13, 13, 13, 11, 13];
    static OPCODES: [&[OpCode]; 12] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(4)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(4), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(4), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(2), OpCode::T(3)], &[OpCode::Loop(1), OpCode::Exit(4), OpCode::NT(2), OpCode::T(0)], &[OpCode::Exit(5)], &[OpCode::NT(3), OpCode::Exit(6), OpCode::NT(4)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(4), OpCode::T(1)], &[OpCode::Loop(3), OpCode::Exit(8), OpCode::NT(4), OpCode::T(2)], &[OpCode::Exit(9)], &[OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11), OpCode::T(4)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener2]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener3 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener3]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{OpCode, Symbol, ll1::LLParser}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Sub", Some("-")), ("Mul", Some("*")), ("Div", Some("/")), ("Add", Some("+")), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1", "e_2", "e_3", "e_4"];
    static ALT_VAR: [VarId; 12] = [0, 1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 4];
    static ALTERNATIVES: [&[Symbol]; 12] = [&[Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(3), Symbol::NT(0), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(0), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(3)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(3)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(4)], &[Symbol::T(4)]];
    static PARSING_TABLE: [AltId; 30] = [0, 13, 13, 13, 0, 13, 4, 1, 2, 3, 12, 5, 6, 13, 13, 13, 6, 13, 9, 7, 8, 9, 12, 9, 10, 13, 13, 13, 11, 13];
    static OPCODES: [&[OpCode]; 12] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(4)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(2), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(0), OpCode::T(3)], &[OpCode::Loop(1), OpCode::Exit(4), OpCode::NT(0), OpCode::T(0)], &[OpCode::Exit(5)], &[OpCode::NT(3), OpCode::Exit(6), OpCode::NT(4)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(3), OpCode::Exit(8), OpCode::NT(2), OpCode::T(2)], &[OpCode::Exit(9)], &[OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11), OpCode::T(4)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener3]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener4 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener4]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{OpCode, Symbol, ll1::LLParser}};

    const PARSER_NUM_T: usize = 5;
    const PARSER_NUM_NT: usize = 5;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Sub", Some("-")), ("Mul", Some("*")), ("Div", Some("/")), ("Add", Some("+")), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["e", "e_1", "e_2", "e_3", "e_4"];
    static ALT_VAR: [VarId; 12] = [0, 1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 4];
    static ALTERNATIVES: [&[Symbol]; 12] = [&[Symbol::NT(4), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(3), Symbol::NT(2), Symbol::NT(1)], &[Symbol::T(0), Symbol::NT(2), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::NT(4), Symbol::NT(3)], &[Symbol::T(1), Symbol::NT(2), Symbol::NT(3)], &[Symbol::T(2), Symbol::NT(2), Symbol::NT(3)], &[Symbol::Empty], &[Symbol::T(0), Symbol::NT(4)], &[Symbol::T(4)]];
    static PARSING_TABLE: [AltId; 30] = [0, 12, 12, 12, 0, 13, 4, 1, 2, 3, 12, 5, 6, 13, 13, 13, 6, 13, 9, 7, 8, 9, 12, 9, 10, 13, 13, 13, 11, 13];
    static OPCODES: [&[OpCode]; 12] = [&[OpCode::NT(1), OpCode::Exit(0), OpCode::NT(4)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(1), OpCode::Exit(2), OpCode::NT(2), OpCode::T(2)], &[OpCode::Loop(1), OpCode::Exit(3), OpCode::NT(2), OpCode::T(3)], &[OpCode::Loop(1), OpCode::Exit(4), OpCode::NT(2), OpCode::T(0)], &[OpCode::Exit(5)], &[OpCode::NT(3), OpCode::Exit(6), OpCode::NT(4)], &[OpCode::Loop(3), OpCode::Exit(7), OpCode::NT(2), OpCode::T(1)], &[OpCode::Loop(3), OpCode::Exit(8), OpCode::NT(2), OpCode::T(2)], &[OpCode::Exit(9)], &[OpCode::Exit(10), OpCode::NT(4), OpCode::T(0)], &[OpCode::Exit(11), OpCode::T(4)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener4]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener5 {
    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener5]

    use lexigram_lib::{AltId, VarId, alt::Alternative, fixed_sym_table::FixedSymTable, parser::{OpCode, Symbol, ll1::LLParser}};

    const PARSER_NUM_T: usize = 2;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Num", None), ("Exp", Some("^"))];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["expr", "expr_1"];
    static ALT_VAR: [VarId; 3] = [0, 1, 1];
    static ALTERNATIVES: [&[Symbol]; 3] = [&[Symbol::T(0), Symbol::NT(1)], &[Symbol::T(1), Symbol::NT(0)], &[Symbol::Empty]];
    static PARSING_TABLE: [AltId; 6] = [0, 3, 4, 3, 1, 2];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::NT(1), OpCode::T(0)], &[OpCode::Loop(0), OpCode::Exit(1), OpCode::T(1)], &[OpCode::Exit(2)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    // [write_source_code_for_integration_listener5]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener6 {
    /// User-defined type for `e`
    #[derive(Debug, PartialEq)]
    pub struct SynA();

    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener6]

    use lexigram_lib::{AltId, VarId, fixed_sym_table::FixedSymTable, parser::{OpCode, ll1::LLParser}};

    const PARSER_NUM_T: usize = 4;
    const PARSER_NUM_NT: usize = 2;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("A", None), ("Id", None), ("Comma", Some(",")), ("C", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["a", "a_1"];
    static ALT_VAR: [VarId; 3] = [0, 1, 1];
    static PARSING_TABLE: [AltId; 10] = [0, 3, 3, 3, 4, 3, 3, 1, 2, 3];
    static OPCODES: [&[OpCode]; 3] = [&[OpCode::Exit(0), OpCode::T(3), OpCode::NT(1), OpCode::T(1), OpCode::T(0)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::T(1), OpCode::T(2)], &[OpCode::Exit(2)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
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

    // [write_source_code_for_integration_listener6]
    // -------------------------------------------------------------------------
}

#[allow(unused)]
pub(crate) mod listener7 {
    /// User-defined type for `a`
    #[derive(Debug, PartialEq)]
    pub struct SynA(Vec<String>);

    /// User-defined type for `b`
    #[derive(Debug, PartialEq)]
    pub struct SynB(String);

    // -------------------------------------------------------------------------
    // [write_source_code_for_integration_listener7]

    use lexigram_lib::{AltId, LALR, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, ListenerWrapper, Terminate, lr::{LRAction::{self, Accept as LRA, Error as LRE, Reduce as LRR, Shift as LRS}, LRParser, LRStateId}}};

    static NUM_NT: usize = 3;
    static NUM_T_FULL: usize = 4;
    static ACTION: [LRAction; 32] = [
        LRS(1),LRE,LRE,LRE,LRE,LRE,LRS(4),LRE,LRE,LRS(6),LRS(4),LRE,LRE,LRE,LRE,LRA,LRE,LRR(1),LRR(1),LRE,LRE,LRR(3),LRR(3),LRE,LRE,LRE,LRE,LRR(0),LRE,LRR(2),LRR(2),LRE];
    static GOTO: [LRStateId; 9] = [
        3,8,8,8,5,2,8,7,8];
    static ALT_NT_LEN: [(VarId, u16, u16); 5] = [
        (0, 3, 2),(1, 1, 1),(2, 2, 0),(2, 1, 0),(3, 1, 0)];
    static SYMBOL_TABLE_T: [(&str, Option<&str>); 3] = [
        ("A", None),("C", None),("Id", None)];
    static SYMBOL_TABLE_NT: [&str; 4] = [
        "a","b","a_1","<goal>"];

    pub fn build_parser() -> LRParser<'static, LALR> {
        LRParser::new(
            NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,
            FixedSymTable::new(
                SYMBOL_TABLE_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),
                SYMBOL_TABLE_NT.into_iter().map(|s| s.to_string()).collect()
            ),
            false
        )
    }

    #[derive(Debug)]
    pub enum CtxA {
        /// `a -> A b+ C`
        V1 { a: String, plus: SynA1, c: String },
    }
    #[derive(Debug)]
    pub enum CtxB {
        /// `b -> Id`
        V1 { id: String },
    }

    /// Computed `b+` array in `a -> A  ►► b+ ◄◄  C`
    #[derive(Debug, PartialEq)]
    pub struct SynA1(pub Vec<SynB>);

    #[derive(Debug)]
    enum EnumSynValue { A(SynA), B(SynB), A1(SynA1) }

    impl EnumSynValue {
        fn get_a(self) -> SynA {
            if let EnumSynValue::A(val) = self { val } else { panic!() }
        }
        fn get_b(self) -> SynB {
            if let EnumSynValue::B(val) = self { val } else { panic!() }
        }
        fn get_a1(self) -> SynA1 {
            if let EnumSynValue::A1(val) = self { val } else { panic!() }
        }
    }

    pub trait TestListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_log_mut(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn handle_msg(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
            self.get_log_mut().add(msg);
        }
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, a: SynA) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn exit_a(&mut self, ctx: CtxA) -> SynA;
        fn exit_b(&mut self, ctx: CtxB) -> SynB;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<EnumSynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: TestListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_a(),                         // a -> A a_1 C
                        2 |                                         // a_1 -> a_1 b
                        3 => self.exit_a1(alt_id),                  // a_1 -> b
                        1 => self.exit_b(),                         // b -> Id
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let val = self.stack.pop().unwrap().get_a();
                            self.listener.exit(val);
                        }
                        Terminate::Abort | Terminate::Conclude => self.listener.abort(terminate),
                    }
                }
                _ => panic!("unexpected call {call:?}, nt {nt}, alt_id {alt_id}")
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

        fn get_log_mut(&mut self) -> &mut impl Logger {
            self.listener.get_log_mut()
        }

        fn report(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
            self.listener.handle_msg(span_opt, msg);
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

    impl<T: TestListener> Wrapper<T> {
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

        fn exit_a(&mut self) {
            let c = self.stack_t.pop().unwrap();
            let plus = self.stack.pop().unwrap().get_a1();
            let a = self.stack_t.pop().unwrap();
            let ctx = CtxA::V1 { a, plus, c };
            let val = self.listener.exit_a(ctx);
            self.stack.push(EnumSynValue::A(val));
        }

        fn init_a1(&mut self) {
            let val = SynA1(Vec::new());
            self.stack.push(EnumSynValue::A1(val));
        }

        fn exit_a1(&mut self, alt_id: AltId) {
            let b = self.stack.pop().unwrap().get_b();
            if matches!(alt_id, 3) { self.init_a1(); }
            let Some(EnumSynValue::A1(SynA1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynA1 item on wrapper stack");
            };
            plus_acc.push(b);
        }

        fn exit_b(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxB::V1 { id };
            let val = self.listener.exit_b(ctx);
            self.stack.push(EnumSynValue::B(val));
        }
    }

    // [write_source_code_for_integration_listener7]
    // -------------------------------------------------------------------------

    mod test {
        use lexigram_core::CollectJoin;
        use lexigram_core::log::BufLog;
        use lexigram_lib::make_stream;
        use super::*;

        struct Listener {
            log: BufLog,
            list: Option<Vec<String>>,
        }

        #[allow(unused)]
        impl TestListener for Listener {
            fn get_log_mut(&mut self) -> &mut impl Logger {
                &mut self.log
            }

            fn exit(&mut self, a: SynA) {
                let SynA(ids) = a;
                self.list = Some(ids);
            }

            fn exit_a(&mut self, ctx: CtxA) -> SynA {
                // a -> A b+ C
                let CtxA::V1 { a, plus: SynA1(ids), c } = ctx;
                SynA(ids.into_iter().map(|SynB(s)| s).collect())
            }

            fn exit_b(&mut self, ctx: CtxB) -> SynB {
                // b -> Id
                let CtxB::V1 { id } = ctx;
                SynB(id)
            }
        }

        impl Listener {
            fn new() -> Self {
                Listener { log: BufLog::new(), list: None }
            }
        }

        #[test]
        fn test() {
            const VERBOSE: bool = false;
            let sequences = vec![
                ("A x y z C", false, Some(vec!["x", "y", "z"])),
                ("x", true, None),
            ];
            let mut parser = build_parser();
            for (input, expected_error, expected_list) in sequences {
                if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
                let stream = make_stream(input, SYMBOL_TABLE_T, true, 2, 999, VERBOSE);
                let mut listener = Listener::new();
                let mut wrapper = Wrapper::new(listener, VERBOSE);
                let is_error = match parser.parse_stream(&mut wrapper, stream) {
                    Ok(_) => {
                        if VERBOSE { println!("parsing completed successfully"); }
                        false
                    }
                    Err(e) => {
                        if VERBOSE { println!("parsing failed: {e}"); }
                        true
                    }
                };
                let result = &wrapper.get_listener().list;
                if VERBOSE { println!("list = {result:?}"); }
                assert_eq!(is_error, expected_error, "parser error with input {input}");
                let expected_list = expected_list.map(|maybe| maybe.into_iter().map(|s| s.to_string()).to_vec());
                assert_eq!(result, &expected_list, "")
            }
        }
    }

}