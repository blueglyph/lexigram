// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

//==============================================================================

pub mod listener_id_type_types {
    /// User-defined type for `program`
    #[derive(Debug, PartialEq)] pub struct SynProgram();
    /// User-defined type for `stmt`
    #[derive(Debug, PartialEq)] pub struct SynStmt();
    /// User-defined type for `decl`
    #[derive(Debug, PartialEq)] pub struct SynDecl();
    /// User-defined type for `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
    #[derive(Debug, PartialEq)] pub struct SynIdI();
    /// User-defined type for `inst`
    #[derive(Debug, PartialEq)] pub struct SynInst();
    /// User-defined type for `expr`
    #[derive(Debug, PartialEq)] pub struct SynExpr();
}

pub mod typedef_id_type_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_id_type_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 22;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 4;
    const NBR_STATES: StateId = 26;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         13,  13,  13,  13,  13,  13,  13,  13,  13,   0,  21,  13,  13,  21,  13,  13,   // 0-15
         13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,   // 16-31
          0,  13,  13,  13,  13,  13,  13,  13,  13,  13,  11,   1,   2,   3,  13,   4,   // 32-47
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  13,   6,  13,   7,  13,  13,   // 48-63
         13,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   // 64-79
          8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,  13,  13,  13,  13,  12,   // 80-95
         13,   8,   8,   8,  17,  16,  18,   8,   8,  19,   8,   8,   8,   8,  20,   8,   // 96-111
          9,   8,  14,   8,  10,   8,   8,   8,   8,  15,   8,  13,  13,  13,  13,  13,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 13),
        (Seg(57344, 1114111), 13),
    ];
    static TERMINAL_TABLE: [Terminal;22] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 573] = [
          4,   5,   6,   7,   1,   8,   9,  10,  11,  12,  13,  26,  26,  26,  11,  11,  11,  11,  11,  11,  11,   4, // state 0
         26,  26,  26,  26,  24,  26,  26,  26,  26,  26,  26,   2,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 1
          2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   3,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 2
          2,   2,   2,   2,  25,   2,   2,   2,   2,   2,   2,   3,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 3
          4,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,   4, // state 4 <skip>
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 5 <end:4>
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 6 <end:0>
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 7 <end:3>
         26,  26,  26,  26,  26,   8,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 8 <end:7>
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 9 <end:1>
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 10 <end:2>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  11,  11,  11,  11,  26, // state 11 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  20,  11,  11,  11,  11,  11,  11,  26, // state 12 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  14,  11,  11,  11,  11,  11,  26, // state 13 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  15,  11,  26,  11,  26,  11,  11,  11,  11,  11,  11,  11,  26, // state 14 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  16,  11,  11,  11,  11,  26, // state 15 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  17,  11,  11,  11,  26, // state 16 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  18,  11,  11,  11,  11,  26, // state 17 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  11,  19,  11,  11,  26, // state 18 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  11,  11,  11,  11,  26, // state 19 <end:5>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  11,  11,  21,  11,  26, // state 20 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  11,  11,  11,  22,  26, // state 21 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  23,  26,  11,  26,  11,  11,  11,  11,  11,  11,  11,  26, // state 22 <end:8>
         26,  26,  26,  26,  26,  11,  26,  26,  11,  11,  11,  26,  11,  26,  11,  11,  11,  11,  11,  11,  11,  26, // state 23 <end:6>
         24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  26, // state 24 <skip>
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26, // state 25 <skip>
         26 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [typedef_id_type_lexer]
}

pub mod typedef_id_type_parser {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_id_type_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_id_type_types::*;

    const PARSER_NUM_T: usize = 10;
    const PARSER_NUM_NT: usize = 9;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Comma", Some(",")), ("SemiColon", Some(";")), ("Eq", Some("=")), ("Sub", Some("-")), ("Add", Some("+")), ("Typedef", Some("typedef")), ("Print", Some("print")), ("Num", None), ("Id", None), ("Type", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "stmt_i", "stmt", "decl", "id_i", "inst", "expr", "expr_1", "expr_2"];
    static ALT_VAR: [VarId; 18] = [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 7, 8, 8, 8];
    static PARSING_TABLE: [AltId; 99] = [18, 18, 18, 18, 18, 0, 0, 18, 0, 0, 0, 18, 18, 18, 18, 18, 1, 1, 18, 1, 1, 2, 18, 18, 18, 18, 18, 3, 4, 18, 4, 3, 19, 18, 18, 18, 18, 18, 6, 19, 18, 19, 5, 19, 7, 8, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 10, 18, 9, 19, 19, 18, 19, 18, 11, 18, 18, 18, 11, 11, 18, 18, 18, 14, 18, 13, 12, 18, 18, 18, 18, 18, 18, 18, 19, 18, 15, 19, 18, 18, 17, 16, 18, 18];
    static OPCODES: [&[OpCode]; 18] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Hook, OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(1), OpCode::NT(4), OpCode::T(8), OpCode::Hook, OpCode::T(9)], &[OpCode::Exit(6), OpCode::T(1), OpCode::T(8), OpCode::Hook, OpCode::T(9), OpCode::Hook, OpCode::T(5)], &[OpCode::Loop(4), OpCode::Exit(7), OpCode::T(8), OpCode::Hook, OpCode::T(0)], &[OpCode::Exit(8)], &[OpCode::Exit(9), OpCode::T(1), OpCode::NT(6), OpCode::Hook, OpCode::T(2), OpCode::T(8)], &[OpCode::Exit(10), OpCode::T(1), OpCode::NT(6), OpCode::Hook, OpCode::T(6)], &[OpCode::NT(7), OpCode::Exit(11), OpCode::NT(8)], &[OpCode::Loop(7), OpCode::Exit(12), OpCode::NT(8), OpCode::Hook, OpCode::T(4)], &[OpCode::Loop(7), OpCode::Exit(13), OpCode::NT(8), OpCode::Hook, OpCode::T(3)], &[OpCode::Exit(14)], &[OpCode::Exit(15), OpCode::NT(8), OpCode::Hook, OpCode::T(3)], &[OpCode::Exit(16), OpCode::T(8)], &[OpCode::Exit(17), OpCode::T(7)]];
    static INIT_OPCODES: [OpCode; 3] = [OpCode::End, OpCode::NT(0), OpCode::Hook];
    static START_SYMBOL: VarId = 0;


    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum Term {
        #[doc = "','"]        Comma = 0,
        #[doc = "';'"]        SemiColon = 1,
        #[doc = "'='"]        Eq = 2,
        #[doc = "'-'"]        Sub = 3,
        #[doc = "'+'"]        Add = 4,
        #[doc = "'typedef'"]  Typedef = 5,
        #[doc = "'print'"]    Print = 6,
        #[doc = "(variable)"] Num = 7,
        #[doc = "(variable)"] Id = 8,
        #[doc = "(variable)"] Type = 9,
    }

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum NTerm {
        #[doc = "`program`"]                   Program = 0,
        #[doc = "`stmt_i`, parent: `program`"] StmtI = 1,
        #[doc = "`stmt`"]                      Stmt = 2,
        #[doc = "`decl`"]                      Decl = 3,
        #[doc = "`id_i`, parent: `decl`"]      IdI = 4,
        #[doc = "`inst`"]                      Inst = 5,
        #[doc = "`expr`"]                      Expr = 6,
        #[doc = "`expr_1`, parent: `expr`"]    Expr1 = 7,
        #[doc = "`expr_2`, parent: `expr`"]    Expr2 = 8,
    }

        pub fn get_term_name(t: TokenId) -> (&'static str, Option<&'static str>) {
            SYMBOLS_T[t as usize]
        }

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
        /// `program -> (<L> stmt)*`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxStmtI {
        /// `<L> stmt` iteration in `program -> ( ►► <L> stmt ◄◄ )*`
        V1 { stmt: SynStmt },
    }
    #[derive(Debug)]
    pub enum CtxStmt {
        /// `stmt -> decl`
        V1 { decl: SynDecl },
        /// `stmt -> inst`
        V2 { inst: SynInst },
    }
    #[derive(Debug)]
    pub enum CtxDecl {
        /// `decl -> Type Id (<L> "," Id)* ";"`
        V1 { type1: String, id: String, star: SynIdI },
        /// `decl -> "typedef" Type Id ";"`
        V2 { type1: String, id: String },
    }
    #[derive(Debug)]
    pub enum CtxIdI {
        /// `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
        V1 { star_acc: SynIdI, id: String },
    }
    #[derive(Debug)]
    pub enum CtxInst {
        /// `inst -> Id "=" expr ";"`
        V1 { id: String, expr: SynExpr },
        /// `inst -> "print" expr ";"`
        V2 { expr: SynExpr },
    }
    #[derive(Debug)]
    pub enum CtxExpr {
        /// `expr -> "-" expr`
        V1 { expr: SynExpr },
        /// `expr -> expr "+" expr`
        V2 { expr: [SynExpr; 2] },
        /// `expr -> expr <P> "-" expr`
        V3 { expr: [SynExpr; 2] },
        /// `expr -> Id`
        V4 { id: String },
        /// `expr -> Num`
        V5 { num: String },
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `program`
    // #[derive(Debug, PartialEq)] pub struct SynProgram();
    // /// User-defined type for `stmt`
    // #[derive(Debug, PartialEq)] pub struct SynStmt();
    // /// User-defined type for `decl`
    // #[derive(Debug, PartialEq)] pub struct SynDecl();
    // /// User-defined type for `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
    // #[derive(Debug, PartialEq)] pub struct SynIdI();
    // /// User-defined type for `inst`
    // #[derive(Debug, PartialEq)] pub struct SynInst();
    // /// User-defined type for `expr`
    // #[derive(Debug, PartialEq)] pub struct SynExpr();

    #[derive(Debug)]
    enum SynValue { Program(SynProgram), Stmt(SynStmt), Decl(SynDecl), IdI(SynIdI), Inst(SynInst), Expr(SynExpr) }

    impl SynValue {
        fn get_program(self) -> SynProgram {
            if let SynValue::Program(val) = self { val } else { panic!() }
        }
        fn get_stmt(self) -> SynStmt {
            if let SynValue::Stmt(val) = self { val } else { panic!() }
        }
        fn get_decl(self) -> SynDecl {
            if let SynValue::Decl(val) = self { val } else { panic!() }
        }
        fn get_id_i(self) -> SynIdI {
            if let SynValue::IdI(val) = self { val } else { panic!() }
        }
        fn get_inst(self) -> SynInst {
            if let SynValue::Inst(val) = self { val } else { panic!() }
        }
        fn get_expr(self) -> SynExpr {
            if let SynValue::Expr(val) = self { val } else { panic!() }
        }
    }

    pub trait TypedefListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused)]
        fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused)]
        fn exit(&mut self, program: SynProgram, span: PosSpan) {}
        fn init_program(&mut self) {}
        fn exit_program(&mut self, ctx: CtxProgram, spans: Vec<PosSpan>) -> SynProgram;
        fn init_stmt_i(&mut self) {}
        #[allow(unused)]
        fn exit_stmt_i(&mut self, ctx: CtxStmtI, spans: Vec<PosSpan>) {}
        fn init_stmt(&mut self) {}
        fn exit_stmt(&mut self, ctx: CtxStmt, spans: Vec<PosSpan>) -> SynStmt;
        fn init_decl(&mut self) {}
        fn exit_decl(&mut self, ctx: CtxDecl, spans: Vec<PosSpan>) -> SynDecl;
        fn init_id_i(&mut self) -> SynIdI;
        fn exit_id_i(&mut self, ctx: CtxIdI, spans: Vec<PosSpan>) -> SynIdI;
        #[allow(unused)]
        fn exitloop_id_i(&mut self, star_acc: &mut SynIdI) {}
        fn init_inst(&mut self) {}
        fn exit_inst(&mut self, ctx: CtxInst, spans: Vec<PosSpan>) -> SynInst;
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

    impl<T: TypedefListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    if matches!(nt, 1 | 4) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_program(),          // program
                        1 => self.listener.init_stmt_i(),           // stmt_i
                        2 => self.listener.init_stmt(),             // stmt
                        3 => self.listener.init_decl(),             // decl
                        4 => self.init_id_i(),                      // id_i
                        5 => self.listener.init_inst(),             // inst
                        6 => self.listener.init_expr(),             // expr
                        7 | 8 => {}                                 // expr_1, expr_2
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_program(),                   // program -> stmt_i
                        1 => self.exit_stmt_i(),                    // stmt_i -> <L> stmt stmt_i
                        2 => {}                                     // stmt_i -> <L> ε (not used)
                        3 |                                         // stmt -> decl
                        4 => self.exit_stmt(alt_id),                // stmt -> inst
                        5 |                                         // decl -> Type Id id_i ";"
                        6 => self.exit_decl(alt_id),                // decl -> "typedef" Type Id ";"
                        7 => self.exit_id_i(),                      // id_i -> <L> "," Id id_i
                        8 => self.exitloop_id_i(),                  // id_i -> <L> ε
                        9 |                                         // inst -> Id "=" expr ";"
                        10 => self.exit_inst(alt_id),               // inst -> "print" expr ";"
                        12 |                                        // expr_1 -> "+" expr_2 expr_1
                        13 => self.exit_expr1(alt_id),              // expr_1 -> "-" expr_2 expr_1
                        15 |                                        // expr_2 -> "-" expr_2
                        16 |                                        // expr_2 -> Id
                        17 => self.exit_expr2(alt_id),              // expr_2 -> Num
                        11 => {}                                    // expr -> expr_2 expr_1 (not used)
                        14 => {}                                    // expr_1 -> ε (not used)
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

        fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
            self.listener.hook(token, text, span)
        }
    }

    impl<T: TypedefListener> Wrapper<T> {
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

        fn exit(&mut self) {
            let program = self.stack.pop().unwrap().get_program();
            let span = self.stack_span.pop().unwrap();
            self.listener.exit(program, span);
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

        fn exit_stmt_i(&mut self) {
            let stmt = self.stack.pop().unwrap().get_stmt();
            let ctx = CtxStmtI::V1 { stmt };
            let n = 2;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_stmt_i(ctx, spans);
        }

        fn exit_stmt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                3 => {
                    let decl = self.stack.pop().unwrap().get_decl();
                    (1, CtxStmt::V1 { decl })
                }
                4 => {
                    let inst = self.stack.pop().unwrap().get_inst();
                    (1, CtxStmt::V2 { inst })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_stmt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_stmt(ctx, spans);
            self.stack.push(SynValue::Stmt(val));
        }

        fn exit_decl(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                5 => {
                    let star = self.stack.pop().unwrap().get_id_i();
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    (4, CtxDecl::V1 { type1, id, star })
                }
                6 => {
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    (4, CtxDecl::V2 { type1, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_decl")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_decl(ctx, spans);
            self.stack.push(SynValue::Decl(val));
        }

        fn init_id_i(&mut self) {
            let val = self.listener.init_id_i();
            self.stack.push(SynValue::IdI(val));
        }

        fn exit_id_i(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let star_acc = self.stack.pop().unwrap().get_id_i();
            let ctx = CtxIdI::V1 { star_acc, id };
            let n = 3;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_id_i(ctx, spans);
            self.stack.push(SynValue::IdI(val));
        }

        fn exitloop_id_i(&mut self) {
            let SynValue::IdI(star_acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_id_i(star_acc);
        }

        fn exit_inst(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                9 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxInst::V1 { id, expr })
                }
                10 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    (3, CtxInst::V2 { expr })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_inst")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_inst(ctx, spans);
            self.stack.push(SynValue::Inst(val));
        }

        fn exit_expr1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                12 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    (3, CtxExpr::V2 { expr: [expr_1, expr_2] })
                }
                13 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    (3, CtxExpr::V3 { expr: [expr_1, expr_2] })
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

        fn exit_expr2(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                15 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    (2, CtxExpr::V1 { expr })
                }
                16 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxExpr::V4 { id })
                }
                17 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxExpr::V5 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr2")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_expr(ctx, spans);
            self.stack.push(SynValue::Expr(val));
        }
    }

    // [typedef_id_type_parser]
}
