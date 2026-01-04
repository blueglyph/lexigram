pub mod typedef_type_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_type_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 18;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 1;
    const NBR_STATES: StateId = 23;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,   // 0-15
         18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,   // 16-31
         18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,   0,   1,   2,  18,  18,   // 32-47
          3,   3,   3,   3,   3,   3,   3,   3,   3,   3,  18,   4,  18,   5,  18,  18,   // 48-63
         18,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   // 64-79
          6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  18,  18,  18,  18,  10,   // 80-95
         18,   6,   6,   6,  14,  11,  15,   6,   6,  16,   6,   6,   7,   6,  17,   6,   // 96-111
          8,   6,  12,   6,   9,   6,   6,   6,   6,  13,   6,  18,  18,  18,  18,  18,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 0] = [
    ];
    static TERMINAL_TABLE: [Terminal;22] = [
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 415] = [
          1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  23,   7,   7,   7,   7,   7,   7,   7, // state 0
         23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23, // state 1 <end:4>
         23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23, // state 2 <end:0>
         23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23, // state 3 <end:3>
         23,  23,  23,   4,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23, // state 4 <end:8>
         23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23, // state 5 <end:1>
         23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23, // state 6 <end:2>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 7 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,  17,   7,   7,   7,   7,   7,   7, // state 8 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,  19,   7,   7,   7,   7,   7, // state 9 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,  11,   7,   7,   7,   7, // state 10 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,  12,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 11 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,  13,   7,   7,   7,   7,   7,   7, // state 12 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,  14,   7,   7,   7, // state 13 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,  15,   7,   7,   7,   7,   7,   7, // state 14 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,  16,   7,   7, // state 15 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 16 <end:5>
         23,  23,  23,   7,  23,  23,   7,   7,   7,  18,   7,   7,   7,   7,   7,   7,   7,   7, // state 17 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 18 <end:6>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,  20,   7, // state 19 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,  21, // state 20 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,  22,   7,   7,   7,   7,   7,   7,   7,   7, // state 21 <end:9>
         23,  23,  23,   7,  23,  23,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 22 <end:7>
         23 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [typedef_type_lexer]
}

pub mod listener_type_types {
    /// User-defined type for `program`
    #[derive(Debug, PartialEq)] pub struct SynProgram();
    /// User-defined type for `stmt`
    #[derive(Debug, PartialEq)] pub struct SynStmt();
    /// User-defined type for `decl`
    #[derive(Debug, PartialEq)] pub struct SynDecl();
    /// User-defined type for `inst`
    #[derive(Debug, PartialEq)] pub struct SynInst();
    /// User-defined type for `expr`
    #[derive(Debug, PartialEq)] pub struct SynExpr();
}

pub mod typedef_type_parser {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_type_parser]

    use lexigram_core::{AltId, VarId, fixed_sym_table::FixedSymTable, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_type_types::*;

    const PARSER_NUM_T: usize = 11;
    const PARSER_NUM_NT: usize = 10;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Comma", Some(",")), ("SemiColon", Some(";")), ("Eq", Some("=")), ("Sub", Some("-")), ("Add", Some("+")), ("Typedef", Some("typedef")), ("Let", Some("let")), ("Print", Some("print")), ("Num", None), ("Id", None), ("Type", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "decl_i", "inst_i", "decl", "inst", "expr", "decl_1", "expr_1", "expr_2", "inst_i_1"];
    static ALT_VAR: [VarId; 19] = [0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9];
    static PARSING_TABLE: [AltId; 120] = [19, 19, 19, 19, 19, 0, 0, 0, 19, 19, 0, 20, 19, 19, 19, 19, 19, 1, 2, 2, 19, 19, 1, 19, 19, 19, 19, 19, 19, 19, 3, 3, 19, 19, 19, 20, 19, 19, 19, 19, 19, 5, 20, 20, 19, 19, 4, 19, 19, 19, 19, 19, 19, 19, 6, 7, 19, 19, 19, 20, 19, 20, 19, 8, 19, 19, 19, 19, 8, 8, 19, 19, 9, 10, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 13, 19, 12, 11, 19, 19, 19, 19, 19, 19, 19, 19, 20, 19, 14, 20, 19, 19, 19, 16, 15, 19, 19, 19, 19, 19, 19, 19, 19, 17, 17, 19, 19, 19, 18];
    static OPCODES: [&[OpCode]; 19] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Hook, OpCode::Exit(1), OpCode::NT(3)], &[OpCode::Exit(2)], &[OpCode::NT(9), OpCode::NT(4)], &[OpCode::Exit(4), OpCode::T(1), OpCode::NT(6), OpCode::T(9), OpCode::T(10)], &[OpCode::Exit(5), OpCode::T(1), OpCode::T(9), OpCode::T(10), OpCode::Hook, OpCode::T(5)], &[OpCode::Exit(6), OpCode::T(1), OpCode::NT(5), OpCode::T(2), OpCode::T(9), OpCode::T(6)], &[OpCode::Exit(7), OpCode::T(1), OpCode::NT(5), OpCode::T(7)], &[OpCode::NT(7), OpCode::Exit(8), OpCode::NT(8)], &[OpCode::Loop(6), OpCode::Exit(9), OpCode::T(9), OpCode::T(0)], &[OpCode::Exit(10)], &[OpCode::Loop(7), OpCode::Exit(11), OpCode::NT(8), OpCode::T(4)], &[OpCode::Loop(7), OpCode::Exit(12), OpCode::NT(8), OpCode::T(3)], &[OpCode::Exit(13)], &[OpCode::Exit(14), OpCode::NT(8), OpCode::T(3)], &[OpCode::Exit(15), OpCode::T(9)], &[OpCode::Exit(16), OpCode::T(8)], &[OpCode::Loop(2), OpCode::Exit(17)], &[OpCode::Exit(18)]];
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
    pub enum CtxProgram {
        /// `program -> (<L> decl)* (<L> inst)+`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxDeclI {
        /// `<L> decl` iteration in `program -> ( ►► <L> decl ◄◄ )* (<L> inst)+`
        V1 { decl: SynDecl },
    }
    #[derive(Debug)]
    pub enum CtxInstI {
        /// `<L> inst` iteration in `program -> (<L> decl)* ( ►► <L> inst ◄◄ )+`
        V1 { inst: SynInst, last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxDecl {
        /// `decl -> Type Id ("," Id)* ";"`
        V1 { type1: String, id: String, star: SynDecl1 },
        /// `decl -> "typedef" Type Id ";"`
        V2 { type1: String, id: String },
    }
    #[derive(Debug)]
    pub enum CtxInst {
        /// `inst -> "let" Id "=" expr ";"`
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
    // /// User-defined type for `decl`
    // #[derive(Debug, PartialEq)] pub struct SynDecl();
    // /// User-defined type for `inst`
    // #[derive(Debug, PartialEq)] pub struct SynInst();
    // /// User-defined type for `expr`
    // #[derive(Debug, PartialEq)] pub struct SynExpr();
    /// Computed `("," Id)*` array in `decl -> Type Id  ►► ("," Id)* ◄◄  ";" | "typedef" Type Id ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynDecl1(pub Vec<String>);

    #[derive(Debug)]
    enum SynValue { Program(SynProgram), Decl(SynDecl), Inst(SynInst), Expr(SynExpr), Decl1(SynDecl1) }

    impl SynValue {
        fn get_program(self) -> SynProgram {
            if let SynValue::Program(val) = self { val } else { panic!() }
        }
        fn get_decl(self) -> SynDecl {
            if let SynValue::Decl(val) = self { val } else { panic!() }
        }
        fn get_inst(self) -> SynInst {
            if let SynValue::Inst(val) = self { val } else { panic!() }
        }
        fn get_expr(self) -> SynExpr {
            if let SynValue::Expr(val) = self { val } else { panic!() }
        }
        fn get_decl1(self) -> SynDecl1 {
            if let SynValue::Decl1(val) = self { val } else { panic!() }
        }
    }

    pub trait TypedefListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused)]
        fn exit(&mut self, program: SynProgram) {}
        fn init_program(&mut self) {}
        fn exit_program(&mut self, ctx: CtxProgram) -> SynProgram;
        fn init_decl_i(&mut self) {}
        #[allow(unused)]
        fn exit_decl_i(&mut self, ctx: CtxDeclI) {}
        fn init_inst_i(&mut self) {}
        #[allow(unused)]
        fn exit_inst_i(&mut self, ctx: CtxInstI) {}
        fn init_decl(&mut self) {}
        fn exit_decl(&mut self, ctx: CtxDecl) -> SynDecl;
        fn init_inst(&mut self) {}
        fn exit_inst(&mut self, ctx: CtxInst) -> SynInst;
        fn init_expr(&mut self) {}
        fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
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
                    match nt {
                        0 => self.listener.init_program(),          // program
                        1 => self.listener.init_decl_i(),           // decl_i
                        2 => self.listener.init_inst_i(),           // inst_i
                        9 => {}                                     // inst_i_1
                        3 => self.listener.init_decl(),             // decl
                        6 => self.init_decl1(),                     // decl_1
                        4 => self.listener.init_inst(),             // inst
                        5 => self.listener.init_expr(),             // expr
                        7 | 8 => {}                                 // expr_1, expr_2
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_program(),                   // program -> decl_i inst_i
                        1 => self.exit_decl_i(),                    // decl_i -> <L> decl decl_i
                        17 |                                        // inst_i_1 -> inst_i
                        18 => self.exit_inst_i(alt_id),             // inst_i_1 -> ε
                        2 => {}                                     // decl_i -> <L> ε (not used)
                     /* 3 */                                        // inst_i -> <L> inst inst_i_1 (never called)
                        4 |                                         // decl -> Type Id decl_1 ";"
                        5 => self.exit_decl(alt_id),                // decl -> "typedef" Type Id ";"
                        9 => self.exit_decl1(),                     // decl_1 -> "," Id decl_1
                        10 => {}                                    // decl_1 -> ε
                        6 |                                         // inst -> "let" Id "=" expr ";"
                        7 => self.exit_inst(alt_id),                // inst -> "print" expr ";"
                        11 |                                        // expr_1 -> "+" expr_2 expr_1
                        12 => self.exit_expr1(alt_id),              // expr_1 -> "-" expr_2 expr_1
                        14 |                                        // expr_2 -> "-" expr_2
                        15 |                                        // expr_2 -> Id
                        16 => self.exit_expr2(alt_id),              // expr_2 -> Num
                        8 => {}                                     // expr -> expr_2 expr_1 (not used)
                        13 => {}                                    // expr_1 -> ε (not used)
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

    impl<T: TypedefListener> Wrapper<T> {
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
            let program = self.stack.pop().unwrap().get_program();
            self.listener.exit(program);
        }

        fn exit_program(&mut self) {
            let ctx = CtxProgram::V1;
            let val = self.listener.exit_program(ctx);
            self.stack.push(SynValue::Program(val));
        }

        fn exit_decl_i(&mut self) {
            let decl = self.stack.pop().unwrap().get_decl();
            let ctx = CtxDeclI::V1 { decl };
            self.listener.exit_decl_i(ctx);
        }

        fn exit_inst_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 18;
            let inst = self.stack.pop().unwrap().get_inst();
            let ctx = CtxInstI::V1 { inst, last_iteration };
            self.listener.exit_inst_i(ctx);
        }

        fn exit_decl(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                4 => {
                    let star = self.stack.pop().unwrap().get_decl1();
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    CtxDecl::V1 { type1, id, star }
                }
                5 => {
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    CtxDecl::V2 { type1, id }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_decl")
            };
            let val = self.listener.exit_decl(ctx);
            self.stack.push(SynValue::Decl(val));
        }

        fn init_decl1(&mut self) {
            let val = SynDecl1(Vec::new());
            self.stack.push(SynValue::Decl1(val));
        }

        fn exit_decl1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let Some(SynValue::Decl1(SynDecl1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynDecl1 item on wrapper stack");
            };
            star_acc.push(id);
        }

        fn exit_inst(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                6 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    CtxInst::V1 { id, expr }
                }
                7 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInst::V2 { expr }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_inst")
            };
            let val = self.listener.exit_inst(ctx);
            self.stack.push(SynValue::Inst(val));
        }

        fn exit_expr1(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                11 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V2 { expr: [expr_1, expr_2] }
                }
                12 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V3 { expr: [expr_1, expr_2] }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr1")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }

        fn exit_expr2(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                14 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V1 { expr }
                }
                15 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxExpr::V4 { id }
                }
                16 => {
                    let num = self.stack_t.pop().unwrap();
                    CtxExpr::V5 { num }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr2")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }
    }

    // [typedef_type_parser]
}

pub mod typedef_id_type_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_id_type_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 17;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 1;
    const NBR_STATES: StateId = 20;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,   // 0-15
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,   // 16-31
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  17,   0,   1,   2,  17,  17,   // 32-47
          3,   3,   3,   3,   3,   3,   3,   3,   3,   3,  17,   4,  17,   5,  17,  17,   // 48-63
         17,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   // 64-79
          6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  17,  17,  17,  17,   9,   // 80-95
         17,   6,   6,   6,  13,  12,  14,   6,   6,  15,   6,   6,   6,   6,  16,   6,   // 96-111
          7,   6,  10,   6,   8,   6,   6,   6,   6,  11,   6,  17,  17,  17,  17,  17,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 0] = [
    ];
    static TERMINAL_TABLE: [Terminal;19] = [
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
    ];
    static STATE_TABLE: [StateId; 341] = [
          1,   2,   3,   4,   5,   6,   7,   8,   9,  20,   7,   7,   7,   7,   7,   7,   7, // state 0
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20, // state 1 <end:4>
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20, // state 2 <end:0>
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20, // state 3 <end:3>
         20,  20,  20,   4,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20, // state 4 <end:7>
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20, // state 5 <end:1>
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20, // state 6 <end:2>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 7 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,  16,   7,   7,   7,   7,   7,   7, // state 8 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,  10,   7,   7,   7,   7,   7, // state 9 <end:8>
         20,  20,  20,   7,  20,  20,   7,  11,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 10 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,  12,   7,   7,   7,   7, // state 11 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,  13,   7,   7,   7, // state 12 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,  14,   7,   7,   7,   7, // state 13 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,   7,  15,   7,   7, // state 14 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 15 <end:5>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,   7,   7,  17,   7, // state 16 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,  18, // state 17 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,  19,   7,   7,   7,   7,   7,   7,   7,   7, // state 18 <end:8>
         20,  20,  20,   7,  20,  20,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,   7, // state 19 <end:6>
         20 // error group in [nbr_state * nbr_group + nbr_group]
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

pub mod listener_id_type_types {
    /// User-defined type for `program`
    #[derive(Debug, PartialEq)] pub struct SynProgram();
    /// User-defined type for `decl`
    #[derive(Debug, PartialEq)] pub struct SynDecl();
    /// User-defined type for `inst`
    #[derive(Debug, PartialEq)] pub struct SynInst();
    /// User-defined type for `expr`
    #[derive(Debug, PartialEq)] pub struct SynExpr();
}

pub mod typedef_id_type_parser {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_id_type_parser]

    use lexigram_core::{AltId, VarId, fixed_sym_table::FixedSymTable, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_id_type_types::*;

    const PARSER_NUM_T: usize = 10;
    const PARSER_NUM_NT: usize = 9;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Comma", Some(",")), ("SemiColon", Some(";")), ("Eq", Some("=")), ("Sub", Some("-")), ("Add", Some("+")), ("Typedef", Some("typedef")), ("Print", Some("print")), ("Num", None), ("Id", None), ("Type", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "stmt_i", "stmt", "decl", "inst", "expr", "decl_1", "expr_1", "expr_2"];
    static ALT_VAR: [VarId; 18] = [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 6, 6, 7, 7, 7, 8, 8, 8];
    static PARSING_TABLE: [AltId; 99] = [18, 18, 18, 18, 18, 0, 0, 18, 0, 0, 0, 18, 18, 18, 18, 18, 1, 1, 18, 1, 1, 2, 18, 18, 18, 18, 18, 3, 4, 18, 4, 3, 19, 18, 18, 18, 18, 18, 6, 19, 18, 19, 5, 19, 18, 18, 18, 18, 18, 19, 8, 18, 7, 19, 19, 18, 19, 18, 9, 18, 18, 18, 9, 9, 18, 18, 10, 11, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 14, 18, 13, 12, 18, 18, 18, 18, 18, 18, 18, 19, 18, 15, 19, 18, 18, 17, 16, 18, 18];
    static OPCODES: [&[OpCode]; 18] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Hook, OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(4)], &[OpCode::Exit(5), OpCode::T(1), OpCode::NT(6), OpCode::T(8), OpCode::Hook, OpCode::T(9)], &[OpCode::Exit(6), OpCode::T(1), OpCode::T(8), OpCode::Hook, OpCode::T(9), OpCode::Hook, OpCode::T(5)], &[OpCode::Exit(7), OpCode::T(1), OpCode::NT(5), OpCode::Hook, OpCode::T(2), OpCode::T(8)], &[OpCode::Exit(8), OpCode::T(1), OpCode::NT(5), OpCode::Hook, OpCode::T(6)], &[OpCode::NT(7), OpCode::Exit(9), OpCode::NT(8)], &[OpCode::Loop(6), OpCode::Exit(10), OpCode::T(8), OpCode::Hook, OpCode::T(0)], &[OpCode::Exit(11)], &[OpCode::Loop(7), OpCode::Exit(12), OpCode::NT(8), OpCode::Hook, OpCode::T(4)], &[OpCode::Loop(7), OpCode::Exit(13), OpCode::NT(8), OpCode::Hook, OpCode::T(3)], &[OpCode::Exit(14)], &[OpCode::Exit(15), OpCode::NT(8), OpCode::Hook, OpCode::T(3)], &[OpCode::Exit(16), OpCode::T(8)], &[OpCode::Exit(17), OpCode::T(7)]];
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
        /// `decl -> Type Id ("," Id)* ";"`
        V1 { type1: String, id: String, star: SynDecl1 },
        /// `decl -> "typedef" Type Id ";"`
        V2 { type1: String, id: String },
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
    // /// User-defined type for `inst`
    // #[derive(Debug, PartialEq)] pub struct SynInst();
    // /// User-defined type for `expr`
    // #[derive(Debug, PartialEq)] pub struct SynExpr();
    /// Computed `("," Id)*` array in `decl -> Type Id  ►► ("," Id)* ◄◄  ";" | "typedef" Type Id ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynDecl1(pub Vec<String>);

    #[derive(Debug)]
    enum SynValue { Program(SynProgram), Stmt(SynStmt), Decl(SynDecl), Inst(SynInst), Expr(SynExpr), Decl1(SynDecl1) }

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
        fn get_inst(self) -> SynInst {
            if let SynValue::Inst(val) = self { val } else { panic!() }
        }
        fn get_expr(self) -> SynExpr {
            if let SynValue::Expr(val) = self { val } else { panic!() }
        }
        fn get_decl1(self) -> SynDecl1 {
            if let SynValue::Decl1(val) = self { val } else { panic!() }
        }
    }

    pub trait TypedefListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused)]
        fn exit(&mut self, program: SynProgram) {}
        fn init_program(&mut self) {}
        fn exit_program(&mut self, ctx: CtxProgram) -> SynProgram;
        fn init_stmt_i(&mut self) {}
        #[allow(unused)]
        fn exit_stmt_i(&mut self, ctx: CtxStmtI) {}
        fn init_stmt(&mut self) {}
        fn exit_stmt(&mut self, ctx: CtxStmt) -> SynStmt;
        fn init_decl(&mut self) {}
        fn exit_decl(&mut self, ctx: CtxDecl) -> SynDecl;
        fn init_inst(&mut self) {}
        fn exit_inst(&mut self, ctx: CtxInst) -> SynInst;
        fn init_expr(&mut self) {}
        fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
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
                    match nt {
                        0 => self.listener.init_program(),          // program
                        1 => self.listener.init_stmt_i(),           // stmt_i
                        2 => self.listener.init_stmt(),             // stmt
                        3 => self.listener.init_decl(),             // decl
                        6 => self.init_decl1(),                     // decl_1
                        4 => self.listener.init_inst(),             // inst
                        5 => self.listener.init_expr(),             // expr
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
                        5 |                                         // decl -> Type Id decl_1 ";"
                        6 => self.exit_decl(alt_id),                // decl -> "typedef" Type Id ";"
                        10 => self.exit_decl1(),                    // decl_1 -> "," Id decl_1
                        11 => {}                                    // decl_1 -> ε
                        7 |                                         // inst -> Id "=" expr ";"
                        8 => self.exit_inst(alt_id),                // inst -> "print" expr ";"
                        12 |                                        // expr_1 -> "+" expr_2 expr_1
                        13 => self.exit_expr1(alt_id),              // expr_1 -> "-" expr_2 expr_1
                        15 |                                        // expr_2 -> "-" expr_2
                        16 |                                        // expr_2 -> Id
                        17 => self.exit_expr2(alt_id),              // expr_2 -> Num
                        9 => {}                                     // expr -> expr_2 expr_1 (not used)
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

        fn is_stack_empty(&self) -> bool {
            self.stack.is_empty()
        }

        fn is_stack_t_empty(&self) -> bool {
            self.stack_t.is_empty()
        }
    }

    impl<T: TypedefListener> Wrapper<T> {
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
            let program = self.stack.pop().unwrap().get_program();
            self.listener.exit(program);
        }

        fn exit_program(&mut self) {
            let ctx = CtxProgram::V1;
            let val = self.listener.exit_program(ctx);
            self.stack.push(SynValue::Program(val));
        }

        fn exit_stmt_i(&mut self) {
            let stmt = self.stack.pop().unwrap().get_stmt();
            let ctx = CtxStmtI::V1 { stmt };
            self.listener.exit_stmt_i(ctx);
        }

        fn exit_stmt(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                3 => {
                    let decl = self.stack.pop().unwrap().get_decl();
                    CtxStmt::V1 { decl }
                }
                4 => {
                    let inst = self.stack.pop().unwrap().get_inst();
                    CtxStmt::V2 { inst }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_stmt")
            };
            let val = self.listener.exit_stmt(ctx);
            self.stack.push(SynValue::Stmt(val));
        }

        fn exit_decl(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                5 => {
                    let star = self.stack.pop().unwrap().get_decl1();
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    CtxDecl::V1 { type1, id, star }
                }
                6 => {
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    CtxDecl::V2 { type1, id }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_decl")
            };
            let val = self.listener.exit_decl(ctx);
            self.stack.push(SynValue::Decl(val));
        }

        fn init_decl1(&mut self) {
            let val = SynDecl1(Vec::new());
            self.stack.push(SynValue::Decl1(val));
        }

        fn exit_decl1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let Some(SynValue::Decl1(SynDecl1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynDecl1 item on wrapper stack");
            };
            star_acc.push(id);
        }

        fn exit_inst(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                7 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    CtxInst::V1 { id, expr }
                }
                8 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxInst::V2 { expr }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_inst")
            };
            let val = self.listener.exit_inst(ctx);
            self.stack.push(SynValue::Inst(val));
        }

        fn exit_expr1(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                12 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V2 { expr: [expr_1, expr_2] }
                }
                13 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V3 { expr: [expr_1, expr_2] }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr1")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }

        fn exit_expr2(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                15 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    CtxExpr::V1 { expr }
                }
                16 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxExpr::V4 { id }
                }
                17 => {
                    let num = self.stack_t.pop().unwrap();
                    CtxExpr::V5 { num }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_expr2")
            };
            let val = self.listener.exit_expr(ctx);
            self.stack.push(SynValue::Expr(val));
        }
    }

    // [typedef_id_type_parser]
}

