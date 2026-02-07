// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::collections::HashMap;
use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Parser;
use lexigram_core::text_span::{GetLine, GetTextSpan};
use lexigram_core::{CollectJoin, TokenId};
use typedef_match_lexer::build_lexer;
use typedef_match_parser::*;
use listener_match_types::*;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

static TXT1: &str = r#"
float a, b;
typedef int type_int;
typedef type_int type_int2;
type_int c;
type_int2 d;
a = 0;
print a;
"#;

static TXT2: &str = r#"
int wrong, wrong2;
float wrong;
int a, b,
    c, wrong2;
print wrong;
"#;

static TXT3: &str = r#"
typedef int a;
typedef float a;
a b;
b = 5;
"#;

#[test]
fn test_match_lexer() {
    let tests = vec![
        (
            TXT1,
            "a:float, b:float, c:int, d:int",
            "type_int2:int, type_int:int",
            vec![],
            vec![
                "token=Id, text='float', span=2:1-5 -> Type",
                "token=Id, text='a', span=2:7 -> Id",
                "token=Id, text='b', span=2:10 -> Id",
                "token=Id, text='typedef', span=3:1-7 -> Typedef",
                "token=Id, text='int', span=3:9-11 -> Type",
                "token=Id, text='type_int', span=3:13-20 -> Id",
                "token=Id, text='typedef', span=4:1-7 -> Typedef",
                "token=Id, text='type_int', span=4:9-16 -> Type",
                "token=Id, text='type_int2', span=4:18-26 -> Id",
                "token=Id, text='type_int', span=5:1-8 -> Type",
                "token=Id, text='c', span=5:10 -> Id",
                "token=Id, text='type_int2', span=6:1-9 -> Type",
                "token=Id, text='d', span=6:11 -> Id",
                "token=Id, text='a', span=7:1 -> Id",
                "token=Id, text='print', span=8:1-5 -> Print",
                "token=Id, text='a', span=8:7 -> Id",
            ],
        ),
        (
            TXT2,
            "", "",
            vec!["var 'wrong' was already declared", "var 'wrong2' was already declared"],
            vec![],
        ),
        (
            TXT3,
            "", "",
            vec!["syntax error: found input 'Type' instead of 'Id'"],
            vec![],
        ),
    ];
    for (test_id, (txt, expected_vars, expected_types, expected_errors, expected_calls)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\n{txt}\n{0:-<80}", ""); }
        let mut parser = MatchParser::new();
        match parser.parse(txt) {
            Ok(ParserData { vars, types, log, hook_calls }) => {
                let mut lvars = vars.into_iter().map(|(k, v)| format!("{k}:{v}")).to_vec();
                lvars.sort();
                let result_vars = lvars.join(", ");
                let mut ltypes = types.into_iter().map(|(k, v)| format!("{k}:{v}")).to_vec();
                ltypes.sort();
                let result_types = ltypes.join(", ");
                if VERBOSE {
                    println!("parsing successful\n{log}\nvars: {result_vars}\ntypes: {result_types}\nhook_calls: {hook_calls:?}");
                }

                assert_eq!(result_vars, expected_vars, "var mismatch in test {test_id}");
                assert_eq!(result_types, expected_types, "type mismatch in test {test_id}");
                assert_eq!(hook_calls, expected_calls, "hook call mismatch in test {test_id}");
                assert!(expected_errors.is_empty(), "errors were expected in test {test_id}: {expected_errors:?}");
            }
            Err(log) => {
                assert!(!expected_errors.is_empty(), "unexpected error(s) in test {test_id}\n{log}");
                if VERBOSE {
                    println!("errors during parsing:\n{log}");
                }
                let mut errors = log.get_errors();
                for exp_err in expected_errors {
                    let mut next_err = errors.next();
                    while let Some(err) = next_err {
                        if err.get_inner_str().contains(exp_err) {
                            break;
                        }
                        next_err = errors.next();
                    }
                    if next_err.is_none() {
                        panic!("didn't find this expected error in test {test_id}: {exp_err}");
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ParserData {
    pub vars: HashMap<String, String>,
    pub types: HashMap<String, String>,
    pub log: BufLog,
    pub hook_calls: Vec<String>,
}

pub struct MatchParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: Parser<'p>,
    wrapper: Option<Wrapper<MatchListener<'ls>>>,
}

impl<'l, 'ls: 'l> MatchParser<'l, '_, 'ls> {
    /// Creates a new parser
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        MatchParser { lexer, parser, wrapper: None }
    }

    /// Parses a text.
    ///
    /// On success, returns
    /// * `vars`, a `HashMap<String, String>` that contains the variables and their resolved type
    /// * `types`, a `HashMap<String, String>` that contains the defined types and what type they resolve to
    /// * `log`, a `BufLog` object.
    ///
    /// On failure, returns the log with the error messages.
    pub fn parse(&'ls mut self, text: &'ls str) -> Result<ParserData, BufLog> {
        self.wrapper = Some(Wrapper::new(MatchListener::new(), VERBOSE_WRAPPER));
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let MatchListener { log, vars, types, hook_calls, .. } = self.wrapper.take().unwrap().give_listener();
        if log.has_no_errors() {
            Ok(ParserData { vars, types, log, hook_calls })
        } else {
            Err(log)
        }
    }
}

// listener

struct MatchListener<'ls> {
    log: BufLog,
    lines: Option<Vec<&'ls str>>,
    vars: HashMap<String, String>,
    types: HashMap<String, String>,
    hook_calls: Vec<String>,
}

impl<'ls> MatchListener<'ls> {
    fn new() -> Self {
        MatchListener {
            log: BufLog::new(),
            lines: None,
            vars: HashMap::new(),
            types: HashMap::new(),
            hook_calls: vec![],
        }
    }

    fn attach_lines(&mut self, lines: Vec<&'ls str>) {
        self.lines = Some(lines);
    }

    fn solve_type<'s>(&'s self, mut typ: &'s str) -> &'s str {
        while let Some(solved) = self.types.get(typ) {
           typ = solved.as_str();
        }
        typ
    }
}

impl GetLine for MatchListener<'_> {
    fn get_line(&self, n: usize) -> &str {
        self.lines.as_ref().unwrap()[n - 1]
    }
}

// listener trait implementation

#[allow(unused)]
impl TypedefListener for MatchListener<'_> {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        if token == Term::Id as u16 {
            let new = match text {
                "int" | "float" | "double" => Term::Type as u16,
                "print" => Term::Print as u16,
                "typedef" => Term::Typedef as u16,
                t => {
                    if self.types.contains_key(t) {
                        Term::Type as u16
                    } else {
                        token
                    }
                }
            };
            let report = format!("token={}, text='{text}', span={span} -> {}", get_term_name(token).0, get_term_name(new).0);
            if VERBOSE {
                println!("    {report},");
            }
            self.hook_calls.push(report);
            new
        } else {
            token
        }
    }

    fn exit_program(&mut self, ctx: CtxProgram, spans: Vec<PosSpan>) -> SynProgram {
        SynProgram()
    }

    fn exit_stmt(&mut self, _ctx: CtxStmt, _spans: Vec<PosSpan>) -> SynStmt {
        SynStmt()
    }

    fn exit_decl(&mut self, ctx: CtxDecl, mut spans: Vec<PosSpan>) -> SynDecl {
        match ctx {
            // decl -> Type Id (<L> "," Id)* ";"
            CtxDecl::V1 { type1, star: SynIdI(mut ids) } => {
                for (i, (id, span)) in ids.into_iter().enumerate() {
                    if let Some(prev) = self.vars.insert(id.clone(), self.solve_type(&type1).to_string()) {
                        self.log.add_error(format!("var '{id}' was already declared ({}):\n{}", &span, self.annotate_text(&span)));
                    } else {
                        self.log.add_note(format!("var '{id}': '{}' declared", self.vars.get(&id).unwrap()));
                    }
                }
            }
            // decl -> "typedef" Type Id ";"
            CtxDecl::V2 { type1, id } => {
                if let Some(prev) = self.types.insert(id.clone(), self.solve_type(&type1).to_string()) {
                    self.log.add_error(format!("type '{id}' was already defined ({}):\n{}", &spans[2], self.annotate_text(&spans[2])));
                } else {
                    self.log.add_note(format!("type '{id}' defined as '{}'", self.types.get(&id).unwrap()));
                }
            }
        }
        SynDecl()
    }

    fn init_id_i(&mut self, ctx: InitCtxIdI, mut spans: Vec<PosSpan>) -> SynIdI {
        let InitCtxIdI::V1 { id } = ctx;
        SynIdI(vec![(id, spans.pop().unwrap())])
    }

    fn exit_id_i(&mut self, acc: &mut SynIdI, ctx: CtxIdI, mut spans: Vec<PosSpan>) {
        // `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";"`
        let CtxIdI::V1 { id } = ctx;
        let span = spans.pop().unwrap();
        acc.0.push((id, span));
    }

    fn exit_inst(&mut self, ctx: CtxInst, spans: Vec<PosSpan>) -> SynInst {
        SynInst()
    }

    fn exit_expr(&mut self, ctx: CtxExpr, spans: Vec<PosSpan>) -> SynExpr {
        SynExpr()
    }
}

//==============================================================================

pub mod listener_match_types {
    use lexigram_core::lexer::PosSpan;

    /// User-defined type for `program`
    #[derive(Debug, PartialEq)] pub struct SynProgram();
    /// User-defined type for `stmt`
    #[derive(Debug, PartialEq)] pub struct SynStmt();
    /// User-defined type for `decl`
    #[derive(Debug, PartialEq)] pub struct SynDecl();
    /// User-defined type for `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
    #[derive(Debug, PartialEq)] pub struct SynIdI(pub Vec<(String, PosSpan)>);
    /// User-defined type for `inst`
    #[derive(Debug, PartialEq)] pub struct SynInst();
    /// User-defined type for `expr`
    #[derive(Debug, PartialEq)] pub struct SynExpr();
}

pub mod typedef_match_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_match_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 13;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 4;
    const NBR_STATES: StateId = 14;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         11,  11,  11,  11,  11,  11,  11,  11,  11,   0,  12,  11,  11,  12,  11,  11,   // 0-15
         11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,   // 16-31
          0,  11,  11,  11,  11,  11,  11,  11,  11,  11,   9,   1,   2,   3,  11,   4,   // 32-47
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  11,   6,  11,   7,  11,  11,   // 48-63
         11,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   // 64-79
          8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,  11,  11,  11,  11,  10,   // 80-95
         11,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   // 96-111
          8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,  11,  11,  11,  11,  11,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 11),
        (Seg(57344, 1114111), 11),
    ];
    static TERMINAL_TABLE: [Terminal;10] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 183] = [
          4,   5,   6,   7,   1,   8,   9,  10,  11,  14,  14,  14,   4, // state 0
         14,  14,  14,  14,  12,  14,  14,  14,  14,   2,  14,  14,  14, // state 1
          2,   2,   2,   2,   2,   2,   2,   2,   2,   3,   2,   2,   2, // state 2
          2,   2,   2,   2,  13,   2,   2,   2,   2,   3,   2,   2,   2, // state 3
          4,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,   4, // state 4 <skip>
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14, // state 5 <end:4>
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14, // state 6 <end:0>
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14, // state 7 <end:3>
         14,  14,  14,  14,  14,   8,  14,  14,  14,  14,  14,  14,  14, // state 8 <end:5>
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14, // state 9 <end:1>
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14, // state 10 <end:2>
         14,  14,  14,  14,  14,  11,  14,  14,  11,  14,  11,  14,  14, // state 11 <end:6>
         12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  14, // state 12 <skip>
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14, // state 13 <skip>
         14 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [typedef_match_lexer]
}

pub mod typedef_match_parser {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_match_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
    use super::listener_match_types::*;

    const PARSER_NUM_T: usize = 10;
    const PARSER_NUM_NT: usize = 9;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Comma", Some(",")), ("SemiColon", Some(";")), ("Eq", Some("=")), ("Sub", Some("-")), ("Add", Some("+")), ("Num", None), ("Id", None), ("Typedef", Some("typedef")), ("Print", Some("print")), ("Type", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "stmt_i", "stmt", "decl", "id_i", "inst", "expr", "expr_1", "expr_2"];
    static ALT_VAR: [VarId; 18] = [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 7, 8, 8, 8];
    static PARSING_TABLE: [AltId; 99] = [18, 18, 18, 18, 18, 18, 0, 0, 0, 0, 0, 18, 18, 18, 18, 18, 18, 1, 1, 1, 1, 2, 18, 18, 18, 18, 18, 18, 4, 3, 4, 3, 19, 18, 18, 18, 18, 18, 18, 19, 6, 19, 5, 19, 7, 8, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 9, 19, 10, 19, 19, 18, 19, 18, 11, 18, 11, 11, 18, 18, 18, 18, 18, 14, 18, 13, 12, 18, 18, 18, 18, 18, 18, 18, 19, 18, 15, 19, 17, 16, 18, 18, 18, 18];
    static OPCODES: [&[OpCode]; 18] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(1), OpCode::NT(4), OpCode::T(6), OpCode::T(9)], &[OpCode::Exit(6), OpCode::T(1), OpCode::T(6), OpCode::T(9), OpCode::T(7)], &[OpCode::Loop(4), OpCode::Exit(7), OpCode::T(6), OpCode::T(0)], &[OpCode::Exit(8)], &[OpCode::Exit(9), OpCode::T(1), OpCode::NT(6), OpCode::T(2), OpCode::T(6)], &[OpCode::Exit(10), OpCode::T(1), OpCode::NT(6), OpCode::T(8)], &[OpCode::NT(7), OpCode::Exit(11), OpCode::NT(8)], &[OpCode::Loop(7), OpCode::Exit(12), OpCode::NT(8), OpCode::T(4)], &[OpCode::Loop(7), OpCode::Exit(13), OpCode::NT(8), OpCode::T(3)], &[OpCode::Exit(14)], &[OpCode::Exit(15), OpCode::NT(8), OpCode::T(3)], &[OpCode::Exit(16), OpCode::T(6)], &[OpCode::Exit(17), OpCode::T(5)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;


    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum Term {
        #[doc = "','"]        Comma = 0,
        #[doc = "';'"]        SemiColon = 1,
        #[doc = "'='"]        Eq = 2,
        #[doc = "'-'"]        Sub = 3,
        #[doc = "'+'"]        Add = 4,
        #[doc = "(variable)"] Num = 5,
        #[doc = "(variable)"] Id = 6,
        #[doc = "'typedef'"]  Typedef = 7,
        #[doc = "'print'"]    Print = 8,
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
        V1 { type1: String, star: SynIdI },
        /// `decl -> "typedef" Type Id ";"`
        V2 { type1: String, id: String },
    }
    #[derive(Debug)]
    pub enum InitCtxIdI {
        /// value of `Id` before `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxIdI {
        /// `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
        V1 { id: String },
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
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, program: SynProgram, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_program(&mut self) {}
        fn exit_program(&mut self, ctx: CtxProgram, spans: Vec<PosSpan>) -> SynProgram;
        fn init_stmt_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_stmt_i(&mut self, ctx: CtxStmtI, spans: Vec<PosSpan>) {}
        fn init_stmt(&mut self) {}
        fn exit_stmt(&mut self, ctx: CtxStmt, spans: Vec<PosSpan>) -> SynStmt;
        fn init_decl(&mut self) {}
        fn exit_decl(&mut self, ctx: CtxDecl, spans: Vec<PosSpan>) -> SynDecl;
        fn init_id_i(&mut self, ctx: InitCtxIdI, spans: Vec<PosSpan>) -> SynIdI;
        fn exit_id_i(&mut self, acc: &mut SynIdI, ctx: CtxIdI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_id_i(&mut self, acc: &mut SynIdI) {}
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
                    if matches!(nt, 1) {
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
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let val = self.stack.pop().unwrap().get_program();
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

        fn exit_program(&mut self) {
            let ctx = CtxProgram::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_program(ctx, spans);
            self.stack.push(SynValue::Program(val));
        }

        fn exit_stmt_i(&mut self) {
            let stmt = self.stack.pop().unwrap().get_stmt();
            let ctx = CtxStmtI::V1 { stmt };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
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
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_stmt(ctx, spans);
            self.stack.push(SynValue::Stmt(val));
        }

        fn exit_decl(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                5 => {
                    let star = self.stack.pop().unwrap().get_id_i();
                    let type1 = self.stack_t.pop().unwrap();
                    (3, CtxDecl::V1 { type1, star })
                }
                6 => {
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    (4, CtxDecl::V2 { type1, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_decl")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_decl(ctx, spans);
            self.stack.push(SynValue::Decl(val));
        }

        fn init_id_i(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = InitCtxIdI::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_id_i(ctx, spans);
            self.stack.push(SynValue::IdI(val));
        }

        fn exit_id_i(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxIdI::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::IdI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_id_i(acc, ctx, spans);
        }

        fn exitloop_id_i(&mut self) {
            let SynValue::IdI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_id_i(acc);
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
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
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
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
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
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_expr(ctx, spans);
            self.stack.push(SynValue::Expr(val));
        }
    }

    // [typedef_match_parser]
}
