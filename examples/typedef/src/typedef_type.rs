// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::collections::HashMap;
use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Parser;
use lexigram_core::{CollectJoin, TokenId};
use lexigram_core::text_span::{GetLine, GetTextSpan};
use crate::typedef_type::typedef_type_lexer::build_lexer;
use listener_type_types::*;
use typedef_type_parser::*;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

static TXT1: &str = r#"
float a, b;
typedef int type_int;
typedef type_int type_int2;
type_int c;
type_int2 d;
type_int type_int; // this is a var
let a = 0;
let type_int = 1 + a;
print a;
print type_int - 1;
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
let b = 5;
"#;

#[test]
fn test_type_lexer() {
    let tests = vec![
        (
            TXT1,
            "a:float, b:float, c:int, d:int, type_int:int",
            "type_int2:int, type_int:int",
            vec![],
            vec![
                "token=Id, text='float', span=2:1-5 -> Type",
                "token=Typedef, text='typedef', span=3:1-7 -> Typedef",
                "token=Id, text='int', span=3:9-11 -> Type",
                "token=Typedef, text='typedef', span=4:1-7 -> Typedef",
                "token=Id, text='type_int', span=4:9-16 -> Type",
                "token=Id, text='type_int', span=5:1-8 -> Type",
                "token=Id, text='type_int2', span=6:1-9 -> Type",
                "token=Id, text='type_int', span=7:1-8 -> Type",
                "token=Let, text='let', span=8:1-3 -> Let"
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
            vec!["type 'a' was already defined"],
            vec![],
        ),
    ];
    for (test_id, (txt, expected_vars, expected_types, expected_errors, expected_calls)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\n{txt}\n{0:-<80}", ""); }
        let mut parser = TypeParser::new();
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
                        if err.contains(exp_err) {
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

pub struct TypeParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: Parser<'p>,
    wrapper: Option<Wrapper<TypeListener<'ls>>>,
}

impl<'l, 'ls: 'l> TypeParser<'l, '_, 'ls> {
    /// Creates a new parser
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        TypeParser { lexer, parser, wrapper: None }
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
        self.wrapper = Some(Wrapper::new(TypeListener::new(), VERBOSE_WRAPPER));
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let TypeListener { log, vars, types, hook_calls, .. } = self.wrapper.take().unwrap().give_listener();
        if log.has_no_errors() {
            Ok(ParserData { vars, types, log, hook_calls })
        } else {
            Err(log)
        }
    }
}

// listener

struct TypeListener<'ls> {
    log: BufLog,
    lines: Option<Vec<&'ls str>>,
    vars: HashMap<String, String>,
    types: HashMap<String, String>,
    hook_calls: Vec<String>,
}

impl<'ls> TypeListener<'ls> {
    fn new() -> Self {
        TypeListener {
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

impl GetLine for TypeListener<'_> {
    fn get_line(&self, n: usize) -> &str {
        self.lines.as_ref().unwrap()[n - 1]
    }
}

// listener trait implementation

#[allow(unused)]
impl TypedefListener for TypeListener<'_> {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        let new = match text {
            "int" | "float" | "double" => Term::Type as u16,
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
    }

    fn exit_program(&mut self, ctx: CtxProgram, spans: Vec<PosSpan>) -> SynProgram {
        SynProgram()
    }

    fn exit_decl(&mut self, ctx: CtxDecl, mut spans: Vec<PosSpan>) -> SynDecl {
        match ctx {
            // decl -> Type Id (<L> "," Id)* ";"
            CtxDecl::V1 { type1, id, star: SynIdI(mut ids) } => {
                ids.push((id, std::mem::take(&mut spans[1])));
                for (i, (id, span)) in ids.into_iter().enumerate() {
                    if let Some(prev) = self.vars.insert(id.clone(), self.solve_type(&type1).to_string()) {
                        self.log.add_error(format!("var '{id}' was already declared ({}):\n{}", &span, self.annotate_text(&span)));
                    }
                }
            }
            // decl -> "typedef" Type Id ";"
            CtxDecl::V2 { type1, id } => {
                if let Some(prev) = self.types.insert(id.clone(), self.solve_type(&type1).to_string()) {
                    self.log.add_error(format!("type '{id}' was already defined ({}):\n{}", &spans[2], self.annotate_text(&spans[2])));
                }
            }
        }
        SynDecl()
    }

    fn init_id_i(&mut self) -> SynIdI {
        SynIdI(vec![])
    }

    fn exit_id_i(&mut self, ctx: CtxIdI, mut spans: Vec<PosSpan>) -> SynIdI {
        // `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";"`
        let CtxIdI::V1 { star_acc: SynIdI(mut items), id } = ctx;
        let span = spans.pop().unwrap();
        items.push((id, span));
        SynIdI(items)
    }

    fn exit_inst(&mut self, ctx: CtxInst, spans: Vec<PosSpan>) -> SynInst {
        SynInst()
    }

    fn exit_expr(&mut self, ctx: CtxExpr, spans: Vec<PosSpan>) -> SynExpr {
        SynExpr()
    }
}

//==============================================================================

pub mod listener_type_types {
    use lexigram_core::lexer::PosSpan;

    /// User-defined type for `program`
    #[derive(Debug, PartialEq)] pub struct SynProgram();
    /// User-defined type for `decl`
    #[derive(Debug, PartialEq)] pub struct SynDecl();
    /// User-defined type for `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
    #[derive(Debug, PartialEq)] pub struct SynIdI(pub Vec<(String, PosSpan)>);
    /// User-defined type for `inst`
    #[derive(Debug, PartialEq)] pub struct SynInst();
    /// User-defined type for `expr`
    #[derive(Debug, PartialEq)] pub struct SynExpr();
}

pub mod typedef_type_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_type_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 23;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 4;
    const NBR_STATES: StateId = 29;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         14,  14,  14,  14,  14,  14,  14,  14,  14,   0,  22,  14,  14,  22,  14,  14,   // 0-15
         14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,   // 16-31
          0,  14,  14,  14,  14,  14,  14,  14,  14,  14,  12,   1,   2,   3,  14,   4,   // 32-47
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  14,   6,  14,   7,  14,  14,   // 48-63
         14,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   // 64-79
          8,   8,   8,   8,   8,   8,   8,   8,   8,   8,   8,  14,  14,  14,  14,  13,   // 80-95
         14,   8,   8,   8,  18,  15,  19,   8,   8,  20,   8,   8,   9,   8,  21,   8,   // 96-111
         10,   8,  16,   8,  11,   8,   8,   8,   8,  17,   8,  14,  14,  14,  14,  14,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 14),
        (Seg(57344, 1114111), 14),
    ];
    static TERMINAL_TABLE: [Terminal;25] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
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
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 668] = [
          4,   5,   6,   7,   1,   8,   9,  10,  11,  12,  13,  14,  29,  29,  29,  11,  11,  11,  11,  11,  11,  11,   4, // state 0
         29,  29,  29,  29,  27,  29,  29,  29,  29,  29,  29,  29,   2,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 1
          2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   3,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 2
          2,   2,   2,   2,  28,   2,   2,   2,   2,   2,   2,   2,   3,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 3
          4,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,   4, // state 4 <skip>
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 5 <end:4>
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 6 <end:0>
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 7 <end:3>
         29,  29,  29,  29,  29,   8,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 8 <end:8>
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 9 <end:1>
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 10 <end:2>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 11 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  21,  11,  11,  11,  11,  11,  11,  29, // state 12 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  23,  11,  11,  11,  11,  11,  29, // state 13 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  15,  11,  11,  11,  11,  29, // state 14 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  16,  11,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 15 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  17,  11,  11,  11,  11,  11,  11,  29, // state 16 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  18,  11,  11,  11,  29, // state 17 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  19,  11,  11,  11,  11,  11,  11,  29, // state 18 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  20,  11,  11,  29, // state 19 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 20 <end:5>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  22,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 21 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 22 <end:6>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  11,  24,  11,  29, // state 23 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  11,  11,  25,  29, // state 24 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  26,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 25 <end:9>
         29,  29,  29,  29,  29,  11,  29,  29,  11,  11,  11,  11,  29,  11,  29,  11,  11,  11,  11,  11,  11,  11,  29, // state 26 <end:7>
         27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  29, // state 27 <skip>
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29, // state 28 <skip>
         29 // error group in [nbr_state * nbr_group + nbr_group]
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

pub mod typedef_type_parser {
    // Generated code, don't modify manually anything between the tags below

    // [typedef_type_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_type_types::*;

    const PARSER_NUM_T: usize = 11;
    const PARSER_NUM_NT: usize = 10;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Comma", Some(",")), ("SemiColon", Some(";")), ("Eq", Some("=")), ("Sub", Some("-")), ("Add", Some("+")), ("Typedef", Some("typedef")), ("Let", Some("let")), ("Print", Some("print")), ("Num", None), ("Id", None), ("Type", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["program", "decl_i", "inst_i", "decl", "id_i", "inst", "expr", "expr_1", "expr_2", "inst_i_1"];
    static ALT_VAR: [VarId; 19] = [0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 7, 8, 8, 8, 9, 9];
    static PARSING_TABLE: [AltId; 120] = [19, 19, 19, 19, 19, 0, 0, 0, 19, 19, 0, 20, 19, 19, 19, 19, 19, 1, 2, 2, 19, 19, 1, 19, 19, 19, 19, 19, 19, 19, 3, 3, 19, 19, 19, 20, 19, 19, 19, 19, 19, 5, 20, 20, 19, 19, 4, 19, 6, 7, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 8, 9, 19, 19, 19, 20, 19, 20, 19, 10, 19, 19, 19, 19, 10, 10, 19, 19, 19, 13, 19, 12, 11, 19, 19, 19, 19, 19, 19, 19, 19, 20, 19, 14, 20, 19, 19, 19, 16, 15, 19, 19, 19, 19, 19, 19, 19, 19, 17, 17, 19, 19, 19, 18];
    static OPCODES: [&[OpCode]; 19] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Hook, OpCode::Exit(1), OpCode::NT(3)], &[OpCode::Exit(2)], &[OpCode::NT(9), OpCode::NT(5)], &[OpCode::Exit(4), OpCode::T(1), OpCode::NT(4), OpCode::T(9), OpCode::T(10)], &[OpCode::Exit(5), OpCode::T(1), OpCode::T(9), OpCode::T(10), OpCode::Hook, OpCode::T(5)], &[OpCode::Loop(4), OpCode::Exit(6), OpCode::T(9), OpCode::T(0)], &[OpCode::Exit(7)], &[OpCode::Exit(8), OpCode::T(1), OpCode::NT(6), OpCode::T(2), OpCode::T(9), OpCode::T(6)], &[OpCode::Exit(9), OpCode::T(1), OpCode::NT(6), OpCode::T(7)], &[OpCode::NT(7), OpCode::Exit(10), OpCode::NT(8)], &[OpCode::Loop(7), OpCode::Exit(11), OpCode::NT(8), OpCode::T(4)], &[OpCode::Loop(7), OpCode::Exit(12), OpCode::NT(8), OpCode::T(3)], &[OpCode::Exit(13)], &[OpCode::Exit(14), OpCode::NT(8), OpCode::T(3)], &[OpCode::Exit(15), OpCode::T(9)], &[OpCode::Exit(16), OpCode::T(8)], &[OpCode::Loop(2), OpCode::Exit(17)], &[OpCode::Exit(18)]];
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
        #[doc = "'let'"]      Let = 6,
        #[doc = "'print'"]    Print = 7,
        #[doc = "(variable)"] Num = 8,
        #[doc = "(variable)"] Id = 9,
        #[doc = "(variable)"] Type = 10,
    }

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum NTerm {
        #[doc = "`program`"]                    Program = 0,
        #[doc = "`decl_i`, parent: `program`"]  DeclI = 1,
        #[doc = "`inst_i`, parent: `program`"]  InstI = 2,
        #[doc = "`decl`"]                       Decl = 3,
        #[doc = "`id_i`, parent: `decl`"]       IdI = 4,
        #[doc = "`inst`"]                       Inst = 5,
        #[doc = "`expr`"]                       Expr = 6,
        #[doc = "`expr_1`, parent: `expr`"]     Expr1 = 7,
        #[doc = "`expr_2`, parent: `expr`"]     Expr2 = 8,
        #[doc = "`inst_i_1`, parent: `inst_i`"] InstI1 = 9,
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
    // /// User-defined type for `<L> "," Id` iteration in `decl -> Type Id ( ►► <L> "," Id ◄◄ )* ";" | "typedef" Type Id ";"`
    // #[derive(Debug, PartialEq)] pub struct SynIdI();
    // /// User-defined type for `inst`
    // #[derive(Debug, PartialEq)] pub struct SynInst();
    // /// User-defined type for `expr`
    // #[derive(Debug, PartialEq)] pub struct SynExpr();

    #[derive(Debug)]
    enum SynValue { Program(SynProgram), Decl(SynDecl), IdI(SynIdI), Inst(SynInst), Expr(SynExpr) }

    impl SynValue {
        fn get_program(self) -> SynProgram {
            if let SynValue::Program(val) = self { val } else { panic!() }
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
        fn init_decl_i(&mut self) {}
        #[allow(unused)]
        fn exit_decl_i(&mut self, ctx: CtxDeclI, spans: Vec<PosSpan>) {}
        fn init_inst_i(&mut self) {}
        #[allow(unused)]
        fn exit_inst_i(&mut self, ctx: CtxInstI, spans: Vec<PosSpan>) {}
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
                    if matches!(nt, 1 | 2 | 4) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_program(),          // program
                        1 => self.listener.init_decl_i(),           // decl_i
                        2 => self.listener.init_inst_i(),           // inst_i
                        9 => {}                                     // inst_i_1
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
                        0 => self.exit_program(),                   // program -> decl_i inst_i
                        1 => self.exit_decl_i(),                    // decl_i -> <L> decl decl_i
                        17 |                                        // inst_i_1 -> inst_i
                        18 => self.exit_inst_i(alt_id),             // inst_i_1 -> ε
                        2 => {}                                     // decl_i -> <L> ε (not used)
                     /* 3 */                                        // inst_i -> <L> inst inst_i_1 (never called)
                        4 |                                         // decl -> Type Id id_i ";"
                        5 => self.exit_decl(alt_id),                // decl -> "typedef" Type Id ";"
                        6 => self.exit_id_i(),                      // id_i -> <L> "," Id id_i
                        7 => self.exitloop_id_i(),                  // id_i -> <L> ε
                        8 |                                         // inst -> "let" Id "=" expr ";"
                        9 => self.exit_inst(alt_id),                // inst -> "print" expr ";"
                        11 |                                        // expr_1 -> "+" expr_2 expr_1
                        12 => self.exit_expr1(alt_id),              // expr_1 -> "-" expr_2 expr_1
                        14 |                                        // expr_2 -> "-" expr_2
                        15 |                                        // expr_2 -> Id
                        16 => self.exit_expr2(alt_id),              // expr_2 -> Num
                        10 => {}                                    // expr -> expr_2 expr_1 (not used)
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
            let n = 2;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_program(ctx, spans);
            self.stack.push(SynValue::Program(val));
        }

        fn exit_decl_i(&mut self) {
            let decl = self.stack.pop().unwrap().get_decl();
            let ctx = CtxDeclI::V1 { decl };
            let n = 2;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_decl_i(ctx, spans);
        }

        fn exit_inst_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 18;
            let inst = self.stack.pop().unwrap().get_inst();
            let ctx = CtxInstI::V1 { inst, last_iteration };
            let n = 2;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_inst_i(ctx, spans);
        }

        fn exit_decl(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                4 => {
                    let star = self.stack.pop().unwrap().get_id_i();
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    (4, CtxDecl::V1 { type1, id, star })
                }
                5 => {
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
                8 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    (5, CtxInst::V1 { id, expr })
                }
                9 => {
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
                11 => {
                    let expr_2 = self.stack.pop().unwrap().get_expr();
                    let expr_1 = self.stack.pop().unwrap().get_expr();
                    (3, CtxExpr::V2 { expr: [expr_1, expr_2] })
                }
                12 => {
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
                14 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    (2, CtxExpr::V1 { expr })
                }
                15 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxExpr::V4 { id })
                }
                16 => {
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

    // [typedef_type_parser]
}
