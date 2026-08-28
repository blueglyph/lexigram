// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::collections::HashMap;
use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogMsg, LogStatus, Logger};
use lexigram_core::{CollectJoin, TokenId, LALR};
use lexigram_core::parser::lr::LRParser;
use lexigram_core::text_span::{GetLine, GetTextSpan};
use typedef_type_lexer::build_lexer;
use typedef_type_parser::*;
use listener_type_types::*;
use crate::transform_msg;

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
    let mut parser = TypeParser::new();
    for (test_id, (txt, expected_vars, expected_types, expected_errors, expected_calls)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\n{txt}\n{0:-<80}", ""); }
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

pub struct TypeParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: LRParser<'p, LALR>,
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
    pub fn parse(&mut self, text: &'ls str) -> Result<ParserData, BufLog> {
        self.wrapper = Some(Wrapper::new(TypeListener::new(), VERBOSE_WRAPPER));
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_log_mut().add_error(e.to_string());
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
    fn get_log_mut(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn handle_msg(&mut self, span_opt: Option<&PosSpan>, mut msg: LogMsg) {
        transform_msg(self, span_opt, &mut msg);
        self.get_log_mut().add(msg);
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
            CtxDecl::V1 { type1, plus: SynIdI(mut ids) } => {
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
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, LexStateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 23;
    const INITIAL_STATE: LexStateId = 0;
    const FIRST_END_STATE: LexStateId = 4;
    const NBR_STATES: LexStateId = 29;
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
    static STATE_TABLE: [LexStateId; 668] = [
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

    use lexigram_core::{AltId, LALR, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, ListenerWrapper, RecoveryNt, Symbol, Terminate, lr::{LRAction::{self, Accept as LRA, Error as LRE, Reduce as LRR, Shift as LRS, ShiftHook as LRSH}, LRParser, LRStateId, WrapperLRErrorRecovery}}};
    use super::listener_type_types::*;

    static SYMBOLS_T: [(&str, Option<&str>); 11] = [
        ("Comma", Some(",")),("SemiColon", Some(";")),("Eq", Some("=")),("Sub", Some("-")),("Add", Some("+")),("Typedef", Some("typedef")),("Let", Some("let")),("Print", Some("print")),("Num", None),("Id", None),
        ("Type", None)];

    static NUM_NT: usize = 7;
    static NUM_T_FULL: usize = 12;
    static ACTION: [LRAction; 396] = [
        LRE,LRE,LRE,LRE,LRE,LRR(2),LRR(2),LRR(2),LRE,LRE,LRR(2),LRE,LRE,LRE,LRE,LRE,LRE,LRSH(10),LRS(11),LRS(2),LRE,LRE,LRS(3),LRE,LRE,LRE,LRE,LRS(5),LRE,LRE,LRE,LRE,LRS(16),LRS(17),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(19),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(11),LRS(2),LRE,LRE,LRE,LRR(0),LRE,LRE,LRE,LRS(5),LRE,LRE,LRE,LRE,LRS(16),LRS(17),
        LRE,LRE,LRE,LRE,LRE,LRS(5),LRE,LRE,LRE,LRE,LRS(16),LRS(17),LRE,LRE,LRE,LRE,LRE,LRS(5),LRE,LRE,LRE,LRE,LRS(16),LRS(17),LRE,LRE,LRE,LRE,LRE,LRS(5),LRE,LRE,LRE,LRE,LRS(16),
        LRS(17),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRA,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(14),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(15),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(1),LRR(1),LRR(1),LRE,LRE,LRR(1),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(4),LRR(4),LRE,LRE,LRE,LRR(4),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(22),LRE,LRE,LRE,LRE,LRS(6),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(15),LRE,LRR(15),LRR(15),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(14),LRE,LRR(14),LRR(14),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(24),LRE,LRS(7),LRS(8),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(8),LRR(8),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(25),LRSH(26),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(3),LRR(3),LRE,LRE,LRE,LRR(3),LRE,LRSH(27),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(11),LRE,LRR(11),
        LRR(11),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(10),LRR(10),LRE,LRE,LRE,LRR(10),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(31),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(5),LRR(5),LRR(5),LRE,LRE,LRR(5),LRE,LRE,LRE,LRE,LRE,LRE,LRR(6),LRR(6),LRR(6),LRE,LRE,LRR(6),LRE,LRE,LRS(32),LRE,LRS(7),LRS(8),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(13),
        LRE,LRR(13),LRR(13),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(12),LRE,LRR(12),LRR(12),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(7),LRR(7),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(9),LRR(9),LRE,LRE,LRE,LRR(9)];
    static GOTO: [LRStateId; 63] = [
        9,1,0,0,0,0,0,0,0,4,12,0,13,0,0,0,0,0,0,0,18,0,0,0,0,20,0,0,0,0,0,0,0,21,0,0,0,0,0,0,
        0,23,0,0,0,0,0,0,28,0,0,0,0,0,0,29,0,0,0,0,0,0,30];
    static ALT_NT_LEN: [(VarId, u16, u16); 17] = [
        (0, 2, 0),(1, 2, 0),(1, 0, 0),(2, 2, 0),(2, 1, 0),(3, 3, 1),(3, 4, 2),(4, 3, 1),(4, 1, 1),(5, 5, 1),(5, 3, 0),(6, 2, 0),(6, 3, 0),(6, 3, 0),(6, 1, 1),(6, 1, 1),(7, 1, 0)];
    static SYMBOLS_NT: [&str; 8] = [
        "program","decl_i","inst_i","decl","id_i","inst","expr","<goal>"];

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

    // Unfortunately, Rust has no way to safely convert to enum constants...
    impl From<TokenId> for Term {
        fn from(value: TokenId) -> Self {
            match value {
                _ if value == Term::Comma as TokenId => Term::Comma,
                _ if value == Term::SemiColon as TokenId => Term::SemiColon,
                _ if value == Term::Eq as TokenId => Term::Eq,
                _ if value == Term::Sub as TokenId => Term::Sub,
                _ if value == Term::Add as TokenId => Term::Add,
                _ if value == Term::Typedef as TokenId => Term::Typedef,
                _ if value == Term::Let as TokenId => Term::Let,
                _ if value == Term::Print as TokenId => Term::Print,
                _ if value == Term::Num as TokenId => Term::Num,
                _ if value == Term::Id as TokenId => Term::Id,
                _ if value == Term::Type as TokenId => Term::Type,
                _ => panic!("cannot convert terminal index #{value} to Term"),
            }
        }
    }

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum NTerm {
        #[doc = "`program`"]                   Program = 0,
        #[doc = "`decl_i`, parent: `program`"] DeclI = 1,
        #[doc = "`inst_i`, parent: `program`"] InstI = 2,
        #[doc = "`decl`"]                      Decl = 3,
        #[doc = "`id_i`, parent: `decl`"]      IdI = 4,
        #[doc = "`inst`"]                      Inst = 5,
        #[doc = "`expr`"]                      Expr = 6,
    }

    impl TryFrom<TokenId> for NTerm {
        type Error = String;
        fn try_from(value: VarId) -> Result<Self, Self::Error> {
            match value {
                _ if value == NTerm::Program as VarId => Ok(NTerm::Program),
                _ if value == NTerm::DeclI as VarId => Ok(NTerm::DeclI),
                _ if value == NTerm::InstI as VarId => Ok(NTerm::InstI),
                _ if value == NTerm::Decl as VarId => Ok(NTerm::Decl),
                _ if value == NTerm::IdI as VarId => Ok(NTerm::IdI),
                _ if value == NTerm::Inst as VarId => Ok(NTerm::Inst),
                _ if value == NTerm::Expr as VarId => Ok(NTerm::Expr),
                _ => Err(format!("cannot convert nonterminal index #{value} to NTerm")),
            }
        }
    }

    pub fn get_term_name(t: TokenId) -> (&'static str, Option<&'static str>) {
        SYMBOLS_T[t as usize]
    }

    static NT_VALUE: [bool; 8] = [
        true,false,false,true,true,true,true,true];
    static STATE_SYMBOL: [Symbol; 33] = [
        Symbol::Empty,Symbol::NT(1),Symbol::T(7),Symbol::T(10),Symbol::NT(2),Symbol::T(3),Symbol::T(2),Symbol::T(3),Symbol::T(4),Symbol::NT(0),Symbol::T(5),Symbol::T(6),Symbol::NT(3),Symbol::NT(5),Symbol::T(10),Symbol::T(9),Symbol::T(8),Symbol::T(9),Symbol::NT(6),Symbol::T(9),Symbol::NT(4),Symbol::NT(5),Symbol::T(9),Symbol::NT(6),Symbol::T(1),
        Symbol::T(0),Symbol::T(1),Symbol::T(1),Symbol::NT(6),Symbol::NT(6),Symbol::NT(6),Symbol::T(9),Symbol::T(1)];

    pub fn build_parser() -> LRParser<'static, LALR> {
        LRParser::new(
            NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,
            FixedSymTable::new(
                SYMBOLS_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),
                SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
            ),
            true,
            &STATE_SYMBOL,
            &NT_VALUE
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
        V1 { inst: SynInst },
    }
    #[derive(Debug)]
    pub enum CtxDecl {
        /// `decl -> Type (<L> Id / ",")+ ";"`
        V1 { type1: String, plus: SynIdI },
        /// `decl -> "typedef" Type Id ";"`
        V2 { type1: String, id: String },
    }
    #[derive(Debug)]
    pub enum CtxIdI {
        /// `<L> Id / ","` iteration in `decl -> Type ( ►► <L> Id / "," ◄◄ )+ ";" | "typedef" Type Id ";"`
        V1 { id: String },
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

    #[derive(Debug)]
    pub enum EnumSynValue { Program(SynProgram), Decl(SynDecl), IdI(SynIdI), Inst(SynInst), Expr(SynExpr) }

    impl EnumSynValue {
        fn get_program(self) -> SynProgram {
            if let EnumSynValue::Program(val) = self { val } else { panic!() }
        }
        fn get_decl(self) -> SynDecl {
            if let EnumSynValue::Decl(val) = self { val } else { panic!() }
        }
        fn get_id_i(self) -> SynIdI {
            if let EnumSynValue::IdI(val) = self { val } else { panic!() }
        }
        fn get_inst(self) -> SynInst {
            if let EnumSynValue::Inst(val) = self { val } else { panic!() }
        }
        fn get_expr(self) -> SynExpr {
            if let EnumSynValue::Expr(val) = self { val } else { panic!() }
        }
        #[allow(unused)]
        fn nt(&self) -> VarId {
            match &self {
                EnumSynValue::Program(_) => 0,
                EnumSynValue::Decl(_) => 3,
                EnumSynValue::IdI(_) => 4,
                EnumSynValue::Inst(_) => 5,
                EnumSynValue::Expr(_) => 6,
            }
        }
    }

    /// Result returned by [TestListener::get_recovery_value].
    ///
    /// * [Abort](RecoveryNtValue::Abort): stops using the wrapper/listener
    /// * [Skip](RecoveryNtValue::Skip): skips this nonterminal and tries to recover from a more global nonterminal
    /// * [Value](RecoveryNtValue::Value): recovery nonterminal has been pushed, parsing resumes normally
    pub enum RecoveryNtValue {
        /// Aborts the wrapper/listener. Tries to recover the parser and continue to parse without calling the wrapper/listener any more.
        Abort,
        /// Skips the recovery at this level. Tries to recover from another nonterminal.
        Skip,
        /// The recovery nonterminal has been pushed. The parser can continue to parse the stream normally.
        Value(EnumSynValue),
    }

    pub trait TypedefListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_log_mut(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn handle_msg(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
            self.get_log_mut().add(msg);
        }
        #[allow(unused_variables)]
        fn drop_nt_value(&mut self, value: &EnumSynValue) {}
        #[allow(unused_variables)]
        fn get_recovery_value(&mut self, nt: VarId, last_dropped: Option<EnumSynValue>, err_span: &PosSpan) -> RecoveryNtValue { RecoveryNtValue::Abort }
        fn syntax_error_recovered(&mut self) {}
        #[allow(unused_variables)]
        fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, program: SynProgram, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn exit_program(&mut self, ctx: CtxProgram, spans: Vec<PosSpan>) -> SynProgram;
        fn init_decl_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_decl_i(&mut self, ctx: CtxDeclI, spans: Vec<PosSpan>) {}
        fn init_inst_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_inst_i(&mut self, ctx: CtxInstI, spans: Vec<PosSpan>) {}
        fn exit_decl(&mut self, ctx: CtxDecl, spans: Vec<PosSpan>) -> SynDecl;
        fn init_id_i(&mut self) -> SynIdI;
        fn exit_id_i(&mut self, acc: &mut SynIdI, ctx: CtxIdI, spans: Vec<PosSpan>);
        fn exit_inst(&mut self, ctx: CtxInst, spans: Vec<PosSpan>) -> SynInst;
        fn exit_expr(&mut self, ctx: CtxExpr, spans: Vec<PosSpan>) -> SynExpr;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<EnumSynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
        stack_span: Vec<PosSpan>,
        last_dropped_nt_value: Option<EnumSynValue>,
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
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_program(),                   // program -> decl_i inst_i
                        1 => self.exit_decl_i(),                    // decl_i -> <L> decl_i decl
                        2 => self.init_decl_i(),                    // decl_i -> <L> ε
                        3 |                                         // inst_i -> <L> inst_i inst
                        4 => self.exit_inst_i(alt_id),              // inst_i -> <L> inst
                        5 |                                         // decl -> Type id_i ";"
                        6 => self.exit_decl(alt_id),                // decl -> "typedef" Type Id ";"
                        7 => self.exit_id_i(),                      // id_i -> <L> id_i "," Id
                        8 => self.init_id_i(),                      // id_i -> <L> Id
                        9 |                                         // inst -> "let" Id "=" expr ";"
                        10 => self.exit_inst(alt_id),               // inst -> "print" expr ";"
                        11 |                                        // expr -> "-" expr
                        12 |                                        // expr -> expr "+" expr
                        13 |                                        // expr -> <P> expr "-" expr
                        14 |                                        // expr -> Id
                        15 => self.exit_expr(alt_id),               // expr -> Num
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
                _ => panic!("unexpected call {call:?}, nt {nt}, alt_id {alt_id}")
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("{}", self.get_status().join("\n"));
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

        fn is_stack_span_empty(&self) -> bool {
            self.stack_span.is_empty()
        }

        fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
            self.listener.hook(token, text, span)
        }

        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
            self.listener.intercept_token(token, text, span)
        }

        fn get_status(&self) -> Vec<String> {
            vec![
                format!("> stack_t:    [{}]", self.stack_t.join(", ")),
                format!("> stack:      [{}]", self.stack.iter().map(|it| format!("{it:?}")).collect::<Vec<_>>().join(", ")),
                format!("> stack_span: [{}]", self.stack_span.iter().map(PosSpan::to_string).collect::<Vec<_>>().join(", ")),
            ]
        }

        fn push_span(&mut self, span: PosSpan) {
            self.stack_span.push(span);
        }

        fn pop_span(&mut self) -> PosSpan {
            self.stack_span.pop().unwrap()
        }
    }

    impl<T: TypedefListener> WrapperLRErrorRecovery for Wrapper<T> {
        fn pop_nt_value(&mut self) {
            self.last_dropped_nt_value = self.stack.pop();
            if self.verbose { println!("dropped {:?} value", self.last_dropped_nt_value.as_ref().unwrap()); }
            self.listener.drop_nt_value(self.last_dropped_nt_value.as_ref().unwrap());
        }

        fn push_nt_recovery_value(&mut self, nt: VarId, err_span: &PosSpan) -> RecoveryNt {
            match self.listener.get_recovery_value(nt, self.last_dropped_nt_value.take(), err_span) {
                RecoveryNtValue::Abort => RecoveryNt::Abort,
                RecoveryNtValue::Skip => RecoveryNt::Skip,
                RecoveryNtValue::Value(val) => {
                    self.stack.push(val);
                    RecoveryNt::Done
                }
            }
        }

        fn syntax_error_recovered(&mut self) {
            self.listener.syntax_error_recovered();
        }
    }

    impl<T: TypedefListener> Wrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new(), stack_span: Vec::new(), last_dropped_nt_value: None }
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
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_program(ctx, spans);
            self.stack.push(EnumSynValue::Program(val));
        }

        fn init_decl_i(&mut self) {
            self.listener.init_decl_i();
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_decl_i(&mut self) {
            let decl = self.stack.pop().unwrap().get_decl();
            let ctx = CtxDeclI::V1 { decl };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_decl_i(ctx, spans);
        }

        fn init_inst_i(&mut self) {
            self.listener.init_inst_i();
            self.stack_span.insert(self.stack_span.len() - 1, PosSpan::empty());
        }

        fn exit_inst_i(&mut self, alt_id: AltId) {
            let inst = self.stack.pop().unwrap().get_inst();
            let ctx = CtxInstI::V1 { inst };
            if matches!(alt_id, 4) { self.init_inst_i(); }
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_inst_i(ctx, spans);
        }

        fn exit_decl(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                5 => {
                    let plus = self.stack.pop().unwrap().get_id_i();
                    let type1 = self.stack_t.pop().unwrap();
                    (3, CtxDecl::V1 { type1, plus })
                }
                6 => {
                    let id = self.stack_t.pop().unwrap();
                    let type1 = self.stack_t.pop().unwrap();
                    (4, CtxDecl::V2 { type1, id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_decl")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_decl(ctx, spans);
            self.stack.push(EnumSynValue::Decl(val));
        }

        fn init_id_i(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxIdI::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let mut val = self.listener.init_id_i();
            self.listener.exit_id_i(&mut val, ctx, spans);
            self.stack.push(EnumSynValue::IdI(val));
        }

        fn exit_id_i(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxIdI::V1 { id };
            let mut spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..2);
            let Some(EnumSynValue::IdI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_id_i(acc, ctx, spans);
        }

        fn exit_inst(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                9 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    let id = self.stack_t.pop().unwrap();
                    (5, CtxInst::V1 { id, expr })
                }
                10 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    (3, CtxInst::V2 { expr })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_inst")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_inst(ctx, spans);
            self.stack.push(EnumSynValue::Inst(val));
        }

        fn exit_expr(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                11 => {
                    let expr = self.stack.pop().unwrap().get_expr();
                    (2, CtxExpr::V1 { expr })
                }
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
                14 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxExpr::V4 { id })
                }
                15 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxExpr::V5 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_expr")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_expr(ctx, spans);
            self.stack.push(EnumSynValue::Expr(val));
        }
    }

    // [typedef_type_parser]
}
