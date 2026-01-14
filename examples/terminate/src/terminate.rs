// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Parser;
use lexigram_core::text_span::GetLine;
use terminate_lexer::build_lexer;
use terminate_parser::*;
use listener_terminate_types::*;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

static TXT1: &str = r#"
"#;

#[test]
fn test_terminate() {
    let tests = vec![
        (
            TXT1,
            vec![""],
            vec![""],
        ),
    ];
    for (test_id, (txt, expected_vars, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\n{txt}\n{0:-<80}", ""); }
        let mut parser = TerminateParser::new();
        match parser.parse(txt) {
            Ok(ParserData { log, vars }) => {
                if VERBOSE {
                    println!("parsing successful\n{log}");
                }
                assert_eq!(vars, expected_vars, "var mismatch in test {test_id}");
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
    pub log: BufLog,
    pub vars: Vec<String>,
}

pub struct TerminateParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: Parser<'p>,
    wrapper: Option<Wrapper<Listener<'ls>>>,
}

impl<'l, 'ls: 'l> TerminateParser<'l, '_, 'ls> {
    /// Creates a new parser
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        TerminateParser { lexer, parser, wrapper: None }
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
        self.wrapper = Some(Wrapper::new(Listener::new(), VERBOSE_WRAPPER));
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let Listener { log, vars, .. } = self.wrapper.take().unwrap().give_listener();
        if log.has_no_errors() {
            Ok(ParserData { log, vars })
        } else {
            Err(log)
        }
    }
}

// listener

struct Listener<'ls> {
    log: BufLog,
    lines: Option<Vec<&'ls str>>,
    vars: Vec<String>,
}

impl<'ls> Listener<'ls> {
    fn new() -> Self {
        Listener {
            log: BufLog::new(),
            lines: None,
            vars: vec![],
        }
    }

    fn attach_lines(&mut self, lines: Vec<&'ls str>) {
        self.lines = Some(lines);
    }
}

impl GetLine for Listener<'_> {
    fn get_line(&self, n: usize) -> &str {
        self.lines.as_ref().unwrap()[n - 1]
    }
}

// listener trait implementation

#[allow(unused)]
impl TerminateListener for Listener<'_> {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) -> SynLog {
        todo!()
    }

    fn init_log_i(&mut self) -> SynLogI {
        todo!()
    }

    fn exit_log_i(&mut self, ctx: CtxLogI, spans: Vec<PosSpan>) -> SynLogI {
        todo!()
    }

    fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) -> SynLine {
        todo!()
    }

    fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) -> SynMessage {
        todo!()
    }
}

//==============================================================================

pub mod listener_terminate_types {
    use lexigram_core::lexer::PosSpan;

    /// User-defined type for `log`
    #[derive(Debug, PartialEq)] pub struct SynLog();
    /// User-defined type for `<L> line` iteration in `log -> ( ►► <L> line ◄◄ )*`
    #[derive(Debug, PartialEq)] pub struct SynLogI();
    /// User-defined type for `line`
    #[derive(Debug, PartialEq)] pub struct SynLine();
    /// User-defined type for `message`
    #[derive(Debug, PartialEq)] pub struct SynMessage();
}

pub mod terminate_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [terminate_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 23;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 16;
    const NBR_STATES: StateId = 50;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         20,  20,  20,  20,  20,  20,  20,  20,  20,  14,   1,  20,  20,   2,  20,  20,   // 0-15
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   // 16-31
         14,  20,  20,   3,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   // 32-47
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  18,  20,  20,  20,  20,  20,   // 48-63
         20,  19,  16,   7,   8,   4,  16,  22,  15,  21,  16,  16,  16,  16,  12,   0,   // 64-79
          5,  16,  13,   6,  10,   9,  16,  11,  16,  16,  16,  20,  20,  20,  20,  17,   // 80-95
         20,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,   // 96-111
         16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  20,  20,  20,  20,  20,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 20),
        (Seg(57344, 1114111), 20),
    ];
    static TERMINAL_TABLE: [Terminal;34] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(1), mode_state: Some(12), pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::Mode(2), mode_state: Some(13), pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::Mode(2), mode_state: Some(13), pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(14), pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
    ];
    static STATE_TABLE: [StateId; 1151] = [
         16,  17,  18,  19,  20,  21,  22,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16, // state 0
          2,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 1
         50,  50,  50,  50,  50,  50,  50,   3,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 2
         50,  50,  50,  50,   4,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 3
         50,  50,  50,  50,  50,  50,   5,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 4
         50,  50,  50,  50,  50,  50,  24,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 5
         50,  50,  50,  50,  50,  50,  50,  50,  25,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 6
         50,  50,  50,  50,  50,  50,  50,  50,  50,   8,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 7
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   9,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 8
         50,  50,  50,  50,  50,  50,  50,  50,  15,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 9
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  11,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 10
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  26,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 11
         28,  50,  50,  50,  29,  28,  28,  28,  28,  28,  28,  31,  30,  28,  50,  28,  28,  50,  50,  28,  50,  28,  28, // state 12
         48,  50,  50,  50,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  50,  48,  48,  50,  50,  48,  50,  48,  48, // state 13
         49,  50,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49,  49, // state 14
         10,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 15
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 16 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 17 <skip>
         50,  50,  27,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 18 <skip>
         50,  50,  50,  23,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 19 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   6,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 20 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   1,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 21 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   7,  50,  50,  50,  50,  50,  50,  50, // state 22 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  23,  50,  50,  50,  50,  50,  50,  50,  50, // state 23 <skip,mode(1,state 12)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  50,  50,  50,  50,  50,  50, // state 24 <end:0,mode(2,state 13)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  25,  50,  50,  50,  50,  50,  50,  50,  50, // state 25 <end:1,mode(2,state 13)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 26 <end:2>
         50,  50,  27,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 27 <skip>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 28 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  43,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 29 <end:6,mode(3,state 14)>
         32,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 30 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  36,  50,  28,  28, // state 31 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  33,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 32 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  34,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 33 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  35,  28,  50,  28,  28, // state 34 <end:6,mode(3,state 14)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  35,  50,  50,  50,  50,  50,  50,  50,  50, // state 35 <end:3,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  37,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 36 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  38,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 37 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  39,  28, // state 38 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  40,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 39 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  28,  41, // state 40 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  42,  28,  50,  28,  28, // state 41 <end:6,mode(3,state 14)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  42,  50,  50,  50,  50,  50,  50,  50,  50, // state 42 <end:4,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  44,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 43 <end:6,mode(3,state 14)>
         45,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 44 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  46,  50,  28,  28,  28,  50,  28,  50,  28,  28, // state 45 <end:6,mode(3,state 14)>
         28,  50,  50,  50,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  50,  28,  28,  28,  47,  28,  50,  28,  28, // state 46 <end:6,mode(3,state 14)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  47,  50,  50,  50,  50,  50,  50,  50,  50, // state 47 <end:5,mode(3,state 14)>
         48,  50,  50,  50,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  50,  48,  48,  48,  50,  48,  50,  48,  48, // state 48 <end:8,mode(0,state 0)>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 49 <end:7,mode(0,state 0)>
         50 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [terminate_lexer]
}

pub mod terminate_parser {
    // Generated code, don't modify manually anything between the tags below

    // [terminate_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_terminate_types::*;

    const PARSER_NUM_T: usize = 9;
    const PARSER_NUM_NT: usize = 4;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Process", None), ("End", None), ("Shutdown", Some("SHUTDOWN")), ("Note", None), ("Warning", None), ("Error", None), ("Header", None), ("Message", None), ("Id", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["log", "log_i", "line", "message"];
    static ALT_VAR: [VarId; 11] = [0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3];
    static PARSING_TABLE: [AltId; 40] = [0, 0, 0, 0, 0, 0, 0, 11, 11, 0, 1, 1, 1, 1, 1, 1, 1, 11, 11, 2, 4, 5, 6, 3, 3, 3, 3, 11, 11, 12, 12, 12, 12, 7, 8, 9, 10, 11, 11, 12];
    static OPCODES: [&[OpCode]; 11] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3)], &[OpCode::Exit(4), OpCode::T(8), OpCode::T(0)], &[OpCode::Exit(5), OpCode::T(8), OpCode::T(1)], &[OpCode::Exit(6), OpCode::T(2)], &[OpCode::Exit(7), OpCode::T(7), OpCode::T(3)], &[OpCode::Exit(8), OpCode::T(7), OpCode::T(4)], &[OpCode::Exit(9), OpCode::T(7), OpCode::T(5)], &[OpCode::Exit(10), OpCode::T(7), OpCode::T(6)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

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
    pub enum CtxLog {
        /// `log -> (<L> line)*`
        V1 { star: SynLogI },
    }
    #[derive(Debug)]
    pub enum CtxLogI {
        /// `<L> line` iteration in `log -> ( ►► <L> line ◄◄ )*`
        V1 { star_acc: SynLogI, line: SynLine },
    }
    #[derive(Debug)]
    pub enum CtxLine {
        /// `line -> message`
        V1 { message: SynMessage },
        /// `line -> Process Id`
        V2 { process: String, id: String },
        /// `line -> End Id`
        V3 { end: String, id: String },
        /// `line -> "SHUTDOWN"`
        V4,
    }
    #[derive(Debug)]
    pub enum CtxMessage {
        /// `message -> Note Message`
        V1 { note: String, message: String },
        /// `message -> Warning Message`
        V2 { warning: String, message: String },
        /// `message -> Error Message`
        V3 { error: String, message: String },
        /// `message -> Header Message`
        V4 { header: String, message: String },
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `log`
    // #[derive(Debug, PartialEq)] pub struct SynLog();
    // /// User-defined type for `<L> line` iteration in `log -> ( ►► <L> line ◄◄ )*`
    // #[derive(Debug, PartialEq)] pub struct SynLogI();
    // /// User-defined type for `line`
    // #[derive(Debug, PartialEq)] pub struct SynLine();
    // /// User-defined type for `message`
    // #[derive(Debug, PartialEq)] pub struct SynMessage();

    #[derive(Debug)]
    enum SynValue { Log(SynLog), LogI(SynLogI), Line(SynLine), Message(SynMessage) }

    impl SynValue {
        fn get_log(self) -> SynLog {
            if let SynValue::Log(val) = self { val } else { panic!() }
        }
        fn get_log_i(self) -> SynLogI {
            if let SynValue::LogI(val) = self { val } else { panic!() }
        }
        fn get_line(self) -> SynLine {
            if let SynValue::Line(val) = self { val } else { panic!() }
        }
        fn get_message(self) -> SynMessage {
            if let SynValue::Message(val) = self { val } else { panic!() }
        }
    }

    pub trait TerminateListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, log: SynLog, span: PosSpan) {}
        fn init_log(&mut self) {}
        fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) -> SynLog;
        fn init_log_i(&mut self) -> SynLogI;
        fn exit_log_i(&mut self, ctx: CtxLogI, spans: Vec<PosSpan>) -> SynLogI;
        #[allow(unused_variables)]
        fn exitloop_log_i(&mut self, star_acc: &mut SynLogI) {}
        fn init_line(&mut self) {}
        fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) -> SynLine;
        fn init_message(&mut self) {}
        fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) -> SynMessage;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
        stack_span: Vec<PosSpan>,
    }

    impl<T: TerminateListener> ListenerWrapper for Wrapper<T> {
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
                        0 => self.listener.init_log(),              // log
                        1 => self.init_log_i(),                     // log_i
                        2 => self.listener.init_line(),             // line
                        3 => self.listener.init_message(),          // message
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_log(),                       // log -> log_i
                        1 => self.exit_log_i(),                     // log_i -> <L> line log_i
                        2 => self.exitloop_log_i(),                 // log_i -> <L> ε
                        3 |                                         // line -> message
                        4 |                                         // line -> Process Id
                        5 |                                         // line -> End Id
                        6 => self.exit_line(alt_id),                // line -> "SHUTDOWN"
                        7 |                                         // message -> Note Message
                        8 |                                         // message -> Warning Message
                        9 |                                         // message -> Error Message
                        10 => self.exit_message(alt_id),            // message -> Header Message
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

        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
            self.listener.intercept_token(token, text, span)
        }
    }

    impl<T: TerminateListener> Wrapper<T> {
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
            let log = self.stack.pop().unwrap().get_log();
            let span = self.stack_span.pop().unwrap();
            self.listener.exit(log, span);
        }

        fn exit_log(&mut self) {
            let star = self.stack.pop().unwrap().get_log_i();
            let ctx = CtxLog::V1 { star };
            let n = 1;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_log(ctx, spans);
            self.stack.push(SynValue::Log(val));
        }

        fn init_log_i(&mut self) {
            let val = self.listener.init_log_i();
            self.stack.push(SynValue::LogI(val));
        }

        fn exit_log_i(&mut self) {
            let line = self.stack.pop().unwrap().get_line();
            let star_acc = self.stack.pop().unwrap().get_log_i();
            let ctx = CtxLogI::V1 { star_acc, line };
            let n = 2;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_log_i(ctx, spans);
            self.stack.push(SynValue::LogI(val));
        }

        fn exitloop_log_i(&mut self) {
            let SynValue::LogI(star_acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_log_i(star_acc);
        }

        fn exit_line(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                3 => {
                    let message = self.stack.pop().unwrap().get_message();
                    (1, CtxLine::V1 { message })
                }
                4 => {
                    let id = self.stack_t.pop().unwrap();
                    let process = self.stack_t.pop().unwrap();
                    (2, CtxLine::V2 { process, id })
                }
                5 => {
                    let id = self.stack_t.pop().unwrap();
                    let end = self.stack_t.pop().unwrap();
                    (2, CtxLine::V3 { end, id })
                }
                6 => {
                    (1, CtxLine::V4)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_line")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_line(ctx, spans);
            self.stack.push(SynValue::Line(val));
        }

        fn exit_message(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                7 => {
                    let message = self.stack_t.pop().unwrap();
                    let note = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V1 { note, message })
                }
                8 => {
                    let message = self.stack_t.pop().unwrap();
                    let warning = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V2 { warning, message })
                }
                9 => {
                    let message = self.stack_t.pop().unwrap();
                    let error = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V3 { error, message })
                }
                10 => {
                    let message = self.stack_t.pop().unwrap();
                    let header = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V4 { header, message })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_message")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_message(ctx, spans);
            self.stack.push(SynValue::Message(val));
        }
    }

    // [terminate_parser]
}
