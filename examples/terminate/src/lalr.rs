// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::{CollectJoin, LALR};
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Terminate;
use lexigram_core::parser::lr::LRParser;
use lexigram_core::text_span::{GetLine, GetTextSpan};
use terminate_lexer::build_lexer;
use terminate_parser::*;
use listener_terminate_types::*;
use super::*;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

#[test]
fn test_terminate() {
    let tests = vec![
        (
            TXT1,
            vec!["A1,N,first note", "A1,W,first warning", "B1,E,first error"],
            vec![],
        ),
        (
            TXT2,
            vec!["C2,N,start a", "C2,N,start b", "C2,W,limit reached", "C3,N,start x", "C3,E,parameter is out of range", "C3,S"],
            vec![],
        ),
        (
            TXT3,
            vec!["D1,N,OK so far", "-,A"],
            vec!["the END ID 'D2' doesn't match the expected ID 'D1'"],
        ),
    ];
    let mut parser = TerminateParser::new();
    for (test_id, (txt, expected_messages, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80} {test_id}\n{txt}\n{0:-<80}", ""); }
        match parser.parse(txt) {
            Ok(ParserData { log, messages }) => {
                if VERBOSE {
                    println!("messages: {}", messages.iter().map(|s| format!("{s:?}")).join(", "));
                    println!("parsing successful\n{log}");
                }
                assert_eq!(messages, expected_messages, "var mismatch in test {test_id}");
            }
            Err(ParserData { log, messages }) => {
                if VERBOSE {
                    println!("messages: {}", messages.iter().map(|s| format!("{s:?}")).join(", "));
                    println!("errors during parsing:\n{log}");
                }
                assert_eq!(messages, expected_messages, "var mismatch in test {test_id}");
                assert!(!expected_errors.is_empty(), "unexpected error(s) in test {test_id}\n{log}");
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
                        panic!("didn't find this expected error in test {test_id}: {exp_err}\n{log}");
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ParserData {
    pub log: BufLog,
    pub messages: Vec<String>,
}

pub struct TerminateParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: LRParser<'p, LALR>,
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
    pub fn parse(&mut self, text: &'ls str) -> Result<ParserData, ParserData> {
        self.wrapper = Some(Wrapper::new(Listener::new(), VERBOSE_WRAPPER));
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_log_mut().add_error(e.to_string());
        }
        let Listener { log, messages, .. } = self.wrapper.take().unwrap().give_listener();
        if log.has_no_errors() {
            Ok(ParserData { log, messages })
        } else {
            Err(ParserData { log, messages })
        }
    }
}

// listener

struct Listener<'ls> {
    log: BufLog,
    abort: Terminate,
    lines: Option<Vec<&'ls str>>,
    messages: Vec<String>,
    sync_first: bool,
    curr_process: Option<String>,
}

impl<'ls> Listener<'ls> {
    fn new() -> Self {
        Listener {
            log: BufLog::new(),
            abort: Terminate::None,
            lines: None,
            messages: vec![],
            sync_first: true,
            curr_process: None,
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
    fn check_abort_request(&self) -> Terminate {
        self.abort
    }

    fn get_log_mut(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn abort(&mut self, terminate: Terminate) {
        assert_ne!(terminate, Terminate::None);
        self.messages.push(
            format!(
                "{},{}", self.curr_process.take().unwrap_or_else(|| "-".to_string()),
                if terminate == Terminate::Conclude { 'S' } else { 'A' }));
    }

    fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) -> SynLog {
        SynLog()
    }

    fn init_log_i(&mut self) -> SynLogI {
        SynLogI()
    }

    fn exit_log_i(&mut self, acc: &mut SynLogI, ctx: CtxLogI, spans: Vec<PosSpan>) {}

    fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) -> SynLine {
        let span = spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp);
        match ctx {
            // line -> message
            CtxLine::V1 { .. } => {} // already processed in exit_message()
            // line -> Process Id
            CtxLine::V2 { process, id } => {
                self.sync_first = false;
                if self.curr_process.is_some() {
                    self.log.add_error(
                        format!("new PROCESS but {} wasn't closed with an END:\n{}", self.curr_process.as_ref().unwrap(), self.annotate_text(&span)));
                }
                self.curr_process = Some(id);
            }
            // line -> End Id
            CtxLine::V3 { end, id } => {
                if !self.sync_first {
                    match &self.curr_process {
                        None => {
                            self.abort = Terminate::Abort;
                            self.log.add_error(format!("END but there was no open PROCESS:\n{}", self.annotate_text(&span)));
                        }
                        Some(p_id) => {
                            if &id != p_id {
                                self.abort = Terminate::Abort;
                                self.log.add_error(
                                    format!("the END ID '{id}' doesn't match the expected ID '{p_id}':\n{}", self.annotate_text(&span)));
                            }
                            self.curr_process = None;
                        }
                    }
                }
            }
            // line -> "SHUTDOWN"
            CtxLine::V4 => {
                self.log.add_note(format!("encountered SHUTDOWN, parsing ended:\n{}", self.annotate_text(&span)));
                self.abort = Terminate::Conclude;
            }
        }
        SynLine()
    }

    fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) -> SynMessage {
        if !self.sync_first {
            if self.curr_process.is_none() && !matches!(ctx, CtxMessage::V4 { .. }) {
                let span = spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp);
                self.log.add_error(format!("out-of-process message:\n{}", self.annotate_text(&span)));
            } else {
                let msg_opt = match ctx {
                    CtxMessage::V1 { note, message } => Some(format!("{},N,{message}", self.curr_process.as_ref().unwrap())),
                    CtxMessage::V2 { warning, message } => Some(format!("{},W,{message}", self.curr_process.as_ref().unwrap())),
                    CtxMessage::V3 { error, message } => Some(format!("{},E,{message}", self.curr_process.as_ref().unwrap())),
                    CtxMessage::V4 { header, message } => {
                        self.log.add_error(format!("unknown message category '{header}':\n{}", self.annotate_text(&spans[0])));
                        None
                    }
                };
                if let Some(msg) = msg_opt {
                    self.messages.push(msg);
                }
            }
        }
        SynMessage()
    }
}

//==============================================================================

pub mod listener_terminate_types {
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
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, LexStateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 23;
    const INITIAL_STATE: LexStateId = 0;
    const FIRST_END_STATE: LexStateId = 20;
    const NBR_STATES: LexStateId = 48;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         20,  20,  20,  20,  20,  20,  20,  20,  20,  16,   0,  20,  20,   1,  20,  20,   // 0-15
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   // 16-31
         16,  20,  20,   2,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   // 32-47
         17,  17,  17,  17,  17,  17,  17,  17,  17,  17,  18,  20,  20,  20,  20,  20,   // 48-63
         20,  19,  15,  10,  11,   3,  15,  22,   8,  21,  15,  15,  15,  15,   6,   9,   // 64-79
          4,  15,   7,   5,  13,  12,  15,  14,  15,  15,  15,  20,  20,  20,  20,  17,   // 80-95
         20,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,   // 96-111
         15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  15,  20,  20,  20,  20,  20,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 20),
        (Seg(57344, 1114111), 20),
    ];
    static TERMINAL_TABLE: [Terminal;28] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(1), mode_state: Some(16), pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::Mode(2), mode_state: Some(17), pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::Mode(2), mode_state: Some(17), pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(18), pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
    ];
    static STATE_TABLE: [LexStateId; 1105] = [
         20,  21,   1,   2,   3,   4,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 0
         48,  48,  22,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 1
         48,  48,  48,  48,  48,  48,  10,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 2
         48,  48,  48,  48,  48,  48,  48,   5,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 3
         48,  48,  48,  48,  48,  48,  48,  48,  11,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 4
         48,  48,  48,  48,  48,  48,  48,  48,  48,   6,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 5
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,   7,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 6
         48,  48,  48,   8,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 7
         48,  48,  48,  48,  48,   9,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 8
         48,  48,  48,  48,  48,  23,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 9
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  24,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 10
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  12,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 11
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  13,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 12
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  19,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 13
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  15,  48,  48,  48,  48,  48,  48,  48,  48, // state 14
         48,  48,  48,  48,  48,  48,  25,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 15
         48,  48,  48,  27,  26,  26,  28,  26,  26,  26,  26,  26,  26,  26,  29,  26,  48,  48,  48,  26,  48,  26,  26, // state 16
         48,  48,  48,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  48,  48,  48,  46,  48,  46,  46, // state 17
         48,  48,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47, // state 18
         48,  48,  48,  48,  48,  48,  48,  48,  48,  14,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 19
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 20 <skip>
         48,  21,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 21 <skip>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  22,  48,  48,  48,  48,  48,  48, // state 22 <skip,mode(1,state 16)>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  23,  48,  48,  48,  48,  48,  48, // state 23 <end:0,mode(2,state 17)>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  24,  48,  48,  48,  48,  48,  48, // state 24 <end:1,mode(2,state 17)>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48, // state 25 <end:2>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 26 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  41,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 27 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  30,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 28 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  34,  48,  26,  26, // state 29 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  31,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 30 <end:6,mode(3,state 18)>
         48,  48,  48,  32,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 31 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  33,  26,  48,  26,  26, // state 32 <end:6,mode(3,state 18)>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  33,  48,  48,  48,  48,  48,  48, // state 33 <end:3,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  35,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 34 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  36,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 35 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  37,  26, // state 36 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  38,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 37 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  39, // state 38 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  40,  26,  48,  26,  26, // state 39 <end:6,mode(3,state 18)>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  40,  48,  48,  48,  48,  48,  48, // state 40 <end:4,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  42,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 41 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  43,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 42 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  44,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  48,  26,  48,  26,  26, // state 43 <end:6,mode(3,state 18)>
         48,  48,  48,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  48,  26,  45,  26,  48,  26,  26, // state 44 <end:6,mode(3,state 18)>
         48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  48,  45,  48,  48,  48,  48,  48,  48, // state 45 <end:5,mode(3,state 18)>
         48,  48,  48,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  48,  46,  48,  46,  48,  46,  46, // state 46 <end:8,mode(0,state 0)>
         48,  48,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,  47, // state 47 <end:7,mode(0,state 0)>
         48 // error group in [nbr_state * nbr_group + nbr_group]
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
    #![allow(unused)]
    // Generated code, don't modify manually anything between the tags below

    // [terminate_parser]

    use lexigram_core::{AltId, LALR, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, ListenerWrapper, Terminate, lr::{LRAction::{self, Accept as LRA, Error as LRE, Reduce as LRR, Shift as LRS}, LRParser, LRStateId}}};
    use super::listener_terminate_types::*;

    static NUM_NT: usize = 4;
    static NUM_T_FULL: usize = 10;
    static ACTION: [LRAction; 180] = [
        LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRE,LRE,LRR(2),LRS(3),LRS(4),LRS(5),LRS(6),LRS(7),LRS(8),LRS(9),LRE,LRE,LRR(0),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRA,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(12),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(13),LRE,LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRE,LRE,LRR(6),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(14),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(15),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(16),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(17),LRE,LRE,LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),
        LRR(1),LRR(1),LRE,LRE,LRR(1),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRE,LRE,LRR(3),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRE,LRE,LRR(4),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRE,LRE,LRR(5),
        LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRE,LRE,LRR(7),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRE,LRE,LRR(8),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRE,LRE,LRR(9),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),
        LRR(10),LRR(10),LRE,LRE,LRR(10)];
    static GOTO: [LRStateId; 8] = [
        2,1,18,18,18,18,10,11];
    static ALT_NT_LEN: [(VarId, u16, u16); 12] = [
        (0, 1, 0),(1, 2, 0),(1, 0, 0),(2, 1, 0),(2, 2, 2),(2, 2, 2),(2, 1, 0),(3, 2, 2),(3, 2, 2),(3, 2, 2),(3, 2, 2),(4, 1, 0)];
    static SYMBOL_TABLE_T: [(&str, Option<&str>); 9] = [
        ("Process", None),("End", None),("Shutdown", Some("SHUTDOWN")),("Note", None),("Warning", None),("Error", None),("Header", None),("Message", None),("Id", None)];
    static SYMBOL_TABLE_NT: [&str; 5] = [
        "log","log_i","line","message","<goal>"];

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
    pub enum CtxLog {
        /// `log -> (<L> line)*`
        V1 { star: SynLogI },
    }
    #[derive(Debug)]
    pub enum CtxLogI {
        /// `<L> line` iteration in `log -> ( ►► <L> line ◄◄ )*`
        V1 { line: SynLine },
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

    #[derive(Debug)]
    enum EnumSynValue { Log(SynLog), LogI(SynLogI), Line(SynLine), Message(SynMessage) }

    impl EnumSynValue {
        fn get_log(self) -> SynLog {
            if let EnumSynValue::Log(val) = self { val } else { panic!() }
        }
        fn get_log_i(self) -> SynLogI {
            if let EnumSynValue::LogI(val) = self { val } else { panic!() }
        }
        fn get_line(self) -> SynLine {
            if let EnumSynValue::Line(val) = self { val } else { panic!() }
        }
        fn get_message(self) -> SynMessage {
            if let EnumSynValue::Message(val) = self { val } else { panic!() }
        }
    }

    pub trait TerminateListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_log_mut(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn handle_msg(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
            self.get_log_mut().add(msg);
        }
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, log: SynLog, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) -> SynLog;
        fn init_log_i(&mut self) -> SynLogI;
        fn exit_log_i(&mut self, acc: &mut SynLogI, ctx: CtxLogI, spans: Vec<PosSpan>);
        fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) -> SynLine;
        fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) -> SynMessage;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<EnumSynValue>,
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
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_log(),                       // log -> log_i
                        1 => self.exit_log_i(),                     // log_i -> <L> log_i line
                        2 => self.init_log_i(),                     // log_i -> <L> ε
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
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let val = self.stack.pop().unwrap().get_log();
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

        fn get_log_mut(&mut self) -> &mut impl Logger {
            self.listener.get_log_mut()
        }

        fn report(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
            self.listener.handle_msg(span_opt, msg);
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

        fn exit_log(&mut self) {
            let star = self.stack.pop().unwrap().get_log_i();
            let ctx = CtxLog::V1 { star };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_log(ctx, spans);
            self.stack.push(EnumSynValue::Log(val));
        }

        fn init_log_i(&mut self) {
            let val = self.listener.init_log_i();
            self.stack.push(EnumSynValue::LogI(val));
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_log_i(&mut self) {
            let line = self.stack.pop().unwrap().get_line();
            let ctx = CtxLogI::V1 { line };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LogI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_log_i(acc, ctx, spans);
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
                _ => panic!("unexpected alt id {alt_id} in method exit_line")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_line(ctx, spans);
            self.stack.push(EnumSynValue::Line(val));
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
                _ => panic!("unexpected alt id {alt_id} in method exit_message")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_message(ctx, spans);
            self.stack.push(EnumSynValue::Message(val));
        }
    }

    // [terminate_parser]
}
