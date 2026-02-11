// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::io::Cursor;
use lexigram_core::char_reader::CharReader;
use lexigram_core::CollectJoin;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::{Parser, Terminate};
use watcher_lexer::build_lexer;
use watcher_parser::*;
use listener_watcher_types::*;

static TXT1: &str = r#"
category right-recursive
## note: first note
## note: second note

## warning: incoming danger!
## custom_header: #"!!&
## info: incoming danger
## error: ERROR 1
end

category star
## info: new category
## warning: 1
## warning: 2
## warning: 3
## warning: 4
## error: ERROR
end

shutdown

## fatal: shouldn't take this message
"#;

static TXT2: &str = r#"
category star
## info: 1
## info: 2
## info: 3
## error:FATAL ERROR!
shutdown

## fatal: shouldn't take this message
"#;


const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

#[test]
fn test_watcher() {
    let tests: Vec<(&str, Vec<&str>, Vec<&str>)> = vec![
        (
            TXT1,
            vec!["star,S"],
            vec![],
        ),
        (
            TXT2,
            vec![],
            vec![],
        ),
    ];
    for (test_id, (txt, expected_messages, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80} {test_id}\n{txt}\n{0:-<80}", ""); }
        let mut parser = WatcherParser::new();
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

// parser

#[derive(Debug)]
pub struct ParserData {
    pub log: BufLog,
    pub messages: Vec<String>,
}

struct WatcherParser<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'ls str>>,
    parser: Parser<'p>,
    wrapper: Option<Wrapper<Listener>>,
}

impl<'l, 'ls: 'l> WatcherParser<'l, '_, 'ls> {
    /// Creates a new parser
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        WatcherParser { lexer, parser, wrapper: None }
    }

    pub fn parse(&'ls mut self, text: &'ls str) -> Result<ParserData, ParserData> {
        let stream = CharReader::new(Cursor::new(text));
        self.wrapper = Some(Wrapper::new(Listener::new(), VERBOSE_WRAPPER));
        self.lexer.attach_stream(stream);
        // self.wrapper.as_mut().unwrap().get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_mut_log().add_error(e.to_string());
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

struct Listener {
    log: BufLog,
    messages: Vec<String>,
    abort: Terminate,
    curr_category: Option<String>,
}

impl Listener {
    fn new() -> Self {
        Listener {
            log: BufLog::new(),
            messages: vec![],
            abort: Terminate::None,
            curr_category: None,
        }
    }

    fn open_category(&mut self, new_cat: String, spans: &[PosSpan]) {
        if let Some(cat) = &self.curr_category {
            let span = spans.iter().fold(PosSpan::empty(), |acc, s| acc + s);
            self.log.add_error(format!("{span}: missing end for category {cat} when opening {new_cat}"));
        }
        self.log.add_note(format!("new category {new_cat}"));
        self.curr_category = Some(new_cat);
    }
}

#[allow(unused)]
impl WatcherListener for Listener {
    fn check_abort_request(&self) -> Terminate {
        self.abort
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn abort(&mut self, terminate: Terminate) {
        println!("fn abort");
        self.log.add_note("aborted");
        self.messages.push(
            format!(
                "{},{}", self.curr_category.take().unwrap_or_else(|| "-".to_string()),
                if terminate == Terminate::Conclude { 'S' } else { 'A' }));
    }

    fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) -> SynLog {
        println!("fn exit_log");
        match ctx {
            // log -> log shutdown
            CtxLog::V1 { log, shutdown } => {
                self.abort = Terminate::Conclude;
            }
            // log -> log "category" category
            CtxLog::V2 { log, category, .. } => {}
            // log -> "category" category
            CtxLog::V3 { category, .. } => {}
        }
        SynLog()
    }

    fn exit_shutdown(&mut self, ctx: CtxShutdown, spans: Vec<PosSpan>) -> SynShutdown {
        println!("fn exit_shutdown");
        // shutdown -> "shutdown"
        let CtxShutdown::V1 = ctx;
        self.abort = Terminate::Conclude;
        SynShutdown()
    }

    fn exit_open_category(&mut self, ctx: CtxOpenCategory, spans: Vec<PosSpan>) -> SynOpenCategory {
        if let Some(cat) = &self.curr_category {
            let span = spans.iter().fold(PosSpan::empty(), |acc, s| acc + s);
            self.log.add_error(format!("{span}: missing end for category {cat} when opening new category"));
        }
        SynOpenCategory()
    }

    fn exit_end_category(&mut self, ctx: CtxEndCategory, spans: Vec<PosSpan>) -> SynEndCategory {
        if self.curr_category.is_none() {
            let span = spans.iter().fold(PosSpan::empty(), |acc, s| acc + s);
            self.log.add_error(format!("{span}: 'end' encountered outside any category"));
        }
        SynEndCategory()
    }

    fn exit_category(&mut self, ctx: CtxCategory, spans: Vec<PosSpan>) -> SynCategory {
        println!("fn exit_category");
        match ctx {
            // category -> "right-recursive" right_recursive
            CtxCategory::V1 { .. } => {}
            // category -> "star" star
            CtxCategory::V2 { .. } => {}
        }
        SynCategory()
    }

    fn init_right_recursive(&mut self) -> SynRightRecursive {
        println!("fn init_right_recursive");
        SynRightRecursive()
    }

    fn exit_right_recursive(&mut self, acc: &mut SynRightRecursive, ctx: CtxRightRecursive, spans: Vec<PosSpan>) {
        println!("fn exit_right_recursive");
        match ctx {
            // right_recursive -> <L> line right_recursive
            CtxRightRecursive::V1 { line } => {}
            // right_recursive -> "end"
            CtxRightRecursive::V2 => {
                if let Some(cat) = &self.curr_category {
                    self.log.add_note(format!("end {cat}"));
                    self.curr_category = None;
                } else {
                    let span = &spans[1];
                    self.log.add_error("{span}: end outside category");
                }
            }
        }
    }


    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar {
        println!("fn exit_star");
        // star -> (<L> line)* "end"
        let CtxStar::V1 { star } = ctx;
        if let Some(cat) = &self.curr_category {
            self.log.add_note(format!("end {cat}"));
            self.curr_category = None;
        } else {
            let span = &spans[1];
            self.log.add_error("{span}: end outside category");
        }
        SynStar()
    }

    fn init_star_i(&mut self) -> SynStarI {
        println!("fn init_star_i");
        SynStarI()
    }

    fn exit_star_i(&mut self, acc: &mut SynStarI, ctx: CtxStarI, spans: Vec<PosSpan>) {
        println!("fn exit_star_i");
        // `<L> line` iteration in `star -> ( ►► <L> line ◄◄ )* "end"`
        let CtxStarI::V1 { line } = ctx;
    }

    fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) -> SynLine {
        println!("fn exit_line");
        match ctx {
            // line -> message
            CtxLine::V1 { message } => {}
            // line -> shutdown
            CtxLine::V2 { shutdown } => {
                self.abort = Terminate::Conclude;
            }
        }
        SynLine()
    }

    fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) -> SynMessage {
        println!("fn exit_message");
        match ctx {
            // message -> Note Message
            CtxMessage::V1 { note, message } => {}
            // message -> Info Message
            CtxMessage::V2 { info, message } => {}
            // message -> Warning Message
            CtxMessage::V3 { warning, message } => {}
            // message -> Error Message
            CtxMessage::V4 { error, message } => {}
            // message -> Header Message
            CtxMessage::V5 { header, message } => {}
        }
        SynMessage()
    }
}

//==============================================================================

pub mod listener_watcher_types {
    /// User-defined type for `log`
    #[derive(Debug, PartialEq)] pub struct SynLog();
    /// User-defined type for `shutdown`
    #[derive(Debug, PartialEq)] pub struct SynShutdown();
    /// User-defined type for `category`
    #[derive(Debug, PartialEq)] pub struct SynCategory();
    /// User-defined type for `open_category`
    #[derive(Debug, PartialEq)] pub struct SynOpenCategory();
    /// User-defined type for `end_category`
    #[derive(Debug, PartialEq)] pub struct SynEndCategory();
    /// User-defined type for `right_recursive`
    #[derive(Debug, PartialEq)] pub struct SynRightRecursive();
    /// User-defined type for `star`
    #[derive(Debug, PartialEq)] pub struct SynStar();
    /// User-defined type for `<L> line` iteration in `star -> ( ►► <L> line ◄◄ )* "end"`
    #[derive(Debug, PartialEq)] pub struct SynStarI();
    /// User-defined type for `line`
    #[derive(Debug, PartialEq)] pub struct SynLine();
    /// User-defined type for `message`
    #[derive(Debug, PartialEq)] pub struct SynMessage();
}

pub mod watcher_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [watcher_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 26;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 57;
    const NBR_STATES: StateId = 72;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         25,  25,  25,  25,  25,  25,  25,  25,  25,  24,   0,  25,  25,   1,  25,  25,   // 0-15
         25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,   // 16-31
         24,  25,  25,   2,  25,  25,  25,  25,  25,  25,  25,  25,  25,  15,  25,  25,   // 32-47
         21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  22,  25,  25,  25,  25,  25,   // 48-63
         25,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   // 64-79
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  25,  25,  25,  25,  21,   // 80-95
         25,   7,  20,   3,  18,   4,  23,  12,  10,   9,  20,  20,  20,  20,   8,  13,   // 96-111
         20,  20,   5,   6,  11,  16,  17,  19,  20,  14,  20,  25,  25,  25,  25,  25,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 25),
        (Seg(57344, 1114111), 25),
    ];
    static TERMINAL_TABLE: [Terminal;15] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(65), pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(65), pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(65), pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(65), pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(65), pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(2), mode_state: Some(24), pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
    ];
    static STATE_TABLE: [StateId; 1873] = [
         57,  58,   1,   2,   3,   4,   5,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 0
         72,  72,  64,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 1
         72,  72,  72,  72,  72,  72,  72,   6,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 2
         72,  72,  72,  72,  72,  72,  72,  72,  20,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 3
         72,  72,  72,  72,  72,  72,  72,  72,  72,  45,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 4
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  49,  40,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 5
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,   7,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 6
         72,  72,  72,  72,   8,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 7
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,   9,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 8
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  10,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 9
         72,  72,  72,  72,  72,  11,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 10
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  59,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 11
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  42,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 12
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  47,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 13
         72,  72,  72,  15,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 14
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  48,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 15
         72,  72,  72,  72,  72,  72,  41,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 16
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  18,  72,  72,  72,  72,  72,  72,  72,  72, // state 17
         72,  72,  72,  72,  60,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 18
         72,  72,  72,  72,  72,  61,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 19
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  62,  72,  72,  72,  72,  72,  72,  72, // state 20
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  46,  72,  72,  72,  72,  72,  72,  72, // state 21
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  23,  72,  72,  72,  72,  72,  72, // state 22
         72,  72,  72,  72,  72,  72,  72,  72,  63,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 23
         72,  72,  72,  25,  26,  25,  25,  25,  28,  27,  25,  25,  25,  25,  25,  72,  25,  25,  25,  29,  25,  72,  72,  25,  72,  72, // state 24
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 25
         72,  72,  72,  25,  25,  51,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 26
         72,  72,  72,  25,  25,  25,  25,  25,  33,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 27
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  30,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 28
         72,  72,  72,  25,  25,  25,  25,  50,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 29
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  31,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 30
         72,  72,  72,  25,  32,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 31
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  67,  25,  72,  72, // state 32
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  55,  72,  72, // state 33
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  68,  25,  72,  72, // state 34
         72,  72,  72,  25,  25,  25,  25,  25,  25,  54,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 35
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  37,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 36
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  69,  25,  72,  72, // state 37
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  70,  25,  72,  72, // state 38
         72,  72,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 39
         72,  72,  72,  72,  72,  72,  72,  19,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 40
         72,  72,  72,  72,  72,  72,  72,  72,  72,  17,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 41
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  13,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 42
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  21,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 43
         72,  72,  72,  72,  14,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 44
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  12,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 45
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  22,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 46
         72,  72,  72,  72,  72,  44,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 47
         72,  72,  72,  72,  72,  16,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 48
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  43,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 49
         72,  72,  72,  25,  25,  53,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 50
         72,  72,  72,  25,  25,  56,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 51
         72,  72,  72,  25,  25,  38,  25,  25,  25,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 52
         72,  72,  72,  25,  25,  25,  25,  25,  35,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 53
         72,  72,  72,  25,  25,  25,  25,  25,  36,  25,  25,  25,  25,  25,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 54
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  34,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 55
         72,  72,  72,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  52,  25,  72,  25,  25,  25,  25,  25,  25,  66,  25,  72,  72, // state 56
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 57 <skip>
         72,  58,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 58 <skip>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 59 <end:0,mode(1,state 65)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 60 <end:1,mode(1,state 65)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 61 <end:2,mode(1,state 65)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 62 <end:3,mode(1,state 65)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72, // state 63 <end:4,mode(1,state 65)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  64,  72, // state 64 <skip,mode(2,state 24)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  65,  72, // state 65 <skip,mode(0,state 0)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  66,  72, // state 66 <end:9,mode(3,state 39)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  67,  72, // state 67 <end:5,mode(3,state 39)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  68,  72, // state 68 <end:6,mode(3,state 39)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  69,  72, // state 69 <end:7,mode(3,state 39)>
         72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  72,  70,  72, // state 70 <end:8,mode(3,state 39)>
         72,  72,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 71 <end:10,mode(0,state 0)>
         72 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [watcher_lexer]
}

pub mod watcher_parser {
    #![allow(unused)]
    // Generated code, don't modify manually anything between the tags below

    // [watcher_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
    use super::listener_watcher_types::*;

    const PARSER_NUM_T: usize = 11;
    const PARSER_NUM_NT: usize = 11;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Category", Some("category")), ("RightRecursive", Some("right-recursive")), ("Star", Some("star")), ("End", Some("end")), ("Shutdown", Some("shutdown")), ("Note", None), ("Info", None), ("Warning", None), ("Error", None), ("Header", None), ("Message", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["log", "shutdown", "open_category", "end_category", "category", "right_recursive", "star", "star_i", "line", "message", "log_1"];
    static ALT_VAR: [VarId; 21] = [0, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10];
    static PARSING_TABLE: [AltId; 132] = [0, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 21, 21, 22, 1, 22, 22, 22, 22, 22, 21, 22, 2, 22, 22, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 21, 21, 3, 22, 21, 21, 21, 21, 21, 21, 22, 22, 4, 5, 21, 22, 21, 21, 21, 21, 21, 21, 22, 22, 21, 21, 7, 6, 6, 6, 6, 6, 6, 21, 22, 22, 21, 21, 8, 8, 8, 8, 8, 8, 8, 21, 22, 21, 21, 21, 10, 9, 9, 9, 9, 9, 9, 21, 21, 21, 21, 21, 22, 12, 11, 11, 11, 11, 11, 21, 21, 21, 21, 21, 22, 22, 13, 14, 15, 16, 17, 21, 21, 19, 21, 21, 21, 18, 21, 21, 21, 21, 21, 21, 20];
    static OPCODES: [&[OpCode]; 21] = [&[OpCode::NT(10), OpCode::Exit(0), OpCode::NT(4), OpCode::NT(2)], &[OpCode::Exit(1), OpCode::T(4)], &[OpCode::Exit(2), OpCode::T(0)], &[OpCode::Exit(3), OpCode::T(3)], &[OpCode::Exit(4), OpCode::NT(5), OpCode::T(1)], &[OpCode::Exit(5), OpCode::NT(6), OpCode::T(2)], &[OpCode::Loop(5), OpCode::Exit(6), OpCode::NT(8)], &[OpCode::Exit(7), OpCode::NT(3)], &[OpCode::Exit(8), OpCode::NT(3), OpCode::NT(7)], &[OpCode::Loop(7), OpCode::Exit(9), OpCode::NT(8)], &[OpCode::Exit(10)], &[OpCode::Exit(11), OpCode::NT(9)], &[OpCode::Exit(12), OpCode::NT(1)], &[OpCode::Exit(13), OpCode::T(10), OpCode::T(5)], &[OpCode::Exit(14), OpCode::T(10), OpCode::T(6)], &[OpCode::Exit(15), OpCode::T(10), OpCode::T(7)], &[OpCode::Exit(16), OpCode::T(10), OpCode::T(8)], &[OpCode::Exit(17), OpCode::T(10), OpCode::T(9)], &[OpCode::Loop(10), OpCode::Exit(18), OpCode::NT(1)], &[OpCode::Loop(10), OpCode::Exit(19), OpCode::NT(4), OpCode::NT(2)], &[OpCode::Exit(20)]];
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
        /// `log -> log shutdown`
        V1 { log: SynLog, shutdown: SynShutdown },
        /// `log -> log open_category category`
        V2 { log: SynLog, open_category: SynOpenCategory, category: SynCategory },
        /// `log -> open_category category`
        V3 { open_category: SynOpenCategory, category: SynCategory },
    }
    #[derive(Debug)]
    pub enum CtxShutdown {
        /// `shutdown -> "shutdown"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxOpenCategory {
        /// `open_category -> "category"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxEndCategory {
        /// `end_category -> "end"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxCategory {
        /// `category -> "right-recursive" right_recursive`
        V1 { right_recursive: SynRightRecursive },
        /// `category -> "star" star`
        V2 { star: SynStar },
    }
    #[derive(Debug)]
    pub enum CtxRightRecursive {
        /// `right_recursive -> <L> line right_recursive`
        V1 { line: SynLine },
        /// `right_recursive -> end_category`
        V2 { end_category: SynEndCategory },
    }
    #[derive(Debug)]
    pub enum CtxStar {
        /// `star -> (<L> line)* end_category`
        V1 { star: SynStarI, end_category: SynEndCategory },
    }
    #[derive(Debug)]
    pub enum CtxStarI {
        /// `<L> line` iteration in `star -> ( ►► <L> line ◄◄ )* end_category`
        V1 { line: SynLine },
    }
    #[derive(Debug)]
    pub enum CtxLine {
        /// `line -> message`
        V1 { message: SynMessage },
        /// `line -> shutdown`
        V2 { shutdown: SynShutdown },
    }
    #[derive(Debug)]
    pub enum CtxMessage {
        /// `message -> Note Message`
        V1 { note: String, message: String },
        /// `message -> Info Message`
        V2 { info: String, message: String },
        /// `message -> Warning Message`
        V3 { warning: String, message: String },
        /// `message -> Error Message`
        V4 { error: String, message: String },
        /// `message -> Header Message`
        V5 { header: String, message: String },
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `log`
    // #[derive(Debug, PartialEq)] pub struct SynLog();
    // /// User-defined type for `shutdown`
    // #[derive(Debug, PartialEq)] pub struct SynShutdown();
    // /// User-defined type for `open_category`
    // #[derive(Debug, PartialEq)] pub struct SynOpenCategory();
    // /// User-defined type for `end_category`
    // #[derive(Debug, PartialEq)] pub struct SynEndCategory();
    // /// User-defined type for `category`
    // #[derive(Debug, PartialEq)] pub struct SynCategory();
    // /// User-defined type for `right_recursive`
    // #[derive(Debug, PartialEq)] pub struct SynRightRecursive();
    // /// User-defined type for `star`
    // #[derive(Debug, PartialEq)] pub struct SynStar();
    // /// User-defined type for `<L> line` iteration in `star -> ( ►► <L> line ◄◄ )* end_category`
    // #[derive(Debug, PartialEq)] pub struct SynStarI();
    // /// User-defined type for `line`
    // #[derive(Debug, PartialEq)] pub struct SynLine();
    // /// User-defined type for `message`
    // #[derive(Debug, PartialEq)] pub struct SynMessage();

    #[derive(Debug)]
    enum SynValue { Log(SynLog), Shutdown(SynShutdown), OpenCategory(SynOpenCategory), EndCategory(SynEndCategory), Category(SynCategory), RightRecursive(SynRightRecursive), Star(SynStar), StarI(SynStarI), Line(SynLine), Message(SynMessage) }

    impl SynValue {
        fn get_log(self) -> SynLog {
            if let SynValue::Log(val) = self { val } else { panic!() }
        }
        fn get_shutdown(self) -> SynShutdown {
            if let SynValue::Shutdown(val) = self { val } else { panic!() }
        }
        fn get_open_category(self) -> SynOpenCategory {
            if let SynValue::OpenCategory(val) = self { val } else { panic!() }
        }
        fn get_end_category(self) -> SynEndCategory {
            if let SynValue::EndCategory(val) = self { val } else { panic!() }
        }
        fn get_category(self) -> SynCategory {
            if let SynValue::Category(val) = self { val } else { panic!() }
        }
        fn get_right_recursive(self) -> SynRightRecursive {
            if let SynValue::RightRecursive(val) = self { val } else { panic!() }
        }
        fn get_star(self) -> SynStar {
            if let SynValue::Star(val) = self { val } else { panic!() }
        }
        fn get_star_i(self) -> SynStarI {
            if let SynValue::StarI(val) = self { val } else { panic!() }
        }
        fn get_line(self) -> SynLine {
            if let SynValue::Line(val) = self { val } else { panic!() }
        }
        fn get_message(self) -> SynMessage {
            if let SynValue::Message(val) = self { val } else { panic!() }
        }
    }

    pub trait WatcherListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, log: SynLog, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_log(&mut self) {}
        fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) -> SynLog;
        #[allow(unused_variables)]
        fn exitloop_log(&mut self, log: &mut SynLog) {}
        fn init_shutdown(&mut self) {}
        fn exit_shutdown(&mut self, ctx: CtxShutdown, spans: Vec<PosSpan>) -> SynShutdown;
        fn init_open_category(&mut self) {}
        fn exit_open_category(&mut self, ctx: CtxOpenCategory, spans: Vec<PosSpan>) -> SynOpenCategory;
        fn init_end_category(&mut self) {}
        fn exit_end_category(&mut self, ctx: CtxEndCategory, spans: Vec<PosSpan>) -> SynEndCategory;
        fn init_category(&mut self) {}
        fn exit_category(&mut self, ctx: CtxCategory, spans: Vec<PosSpan>) -> SynCategory;
        fn init_right_recursive(&mut self) -> SynRightRecursive;
        fn exit_right_recursive(&mut self, acc: &mut SynRightRecursive, ctx: CtxRightRecursive, spans: Vec<PosSpan>);
        fn init_star(&mut self) {}
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar;
        fn init_star_i(&mut self) -> SynStarI;
        fn exit_star_i(&mut self, acc: &mut SynStarI, ctx: CtxStarI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_star_i(&mut self, acc: &mut SynStarI) {}
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

    impl<T: WatcherListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    if matches!(nt, 5 | 7) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_log(),              // log
                        10 => {}                                    // log_1
                        1 => self.listener.init_shutdown(),         // shutdown
                        2 => self.listener.init_open_category(),    // open_category
                        3 => self.listener.init_end_category(),     // end_category
                        4 => self.listener.init_category(),         // category
                        5 => self.init_right_recursive(),           // right_recursive
                        6 => self.listener.init_star(),             // star
                        7 => self.init_star_i(),                    // star_i
                        8 => self.listener.init_line(),             // line
                        9 => self.listener.init_message(),          // message
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.inter_log(),                      // log -> open_category category log_1
                        18 |                                        // log_1 -> shutdown log_1
                        19 => self.exit_log1(alt_id),               // log_1 -> open_category category log_1
                        20 => self.exitloop_log1(),                 // log_1 -> ε
                        1 => self.exit_shutdown(),                  // shutdown -> "shutdown"
                        2 => self.exit_open_category(),             // open_category -> "category"
                        3 => self.exit_end_category(),              // end_category -> "end"
                        4 |                                         // category -> "right-recursive" right_recursive
                        5 => self.exit_category(alt_id),            // category -> "star" star
                        6 |                                         // right_recursive -> <L> line right_recursive
                        7 => self.exit_right_recursive(alt_id),     // right_recursive -> <L> end_category
                        8 => self.exit_star(),                      // star -> star_i end_category
                        9 => self.exit_star_i(),                    // star_i -> <L> line star_i
                        10 => self.exitloop_star_i(),               // star_i -> <L> ε
                        11 |                                        // line -> message
                        12 => self.exit_line(alt_id),               // line -> shutdown
                        13 |                                        // message -> Note Message
                        14 |                                        // message -> Info Message
                        15 |                                        // message -> Warning Message
                        16 |                                        // message -> Error Message
                        17 => self.exit_message(alt_id),            // message -> Header Message
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

    impl<T: WatcherListener> Wrapper<T> {
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

        fn inter_log(&mut self) {
            let category = self.stack.pop().unwrap().get_category();
            let open_category = self.stack.pop().unwrap().get_open_category();
            let ctx = CtxLog::V3 { open_category, category };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_log(ctx, spans);
            self.stack.push(SynValue::Log(val));
        }

        fn exit_log1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                18 => {
                    let shutdown = self.stack.pop().unwrap().get_shutdown();
                    let log = self.stack.pop().unwrap().get_log();
                    (2, CtxLog::V1 { log, shutdown })
                }
                19 => {
                    let category = self.stack.pop().unwrap().get_category();
                    let open_category = self.stack.pop().unwrap().get_open_category();
                    let log = self.stack.pop().unwrap().get_log();
                    (3, CtxLog::V2 { log, open_category, category })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_log1")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_log(ctx, spans);
            self.stack.push(SynValue::Log(val));
        }

        fn exitloop_log1(&mut self) {
            let SynValue::Log(log) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_log(log);
        }

        fn exit_shutdown(&mut self) {
            let ctx = CtxShutdown::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_shutdown(ctx, spans);
            self.stack.push(SynValue::Shutdown(val));
        }

        fn exit_open_category(&mut self) {
            let ctx = CtxOpenCategory::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_open_category(ctx, spans);
            self.stack.push(SynValue::OpenCategory(val));
        }

        fn exit_end_category(&mut self) {
            let ctx = CtxEndCategory::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_end_category(ctx, spans);
            self.stack.push(SynValue::EndCategory(val));
        }

        fn exit_category(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                4 => {
                    let right_recursive = self.stack.pop().unwrap().get_right_recursive();
                    (2, CtxCategory::V1 { right_recursive })
                }
                5 => {
                    let star = self.stack.pop().unwrap().get_star();
                    (2, CtxCategory::V2 { star })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_category")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_category(ctx, spans);
            self.stack.push(SynValue::Category(val));
        }

        fn init_right_recursive(&mut self) {
            let val = self.listener.init_right_recursive();
            self.stack.push(SynValue::RightRecursive(val));
        }

        fn exit_right_recursive(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                6 => {
                    let line = self.stack.pop().unwrap().get_line();
                    (2, CtxRightRecursive::V1 { line })
                }
                7 => {
                    let end_category = self.stack.pop().unwrap().get_end_category();
                    (2, CtxRightRecursive::V2 { end_category })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_right_recursive")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::RightRecursive(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_right_recursive(acc, ctx, spans);
        }

        fn exit_star(&mut self) {
            let end_category = self.stack.pop().unwrap().get_end_category();
            let star = self.stack.pop().unwrap().get_star_i();
            let ctx = CtxStar::V1 { star, end_category };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_star(ctx, spans);
            self.stack.push(SynValue::Star(val));
        }

        fn init_star_i(&mut self) {
            let val = self.listener.init_star_i();
            self.stack.push(SynValue::StarI(val));
        }

        fn exit_star_i(&mut self) {
            let line = self.stack.pop().unwrap().get_line();
            let ctx = CtxStarI::V1 { line };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(SynValue::StarI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_star_i(acc, ctx, spans);
        }

        fn exitloop_star_i(&mut self) {
            let SynValue::StarI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_star_i(acc);
        }

        fn exit_line(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                11 => {
                    let message = self.stack.pop().unwrap().get_message();
                    (1, CtxLine::V1 { message })
                }
                12 => {
                    let shutdown = self.stack.pop().unwrap().get_shutdown();
                    (1, CtxLine::V2 { shutdown })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_line")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_line(ctx, spans);
            self.stack.push(SynValue::Line(val));
        }

        fn exit_message(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                13 => {
                    let message = self.stack_t.pop().unwrap();
                    let note = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V1 { note, message })
                }
                14 => {
                    let message = self.stack_t.pop().unwrap();
                    let info = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V2 { info, message })
                }
                15 => {
                    let message = self.stack_t.pop().unwrap();
                    let warning = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V3 { warning, message })
                }
                16 => {
                    let message = self.stack_t.pop().unwrap();
                    let error = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V4 { error, message })
                }
                17 => {
                    let message = self.stack_t.pop().unwrap();
                    let header = self.stack_t.pop().unwrap();
                    (2, CtxMessage::V5 { header, message })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_message")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_message(ctx, spans);
            self.stack.push(SynValue::Message(val));
        }
    }

    // [watcher_parser]
}
