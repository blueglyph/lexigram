// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::cell::RefCell;
use std::io::Read;
use std::rc::Rc;
use lexigram_core::char_reader::CharReader;
use lexigram_core::CollectJoin;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::{Parser, Terminate};
use watcher_lexer::build_lexer;
use watcher_parser::*;

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
## warning:  4
## error: ERROR
end

shutdown

some garbage
"#;

static TXT2: &str = r#"
category star
## info: 1
## info: 2
## info: 3
## error:FATAL ERROR!
shutdown

some garbage
"#;

static TXT3: &str = r#"category star
## warning: 1
## warning: 2
end"#;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;
const VERBOSE_TRACE: bool = false;

#[test]
fn test_watcher() {
    let tests: Vec<(&str, Vec<&str>, Vec<&str>)> = vec![
        (
            TXT1,
            vec![
                "right-recursive,N,first note",
                "right-recursive,N,second note",
                "right-recursive,W,incoming danger!",
                "right-recursive,C,custom_header: ,#\"!!&",
                "right-recursive,I,incoming danger",
                "right-recursive,E,ERROR 1",
                "star,I,new category", "star,W,1", "star,W,2", "star,W,3", "star,W,4", "star,E,ERROR",
                "-,S"],
            vec![],
        ),
        (
            TXT2,
            vec![
                "star,I,1", "star,I,2", "star,I,3", "star,E,FATAL ERROR!", "star,S"],
            vec![],
        ),
        (
            TXT3,
            vec![
                "star,W,1", "star,W,2"],
            vec![],
        ),
    ];
    for (test_id, (txt, expected_messages, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80} {test_id}\n{txt}\n{0:-<80}", ""); }
        let mut parser = WatcherParser::new();
        match parser.parse(txt) {
            Ok(ParserData { log, messages, trace }) => {
                if VERBOSE {
                    println!("messages: {}", messages.iter().map(|s| format!("{s:?}")).join(", "));
                    println!("trace:{}", trace.into_iter().map(|s| format!("\n{s}")).join(""));
                    println!("parsing successful\n{log}");
                }
                assert_eq!(messages, expected_messages, "var mismatch in test {test_id}");
            }
            Err(ParserData { log, messages, trace }) => {
                if VERBOSE {
                    println!("messages: {}", messages.iter().map(|s| format!("{s:?}")).join(", "));
                    println!("trace:{}", trace.into_iter().map(|s| format!("\n{s}")).join(""));
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

// logged reader, used to monitor when lines are fetched and when
// they're parsed, to show the latency.

struct LoggedReader<'t> {
    text: &'t str,
    pos: usize,
    cursor: usize,
    trace: Rc<RefCell<Vec<String>>>,
}

impl<'t> LoggedReader<'t> {
    const STR_BEFORE_ANSI: &'static str = "\u{1b}[35m";
    const STR_AFTER_ANSI: &'static str = "\u{1b}[0m";

    fn new<'o: 't>(text: &'o str) -> Self {
        if VERBOSE_TRACE { println!("{}\nnew trace:{}", Self::STR_BEFORE_ANSI, Self::STR_AFTER_ANSI); }
        LoggedReader {
            text,
            pos: 0,
            cursor: 0,
            trace: Rc::new(RefCell::new(vec![])),
        }
    }

    fn output(&mut self, len: usize) -> &[u8] {
       let pos = self.pos;

        // 0 1 2 3 4 5 6 7 8 9 0 1 2 3     0 1 2 3 4 5 6 7 8 9 0 1 2 3
        // a b c \ d e f g \ h i \ j k     a b c \ d e f g \ h i \ j k
        //   ^-- pos = 1                                         ^--pos = 11
        //   --------------> len = 8                             --> len = 2
        // ^ cursor = 0 =>   ^ cursor = 9                          ^ cursor = 12 => cursor = 14

        while self.cursor < pos + len {
            if let Some(nbytes) = self.text[self.cursor..].as_bytes().iter().position(|b| *b == b'\n') {
                if VERBOSE_TRACE { println!("{}> {:?} {}", Self::STR_BEFORE_ANSI, &self.text[self.cursor..self.cursor + nbytes], Self::STR_AFTER_ANSI); }
                self.cursor += nbytes + 1;
            } else {
                if VERBOSE_TRACE { println!("{}> {:?} {}", Self::STR_BEFORE_ANSI, &self.text[self.cursor..], Self::STR_AFTER_ANSI); }
                self.cursor = self.text.len();
            }
        }
        self.pos += len;
        &self.text[pos..pos + len].as_bytes()
    }
}

impl Read for LoggedReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let len = buf.len();
        let left = self.text.len() - self.pos;
        if left >= len {
            buf[..len].copy_from_slice(self.output(len));
            Ok(len)
        } else {
            buf[..left].copy_from_slice(self.output(left));
            Ok(left)
        }
    }
}

// parser

#[derive(Debug)]
struct ParserData {
    log: BufLog,
    messages: Vec<String>,
    trace: Vec<String>,
}

struct WatcherParser<'l, 'p, 'lr> {
    lexer: Lexer<'l, LoggedReader<'lr>>,
    parser: Parser<'p>,
    wrapper: Option<Wrapper<Listener>>,
}

impl<'lr, 'l: 'lr> WatcherParser<'l, '_, 'lr> {
    /// Creates a new parser
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        WatcherParser { lexer, parser, wrapper: None }
    }

    pub fn parse<'o: 'lr>(&mut self, text: &'o str) -> Result<ParserData, ParserData> {
        let log_reader = LoggedReader::new(text);
        let trace = log_reader.trace.clone();
        let stream = CharReader::new(log_reader);
        self.wrapper = Some(Wrapper::new(Listener::new(trace), VERBOSE_WRAPPER));
        self.lexer.attach_stream(stream);
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(self.wrapper.as_mut().unwrap(), tokens) {
            self.wrapper.as_mut().unwrap().get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let Listener { log, messages, trace, .. } = self.wrapper.take().unwrap().give_listener();
        let t = trace.take();
        if log.has_no_errors() {
            Ok(ParserData { log, messages, trace: t })
        } else {
            Err(ParserData { log, messages, trace: t })
        }
    }
}

// listener

struct Listener {
    log: BufLog,
    messages: Vec<String>,
    abort: Terminate,
    curr_category: Option<String>,
    trace: Rc<RefCell<Vec<String>>>,
}

impl Listener {
    fn new(trace: Rc<RefCell<Vec<String>>>) -> Self {
        Listener {
            log: BufLog::new(),
            messages: vec![],
            abort: Terminate::None,
            curr_category: None,
            trace,
        }
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
        self.log.add_note("aborted");
        self.messages.push(format!(
            "{},{}",
            self.curr_category.take().unwrap_or_else(|| "-".to_string()),
            if terminate == Terminate::Conclude { 'S' } else { 'A' }));
    }

    fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) {
        match ctx {
            // log -> log shutdown
            CtxLog::V1 => { /* shutdown processed in `shutdown` nonterminal */ }
            // log -> log "category" category
            CtxLog::V2 => {}
            // log -> "category" category
            CtxLog::V3 => {}
        }
    }

    fn init_shutdown(&mut self) {
        // We intercept the shutdown in the init rather than the exit callback because it
        // prevents the parser from fetching a new token.
        //
        // The parser follows this sequence at the end:
        //   ("shutdown" is already the next token delivered by the lexer)
        //   - EXIT  log_1 -> open_category category log_1
        //   - LOOP  log_1 -> shutdown log_1
        //   - ENTER shutdown -> "shutdown"
        //      => calls this function
        //   - MATCH shutdown
        //      => removes the "shutdown" token
        //   (>>> fetches next token <<<)
        //   - EXIT  shutdown -> "shutdown"
        //      => calls exit_shutdown(...)
        //
        // Another way is to use the exit_shutdown(...) callback with the parser in "delay_stream_interception"
        // mode by enabling that feature. It delays fetching the next token as long as the parser is processing
        // an EXIT opcode, which is the case here after popping the MATCH:
        //
        //   - ENTER shutdown -> "shutdown"
        //      => calls this function
        //   - MATCH shutdown
        //      => removes the "shutdown" token
        //   (next op = EXIT => doesn't fetch the next token yet)
        //   - EXIT  shutdown -> "shutdown"
        //      => calls exit_shutdown(...)
        //   (would normally fetch the token here, but the abort has stopped the parser loop)

        self.abort = Terminate::Conclude;
    }

    #[cfg(any())] // see explanation above in init_shutdown(...)
    fn exit_shutdown(&mut self, ctx: CtxShutdown, spans: Vec<PosSpan>) {
        // shutdown -> "shutdown"
        let CtxShutdown::V1 = ctx;
        self.abort = Terminate::Conclude;
    }

    fn exit_open_category(&mut self, ctx: CtxOpenCategory, spans: Vec<PosSpan>) {
        if let Some(cat) = &self.curr_category {
            let span = spans.iter().fold(PosSpan::empty(), |acc, s| acc + s);
            self.log.add_error(format!("{span}: missing end for category {cat} when opening new category"));
        }
    }

    fn exit_end_category(&mut self, ctx: CtxEndCategory, spans: Vec<PosSpan>) {
        if self.curr_category.is_none() {
            let span = spans.iter().fold(PosSpan::empty(), |acc, s| acc + s);
            self.log.add_error(format!("{span}: 'end' encountered outside any category"));
        }
        self.curr_category = None;
    }

    fn exit_category(&mut self, ctx: CtxCategory, spans: Vec<PosSpan>) {
        match ctx {
            // category -> "right-recursive" right_recursive
            CtxCategory::V1 { .. } => {}
            // category -> "star" star
            CtxCategory::V2 { .. } => {}
        }
    }

    fn init_right_recursive(&mut self) {
        self.curr_category = Some("right-recursive".to_string());
    }

    fn exit_right_recursive(&mut self, ctx: CtxRightRecursive, spans: Vec<PosSpan>) {
        match ctx {
            // right_recursive -> <L> line right_recursive
            CtxRightRecursive::V1 => {}
            // right_recursive -> "end"
            CtxRightRecursive::V2 => {}
        }
    }

    fn init_star(&mut self) {
        self.curr_category = Some("star".to_string());
    }

    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) {
        // star -> (<L> line)* "end"
        let CtxStar::V1 = ctx;
    }

    fn init_star_i(&mut self) {
    }

    fn exit_star_i(&mut self, ctx: CtxStarI, spans: Vec<PosSpan>) {
        // `<L> line` iteration in `star -> ( ►► <L> line ◄◄ )* "end"`
        let CtxStarI::V1 = ctx;
    }

    fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) {
        match ctx {
            // line -> message
            CtxLine::V1 => {}
            // line -> shutdown
            CtxLine::V2 => { /* shutdown processed in `shutdown` nonterminal */ }
        }
    }

    fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) {
        if let Some(category) = &self.curr_category {
            let msg_opt = match ctx {
                // message -> Note Message
                CtxMessage::V1 { note, message } => Some(format!("{category},N,{message}")),
                // message -> Info Message
                CtxMessage::V2 { info, message } => Some(format!("{category},I,{message}")),
                // message -> Warning Message
                CtxMessage::V3 { warning, message } => Some(format!("{category},W,{message}")),
                // message -> Error Message
                CtxMessage::V4 { error, message } => Some(format!("{category},E,{message}")),
                // message -> Header Message
                CtxMessage::V5 { header, message } => Some(format!("{category},C,{header},{message}")),
            };
            if let Some(msg) = msg_opt {
                const STR_BEFORE_ANSI: &str = "\u{1b}[36m";
                const STR_AFTER_ANSI: &str = "\u{1b}[0m";
                if VERBOSE_TRACE { println!("{STR_BEFORE_ANSI}< {msg}{STR_AFTER_ANSI}"); }
                self.trace.borrow_mut().push(msg.clone());
                self.messages.push(msg);
            }
        } else {
            self.log.add_error(format!("out-of-category message: {ctx:?}"));
        }
    }
}

//==============================================================================

pub mod watcher_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [watcher_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 25;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 57;
    const NBR_STATES: StateId = 71;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         24,  24,  24,  24,  24,  24,  24,  24,  24,  23,   0,  24,  24,   0,  24,  24,   // 0-15
         24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,   // 16-31
         23,  24,  24,   1,  24,  24,  24,  24,  24,  24,  24,  24,  24,  14,  24,  24,   // 32-47
         20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  21,  24,  24,  24,  24,  24,   // 48-63
         24,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,   // 64-79
         19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  24,  24,  24,  24,  20,   // 80-95
         24,   6,  19,   2,  17,   3,  22,  11,   9,   8,  19,  19,  19,  19,   7,  12,   // 96-111
         19,  19,   4,   5,  10,  15,  16,  18,  19,  13,  19,  24,  24,  24,  24,  24,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 24),
        (Seg(57344, 1114111), 24),
    ];
    static TERMINAL_TABLE: [Terminal;14] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(64), pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(64), pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(64), pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(64), pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(64), pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(2), mode_state: Some(24), pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::Mode(3), mode_state: Some(39), pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::Mode(0), mode_state: Some(0), pop: false },
    ];
    static STATE_TABLE: [StateId; 1776] = [
         57,   1,   2,   3,   4,   5,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 0
         71,  63,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 1
         71,  71,  71,  71,  71,  71,   6,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 2
         71,  71,  71,  71,  71,  71,  71,  20,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 3
         71,  71,  71,  71,  71,  71,  71,  71,  45,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 4
         71,  71,  71,  71,  71,  71,  71,  71,  71,  49,  40,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 5
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,   7,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 6
         71,  71,  71,   8,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 7
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,   9,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 8
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  10,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 9
         71,  71,  71,  71,  11,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 10
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  58,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 11
         71,  71,  71,  71,  71,  71,  71,  71,  71,  42,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 12
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  47,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 13
         71,  71,  15,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 14
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  48,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 15
         71,  71,  71,  71,  71,  41,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 16
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  18,  71,  71,  71,  71,  71,  71,  71,  71, // state 17
         71,  71,  71,  59,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 18
         71,  71,  71,  71,  60,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 19
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  61,  71,  71,  71,  71,  71,  71,  71, // state 20
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  46,  71,  71,  71,  71,  71,  71,  71, // state 21
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  23,  71,  71,  71,  71,  71,  71, // state 22
         71,  71,  71,  71,  71,  71,  71,  62,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 23
         71,  71,  25,  26,  25,  25,  25,  28,  27,  25,  25,  25,  25,  25,  71,  25,  25,  25,  29,  25,  71,  71,  25,  71,  71, // state 24
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 25
         71,  71,  25,  25,  51,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 26
         71,  71,  25,  25,  25,  25,  25,  33,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 27
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  30,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 28
         71,  71,  25,  25,  25,  25,  50,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 29
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  31,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 30
         71,  71,  25,  32,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 31
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  66,  25,  71,  71, // state 32
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  55,  71,  71, // state 33
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  67,  25,  71,  71, // state 34
         71,  71,  25,  25,  25,  25,  25,  25,  54,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 35
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  37,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 36
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  68,  25,  71,  71, // state 37
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  69,  25,  71,  71, // state 38
         71,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70, // state 39
         71,  71,  71,  71,  71,  71,  19,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 40
         71,  71,  71,  71,  71,  71,  71,  71,  17,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 41
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  13,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 42
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  21,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 43
         71,  71,  71,  14,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 44
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  12,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 45
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  22,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 46
         71,  71,  71,  71,  44,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 47
         71,  71,  71,  71,  16,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 48
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  43,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 49
         71,  71,  25,  25,  53,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 50
         71,  71,  25,  25,  56,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 51
         71,  71,  25,  25,  38,  25,  25,  25,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 52
         71,  71,  25,  25,  25,  25,  25,  35,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 53
         71,  71,  25,  25,  25,  25,  25,  36,  25,  25,  25,  25,  25,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 54
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  34,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 55
         71,  71,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  52,  25,  71,  25,  25,  25,  25,  25,  25,  65,  25,  71,  71, // state 56
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 57 <skip>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 58 <end:0,mode(1,state 64)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 59 <end:1,mode(1,state 64)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 60 <end:2,mode(1,state 64)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 61 <end:3,mode(1,state 64)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71, // state 62 <end:4,mode(1,state 64)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  63,  71, // state 63 <skip,mode(2,state 24)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  64,  71, // state 64 <skip,mode(0,state 0)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  65,  71, // state 65 <end:9,mode(3,state 39)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  66,  71, // state 66 <end:5,mode(3,state 39)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  67,  71, // state 67 <end:6,mode(3,state 39)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  68,  71, // state 68 <end:7,mode(3,state 39)>
         71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  71,  69,  71, // state 69 <end:8,mode(3,state 39)>
         71,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70,  70, // state 70 <end:10,mode(0,state 0)>
         71 // error group in [nbr_state * nbr_group + nbr_group]
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
        V1,
        /// `log -> log open_category category`
        V2,
        /// `log -> open_category category`
        V3,
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
        V1,
        /// `category -> "star" star`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxRightRecursive {
        /// `right_recursive -> <L> line right_recursive`
        V1,
        /// `right_recursive -> end_category`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxStar {
        /// `star -> (<L> line)* end_category`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxStarI {
        /// `<L> line` iteration in `star -> ( ►► <L> line ◄◄ )* end_category`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxLine {
        /// `line -> message`
        V1,
        /// `line -> shutdown`
        V2,
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

    /// Top non-terminal Log (has no value)
    #[derive(Debug, PartialEq)]
    pub struct SynLog();

    #[derive(Debug)]
    enum SynValue {  }

    pub trait WatcherListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_log(&mut self) {}
        #[allow(unused_variables)]
        fn exit_log(&mut self, ctx: CtxLog, spans: Vec<PosSpan>) {}
        fn init_shutdown(&mut self) {}
        #[allow(unused_variables)]
        fn exit_shutdown(&mut self, ctx: CtxShutdown, spans: Vec<PosSpan>) {}
        fn init_open_category(&mut self) {}
        #[allow(unused_variables)]
        fn exit_open_category(&mut self, ctx: CtxOpenCategory, spans: Vec<PosSpan>) {}
        fn init_end_category(&mut self) {}
        #[allow(unused_variables)]
        fn exit_end_category(&mut self, ctx: CtxEndCategory, spans: Vec<PosSpan>) {}
        fn init_category(&mut self) {}
        #[allow(unused_variables)]
        fn exit_category(&mut self, ctx: CtxCategory, spans: Vec<PosSpan>) {}
        fn init_right_recursive(&mut self) {}
        #[allow(unused_variables)]
        fn exit_right_recursive(&mut self, ctx: CtxRightRecursive, spans: Vec<PosSpan>) {}
        fn init_star(&mut self) {}
        #[allow(unused_variables)]
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) {}
        fn init_star_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_star_i(&mut self, ctx: CtxStarI, spans: Vec<PosSpan>) {}
        fn init_line(&mut self) {}
        #[allow(unused_variables)]
        fn exit_line(&mut self, ctx: CtxLine, spans: Vec<PosSpan>) {}
        fn init_message(&mut self) {}
        #[allow(unused_variables)]
        fn exit_message(&mut self, ctx: CtxMessage, spans: Vec<PosSpan>) {}
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
                        5 => self.listener.init_right_recursive(),  // right_recursive
                        6 => self.listener.init_star(),             // star
                        7 => self.listener.init_star_i(),           // star_i
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
                        20 => {}                                    // log_1 -> ε (not used)
                        1 => self.exit_shutdown(),                  // shutdown -> "shutdown"
                        2 => self.exit_open_category(),             // open_category -> "category"
                        3 => self.exit_end_category(),              // end_category -> "end"
                        4 |                                         // category -> "right-recursive" right_recursive
                        5 => self.exit_category(alt_id),            // category -> "star" star
                        6 |                                         // right_recursive -> <L> line right_recursive
                        7 => self.exit_right_recursive(alt_id),     // right_recursive -> <L> end_category
                        8 => self.exit_star(),                      // star -> star_i end_category
                        9 => self.exit_star_i(),                    // star_i -> <L> line star_i
                        10 => {}                                    // star_i -> <L> ε (not used)
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
                            let span = self.stack_span.pop().unwrap();
                            self.listener.exit(span);
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
            let ctx = CtxLog::V3;
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_log(ctx, spans);
        }

        fn exit_log1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                18 => {
                    (2, CtxLog::V1)
                }
                19 => {
                    (3, CtxLog::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_log1")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_log(ctx, spans);
        }

        fn exit_shutdown(&mut self) {
            let ctx = CtxShutdown::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_shutdown(ctx, spans);
        }

        fn exit_open_category(&mut self) {
            let ctx = CtxOpenCategory::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_open_category(ctx, spans);
        }

        fn exit_end_category(&mut self) {
            let ctx = CtxEndCategory::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_end_category(ctx, spans);
        }

        fn exit_category(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                4 => {
                    (2, CtxCategory::V1)
                }
                5 => {
                    (2, CtxCategory::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_category")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_category(ctx, spans);
        }

        fn exit_right_recursive(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                6 => {
                    (2, CtxRightRecursive::V1)
                }
                7 => {
                    (2, CtxRightRecursive::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_right_recursive")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_right_recursive(ctx, spans);
        }

        fn exit_star(&mut self) {
            let ctx = CtxStar::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_star(ctx, spans);
        }

        fn exit_star_i(&mut self) {
            let ctx = CtxStarI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_star_i(ctx, spans);
        }

        fn exit_line(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                11 => {
                    (1, CtxLine::V1)
                }
                12 => {
                    (1, CtxLine::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_line")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_line(ctx, spans);
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
            self.listener.exit_message(ctx, spans);
        }
    }

    // [watcher_parser]
}
