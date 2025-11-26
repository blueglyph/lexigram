// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::io::Cursor;
use lexigram_lib::CollectJoin;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_lib::log::{BufLog, LogStatus, Logger};
use lexigram_lib::parser::Parser;
use crate::listener_types::*;
use crate::pandemonium_lexer::build_lexer;
use crate::pandemonium_parser::*;

const VERBOSE_WRAPPER: bool = false;

static TXT1: &str = r#"
star    Alpha   = 101, 110, 150;
plus    Bravo   = 102, 120, 250;
l-star  Charlie = 103, 130, 350;
l-plus  Delta   = 104, 140, 450;
rrec    Echo    = 105, 150, 550;
l-rrec  Foxtrot = 106, 160, 650;
lrec    Golf    = 107, 170, 750;
amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;
"#;

fn main() {
    println!("{:=<80}\n{TXT1}\n{0:-<80}", "");
    match PanDemo::parse_text(TXT1.to_string()) {
        Ok(log) => println!("parsing successful\n{log}"),
        Err(log) => panic!("errors during parsing:\n{log}"),
    }
}

// -------------------------------------------------------------------------
// minimalist parser, top level

pub struct PanDemo<'l, 'p> {
    lexer: Lexer<'l, Cursor<String>>,
    parser: Parser<'p>,
    wrapper: Wrapper<PanDemoListener>,
}

impl PanDemo<'_, '_> {
    pub fn parse_text(text: String) -> Result<BufLog, BufLog> {
        let mcalc = PanDemo::new();
        mcalc.parse(text)
    }

    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        let wrapper = Wrapper::new(PanDemoListener::new(), VERBOSE_WRAPPER);
        PanDemo { lexer, parser, wrapper }
    }

    pub fn parse(mut self, text: String) -> Result<BufLog, BufLog> {
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(&mut self.wrapper, tokens) {
            self.wrapper.get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let log = std::mem::take(&mut self.wrapper.get_listener_mut().log);
        if log.has_no_errors() {
            let listener = self.wrapper.give_listener();
            println!("Spans:\n{}", listener.spans.into_iter().map(|s| format!("- {s}")).join("\n"));
            Ok(log)
        } else {
            Err(log)
        }
    }
}

// listener implementation

struct PanDemoListener {
    log: BufLog,
    abort: bool,
    spans: Vec<String>
}

impl PanDemoListener {
    fn new() -> Self {
        PanDemoListener {
            log: BufLog::new(),
            abort: false,
            spans: vec![],
        }
    }
}

#[allow(unused)]
impl PandemoniumListener for PanDemoListener {
    fn check_abort_request(&self) -> bool {
        self.abort
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit(&mut self, text: SynText, span: PosSpan) {
    }

    fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText {
        self.spans.push(format!("exit_text({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxText::V1 => {} // text -> (<L> example)*
        }
        SynText()
    }

    fn exit_i(&mut self, ctx: CtxI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxI::V1 { example: SynExample() } => {}
        }
    }

    fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample {
        self.spans.push(format!("exit_example({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxExample::V1 { star: SynStar() } => {}
            CtxExample::V2 { plus: SynPlus() } => {}
            CtxExample::V3 { l_star: SynLStar() } => {}
            CtxExample::V4 { l_plus: SynLPlus() } => {}
            CtxExample::V5 { rrec: SynRrec() } => {}
            CtxExample::V6 { l_rrec: SynLRrec() } => {}
            CtxExample::V7 { lrec: SynLrec() } => {}
            CtxExample::V8 { amb: SynAmb() } => {}
        }
        SynExample()
    }

    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar {
        self.spans.push(format!("exit_star({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxStar::V1 { id, num, star: SynStar1(star) } => {}
        }
        SynStar()
    }

    fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus {
        self.spans.push(format!("exit_plus({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxPlus::V1 { id, num, plus: SynPlus1(plus) } => {}
        }
        SynPlus()
    }

    fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar {
        self.spans.push(format!("exit_l_star({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLStar::V1 { id, num } => {}
        }
        SynLStar()
    }

    fn exit_l_star_i(&mut self, ctx: CtxLStarI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLStarI::V1 { num } => {}
        }
    }

    fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus {
        self.spans.push(format!("exit_l_plus({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLPlus::V1 { id, num } => {}
        }
        SynLPlus()
    }

    fn exit_l_plus_i(&mut self, ctx: CtxLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLPlusI::V1 { num, last_iteration } => {}
        }
    }

    fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxRrec::V1 { id, num, rrec_i: SynRrecI() } => {}
        }
        SynRrec()
    }

    fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) -> SynLRrec {
        self.spans.push(format!("exit_l_rrec({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLRrec::V1 { id, num, l_rrec_i: SynLRrecI() } => {}
        }
        SynLRrec()
    }

    fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec {
        self.spans.push(format!("exit_lrec({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLrec::V1 { id, lrec_i: SynLrecI() } => {}
        }
        SynLrec()
    }

    fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb {
        self.spans.push(format!("exit_amb({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxAmb::V1 { id, amb_i: SynAmbI() } => {}
        }
        SynAmb()
    }

    fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI {
        self.spans.push(format!("exit_rrec_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxRrecI::V1 { num, rrec_i: SynRrecI() } => {}
            CtxRrecI::V2 => {}
        }
        SynRrecI()
    }

    fn init_l_rrec_i(&mut self) -> SynLRrecI {
        SynLRrecI()
    }

    fn exit_l_rrec_i(&mut self, ctx: CtxLRrecI, spans: Vec<PosSpan>) -> SynLRrecI {
        self.spans.push(format!("exit_l_rrec_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLRrecI::V1 { l_rrec_i: SynLRrecI(), num } => {}
            CtxLRrecI::V2 { l_rrec_i: SynLRrecI() } => {}
        }
        SynLRrecI()
    }

    fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI {
        self.spans.push(format!("exit_lrec_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            CtxLrecI::V1 { lrec_i: SynLrecI(), num } => {}
            CtxLrecI::V2 { num } => {}
        }
        SynLrecI()
    }

    fn exitloop_lrec_i(&mut self, _lrec_i: &mut SynLrecI) {
    }

    fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI {
        self.spans.push(format!("exit_amb_i({})", spans.into_iter().map(|s| s.to_string()).join(", ")));
        match ctx {
            // `amb_i -> <R> amb_i "^" amb_i`
            CtxAmbI::V1 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i "*" amb_i`
            CtxAmbI::V2 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i <P> "/" amb_i`
            CtxAmbI::V3 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i "+" amb_i`
            CtxAmbI::V4 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> amb_i <P> "-" amb_i`
            CtxAmbI::V5 { amb_i: [SynAmbI(), SynAmbI()] } => {}
            // `amb_i -> "-" amb_i`
            CtxAmbI::V6 { amb_i: SynAmbI() } => {}
            // `amb_i -> "(" amb_i ")"`
            CtxAmbI::V7 { amb_i: SynAmbI() } => {}
            // `amb_i -> Id`
            CtxAmbI::V8 { id } => {}
            // `amb_i -> Num`
            CtxAmbI::V9 { num } => {}
        }
        SynAmbI()
    }
}

// -------------------------------------------------------------------------
// User types used in the listener interface:
// (initially copied/uncommented from the generated parser code)

pub mod listener_types {
    /// User-defined type for `text`
    #[derive(Debug, PartialEq)] pub struct SynText();
    /// User-defined type for `example`
    #[derive(Debug, PartialEq)] pub struct SynExample();
    /// User-defined type for `star`
    #[derive(Debug, PartialEq)] pub struct SynStar();
    /// User-defined type for `plus`
    #[derive(Debug, PartialEq)] pub struct SynPlus();
    /// User-defined type for `l_star`
    #[derive(Debug, PartialEq)] pub struct SynLStar();
    /// User-defined type for `l_plus`
    #[derive(Debug, PartialEq)] pub struct SynLPlus();
    /// User-defined type for `rrec`
    #[derive(Debug, PartialEq)] pub struct SynRrec();
    /// User-defined type for `l_rrec`
    #[derive(Debug, PartialEq)] pub struct SynLRrec();
    /// User-defined type for `lrec`
    #[derive(Debug, PartialEq)] pub struct SynLrec();
    /// User-defined type for `amb`
    #[derive(Debug, PartialEq)] pub struct SynAmb();
    /// User-defined type for `rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynRrecI();
    /// User-defined type for `l_rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynLRrecI();
    /// User-defined type for `lrec_i`
    #[derive(Debug, PartialEq)] pub struct SynLrecI();
    /// User-defined type for `amb_i`
    #[derive(Debug, PartialEq)] pub struct SynAmbI();
}

// -------------------------------------------------------------------------

pub mod pandemonium_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [pandemonium_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_lib::dfa::{StateId, Terminal, ActionOption, ModeOption};
    use lexigram_lib::lexer::Lexer;
    use lexigram_lib::lexergen::GroupId;
    use lexigram_lib::segments::{Seg, SegMap};

    const NBR_GROUPS: u32 = 28;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 13;
    const NBR_STATES: StateId = 50;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         24,  24,  24,  24,  24,  24,  24,  24,  24,   0,  26,  24,  24,  26,  24,  24,   // 0-15
         24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,   // 16-31
          0,  24,  24,  24,  24,  24,  24,  24,   1,   2,   3,   4,   5,   6,  24,   7,   // 32-47
         18,   8,   8,   8,   8,   8,   8,   8,   8,   8,  24,   9,  24,  10,  24,  24,   // 48-63
         24,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,   // 64-79
         22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  24,  24,  24,  12,  23,   // 80-95
         24,  13,  27,  21,  22,  20,  22,  22,  22,  22,  22,  22,  14,  25,  22,  22,   // 96-111
         15,  22,  16,  17,  11,  19,  22,  22,  22,  22,  22,  24,  24,  24,  24,  24,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 24),
        (Seg(57344, 1114111), 24),
    ];
    static TERMINAL_TABLE: [Terminal;37] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 1401] = [
         13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  50,  24,  24,  24,  24,  50,  50,  24,  13,  24, // state 0
          1,   1,   1,  12,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   3,   4,   5,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 2
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   8,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 3
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  10,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 4
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   6,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 5
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   7,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 6
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  39,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 7
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,   9,  50,  50,  50,  50,  50,  50,  50,  50, // state 8
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  40,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 9
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  11,  50,  50,  50,  50,  50,  50,  50, // state 10
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  41,  50,  50,  50,  50,  50,  50, // state 11
          1,   1,   1,  12,   1,   1,   1,  49,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 12
         13,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  13,  50, // state 13 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 14 <end:4>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 15 <end:6>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 16 <end:5>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 17 <end:0>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 18 <end:8>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 19 <end:7>
         50,  50,  50,   1,  50,  50,  50,  31,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 20 <end:1>
         50,  50,  50,  50,  50,  50,  50,  50,  21,  50,  50,  50,  50,  50,  50,  50,  50,  50,  21,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 21 <end:19>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 22 <end:9>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 23 <end:2>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 24 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 25 <end:3>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  47,  50,  24, // state 26 <end:18>
         50,  50,  50,  50,  50,  50,   2,  50,  24,  50,  50,  24,  50,  24,  24,  24,  38,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 27 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  35,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 28 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  42,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 29 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  32,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 30 <end:18>
         31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,  50,  31, // state 31 <skip>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  33,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 32 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  34,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 33 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 34 <end:10>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  36,  24,  24,  24,  24,  50,  24,  50,  24, // state 35 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  37,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 36 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 37 <end:11>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  45,  24,  24,  24,  50,  24,  50,  24, // state 38 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 39 <end:12>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 40 <end:13>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 41 <end:14>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  43,  24,  24,  24,  50,  24,  50,  24, // state 42 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  44,  24,  24,  50,  24,  50,  24, // state 43 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 44 <end:15>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  46,  24,  24,  50,  24,  50,  24, // state 45 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 46 <end:16>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  48, // state 47 <end:18>
         50,  50,  50,  50,  50,  50,  50,  50,  24,  50,  50,  24,  50,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  50,  24,  50,  24, // state 48 <end:17>
         50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50,  50, // state 49 <skip>
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

    // [pandemonium_lexer]
}

// -------------------------------------------------------------------------

pub mod pandemonium_parser {
    // Generated code, don't modify manually anything between the tags below

    // [pandemonium_parser]

    use lexigram_lib::{CollectJoin, FixedSymTable, grammar::{AltId, Alternative, Symbol, VarId}, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 20;
    const PARSER_NUM_NT: usize = 28;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Div", Some("/")), ("Equal", Some("=")), ("Exp", Some("^")), ("Lpar", Some("(")), ("Mul", Some("*")), ("Rpar", Some(")")), ("Sub", Some("-")), ("Comma", Some(",")), ("Semi", Some(";")), ("Star", Some("star")), ("Plus", Some("plus")), ("L_Star", Some("l-star")), ("L_Plus", Some("l-plus")), ("L_Rrec", Some("l-rrec")), ("Rrec", Some("rrec")), ("Lrec", Some("lrec")), ("Amb", Some("amb")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["text", "i", "example", "star", "plus", "l_star", "l_star_i", "l_plus", "l_plus_i", "rrec", "l_rrec", "lrec", "amb", "rrec_i", "l_rrec_i", "lrec_i", "amb_i", "star_1", "plus_1", "lrec_i_1", "amb_i_1", "amb_i_2", "amb_i_3", "amb_i_4", "amb_i_5", "amb_i_6", "l_plus_1", "plus_2"];
    static ALT_VAR: [VarId; 55] = [0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 13, 14, 14, 15, 16, 17, 17, 18, 19, 19, 20, 20, 20, 20, 20, 20, 21, 22, 22, 22, 22, 23, 24, 24, 25, 25, 25, 25, 26, 26, 27, 27];
    static ALTERNATIVES: [&[Symbol]; 55] = [&[Symbol::NT(1)], &[Symbol::NT(2), Symbol::NT(1)], &[Symbol::Empty], &[Symbol::T(10), Symbol::NT(3)], &[Symbol::T(11), Symbol::NT(4)], &[Symbol::T(12), Symbol::NT(5)], &[Symbol::T(13), Symbol::NT(7)], &[Symbol::T(15), Symbol::NT(9)], &[Symbol::T(14), Symbol::NT(10)], &[Symbol::T(16), Symbol::NT(11)], &[Symbol::T(17), Symbol::NT(12)], &[Symbol::T(18), Symbol::T(2), Symbol::T(19), Symbol::NT(17), Symbol::T(9)], &[Symbol::T(18), Symbol::T(2), Symbol::T(19), Symbol::NT(18), Symbol::T(9)], &[Symbol::T(18), Symbol::T(2), Symbol::T(19), Symbol::NT(6), Symbol::T(9)], &[Symbol::T(8), Symbol::T(19), Symbol::NT(6)], &[Symbol::Empty], &[Symbol::T(18), Symbol::T(2), Symbol::T(19), Symbol::NT(8), Symbol::T(9)], &[Symbol::T(8), Symbol::T(19), Symbol::NT(26)], &[Symbol::T(18), Symbol::T(2), Symbol::T(19), Symbol::NT(13)], &[Symbol::T(18), Symbol::T(2), Symbol::T(19), Symbol::NT(14)], &[Symbol::T(18), Symbol::T(2), Symbol::NT(15), Symbol::T(9)], &[Symbol::T(18), Symbol::T(2), Symbol::NT(16), Symbol::T(9)], &[Symbol::T(8), Symbol::T(19), Symbol::NT(13)], &[Symbol::T(9)], &[Symbol::T(8), Symbol::T(19), Symbol::NT(14)], &[Symbol::T(9)], &[Symbol::T(19), Symbol::NT(19)], &[Symbol::NT(25), Symbol::NT(20)], &[Symbol::T(8), Symbol::T(19), Symbol::NT(17)], &[Symbol::Empty], &[Symbol::T(8), Symbol::T(19), Symbol::NT(27)], &[Symbol::T(8), Symbol::T(19), Symbol::NT(19)], &[Symbol::Empty], &[Symbol::T(3), Symbol::NT(23), Symbol::NT(20)], &[Symbol::T(5), Symbol::NT(23), Symbol::NT(20)], &[Symbol::T(1), Symbol::NT(23), Symbol::NT(20)], &[Symbol::T(0), Symbol::NT(21), Symbol::NT(20)], &[Symbol::T(7), Symbol::NT(21), Symbol::NT(20)], &[Symbol::Empty], &[Symbol::NT(25), Symbol::NT(22)], &[Symbol::T(3), Symbol::NT(23), Symbol::NT(22)], &[Symbol::T(5), Symbol::NT(23), Symbol::NT(22)], &[Symbol::T(1), Symbol::NT(23), Symbol::NT(22)], &[Symbol::Empty], &[Symbol::NT(25), Symbol::NT(24)], &[Symbol::T(3), Symbol::NT(23), Symbol::NT(24)], &[Symbol::Empty], &[Symbol::T(7), Symbol::NT(16)], &[Symbol::T(4), Symbol::NT(16), Symbol::T(6)], &[Symbol::T(18)], &[Symbol::T(19)], &[Symbol::NT(8)], &[Symbol::Empty], &[Symbol::NT(18)], &[Symbol::Empty]];
    static PARSING_TABLE: [AltId; 588] = [55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 0, 0, 0, 0, 0, 0, 0, 0, 55, 55, 0, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 1, 1, 1, 1, 1, 1, 1, 1, 55, 55, 2, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 3, 4, 5, 6, 8, 7, 9, 10, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 11, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 12, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 13, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 14, 15, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 16, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 17, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 18, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 19, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 20, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 21, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 22, 23, 56, 56, 56, 56, 56, 56, 56, 56, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 24, 25, 56, 56, 56, 56, 56, 56, 56, 56, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 26, 55, 56, 56, 55, 56, 27, 56, 56, 27, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 27, 27, 55, 55, 55, 55, 55, 55, 55, 55, 55, 28, 29, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 30, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 31, 32, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 36, 35, 55, 33, 55, 34, 38, 37, 55, 38, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 55, 56, 39, 56, 56, 39, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 39, 39, 55, 43, 42, 55, 40, 55, 41, 43, 43, 55, 43, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 55, 56, 44, 56, 56, 44, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 44, 44, 55, 46, 46, 55, 45, 55, 46, 46, 46, 55, 46, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 55, 56, 48, 56, 56, 47, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 49, 50, 55, 55, 55, 55, 55, 55, 55, 55, 55, 51, 52, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 53, 54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55];
    static OPCODES: [&[OpCode]; 55] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3), OpCode::T(10)], &[OpCode::Exit(4), OpCode::NT(4), OpCode::T(11)], &[OpCode::Exit(5), OpCode::NT(5), OpCode::T(12)], &[OpCode::Exit(6), OpCode::NT(7), OpCode::T(13)], &[OpCode::Exit(7), OpCode::NT(9), OpCode::T(15)], &[OpCode::Exit(8), OpCode::NT(10), OpCode::T(14)], &[OpCode::Exit(9), OpCode::NT(11), OpCode::T(16)], &[OpCode::Exit(10), OpCode::NT(12), OpCode::T(17)], &[OpCode::Exit(11), OpCode::T(9), OpCode::NT(17), OpCode::T(19), OpCode::T(2), OpCode::T(18)], &[OpCode::Exit(12), OpCode::T(9), OpCode::NT(18), OpCode::T(19), OpCode::T(2), OpCode::T(18)], &[OpCode::Exit(13), OpCode::T(9), OpCode::NT(6), OpCode::T(19), OpCode::T(2), OpCode::T(18)], &[OpCode::Loop(6), OpCode::Exit(14), OpCode::T(19), OpCode::T(8)], &[OpCode::Exit(15)], &[OpCode::Exit(16), OpCode::T(9), OpCode::NT(8), OpCode::T(19), OpCode::T(2), OpCode::T(18)], &[OpCode::NT(26), OpCode::T(19), OpCode::T(8)], &[OpCode::Exit(18), OpCode::NT(13), OpCode::T(19), OpCode::T(2), OpCode::T(18)], &[OpCode::Exit(19), OpCode::NT(14), OpCode::T(19), OpCode::T(2), OpCode::T(18)], &[OpCode::Exit(20), OpCode::T(9), OpCode::NT(15), OpCode::T(2), OpCode::T(18)], &[OpCode::Exit(21), OpCode::T(9), OpCode::NT(16), OpCode::T(2), OpCode::T(18)], &[OpCode::Exit(22), OpCode::NT(13), OpCode::T(19), OpCode::T(8)], &[OpCode::Exit(23), OpCode::T(9)], &[OpCode::Loop(14), OpCode::Exit(24), OpCode::T(19), OpCode::T(8)], &[OpCode::Exit(25), OpCode::T(9)], &[OpCode::NT(19), OpCode::Exit(26), OpCode::T(19)], &[OpCode::NT(20), OpCode::Exit(27), OpCode::NT(25)], &[OpCode::Loop(17), OpCode::Exit(28), OpCode::T(19), OpCode::T(8)], &[OpCode::Exit(29)], &[OpCode::NT(27), OpCode::T(19), OpCode::T(8)], &[OpCode::Loop(19), OpCode::Exit(31), OpCode::T(19), OpCode::T(8)], &[OpCode::Exit(32)], &[OpCode::Loop(20), OpCode::Exit(33), OpCode::NT(23), OpCode::T(3)], &[OpCode::Loop(20), OpCode::Exit(34), OpCode::NT(23), OpCode::T(5)], &[OpCode::Loop(20), OpCode::Exit(35), OpCode::NT(23), OpCode::T(1)], &[OpCode::Loop(20), OpCode::Exit(36), OpCode::NT(21), OpCode::T(0)], &[OpCode::Loop(20), OpCode::Exit(37), OpCode::NT(21), OpCode::T(7)], &[OpCode::Exit(38)], &[OpCode::NT(22), OpCode::Exit(39), OpCode::NT(25)], &[OpCode::Loop(22), OpCode::Exit(40), OpCode::NT(23), OpCode::T(3)], &[OpCode::Loop(22), OpCode::Exit(41), OpCode::NT(23), OpCode::T(5)], &[OpCode::Loop(22), OpCode::Exit(42), OpCode::NT(23), OpCode::T(1)], &[OpCode::Exit(43)], &[OpCode::NT(24), OpCode::Exit(44), OpCode::NT(25)], &[OpCode::Loop(24), OpCode::Exit(45), OpCode::NT(23), OpCode::T(3)], &[OpCode::Exit(46)], &[OpCode::Exit(47), OpCode::NT(16), OpCode::T(7)], &[OpCode::Exit(48), OpCode::T(6), OpCode::NT(16), OpCode::T(4)], &[OpCode::Exit(49), OpCode::T(18)], &[OpCode::Exit(50), OpCode::T(19)], &[OpCode::Loop(8), OpCode::Exit(51)], &[OpCode::Exit(52)], &[OpCode::Loop(18), OpCode::Exit(53)], &[OpCode::Exit(54)]];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            ALTERNATIVES.into_iter().map(|s| Alternative::new(s.to_vec())).collect(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }

    #[derive(Debug)]
    pub enum CtxText {
        /// `text -> (<L> example)*`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxI {
        /// `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )*`
        V1 { example: SynExample },
    }
    #[derive(Debug)]
    pub enum CtxExample {
        /// `example -> "star" star`
        V1 { star: SynStar },
        /// `example -> "plus" plus`
        V2 { plus: SynPlus },
        /// `example -> "l-star" l_star`
        V3 { l_star: SynLStar },
        /// `example -> "l-plus" l_plus`
        V4 { l_plus: SynLPlus },
        /// `example -> "rrec" rrec`
        V5 { rrec: SynRrec },
        /// `example -> "l-rrec" l_rrec`
        V6 { l_rrec: SynLRrec },
        /// `example -> "lrec" lrec`
        V7 { lrec: SynLrec },
        /// `example -> "amb" amb`
        V8 { amb: SynAmb },
    }
    #[derive(Debug)]
    pub enum CtxStar {
        /// `star -> Id "=" Num ("," Num)* ";"`
        V1 { id: String, num: String, star: SynStar1 },
    }
    #[derive(Debug)]
    pub enum CtxPlus {
        /// `plus -> Id "=" Num ("," Num)+ ";"`
        V1 { id: String, num: String, plus: SynPlus1 },
    }
    #[derive(Debug)]
    pub enum CtxLStar {
        /// `l_star -> Id "=" Num (<L> "," Num)* ";"`
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxLStarI {
        /// `<L> "," Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," Num ◄◄ )* ";"`
        V1 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlus {
        /// `l_plus -> Id "=" Num (<L> "," Num)+ ";"`
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlusI {
        /// `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
        V1 { num: String, last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxRrec {
        /// `rrec -> Id "=" Num rrec_i`
        V1 { id: String, num: String, rrec_i: SynRrecI },
    }
    #[derive(Debug)]
    pub enum CtxLRrec {
        /// `l_rrec -> Id "=" Num l_rrec_i`
        V1 { id: String, num: String, l_rrec_i: SynLRrecI },
    }
    #[derive(Debug)]
    pub enum CtxLrec {
        /// `lrec -> Id "=" lrec_i ";"`
        V1 { id: String, lrec_i: SynLrecI },
    }
    #[derive(Debug)]
    pub enum CtxAmb {
        /// `amb -> Id "=" amb_i ";"`
        V1 { id: String, amb_i: SynAmbI },
    }
    #[derive(Debug)]
    pub enum CtxRrecI {
        /// `rrec_i -> "," Num rrec_i`
        V1 { num: String, rrec_i: SynRrecI },
        /// `rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxLRrecI {
        /// `l_rrec_i -> <L> "," Num l_rrec_i`
        V1 { l_rrec_i: SynLRrecI, num: String },
        /// `l_rrec_i -> ";"`
        V2 { l_rrec_i: SynLRrecI },
    }
    #[derive(Debug)]
    pub enum CtxLrecI {
        /// `lrec_i -> lrec_i "," Num`
        V1 { lrec_i: SynLrecI, num: String },
        /// `lrec_i -> Num`
        V2 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxAmbI {
        /// `amb_i -> <R> amb_i "^" amb_i`
        V1 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i "*" amb_i`
        V2 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i <P> "/" amb_i`
        V3 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i "+" amb_i`
        V4 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i <P> "-" amb_i`
        V5 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> "-" amb_i`
        V6 { amb_i: SynAmbI },
        /// `amb_i -> "(" amb_i ")"`
        V7 { amb_i: SynAmbI },
        /// `amb_i -> Id`
        V8 { id: String },
        /// `amb_i -> Num`
        V9 { num: String },
    }

    // NT types and user-defined type templates (copy elsewhere and uncomment when necessary):

    // /// User-defined type for `text`
    // #[derive(Debug, PartialEq)] pub struct SynText();
    // /// User-defined type for `example`
    // #[derive(Debug, PartialEq)] pub struct SynExample();
    // /// User-defined type for `star`
    // #[derive(Debug, PartialEq)] pub struct SynStar();
    // /// User-defined type for `plus`
    // #[derive(Debug, PartialEq)] pub struct SynPlus();
    // /// User-defined type for `l_star`
    // #[derive(Debug, PartialEq)] pub struct SynLStar();
    // /// User-defined type for `l_plus`
    // #[derive(Debug, PartialEq)] pub struct SynLPlus();
    // /// User-defined type for `rrec`
    // #[derive(Debug, PartialEq)] pub struct SynRrec();
    // /// User-defined type for `l_rrec`
    // #[derive(Debug, PartialEq)] pub struct SynLRrec();
    // /// User-defined type for `lrec`
    // #[derive(Debug, PartialEq)] pub struct SynLrec();
    // /// User-defined type for `amb`
    // #[derive(Debug, PartialEq)] pub struct SynAmb();
    // /// User-defined type for `rrec_i`
    // #[derive(Debug, PartialEq)] pub struct SynRrecI();
    // /// User-defined type for `l_rrec_i`
    // #[derive(Debug, PartialEq)] pub struct SynLRrecI();
    // /// User-defined type for `lrec_i`
    // #[derive(Debug, PartialEq)] pub struct SynLrecI();
    // /// User-defined type for `amb_i`
    // #[derive(Debug, PartialEq)] pub struct SynAmbI();
    /// Computed `("," Num)*` array in `star -> Id "=" Num  ►► ("," Num)* ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynStar1(pub Vec<String>);
    /// Computed `("," Num)+` array in `plus -> Id "=" Num  ►► ("," Num)+ ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynPlus1(pub Vec<String>);

    #[derive(Debug)]
    enum SynValue { Text(SynText), Example(SynExample), Star(SynStar), Plus(SynPlus), LStar(SynLStar), LPlus(SynLPlus), Rrec(SynRrec), LRrec(SynLRrec), Lrec(SynLrec), Amb(SynAmb), RrecI(SynRrecI), LRrecI(SynLRrecI), LrecI(SynLrecI), AmbI(SynAmbI), Star1(SynStar1), Plus1(SynPlus1) }

    impl SynValue {
        fn get_text(self) -> SynText {
            if let SynValue::Text(val) = self { val } else { panic!() }
        }
        fn get_example(self) -> SynExample {
            if let SynValue::Example(val) = self { val } else { panic!() }
        }
        fn get_star(self) -> SynStar {
            if let SynValue::Star(val) = self { val } else { panic!() }
        }
        fn get_plus(self) -> SynPlus {
            if let SynValue::Plus(val) = self { val } else { panic!() }
        }
        fn get_l_star(self) -> SynLStar {
            if let SynValue::LStar(val) = self { val } else { panic!() }
        }
        fn get_l_plus(self) -> SynLPlus {
            if let SynValue::LPlus(val) = self { val } else { panic!() }
        }
        fn get_rrec(self) -> SynRrec {
            if let SynValue::Rrec(val) = self { val } else { panic!() }
        }
        fn get_l_rrec(self) -> SynLRrec {
            if let SynValue::LRrec(val) = self { val } else { panic!() }
        }
        fn get_lrec(self) -> SynLrec {
            if let SynValue::Lrec(val) = self { val } else { panic!() }
        }
        fn get_amb(self) -> SynAmb {
            if let SynValue::Amb(val) = self { val } else { panic!() }
        }
        fn get_rrec_i(self) -> SynRrecI {
            if let SynValue::RrecI(val) = self { val } else { panic!() }
        }
        fn get_l_rrec_i(self) -> SynLRrecI {
            if let SynValue::LRrecI(val) = self { val } else { panic!() }
        }
        fn get_lrec_i(self) -> SynLrecI {
            if let SynValue::LrecI(val) = self { val } else { panic!() }
        }
        fn get_amb_i(self) -> SynAmbI {
            if let SynValue::AmbI(val) = self { val } else { panic!() }
        }
        fn get_star1(self) -> SynStar1 {
            if let SynValue::Star1(val) = self { val } else { panic!() }
        }
        fn get_plus1(self) -> SynPlus1 {
            if let SynValue::Plus1(val) = self { val } else { panic!() }
        }
    }

    pub trait PandemoniumListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> bool { false }
        fn get_mut_log(&mut self) -> &mut impl Logger;
        #[allow(unused)]
        fn exit(&mut self, text: SynText, span: PosSpan) {}
        fn init_text(&mut self) {}
        fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText;
        fn init_i(&mut self) {}
        #[allow(unused)]
        fn exit_i(&mut self, ctx: CtxI, spans: Vec<PosSpan>) {}
        fn init_example(&mut self) {}
        fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample;
        fn init_star(&mut self) {}
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar;
        fn init_plus(&mut self) {}
        fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus;
        fn init_l_star(&mut self) {}
        fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar;
        fn init_l_star_i(&mut self) {}
        #[allow(unused)]
        fn exit_l_star_i(&mut self, ctx: CtxLStarI, spans: Vec<PosSpan>) {}
        fn init_l_plus(&mut self) {}
        fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus;
        fn init_l_plus_i(&mut self) {}
        #[allow(unused)]
        fn exit_l_plus_i(&mut self, ctx: CtxLPlusI, spans: Vec<PosSpan>) {}
        fn init_rrec(&mut self) {}
        fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec;
        fn init_l_rrec(&mut self) {}
        fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) -> SynLRrec;
        fn init_lrec(&mut self) {}
        fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec;
        fn init_amb(&mut self) {}
        fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb;
        fn init_rrec_i(&mut self) {}
        fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI;
        fn init_l_rrec_i(&mut self) -> SynLRrecI;
        fn exit_l_rrec_i(&mut self, ctx: CtxLRrecI, spans: Vec<PosSpan>) -> SynLRrecI;
        fn init_lrec_i(&mut self) {}
        fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI;
        #[allow(unused)]
        fn exitloop_lrec_i(&mut self, lrec_i: &mut SynLrecI) {}
        fn init_amb_i(&mut self) {}
        fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<SynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
        stack_span: Vec<PosSpan>,
    }

    impl<T: PandemoniumListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    if matches!(nt, 1 | 6 | 8 | 14 | 17 | 18) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_text(),             // text
                        1 => self.listener.init_i(),                // i
                        2 => self.listener.init_example(),          // example
                        3 => self.listener.init_star(),             // star
                        17 => self.init_star1(),                    // star_1
                        4 => self.listener.init_plus(),             // plus
                        18 => self.init_plus1(),                    // plus_1
                        27 => {}                                    // plus_2
                        5 => self.listener.init_l_star(),           // l_star
                        6 => self.listener.init_l_star_i(),         // l_star_i
                        7 => self.listener.init_l_plus(),           // l_plus
                        8 => self.listener.init_l_plus_i(),         // l_plus_i
                        26 => {}                                    // l_plus_1
                        9 => self.listener.init_rrec(),             // rrec
                        10 => self.listener.init_l_rrec(),          // l_rrec
                        11 => self.listener.init_lrec(),            // lrec
                        12 => self.listener.init_amb(),             // amb
                        13 => self.listener.init_rrec_i(),          // rrec_i
                        14 => self.init_l_rrec_i(),                 // l_rrec_i
                        15 => self.listener.init_lrec_i(),          // lrec_i
                        19 => {}                                    // lrec_i_1
                        16 => self.listener.init_amb_i(),           // amb_i
                        20 ..= 25 => {}                             // amb_i_1, amb_i_2, amb_i_3, amb_i_4, amb_i_5, amb_i_6
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_text(),                      // text -> i
                        1 => self.exit_i(),                         // i -> <L> example i
                        2 => {}                                     // i -> <L> ε (not used)
                        3 |                                         // example -> "star" star
                        4 |                                         // example -> "plus" plus
                        5 |                                         // example -> "l-star" l_star
                        6 |                                         // example -> "l-plus" l_plus
                        7 |                                         // example -> "rrec" rrec
                        8 |                                         // example -> "l-rrec" l_rrec
                        9 |                                         // example -> "lrec" lrec
                        10 => self.exit_example(alt_id),            // example -> "amb" amb
                        11 => self.exit_star(),                     // star -> Id "=" Num star_1 ";"
                        28 => self.exit_star1(),                    // star_1 -> "," Num star_1
                        29 => {}                                    // star_1 -> ε
                        12 => self.exit_plus(),                     // plus -> Id "=" Num plus_1 ";"
                        53 |                                        // plus_2 -> plus_1
                        54 => self.exit_plus1(),                    // plus_2 -> ε
                     /* 30 */                                       // plus_1 -> "," Num plus_2 (never called)
                        13 => self.exit_l_star(),                   // l_star -> Id "=" Num l_star_i ";"
                        14 => self.exit_l_star_i(),                 // l_star_i -> <L> "," Num l_star_i
                        15 => {}                                    // l_star_i -> <L> ε (not used)
                        16 => self.exit_l_plus(),                   // l_plus -> Id "=" Num l_plus_i ";"
                        51 |                                        // l_plus_1 -> l_plus_i
                        52 => self.exit_l_plus_i(alt_id),           // l_plus_1 -> ε
                     /* 17 */                                       // l_plus_i -> <L> "," Num l_plus_1 (never called)
                        18 => self.exit_rrec(),                     // rrec -> Id "=" Num rrec_i
                        19 => self.exit_l_rrec(),                   // l_rrec -> Id "=" Num l_rrec_i
                        20 => self.exit_lrec(),                     // lrec -> Id "=" lrec_i ";"
                        21 => self.exit_amb(),                      // amb -> Id "=" amb_i ";"
                        22 |                                        // rrec_i -> "," Num rrec_i
                        23 => self.exit_rrec_i(alt_id),             // rrec_i -> ";"
                        24 |                                        // l_rrec_i -> <L> "," Num l_rrec_i
                        25 => self.exit_l_rrec_i(alt_id),           // l_rrec_i -> <L> ";"
                        26 => self.inter_lrec_i(),                  // lrec_i -> Num lrec_i_1
                        31 => self.exit_lrec_i1(),                  // lrec_i_1 -> "," Num lrec_i_1
                        32 => self.exitloop_lrec_i1(),              // lrec_i_1 -> ε
                        33 |                                        // amb_i_1 -> <R> "^" amb_i_4 amb_i_1
                        34 |                                        // amb_i_1 -> "*" amb_i_4 amb_i_1
                        35 |                                        // amb_i_1 -> "/" amb_i_4 amb_i_1
                        36 |                                        // amb_i_1 -> "+" amb_i_2 amb_i_1
                        37 => self.exit_amb_i1(alt_id),             // amb_i_1 -> "-" amb_i_2 amb_i_1
                        40 |                                        // amb_i_3 -> <R> "^" amb_i_4 amb_i_3 (duplicate of 33)
                        45 => self.exit_amb_i1(33),                 // amb_i_5 -> <R> "^" amb_i_4 amb_i_5 (duplicate of 33)
                        41 => self.exit_amb_i1(34),                 // amb_i_3 -> "*" amb_i_4 amb_i_3 (duplicate of 34)
                        42 => self.exit_amb_i1(35),                 // amb_i_3 -> "/" amb_i_4 amb_i_3 (duplicate of 35)
                        47 |                                        // amb_i_6 -> "-" amb_i
                        48 |                                        // amb_i_6 -> "(" amb_i ")"
                        49 |                                        // amb_i_6 -> Id
                        50 => self.exit_amb_i6(alt_id),             // amb_i_6 -> Num
                        27 => {}                                    // amb_i -> amb_i_6 amb_i_1 (not used)
                        38 => {}                                    // amb_i_1 -> ε (not used)
                        39 => {}                                    // amb_i_2 -> amb_i_6 amb_i_3 (not used)
                        43 => {}                                    // amb_i_3 -> ε (not used)
                        44 => {}                                    // amb_i_4 -> amb_i_6 amb_i_5 (not used)
                        46 => {}                                    // amb_i_5 -> ε (not used)
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
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).join(", "));
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
    }

    impl<T: PandemoniumListener> Wrapper<T> {
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
            let text = self.stack.pop().unwrap().get_text();
            let span = self.stack_span.pop().unwrap();
            self.listener.exit(text, span);
        }

        fn exit_text(&mut self) {
            let ctx = CtxText::V1;
            let n = 1;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_text(ctx, spans);
            self.stack.push(SynValue::Text(val));
        }

        fn exit_i(&mut self) {
            let example = self.stack.pop().unwrap().get_example();
            let ctx = CtxI::V1 { example };
            let n = 2;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_i(ctx, spans);
        }

        fn exit_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                3 => {
                    let star = self.stack.pop().unwrap().get_star();
                    (2, CtxExample::V1 { star })
                }
                4 => {
                    let plus = self.stack.pop().unwrap().get_plus();
                    (2, CtxExample::V2 { plus })
                }
                5 => {
                    let l_star = self.stack.pop().unwrap().get_l_star();
                    (2, CtxExample::V3 { l_star })
                }
                6 => {
                    let l_plus = self.stack.pop().unwrap().get_l_plus();
                    (2, CtxExample::V4 { l_plus })
                }
                7 => {
                    let rrec = self.stack.pop().unwrap().get_rrec();
                    (2, CtxExample::V5 { rrec })
                }
                8 => {
                    let l_rrec = self.stack.pop().unwrap().get_l_rrec();
                    (2, CtxExample::V6 { l_rrec })
                }
                9 => {
                    let lrec = self.stack.pop().unwrap().get_lrec();
                    (2, CtxExample::V7 { lrec })
                }
                10 => {
                    let amb = self.stack.pop().unwrap().get_amb();
                    (2, CtxExample::V8 { amb })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_example")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_example(ctx, spans);
            self.stack.push(SynValue::Example(val));
        }

        fn exit_star(&mut self) {
            let star = self.stack.pop().unwrap().get_star1();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxStar::V1 { id, num, star };
            let n = 5;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_star(ctx, spans);
            self.stack.push(SynValue::Star(val));
        }

        fn init_star1(&mut self) {
            let val = SynStar1(Vec::new());
            self.stack.push(SynValue::Star1(val));
        }

        fn exit_star1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let n = 3;
            let Some(SynValue::Star1(SynStar1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynStar1 item on wrapper stack");
            };
            star_acc.push(num);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
        }

        fn exit_plus(&mut self) {
            let plus = self.stack.pop().unwrap().get_plus1();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxPlus::V1 { id, num, plus };
            let n = 5;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_plus(ctx, spans);
            self.stack.push(SynValue::Plus(val));
        }

        fn init_plus1(&mut self) {
            let val = SynPlus1(Vec::new());
            self.stack.push(SynValue::Plus1(val));
        }

        fn exit_plus1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let n = 3;
            let Some(SynValue::Plus1(SynPlus1(plus_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynPlus1 item on wrapper stack");
            };
            plus_acc.push(num);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
        }

        fn exit_l_star(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStar::V1 { id, num };
            let n = 5;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_l_star(ctx, spans);
            self.stack.push(SynValue::LStar(val));
        }

        fn exit_l_star_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLStarI::V1 { num };
            let n = 3;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_l_star_i(ctx, spans);
        }

        fn exit_l_plus(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlus::V1 { id, num };
            let n = 5;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_l_plus(ctx, spans);
            self.stack.push(SynValue::LPlus(val));
        }

        fn exit_l_plus_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 52;
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusI::V1 { num, last_iteration };
            let n = 3;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_l_plus_i(ctx, spans);
        }

        fn exit_rrec(&mut self) {
            let rrec_i = self.stack.pop().unwrap().get_rrec_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxRrec::V1 { id, num, rrec_i };
            let n = 4;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_rrec(ctx, spans);
            self.stack.push(SynValue::Rrec(val));
        }

        fn exit_l_rrec(&mut self) {
            let l_rrec_i = self.stack.pop().unwrap().get_l_rrec_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLRrec::V1 { id, num, l_rrec_i };
            let n = 4;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_l_rrec(ctx, spans);
            self.stack.push(SynValue::LRrec(val));
        }

        fn exit_lrec(&mut self) {
            let lrec_i = self.stack.pop().unwrap().get_lrec_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLrec::V1 { id, lrec_i };
            let n = 4;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_lrec(ctx, spans);
            self.stack.push(SynValue::Lrec(val));
        }

        fn exit_amb(&mut self) {
            let amb_i = self.stack.pop().unwrap().get_amb_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxAmb::V1 { id, amb_i };
            let n = 4;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_amb(ctx, spans);
            self.stack.push(SynValue::Amb(val));
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                22 => {
                    let rrec_i = self.stack.pop().unwrap().get_rrec_i();
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num, rrec_i })
                }
                23 => {
                    (1, CtxRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_rrec_i(ctx, spans);
            self.stack.push(SynValue::RrecI(val));
        }

        fn init_l_rrec_i(&mut self) {
            let val = self.listener.init_l_rrec_i();
            self.stack.push(SynValue::LRrecI(val));
        }

        fn exit_l_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                24 => {
                    let num = self.stack_t.pop().unwrap();
                    let l_rrec_i = self.stack.pop().unwrap().get_l_rrec_i();
                    (3, CtxLRrecI::V1 { l_rrec_i, num })
                }
                25 => {
                    let l_rrec_i = self.stack.pop().unwrap().get_l_rrec_i();
                    (2, CtxLRrecI::V2 { l_rrec_i })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_l_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_l_rrec_i(ctx, spans);
            self.stack.push(SynValue::LRrecI(val));
        }

        fn inter_lrec_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLrecI::V2 { num };
            let n = 1;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(SynValue::LrecI(val));
        }

        fn exit_lrec_i1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let lrec_i = self.stack.pop().unwrap().get_lrec_i();
            let ctx = CtxLrecI::V1 { lrec_i, num };
            let n = 3;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(SynValue::LrecI(val));
        }

        fn exitloop_lrec_i1(&mut self) {
            let SynValue::LrecI(lrec_i) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_lrec_i(lrec_i);
        }

        fn exit_amb_i1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                33 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V1 { amb_i: [amb_i_1, amb_i_2] })
                }
                34 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V2 { amb_i: [amb_i_1, amb_i_2] })
                }
                35 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V3 { amb_i: [amb_i_1, amb_i_2] })
                }
                36 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V4 { amb_i: [amb_i_1, amb_i_2] })
                }
                37 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V5 { amb_i: [amb_i_1, amb_i_2] })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_amb_i1")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(SynValue::AmbI(val));
        }

        fn exit_amb_i6(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                47 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (2, CtxAmbI::V6 { amb_i })
                }
                48 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V7 { amb_i })
                }
                49 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V8 { id })
                }
                50 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V9 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_amb_i6")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(SynValue::AmbI(val));
        }
    }

    // [pandemonium_parser]
}

// -------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pandemonium() {
        main();
    }
}
