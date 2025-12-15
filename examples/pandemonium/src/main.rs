// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::io::Cursor;
use lexigram_core::CollectJoin;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, Pos, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Parser;
use crate::listener_types::*;
use crate::pandemonium_lexer::build_lexer;
use crate::pandemonium_parser::*;

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

fn main() {
    if VERBOSE { println!("{:=<80}\n{TXT1}\n{0:-<80}", ""); }
    let mut demo = PanDemo::new();
    match demo.parse(TXT1) {
        Ok((log, spans)) => {
            if VERBOSE {
                println!("parsing successful\n{log}");
                println!("Spans:\n{}", spans.iter().map(|s| format!("- {s}")).join("\n"));
            }
            assert_eq!(spans, SPANS1);
        },
        Err(log) => panic!("errors during parsing:\n{log}"),
    }
}

/// Text to parse
static TXT1: &str = r#"
star    Alpha   = 101, 110, 150;
plus    Bravo   = 102, 120, 250;
l-star  Charlie = 103, 130, 350;
l-plus  Delta   = 104, 140, 450;
rrec    Echo    = 105, 150, 550;
l-rrec  Foxtrot = 106, 160, 650;
lrec    Golf    = 107, 170, 750;
amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;

star-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];
plus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];
l-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];
l-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];

star    Mike     = 201;
l-star  November = 202;
rrec    Oscar    = 203;
l-rrec  Papa     = 204;
lrec    Quebec   = 205;
"#;

/// Expected spans collected when parsing TXT1
static SPANS1: &[&str] = &[
    r#"exit_star("Alpha", "=", "101", ", 110, 150", ";")"#,
    r#"exit_example("star", "Alpha   = 101, 110, 150;")"#,
    r#"exit_i("", "star    Alpha   = 101, 110, 150;")"#,
    r#"exit_plus("Bravo", "=", "102", ", 120, 250", ";")"#,
    r#"exit_example("plus", "Bravo   = 102, 120, 250;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;", "plus    Bravo   = 102, 120, 250;")"#,
    r#"exit_l_star_i("", ",", "130")"#,
    r#"exit_l_star_i(", 130", ",", "350")"#,
    r#"exit_l_star("Charlie", "=", "103", ", 130, 350", ";")"#,
    r#"exit_example("l-star", "Charlie = 103, 130, 350;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nplus    Bravo   = 102, 120, 250;", "l-star  Charlie = 103, 130, 350;")"#,
    r#"exit_l_plus_i("", ",", "140")"#,
    r#"exit_l_plus_i(", 140", ",", "450")"#,
    r#"exit_l_plus("Delta", "=", "104", ", 140, 450", ";")"#,
    r#"exit_example("l-plus", "Delta   = 104, 140, 450;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-star  Charlie = 103, 130, 350;", "l-plus  Delta   = 104, 140, 450;")"#,
    r#"exit_rrec_i(";")"#,
    r#"exit_rrec_i(",", "550", ";")"#,
    r#"exit_rrec_i(",", "150", ", 550;")"#,
    r#"exit_rrec("Echo", "=", "105", ", 150, 550;")"#,
    r#"exit_example("rrec", "Echo    = 105, 150, 550;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nl-plus  Delta   = 104, 140, 450;", "rrec    Echo    = 105, 150, 550;")"#,
    r#"exit_l_rrec_i("", ",", "160")"#,
    r#"exit_l_rrec_i(", 160", ",", "650")"#,
    r#"exit_l_rrec_i(", 160, 650", ";")"#,
    r#"exit_l_rrec("Foxtrot", "=", "106", ", 160, 650;")"#,
    r#"exit_example("l-rrec", "Foxtrot = 106, 160, 650;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nrrec    Echo    = 105, 150, 550;", "l-rrec  Foxtrot = 106, 160, 650;")"#,
    r#"exit_lrec_i("107")"#,
    r#"exit_lrec_i("107", ",", "170")"#,
    r#"exit_lrec_i("107, 170", ",", "750")"#,
    r#"exit_lrec("Golf", "=", "107, 170, 750", ";")"#,
    r#"exit_example("lrec", "Golf    = 107, 170, 750;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nl-rrec  Foxtrot = 106, 160, 650;", "lrec    Golf    = 107, 170, 750;")"#,
    r#"exit_amb_i("5")"#,
    r#"exit_amb_i("2")"#,
    r#"exit_amb_i("6")"#,
    r#"exit_amb_i("3")"#,
    r#"exit_amb_i("2")"#,
    r#"exit_amb_i("4")"#,
    r#"exit_amb_i("2", "^", "4")"#,
    r#"exit_amb_i("3", "^", "2^4")"#,
    r#"exit_amb_i("81")"#,
    r#"exit_amb_i("3^2^4", "/", "81")"#,
    r#"exit_amb_i("6", "+", "3^2^4 / 81")"#,
    r#"exit_amb_i("-", "6 + 3^2^4 / 81")"#,
    r#"exit_amb_i("2", "*", "-6 + 3^2^4 / 81")"#,
    r#"exit_amb_i("5", "-", "2*-6 + 3^2^4 / 81")"#,
    r#"exit_amb("Hotel", "=", "5 - 2*-6 + 3^2^4 / 81", ";")"#,
    r#"exit_example("amb", "Hotel   = 5 - 2*-6 + 3^2^4 / 81;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\nlrec    Golf    = 107, 170, 750;", "amb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;")"#,

    r#"exit_star_a("India", "=", "[", "1:Alpha Beta 4:Delta Echo 10:Juliet", "]", ";")"#,
    r#"exit_example("star-a", "India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;", "star-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];")"#,
    r#"exit_plus_a("Juliet", "=", "[", "11:Kilo Lima Mike 26:Zoulou", "]", ";")"#,
    r#"exit_example("plus-a", "Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];", "plus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];")"#,
    r#"exit_l_star_a_i("", "2", ":", "Beta")"#,
    r#"exit_l_star_a_i("2:Beta", "Charlie")"#,
    r#"exit_l_star_a_i("2:Beta Charlie", "5", ":", "Echo")"#,
    r#"exit_l_star_a("Kilo", "=", "[", "2:Beta Charlie 5:Echo", "]", ";")"#,
    r#"exit_example("l-star-a", "Kilo   = [ 2:Beta Charlie 5:Echo ];")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];", "l-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];")"#,
    r#"exit_l_plus_a_i("", "21", ":", "Uniform")"#,
    r#"exit_l_plus_a_i("21:Uniform", "Victor")"#,
    r#"exit_l_plus_a_i("21:Uniform Victor", "25", ":", "Yankee")"#,
    r#"exit_l_plus_a("Lima", "=", "[", "21:Uniform Victor 25:Yankee", "]", ";")"#,
    r#"exit_example("l-plus-a", "Lima   = [ 21:Uniform Victor 25:Yankee ];")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];", "l-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];")"#,

    r#"exit_star("Mike", "=", "201", "", ";")"#,
    r#"exit_example("star", "Mike     = 201;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];", "star    Mike     = 201;")"#,
    r#"exit_l_star("November", "=", "202", "", ";")"#,
    r#"exit_example("l-star", "November = 202;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = 201;\nstar    Mike     = 201;", "l-star  November = 202;")"#,
    r#"exit_rrec_i(";")"#,
    r#"exit_rrec("Oscar", "=", "203", ";")"#,
    r#"exit_example("rrec", "Oscar    = 203;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = 201;\nl-star  November = 202;\nl-star  November = 202;", "rrec    Oscar    = 203;")"#,
    r#"exit_l_rrec_i("", ";")"#,
    r#"exit_l_rrec("Papa", "=", "204", ";")"#,
    r#"exit_example("l-rrec", "Papa     = 204;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = 201;\nl-star  November = 202;\nrrec    Oscar    = 203;\nrrec    Oscar    = 203;", "l-rrec  Papa     = 204;")"#,
    r#"exit_lrec_i("205")"#,
    r#"exit_lrec("Quebec", "=", "205", ";")"#,
    r#"exit_example("lrec", "Quebec   = 205;")"#,
    r#"exit_i("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = 201;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nl-rrec  Papa     = 204;", "lrec    Quebec   = 205;")"#,

    r#"exit_text("star    Alpha   = 101, 110, 150;\nplus    Bravo   = 102, 120, 250;\nl-star  Charlie = 103, 130, 350;\nl-plus  Delta   = 104, 140, 450;\nrrec    Echo    = 105, 150, 550;\nl-rrec  Foxtrot = 106, 160, 650;\nlrec    Golf    = 107, 170, 750;\namb     Hotel   = 5 - 2*-6 + 3^2^4 / 81;\n\nstar-a   India  = [ 1:Alpha Beta 4:Delta Echo 10:Juliet ];\nplus-a   Juliet = [ 11:Kilo Lima Mike 26:Zoulou ];\nl-star-a Kilo   = [ 2:Beta Charlie 5:Echo ];\nl-plus-a Lima   = [ 21:Uniform Victor 25:Yankee ];\n\nstar    Mike     = 201;\nl-star  November = 202;\nrrec    Oscar    = 203;\nl-rrec  Papa     = 204;\nlrec    Quebec   = 205;\nlrec    Quebec   = 205;")"#,
];

// -------------------------------------------------------------------------
// minimalist parser, top level

pub struct PanDemo<'l, 'p, 'ls> {
    lexer: Lexer<'l, Cursor<&'l str>>,
    parser: Parser<'p>,
    wrapper: Wrapper<PanDemoListener<'ls>>,
    lines: Vec<String>,
}

impl<'l, 'ls: 'l> PanDemo<'l, '_, 'ls> {
    pub fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        let wrapper = Wrapper::new(PanDemoListener::new(), VERBOSE_WRAPPER);
        PanDemo { lexer, parser, wrapper, lines: vec![] }
    }

    pub fn parse(&'ls mut self, text: &'ls str) -> Result<(BufLog, Vec<String>), BufLog> {
        let stream = CharReader::new(Cursor::new(text));
        self.lexer.attach_stream(stream);
        self.lines = text.lines().map(|l| l.to_string()).collect();
        self.wrapper.get_listener_mut().attach_lines(&self.lines);
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(&mut self.wrapper, tokens) {
            self.wrapper.get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let log = std::mem::take(&mut self.wrapper.get_listener_mut().log);
        if log.has_no_errors() {
            let listener = self.wrapper.get_listener_mut();
            let spans = std::mem::take(&mut listener.spans);
            Ok((log, spans))
        } else {
            Err(log)
        }
    }
}

// listener implementation

struct PanDemoListener<'ls> {
    log: BufLog,
    abort: bool,
    spans: Vec<String>,
    lines: Option<&'ls Vec<String>>,
}

impl<'ls> PanDemoListener<'ls> {
    fn new() -> Self {
        PanDemoListener {
            log: BufLog::new(),
            abort: false,
            spans: vec![],
            lines: None,
        }
    }

    fn attach_lines(&mut self, lines: &'ls Vec<String>) {
        self.lines = Some(lines);
    }

    fn get_text_span(&self, span: &PosSpan) -> String {
        if span.is_empty() { return String::new() }
        let &PosSpan { first: Pos(l1, c1), last: Pos(l2, c2) } = span;
        if l1 == l2 {
            self.lines.unwrap()[l1 as usize - 1].chars().skip(c1 as usize - 1).take((c2 - c1) as usize + 1).collect()
        } else {
            let mut result = self.lines.unwrap()[l1 as usize - 1].chars().skip(c1 as usize - 1).collect::<String>();
            for i in (l1 as usize)..(l2 as usize) {
                result.push('\n');
                result.push_str(&self.lines.unwrap()[i]);
            }
            result.push('\n');
            result.push_str(&self.lines.unwrap()[l2 as usize - 1].chars().take(c2 as usize).collect::<String>());
            result
        }
    }
}

#[allow(unused)]
impl PandemoniumListener for PanDemoListener<'_> {
    fn check_abort_request(&self) -> bool {
        self.abort
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit(&mut self, text: SynText, span: PosSpan) {
    }

    fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText {
        self.spans.push(format!("exit_text({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxText::V1 => {} // text -> (<L> example)*
        }
        SynText()
    }

    fn exit_i(&mut self, ctx: CtxI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxI::V1 { example: SynExample() } => {}
        }
    }

    fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample {
        self.spans.push(format!("exit_example({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxExample::V1 { star: SynStar() } => {}
            CtxExample::V2 { plus: SynPlus() } => {}
            CtxExample::V3 { l_star: SynLStar() } => {}
            CtxExample::V4 { l_plus: SynLPlus() } => {}
            CtxExample::V5 { rrec: SynRrec() } => {}
            CtxExample::V6 { l_rrec: SynLRrec() } => {}
            CtxExample::V7 { lrec: SynLrec() } => {}
            CtxExample::V8 { amb: SynAmb() } => {}
            CtxExample::V9 { star_a: SynStarA() } => {}
            CtxExample::V10 { plus_a: SynPlusA() } => {}
            CtxExample::V11 { l_star_a: SynLStarA() } => {}
            CtxExample::V12 { l_plus_a: SynLPlusA() } => {}
        }
        SynExample()
    }

    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar {
        self.spans.push(format!("exit_star({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxStar::V1 { id, num, star: SynStar1(star) } => {}
        }
        SynStar()
    }

    fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus {
        self.spans.push(format!("exit_plus({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxPlus::V1 { id, num, plus: SynPlus1(plus) } => {}
        }
        SynPlus()
    }

    fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar {
        self.spans.push(format!("exit_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLStar::V1 { id, num } => {}
        }
        SynLStar()
    }

    fn exit_l_star_i(&mut self, ctx: CtxLStarI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLStarI::V1 { num } => {}
        }
    }

    fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus {
        self.spans.push(format!("exit_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLPlus::V1 { id, num } => {}
        }
        SynLPlus()
    }

    fn exit_l_plus_i(&mut self, ctx: CtxLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLPlusI::V1 { num, last_iteration } => {}
        }
    }

    fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxRrec::V1 { id, num, rrec_i: SynRrecI() } => {}
        }
        SynRrec()
    }

    fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) -> SynLRrec {
        self.spans.push(format!("exit_l_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLRrec::V1 { id, num, l_rrec_i: SynLRrecI() } => {}
        }
        SynLRrec()
    }

    fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec {
        self.spans.push(format!("exit_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLrec::V1 { id, lrec_i: SynLrecI() } => {}
        }
        SynLrec()
    }

    fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb {
        self.spans.push(format!("exit_amb({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxAmb::V1 { id, amb_i: SynAmbI() } => {}
        }
        SynAmb()
    }

    fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) -> SynStarA {
        self.spans.push(format!("exit_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            // star_a -> Id "=" "[" (Id | Num ":" Id)* "]" ";"
            CtxStarA::V1 { id, star: SynStarA1(items) } => {}
        }
        SynStarA()
    }

    fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) -> SynPlusA {
        self.spans.push(format!("exit_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            // plus_a -> Id "=" "[" (Id | Num ":" Id)+ "]" ";"
            CtxPlusA::V1 { id, plus: SynPlusA1(items) } => {}
        }
        SynPlusA()
    }

    fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) -> SynLStarA {
        self.spans.push(format!("exit_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            // l_star_a -> Id "=" "[" (<L> Id | Num ":" Id)* "]" ";"
            CtxLStarA::V1 { id } => {}
        }
        SynLStarA()
    }

    fn exit_l_star_a_i(&mut self, ctx: CtxLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            // `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
            CtxLStarAI::V1 { id } => {}
            // `Num ":" Id` iteration in `l_star_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
            CtxLStarAI::V2 { num, id } => {}
        }
    }

    fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA {
        self.spans.push(format!("exit_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            // l_plus_a -> Id "=" "[" (<L> Id | Num ":" Id)+ "]" ";"
            CtxLPlusA::V1 { id } => {}
        }
        SynLPlusA()
    }

    fn exit_l_plus_a_i(&mut self, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            // `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
            CtxLPlusAI::V1 { id, last_iteration } => {}
            // `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
            CtxLPlusAI::V2 { num, id, last_iteration } => {}
        }
    }

    fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI {
        self.spans.push(format!("exit_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
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
        self.spans.push(format!("exit_l_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLRrecI::V1 { l_rrec_i: SynLRrecI(), num } => {}
            CtxLRrecI::V2 { l_rrec_i: SynLRrecI() } => {}
        }
        SynLRrecI()
    }

    fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI {
        self.spans.push(format!("exit_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
        match ctx {
            CtxLrecI::V1 { lrec_i: SynLrecI(), num } => {}
            CtxLrecI::V2 { num } => {}
        }
        SynLrecI()
    }

    fn exitloop_lrec_i(&mut self, _lrec_i: &mut SynLrecI) {
    }

    fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI {
        self.spans.push(format!("exit_amb_i({})", spans.into_iter().map(|s| format!("{:?}", self.get_text_span(&s))).join(", ")));
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
    /// User-defined type for `star_a`
    #[derive(Debug, PartialEq)] pub struct SynStarA();
    /// User-defined type for `plus_a`
    #[derive(Debug, PartialEq)] pub struct SynPlusA();
    /// User-defined type for `l_star_a`
    #[derive(Debug, PartialEq)] pub struct SynLStarA();
    /// User-defined type for `l_plus_a`
    #[derive(Debug, PartialEq)] pub struct SynLPlusA();
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
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 31;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 17;
    const NBR_STATES: StateId = 61;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         27,  27,  27,  27,  27,  27,  27,  27,  27,   0,  29,  27,  27,  29,  27,  27,   // 0-15
         27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,   // 16-31
          0,  27,  27,  27,  27,  27,  27,  27,   1,   2,   3,   4,   5,   6,  27,   7,   // 32-47
         21,   8,   8,   8,   8,   8,   8,   8,   8,   8,   9,  10,  27,  11,  27,  27,   // 48-63
         27,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,   // 64-79
         25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  13,  27,  14,  15,  26,   // 80-95
         27,  16,  30,  24,  25,  23,  25,  25,  25,  25,  25,  25,  17,  28,  25,  25,   // 96-111
         18,  25,  19,  20,  12,  22,  25,  25,  25,  25,  25,  27,  27,  27,  27,  27,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 27),
        (Seg(57344, 1114111), 27),
    ];
    static TERMINAL_TABLE: [Terminal;44] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 1892] = [
         17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  61,  29,  29,  29,  29,  61,  61,  29,  17,  29, // state 0
          1,   1,   1,  16,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  56,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 2
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  57,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 3
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,   5,   6,   7,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 4
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  11,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 5
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  14,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 6
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,   8,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 7
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,   9,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 8
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  46,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 9
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  58,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 10
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  12,  61,  61,  61,  61,  61,  61,  61,  61, // state 11
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  47,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 12
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  59,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 13
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  15,  61,  61,  61,  61,  61,  61,  61, // state 14
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  48,  61,  61,  61,  61,  61,  61, // state 15
          1,   1,   1,  16,   1,   1,   1,  60,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 16
         17,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  17,  61, // state 17 <skip>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 18 <end:4>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 19 <end:7>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 20 <end:6>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 21 <end:0>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 22 <end:11>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 23 <end:9>
         61,  61,  61,   1,  61,  61,  61,  38,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 24 <end:1>
         61,  61,  61,  61,  61,  61,  61,  61,  25,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  25,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 25 <end:26>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 26 <end:10>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 27 <end:12>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 28 <end:2>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 29 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 30 <end:5>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 31 <end:8>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 32 <end:3>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  54,  61,  29, // state 33 <end:25>
         61,  61,  61,  61,  61,  61,   4,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  45,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 34 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  42,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 35 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  49,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 36 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  39,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 37 <end:25>
         38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  61,  38, // state 38 <skip>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  40,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 39 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  41,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 40 <end:25>
         61,  61,  61,  61,  61,  61,   2,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 41 <end:13>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  43,  29,  29,  29,  29,  61,  29,  61,  29, // state 42 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  44,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 43 <end:25>
         61,  61,  61,  61,  61,  61,   3,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 44 <end:14>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  52,  29,  29,  29,  61,  29,  61,  29, // state 45 <end:25>
         61,  61,  61,  61,  61,  61,  10,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 46 <end:15>
         61,  61,  61,  61,  61,  61,  13,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 47 <end:16>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 48 <end:17>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  50,  29,  29,  29,  61,  29,  61,  29, // state 49 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  51,  29,  29,  61,  29,  61,  29, // state 50 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 51 <end:18>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  53,  29,  29,  61,  29,  61,  29, // state 52 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 53 <end:19>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  55, // state 54 <end:25>
         61,  61,  61,  61,  61,  61,  61,  61,  29,  61,  61,  61,  29,  61,  61,  61,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  61,  29,  61,  29, // state 55 <end:20>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 56 <end:21>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 57 <end:22>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 58 <end:23>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 59 <end:24>
         61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61,  61, // state 60 <skip>
         61 // error group in [nbr_state * nbr_group + nbr_group]
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

    use lexigram_core::{AltId, fixed_sym_table::FixedSymTable, VarId, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 27;
    const PARSER_NUM_NT: usize = 40;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Div", Some("/")), ("Equal", Some("=")), ("Exp", Some("^")), ("Lpar", Some("(")), ("Lsbracket", Some("[")), ("Mul", Some("*")), ("Rpar", Some(")")), ("Rsbracket", Some("]")), ("Sub", Some("-")), ("Colon", Some(":")), ("Comma", Some(",")), ("Semi", Some(";")), ("Star", Some("star")), ("Plus", Some("plus")), ("L_Star", Some("l-star")), ("L_Plus", Some("l-plus")), ("L_Rrec", Some("l-rrec")), ("Rrec", Some("rrec")), ("Lrec", Some("lrec")), ("Amb", Some("amb")), ("Star_A", Some("star-a")), ("Plus_A", Some("plus-a")), ("L_Star_A", Some("l-star-a")), ("L_Plus_A", Some("l-plus-a")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["text", "i", "example", "star", "plus", "l_star", "l_star_i", "l_plus", "l_plus_i", "rrec", "l_rrec", "lrec", "amb", "star_a", "plus_a", "l_star_a", "l_star_a_i", "l_plus_a", "l_plus_a_i", "rrec_i", "l_rrec_i", "lrec_i", "amb_i", "star_1", "plus_1", "star_a_1", "plus_a_1", "lrec_i_1", "amb_i_1", "amb_i_2", "amb_i_3", "amb_i_4", "amb_i_5", "amb_i_6", "l_plus_1", "l_plus_a_1", "l_plus_a_2", "plus_2", "plus_a_2", "plus_a_3"];
    static ALT_VAR: [VarId; 81] = [0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16, 16, 17, 18, 18, 19, 19, 20, 20, 21, 22, 23, 23, 24, 25, 25, 25, 26, 26, 27, 27, 28, 28, 28, 28, 28, 28, 29, 30, 30, 30, 30, 31, 32, 32, 33, 33, 33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38, 38, 39, 39];
    static PARSING_TABLE: [AltId; 1120] = [81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 81, 81, 0, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 81, 81, 2, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 3, 4, 5, 6, 8, 7, 9, 10, 11, 12, 13, 14, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 15, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 16, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 17, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 18, 19, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 20, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 21, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 22, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 23, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 24, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 25, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 26, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 27, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 28, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 31, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 29, 30, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 32, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 33, 34, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 35, 36, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 37, 38, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 39, 81, 82, 82, 81, 82, 40, 81, 82, 82, 81, 40, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 40, 40, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 41, 42, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 43, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 46, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 44, 45, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 47, 48, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 49, 50, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 54, 53, 81, 51, 81, 81, 52, 56, 81, 55, 81, 81, 56, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 81, 82, 57, 81, 82, 82, 81, 57, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 57, 57, 81, 61, 60, 81, 58, 81, 81, 59, 61, 81, 61, 81, 81, 61, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 81, 82, 62, 81, 82, 82, 81, 62, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 62, 62, 81, 64, 64, 81, 63, 81, 81, 64, 64, 81, 64, 81, 81, 64, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 81, 82, 66, 81, 82, 82, 81, 65, 81, 81, 82, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 67, 68, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 69, 70, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 72, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 71, 71, 81, 81, 81, 81, 81, 81, 81, 81, 81, 74, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 73, 73, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 75, 76, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 78, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 77, 77, 81, 81, 81, 81, 81, 81, 81, 81, 81, 80, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 79, 79, 81];
    static OPCODES: [&[OpCode]; 81] = [&[OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2)], &[OpCode::Exit(3), OpCode::NT(3), OpCode::T(13)], &[OpCode::Exit(4), OpCode::NT(4), OpCode::T(14)], &[OpCode::Exit(5), OpCode::NT(5), OpCode::T(15)], &[OpCode::Exit(6), OpCode::NT(7), OpCode::T(16)], &[OpCode::Exit(7), OpCode::NT(9), OpCode::T(18)], &[OpCode::Exit(8), OpCode::NT(10), OpCode::T(17)], &[OpCode::Exit(9), OpCode::NT(11), OpCode::T(19)], &[OpCode::Exit(10), OpCode::NT(12), OpCode::T(20)], &[OpCode::Exit(11), OpCode::NT(13), OpCode::T(21)], &[OpCode::Exit(12), OpCode::NT(14), OpCode::T(22)], &[OpCode::Exit(13), OpCode::NT(15), OpCode::T(23)], &[OpCode::Exit(14), OpCode::NT(17), OpCode::T(24)], &[OpCode::Exit(15), OpCode::T(12), OpCode::NT(23), OpCode::T(26), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(16), OpCode::T(12), OpCode::NT(24), OpCode::T(26), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(17), OpCode::T(12), OpCode::NT(6), OpCode::T(26), OpCode::T(2), OpCode::T(25)], &[OpCode::Loop(6), OpCode::Exit(18), OpCode::T(26), OpCode::T(11)], &[OpCode::Exit(19)], &[OpCode::Exit(20), OpCode::T(12), OpCode::NT(8), OpCode::T(26), OpCode::T(2), OpCode::T(25)], &[OpCode::NT(34), OpCode::T(26), OpCode::T(11)], &[OpCode::Exit(22), OpCode::NT(19), OpCode::T(26), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(23), OpCode::NT(20), OpCode::T(26), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(24), OpCode::T(12), OpCode::NT(21), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(25), OpCode::T(12), OpCode::NT(22), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(26), OpCode::T(12), OpCode::T(8), OpCode::NT(25), OpCode::T(5), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(27), OpCode::T(12), OpCode::T(8), OpCode::NT(26), OpCode::T(5), OpCode::T(2), OpCode::T(25)], &[OpCode::Exit(28), OpCode::T(12), OpCode::T(8), OpCode::NT(16), OpCode::T(5), OpCode::T(2), OpCode::T(25)], &[OpCode::Loop(16), OpCode::Exit(29), OpCode::T(25)], &[OpCode::Loop(16), OpCode::Exit(30), OpCode::T(25), OpCode::T(10), OpCode::T(26)], &[OpCode::Exit(31)], &[OpCode::Exit(32), OpCode::T(12), OpCode::T(8), OpCode::NT(18), OpCode::T(5), OpCode::T(2), OpCode::T(25)], &[OpCode::NT(35), OpCode::T(25)], &[OpCode::NT(36), OpCode::T(25), OpCode::T(10), OpCode::T(26)], &[OpCode::Exit(35), OpCode::NT(19), OpCode::T(26), OpCode::T(11)], &[OpCode::Exit(36), OpCode::T(12)], &[OpCode::Loop(20), OpCode::Exit(37), OpCode::T(26), OpCode::T(11)], &[OpCode::Exit(38), OpCode::T(12)], &[OpCode::NT(27), OpCode::Exit(39), OpCode::T(26)], &[OpCode::NT(28), OpCode::Exit(40), OpCode::NT(33)], &[OpCode::Loop(23), OpCode::Exit(41), OpCode::T(26), OpCode::T(11)], &[OpCode::Exit(42)], &[OpCode::NT(37), OpCode::T(26), OpCode::T(11)], &[OpCode::Loop(25), OpCode::Exit(44), OpCode::T(25)], &[OpCode::Loop(25), OpCode::Exit(45), OpCode::T(25), OpCode::T(10), OpCode::T(26)], &[OpCode::Exit(46)], &[OpCode::NT(38), OpCode::T(25)], &[OpCode::NT(39), OpCode::T(25), OpCode::T(10), OpCode::T(26)], &[OpCode::Loop(27), OpCode::Exit(49), OpCode::T(26), OpCode::T(11)], &[OpCode::Exit(50)], &[OpCode::Loop(28), OpCode::Exit(51), OpCode::NT(31), OpCode::T(3)], &[OpCode::Loop(28), OpCode::Exit(52), OpCode::NT(31), OpCode::T(6)], &[OpCode::Loop(28), OpCode::Exit(53), OpCode::NT(31), OpCode::T(1)], &[OpCode::Loop(28), OpCode::Exit(54), OpCode::NT(29), OpCode::T(0)], &[OpCode::Loop(28), OpCode::Exit(55), OpCode::NT(29), OpCode::T(9)], &[OpCode::Exit(56)], &[OpCode::NT(30), OpCode::Exit(57), OpCode::NT(33)], &[OpCode::Loop(30), OpCode::Exit(58), OpCode::NT(31), OpCode::T(3)], &[OpCode::Loop(30), OpCode::Exit(59), OpCode::NT(31), OpCode::T(6)], &[OpCode::Loop(30), OpCode::Exit(60), OpCode::NT(31), OpCode::T(1)], &[OpCode::Exit(61)], &[OpCode::NT(32), OpCode::Exit(62), OpCode::NT(33)], &[OpCode::Loop(32), OpCode::Exit(63), OpCode::NT(31), OpCode::T(3)], &[OpCode::Exit(64)], &[OpCode::Exit(65), OpCode::NT(22), OpCode::T(9)], &[OpCode::Exit(66), OpCode::T(7), OpCode::NT(22), OpCode::T(4)], &[OpCode::Exit(67), OpCode::T(25)], &[OpCode::Exit(68), OpCode::T(26)], &[OpCode::Loop(8), OpCode::Exit(69)], &[OpCode::Exit(70)], &[OpCode::Loop(18), OpCode::Exit(71)], &[OpCode::Exit(72)], &[OpCode::Loop(18), OpCode::Exit(73)], &[OpCode::Exit(74)], &[OpCode::Loop(24), OpCode::Exit(75)], &[OpCode::Exit(76)], &[OpCode::Loop(26), OpCode::Exit(77)], &[OpCode::Exit(78)], &[OpCode::Loop(26), OpCode::Exit(79)], &[OpCode::Exit(80)]];
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
        /// `example -> "star-a" star_a`
        V9 { star_a: SynStarA },
        /// `example -> "plus-a" plus_a`
        V10 { plus_a: SynPlusA },
        /// `example -> "l-star-a" l_star_a`
        V11 { l_star_a: SynLStarA },
        /// `example -> "l-plus-a" l_plus_a`
        V12 { l_plus_a: SynLPlusA },
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
    pub enum CtxStarA {
        /// `star_a -> Id "=" "[" (Id | Num ":" Id)* "]" ";"`
        V1 { id: String, star: SynStarA1 },
    }
    #[derive(Debug)]
    pub enum CtxPlusA {
        /// `plus_a -> Id "=" "[" (Id | Num ":" Id)+ "]" ";"`
        V1 { id: String, plus: SynPlusA1 },
    }
    #[derive(Debug)]
    pub enum CtxLStarA {
        /// `l_star_a -> Id "=" "[" (<L> Id | Num ":" Id)* "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxLStarAI {
        /// `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
        V1 { id: String },
        /// `Num ":" Id` iteration in `l_star_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
        V2 { num: String, id: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlusA {
        /// `l_plus_a -> Id "=" "[" (<L> Id | Num ":" Id)+ "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlusAI {
        /// `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
        V1 { id: String, last_iteration: bool },
        /// `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
        V2 { num: String, id: String, last_iteration: bool },
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
    // /// User-defined type for `star_a`
    // #[derive(Debug, PartialEq)] pub struct SynStarA();
    // /// User-defined type for `plus_a`
    // #[derive(Debug, PartialEq)] pub struct SynPlusA();
    // /// User-defined type for `l_star_a`
    // #[derive(Debug, PartialEq)] pub struct SynLStarA();
    // /// User-defined type for `l_plus_a`
    // #[derive(Debug, PartialEq)] pub struct SynLPlusA();
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
    /// Computed `(Id | Num ":" Id)*` array in `star_a -> Id "=" "["  ►► (Id | Num ":" Id)* ◄◄  "]" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynStarA1(pub Vec<SynStarA1Item>);
    #[derive(Debug, PartialEq)]
    pub enum SynStarA1Item {
        /// `Id` item in `star_a -> Id "=" "[" ( ►► Id ◄◄  | Num ":" Id)* "]" ";"`
        V1 { id: String },
        /// `Num ":" Id` item in `star_a -> Id "=" "[" (Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
        V2 { num: String, id: String },
    }
    /// Computed `(Id | Num ":" Id)+` array in `plus_a -> Id "=" "["  ►► (Id | Num ":" Id)+ ◄◄  "]" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynPlusA1(pub Vec<SynPlusA1Item>);
    #[derive(Debug, PartialEq)]
    pub enum SynPlusA1Item {
        /// `Id` item in `plus_a -> Id "=" "[" ( ►► Id ◄◄  | Num ":" Id)+ "]" ";"`
        V1 { id: String },
        /// `Num ":" Id` item in `plus_a -> Id "=" "[" (Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
        V2 { num: String, id: String },
    }

    #[derive(Debug)]
    enum SynValue { Text(SynText), Example(SynExample), Star(SynStar), Plus(SynPlus), LStar(SynLStar), LPlus(SynLPlus), Rrec(SynRrec), LRrec(SynLRrec), Lrec(SynLrec), Amb(SynAmb), StarA(SynStarA), PlusA(SynPlusA), LStarA(SynLStarA), LPlusA(SynLPlusA), RrecI(SynRrecI), LRrecI(SynLRrecI), LrecI(SynLrecI), AmbI(SynAmbI), Star1(SynStar1), Plus1(SynPlus1), StarA1(SynStarA1), PlusA1(SynPlusA1) }

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
        fn get_star_a(self) -> SynStarA {
            if let SynValue::StarA(val) = self { val } else { panic!() }
        }
        fn get_plus_a(self) -> SynPlusA {
            if let SynValue::PlusA(val) = self { val } else { panic!() }
        }
        fn get_l_star_a(self) -> SynLStarA {
            if let SynValue::LStarA(val) = self { val } else { panic!() }
        }
        fn get_l_plus_a(self) -> SynLPlusA {
            if let SynValue::LPlusA(val) = self { val } else { panic!() }
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
        fn get_star_a1(self) -> SynStarA1 {
            if let SynValue::StarA1(val) = self { val } else { panic!() }
        }
        fn get_plus_a1(self) -> SynPlusA1 {
            if let SynValue::PlusA1(val) = self { val } else { panic!() }
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
        fn init_star_a(&mut self) {}
        fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) -> SynStarA;
        fn init_plus_a(&mut self) {}
        fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) -> SynPlusA;
        fn init_l_star_a(&mut self) {}
        fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) -> SynLStarA;
        fn init_l_star_a_i(&mut self) {}
        #[allow(unused)]
        fn exit_l_star_a_i(&mut self, ctx: CtxLStarAI, spans: Vec<PosSpan>) {}
        fn init_l_plus_a(&mut self) {}
        fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA;
        fn init_l_plus_a_i(&mut self) {}
        #[allow(unused)]
        fn exit_l_plus_a_i(&mut self, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {}
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
                    if matches!(nt, 1 | 6 | 8 | 16 | 18 | 20 | 23 ..= 26) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_text(),             // text
                        1 => self.listener.init_i(),                // i
                        2 => self.listener.init_example(),          // example
                        3 => self.listener.init_star(),             // star
                        23 => self.init_star1(),                    // star_1
                        4 => self.listener.init_plus(),             // plus
                        24 => self.init_plus1(),                    // plus_1
                        37 => {}                                    // plus_2
                        5 => self.listener.init_l_star(),           // l_star
                        6 => self.listener.init_l_star_i(),         // l_star_i
                        7 => self.listener.init_l_plus(),           // l_plus
                        8 => self.listener.init_l_plus_i(),         // l_plus_i
                        34 => {}                                    // l_plus_1
                        9 => self.listener.init_rrec(),             // rrec
                        10 => self.listener.init_l_rrec(),          // l_rrec
                        11 => self.listener.init_lrec(),            // lrec
                        12 => self.listener.init_amb(),             // amb
                        13 => self.listener.init_star_a(),          // star_a
                        25 => self.init_star_a1(),                  // star_a_1
                        14 => self.listener.init_plus_a(),          // plus_a
                        26 => self.init_plus_a1(),                  // plus_a_1
                        38 | 39 => {}                               // plus_a_2, plus_a_3
                        15 => self.listener.init_l_star_a(),        // l_star_a
                        16 => self.listener.init_l_star_a_i(),      // l_star_a_i
                        17 => self.listener.init_l_plus_a(),        // l_plus_a
                        18 => self.listener.init_l_plus_a_i(),      // l_plus_a_i
                        35 | 36 => {}                               // l_plus_a_1, l_plus_a_2
                        19 => self.listener.init_rrec_i(),          // rrec_i
                        20 => self.init_l_rrec_i(),                 // l_rrec_i
                        21 => self.listener.init_lrec_i(),          // lrec_i
                        27 => {}                                    // lrec_i_1
                        22 => self.listener.init_amb_i(),           // amb_i
                        28 ..= 33 => {}                             // amb_i_1, amb_i_2, amb_i_3, amb_i_4, amb_i_5, amb_i_6
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
                        10 |                                        // example -> "amb" amb
                        11 |                                        // example -> "star-a" star_a
                        12 |                                        // example -> "plus-a" plus_a
                        13 |                                        // example -> "l-star-a" l_star_a
                        14 => self.exit_example(alt_id),            // example -> "l-plus-a" l_plus_a
                        15 => self.exit_star(),                     // star -> Id "=" Num star_1 ";"
                        41 => self.exit_star1(),                    // star_1 -> "," Num star_1
                        42 => {}                                    // star_1 -> ε
                        16 => self.exit_plus(),                     // plus -> Id "=" Num plus_1 ";"
                        75 |                                        // plus_2 -> plus_1
                        76 => self.exit_plus1(),                    // plus_2 -> ε
                     /* 43 */                                       // plus_1 -> "," Num plus_2 (never called)
                        17 => self.exit_l_star(),                   // l_star -> Id "=" Num l_star_i ";"
                        18 => self.exit_l_star_i(),                 // l_star_i -> <L> "," Num l_star_i
                        19 => {}                                    // l_star_i -> <L> ε (not used)
                        20 => self.exit_l_plus(),                   // l_plus -> Id "=" Num l_plus_i ";"
                        69 |                                        // l_plus_1 -> l_plus_i
                        70 => self.exit_l_plus_i(alt_id),           // l_plus_1 -> ε
                     /* 21 */                                       // l_plus_i -> <L> "," Num l_plus_1 (never called)
                        22 => self.exit_rrec(),                     // rrec -> Id "=" Num rrec_i
                        23 => self.exit_l_rrec(),                   // l_rrec -> Id "=" Num l_rrec_i
                        24 => self.exit_lrec(),                     // lrec -> Id "=" lrec_i ";"
                        25 => self.exit_amb(),                      // amb -> Id "=" amb_i ";"
                        26 => self.exit_star_a(),                   // star_a -> Id "=" "[" star_a_1 "]" ";"
                        44 |                                        // star_a_1 -> Id star_a_1
                        45 => self.exit_star_a1(alt_id),            // star_a_1 -> Num ":" Id star_a_1
                        46 => {}                                    // star_a_1 -> ε
                        27 => self.exit_plus_a(),                   // plus_a -> Id "=" "[" plus_a_1 "]" ";"
                        77 |                                        // plus_a_2 -> plus_a_1
                        78 |                                        // plus_a_2 -> ε
                        79 |                                        // plus_a_3 -> plus_a_1
                        80 => self.exit_plus_a1(alt_id),            // plus_a_3 -> ε
                     /* 47 */                                       // plus_a_1 -> Id plus_a_2 (never called)
                     /* 48 */                                       // plus_a_1 -> Num ":" Id plus_a_3 (never called)
                        28 => self.exit_l_star_a(),                 // l_star_a -> Id "=" "[" l_star_a_i "]" ";"
                        29 |                                        // l_star_a_i -> <L> Id l_star_a_i
                        30 => self.exit_l_star_a_i(alt_id),         // l_star_a_i -> <L> Num ":" Id l_star_a_i
                        31 => {}                                    // l_star_a_i -> <L> ε (not used)
                        32 => self.exit_l_plus_a(),                 // l_plus_a -> Id "=" "[" l_plus_a_i "]" ";"
                        71 |                                        // l_plus_a_1 -> l_plus_a_i
                        72 |                                        // l_plus_a_1 -> ε
                        73 |                                        // l_plus_a_2 -> l_plus_a_i
                        74 => self.exit_l_plus_a_i(alt_id),         // l_plus_a_2 -> ε
                     /* 33 */                                       // l_plus_a_i -> <L> Id l_plus_a_1 (never called)
                     /* 34 */                                       // l_plus_a_i -> <L> Num ":" Id l_plus_a_2 (never called)
                        35 |                                        // rrec_i -> "," Num rrec_i
                        36 => self.exit_rrec_i(alt_id),             // rrec_i -> ";"
                        37 |                                        // l_rrec_i -> <L> "," Num l_rrec_i
                        38 => self.exit_l_rrec_i(alt_id),           // l_rrec_i -> <L> ";"
                        39 => self.inter_lrec_i(),                  // lrec_i -> Num lrec_i_1
                        49 => self.exit_lrec_i1(),                  // lrec_i_1 -> "," Num lrec_i_1
                        50 => self.exitloop_lrec_i1(),              // lrec_i_1 -> ε
                        51 |                                        // amb_i_1 -> <R> "^" amb_i_4 amb_i_1
                        52 |                                        // amb_i_1 -> "*" amb_i_4 amb_i_1
                        53 |                                        // amb_i_1 -> "/" amb_i_4 amb_i_1
                        54 |                                        // amb_i_1 -> "+" amb_i_2 amb_i_1
                        55 => self.exit_amb_i1(alt_id),             // amb_i_1 -> "-" amb_i_2 amb_i_1
                        58 |                                        // amb_i_3 -> <R> "^" amb_i_4 amb_i_3 (duplicate of 51)
                        63 => self.exit_amb_i1(51),                 // amb_i_5 -> <R> "^" amb_i_4 amb_i_5 (duplicate of 51)
                        59 => self.exit_amb_i1(52),                 // amb_i_3 -> "*" amb_i_4 amb_i_3 (duplicate of 52)
                        60 => self.exit_amb_i1(53),                 // amb_i_3 -> "/" amb_i_4 amb_i_3 (duplicate of 53)
                        65 |                                        // amb_i_6 -> "-" amb_i
                        66 |                                        // amb_i_6 -> "(" amb_i ")"
                        67 |                                        // amb_i_6 -> Id
                        68 => self.exit_amb_i6(alt_id),             // amb_i_6 -> Num
                        40 => {}                                    // amb_i -> amb_i_6 amb_i_1 (not used)
                        56 => {}                                    // amb_i_1 -> ε (not used)
                        57 => {}                                    // amb_i_2 -> amb_i_6 amb_i_3 (not used)
                        61 => {}                                    // amb_i_3 -> ε (not used)
                        62 => {}                                    // amb_i_4 -> amb_i_6 amb_i_5 (not used)
                        64 => {}                                    // amb_i_5 -> ε (not used)
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
                11 => {
                    let star_a = self.stack.pop().unwrap().get_star_a();
                    (2, CtxExample::V9 { star_a })
                }
                12 => {
                    let plus_a = self.stack.pop().unwrap().get_plus_a();
                    (2, CtxExample::V10 { plus_a })
                }
                13 => {
                    let l_star_a = self.stack.pop().unwrap().get_l_star_a();
                    (2, CtxExample::V11 { l_star_a })
                }
                14 => {
                    let l_plus_a = self.stack.pop().unwrap().get_l_plus_a();
                    (2, CtxExample::V12 { l_plus_a })
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
            let last_iteration = alt_id == 70;
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

        fn exit_star_a(&mut self) {
            let star = self.stack.pop().unwrap().get_star_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxStarA::V1 { id, star };
            let n = 6;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_star_a(ctx, spans);
            self.stack.push(SynValue::StarA(val));
        }

        fn init_star_a1(&mut self) {
            let val = SynStarA1(Vec::new());
            self.stack.push(SynValue::StarA1(val));
        }

        fn exit_star_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                44 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynStarA1Item::V1 { id })
                }
                45 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynStarA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_star_a1"),
            };
            let Some(SynValue::StarA1(SynStarA1(star_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynStarA1 item on wrapper stack");
            };
            star_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
        }

        fn exit_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_plus_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxPlusA::V1 { id, plus };
            let n = 6;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_plus_a(ctx, spans);
            self.stack.push(SynValue::PlusA(val));
        }

        fn init_plus_a1(&mut self) {
            let val = SynPlusA1(Vec::new());
            self.stack.push(SynValue::PlusA1(val));
        }

        fn exit_plus_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                77 | 78 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynPlusA1Item::V1 { id })
                }
                79 | 80 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynPlusA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_plus_a1"),
            };
            let Some(SynValue::PlusA1(SynPlusA1(plus_acc))) = self.stack.last_mut() else {
                panic!("unexpected SynPlusA1 item on wrapper stack");
            };
            plus_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
        }

        fn exit_l_star_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStarA::V1 { id };
            let n = 6;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_l_star_a(ctx, spans);
            self.stack.push(SynValue::LStarA(val));
        }

        fn exit_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                29 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLStarAI::V1 { id })
                }
                30 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLStarAI::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_l_star_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_l_star_a_i(ctx, spans);
        }

        fn exit_l_plus_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusA::V1 { id };
            let n = 6;
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            let val = self.listener.exit_l_plus_a(ctx, spans);
            self.stack.push(SynValue::LPlusA(val));
        }

        fn exit_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                71 | 72 => {
                    let last_iteration = alt_id == 72;
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLPlusAI::V1 { id, last_iteration })
                }
                73 | 74 => {
                    let last_iteration = alt_id == 74;
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLPlusAI::V2 { num, id, last_iteration })
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_l_plus_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            let mut new_span = PosSpan::empty();
            spans.iter().for_each(|span| new_span += span);
            self.stack_span.push(new_span);
            self.listener.exit_l_plus_a_i(ctx, spans);
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                35 => {
                    let rrec_i = self.stack.pop().unwrap().get_rrec_i();
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num, rrec_i })
                }
                36 => {
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
                37 => {
                    let num = self.stack_t.pop().unwrap();
                    let l_rrec_i = self.stack.pop().unwrap().get_l_rrec_i();
                    (3, CtxLRrecI::V1 { l_rrec_i, num })
                }
                38 => {
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
                51 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V1 { amb_i: [amb_i_1, amb_i_2] })
                }
                52 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V2 { amb_i: [amb_i_1, amb_i_2] })
                }
                53 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V3 { amb_i: [amb_i_1, amb_i_2] })
                }
                54 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V4 { amb_i: [amb_i_1, amb_i_2] })
                }
                55 => {
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
                65 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (2, CtxAmbI::V6 { amb_i })
                }
                66 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V7 { amb_i })
                }
                67 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V8 { id })
                }
                68 => {
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
