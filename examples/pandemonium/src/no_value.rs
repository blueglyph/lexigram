// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use lexigram_core::CollectJoin;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::{LLParser, Terminate};
use lexigram_core::text_span::{GetLine, GetTextSpan};
use pandemonium_lexer::build_lexer;
use pandemonium_parser::*;
use crate::{SPANS1, TXT1};

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

#[test]
fn test_pandemonium() {
    if VERBOSE { println!("{:=<80}\n{TXT1}\n{0:-<80}", ""); }
    let mut demo = PanDemo::new();
    match demo.parse(TXT1) {
        Ok(PanDemoResult { log, spans, rebuilt_txt }) => {
            if VERBOSE {
                println!("parsing successful\n{log}");
                println!("Spans:\n{}", spans.join("\n"));
            }
            // checks that the text rebuilt from spans matches the original:
            assert!(TXT1.contains(&rebuilt_txt), "rebuilt text is wrong:\n{rebuilt_txt:?}");
            // checks the individual spans:
            // (tedious visual verification each time the test changes!)
            assert_eq!(
                spans, SPANS1, "span mismatch:\n{}",
                spans.iter().zip(SPANS1).enumerate()
                    .find_map(|(i, (left, right))| {
                        if left != right {
                            Some(format!("{i}:\t{left}\n\t{right}"))
                        } else {
                            None
                        }
                    })
                    .unwrap()
            );
        },
        Err(log) => panic!("errors during parsing:\n{log}"),
    }
}

// -------------------------------------------------------------------------
// minimalist parser, top level

pub struct PanDemo<'l, 'p, 'ls> {
    lexer: Lexer<'l, &'ls [u8]>,
    parser: LLParser<'p>,
    wrapper: Wrapper<PanDemoListener<'ls>>,
}

impl<'l, 'ls: 'l> PanDemo<'l, '_, 'ls> {
    fn new() -> Self {
        let lexer = build_lexer();
        let parser = build_parser();
        let wrapper = Wrapper::new(PanDemoListener::new(), VERBOSE_WRAPPER);
        PanDemo { lexer, parser, wrapper }
    }

    fn parse(&mut self, text: &'ls str) -> Result<PanDemoResult, BufLog> {
        let stream = CharReader::new(text.as_bytes());
        self.lexer.attach_stream(stream);
        self.wrapper.get_listener_mut().attach_lines(text.lines().collect());
        let tokens = self.lexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} while parsing a file at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.parser.parse_stream(&mut self.wrapper, tokens) {
            self.wrapper.get_listener_mut().get_log_mut().add_error(e.to_string());
        }
        let log = std::mem::take(&mut self.wrapper.get_listener_mut().log);
        if log.has_no_errors() {
            let listener = self.wrapper.get_listener_mut();
            let spans = std::mem::take(&mut listener.spans);
            let rebuilt_txt = listener.rebuilt_txt.take().unwrap();
            Ok(PanDemoResult { log, spans, rebuilt_txt })
        } else {
            Err(log)
        }
    }
}

// listener implementation

struct PanDemoResult {
    log: BufLog,
    spans: Vec<String>,
    rebuilt_txt: String,
}

struct PanDemoListener<'ls> {
    log: BufLog,
    abort: Terminate,
    spans: Vec<String>,
    lines: Option<Vec<&'ls str>>,
    rebuilt_txt: Option<String>,
}

impl<'ls> PanDemoListener<'ls> {
    fn new() -> Self {
        PanDemoListener {
            log: BufLog::new(),
            abort: Terminate::None,
            spans: vec![],
            lines: None,
            rebuilt_txt: None,
        }
    }

    fn attach_lines(&mut self, lines: Vec<&'ls str>) {
        self.lines = Some(lines);
    }
}

impl GetLine for PanDemoListener<'_> {
    fn get_line(&self, n: usize) -> &str {
        self.lines.as_ref().unwrap()[n - 1]
    }
}

#[allow(unused)]
impl PandemoniumListener for PanDemoListener<'_> {
    fn check_abort_request(&self) -> Terminate {
        self.abort
    }

    fn get_log_mut(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit(&mut self, span: PosSpan) {
        self.rebuilt_txt = Some(self.extract_text(&span));
    }

    fn exit_text(&mut self, _ctx: CtxText, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_text({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_i(&mut self, _ctx: CtxI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_example(&mut self, _ctx: CtxExample, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_star(&mut self, _ctx: CtxStar, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_plus(&mut self, _ctx: CtxPlus, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_star(&mut self, _ctx: CtxLStar, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_star_i(&mut self, _ctx: CtxLStarI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_plus(&mut self, _ctx: CtxLPlus, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_plus_i(&mut self, _ctx: CtxLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_rrec(&mut self, _ctx: CtxRrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_rrec(&mut self, _ctx: CtxLRrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }


    fn exit_lrec(&mut self, _ctx: CtxLrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_amb(&mut self, _ctx: CtxAmb, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_amb({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_star_a(&mut self, _ctx: CtxStarA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_plus_a(&mut self, _ctx: CtxPlusA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_star_a(&mut self, _ctx: CtxLStarA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_star_a_i(&mut self, _ctx: CtxLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_plus_a(&mut self, _ctx: CtxLPlusA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_plus_a_i(&mut self, _ctx: CtxLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_sep_list(&mut self, _ctx: CtxSepList, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_sep_list_opt(&mut self, _ctx: CtxSepListOpt, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_rrec_i(&mut self, _ctx: CtxRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_l_rrec_i(&mut self, _ctx: CtxLRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_lrec_i(&mut self, _ctx: CtxLrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    fn exit_amb_i(&mut self, _ctx: CtxAmbI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_amb_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
    }

    // TODO:

    fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) {
        match ctx {
            // nv_example -> "star" nv_star
            CtxNvExample::V1 => {}
            // nv_example -> "plus" nv_plus
            CtxNvExample::V2 => {}
            // nv_example -> "l-star" nv_l_star
            CtxNvExample::V3 => {}
            // nv_example -> "l-plus" nv_l_plus
            CtxNvExample::V4 => {}
            // nv_example -> "rrec" nv_rrec
            CtxNvExample::V5 => {}
            // nv_example -> "l-rrec" nv_l_rrec
            CtxNvExample::V6 => {}
            // nv_example -> "lrec" nv_lrec
            CtxNvExample::V7 => {}
            // nv_example -> "star-a" nv_star_a
            CtxNvExample::V8 => {}
            // nv_example -> "plus-a" nv_plus_a
            CtxNvExample::V9 => {}
            // nv_example -> "l-star-a" nv_l_star_a
            CtxNvExample::V10 => {}
            // nv_example -> "l-plus-a" nv_l_plus_a
            CtxNvExample::V11 => {}
            // nv_example -> "sep-list" nv_sep_list
            CtxNvExample::V12 => {}
            // nv_example -> "sep-list-opt" nv_sep_list_opt
            CtxNvExample::V13 => {}
        }
    }

    fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) {
        // nv_star -> Id "=" "+" ("," "*")* ";"
        let CtxNvStar::V1 { id } = ctx;
    }

    fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) {
        // nv_plus -> Id "=" "+" ("," "*")+ ";"
        let CtxNvPlus::V1 { id } = ctx;
    }

    fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) {
        // nv_l_star -> Id "=" "+" (<L> "," "*")* ";"
        let CtxNvLStar::V1 { id } = ctx;
    }

    fn exit_nv_l_star_i(&mut self, ctx: CtxNvLStarI, spans: Vec<PosSpan>) {
        // `<L> "," "*"` iteration in `nv_l_star -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )* ";"`
        let CtxNvLStarI::V1 = ctx;
    }

    fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) {
        // nv_l_plus -> Id "=" "+" (<L> "," "*")+ ";"
        let CtxNvLPlus::V1 { id } = ctx;
    }

    fn exit_nv_l_plus_i(&mut self, ctx: CtxNvLPlusI, spans: Vec<PosSpan>) {
        // `<L> "," "*"` iteration in `nv_l_plus -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )+ ";"`
        let CtxNvLPlusI::V1 { last_iteration } = ctx;
    }

    fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) {
        // nv_rrec -> Id "=" "+" nv_rrec_i
        let CtxNvRrec::V1 { id } = ctx;
    }

    fn exit_nv_l_rrec(&mut self, ctx: CtxNvLRrec, spans: Vec<PosSpan>) {
        // nv_l_rrec -> Id "=" "+" nv_l_rrec_i
        let CtxNvLRrec::V1 { id } = ctx;
    }

    fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) {
        // nv_lrec -> Id "=" nv_lrec_i ";"
        let CtxNvLrec::V1 { id } = ctx;
    }

    fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) {
        // nv_star_a -> Id "=" "[" ("+" | "*" ":" Id)* "]" ";"
        let CtxNvStarA::V1 { id, star } = ctx;
    }

    fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) {
        // nv_plus_a -> Id "=" "[" ("+" | "*" ":" Id)+ "]" ";"
        let CtxNvPlusA::V1 { id, plus } = ctx;
    }

    fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) {
        // nv_l_star_a -> Id "=" "[" (<L> "+" | "*" ":" Id)* "]" ";"
        let CtxNvLStarA::V1 { id } = ctx;
    }

    fn exit_nv_l_star_a_i(&mut self, ctx: CtxNvLStarAI, spans: Vec<PosSpan>) {
        match ctx {
            // `<L> "+"` iteration in `nv_l_star_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" ":" Id)* "]" ";"`
            CtxNvLStarAI::V1 => {}
            // `"*" ":" Id` iteration in `nv_l_star_a -> Id "=" "[" (<L> "+" |  ►► "*" ":" Id ◄◄ )* "]" ";"`
            CtxNvLStarAI::V2 { id } => {}
        }
    }

    fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) {
        // nv_l_plus_a -> Id "=" "[" (<L> "+" | "*" ":" Id)+ "]" ";"
        let CtxNvLPlusA::V1 { id } = ctx;
    }

    fn exit_nv_l_plus_a_i(&mut self, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>) {
        match ctx {
            // `<L> "+"` iteration in `nv_l_plus_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" ":" Id)+ "]" ";"`
            CtxNvLPlusAI::V1 { last_iteration } => {}
            // `"*" ":" Id` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" ":" Id ◄◄ )+ "]" ";"`
            CtxNvLPlusAI::V2 { id, last_iteration } => {}
        }
    }

    fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) {
        // nv_sep_list -> Id "=" ("*" / "," "then")+ ";"
        let CtxNvSepList::V1 { id } = ctx;
    }

    fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) {
        match ctx {
            // nv_sep_list_opt -> Id "=" ("*" / "," "then")+ ";"
            CtxNvSepListOpt::V1 { id } => {}
            // nv_sep_list_opt -> Id "=" ";"
            CtxNvSepListOpt::V2 { id } => {}
        }
    }

    fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) {
        match ctx {
            // nv_rrec_i -> "," "*" nv_rrec_i
            CtxNvRrecI::V1 => {}
            // nv_rrec_i -> ";"
            CtxNvRrecI::V2 => {}
        }
    }

    fn exit_nv_l_rrec_i(&mut self, ctx: CtxNvLRrecI, spans: Vec<PosSpan>) {
        match ctx {
            // nv_l_rrec_i -> <L> "," "*" nv_l_rrec_i
            CtxNvLRrecI::V1 => {}
            // nv_l_rrec_i -> ";"
            CtxNvLRrecI::V2 => {}
        }
    }

    fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) {
        match ctx {
            // nv_lrec_i -> nv_lrec_i "," "*"
            CtxNvLrecI::V1 => {}
            // nv_lrec_i -> "+"
            CtxNvLrecI::V2 => {}
        }
    }
}

// -------------------------------------------------------------------------

pub mod pandemonium_lexer {
    // Generated code, don't modify manually anything between the tags below

    // [pandemonium_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, LexStateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 35;
    const INITIAL_STATE: LexStateId = 0;
    const FIRST_END_STATE: LexStateId = 24;
    const NBR_STATES: LexStateId = 76;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         29,  29,  29,  29,  29,  29,  29,  29,  29,   0,  32,  29,  29,  32,  29,  29,   // 0-15
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,   // 16-31
          0,  29,  29,  29,  29,  29,  29,  29,   1,   2,   3,   4,   5,   6,  29,   7,   // 32-47
         22,   8,   8,   8,   8,   8,   8,   8,   8,   8,   9,  10,  29,  11,  29,  29,   // 48-63
         29,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,   // 64-79
         27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  13,  29,  14,  15,  28,   // 80-95
         29,  16,  34,  24,  27,  23,  27,  27,  31,  25,  27,  27,  17,  30,  33,  26,   // 96-111
         18,  27,  19,  20,  21,  12,  27,  27,  27,  27,  27,  29,  29,  29,  29,  29,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 29),
        (Seg(57344, 1114111), 29),
    ];
    static TERMINAL_TABLE: [Terminal;52] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [LexStateId; 2661] = [
         24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  76,  36,  36,  36,  36,  36,  76,  76,  36,  36,  24,  36,  36, // state 0
          1,   1,   1,  22,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  68,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 2
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  69,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 3
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,   5,   6,   7,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 4
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  11,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 5
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  14,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 6
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,   8,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 7
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,   9,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 8
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  58,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 9
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  70,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 10
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  12,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 11
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  59,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 12
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  71,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 13
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  15,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 14
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  60,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 15
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  17,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 16
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  18,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 17
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  73,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 18
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  20,  76,  76,  76,  76,  76,  76,  76,  76, // state 19
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  21,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 20
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  74,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 21
          1,   1,   1,  22,   1,   1,   1,  75,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 22
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  16,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 23
         24,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  24,  76,  76, // state 24 <skip>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 25 <end:4>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 26 <end:7>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 27 <end:6>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 28 <end:0>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 29 <end:11>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 30 <end:9>
         76,  76,  76,   1,  76,  76,  76,  46,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 31 <end:1>
         76,  76,  76,  76,  76,  76,  76,  76,  32,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  32,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 32 <end:29>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 33 <end:10>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 34 <end:12>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 35 <end:2>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 36 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 37 <end:5>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 38 <end:8>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 39 <end:3>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  66,  36,  76,  36,  36, // state 40 <end:28>
         76,  76,  76,  76,  76,  76,   4,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  57,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 41 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  54,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 42 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  61,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 43 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  51,  36,  50,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 44 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  47,  76,  36,  36, // state 45 <end:28>
         46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  46,  76,  46,  46, // state 46 <skip>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  48,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 47 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  49,  36, // state 48 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 49 <end:13>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  72,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 50 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  52,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 51 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  53,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 52 <end:28>
         76,  76,  76,  76,  76,  76,   2,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 53 <end:14>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  55,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 54 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  56,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 55 <end:28>
         76,  76,  76,  76,  76,  76,   3,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 56 <end:15>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  64,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 57 <end:28>
         76,  76,  76,  76,  76,  76,  10,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 58 <end:16>
         76,  76,  76,  76,  76,  76,  13,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 59 <end:17>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 60 <end:18>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  62,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 61 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  63,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 62 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 63 <end:19>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  65,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 64 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 65 <end:20>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  67, // state 66 <end:28>
         76,  76,  76,  76,  76,  76,  76,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 67 <end:21>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 68 <end:22>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 69 <end:23>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 70 <end:24>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 71 <end:25>
         76,  76,  76,  76,  76,  76,  23,  76,  36,  76,  76,  76,  36,  76,  76,  76,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  76,  36,  36,  76,  36,  36, // state 72 <end:28>
         76,  76,  76,  76,  76,  76,  19,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 73 <end:26>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 74 <end:27>
         76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76,  76, // state 75 <skip>
         76 // error group in [nbr_state * nbr_group + nbr_group]
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

#[allow(unused)]
pub mod pandemonium_parser {
    // Generated code, don't modify manually anything between the tags below

    // [pandemonium_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, LLParser, ListenerWrapper, OpCode, Terminate}};

    const PARSER_NUM_T: usize = 30;
    const PARSER_NUM_NT: usize = 81;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Div", Some("/")), ("Equal", Some("=")), ("Exp", Some("^")), ("Lpar", Some("(")), ("Lsbracket", Some("[")), ("Mul", Some("*")), ("Rpar", Some(")")), ("Rsbracket", Some("]")), ("Sub", Some("-")), ("Colon", Some(":")), ("Comma", Some(",")), ("Semi", Some(";")), ("Then", Some("then")), ("Star", Some("star")), ("Plus", Some("plus")), ("L_Star", Some("l-star")), ("L_Plus", Some("l-plus")), ("L_Rrec", Some("l-rrec")), ("Rrec", Some("rrec")), ("Lrec", Some("lrec")), ("Amb", Some("amb")), ("Star_A", Some("star-a")), ("Plus_A", Some("plus-a")), ("L_Star_A", Some("l-star-a")), ("L_Plus_A", Some("l-plus-a")), ("SepList", Some("sep-list")), ("SepList_Opt", Some("sep-list-opt")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["text", "i", "nv_i", "example", "star", "plus", "l_star", "l_star_i", "l_plus", "l_plus_i", "rrec", "l_rrec", "lrec", "amb", "star_a", "plus_a", "l_star_a", "l_star_a_i", "l_plus_a", "l_plus_a_i", "sep_list", "sep_list_opt", "rrec_i", "l_rrec_i", "lrec_i", "amb_i", "nv_example", "nv_star", "nv_plus", "nv_l_star", "nv_l_star_i", "nv_l_plus", "nv_l_plus_i", "nv_rrec", "nv_l_rrec", "nv_lrec", "nv_star_a", "nv_plus_a", "nv_l_star_a", "nv_l_star_a_i", "nv_l_plus_a", "nv_l_plus_a_i", "nv_sep_list", "nv_sep_list_opt", "nv_rrec_i", "nv_l_rrec_i", "nv_lrec_i", "star_1", "plus_1", "star_a_1", "plus_a_1", "sep_list_1", "sep_list_opt_1", "nv_star_1", "nv_plus_1", "nv_star_a_1", "nv_plus_a_1", "nv_sep_list_1", "nv_sep_list_opt_1", "lrec_i_1", "amb_i_1", "amb_i_2", "amb_i_3", "amb_i_4", "amb_i_5", "amb_i_6", "nv_lrec_i_1", "l_plus_i_1", "l_plus_a_i_1", "l_plus_a_i_2", "sep_list_opt_2", "nv_l_plus_i_1", "nv_l_plus_a_i_1", "nv_l_plus_a_i_2", "nv_sep_list_opt_2", "plus_2", "plus_a_2", "plus_a_3", "nv_plus_2", "nv_plus_a_2", "nv_plus_a_3"];
    static ALT_VAR: [VarId; 160] = [0, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 17, 17, 18, 19, 19, 20, 21, 22, 22, 23, 23, 24, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 28, 29, 30, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 39, 39, 40, 41, 41, 42, 43, 44, 44, 45, 45, 46, 47, 47, 48, 49, 49, 49, 50, 50, 51, 51, 52, 52, 53, 53, 54, 55, 55, 55, 56, 56, 57, 57, 58, 58, 59, 59, 60, 60, 60, 60, 60, 60, 61, 62, 62, 62, 62, 63, 64, 64, 65, 65, 65, 65, 66, 66, 67, 67, 68, 68, 69, 69, 70, 70, 71, 71, 72, 72, 73, 73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 79, 79, 80, 80];
    static PARSING_TABLE: [AltId; 2511] = [160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 0, 160, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 2, 160, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 3, 3, 3, 3, 3, 3, 3, 160, 3, 3, 3, 3, 3, 3, 160, 160, 4, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 5, 6, 7, 8, 10, 9, 11, 12, 13, 14, 15, 16, 17, 18, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 19, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 20, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 21, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 22, 23, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 24, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 25, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 26, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 27, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 28, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 29, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 30, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 31, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 32, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 35, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 33, 34, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 36, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 37, 38, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 39, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 40, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 41, 42, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 43, 44, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 45, 160, 160, 160, 160, 160, 46, 160, 160, 161, 160, 46, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 46, 46, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 47, 48, 49, 50, 52, 51, 53, 160, 54, 55, 56, 57, 58, 59, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 60, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 61, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 62, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 63, 64, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 65, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 66, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 67, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 68, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 69, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 70, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 71, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 72, 160, 161, 73, 160, 160, 160, 160, 160, 74, 160, 75, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 76, 160, 161, 77, 160, 160, 160, 160, 160, 78, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 79, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 80, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 81, 82, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 83, 84, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 160, 160, 161, 85, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 86, 87, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 88, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 91, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 89, 90, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 92, 93, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 94, 95, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 96, 97, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 98, 99, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 100, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 101, 160, 160, 160, 160, 160, 102, 160, 103, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 104, 160, 160, 160, 160, 160, 105, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 106, 107, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 108, 109, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 110, 111, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 115, 114, 160, 112, 160, 160, 113, 117, 160, 116, 160, 160, 117, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 160, 161, 118, 160, 161, 161, 160, 118, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 118, 118, 160, 122, 121, 160, 119, 160, 160, 120, 122, 160, 122, 160, 160, 122, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 160, 161, 123, 160, 161, 161, 160, 123, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 123, 123, 160, 125, 125, 160, 124, 160, 160, 125, 125, 160, 125, 160, 160, 125, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 161, 161, 160, 161, 127, 160, 161, 161, 160, 126, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 128, 129, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 130, 131, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 132, 133, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 135, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 134, 134, 160, 160, 160, 160, 160, 160, 160, 160, 160, 137, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 136, 136, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 138, 160, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 139, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 140, 141, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 142, 160, 160, 160, 160, 160, 142, 160, 143, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 144, 160, 160, 160, 160, 160, 144, 160, 145, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 146, 160, 160, 160, 160, 160, 147, 160, 161, 161, 161, 161, 161, 161, 161, 160, 161, 161, 161, 161, 161, 161, 160, 160, 161, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 148, 149, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 151, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 150, 150, 160, 160, 160, 160, 160, 160, 160, 160, 160, 153, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 152, 152, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 154, 155, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 156, 160, 160, 160, 160, 160, 156, 160, 157, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 158, 160, 160, 160, 160, 160, 158, 160, 159, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160];
    static OPCODES: [&[OpCode]; 160] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::T(12), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(3)], &[OpCode::Exit(2)], &[OpCode::Loop(2), OpCode::Exit(3), OpCode::NT(26)], &[OpCode::Exit(4)], &[OpCode::Exit(5), OpCode::NT(4), OpCode::T(14)], &[OpCode::Exit(6), OpCode::NT(5), OpCode::T(15)], &[OpCode::Exit(7), OpCode::NT(6), OpCode::T(16)], &[OpCode::Exit(8), OpCode::NT(8), OpCode::T(17)], &[OpCode::Exit(9), OpCode::NT(10), OpCode::T(19)], &[OpCode::Exit(10), OpCode::NT(11), OpCode::T(18)], &[OpCode::Exit(11), OpCode::NT(12), OpCode::T(20)], &[OpCode::Exit(12), OpCode::NT(13), OpCode::T(21)], &[OpCode::Exit(13), OpCode::NT(14), OpCode::T(22)], &[OpCode::Exit(14), OpCode::NT(15), OpCode::T(23)], &[OpCode::Exit(15), OpCode::NT(16), OpCode::T(24)], &[OpCode::Exit(16), OpCode::NT(18), OpCode::T(25)], &[OpCode::Exit(17), OpCode::NT(20), OpCode::T(26)], &[OpCode::Exit(18), OpCode::NT(21), OpCode::T(27)], &[OpCode::Exit(19), OpCode::T(12), OpCode::NT(47), OpCode::T(28), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(20), OpCode::T(12), OpCode::NT(48), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(21), OpCode::T(12), OpCode::NT(7), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Loop(7), OpCode::Exit(22), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(23)], &[OpCode::Exit(24), OpCode::T(12), OpCode::NT(9), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(67), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(26), OpCode::NT(22), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(27), OpCode::NT(23), OpCode::T(29), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(28), OpCode::T(12), OpCode::NT(24), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(29), OpCode::T(12), OpCode::NT(25), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(30), OpCode::T(12), OpCode::T(8), OpCode::NT(49), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(31), OpCode::T(12), OpCode::T(8), OpCode::NT(50), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(32), OpCode::T(12), OpCode::T(8), OpCode::NT(17), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Loop(17), OpCode::Exit(33), OpCode::T(28)], &[OpCode::Loop(17), OpCode::Exit(34), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Exit(35)], &[OpCode::Exit(36), OpCode::T(12), OpCode::T(8), OpCode::NT(19), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(68), OpCode::T(28)], &[OpCode::NT(69), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Exit(39), OpCode::T(12), OpCode::NT(51), OpCode::T(29), OpCode::T(10), OpCode::T(28), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(70), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(41), OpCode::NT(22), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(42), OpCode::T(12)], &[OpCode::Loop(23), OpCode::Exit(43), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(44), OpCode::T(12)], &[OpCode::NT(59), OpCode::Exit(45), OpCode::T(29)], &[OpCode::NT(60), OpCode::Exit(46), OpCode::NT(65)], &[OpCode::Exit(47), OpCode::NT(27), OpCode::T(14)], &[OpCode::Exit(48), OpCode::NT(28), OpCode::T(15)], &[OpCode::Exit(49), OpCode::NT(29), OpCode::T(16)], &[OpCode::Exit(50), OpCode::NT(31), OpCode::T(17)], &[OpCode::Exit(51), OpCode::NT(33), OpCode::T(19)], &[OpCode::Exit(52), OpCode::NT(34), OpCode::T(18)], &[OpCode::Exit(53), OpCode::NT(35), OpCode::T(20)], &[OpCode::Exit(54), OpCode::NT(36), OpCode::T(22)], &[OpCode::Exit(55), OpCode::NT(37), OpCode::T(23)], &[OpCode::Exit(56), OpCode::NT(38), OpCode::T(24)], &[OpCode::Exit(57), OpCode::NT(40), OpCode::T(25)], &[OpCode::Exit(58), OpCode::NT(42), OpCode::T(26)], &[OpCode::Exit(59), OpCode::NT(43), OpCode::T(27)], &[OpCode::Exit(60), OpCode::T(12), OpCode::NT(53), OpCode::T(0), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(61), OpCode::T(12), OpCode::NT(54), OpCode::T(0), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(62), OpCode::T(12), OpCode::NT(30), OpCode::T(0), OpCode::T(2), OpCode::T(28)], &[OpCode::Loop(30), OpCode::Exit(63), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(64)], &[OpCode::Exit(65), OpCode::T(12), OpCode::NT(32), OpCode::T(0), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(71), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(67), OpCode::NT(44), OpCode::T(0), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(68), OpCode::NT(45), OpCode::T(0), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(69), OpCode::T(12), OpCode::NT(46), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(70), OpCode::T(12), OpCode::T(8), OpCode::NT(55), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(71), OpCode::T(12), OpCode::T(8), OpCode::NT(56), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(72), OpCode::T(12), OpCode::T(8), OpCode::NT(39), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::Loop(39), OpCode::Exit(73), OpCode::T(0)], &[OpCode::Loop(39), OpCode::Exit(74), OpCode::T(28), OpCode::T(10), OpCode::T(6)], &[OpCode::Exit(75)], &[OpCode::Exit(76), OpCode::T(12), OpCode::T(8), OpCode::NT(41), OpCode::T(5), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(72), OpCode::T(0)], &[OpCode::NT(73), OpCode::T(28), OpCode::T(10), OpCode::T(6)], &[OpCode::Exit(79), OpCode::T(12), OpCode::NT(57), OpCode::T(6), OpCode::T(2), OpCode::T(28)], &[OpCode::NT(74), OpCode::T(2), OpCode::T(28)], &[OpCode::Exit(81), OpCode::NT(44), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(82), OpCode::T(12)], &[OpCode::Loop(45), OpCode::Exit(83), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(84), OpCode::T(12)], &[OpCode::NT(66), OpCode::Exit(85), OpCode::T(0)], &[OpCode::Loop(47), OpCode::Exit(86), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(87)], &[OpCode::NT(75), OpCode::T(29), OpCode::T(11)], &[OpCode::Loop(49), OpCode::Exit(89), OpCode::T(28)], &[OpCode::Loop(49), OpCode::Exit(90), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Exit(91)], &[OpCode::NT(76), OpCode::T(28)], &[OpCode::NT(77), OpCode::T(28), OpCode::T(10), OpCode::T(29)], &[OpCode::Loop(51), OpCode::Exit(94), OpCode::T(29), OpCode::T(10), OpCode::T(28), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(95)], &[OpCode::Loop(52), OpCode::Exit(96), OpCode::T(29), OpCode::T(10), OpCode::T(28), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(97)], &[OpCode::Loop(53), OpCode::Exit(98), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(99)], &[OpCode::NT(78), OpCode::T(6), OpCode::T(11)], &[OpCode::Loop(55), OpCode::Exit(101), OpCode::T(0)], &[OpCode::Loop(55), OpCode::Exit(102), OpCode::T(28), OpCode::T(10), OpCode::T(6)], &[OpCode::Exit(103)], &[OpCode::NT(79), OpCode::T(0)], &[OpCode::NT(80), OpCode::T(28), OpCode::T(10), OpCode::T(6)], &[OpCode::Loop(57), OpCode::Exit(106), OpCode::T(6), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(107)], &[OpCode::Loop(58), OpCode::Exit(108), OpCode::T(6), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(109)], &[OpCode::Loop(59), OpCode::Exit(110), OpCode::T(29), OpCode::T(11)], &[OpCode::Exit(111)], &[OpCode::Loop(60), OpCode::Exit(112), OpCode::NT(63), OpCode::T(3)], &[OpCode::Loop(60), OpCode::Exit(113), OpCode::NT(63), OpCode::T(6)], &[OpCode::Loop(60), OpCode::Exit(114), OpCode::NT(63), OpCode::T(1)], &[OpCode::Loop(60), OpCode::Exit(115), OpCode::NT(61), OpCode::T(0)], &[OpCode::Loop(60), OpCode::Exit(116), OpCode::NT(61), OpCode::T(9)], &[OpCode::Exit(117)], &[OpCode::NT(62), OpCode::Exit(118), OpCode::NT(65)], &[OpCode::Loop(62), OpCode::Exit(119), OpCode::NT(63), OpCode::T(3)], &[OpCode::Loop(62), OpCode::Exit(120), OpCode::NT(63), OpCode::T(6)], &[OpCode::Loop(62), OpCode::Exit(121), OpCode::NT(63), OpCode::T(1)], &[OpCode::Exit(122)], &[OpCode::NT(64), OpCode::Exit(123), OpCode::NT(65)], &[OpCode::Loop(64), OpCode::Exit(124), OpCode::NT(63), OpCode::T(3)], &[OpCode::Exit(125)], &[OpCode::Exit(126), OpCode::NT(65), OpCode::T(9)], &[OpCode::Exit(127), OpCode::T(7), OpCode::NT(25), OpCode::T(4)], &[OpCode::Exit(128), OpCode::T(28)], &[OpCode::Exit(129), OpCode::T(29)], &[OpCode::Loop(66), OpCode::Exit(130), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(131)], &[OpCode::Loop(9), OpCode::Exit(132)], &[OpCode::Exit(133)], &[OpCode::Loop(19), OpCode::Exit(134)], &[OpCode::Exit(135)], &[OpCode::Loop(19), OpCode::Exit(136)], &[OpCode::Exit(137)], &[OpCode::Exit(138), OpCode::T(12)], &[OpCode::Exit(139), OpCode::T(12), OpCode::NT(52), OpCode::T(29), OpCode::T(10), OpCode::T(28)], &[OpCode::Loop(32), OpCode::Exit(140)], &[OpCode::Exit(141)], &[OpCode::Loop(41), OpCode::Exit(142)], &[OpCode::Exit(143)], &[OpCode::Loop(41), OpCode::Exit(144)], &[OpCode::Exit(145)], &[OpCode::Exit(146), OpCode::T(12), OpCode::NT(58), OpCode::T(6)], &[OpCode::Exit(147), OpCode::T(12)], &[OpCode::Loop(48), OpCode::Exit(148)], &[OpCode::Exit(149)], &[OpCode::Loop(50), OpCode::Exit(150)], &[OpCode::Exit(151)], &[OpCode::Loop(50), OpCode::Exit(152)], &[OpCode::Exit(153)], &[OpCode::Loop(54), OpCode::Exit(154)], &[OpCode::Exit(155)], &[OpCode::Loop(56), OpCode::Exit(156)], &[OpCode::Exit(157)], &[OpCode::Loop(56), OpCode::Exit(158)], &[OpCode::Exit(159)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> LLParser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        LLParser::new(
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
    pub enum CtxText {
        /// `text -> (<L> example)* ";" (<L> nv_example)*`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxI {
        /// `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )* ";" (<L> nv_example)*`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvI {
        /// `<L> nv_example` iteration in `text -> (<L> example)* ";" ( ►► <L> nv_example ◄◄ )*`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxExample {
        /// `example -> "star" star`
        V1,
        /// `example -> "plus" plus`
        V2,
        /// `example -> "l-star" l_star`
        V3,
        /// `example -> "l-plus" l_plus`
        V4,
        /// `example -> "rrec" rrec`
        V5,
        /// `example -> "l-rrec" l_rrec`
        V6,
        /// `example -> "lrec" lrec`
        V7,
        /// `example -> "amb" amb`
        V8,
        /// `example -> "star-a" star_a`
        V9,
        /// `example -> "plus-a" plus_a`
        V10,
        /// `example -> "l-star-a" l_star_a`
        V11,
        /// `example -> "l-plus-a" l_plus_a`
        V12,
        /// `example -> "sep-list" sep_list`
        V13,
        /// `example -> "sep-list-opt" sep_list_opt`
        V14,
    }
    #[derive(Debug)]
    pub enum CtxStar {
        /// `star -> Id "=" Id ("," Num)* ";"`
        V1 { id: [String; 2], star: SynStar1 },
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
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxLRrec {
        /// `l_rrec -> Id "=" Num l_rrec_i`
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxLrec {
        /// `lrec -> Id "=" lrec_i ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxAmb {
        /// `amb -> Id "=" amb_i ";"`
        V1 { id: String },
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
    pub enum CtxSepList {
        /// `sep_list -> Id "=" (Id ":" Num / "," "then")+ ";"`
        V1 { id: String, plus: SynSepList1 },
    }
    #[derive(Debug)]
    pub enum CtxSepListOpt {
        /// `sep_list_opt -> Id "=" (Id ":" Num / "," "then")+ ";"`
        V1 { id: String, plus: SynSepListOpt1 },
        /// `sep_list_opt -> Id "=" ";"`
        V2 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxRrecI {
        /// `rrec_i -> "," Num rrec_i`
        V1 { num: String },
        /// `rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxLRrecI {
        /// `l_rrec_i -> <L> "," Num l_rrec_i`
        V1 { num: String },
        /// `l_rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxLrecI {
        /// `lrec_i -> lrec_i "," Num`
        V1 { num: String },
        /// `lrec_i -> Num`
        V2 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxAmbI {
        /// `amb_i -> "-" amb_i`
        V1,
        /// `amb_i -> <R> amb_i "^" amb_i`
        V2,
        /// `amb_i -> amb_i "*" amb_i`
        V3,
        /// `amb_i -> amb_i <P> "/" amb_i`
        V4,
        /// `amb_i -> amb_i "+" amb_i`
        V5,
        /// `amb_i -> amb_i <P> "-" amb_i`
        V6,
        /// `amb_i -> "(" amb_i ")"`
        V7,
        /// `amb_i -> Id`
        V8 { id: String },
        /// `amb_i -> Num`
        V9 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxNvExample {
        /// `nv_example -> "star" nv_star`
        V1,
        /// `nv_example -> "plus" nv_plus`
        V2,
        /// `nv_example -> "l-star" nv_l_star`
        V3,
        /// `nv_example -> "l-plus" nv_l_plus`
        V4,
        /// `nv_example -> "rrec" nv_rrec`
        V5,
        /// `nv_example -> "l-rrec" nv_l_rrec`
        V6,
        /// `nv_example -> "lrec" nv_lrec`
        V7,
        /// `nv_example -> "star-a" nv_star_a`
        V8,
        /// `nv_example -> "plus-a" nv_plus_a`
        V9,
        /// `nv_example -> "l-star-a" nv_l_star_a`
        V10,
        /// `nv_example -> "l-plus-a" nv_l_plus_a`
        V11,
        /// `nv_example -> "sep-list" nv_sep_list`
        V12,
        /// `nv_example -> "sep-list-opt" nv_sep_list_opt`
        V13,
    }
    #[derive(Debug)]
    pub enum CtxNvStar {
        /// `nv_star -> Id "=" "+" ("," "*")* ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvPlus {
        /// `nv_plus -> Id "=" "+" ("," "*")+ ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLStar {
        /// `nv_l_star -> Id "=" "+" (<L> "," "*")* ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLStarI {
        /// `<L> "," "*"` iteration in `nv_l_star -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )* ";"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvLPlus {
        /// `nv_l_plus -> Id "=" "+" (<L> "," "*")+ ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLPlusI {
        /// `<L> "," "*"` iteration in `nv_l_plus -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )+ ";"`
        V1 { last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxNvRrec {
        /// `nv_rrec -> Id "=" "+" nv_rrec_i`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLRrec {
        /// `nv_l_rrec -> Id "=" "+" nv_l_rrec_i`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLrec {
        /// `nv_lrec -> Id "=" nv_lrec_i ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvStarA {
        /// `nv_star_a -> Id "=" "[" ("+" | "*" ":" Id)* "]" ";"`
        V1 { id: String, star: SynNvStarA1 },
    }
    #[derive(Debug)]
    pub enum CtxNvPlusA {
        /// `nv_plus_a -> Id "=" "[" ("+" | "*" ":" Id)+ "]" ";"`
        V1 { id: String, plus: SynNvPlusA1 },
    }
    #[derive(Debug)]
    pub enum CtxNvLStarA {
        /// `nv_l_star_a -> Id "=" "[" (<L> "+" | "*" ":" Id)* "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLStarAI {
        /// `<L> "+"` iteration in `nv_l_star_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" ":" Id)* "]" ";"`
        V1,
        /// `"*" ":" Id` iteration in `nv_l_star_a -> Id "=" "[" (<L> "+" |  ►► "*" ":" Id ◄◄ )* "]" ";"`
        V2 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLPlusA {
        /// `nv_l_plus_a -> Id "=" "[" (<L> "+" | "*" ":" Id)+ "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLPlusAI {
        /// `<L> "+"` iteration in `nv_l_plus_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" ":" Id)+ "]" ";"`
        V1 { last_iteration: bool },
        /// `"*" ":" Id` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" ":" Id ◄◄ )+ "]" ";"`
        V2 { id: String, last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxNvSepList {
        /// `nv_sep_list -> Id "=" ("*" / "," "then")+ ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvSepListOpt {
        /// `nv_sep_list_opt -> Id "=" ("*" / "," "then")+ ";"`
        V1 { id: String },
        /// `nv_sep_list_opt -> Id "=" ";"`
        V2 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvRrecI {
        /// `nv_rrec_i -> "," "*" nv_rrec_i`
        V1,
        /// `nv_rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxNvLRrecI {
        /// `nv_l_rrec_i -> <L> "," "*" nv_l_rrec_i`
        V1,
        /// `nv_l_rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxNvLrecI {
        /// `nv_lrec_i -> nv_lrec_i "," "*"`
        V1,
        /// `nv_lrec_i -> "+"`
        V2,
    }

    /// Computed `("," Num)*` array in `star -> Id "=" Id  ►► ("," Num)* ◄◄  ";"`
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
    /// Computed `(Id ":" Num / "," "then")+` array in `sep_list -> Id "="  ►► (Id ":" Num / "," "then")+ ◄◄  ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepList1(pub Vec<SynSepList1Item>);
    /// `Id ":" Num / "," "then"` item in `sep_list -> Id "=" ( ►► Id ":" Num / "," "then" ◄◄ )+ ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepList1Item { pub id: String, pub num: String }
    /// Computed `(Id ":" Num / "," "then")+` array in `sep_list_opt -> Id "="  ►► (Id ":" Num / "," "then")+ ◄◄  ";" | Id "=" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepListOpt1(pub Vec<SynSepListOpt1Item>);
    /// `Id ":" Num / "," "then"` item in `sep_list_opt -> Id "=" ( ►► Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynSepListOpt1Item { pub id: String, pub num: String }
    /// Computed `("+" | "*" ":" Id)*` array in `nv_star_a -> Id "=" "["  ►► ("+" | "*" ":" Id)* ◄◄  "]" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynNvStarA1(pub Vec<SynNvStarA1Item>);
    #[derive(Debug, PartialEq)]
    pub enum SynNvStarA1Item {
        /// `"+"` item in `nv_star_a -> Id "=" "[" ( ►► "+" ◄◄  | "*" ":" Id)* "]" ";"`
        V1 {  },
        /// `"*" ":" Id` item in `nv_star_a -> Id "=" "[" ("+" |  ►► "*" ":" Id ◄◄ )* "]" ";"`
        V2 { id: String },
    }
    /// Computed `("+" | "*" ":" Id)+` array in `nv_plus_a -> Id "=" "["  ►► ("+" | "*" ":" Id)+ ◄◄  "]" ";"`
    #[derive(Debug, PartialEq)]
    pub struct SynNvPlusA1(pub Vec<SynNvPlusA1Item>);
    #[derive(Debug, PartialEq)]
    pub enum SynNvPlusA1Item {
        /// `"+"` item in `nv_plus_a -> Id "=" "[" ( ►► "+" ◄◄  | "*" ":" Id)+ "]" ";"`
        V1 {  },
        /// `"*" ":" Id` item in `nv_plus_a -> Id "=" "[" ("+" |  ►► "*" ":" Id ◄◄ )+ "]" ";"`
        V2 { id: String },
    }
    /// Top non-terminal Text (has no value)
    #[derive(Debug, PartialEq)]
    pub struct SynText();

    #[derive(Debug)]
    enum EnumSynValue { Star1(SynStar1), Plus1(SynPlus1), StarA1(SynStarA1), PlusA1(SynPlusA1), SepList1(SynSepList1), SepListOpt1(SynSepListOpt1), NvStarA1(SynNvStarA1), NvPlusA1(SynNvPlusA1) }

    impl EnumSynValue {
        fn get_star1(self) -> SynStar1 {
            if let EnumSynValue::Star1(val) = self { val } else { panic!() }
        }
        fn get_plus1(self) -> SynPlus1 {
            if let EnumSynValue::Plus1(val) = self { val } else { panic!() }
        }
        fn get_star_a1(self) -> SynStarA1 {
            if let EnumSynValue::StarA1(val) = self { val } else { panic!() }
        }
        fn get_plus_a1(self) -> SynPlusA1 {
            if let EnumSynValue::PlusA1(val) = self { val } else { panic!() }
        }
        fn get_sep_list1(self) -> SynSepList1 {
            if let EnumSynValue::SepList1(val) = self { val } else { panic!() }
        }
        fn get_sep_list_opt1(self) -> SynSepListOpt1 {
            if let EnumSynValue::SepListOpt1(val) = self { val } else { panic!() }
        }
        fn get_nv_star_a1(self) -> SynNvStarA1 {
            if let EnumSynValue::NvStarA1(val) = self { val } else { panic!() }
        }
        fn get_nv_plus_a1(self) -> SynNvPlusA1 {
            if let EnumSynValue::NvPlusA1(val) = self { val } else { panic!() }
        }
    }

    pub trait PandemoniumListener {
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
        fn exit(&mut self, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_text(&mut self) {}
        #[allow(unused_variables)]
        fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) {}
        fn init_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_i(&mut self, ctx: CtxI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_i(&mut self) {}
        fn init_nv_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_i(&mut self, ctx: CtxNvI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_nv_i(&mut self) {}
        fn init_example(&mut self) {}
        #[allow(unused_variables)]
        fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) {}
        fn init_star(&mut self) {}
        #[allow(unused_variables)]
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) {}
        fn init_plus(&mut self) {}
        #[allow(unused_variables)]
        fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) {}
        fn init_l_star(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) {}
        fn init_l_star_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_star_i(&mut self, ctx: CtxLStarI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_l_star_i(&mut self) {}
        fn init_l_plus(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) {}
        fn init_l_plus_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_plus_i(&mut self, ctx: CtxLPlusI, spans: Vec<PosSpan>) {}
        fn init_rrec(&mut self) {}
        #[allow(unused_variables)]
        fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) {}
        fn init_l_rrec(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) {}
        fn init_lrec(&mut self) {}
        #[allow(unused_variables)]
        fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) {}
        fn init_amb(&mut self) {}
        #[allow(unused_variables)]
        fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) {}
        fn init_star_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) {}
        fn init_plus_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) {}
        fn init_l_star_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) {}
        fn init_l_star_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_star_a_i(&mut self, ctx: CtxLStarAI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_l_star_a_i(&mut self) {}
        fn init_l_plus_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) {}
        fn init_l_plus_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_plus_a_i(&mut self, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {}
        fn init_sep_list(&mut self) {}
        #[allow(unused_variables)]
        fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) {}
        fn init_sep_list_opt(&mut self) {}
        #[allow(unused_variables)]
        fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) {}
        fn init_rrec_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) {}
        fn init_l_rrec_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_rrec_i(&mut self, ctx: CtxLRrecI, spans: Vec<PosSpan>) {}
        fn init_lrec_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_lrec_i1(&mut self) {}
        fn init_amb_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) {}
        fn init_nv_example(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) {}
        fn init_nv_star(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) {}
        fn init_nv_plus(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) {}
        fn init_nv_l_star(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) {}
        fn init_nv_l_star_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star_i(&mut self, ctx: CtxNvLStarI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_nv_l_star_i(&mut self) {}
        fn init_nv_l_plus(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) {}
        fn init_nv_l_plus_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus_i(&mut self, ctx: CtxNvLPlusI, spans: Vec<PosSpan>) {}
        fn init_nv_rrec(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) {}
        fn init_nv_l_rrec(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_rrec(&mut self, ctx: CtxNvLRrec, spans: Vec<PosSpan>) {}
        fn init_nv_lrec(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) {}
        fn init_nv_star_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) {}
        fn init_nv_plus_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) {}
        fn init_nv_l_star_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) {}
        fn init_nv_l_star_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star_a_i(&mut self, ctx: CtxNvLStarAI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_nv_l_star_a_i(&mut self) {}
        fn init_nv_l_plus_a(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) {}
        fn init_nv_l_plus_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus_a_i(&mut self, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>) {}
        fn init_nv_sep_list(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) {}
        fn init_nv_sep_list_opt(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) {}
        fn init_nv_rrec_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) {}
        fn init_nv_l_rrec_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_rrec_i(&mut self, ctx: CtxNvLRrecI, spans: Vec<PosSpan>) {}
        fn init_nv_lrec_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exitloop_nv_lrec_i1(&mut self) {}
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<EnumSynValue>,
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
                    if matches!(nt, 1 | 2 | 7 | 9 | 17 | 19 | 23 | 30 | 32 | 39 | 41 | 45 | 47 ..= 50 | 53 ..= 56) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_text(),             // text
                        1 => self.listener.init_i(),                // i
                        2 => self.listener.init_nv_i(),             // nv_i
                        3 => self.listener.init_example(),          // example
                        4 => self.listener.init_star(),             // star
                        47 => self.init_star1(),                    // star_1
                        5 => self.listener.init_plus(),             // plus
                        48 => self.init_plus1(),                    // plus_1
                        75 => {}                                    // plus_2
                        6 => self.listener.init_l_star(),           // l_star
                        7 => self.listener.init_l_star_i(),         // l_star_i
                        8 => self.listener.init_l_plus(),           // l_plus
                        9 => self.listener.init_l_plus_i(),         // l_plus_i
                        67 => {}                                    // l_plus_i_1
                        10 => self.listener.init_rrec(),            // rrec
                        11 => self.listener.init_l_rrec(),          // l_rrec
                        12 => self.listener.init_lrec(),            // lrec
                        13 => self.listener.init_amb(),             // amb
                        14 => self.listener.init_star_a(),          // star_a
                        49 => self.init_star_a1(),                  // star_a_1
                        15 => self.listener.init_plus_a(),          // plus_a
                        50 => self.init_plus_a1(),                  // plus_a_1
                        76 | 77 => {}                               // plus_a_2, plus_a_3
                        16 => self.listener.init_l_star_a(),        // l_star_a
                        17 => self.listener.init_l_star_a_i(),      // l_star_a_i
                        18 => self.listener.init_l_plus_a(),        // l_plus_a
                        19 => self.listener.init_l_plus_a_i(),      // l_plus_a_i
                        68 | 69 => {}                               // l_plus_a_i_1, l_plus_a_i_2
                        20 => self.listener.init_sep_list(),        // sep_list
                        51 => self.init_sep_list1(),                // sep_list_1
                        21 => self.listener.init_sep_list_opt(),    // sep_list_opt
                        52 => self.init_sep_list_opt1(),            // sep_list_opt_1
                        70 => {}                                    // sep_list_opt_2
                        22 => self.listener.init_rrec_i(),          // rrec_i
                        23 => self.listener.init_l_rrec_i(),        // l_rrec_i
                        24 => self.listener.init_lrec_i(),          // lrec_i
                        59 => {}                                    // lrec_i_1
                        25 => self.listener.init_amb_i(),           // amb_i
                        60 ..= 65 => {}                             // amb_i_1, amb_i_2, amb_i_3, amb_i_4, amb_i_5, amb_i_6
                        26 => self.listener.init_nv_example(),      // nv_example
                        27 => self.listener.init_nv_star(),         // nv_star
                        53 => {}                                    // nv_star_1
                        28 => self.listener.init_nv_plus(),         // nv_plus
                        54 => {}                                    // nv_plus_1
                        78 => {}                                    // nv_plus_2
                        29 => self.listener.init_nv_l_star(),       // nv_l_star
                        30 => self.listener.init_nv_l_star_i(),     // nv_l_star_i
                        31 => self.listener.init_nv_l_plus(),       // nv_l_plus
                        32 => self.listener.init_nv_l_plus_i(),     // nv_l_plus_i
                        71 => {}                                    // nv_l_plus_i_1
                        33 => self.listener.init_nv_rrec(),         // nv_rrec
                        34 => self.listener.init_nv_l_rrec(),       // nv_l_rrec
                        35 => self.listener.init_nv_lrec(),         // nv_lrec
                        36 => self.listener.init_nv_star_a(),       // nv_star_a
                        55 => self.init_nv_star_a1(),               // nv_star_a_1
                        37 => self.listener.init_nv_plus_a(),       // nv_plus_a
                        56 => self.init_nv_plus_a1(),               // nv_plus_a_1
                        79 | 80 => {}                               // nv_plus_a_2, nv_plus_a_3
                        38 => self.listener.init_nv_l_star_a(),     // nv_l_star_a
                        39 => self.listener.init_nv_l_star_a_i(),   // nv_l_star_a_i
                        40 => self.listener.init_nv_l_plus_a(),     // nv_l_plus_a
                        41 => self.listener.init_nv_l_plus_a_i(),   // nv_l_plus_a_i
                        72 | 73 => {}                               // nv_l_plus_a_i_1, nv_l_plus_a_i_2
                        42 => self.listener.init_nv_sep_list(),     // nv_sep_list
                        57 => self.init_nv_sep_list1(),             // nv_sep_list_1
                        43 => self.listener.init_nv_sep_list_opt(), // nv_sep_list_opt
                        58 => self.init_nv_sep_list_opt1(),         // nv_sep_list_opt_1
                        74 => {}                                    // nv_sep_list_opt_2
                        44 => self.listener.init_nv_rrec_i(),       // nv_rrec_i
                        45 => self.listener.init_nv_l_rrec_i(),     // nv_l_rrec_i
                        46 => self.listener.init_nv_lrec_i(),       // nv_lrec_i
                        66 => {}                                    // nv_lrec_i_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_text(),                        // text -> i ";" nv_i
                        1 => self.exit_i(),                           // i -> <L> example i
                        2 => self.listener.exitloop_i(),              // i -> <L> ε
                        3 => self.exit_nv_i(),                        // nv_i -> <L> nv_example nv_i
                        4 => self.listener.exitloop_nv_i(),           // nv_i -> <L> ε
                        5 |                                           // example -> "star" star
                        6 |                                           // example -> "plus" plus
                        7 |                                           // example -> "l-star" l_star
                        8 |                                           // example -> "l-plus" l_plus
                        9 |                                           // example -> "rrec" rrec
                        10 |                                          // example -> "l-rrec" l_rrec
                        11 |                                          // example -> "lrec" lrec
                        12 |                                          // example -> "amb" amb
                        13 |                                          // example -> "star-a" star_a
                        14 |                                          // example -> "plus-a" plus_a
                        15 |                                          // example -> "l-star-a" l_star_a
                        16 |                                          // example -> "l-plus-a" l_plus_a
                        17 |                                          // example -> "sep-list" sep_list
                        18 => self.exit_example(alt_id),              // example -> "sep-list-opt" sep_list_opt
                        19 => self.exit_star(),                       // star -> Id "=" Id star_1 ";"
                        86 => self.exit_star1(),                      // star_1 -> "," Num star_1
                        87 => {}                                      // star_1 -> ε
                        20 => self.exit_plus(),                       // plus -> Id "=" Num plus_1 ";"
                        148 |                                         // plus_2 -> plus_1
                        149 => self.exit_plus1(),                     // plus_2 -> ε
                     /* 88 */                                         // plus_1 -> "," Num plus_2 (never called)
                        21 => self.exit_l_star(),                     // l_star -> Id "=" Num l_star_i ";"
                        22 => self.exit_l_star_i(),                   // l_star_i -> <L> "," Num l_star_i
                        23 => self.listener.exitloop_l_star_i(),      // l_star_i -> <L> ε
                        24 => self.exit_l_plus(),                     // l_plus -> Id "=" Num l_plus_i ";"
                        132 |                                         // l_plus_i_1 -> l_plus_i
                        133 => self.exit_l_plus_i(alt_id),            // l_plus_i_1 -> ε
                     /* 25 */                                         // l_plus_i -> <L> "," Num l_plus_i_1 (never called)
                        26 => self.exit_rrec(),                       // rrec -> Id "=" Num rrec_i
                        27 => self.exit_l_rrec(),                     // l_rrec -> Id "=" Num l_rrec_i
                        28 => self.exit_lrec(),                       // lrec -> Id "=" lrec_i ";"
                        29 => self.exit_amb(),                        // amb -> Id "=" amb_i ";"
                        30 => self.exit_star_a(),                     // star_a -> Id "=" "[" star_a_1 "]" ";"
                        89 |                                          // star_a_1 -> Id star_a_1
                        90 => self.exit_star_a1(alt_id),              // star_a_1 -> Num ":" Id star_a_1
                        91 => {}                                      // star_a_1 -> ε
                        31 => self.exit_plus_a(),                     // plus_a -> Id "=" "[" plus_a_1 "]" ";"
                        150 |                                         // plus_a_2 -> plus_a_1
                        151 |                                         // plus_a_2 -> ε
                        152 |                                         // plus_a_3 -> plus_a_1
                        153 => self.exit_plus_a1(alt_id),             // plus_a_3 -> ε
                     /* 92 */                                         // plus_a_1 -> Id plus_a_2 (never called)
                     /* 93 */                                         // plus_a_1 -> Num ":" Id plus_a_3 (never called)
                        32 => self.exit_l_star_a(),                   // l_star_a -> Id "=" "[" l_star_a_i "]" ";"
                        33 |                                          // l_star_a_i -> <L> Id l_star_a_i
                        34 => self.exit_l_star_a_i(alt_id),           // l_star_a_i -> <L> Num ":" Id l_star_a_i
                        35 => self.listener.exitloop_l_star_a_i(),    // l_star_a_i -> <L> ε
                        36 => self.exit_l_plus_a(),                   // l_plus_a -> Id "=" "[" l_plus_a_i "]" ";"
                        134 |                                         // l_plus_a_i_1 -> l_plus_a_i
                        135 |                                         // l_plus_a_i_1 -> ε
                        136 |                                         // l_plus_a_i_2 -> l_plus_a_i
                        137 => self.exit_l_plus_a_i(alt_id),          // l_plus_a_i_2 -> ε
                     /* 37 */                                         // l_plus_a_i -> <L> Id l_plus_a_i_1 (never called)
                     /* 38 */                                         // l_plus_a_i -> <L> Num ":" Id l_plus_a_i_2 (never called)
                        39 => self.exit_sep_list(),                   // sep_list -> Id "=" Id ":" Num sep_list_1 ";"
                        94 => self.exit_sep_list1(),                  // sep_list_1 -> "," "then" Id ":" Num sep_list_1
                        95 => {}                                      // sep_list_1 -> ε
                        138 |                                         // sep_list_opt_2 -> ";"
                        139 => self.exit_sep_list_opt(alt_id),        // sep_list_opt_2 -> Id ":" Num sep_list_opt_1 ";"
                        96 => self.exit_sep_list_opt1(),              // sep_list_opt_1 -> "," "then" Id ":" Num sep_list_opt_1
                        97 => {}                                      // sep_list_opt_1 -> ε
                     /* 40 */                                         // sep_list_opt -> Id "=" sep_list_opt_2 (never called)
                        41 |                                          // rrec_i -> "," Num rrec_i
                        42 => self.exit_rrec_i(alt_id),               // rrec_i -> ";"
                        43 |                                          // l_rrec_i -> <L> "," Num l_rrec_i
                        44 => self.exit_l_rrec_i(alt_id),             // l_rrec_i -> <L> ";"
                        45 => self.inter_lrec_i(),                    // lrec_i -> Num lrec_i_1
                        110 => self.exit_lrec_i1(),                   // lrec_i_1 -> "," Num lrec_i_1
                        111 => self.listener.exitloop_lrec_i1(),      // lrec_i_1 -> ε
                        112 |                                         // amb_i_1 -> <R> "^" amb_i_4 amb_i_1
                        113 |                                         // amb_i_1 -> "*" amb_i_4 amb_i_1
                        114 |                                         // amb_i_1 -> "/" amb_i_4 amb_i_1
                        115 |                                         // amb_i_1 -> "+" amb_i_2 amb_i_1
                        116 => self.exit_amb_i1(alt_id),              // amb_i_1 -> "-" amb_i_2 amb_i_1
                        119 |                                         // amb_i_3 -> <R> "^" amb_i_4 amb_i_3 (duplicate of 112)
                        124 => self.exit_amb_i1(112),                 // amb_i_5 -> <R> "^" amb_i_4 amb_i_5 (duplicate of 112)
                        120 => self.exit_amb_i1(113),                 // amb_i_3 -> "*" amb_i_4 amb_i_3 (duplicate of 113)
                        121 => self.exit_amb_i1(114),                 // amb_i_3 -> "/" amb_i_4 amb_i_3 (duplicate of 114)
                        126 |                                         // amb_i_6 -> "-" amb_i_6
                        127 |                                         // amb_i_6 -> "(" amb_i ")"
                        128 |                                         // amb_i_6 -> Id
                        129 => self.exit_amb_i6(alt_id),              // amb_i_6 -> Num
                        46 => {}                                      // amb_i -> amb_i_6 amb_i_1 (not used)
                        117 => {}                                     // amb_i_1 -> ε (not used)
                        118 => {}                                     // amb_i_2 -> amb_i_6 amb_i_3 (not used)
                        122 => {}                                     // amb_i_3 -> ε (not used)
                        123 => {}                                     // amb_i_4 -> amb_i_6 amb_i_5 (not used)
                        125 => {}                                     // amb_i_5 -> ε (not used)
                        47 |                                          // nv_example -> "star" nv_star
                        48 |                                          // nv_example -> "plus" nv_plus
                        49 |                                          // nv_example -> "l-star" nv_l_star
                        50 |                                          // nv_example -> "l-plus" nv_l_plus
                        51 |                                          // nv_example -> "rrec" nv_rrec
                        52 |                                          // nv_example -> "l-rrec" nv_l_rrec
                        53 |                                          // nv_example -> "lrec" nv_lrec
                        54 |                                          // nv_example -> "star-a" nv_star_a
                        55 |                                          // nv_example -> "plus-a" nv_plus_a
                        56 |                                          // nv_example -> "l-star-a" nv_l_star_a
                        57 |                                          // nv_example -> "l-plus-a" nv_l_plus_a
                        58 |                                          // nv_example -> "sep-list" nv_sep_list
                        59 => self.exit_nv_example(alt_id),           // nv_example -> "sep-list-opt" nv_sep_list_opt
                        60 => self.exit_nv_star(),                    // nv_star -> Id "=" "+" nv_star_1 ";"
                        98 => self.exit_nv_star1(),                   // nv_star_1 -> "," "*" nv_star_1
                        99 => {}                                      // nv_star_1 -> ε
                        61 => self.exit_nv_plus(),                    // nv_plus -> Id "=" "+" nv_plus_1 ";"
                        154 |                                         // nv_plus_2 -> nv_plus_1
                        155 => self.exit_nv_plus1(),                  // nv_plus_2 -> ε
                     /* 100 */                                        // nv_plus_1 -> "," "*" nv_plus_2 (never called)
                        62 => self.exit_nv_l_star(),                  // nv_l_star -> Id "=" "+" nv_l_star_i ";"
                        63 => self.exit_nv_l_star_i(),                // nv_l_star_i -> <L> "," "*" nv_l_star_i
                        64 => self.listener.exitloop_nv_l_star_i(),   // nv_l_star_i -> <L> ε
                        65 => self.exit_nv_l_plus(),                  // nv_l_plus -> Id "=" "+" nv_l_plus_i ";"
                        140 |                                         // nv_l_plus_i_1 -> nv_l_plus_i
                        141 => self.exit_nv_l_plus_i(alt_id),         // nv_l_plus_i_1 -> ε
                     /* 66 */                                         // nv_l_plus_i -> <L> "," "*" nv_l_plus_i_1 (never called)
                        67 => self.exit_nv_rrec(),                    // nv_rrec -> Id "=" "+" nv_rrec_i
                        68 => self.exit_nv_l_rrec(),                  // nv_l_rrec -> Id "=" "+" nv_l_rrec_i
                        69 => self.exit_nv_lrec(),                    // nv_lrec -> Id "=" nv_lrec_i ";"
                        70 => self.exit_nv_star_a(),                  // nv_star_a -> Id "=" "[" nv_star_a_1 "]" ";"
                        101 |                                         // nv_star_a_1 -> "+" nv_star_a_1
                        102 => self.exit_nv_star_a1(alt_id),          // nv_star_a_1 -> "*" ":" Id nv_star_a_1
                        103 => {}                                     // nv_star_a_1 -> ε
                        71 => self.exit_nv_plus_a(),                  // nv_plus_a -> Id "=" "[" nv_plus_a_1 "]" ";"
                        156 |                                         // nv_plus_a_2 -> nv_plus_a_1
                        157 |                                         // nv_plus_a_2 -> ε
                        158 |                                         // nv_plus_a_3 -> nv_plus_a_1
                        159 => self.exit_nv_plus_a1(alt_id),          // nv_plus_a_3 -> ε
                     /* 104 */                                        // nv_plus_a_1 -> "+" nv_plus_a_2 (never called)
                     /* 105 */                                        // nv_plus_a_1 -> "*" ":" Id nv_plus_a_3 (never called)
                        72 => self.exit_nv_l_star_a(),                // nv_l_star_a -> Id "=" "[" nv_l_star_a_i "]" ";"
                        73 |                                          // nv_l_star_a_i -> <L> "+" nv_l_star_a_i
                        74 => self.exit_nv_l_star_a_i(alt_id),        // nv_l_star_a_i -> <L> "*" ":" Id nv_l_star_a_i
                        75 => self.listener.exitloop_nv_l_star_a_i(), // nv_l_star_a_i -> <L> ε
                        76 => self.exit_nv_l_plus_a(),                // nv_l_plus_a -> Id "=" "[" nv_l_plus_a_i "]" ";"
                        142 |                                         // nv_l_plus_a_i_1 -> nv_l_plus_a_i
                        143 |                                         // nv_l_plus_a_i_1 -> ε
                        144 |                                         // nv_l_plus_a_i_2 -> nv_l_plus_a_i
                        145 => self.exit_nv_l_plus_a_i(alt_id),       // nv_l_plus_a_i_2 -> ε
                     /* 77 */                                         // nv_l_plus_a_i -> <L> "+" nv_l_plus_a_i_1 (never called)
                     /* 78 */                                         // nv_l_plus_a_i -> <L> "*" ":" Id nv_l_plus_a_i_2 (never called)
                        79 => self.exit_nv_sep_list(),                // nv_sep_list -> Id "=" "*" nv_sep_list_1 ";"
                        106 => self.exit_nv_sep_list1(),              // nv_sep_list_1 -> "," "then" "*" nv_sep_list_1
                        107 => {}                                     // nv_sep_list_1 -> ε
                        146 |                                         // nv_sep_list_opt_2 -> "*" nv_sep_list_opt_1 ";"
                        147 => self.exit_nv_sep_list_opt(alt_id),     // nv_sep_list_opt_2 -> ";"
                        108 => self.exit_nv_sep_list_opt1(),          // nv_sep_list_opt_1 -> "," "then" "*" nv_sep_list_opt_1
                        109 => {}                                     // nv_sep_list_opt_1 -> ε
                     /* 80 */                                         // nv_sep_list_opt -> Id "=" nv_sep_list_opt_2 (never called)
                        81 |                                          // nv_rrec_i -> "," "*" nv_rrec_i
                        82 => self.exit_nv_rrec_i(alt_id),            // nv_rrec_i -> ";"
                        83 |                                          // nv_l_rrec_i -> <L> "," "*" nv_l_rrec_i
                        84 => self.exit_nv_l_rrec_i(alt_id),          // nv_l_rrec_i -> <L> ";"
                        85 => self.inter_nv_lrec_i(),                 // nv_lrec_i -> "+" nv_lrec_i_1
                        130 => self.exit_nv_lrec_i1(),                // nv_lrec_i_1 -> "," "*" nv_lrec_i_1
                        131 => self.listener.exitloop_nv_lrec_i1(),   // nv_lrec_i_1 -> ε
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

        fn exit_text(&mut self) {
            let ctx = CtxText::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_text(ctx, spans);
        }

        fn exit_i(&mut self) {
            let ctx = CtxI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_i(ctx, spans);
        }

        fn exit_nv_i(&mut self) {
            let ctx = CtxNvI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_i(ctx, spans);
        }

        fn exit_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                5 => {
                    (2, CtxExample::V1)
                }
                6 => {
                    (2, CtxExample::V2)
                }
                7 => {
                    (2, CtxExample::V3)
                }
                8 => {
                    (2, CtxExample::V4)
                }
                9 => {
                    (2, CtxExample::V5)
                }
                10 => {
                    (2, CtxExample::V6)
                }
                11 => {
                    (2, CtxExample::V7)
                }
                12 => {
                    (2, CtxExample::V8)
                }
                13 => {
                    (2, CtxExample::V9)
                }
                14 => {
                    (2, CtxExample::V10)
                }
                15 => {
                    (2, CtxExample::V11)
                }
                16 => {
                    (2, CtxExample::V12)
                }
                17 => {
                    (2, CtxExample::V13)
                }
                18 => {
                    (2, CtxExample::V14)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_example")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_example(ctx, spans);
        }

        fn exit_star(&mut self) {
            let star = self.stack.pop().unwrap().get_star1();
            let id_2 = self.stack_t.pop().unwrap();
            let id_1 = self.stack_t.pop().unwrap();
            let ctx = CtxStar::V1 { id: [id_1, id_2], star };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_star(ctx, spans);
        }

        fn init_star1(&mut self) {
            let val = SynStar1(Vec::new());
            self.stack.push(EnumSynValue::Star1(val));
        }

        fn exit_star1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let Some(EnumSynValue::Star1(SynStar1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynStar1 item on wrapper stack");
            };
            star_acc.push(num);
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_plus(&mut self) {
            let plus = self.stack.pop().unwrap().get_plus1();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxPlus::V1 { id, num, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_plus(ctx, spans);
        }

        fn init_plus1(&mut self) {
            let val = SynPlus1(Vec::new());
            self.stack.push(EnumSynValue::Plus1(val));
        }

        fn exit_plus1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let Some(EnumSynValue::Plus1(SynPlus1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynPlus1 item on wrapper stack");
            };
            plus_acc.push(num);
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_l_star(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStar::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_star(ctx, spans);
        }

        fn exit_l_star_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLStarI::V1 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_star_i(ctx, spans);
        }

        fn exit_l_plus(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlus::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_plus(ctx, spans);
        }

        fn exit_l_plus_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 133;
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusI::V1 { num, last_iteration };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_plus_i(ctx, spans);
        }

        fn exit_rrec(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxRrec::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_rrec(ctx, spans);
        }

        fn exit_l_rrec(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLRrec::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_rrec(ctx, spans);
        }

        fn exit_lrec(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLrec::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_lrec(ctx, spans);
        }

        fn exit_amb(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxAmb::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_amb(ctx, spans);
        }

        fn exit_star_a(&mut self) {
            let star = self.stack.pop().unwrap().get_star_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_star_a(ctx, spans);
        }

        fn init_star_a1(&mut self) {
            let val = SynStarA1(Vec::new());
            self.stack.push(EnumSynValue::StarA1(val));
        }

        fn exit_star_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                89 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynStarA1Item::V1 { id })
                }
                90 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynStarA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_star_a1"),
            };
            let Some(EnumSynValue::StarA1(SynStarA1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynStarA1 item on wrapper stack");
            };
            star_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_plus_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxPlusA::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_plus_a(ctx, spans);
        }

        fn init_plus_a1(&mut self) {
            let val = SynPlusA1(Vec::new());
            self.stack.push(EnumSynValue::PlusA1(val));
        }

        fn exit_plus_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                150 | 151 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynPlusA1Item::V1 { id })
                }
                152 | 153 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynPlusA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_plus_a1"),
            };
            let Some(EnumSynValue::PlusA1(SynPlusA1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynPlusA1 item on wrapper stack");
            };
            plus_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_l_star_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStarA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_star_a(ctx, spans);
        }

        fn exit_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                33 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLStarAI::V1 { id })
                }
                34 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLStarAI::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_star_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_star_a_i(ctx, spans);
        }

        fn exit_l_plus_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_plus_a(ctx, spans);
        }

        fn exit_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                134 | 135 => {
                    let last_iteration = alt_id == 135;
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLPlusAI::V1 { id, last_iteration })
                }
                136 | 137 => {
                    let last_iteration = alt_id == 137;
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLPlusAI::V2 { num, id, last_iteration })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_plus_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_plus_a_i(ctx, spans);
        }

        fn exit_sep_list(&mut self) {
            let plus = self.stack.pop().unwrap().get_sep_list1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxSepList::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_sep_list(ctx, spans);
        }

        fn init_sep_list1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepList1Item { id, num };
            self.stack.push(EnumSynValue::SepList1(SynSepList1(vec![val])));
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_sep_list1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepList1Item { id, num };
            let Some(EnumSynValue::SepList1(SynSepList1(sep_list_acc))) = self.stack.last_mut() else {
                panic!("expected SynSepList1 item on wrapper stack");
            };
            sep_list_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                138 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxSepListOpt::V2 { id })
                }
                139 => {
                    let plus = self.stack.pop().unwrap().get_sep_list_opt1();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxSepListOpt::V1 { id, plus })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_sep_list_opt(ctx, spans);
        }

        fn init_sep_list_opt1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepListOpt1Item { id, num };
            self.stack.push(EnumSynValue::SepListOpt1(SynSepListOpt1(vec![val])));
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_sep_list_opt1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let val = SynSepListOpt1Item { id, num };
            let Some(EnumSynValue::SepListOpt1(SynSepListOpt1(sep_list_acc))) = self.stack.last_mut() else {
                panic!("expected SynSepListOpt1 item on wrapper stack");
            };
            sep_list_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                41 => {
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num })
                }
                42 => {
                    (1, CtxRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_rrec_i(ctx, spans);
        }

        fn exit_l_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                43 => {
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxLRrecI::V1 { num })
                }
                44 => {
                    (2, CtxLRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_rrec_i(ctx, spans);
        }

        fn inter_lrec_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLrecI::V2 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_lrec_i(ctx, spans);
        }

        fn exit_lrec_i1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLrecI::V1 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_lrec_i(ctx, spans);
        }

        fn exit_amb_i1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                112 => {
                    (3, CtxAmbI::V2)
                }
                113 => {
                    (3, CtxAmbI::V3)
                }
                114 => {
                    (3, CtxAmbI::V4)
                }
                115 => {
                    (3, CtxAmbI::V5)
                }
                116 => {
                    (3, CtxAmbI::V6)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_amb_i1")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_amb_i(ctx, spans);
        }

        fn exit_amb_i6(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                126 => {
                    (2, CtxAmbI::V1)
                }
                127 => {
                    (3, CtxAmbI::V7)
                }
                128 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V8 { id })
                }
                129 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V9 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_amb_i6")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_amb_i(ctx, spans);
        }

        fn exit_nv_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                47 => {
                    (2, CtxNvExample::V1)
                }
                48 => {
                    (2, CtxNvExample::V2)
                }
                49 => {
                    (2, CtxNvExample::V3)
                }
                50 => {
                    (2, CtxNvExample::V4)
                }
                51 => {
                    (2, CtxNvExample::V5)
                }
                52 => {
                    (2, CtxNvExample::V6)
                }
                53 => {
                    (2, CtxNvExample::V7)
                }
                54 => {
                    (2, CtxNvExample::V8)
                }
                55 => {
                    (2, CtxNvExample::V9)
                }
                56 => {
                    (2, CtxNvExample::V10)
                }
                57 => {
                    (2, CtxNvExample::V11)
                }
                58 => {
                    (2, CtxNvExample::V12)
                }
                59 => {
                    (2, CtxNvExample::V13)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_example")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_example(ctx, spans);
        }

        fn exit_nv_star(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvStar::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_star(ctx, spans);
        }

        fn exit_nv_star1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_plus(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvPlus::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_plus(ctx, spans);
        }

        fn exit_nv_plus1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_l_star(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLStar::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_star(ctx, spans);
        }

        fn exit_nv_l_star_i(&mut self) {
            let ctx = CtxNvLStarI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_star_i(ctx, spans);
        }

        fn exit_nv_l_plus(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLPlus::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_plus(ctx, spans);
        }

        fn exit_nv_l_plus_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 141;
            let ctx = CtxNvLPlusI::V1 { last_iteration };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_plus_i(ctx, spans);
        }

        fn exit_nv_rrec(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvRrec::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_rrec(ctx, spans);
        }

        fn exit_nv_l_rrec(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLRrec::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_rrec(ctx, spans);
        }

        fn exit_nv_lrec(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLrec::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_lrec(ctx, spans);
        }

        fn exit_nv_star_a(&mut self) {
            let star = self.stack.pop().unwrap().get_nv_star_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_star_a(ctx, spans);
        }

        fn init_nv_star_a1(&mut self) {
            let val = SynNvStarA1(Vec::new());
            self.stack.push(EnumSynValue::NvStarA1(val));
        }

        fn exit_nv_star_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                101 => {
                    (2, SynNvStarA1Item::V1 {  })
                }
                102 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, SynNvStarA1Item::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_star_a1"),
            };
            let Some(EnumSynValue::NvStarA1(SynNvStarA1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynNvStarA1 item on wrapper stack");
            };
            star_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_nv_plus_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvPlusA::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_plus_a(ctx, spans);
        }

        fn init_nv_plus_a1(&mut self) {
            let val = SynNvPlusA1(Vec::new());
            self.stack.push(EnumSynValue::NvPlusA1(val));
        }

        fn exit_nv_plus_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                156 | 157 => {
                    (2, SynNvPlusA1Item::V1 {  })
                }
                158 | 159 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, SynNvPlusA1Item::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_plus_a1"),
            };
            let Some(EnumSynValue::NvPlusA1(SynNvPlusA1(plus_acc))) = self.stack.last_mut() else {
                panic!("expected SynNvPlusA1 item on wrapper stack");
            };
            plus_acc.push(val);
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_l_star_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLStarA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_star_a(ctx, spans);
        }

        fn exit_nv_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                73 => {
                    (2, CtxNvLStarAI::V1)
                }
                74 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvLStarAI::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_star_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_star_a_i(ctx, spans);
        }

        fn exit_nv_l_plus_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLPlusA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_plus_a(ctx, spans);
        }

        fn exit_nv_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                142 | 143 => {
                    let last_iteration = alt_id == 143;
                    (2, CtxNvLPlusAI::V1 { last_iteration })
                }
                144 | 145 => {
                    let last_iteration = alt_id == 145;
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvLPlusAI::V2 { id, last_iteration })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_plus_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_plus_a_i(ctx, spans);
        }

        fn exit_nv_sep_list(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvSepList::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_sep_list(ctx, spans);
        }

        fn init_nv_sep_list1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_sep_list1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                146 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvSepListOpt::V1 { id })
                }
                147 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxNvSepListOpt::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_sep_list_opt(ctx, spans);
        }

        fn init_nv_sep_list_opt1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_sep_list_opt1(&mut self) {
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                81 => {
                    (3, CtxNvRrecI::V1)
                }
                82 => {
                    (1, CtxNvRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_rrec_i(ctx, spans);
        }

        fn exit_nv_l_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                83 => {
                    (3, CtxNvLRrecI::V1)
                }
                84 => {
                    (2, CtxNvLRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_rrec_i(ctx, spans);
        }

        fn inter_nv_lrec_i(&mut self) {
            let ctx = CtxNvLrecI::V2;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_lrec_i(ctx, spans);
        }

        fn exit_nv_lrec_i1(&mut self) {
            let ctx = CtxNvLrecI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_lrec_i(ctx, spans);
        }
    }

    // [pandemonium_parser]
}
