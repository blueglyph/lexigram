// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

// =============================================================================================
// Parser to test grammar constructions

use lexigram_core::{CollectJoin, LALR};
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Terminate;
use lexigram_core::parser::lr_parser::LRParser;
use lexigram_core::text_span::{GetLine, GetTextSpan};
use pandemonium_lexer::build_lexer;
use pandemonium_parser::*;
use crate::lalr::{SPANS1, SPANS2, TXT1, TXT2};

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

#[test]
fn test_pandemonium() {
    let mut demo = PanDemo::new();
    static TESTS: &[(&str, &[&str])] = &[(TXT1, SPANS1), (TXT2, SPANS2)];
    for (i, &(txt, expected_spans)) in TESTS.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\nTest {i}\n{0:-<80}", ""); }
        match demo.parse(txt) {
            Ok(PanDemoResult { log, spans, rebuilt_txt }) => {
                if VERBOSE {
                    println!("parsing successful\n{log}");
                    println!("Spans:\n{}", spans.join("\n"));
                }
                // checks that the text rebuilt from spans matches the original:
                assert!(txt.contains(&rebuilt_txt), "rebuilt text is wrong in test {i}:\n{rebuilt_txt:?}");
                // checks the individual spans:
                // (tedious visual verification each time the test changes!)
                assert_eq!(
                    spans, expected_spans, "span mismatch in test {i}:\n{}",
                    spans.iter().zip(expected_spans).enumerate()
                        .find_map(|(i, (left, right))| {
                            if left != right {
                                Some(format!("{i}:\t{left}\n\t{right}"))
                            } else {
                                None
                            }
                        })
                        .unwrap_or_else(|| "different number of strings".to_string())
                );
            },
            Err(log) => panic!("errors during parsing in test {i}:\n{log}"),
        }
    }
}

// -------------------------------------------------------------------------
// minimalist parser, top level

pub struct PanDemo<'l, 'p, 'ls> {
    lexer: Lexer<'l, &'ls [u8]>,
    parser: LRParser<'p, LALR>,
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

    fn exit_i(&mut self, ctx: CtxI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )* ";" (<L> nv_example)*`
        let CtxI::V1 = ctx;
    }

    fn exit_nv_i(&mut self, ctx: CtxNvI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> nv_example` iteration in `text -> (<L> example)* ";" ( ►► <L> nv_example ◄◄ )*`
        let CtxNvI::V1 = ctx;
    }

    fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // example -> "star" star
            CtxExample::V1 => {}
            // example -> "plus" plus
            CtxExample::V2 => {}
            // example -> "l-star" l_star
            CtxExample::V3 => {}
            // example -> "l-plus" l_plus
            CtxExample::V4 => {}
            // example -> "rrec" rrec
            CtxExample::V5 => {}
            // example -> "lrec" lrec
            CtxExample::V6 => {}
            // example -> "amb" amb
            CtxExample::V7 => {}
            // example -> "star-a" star_a
            CtxExample::V8 => {}
            // example -> "plus-a" plus_a
            CtxExample::V9 => {}
            // example -> "l-star-a" l_star_a
            CtxExample::V10 => {}
            // example -> "l-plus-a" l_plus_a
            CtxExample::V11 => {}
            // example -> "sep-list" sep_list
            CtxExample::V12 => {}
            // example -> "sep-list-opt" sep_list_opt
            CtxExample::V13 => {}
            // example -> "l-sep-list" l_sep_list
            CtxExample::V14 => {}
            // example -> "l-sep-list-opt" l_sep_list_opt
            CtxExample::V15 => {}
        }
    }

    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // star -> Id "=" Id ("," Num)* ";"
        let CtxStar::V1 { id, star } = ctx;
    }

    fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // plus -> Id "=" Num ("," Num)+ ";"
        let CtxPlus::V1 { id, num, plus } = ctx;
    }

    fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_star -> Id "=" Num (<L> "," Num)* ";"
        let CtxLStar::V1 { id, num } = ctx;
    }

    fn exit_l_star_i(&mut self, ctx: CtxLStarI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "," Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," Num ◄◄ )* ";"`
        let CtxLStarI::V1 { num } = ctx;
    }

    fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_plus -> Id "=" Num (<L> "," Num)+ ";"
        let CtxLPlus::V1 { id, num } = ctx;
    }

    fn exit_l_plus_i(&mut self, ctx: CtxLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
        let CtxLPlusI::V1 { num } = ctx;
    }

    fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // rrec -> Id "=" Num rrec_i
        let CtxRrec::V1 { id, num } = ctx;
    }

    fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // lrec -> Id "=" lrec_i ";"
        let CtxLrec::V1 { id } = ctx;
    }

    fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_amb({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // amb -> Id "=" amb_i ";"
        let CtxAmb::V1 { id } = ctx;
    }

    fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // star_a -> Id "=" "[" (Id | Num ":" Id)* "]" ";"
        let CtxStarA::V1 { id, star } = ctx;
    }

    fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // plus_a -> Id "=" "[" (Id | Num ":" Id)+ "]" ";"
        let CtxPlusA::V1 { id, plus } = ctx;
    }

    fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_star_a -> Id "=" "[" (<L> Id | Num ":" Id)* "]" ";"
        let CtxLStarA::V1 { id } = ctx;
    }

    fn exit_l_star_a_i(&mut self, ctx: CtxLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
            CtxLStarAI::V1 { id } => {}
            // `Num ":" Id` iteration in `l_star_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
            CtxLStarAI::V2 { num, id } => {}
        }
    }

    fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_plus_a -> Id "=" "[" (<L> Id | Num ":" Id)+ "]" ";"
        let CtxLPlusA::V1 { id } = ctx;
    }

    fn exit_l_plus_a_i(&mut self, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
            CtxLPlusAI::V1 { id } => {}
            // `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
            CtxLPlusAI::V2 { num, id } => {}
        }
    }

    fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // sep_list -> Id "=" (Id ":" Num / "," "then")+ ";"
        let CtxSepList::V1 { id, plus } = ctx;
    }

    fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // sep_list_opt -> Id "=" (Id ":" Num / "," "then")+ ";"
            CtxSepListOpt::V1 { id, plus } => {}
            // sep_list_opt -> Id "=" ";"
            CtxSepListOpt::V2 { id } => {}
        }
    }

    fn exit_l_sep_list(&mut self, ctx: CtxLSepList, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_sep_list -> Id "=" (<L> Id ":" Num / "," "then")+ ";"
        let CtxLSepList::V1 { id } = ctx;
    }

    fn exit_l_sep_list_i(&mut self, ctx: CtxLSepListI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_sep_list_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
        let CtxLSepListI::V1 { id, num } = ctx;
    }

    fn exit_l_sep_list_opt(&mut self, ctx: CtxLSepListOpt, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // l_sep_list_opt -> Id "=" (<L> Id ":" Num / "," "then")+ ";"
            CtxLSepListOpt::V1 { id } => {}
            // l_sep_list_opt -> Id "=" ";"
            CtxLSepListOpt::V2 { id } => {}
        }
    }

    fn exit_l_sep_list_opt_i(&mut self, ctx: CtxLSepListOptI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_sep_list_opt_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> Id ":" Num / "," "then"` iteration in `l_sep_list_opt -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        let CtxLSepListOptI::V1 { id, num } = ctx;
    }

    fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // rrec_i -> "," Num rrec_i
            CtxRrecI::V1 { num } => {}
            // rrec_i -> ";"
            CtxRrecI::V2 => {}
        }
    }

    fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // lrec_i -> lrec_i "," Num
            CtxLrecI::V1 { num } => {}
            // lrec_i -> Num
            CtxLrecI::V2 { num } => {}
        }
    }

    fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_amb_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // amb_i -> "-" amb_i
            CtxAmbI::V1 => {}
            // amb_i -> <R> amb_i "^" amb_i
            CtxAmbI::V2 => {}
            // amb_i -> amb_i "*" amb_i
            CtxAmbI::V3 => {}
            // amb_i -> amb_i <P> "/" amb_i
            CtxAmbI::V4 => {}
            // amb_i -> amb_i "+" amb_i
            CtxAmbI::V5 => {}
            // amb_i -> amb_i <P> "-" amb_i
            CtxAmbI::V6 => {}
            // amb_i -> "(" amb_i ")"
            CtxAmbI::V7 => {}
            // amb_i -> Id
            CtxAmbI::V8 { id } => {}
            // amb_i -> Num
            CtxAmbI::V9 { num } => {}
        }
    }

    fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
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
            // nv_example -> "lrec" nv_lrec
            CtxNvExample::V6 => {}
            // nv_example -> "star-a" nv_star_a
            CtxNvExample::V7 => {}
            // nv_example -> "plus-a" nv_plus_a
            CtxNvExample::V8 => {}
            // nv_example -> "l-star-a" nv_l_star_a
            CtxNvExample::V9 => {}
            // nv_example -> "l-plus-a" nv_l_plus_a
            CtxNvExample::V10 => {}
            // nv_example -> "sep-list" nv_sep_list
            CtxNvExample::V11 => {}
            // nv_example -> "sep-list-opt" nv_sep_list_opt
            CtxNvExample::V12 => {}
            // nv_example -> "l-sep-list" nv_l_sep_list
            CtxNvExample::V13 => {}
            // nv_example -> "l-sep-list-opt" nv_l_sep_list_opt
            CtxNvExample::V14 => {}
        }
    }

    fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_star -> Id "=" "+" ("," "*")* ";"
        let CtxNvStar::V1 { id } = ctx;
    }

    fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_plus -> Id "=" "+" ("," "*")+ ";"
        let CtxNvPlus::V1 { id } = ctx;
    }

    fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_star -> Id "=" "+" (<L> "," "*")* ";"
        let CtxNvLStar::V1 { id } = ctx;
    }

    fn exit_nv_l_star_i(&mut self, ctx: CtxNvLStarI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "," "*"` iteration in `nv_l_star -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )* ";"`
        let CtxNvLStarI::V1 = ctx;
    }

    fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_plus -> Id "=" "+" (<L> "," "*")+ ";"
        let CtxNvLPlus::V1 { id } = ctx;
    }

    fn exit_nv_l_plus_i(&mut self, ctx: CtxNvLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "," "*"` iteration in `nv_l_plus -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )+ ";"`
        let CtxNvLPlusI::V1 = ctx;
    }

    fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_rrec -> Id "=" "+" nv_rrec_i
        let CtxNvRrec::V1 { id } = ctx;
    }

    fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_lrec -> Id "=" nv_lrec_i ";"
        let CtxNvLrec::V1 { id } = ctx;
    }

    fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_star_a -> Id "=" "[" ("+" | "*" "-")* "]" ";"
        let CtxNvStarA::V1 { id } = ctx;
    }

    fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_plus_a -> Id "=" "[" ("+" | "*" "-")+ "]" ";"
        let CtxNvPlusA::V1 { id } = ctx;
    }

    fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_star_a -> Id "=" "[" (<L> "+" | "*" "-")* "]" ";"
        let CtxNvLStarA::V1 { id } = ctx;
    }

    fn exit_nv_l_star_a_i(&mut self, ctx: CtxNvLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // `<L> "+"` iteration in `nv_l_star_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" "-")* "]" ";"`
            CtxNvLStarAI::V1 => {}
            // `"*" "-"` iteration in `nv_l_star_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )* "]" ";"`
            CtxNvLStarAI::V2 => {}
        }
    }

    fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_plus_a -> Id "=" "[" (<L> "+" | "*" "-")+ "]" ";"
        let CtxNvLPlusA::V1 { id } = ctx;
    }

    fn exit_nv_l_plus_a_i(&mut self, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // `<L> "+"` iteration in `nv_l_plus_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" "-")+ "]" ";"`
            CtxNvLPlusAI::V1 => {}
            // `"*" "-"` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )+ "]" ";"`
            CtxNvLPlusAI::V2 => {}
        }
    }

    fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_sep_list -> Id "=" ("*" / "," "then")+ ";"
        let CtxNvSepList::V1 { id } = ctx;
    }

    fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // nv_sep_list_opt -> Id "=" ("*" / "," "then")+ ";"
            CtxNvSepListOpt::V1 { id } => {}
            // nv_sep_list_opt -> Id "=" ";"
            CtxNvSepListOpt::V2 { id } => {}
        }
    }

    fn exit_nv_l_sep_list(&mut self, ctx: CtxNvLSepList, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_sep_list -> Id "=" (<L> "*" / "," "then")+ ";"
        let CtxNvLSepList::V1 { id } = ctx;
    }

    fn exit_nv_l_sep_list_i(&mut self, ctx: CtxNvLSepListI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_sep_list_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "*" / "," "then"` iteration in `nv_l_sep_list -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";"`
        let CtxNvLSepListI::V1 = ctx;
    }

    fn exit_nv_l_sep_list_opt(&mut self, ctx: CtxNvLSepListOpt, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // nv_l_sep_list_opt -> Id "=" (<L> "*" / "," "then")+ ";"
            CtxNvLSepListOpt::V1 { id } => {}
            // nv_l_sep_list_opt -> Id "=" ";"
            CtxNvLSepListOpt::V2 { id } => {}
        }
    }

    fn exit_nv_l_sep_list_opt_i(&mut self, ctx: CtxNvLSepListOptI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_sep_list_opt_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "*" / "," "then"` iteration in `nv_l_sep_list_opt -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        let CtxNvLSepListOptI::V1 = ctx;
    }

    fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // nv_rrec_i -> "," "*" nv_rrec_i
            CtxNvRrecI::V1 => {}
            // nv_rrec_i -> ";"
            CtxNvRrecI::V2 => {}
        }
    }

    fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
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
    const FIRST_END_STATE: LexStateId = 30;
    const NBR_STATES: LexStateId = 83;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         28,  28,  28,  28,  28,  28,  28,  28,  28,   0,  31,  28,  28,  31,  28,  28,   // 0-15
         28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,   // 16-31
          0,  28,  28,  28,  28,  28,  28,  28,   1,   2,   3,   4,   5,   6,  28,   7,   // 32-47
         22,   8,   8,   8,   8,   8,   8,   8,   8,   8,   9,  10,  28,  11,  28,  28,   // 48-63
         28,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,   // 64-79
         26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,  13,  28,  14,  15,  27,   // 80-95
         28,  16,  34,  33,  26,  12,  26,  26,  30,  24,  26,  26,  17,  29,  32,  25,   // 96-111
         18,  26,  19,  20,  21,  23,  26,  26,  26,  26,  26,  28,  28,  28,  28,  28,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 28),
        (Seg(57344, 1114111), 28),
    ];
    static TERMINAL_TABLE: [Terminal;53] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [LexStateId; 2906] = [
         30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  83,  42,  42,  42,  42,  83,  83,  42,  42,  30,  42,  42,  42, // state 0
          1,   1,   1,  22,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  73,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 2
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  74,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 3
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,   5,  83,   6,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 4
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  11,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 5
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,   7,  83,  83,  83,  83,  83,  83,  83,  83,   8,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 6
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  19,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 7
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,   9,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 8
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  64,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 9
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  75,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 10
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  12,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 11
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  65,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 12
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  76,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 13
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  15,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 14
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  16,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 15
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  78,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 16
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  24,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 17
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  79,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 18
         83,  83,  83,  83,  83,  83,  29,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 19
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  80,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 20
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  81,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 21
          1,   1,   1,  22,   1,   1,   1,  82,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 22
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  14,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 23
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  18,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 24
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  21,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 25
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  20,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 26
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  26,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 27
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  25,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 28
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  27,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 29
         30,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  30,  83,  83,  83, // state 30 <skip>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 31 <end:4>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 32 <end:7>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 33 <end:6>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 34 <end:0>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 35 <end:11>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 36 <end:9>
         83,  83,  83,   1,  83,  83,  83,  52,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 37 <end:1>
         83,  83,  83,  83,  83,  83,  83,  83,  38,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  38,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 38 <end:30>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 39 <end:10>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 40 <end:12>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 41 <end:2>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 42 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 43 <end:5>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 44 <end:8>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 45 <end:3>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  71,  42,  83,  42,  42,  42, // state 46 <end:29>
         83,  83,  83,  83,  83,  83,   4,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  63,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 47 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  60,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 48 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  66,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 49 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  56,  83,  83,  83,  42,  42,  42,  42,  42,  57,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 50 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  53,  83,  42,  42,  42, // state 51 <end:29>
         52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  83,  52,  52,  52, // state 52 <skip>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  54,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 53 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  55,  42,  42, // state 54 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 55 <end:13>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  77,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 56 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  58,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 57 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  59,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 58 <end:29>
         83,  83,  83,  83,  83,  83,   2,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 59 <end:14>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  61,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 60 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  62,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 61 <end:29>
         83,  83,  83,  83,  83,  83,   3,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 62 <end:15>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  69,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 63 <end:29>
         83,  83,  83,  83,  83,  83,  10,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 64 <end:16>
         83,  83,  83,  83,  83,  83,  13,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 65 <end:17>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  67,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 66 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  68,  42, // state 67 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 68 <end:18>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  70,  42, // state 69 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 70 <end:19>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  72, // state 71 <end:29>
         83,  83,  83,  83,  83,  83,  83,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 72 <end:20>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 73 <end:21>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 74 <end:22>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 75 <end:23>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 76 <end:24>
         83,  83,  83,  83,  83,  83,  23,  83,  42,  83,  83,  83,  42,  83,  83,  83,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  42,  83,  42,  42,  83,  42,  42,  42, // state 77 <end:29>
         83,  83,  83,  83,  83,  83,  17,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 78 <end:25>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 79 <end:26>
         83,  83,  83,  83,  83,  83,  28,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 80 <end:27>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 81 <end:28>
         83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83, // state 82 <skip>
         83 // error group in [nbr_state * nbr_group + nbr_group]
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

    use lexigram_core::{AltId, LALR, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, ListenerWrapper, Terminate, lr_parser::{LRAction::{self, Accept as LRA, Error as LRE, Reduce as LRR, Shift as LRS}, LRParser, LRStateId}}};

    static NUM_NT: usize = 63;
    static NUM_T_FULL: usize = 32;
    static ACTION: [LRAction; 11328] = [
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(2),LRE,LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRA,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(3),LRE,LRS(4),LRS(5),LRS(6),LRS(7),LRS(8),LRS(9),LRS(10),LRS(11),LRS(12),LRS(13),LRS(14),LRS(15),LRS(16),LRS(17),LRS(18),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRE,LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRE,LRE,LRR(4),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(21),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(23),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(25),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(27),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(29),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(31),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(33),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(35),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(37),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(39),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(41),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(43),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(47),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(49),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(1),LRE,LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),
        LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(51),LRS(52),LRS(53),LRS(54),LRS(55),LRS(56),LRE,LRS(57),LRS(58),LRS(59),LRS(60),
        LRS(61),LRS(62),LRS(63),LRS(64),LRE,LRE,LRR(0),LRE,LRE,LRS(66),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(5),LRE,LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRE,LRE,
        LRE,LRE,LRE,LRS(67),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(6),LRE,LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRE,LRE,LRE,LRE,LRE,LRS(68),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(7),LRE,LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRE,LRE,LRE,LRE,LRE,LRS(69),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(8),LRE,
        LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRE,LRE,LRE,LRE,LRE,LRS(70),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(9),LRE,LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),
        LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRE,LRE,LRE,LRE,LRE,LRS(71),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(10),LRE,LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),
        LRR(10),LRR(10),LRR(10),LRE,LRE,LRE,LRE,LRE,LRS(72),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(11),LRE,LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRE,LRE,LRE,
        LRE,LRE,LRS(73),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(12),LRE,LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRE,LRE,LRE,LRE,LRE,LRS(74),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(13),LRE,LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRE,LRE,LRE,LRE,LRE,LRS(75),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(14),LRE,LRR(14),
        LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRE,LRE,LRE,LRE,LRE,LRS(76),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(15),LRE,LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),
        LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRE,LRE,LRE,LRE,LRE,LRS(77),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(16),LRE,LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),
        LRR(16),LRR(16),LRE,LRE,LRE,LRE,LRE,LRS(78),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(17),LRE,LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRE,LRE,LRE,LRE,
        LRE,LRS(79),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(18),LRE,LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRE,LRE,LRE,LRE,LRE,LRS(80),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(19),LRE,LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(81),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(83),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(85),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(87),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(89),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(91),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(93),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(95),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(97),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(99),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(101),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(103),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(105),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(107),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),
        LRE,LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRE,LRE,LRR(3),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(109),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(110),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(111),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(112),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(113),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(114),LRE,LRE,LRE,LRE,LRE,LRS(116),LRE,
        LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRE,LRE,LRE,LRE,LRE,LRS(121),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(122),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(123),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(124),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(125),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(127),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(128),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(130),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(132),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(133),
        LRE,LRE,LRE,LRE,LRS(135),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRE,LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRE,LRE,LRR(65),LRE,LRE,LRS(136),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRE,LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRE,LRE,LRR(66),LRE,LRE,LRS(137),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRE,LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRE,LRE,LRR(67),LRE,LRE,LRS(138),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),
        LRR(68),LRE,LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRE,LRE,LRR(68),LRE,LRE,LRS(139),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(69),LRR(69),LRR(69),LRR(69),LRR(69),LRR(69),LRE,LRR(69),LRR(69),LRR(69),LRR(69),
        LRR(69),LRR(69),LRR(69),LRR(69),LRE,LRE,LRR(69),LRE,LRE,LRS(140),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRE,LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRE,LRE,
        LRR(70),LRE,LRE,LRS(141),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRE,LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRE,LRE,LRR(71),LRE,LRE,LRS(142),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRE,LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRE,LRE,LRR(72),LRE,LRE,LRS(143),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRE,LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRE,LRE,LRR(73),LRE,LRE,LRS(144),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),
        LRE,LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRE,LRE,LRR(74),LRE,LRE,LRS(145),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),LRE,LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),
        LRR(75),LRR(75),LRR(75),LRE,LRE,LRR(75),LRE,LRE,LRS(146),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRE,LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRE,LRE,LRR(76),
        LRE,LRE,LRS(147),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRE,LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRE,LRE,LRR(77),LRE,LRE,LRS(148),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRE,LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRE,LRE,LRR(78),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(115),
        LRR(115),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(150),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(24),LRR(24),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(153),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(155),LRS(156),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(55),LRR(55),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(158),LRS(159),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(116),LRE,LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRE,
        LRE,LRE,LRE,LRS(116),LRE,LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRR(63),LRR(63),LRE,LRR(63),
        LRE,LRE,LRR(63),LRR(63),LRE,LRR(63),LRE,LRE,LRR(63),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(64),LRR(64),LRE,LRR(64),LRE,LRE,LRR(64),
        LRR(64),LRE,LRR(64),LRE,LRE,LRR(64),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(162),LRS(163),LRE,LRS(164),LRE,LRE,LRS(165),LRE,LRE,LRS(166),
        LRE,LRE,LRS(167),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(120),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(120),LRR(120),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(169),LRS(170),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(36),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(36),LRR(36),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(173),LRS(174),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(176),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(177),LRS(178),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(44),LRE,LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(179),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(180),LRS(181),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(182),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(183),LRS(184),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRR(49),LRE,LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(185),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(186),LRS(187),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(188),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(189),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(190),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(191),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(192),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(193),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(195),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(196),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(197),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(198),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(199),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(201),LRE,LRE,LRE,LRE,LRE,LRS(202),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(204),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(206),LRE,LRE,LRE,LRE,LRE,LRS(207),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(209),LRS(210),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(211),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(212),LRS(213),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(214),LRS(215),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(216),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(217),LRS(218),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(219),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(53),
        LRE,LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(28),LRE,LRR(28),LRR(28),
        LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(220),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(29),LRE,LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),
        LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRE,LRE,LRE,LRS(162),LRS(163),LRE,LRS(164),LRE,LRE,LRS(165),LRS(221),LRE,LRS(166),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(56),LRR(56),LRE,LRR(56),LRE,LRE,LRR(56),LRR(56),LRE,LRR(56),LRE,LRE,LRR(56),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(116),LRE,LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),
        LRE,LRE,LRE,LRE,LRE,LRS(116),LRE,LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRE,LRE,
        LRE,LRE,LRS(116),LRE,LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRE,LRE,LRE,LRE,LRS(116),
        LRE,LRE,LRE,LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRE,LRE,LRE,LRE,LRS(116),LRE,LRE,LRE,
        LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(118),LRS(119),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRR(30),LRE,LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(227),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(228),LRS(229),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(122),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(122),LRR(122),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(230),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(231),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(232),LRS(233),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(234),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(235),LRS(236),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(39),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(39),LRR(39),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(237),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(238),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(239),LRS(240),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(241),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(242),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(42),LRE,LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(243),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(244),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(43),LRE,LRR(43),LRR(43),LRR(43),LRR(43),
        LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(245),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(246),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(45),LRE,LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),
        LRR(45),LRR(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(247),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(248),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(48),LRE,LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(130),LRR(130),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(250),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRR(83),LRR(83),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(253),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(255),LRS(256),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(113),LRR(113),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(258),LRS(259),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(135),LRE,LRE,LRE,LRE,LRE,LRR(135),LRE,LRR(135),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(261),LRE,LRE,LRE,LRE,LRE,LRS(262),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(94),LRE,LRE,LRE,LRE,LRE,LRR(94),LRE,LRR(94),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(265),LRE,LRE,LRE,LRE,LRE,LRS(266),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(141),LRR(141),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(268),LRS(269),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(143),LRR(143),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRE,LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRE,LRE,LRR(102),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(270),LRS(271),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(105),LRR(105),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(272),LRS(273),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(109),LRR(109),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRE,LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),
        LRR(107),LRR(107),LRR(107),LRE,LRE,LRR(107),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(274),LRS(275),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(276),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(20),LRE,LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(117),LRR(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(277),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(21),LRE,LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(278),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(22),LRE,LRR(22),
        LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(27),LRR(27),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(279),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(25),LRE,LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),
        LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(155),LRS(156),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(54),LRR(54),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(62),LRR(62),LRE,LRR(62),LRE,LRE,LRR(62),LRR(62),LRE,LRR(62),LRE,LRE,LRR(62),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(60),
        LRS(163),LRE,LRS(164),LRE,LRE,LRS(165),LRR(60),LRE,LRR(60),LRE,LRE,LRR(60),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(59),LRR(59),LRE,LRS(164),
        LRE,LRE,LRR(59),LRR(59),LRE,LRR(59),LRE,LRE,LRR(59),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(57),LRR(57),LRE,LRS(164),LRE,LRE,LRR(57),
        LRR(57),LRE,LRR(57),LRE,LRE,LRR(57),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(58),LRR(58),LRE,LRS(164),LRE,LRE,LRR(58),LRR(58),LRE,LRR(58),
        LRE,LRE,LRR(58),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(61),LRS(163),LRE,LRS(164),LRE,LRE,LRS(165),LRR(61),LRE,LRR(61),LRE,LRE,LRR(61),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(281),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(118),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(118),LRR(118),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(282),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(283),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(284),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(121),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(121),LRR(121),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(285),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(286),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(34),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(34),LRR(34),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(287),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(288),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(289),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(38),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(38),LRR(38),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(290),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(126),LRR(126),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(291),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(128),LRR(128),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(292),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(47),LRR(47),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(293),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(51),LRR(51),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(294),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(295),
        LRS(296),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(297),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(298),LRS(299),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(300),LRS(301),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(302),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(303),LRS(304),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(305),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRE,LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRE,LRE,LRR(111),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRE,LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRE,LRE,LRR(87),LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(306),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRE,LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRE,LRE,LRR(88),LRS(307),LRE,LRE,LRE,LRE,LRE,LRS(308),LRE,LRS(309),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(137),LRE,LRE,LRE,LRE,LRE,LRR(137),LRE,LRR(137),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(310),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(311),LRE,LRE,LRE,LRE,LRE,LRS(312),LRE,LRS(313),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(314),LRE,LRE,LRE,LRE,LRE,LRS(315),LRE,LRS(316),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(97),LRE,LRE,LRE,LRE,LRE,LRR(97),LRE,LRR(97),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(317),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(318),LRE,LRE,LRE,LRE,LRE,LRS(319),LRE,LRS(320),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(321),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRE,LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRE,LRE,LRR(100),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(322),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRE,LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRE,LRE,LRR(101),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(323),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRE,LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRE,LRE,LRR(103),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(324),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),
        LRE,LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRE,LRE,LRR(106),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(114),LRR(114),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(116),LRR(116),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(23),LRR(23),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(26),LRR(26),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(52),LRE,LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(31),LRE,LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(325),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(124),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(124),LRR(124),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(32),LRE,LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(326),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(33),LRE,LRR(33),LRR(33),LRR(33),LRR(33),
        LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(327),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(41),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(41),LRR(41),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(37),LRE,LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),
        LRR(37),LRR(37),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(328),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(329),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(330),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(331),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(332),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(333),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRE,LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRE,LRE,LRR(79),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(132),LRR(132),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(334),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRE,LRR(80),
        LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRE,LRE,LRR(80),LRE,LRE,LRE,LRE,LRE,LRE,LRS(335),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRE,LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),
        LRR(81),LRE,LRE,LRR(81),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(86),LRR(86),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(336),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRE,LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRE,LRE,LRR(84),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(255),LRS(256),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(112),LRR(112),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(133),LRE,LRE,LRE,LRE,LRE,LRR(133),LRE,LRR(133),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(338),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(339),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(139),LRE,LRE,LRE,LRE,LRE,LRR(139),LRE,LRR(139),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(136),LRE,LRE,LRE,LRE,LRE,LRR(136),LRE,LRR(136),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(340),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(341),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(92),LRE,LRE,LRE,LRE,LRE,LRR(92),LRE,LRR(92),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(342),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(343),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(99),LRE,LRE,LRE,LRE,LRE,
        LRR(99),LRE,LRR(99),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(96),LRE,LRE,LRE,LRE,LRE,LRR(96),LRE,LRR(96),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(344),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(345),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(346),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(347),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(348),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(349),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(119),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(119),
        LRR(119),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(123),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(123),LRR(123),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(35),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(35),LRR(35),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(40),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(40),LRR(40),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(350),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(351),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(352),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(353),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(129),LRR(129),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(131),LRR(131),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(82),LRR(82),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(85),LRR(85),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(110),LRR(110),LRR(110),LRR(110),LRR(110),LRR(110),LRE,LRR(110),LRR(110),LRR(110),LRR(110),LRR(110),LRR(110),LRR(110),LRR(110),LRE,LRE,
        LRR(110),LRR(134),LRE,LRE,LRE,LRE,LRE,LRR(134),LRE,LRR(134),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(89),LRR(89),LRR(89),LRR(89),LRR(89),LRR(89),LRE,LRR(89),LRR(89),LRR(89),LRR(89),LRR(89),LRR(89),LRR(89),LRR(89),LRE,LRE,LRR(89),LRR(138),LRE,LRE,LRE,LRE,
        LRE,LRR(138),LRE,LRR(138),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(90),LRR(90),LRR(90),LRR(90),LRR(90),LRR(90),LRE,LRR(90),LRR(90),LRR(90),LRR(90),LRR(90),LRR(90),LRR(90),LRR(90),LRE,LRE,LRR(90),LRR(93),LRE,LRE,LRE,LRE,LRE,LRR(93),LRE,LRR(93),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(91),LRR(91),LRR(91),LRR(91),LRR(91),LRR(91),LRE,LRR(91),LRR(91),LRR(91),LRR(91),LRR(91),LRR(91),LRR(91),LRR(91),LRE,LRE,LRR(91),LRR(98),LRE,LRE,LRE,LRE,LRE,LRR(98),LRE,LRR(98),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(95),LRR(95),LRR(95),LRR(95),LRR(95),LRR(95),
        LRE,LRR(95),LRR(95),LRR(95),LRR(95),LRR(95),LRR(95),LRR(95),LRR(95),LRE,LRE,LRR(95),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(140),LRR(140),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(142),LRR(142),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(104),LRR(104),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(108),LRR(108),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(125),LRR(125),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(127),LRR(127),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(46),LRR(46),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(50),LRR(50),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE];
    static GOTO: [LRStateId; 22302] = [
        1,2,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,19,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,20,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,22,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        24,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,26,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,28,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,30,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,32,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,34,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,36,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,38,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,40,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,42,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,44,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,46,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,48,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,50,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,65,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,82,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,84,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,86,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,88,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,90,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,92,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,94,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,96,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,98,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,100,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,102,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,104,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,106,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,108,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,115,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,120,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,126,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,129,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,131,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,134,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,149,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,151,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        152,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,154,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,157,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,160,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,161,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,168,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,171,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,172,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,175,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,194,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,200,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,203,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,205,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,208,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,222,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,223,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,224,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,225,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,226,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,249,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,251,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,252,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,254,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,257,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,260,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,263,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,264,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,267,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,280,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,337,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,
        354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354,354];
    static ALT_NT_LEN: [(VarId, u16, u16); 145] = [
        (0, 3, 0),(1, 2, 0),(1, 0, 0),(2, 2, 0),(2, 0, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(4, 5, 2),(5, 5, 2),(6, 5, 2),(7, 3, 1),(7, 0, 0),
        (8, 5, 2),(9, 3, 1),(9, 2, 1),(10, 4, 2),(11, 4, 1),(12, 4, 1),(13, 6, 1),(14, 6, 1),(15, 6, 1),(16, 2, 1),(16, 4, 2),(16, 0, 0),(17, 6, 1),(18, 2, 1),(18, 1, 1),(18, 4, 2),(18, 3, 2),(19, 4, 1),(20, 4, 1),(20, 3, 1),(21, 4, 1),(22, 6, 2),(22, 3, 2),(23, 4, 1),(23, 3, 1),
        (24, 6, 2),(24, 3, 2),(25, 3, 1),(25, 1, 0),(26, 3, 1),(26, 1, 1),(27, 2, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 1, 1),(27, 1, 1),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),
        (28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(29, 5, 1),(30, 5, 1),(31, 5, 1),(32, 3, 0),(32, 0, 0),(33, 5, 1),(34, 3, 0),(34, 2, 0),(35, 4, 1),(36, 4, 1),(37, 6, 1),(38, 6, 1),(39, 6, 1),(40, 2, 0),(40, 3, 0),(40, 0, 0),(41, 6, 1),(42, 2, 0),(42, 1, 0),(42, 3, 0),(42, 2, 0),
        (43, 4, 1),(44, 4, 1),(44, 3, 1),(45, 4, 1),(46, 4, 0),(46, 1, 0),(47, 4, 1),(47, 3, 1),(48, 4, 0),(48, 1, 0),(49, 3, 0),(49, 1, 0),(50, 3, 0),(50, 1, 0),(51, 3, 1),(51, 0, 0),(52, 3, 1),(52, 2, 1),(53, 2, 1),(53, 4, 2),(53, 0, 0),(54, 2, 1),(54, 1, 1),(54, 4, 2),(54, 3, 2),
        (55, 6, 2),(55, 3, 2),(56, 6, 2),(56, 3, 2),(57, 3, 0),(57, 0, 0),(58, 3, 0),(58, 2, 0),(59, 2, 0),(59, 3, 0),(59, 0, 0),(60, 2, 0),(60, 1, 0),(60, 3, 0),(60, 2, 0),(61, 4, 0),(61, 1, 0),(62, 4, 0),(62, 1, 0),(63, 1, 0)];
    static SYMBOL_TABLE_T: [(&str, Option<&str>); 31] = [
        ("Add", Some("+")),("Div", Some("/")),("Equal", Some("=")),("Exp", Some("^")),("Lpar", Some("(")),("Lsbracket", Some("[")),("Mul", Some("*")),("Rpar", Some(")")),("Rsbracket", Some("]")),("Sub", Some("-")),
        ("Colon", Some(":")),("Comma", Some(",")),("Semi", Some(";")),("Then", Some("then")),("Star", Some("star")),("Plus", Some("plus")),("L_Star", Some("l-star")),("L_Plus", Some("l-plus")),("Rrec", Some("rrec")),("Lrec", Some("lrec")),
        ("Amb", Some("amb")),("Star_A", Some("star-a")),("Plus_A", Some("plus-a")),("L_Star_A", Some("l-star-a")),("L_Plus_A", Some("l-plus-a")),("SepList", Some("sep-list")),("SepList_Opt", Some("sep-list-opt")),("L_SepList", Some("l-sep-list")),("L_SepList_Opt", Some("l-sep-list-opt")),("Id", None),
        ("Num", None)];
    static SYMBOL_TABLE_NT: [&str; 64] = [
        "text","i","nv_i","example","star","plus","l_star","l_star_i","l_plus","l_plus_i","rrec","lrec","amb","star_a","plus_a","l_star_a","l_star_a_i","l_plus_a","l_plus_a_i","sep_list",
        "sep_list_opt","l_sep_list","l_sep_list_i","l_sep_list_opt","l_sep_list_opt_i","rrec_i","lrec_i","amb_i","nv_example","nv_star","nv_plus","nv_l_star","nv_l_star_i","nv_l_plus","nv_l_plus_i","nv_rrec","nv_lrec","nv_star_a","nv_plus_a","nv_l_star_a",
        "nv_l_star_a_i","nv_l_plus_a","nv_l_plus_a_i","nv_sep_list","nv_sep_list_opt","nv_l_sep_list","nv_l_sep_list_i","nv_l_sep_list_opt","nv_l_sep_list_opt_i","nv_rrec_i","nv_lrec_i","star_1","plus_1","star_a_1","plus_a_1","sep_list_1","sep_list_opt_1","nv_star_1","nv_plus_1","nv_star_a_1",
        "nv_plus_a_1","nv_sep_list_1","nv_sep_list_opt_1","<goal>"];

    pub fn build_parser() -> LRParser<'static, LALR> {
        LRParser::new(
            NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,
            FixedSymTable::new(
                SYMBOL_TABLE_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),
                SYMBOL_TABLE_NT.into_iter().map(|s| s.to_string()).collect()
            )
        )
    }

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
        /// `example -> "lrec" lrec`
        V6,
        /// `example -> "amb" amb`
        V7,
        /// `example -> "star-a" star_a`
        V8,
        /// `example -> "plus-a" plus_a`
        V9,
        /// `example -> "l-star-a" l_star_a`
        V10,
        /// `example -> "l-plus-a" l_plus_a`
        V11,
        /// `example -> "sep-list" sep_list`
        V12,
        /// `example -> "sep-list-opt" sep_list_opt`
        V13,
        /// `example -> "l-sep-list" l_sep_list`
        V14,
        /// `example -> "l-sep-list-opt" l_sep_list_opt`
        V15,
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
        V1 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxRrec {
        /// `rrec -> Id "=" Num rrec_i`
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
        V1 { id: String },
        /// `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
        V2 { num: String, id: String },
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
    pub enum CtxLSepList {
        /// `l_sep_list -> Id "=" (<L> Id ":" Num / "," "then")+ ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxLSepListI {
        /// `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxLSepListOpt {
        /// `l_sep_list_opt -> Id "=" (<L> Id ":" Num / "," "then")+ ";"`
        V1 { id: String },
        /// `l_sep_list_opt -> Id "=" ";"`
        V2 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxLSepListOptI {
        /// `<L> Id ":" Num / "," "then"` iteration in `l_sep_list_opt -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxRrecI {
        /// `rrec_i -> "," Num rrec_i`
        V1 { num: String },
        /// `rrec_i -> ";"`
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
        /// `nv_example -> "lrec" nv_lrec`
        V6,
        /// `nv_example -> "star-a" nv_star_a`
        V7,
        /// `nv_example -> "plus-a" nv_plus_a`
        V8,
        /// `nv_example -> "l-star-a" nv_l_star_a`
        V9,
        /// `nv_example -> "l-plus-a" nv_l_plus_a`
        V10,
        /// `nv_example -> "sep-list" nv_sep_list`
        V11,
        /// `nv_example -> "sep-list-opt" nv_sep_list_opt`
        V12,
        /// `nv_example -> "l-sep-list" nv_l_sep_list`
        V13,
        /// `nv_example -> "l-sep-list-opt" nv_l_sep_list_opt`
        V14,
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
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvRrec {
        /// `nv_rrec -> Id "=" "+" nv_rrec_i`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLrec {
        /// `nv_lrec -> Id "=" nv_lrec_i ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvStarA {
        /// `nv_star_a -> Id "=" "[" ("+" | "*" "-")* "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvPlusA {
        /// `nv_plus_a -> Id "=" "[" ("+" | "*" "-")+ "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLStarA {
        /// `nv_l_star_a -> Id "=" "[" (<L> "+" | "*" "-")* "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLStarAI {
        /// `<L> "+"` iteration in `nv_l_star_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" "-")* "]" ";"`
        V1,
        /// `"*" "-"` iteration in `nv_l_star_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )* "]" ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxNvLPlusA {
        /// `nv_l_plus_a -> Id "=" "[" (<L> "+" | "*" "-")+ "]" ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLPlusAI {
        /// `<L> "+"` iteration in `nv_l_plus_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" "-")+ "]" ";"`
        V1,
        /// `"*" "-"` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )+ "]" ";"`
        V2,
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
    pub enum CtxNvLSepList {
        /// `nv_l_sep_list -> Id "=" (<L> "*" / "," "then")+ ";"`
        V1 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLSepListI {
        /// `<L> "*" / "," "then"` iteration in `nv_l_sep_list -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvLSepListOpt {
        /// `nv_l_sep_list_opt -> Id "=" (<L> "*" / "," "then")+ ";"`
        V1 { id: String },
        /// `nv_l_sep_list_opt -> Id "=" ";"`
        V2 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNvLSepListOptI {
        /// `<L> "*" / "," "then"` iteration in `nv_l_sep_list_opt -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvRrecI {
        /// `nv_rrec_i -> "," "*" nv_rrec_i`
        V1,
        /// `nv_rrec_i -> ";"`
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
    /// Top non-terminal Text (has no value)
    #[derive(Debug, PartialEq)]
    pub struct SynText();

    #[derive(Debug)]
    enum EnumSynValue { Star1(SynStar1), Plus1(SynPlus1), StarA1(SynStarA1), PlusA1(SynPlusA1), SepList1(SynSepList1), SepListOpt1(SynSepListOpt1) }

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
        #[allow(unused_variables)]
        fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) {}
        fn init_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_i(&mut self, ctx: CtxI, spans: Vec<PosSpan>) {}
        fn init_nv_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_i(&mut self, ctx: CtxNvI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) {}
        fn init_l_star_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_star_i(&mut self, ctx: CtxLStarI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) {}
        fn init_l_plus_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_plus_i(&mut self, ctx: CtxLPlusI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) {}
        fn init_l_star_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_star_a_i(&mut self, ctx: CtxLStarAI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) {}
        fn init_l_plus_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_plus_a_i(&mut self, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_l_sep_list(&mut self, ctx: CtxLSepList, spans: Vec<PosSpan>) {}
        fn init_l_sep_list_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_sep_list_i(&mut self, ctx: CtxLSepListI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_l_sep_list_opt(&mut self, ctx: CtxLSepListOpt, spans: Vec<PosSpan>) {}
        fn init_l_sep_list_opt_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_l_sep_list_opt_i(&mut self, ctx: CtxLSepListOptI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) {}
        fn init_nv_l_star_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star_i(&mut self, ctx: CtxNvLStarI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) {}
        fn init_nv_l_plus_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus_i(&mut self, ctx: CtxNvLPlusI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) {}
        fn init_nv_l_star_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_star_a_i(&mut self, ctx: CtxNvLStarAI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) {}
        fn init_nv_l_plus_a_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_plus_a_i(&mut self, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_l_sep_list(&mut self, ctx: CtxNvLSepList, spans: Vec<PosSpan>) {}
        fn init_nv_l_sep_list_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_sep_list_i(&mut self, ctx: CtxNvLSepListI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_l_sep_list_opt(&mut self, ctx: CtxNvLSepListOpt, spans: Vec<PosSpan>) {}
        fn init_nv_l_sep_list_opt_i(&mut self) {}
        #[allow(unused_variables)]
        fn exit_nv_l_sep_list_opt_i(&mut self, ctx: CtxNvLSepListOptI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) {}
        #[allow(unused_variables)]
        fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) {}
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
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_text(),                      // text -> i ";" nv_i
                        1 => self.exit_i(),                         // i -> <L> i example
                        2 => self.init_i(),                         // i -> <L> ε
                        3 => self.exit_nv_i(),                      // nv_i -> <L> nv_i nv_example
                        4 => self.init_nv_i(),                      // nv_i -> <L> ε
                        5 |                                         // example -> "star" star
                        6 |                                         // example -> "plus" plus
                        7 |                                         // example -> "l-star" l_star
                        8 |                                         // example -> "l-plus" l_plus
                        9 |                                         // example -> "rrec" rrec
                        10 |                                        // example -> "lrec" lrec
                        11 |                                        // example -> "amb" amb
                        12 |                                        // example -> "star-a" star_a
                        13 |                                        // example -> "plus-a" plus_a
                        14 |                                        // example -> "l-star-a" l_star_a
                        15 |                                        // example -> "l-plus-a" l_plus_a
                        16 |                                        // example -> "sep-list" sep_list
                        17 |                                        // example -> "sep-list-opt" sep_list_opt
                        18 |                                        // example -> "l-sep-list" l_sep_list
                        19 => self.exit_example(alt_id),            // example -> "l-sep-list-opt" l_sep_list_opt
                        20 => self.exit_star(),                     // star -> Id "=" Id star_1 ";"
                        114 => self.exit_star1(),                   // star_1 -> star_1 "," Num
                        115 => self.init_star1(),                   // star_1 -> ε
                        21 => self.exit_plus(),                     // plus -> Id "=" Num plus_1 ";"
                        116 |                                       // plus_1 -> plus_1 "," Num
                        117 => self.exit_plus1(alt_id),             // plus_1 -> "," Num
                        22 => self.exit_l_star(),                   // l_star -> Id "=" Num l_star_i ";"
                        23 => self.exit_l_star_i(),                 // l_star_i -> <L> l_star_i "," Num
                        24 => self.init_l_star_i(),                 // l_star_i -> <L> ε
                        25 => self.exit_l_plus(),                   // l_plus -> Id "=" Num l_plus_i ";"
                        26 |                                        // l_plus_i -> <L> l_plus_i "," Num
                        27 => self.exit_l_plus_i(alt_id),           // l_plus_i -> <L> "," Num
                        28 => self.exit_rrec(),                     // rrec -> Id "=" Num rrec_i
                        29 => self.exit_lrec(),                     // lrec -> Id "=" lrec_i ";"
                        30 => self.exit_amb(),                      // amb -> Id "=" amb_i ";"
                        31 => self.exit_star_a(),                   // star_a -> Id "=" "[" star_a_1 "]" ";"
                        118 |                                       // star_a_1 -> star_a_1 Id
                        119 => self.exit_star_a1(alt_id),           // star_a_1 -> star_a_1 Num ":" Id
                        120 => self.init_star_a1(),                 // star_a_1 -> ε
                        32 => self.exit_plus_a(),                   // plus_a -> Id "=" "[" plus_a_1 "]" ";"
                        121 |                                       // plus_a_1 -> plus_a_1 Id
                        122 |                                       // plus_a_1 -> Id
                        123 |                                       // plus_a_1 -> plus_a_1 Num ":" Id
                        124 => self.exit_plus_a1(alt_id),           // plus_a_1 -> Num ":" Id
                        33 => self.exit_l_star_a(),                 // l_star_a -> Id "=" "[" l_star_a_i "]" ";"
                        34 |                                        // l_star_a_i -> <L> l_star_a_i Id
                        35 => self.exit_l_star_a_i(alt_id),         // l_star_a_i -> <L> l_star_a_i Num ":" Id
                        36 => self.init_l_star_a_i(),               // l_star_a_i -> <L> ε
                        37 => self.exit_l_plus_a(),                 // l_plus_a -> Id "=" "[" l_plus_a_i "]" ";"
                        38 |                                        // l_plus_a_i -> <L> l_plus_a_i Id
                        39 |                                        // l_plus_a_i -> <L> Id
                        40 |                                        // l_plus_a_i -> <L> l_plus_a_i Num ":" Id
                        41 => self.exit_l_plus_a_i(alt_id),         // l_plus_a_i -> <L> Num ":" Id
                        42 => self.exit_sep_list(),                 // sep_list -> Id "=" sep_list_1 ";"
                        125 => self.exit_sep_list1(),               // sep_list_1 -> sep_list_1 "," "then" Id ":" Num
                        126 => self.init_sep_list1(),               // sep_list_1 -> Id ":" Num
                        43 |                                        // sep_list_opt -> Id "=" sep_list_opt_1 ";"
                        44 => self.exit_sep_list_opt(alt_id),       // sep_list_opt -> Id "=" ";"
                        127 => self.exit_sep_list_opt1(),           // sep_list_opt_1 -> sep_list_opt_1 "," "then" Id ":" Num
                        128 => self.init_sep_list_opt1(),           // sep_list_opt_1 -> Id ":" Num
                        45 => self.exit_l_sep_list(),               // l_sep_list -> Id "=" l_sep_list_i ";"
                        46 => self.exit_l_sep_list_i(),             // l_sep_list_i -> <L> l_sep_list_i "," "then" Id ":" Num
                        47 => self.init_l_sep_list_i(),             // l_sep_list_i -> <L> Id ":" Num
                        48 |                                        // l_sep_list_opt -> Id "=" l_sep_list_opt_i ";"
                        49 => self.exit_l_sep_list_opt(alt_id),     // l_sep_list_opt -> Id "=" ";"
                        50 => self.exit_l_sep_list_opt_i(),         // l_sep_list_opt_i -> <L> l_sep_list_opt_i "," "then" Id ":" Num
                        51 => self.init_l_sep_list_opt_i(),         // l_sep_list_opt_i -> <L> Id ":" Num
                        52 |                                        // rrec_i -> "," Num rrec_i
                        53 => self.exit_rrec_i(alt_id),             // rrec_i -> ";"
                        54 |                                        // lrec_i -> lrec_i "," Num
                        55 => self.exit_lrec_i(alt_id),             // lrec_i -> Num
                        56 |                                        // amb_i -> "-" amb_i
                        57 |                                        // amb_i -> <R> amb_i "^" amb_i
                        58 |                                        // amb_i -> amb_i "*" amb_i
                        59 |                                        // amb_i -> <P> amb_i "/" amb_i
                        60 |                                        // amb_i -> amb_i "+" amb_i
                        61 |                                        // amb_i -> <P> amb_i "-" amb_i
                        62 |                                        // amb_i -> "(" amb_i ")"
                        63 |                                        // amb_i -> Id
                        64 => self.exit_amb_i(alt_id),              // amb_i -> Num
                        65 |                                        // nv_example -> "star" nv_star
                        66 |                                        // nv_example -> "plus" nv_plus
                        67 |                                        // nv_example -> "l-star" nv_l_star
                        68 |                                        // nv_example -> "l-plus" nv_l_plus
                        69 |                                        // nv_example -> "rrec" nv_rrec
                        70 |                                        // nv_example -> "lrec" nv_lrec
                        71 |                                        // nv_example -> "star-a" nv_star_a
                        72 |                                        // nv_example -> "plus-a" nv_plus_a
                        73 |                                        // nv_example -> "l-star-a" nv_l_star_a
                        74 |                                        // nv_example -> "l-plus-a" nv_l_plus_a
                        75 |                                        // nv_example -> "sep-list" nv_sep_list
                        76 |                                        // nv_example -> "sep-list-opt" nv_sep_list_opt
                        77 |                                        // nv_example -> "l-sep-list" nv_l_sep_list
                        78 => self.exit_nv_example(alt_id),         // nv_example -> "l-sep-list-opt" nv_l_sep_list_opt
                        79 => self.exit_nv_star(),                  // nv_star -> Id "=" "+" nv_star_1 ";"
                        129 => self.exit_nv_star1(),                // nv_star_1 -> nv_star_1 "," "*"
                        130 => self.init_nv_star1(),                // nv_star_1 -> ε
                        80 => self.exit_nv_plus(),                  // nv_plus -> Id "=" "+" nv_plus_1 ";"
                        131 |                                       // nv_plus_1 -> nv_plus_1 "," "*"
                        132 => self.exit_nv_plus1(alt_id),          // nv_plus_1 -> "," "*"
                        81 => self.exit_nv_l_star(),                // nv_l_star -> Id "=" "+" nv_l_star_i ";"
                        82 => self.exit_nv_l_star_i(),              // nv_l_star_i -> <L> nv_l_star_i "," "*"
                        83 => self.init_nv_l_star_i(),              // nv_l_star_i -> <L> ε
                        84 => self.exit_nv_l_plus(),                // nv_l_plus -> Id "=" "+" nv_l_plus_i ";"
                        85 |                                        // nv_l_plus_i -> <L> nv_l_plus_i "," "*"
                        86 => self.exit_nv_l_plus_i(alt_id),        // nv_l_plus_i -> <L> "," "*"
                        87 => self.exit_nv_rrec(),                  // nv_rrec -> Id "=" "+" nv_rrec_i
                        88 => self.exit_nv_lrec(),                  // nv_lrec -> Id "=" nv_lrec_i ";"
                        89 => self.exit_nv_star_a(),                // nv_star_a -> Id "=" "[" nv_star_a_1 "]" ";"
                        133 |                                       // nv_star_a_1 -> nv_star_a_1 "+"
                        134 => self.exit_nv_star_a1(alt_id),        // nv_star_a_1 -> nv_star_a_1 "*" "-"
                        135 => self.init_nv_star_a1(),              // nv_star_a_1 -> ε
                        90 => self.exit_nv_plus_a(),                // nv_plus_a -> Id "=" "[" nv_plus_a_1 "]" ";"
                        136 |                                       // nv_plus_a_1 -> nv_plus_a_1 "+"
                        137 |                                       // nv_plus_a_1 -> "+"
                        138 |                                       // nv_plus_a_1 -> nv_plus_a_1 "*" "-"
                        139 => self.exit_nv_plus_a1(alt_id),        // nv_plus_a_1 -> "*" "-"
                        91 => self.exit_nv_l_star_a(),              // nv_l_star_a -> Id "=" "[" nv_l_star_a_i "]" ";"
                        92 |                                        // nv_l_star_a_i -> <L> nv_l_star_a_i "+"
                        93 => self.exit_nv_l_star_a_i(alt_id),      // nv_l_star_a_i -> <L> nv_l_star_a_i "*" "-"
                        94 => self.init_nv_l_star_a_i(),            // nv_l_star_a_i -> <L> ε
                        95 => self.exit_nv_l_plus_a(),              // nv_l_plus_a -> Id "=" "[" nv_l_plus_a_i "]" ";"
                        96 |                                        // nv_l_plus_a_i -> <L> nv_l_plus_a_i "+"
                        97 |                                        // nv_l_plus_a_i -> <L> "+"
                        98 |                                        // nv_l_plus_a_i -> <L> nv_l_plus_a_i "*" "-"
                        99 => self.exit_nv_l_plus_a_i(alt_id),      // nv_l_plus_a_i -> <L> "*" "-"
                        100 => self.exit_nv_sep_list(),             // nv_sep_list -> Id "=" nv_sep_list_1 ";"
                        140 => self.exit_nv_sep_list1(),            // nv_sep_list_1 -> nv_sep_list_1 "," "then" "*"
                        141 => self.init_nv_sep_list1(),            // nv_sep_list_1 -> "*"
                        101 |                                       // nv_sep_list_opt -> Id "=" nv_sep_list_opt_1 ";"
                        102 => self.exit_nv_sep_list_opt(alt_id),   // nv_sep_list_opt -> Id "=" ";"
                        142 => self.exit_nv_sep_list_opt1(),        // nv_sep_list_opt_1 -> nv_sep_list_opt_1 "," "then" "*"
                        143 => self.init_nv_sep_list_opt1(),        // nv_sep_list_opt_1 -> "*"
                        103 => self.exit_nv_l_sep_list(),           // nv_l_sep_list -> Id "=" nv_l_sep_list_i ";"
                        104 => self.exit_nv_l_sep_list_i(),         // nv_l_sep_list_i -> <L> nv_l_sep_list_i "," "then" "*"
                        105 => self.init_nv_l_sep_list_i(),         // nv_l_sep_list_i -> <L> "*"
                        106 |                                       // nv_l_sep_list_opt -> Id "=" nv_l_sep_list_opt_i ";"
                        107 => self.exit_nv_l_sep_list_opt(alt_id), // nv_l_sep_list_opt -> Id "=" ";"
                        108 => self.exit_nv_l_sep_list_opt_i(),     // nv_l_sep_list_opt_i -> <L> nv_l_sep_list_opt_i "," "then" "*"
                        109 => self.init_nv_l_sep_list_opt_i(),     // nv_l_sep_list_opt_i -> <L> "*"
                        110 |                                       // nv_rrec_i -> "," "*" nv_rrec_i
                        111 => self.exit_nv_rrec_i(alt_id),         // nv_rrec_i -> ";"
                        112 |                                       // nv_lrec_i -> nv_lrec_i "," "*"
                        113 => self.exit_nv_lrec_i(alt_id),         // nv_lrec_i -> "+"
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

        fn init_i(&mut self) {
            self.listener.init_i();
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_i(&mut self) {
            let ctx = CtxI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_i(ctx, spans);
        }

        fn init_nv_i(&mut self) {
            self.listener.init_nv_i();
            self.stack_span.push(PosSpan::empty());
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
                19 => {
                    (2, CtxExample::V15)
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
            self.stack_span.push(PosSpan::empty());
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
            self.stack_span.insert(self.stack_span.len() - 2, PosSpan::empty());
        }

        fn exit_plus1(&mut self, alt_id: AltId) {
            let num = self.stack_t.pop().unwrap();
            if matches!(alt_id, 117) { self.init_plus1(); }
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

        fn init_l_star_i(&mut self) {
            self.listener.init_l_star_i();
            self.stack_span.push(PosSpan::empty());
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

        fn init_l_plus_i(&mut self) {
            self.listener.init_l_plus_i();
            self.stack_span.insert(self.stack_span.len() - 2, PosSpan::empty());
        }

        fn exit_l_plus_i(&mut self, alt_id: AltId) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusI::V1 { num };
            if matches!(alt_id, 27) { self.init_l_plus_i(); }
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
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_star_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                118 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynStarA1Item::V1 { id })
                }
                119 => {
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

        fn init_plus_a1(&mut self, alt_id: AltId) {
            let val = SynPlusA1(Vec::new());
            self.stack.push(EnumSynValue::PlusA1(val));
            let n = match alt_id {
                122 => 1,
                124 => 3,
                _ => panic!("alt_id = {alt_id} unexpected in method init_plus_a1")
            };
            self.stack_span.insert(self.stack_span.len() - n, PosSpan::empty());
        }

        fn exit_plus_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                121 | 122 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynPlusA1Item::V1 { id })
                }
                123 | 124 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, SynPlusA1Item::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_plus_a1"),
            };
            if alt_id.is_multiple_of(2) { self.init_plus_a1(alt_id); } // 122 | 124
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

        fn init_l_star_a_i(&mut self) {
            self.listener.init_l_star_a_i();
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                34 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLStarAI::V1 { id })
                }
                35 => {
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

        fn init_l_plus_a_i(&mut self, alt_id: AltId) {
            self.listener.init_l_plus_a_i();
            let n = match alt_id {
                39 => 1,
                41 => 3,
                _ => panic!("alt_id = {alt_id} unexpected in method init_l_plus_a_i")
            };
            self.stack_span.insert(self.stack_span.len() - n, PosSpan::empty());
        }

        fn exit_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                38 | 39 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLPlusAI::V1 { id })
                }
                40 | 41 => {
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLPlusAI::V2 { num, id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_plus_a_i")
            };
            if !alt_id.is_multiple_of(2) { self.init_l_plus_a_i(alt_id); } // 39 | 41
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
                43 => {
                    let plus = self.stack.pop().unwrap().get_sep_list_opt1();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxSepListOpt::V1 { id, plus })
                }
                44 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxSepListOpt::V2 { id })
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

        fn exit_l_sep_list(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepList::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_sep_list(ctx, spans);
        }

        fn init_l_sep_list_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.init_l_sep_list_i();
            self.listener.exit_l_sep_list_i(ctx, spans);
        }

        fn exit_l_sep_list_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListI::V1 { id, num };
            let mut spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            self.listener.exit_l_sep_list_i(ctx, spans);
        }

        fn exit_l_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                48 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxLSepListOpt::V1 { id })
                }
                49 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxLSepListOpt::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_l_sep_list_opt(ctx, spans);
        }

        fn init_l_sep_list_opt_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListOptI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.init_l_sep_list_opt_i();
            self.listener.exit_l_sep_list_opt_i(ctx, spans);
        }

        fn exit_l_sep_list_opt_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListOptI::V1 { id, num };
            let mut spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            self.listener.exit_l_sep_list_opt_i(ctx, spans);
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                52 => {
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num })
                }
                53 => {
                    (1, CtxRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_rrec_i(ctx, spans);
        }

        fn exit_lrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                54 => {
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxLrecI::V1 { num })
                }
                55 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxLrecI::V2 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_lrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_lrec_i(ctx, spans);
        }

        fn exit_amb_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                56 => {
                    (2, CtxAmbI::V1)
                }
                57 => {
                    (3, CtxAmbI::V2)
                }
                58 => {
                    (3, CtxAmbI::V3)
                }
                59 => {
                    (3, CtxAmbI::V4)
                }
                60 => {
                    (3, CtxAmbI::V5)
                }
                61 => {
                    (3, CtxAmbI::V6)
                }
                62 => {
                    (3, CtxAmbI::V7)
                }
                63 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V8 { id })
                }
                64 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V9 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_amb_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_amb_i(ctx, spans);
        }

        fn exit_nv_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                65 => {
                    (2, CtxNvExample::V1)
                }
                66 => {
                    (2, CtxNvExample::V2)
                }
                67 => {
                    (2, CtxNvExample::V3)
                }
                68 => {
                    (2, CtxNvExample::V4)
                }
                69 => {
                    (2, CtxNvExample::V5)
                }
                70 => {
                    (2, CtxNvExample::V6)
                }
                71 => {
                    (2, CtxNvExample::V7)
                }
                72 => {
                    (2, CtxNvExample::V8)
                }
                73 => {
                    (2, CtxNvExample::V9)
                }
                74 => {
                    (2, CtxNvExample::V10)
                }
                75 => {
                    (2, CtxNvExample::V11)
                }
                76 => {
                    (2, CtxNvExample::V12)
                }
                77 => {
                    (2, CtxNvExample::V13)
                }
                78 => {
                    (2, CtxNvExample::V14)
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

        fn init_nv_star1(&mut self) {
            self.stack_span.push(PosSpan::empty());
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

        fn init_nv_plus1(&mut self) {
            self.stack_span.insert(self.stack_span.len() - 2, PosSpan::empty());
        }

        fn exit_nv_plus1(&mut self, alt_id: AltId) {
            if matches!(alt_id, 132) { self.init_nv_plus1(); }
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

        fn init_nv_l_star_i(&mut self) {
            self.listener.init_nv_l_star_i();
            self.stack_span.push(PosSpan::empty());
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

        fn init_nv_l_plus_i(&mut self) {
            self.listener.init_nv_l_plus_i();
            self.stack_span.insert(self.stack_span.len() - 2, PosSpan::empty());
        }

        fn exit_nv_l_plus_i(&mut self, alt_id: AltId) {
            let ctx = CtxNvLPlusI::V1;
            if matches!(alt_id, 86) { self.init_nv_l_plus_i(); }
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

        fn exit_nv_lrec(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLrec::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_lrec(ctx, spans);
        }

        fn exit_nv_star_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvStarA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_star_a(ctx, spans);
        }

        fn init_nv_star_a1(&mut self) {
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_nv_star_a1(&mut self, alt_id: AltId) {
            let n = match alt_id {
                133 => {
                    2
                }
                134 => {
                    3
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_star_a1"),
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
        }

        fn exit_nv_plus_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvPlusA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_plus_a(ctx, spans);
        }

        fn init_nv_plus_a1(&mut self, alt_id: AltId) {
            let n = match alt_id {
                137 => 1,
                139 => 2,
                _ => panic!("alt_id = {alt_id} unexpected in method init_nv_plus_a1")
            };
            self.stack_span.insert(self.stack_span.len() - n, PosSpan::empty());
        }

        fn exit_nv_plus_a1(&mut self, alt_id: AltId) {
            let n = match alt_id {
                136 | 137 => {
                    2
                }
                138 | 139 => {
                    3
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_plus_a1"),
            };
            if !alt_id.is_multiple_of(2) { self.init_nv_plus_a1(alt_id); } // 137 | 139
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

        fn init_nv_l_star_a_i(&mut self) {
            self.listener.init_nv_l_star_a_i();
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_nv_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                92 => {
                    (2, CtxNvLStarAI::V1)
                }
                93 => {
                    (3, CtxNvLStarAI::V2)
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

        fn init_nv_l_plus_a_i(&mut self, alt_id: AltId) {
            self.listener.init_nv_l_plus_a_i();
            let n = match alt_id {
                97 => 1,
                99 => 2,
                _ => panic!("alt_id = {alt_id} unexpected in method init_nv_l_plus_a_i")
            };
            self.stack_span.insert(self.stack_span.len() - n, PosSpan::empty());
        }

        fn exit_nv_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                96 | 97 => {
                    (2, CtxNvLPlusAI::V1)
                }
                98 | 99 => {
                    (3, CtxNvLPlusAI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_plus_a_i")
            };
            if !alt_id.is_multiple_of(2) { self.init_nv_l_plus_a_i(alt_id); } // 97 | 99
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
                101 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvSepListOpt::V1 { id })
                }
                102 => {
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

        fn exit_nv_l_sep_list(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLSepList::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_sep_list(ctx, spans);
        }

        fn init_nv_l_sep_list_i(&mut self) {
            let ctx = CtxNvLSepListI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.init_nv_l_sep_list_i();
            self.listener.exit_nv_l_sep_list_i(ctx, spans);
        }

        fn exit_nv_l_sep_list_i(&mut self) {
            let ctx = CtxNvLSepListI::V1;
            let mut spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            self.listener.exit_nv_l_sep_list_i(ctx, spans);
        }

        fn exit_nv_l_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                106 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvLSepListOpt::V1 { id })
                }
                107 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxNvLSepListOpt::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_l_sep_list_opt(ctx, spans);
        }

        fn init_nv_l_sep_list_opt_i(&mut self) {
            let ctx = CtxNvLSepListOptI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.init_nv_l_sep_list_opt_i();
            self.listener.exit_nv_l_sep_list_opt_i(ctx, spans);
        }

        fn exit_nv_l_sep_list_opt_i(&mut self) {
            let ctx = CtxNvLSepListOptI::V1;
            let mut spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            self.listener.exit_nv_l_sep_list_opt_i(ctx, spans);
        }

        fn exit_nv_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                110 => {
                    (3, CtxNvRrecI::V1)
                }
                111 => {
                    (1, CtxNvRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_rrec_i(ctx, spans);
        }

        fn exit_nv_lrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                112 => {
                    (3, CtxNvLrecI::V1)
                }
                113 => {
                    (1, CtxNvLrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_lrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            self.listener.exit_nv_lrec_i(ctx, spans);
        }
    }

    // [pandemonium_parser]
}
