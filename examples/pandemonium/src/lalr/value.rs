// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

// =============================================================================================
// Parser to test grammar constructions

use std::collections::BTreeMap;
use lexigram_core::{CollectJoin, LALR};
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::Terminate;
use lexigram_core::parser::lr::LRParser;
use lexigram_core::text_span::{GetLine, GetTextSpan};
use crate::level_string::{ls_binary_op, ls_prefix_op, LevelString};
use listener_types::*;
use pandemonium_lexer::build_lexer;
use pandemonium_parser::*;
use crate::lalr::{SPANS1, SPANS2, TXT1, TXT2};

const VERBOSE: bool = false;
const VERBOSE_WRAPPER: bool = false;

#[test]
fn test_pandemonium() {
    let mut demo = PanDemo::new();
    static TESTS: &[(&str, &[&str], &[&str])] = &[(TXT1, VALUES1, SPANS1), (TXT2, VALUES2, SPANS2)];
    for (i, &(txt, expected_values, expected_spans)) in TESTS.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\nTest {i}\n{0:-<80}", ""); }
        match demo.parse(txt) {
            Ok(PanDemoResult { log, values, spans, rebuilt_txt }) => {
                let result_values = values.iter().map(|(id, v)| format!("[{id}][{v}]")).to_vec();
                if VERBOSE {
                    println!("parsing successful\n{log}");
                    println!("Values:{}\n", result_values.iter().map(|s| format!("\n    {s:?},")).join(""));
                    println!("spans:{}", spans.iter().map(|s| format!("\n    r#\"{s}\"#,")).join(""));
                }
                // checks that the values have been correctly captured from the context data:
                assert_eq!(result_values, expected_values, "value mismatch");
                // checks that the text rebuilt from spans matches the original:
                assert!(txt.contains(&rebuilt_txt), "rebuilt text is wrong:\n{rebuilt_txt:?}");
                // checks the individual spans:
                // (tedious visual verification each time the test changes!)
                assert_eq!(
                    spans, expected_spans, "span mismatch:\n{}",
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
            Err(log) => panic!("errors during parsing:\n{log}"),
        }
    }
}

static VALUES1: &[&str] = &[
    "[Alpha][a*101*110*150]",
    "[Bravo][102+120+250]",
    "[Charlie][103,130,350]",
    "[Delta][104,140,450]",
    "[Echo][105;150;550]",
    "[Golf][(107),(170),(750)]",
    "[Hotel][(5 - (2 * (- 6))) + ((3 ^ (2 ^ 4)) / 81)]",
    "[India][1:Alpha/Beta/4:Delta/Echo/10:Juliet]",
    "[Juliet][11:Kilo/Lima/Mike/26:Zoulou]",
    "[Kilo][2:Beta|Charlie|5:Echo]",
    "[Lima][21:Uniform||Victor||25:Yankee]",
    "[Mike][x]",
    "[November][202]",
    "[Oscar][203]",
    "[Quebec][(205)]",
    "[Romeo][<a:1><b:2><c:3>]",
    "[Sierra][<d:4>]",
    "[Tango][<e/5><f/6><g/7>]",
    "[Uniform][-]",
    "[Victor][<a:1><b:2><c:3>]",
    "[Whiskey][<d:4>]",
    "[Xray][<e/5><f/6><g/7>]",
    "[Yankee][-]",
];

static VALUES2: &[&str] = &[
    "[Alpha][*]",
    "[Bravo][+]",
    "[Charlie][4]",
    "[Delta][false,false,false]",
    "[Echo][2]",
    "[Golf][2]",
    "[India][+*-*]",
    "[Juliet][+*-+]",
    "[Kilo][false|true|false|true|false]",
    "[Lima][false||true||false||true||false]",
    "[Mike][*]",
    "[November][0]",
    "[Oscar][0]",
    "[Quebec][0]",
    "[Romeo][*,then+]",
    "[Sierra][*,then+]",
    "[Tango][a]",
    "[Uniform][b]",
    "[Victor][3]",
    "[Whiskey][1]",
    "[Xray][2]",
    "[Yankee][-]",
];

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
            let values = std::mem::take(&mut listener.values);
            let spans = std::mem::take(&mut listener.spans);
            let rebuilt_txt = listener.rebuilt_txt.take().unwrap();
            Ok(PanDemoResult { log, values, spans, rebuilt_txt })
        } else {
            Err(log)
        }
    }
}

// listener implementation

struct PanDemoResult {
    log: BufLog,
    values: BTreeMap<String, String>,
    spans: Vec<String>,
    rebuilt_txt: String,
}

struct PanDemoListener<'ls> {
    log: BufLog,
    abort: Terminate,
    spans: Vec<String>,
    lines: Option<Vec<&'ls str>>,
    rebuilt_txt: Option<String>,
    values: BTreeMap<String, String>,
}

impl<'ls> PanDemoListener<'ls> {
    fn new() -> Self {
        PanDemoListener {
            log: BufLog::new(),
            abort: Terminate::None,
            spans: vec![],
            lines: None,
            rebuilt_txt: None,
            values: BTreeMap::new(),
        }
    }

    fn attach_lines(&mut self, lines: Vec<&'ls str>) {
        self.lines = Some(lines);
    }

    fn add_value(&mut self, id: String, value: String) {
        if let Some(old) = self.values.insert(id.clone(), value) {
            let new = self.values.get(&id).unwrap();
            panic!("{}", format!("key was already in the values:\n- before: {id} = {old}\n- now   : {id} = {new}"));
        };
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

    fn exit(&mut self, text: SynText, span: PosSpan) {
        self.rebuilt_txt = Some(self.extract_text(&span));
    }

    fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText {
        self.spans.push(format!("exit_text({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxText::V1 { star: SynI(), star1: SynNvI() } = ctx; // text -> (<L> example)* ";" (<L> nv_example)*
        SynText()
    }

    fn init_i(&mut self) -> SynI {
        SynI()
    }

    fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxI::V1 { example: SynExample() } = ctx;
    }

    fn init_nv_i(&mut self) -> SynNvI {
        SynNvI()
    }

    fn exit_nv_i(&mut self, acc: &mut SynNvI, ctx: CtxNvI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxNvI::V1 { nv_example: SynNvExample() } = ctx;
    }

    fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample {
        self.spans.push(format!("exit_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxExample::V1 { star: SynStar() } => {}
            CtxExample::V2 { plus: SynPlus() } => {}
            CtxExample::V3 { l_star: SynLStar() } => {}
            CtxExample::V4 { l_plus: SynLPlus() } => {}
            CtxExample::V5 { rrec: SynRrec() } => {}
            CtxExample::V6 { lrec: SynLrec() } => {}
            CtxExample::V7 { amb: SynAmb() } => {}
            CtxExample::V8 { star_a: SynStarA() } => {}
            CtxExample::V9 { plus_a: SynPlusA() } => {}
            CtxExample::V10 { l_star_a: SynLStarA() } => {}
            CtxExample::V11 { l_plus_a: SynLPlusA() } => {}
            CtxExample::V12 { sep_list: SynSepList() } => {}
            CtxExample::V13 { sep_list_opt: SynSepListOpt() } => {}
            CtxExample::V14 { l_sep_list: SynLSepList() } => {}
            CtxExample::V15 { l_sep_list_opt: SynLSepListOpt() } => {}
        }
        SynExample()
    }

    fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar {
        // star -> Id "=" Id ("," Num)* ";"
        self.spans.push(format!("exit_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxStar::V1 { id: [id0, id1], star: SynStar1(star) } = ctx;
        self.add_value(id0, format!("{id1}{}", star.into_iter().map(|s| format!("*{s}")).join("")));
        SynStar()
    }

    fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus {
        // plus -> Id "=" Num ("," Num)+ ";"
        self.spans.push(format!("exit_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxPlus::V1 { id, num, plus: SynPlus1(plus) } = ctx;
        self.add_value(id, format!("{num}+{}", plus.join("+")));
        SynPlus()
    }

    fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar {
        self.spans.push(format!("exit_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLStar::V1 { id, num, star: SynLStarI(mut items) } = ctx;
        items.insert(0, num);
        self.add_value(id, format!("{}", items.join(",")));
        SynLStar()
    }

    fn init_l_star_i(&mut self) -> SynLStarI {
        SynLStarI(vec![])
    }

    fn exit_l_star_i(&mut self, acc: &mut SynLStarI, ctx: CtxLStarI, spans: Vec<PosSpan>) {
        // `<L> "," Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," Num ◄◄ )* ";"`
        let CtxLStarI::V1 { num } = ctx;
        self.spans.push(format!("exit_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        acc.0.push(num);
    }

    fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus {
        self.spans.push(format!("exit_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLPlus::V1 { id, num, plus: SynLPlusI(items) } = ctx;
        self.add_value(id, format!("{num},{}", items.join(",")));
        SynLPlus()
    }

    fn init_l_plus_i(&mut self) -> SynLPlusI {
        SynLPlusI(vec![])
    }

    fn exit_l_plus_i(&mut self, acc: &mut SynLPlusI, ctx: CtxLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLPlusI::V1 { num } = ctx;
        acc.0.push(num);
    }

    fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxRrec::V1 { id, num, rrec_i: SynRrecI(mut items) } = ctx;
        items.push(num);
        self.add_value(id, items.iter().rev().join(";"));
        SynRrec()
    }

    fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec {
        self.spans.push(format!("exit_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLrec::V1 { id, lrec_i: SynLrecI(items) } = ctx;
        self.add_value(id, items.into_iter().map(|s| format!("({s})")).join(","));
        SynLrec()
    }

    fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb {
        self.spans.push(format!("exit_amb({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxAmb::V1 { id, amb_i: SynAmbI(ls) } = ctx;
        self.add_value(id, ls.get_string());
        SynAmb()
    }

    fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) -> SynStarA {
        self.spans.push(format!("exit_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // star_a -> Id "=" "[" (Id | Num ":" Id)* "]" ";"
        let CtxStarA::V1 { id, star: SynStarA1(items) } = ctx;
        let value = items.into_iter().map(|item|
            match item {
                SynStarA1Item::V1 { id } => id,
                SynStarA1Item::V2 { num, id } => format!("{num}:{id}"),
            })
            .join("/");
        self.add_value(id, value);
        SynStarA()
    }

    fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) -> SynPlusA {
        self.spans.push(format!("exit_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // plus_a -> Id "=" "[" (Id | Num ":" Id)+ "]" ";"
        let CtxPlusA::V1 { id, plus: SynPlusA1(items) } = ctx;
        let value = items.into_iter().map(|item|
            match item {
                SynPlusA1Item::V1 { id } => id,
                SynPlusA1Item::V2 { num, id } => format!("{num}:{id}"),
            })
            .join("/");
        self.add_value(id, value);
        SynPlusA()
    }

    fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) -> SynLStarA {
        self.spans.push(format!("exit_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_star_a -> Id "=" "[" (<L> Id | Num ":" Id)* "]" ";"
        let CtxLStarA::V1 { id, star: SynLStarAI(items) } = ctx;
        self.add_value(id, items.join("|"));
        SynLStarA()
    }

    fn init_l_star_a_i(&mut self) -> SynLStarAI {
        SynLStarAI(vec![])
    }

    fn exit_l_star_a_i(&mut self, acc: &mut SynLStarAI, ctx: CtxLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let value = match ctx {
            // `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
            CtxLStarAI::V1 { id } => id,
            // `Num ":" Id` iteration in `l_star_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )* "]" ";"`
            CtxLStarAI::V2 { num, id } => format!("{num}:{id}"),
        };
        acc.0.push(value);
    }

    fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA {
        self.spans.push(format!("exit_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_plus_a -> Id "=" "[" (<L> Id | Num ":" Id)+ "]" ";"
        let CtxLPlusA::V1 { id, plus: SynLPlusAI(items) } = ctx;
        self.add_value(id, items.join("||"));
        SynLPlusA()
    }

    fn init_l_plus_a_i(&mut self) -> SynLPlusAI {
        SynLPlusAI(vec![])
    }

    fn exit_l_plus_a_i(&mut self, acc: &mut SynLPlusAI, ctx: CtxLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let value = match ctx {
            // `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
            CtxLPlusAI::V1 { id } => id,
            // `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
            CtxLPlusAI::V2 { num, id } => format!("{num}:{id}"),
        };
        acc.0.push(value);
    }

    fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) -> SynSepList {
        self.spans.push(format!("exit_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // sep_list -> Id "=" Id ":" Num ("," Id ":" Num)* ";"
        let CtxSepList::V1 { id, plus: SynSepList1(items) } = ctx;
        let value = items.into_iter().map(|SynSepList1Item { id, num }| format!("<{id}:{num}>")).join("");
        self.add_value(id, value);
        SynSepList()
    }

    fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) -> SynSepListOpt {
        self.spans.push(format!("exit_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let (id, value) = match ctx {
            // `sep_list_opt -> Id "=" Id ":" Num ("," Id ":" Num)* ";"`
            CtxSepListOpt::V1 { id, plus: SynSepListOpt1(items) } =>
                (id, items.into_iter().map(|SynSepListOpt1Item { id, num }| format!("<{id}/{num}>")).join("")),
            // `sep_list_opt -> Id "=" ";"`
            CtxSepListOpt::V2 { id } =>
                (id, "-".to_string()),
        };
        self.add_value(id, value);
        SynSepListOpt()
    }

    fn exit_l_sep_list(&mut self, ctx: CtxLSepList, spans: Vec<PosSpan>) -> SynLSepList {
        self.spans.push(format!("exit_l_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // l_sep_list -> Id "=" (<L> Id ":" Num / "," "then")+ ";"
        let CtxLSepList::V1 { id, plus: SynLSepListI(mut items) } = ctx;
        self.add_value(id, format!("{}", items.join("")));
        SynLSepList()
    }

    fn init_l_sep_list_i(&mut self) -> SynLSepListI {
        SynLSepListI(vec![])
    }

    fn exit_l_sep_list_i(&mut self, acc: &mut SynLSepListI, ctx: CtxLSepListI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_sep_list_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
        let CtxLSepListI::V1 { id, num } = ctx;
        acc.0.push(format!("<{id}:{num}>"));
    }

    fn exit_l_sep_list_opt(&mut self, ctx: CtxLSepListOpt, spans: Vec<PosSpan>) -> SynLSepListOpt {
        self.spans.push(format!("exit_l_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let (id, value) = match ctx {
            // l_sep_list_opt -> Id "=" (<L> Id ":" Num / "," "then")+ ";"
            CtxLSepListOpt::V1 { id, plus: SynLSepListOptI(items) } =>
                (id, items.join("")),
            // l_sep_list_opt -> Id "=" ";"
            CtxLSepListOpt::V2 { id } =>
                (id, "-".to_string()),
        };
        self.add_value(id, value);
        SynLSepListOpt()
    }

    fn init_l_sep_list_opt_i(&mut self) -> SynLSepListOptI {
        SynLSepListOptI(vec![])
    }

    fn exit_l_sep_list_opt_i(&mut self, acc: &mut SynLSepListOptI, ctx: CtxLSepListOptI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_sep_list_opt_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> Id ":" Num / "," "then"` iteration in `l_sep_list_opt -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        let CtxLSepListOptI::V1 { id, num } = ctx;
        acc.0.push(format!("<{id}/{num}>"));
    }

    fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI {
        self.spans.push(format!("exit_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let items = match ctx {
            CtxRrecI::V1 { num, rrec_i: SynRrecI(mut prev_items) } => {
                prev_items.push(num);
                prev_items
            }
            CtxRrecI::V2 => vec![],
        };
        SynRrecI(items)
    }

    fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI {
        self.spans.push(format!("exit_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let items = match ctx {
            CtxLrecI::V1 { lrec_i: SynLrecI(mut prev_items), num } => {
                prev_items.push(num);
                prev_items
            }
            CtxLrecI::V2 { num } => vec![num],
        };
        SynLrecI(items)
    }

    fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI {
        self.spans.push(format!("exit_amb_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        SynAmbI(match ctx {
            // `amb_i -> "-" amb_i`
            CtxAmbI::V1 { amb_i: SynAmbI(ls) } => ls_prefix_op("-", ls),
            // `amb_i -> <R> amb_i "^" amb_i`
            CtxAmbI::V2 { amb_i: [SynAmbI(left), SynAmbI(right)] } => ls_binary_op("^", left, right),
            // `amb_i -> amb_i "*" amb_i`
            CtxAmbI::V3 { amb_i: [SynAmbI(left), SynAmbI(right)] } => ls_binary_op("*", left, right),
            // `amb_i -> amb_i <P> "/" amb_i`
            CtxAmbI::V4 { amb_i: [SynAmbI(left), SynAmbI(right)] } => ls_binary_op("/", left, right),
            // `amb_i -> amb_i "+" amb_i`
            CtxAmbI::V5 { amb_i: [SynAmbI(left), SynAmbI(right)] } => ls_binary_op("+", left, right),
            // `amb_i -> amb_i <P> "-" amb_i`
            CtxAmbI::V6 { amb_i: [SynAmbI(left), SynAmbI(right)] } => ls_binary_op("-", left, right),
            // `amb_i -> "(" amb_i ")"`
            CtxAmbI::V7 { amb_i: SynAmbI(ls) } => ls,
            // `amb_i -> Id`
            CtxAmbI::V8 { id } => LevelString(0, id),
            // `amb_i -> Num`
            CtxAmbI::V9 { num } => LevelString(0, num),
        })
    }

    fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) -> SynNvExample {
        self.spans.push(format!("exit_nv_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxNvExample::V1 { nv_star } => {}              // nv_example -> "star" nv_star
            CtxNvExample::V2 { nv_plus } => {}              // nv_example -> "plus" nv_plus
            CtxNvExample::V3 { nv_l_star } => {}            // nv_example -> "l-star" nv_l_star
            CtxNvExample::V4 { nv_l_plus } => {}            // nv_example -> "l-plus" nv_l_plus
            CtxNvExample::V5 { nv_rrec } => {}              // nv_example -> "rrec" nv_rrec
            CtxNvExample::V6 { nv_lrec } => {}              // nv_example -> "lrec" nv_lrec
            CtxNvExample::V7 { nv_star_a } => {}            // nv_example -> "star-a" nv_star_a
            CtxNvExample::V8 { nv_plus_a } => {}            // nv_example -> "plus-a" nv_plus_a
            CtxNvExample::V9 { nv_l_star_a } => {}          // nv_example -> "l-star-a" nv_l_star_a
            CtxNvExample::V10 { nv_l_plus_a } => {}         // nv_example -> "l-plus-a" nv_l_plus_a
            CtxNvExample::V11 { nv_sep_list } => {}         // nv_example -> "sep-list" nv_sep_list
            CtxNvExample::V12 { nv_sep_list_opt } => {}     // nv_example -> "sep-list-opt" nv_sep_list_opt
            CtxNvExample::V13 { nv_l_sep_list } => {}       // nv_example -> "l-sep-list" nv_l_sep_list
            CtxNvExample::V14 { nv_l_sep_list_opt } => {}   // nv_example -> "l-sep-list-opt" nv_l_sep_list_opt
        }
        SynNvExample()
    }

    fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) -> SynNvStar {
        self.spans.push(format!("exit_nv_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_star -> Id "=" "+" ("," "*")* ";"
        let CtxNvStar::V1 { id } = ctx;
        self.add_value(id, "*".to_string());
        SynNvStar()
    }

    fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) -> SynNvPlus {
        self.spans.push(format!("exit_nv_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_plus -> Id "=" "+" ("," "*")+ ";"
        let CtxNvPlus::V1 { id } = ctx;
        self.add_value(id, "+".to_string());
        SynNvPlus()
    }

    fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) -> SynNvLStar {
        self.spans.push(format!("exit_nv_l_star({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_star -> Id "=" "+" (<L> "," "*")* ";"
        let CtxNvLStar::V1 { id, star } = ctx;
        self.add_value(id, star.0.to_string());
        SynNvLStar()
    }

    fn init_nv_l_star_i(&mut self) -> SynNvLStarI {
        SynNvLStarI(0)
    }

    fn exit_nv_l_star_i(&mut self, acc: &mut SynNvLStarI, ctx: CtxNvLStarI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_star_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "," "*"` iteration in `nv_l_star -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )* ";"`
        let CtxNvLStarI::V1 = ctx;
        acc.0 += 1;
    }

    fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) -> SynNvLPlus {
        self.spans.push(format!("exit_nv_l_plus({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_plus -> Id "=" "+" (<L> "," "*")+ ";"
        let CtxNvLPlus::V1 { id, plus: SynNvLPlusI(items) } = ctx;
        self.add_value(id, items.iter().map(bool::to_string).join(","));
        SynNvLPlus()
    }

    fn init_nv_l_plus_i(&mut self) -> SynNvLPlusI {
        SynNvLPlusI(vec![])
    }

    fn exit_nv_l_plus_i(&mut self, acc: &mut SynNvLPlusI, ctx: CtxNvLPlusI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_plus_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "," "*"` iteration in `nv_l_plus -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )+ ";"`
        let CtxNvLPlusI::V1 = ctx;
        acc.0.push(false);
    }

    fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) -> SynNvRrec {
        self.spans.push(format!("exit_nv_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_rrec -> Id "=" "+" nv_rrec_i
        let CtxNvRrec::V1 { id, nv_rrec_i: SynNvRrecI(n) } = ctx;
        self.add_value(id, n.to_string());
        SynNvRrec()
    }

    fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) -> SynNvLrec {
        self.spans.push(format!("exit_nv_lrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_lrec -> Id "=" nv_lrec_i ";"
        let CtxNvLrec::V1 { id, nv_lrec_i: SynNvLrecI(n) } = ctx;
        self.add_value(id, n.to_string());
        SynNvLrec()
    }

    fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) -> SynNvStarA {
        self.spans.push(format!("exit_nv_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_star_a -> Id "=" "[" ("+" | "*" "-")* "]" ";"
        let CtxNvStarA::V1 { id } = ctx;
        self.add_value(id, "+*-*".to_string());
        SynNvStarA()
    }

    fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) -> SynNvPlusA {
        self.spans.push(format!("exit_nv_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_plus_a -> Id "=" "[" ("+" | "*" "-")+ "]" ";"
        let CtxNvPlusA::V1 { id } = ctx;
        self.add_value(id, "+*-+".to_string());
        SynNvPlusA()
    }

    fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) -> SynNvLStarA {
        self.spans.push(format!("exit_nv_l_star_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_star_a -> Id "=" "[" (<L> "+" | "*" "-")* "]" ";"
        let CtxNvLStarA::V1 { id, star: SynNvLStarAI(items) } = ctx;
        self.add_value(id, items.iter().map(bool::to_string).join("|"));
        SynNvLStarA()
    }

    fn init_nv_l_star_a_i(&mut self) -> SynNvLStarAI {
        SynNvLStarAI(vec![])
    }

    fn exit_nv_l_star_a_i(&mut self, acc: &mut SynNvLStarAI, ctx: CtxNvLStarAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_star_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        acc.0.push(match ctx {
            // `<L> "+"` iteration in `nv_l_star_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" "-")* "]" ";"`
            CtxNvLStarAI::V1 => true,
            // `"*" "-"` iteration in `nv_l_star_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )* "]" ";"`
            CtxNvLStarAI::V2 => false,
        });
    }

    fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) -> SynNvLPlusA {
        self.spans.push(format!("exit_nv_l_plus_a({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_plus_a -> Id "=" "[" (<L> "+" | "*" "-")+ "]" ";"
        let CtxNvLPlusA::V1 { id, plus: SynNvLPlusAI(items) } = ctx;
        self.add_value(id, items.iter().map(bool::to_string).join("||"));
        SynNvLPlusA()
    }

    fn init_nv_l_plus_a_i(&mut self) -> SynNvLPlusAI {
        SynNvLPlusAI(vec![])
    }

    fn exit_nv_l_plus_a_i(&mut self, acc: &mut SynNvLPlusAI, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_plus_a_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        acc.0.push(match ctx {
            // `<L> "+"` iteration in `nv_l_plus_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" "-")+ "]" ";"`
            CtxNvLPlusAI::V1 => true,
            // `"*" "-"` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )+ "]" ";"`
            CtxNvLPlusAI::V2 => false,
        });
    }

    fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) -> SynNvSepList {
        self.spans.push(format!("exit_nv_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_sep_list -> Id "=" ("*" / "," "then")+ ";"
        let CtxNvSepList::V1 { id } = ctx;
        self.add_value(id, "*,then+".to_string());
        SynNvSepList()
    }

    fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) -> SynNvSepListOpt {
        self.spans.push(format!("exit_nv_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let (id, str) = match ctx {
            // nv_sep_list_opt -> Id "=" ("*" / "," "then")+ ";"
            CtxNvSepListOpt::V1 { id } => (id, "a"),
            // nv_sep_list_opt -> Id "=" ";"
            CtxNvSepListOpt::V2 { id } => (id, "b"),
        };
        self.add_value(id, str.to_string());
        SynNvSepListOpt()
    }

    fn exit_nv_l_sep_list(&mut self, ctx: CtxNvLSepList, spans: Vec<PosSpan>) -> SynNvLSepList {
        self.spans.push(format!("exit_nv_l_sep_list({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_sep_list -> Id "=" (<L> "*" / "," "then")+ ";"
        let CtxNvLSepList::V1 { id, plus: SynNvLSepListI(n) } = ctx;
        self.add_value(id, n.to_string());
        SynNvLSepList()
    }

    fn init_nv_l_sep_list_i(&mut self) -> SynNvLSepListI {
        SynNvLSepListI(0)
    }

    fn exit_nv_l_sep_list_i(&mut self, acc: &mut SynNvLSepListI, ctx: CtxNvLSepListI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_sep_list_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "*" / "," "then"` iteration in `nv_l_sep_list -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";"`
        let CtxNvLSepListI::V1 = ctx;
        acc.0 += 1;
    }

    fn exit_nv_l_sep_list_opt(&mut self, ctx: CtxNvLSepListOpt, spans: Vec<PosSpan>) -> SynNvLSepListOpt {
        self.spans.push(format!("exit_nv_l_sep_list_opt({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // nv_l_sep_list_opt -> Id "=" (<L> "*" / "," "then")+ ";"
            CtxNvLSepListOpt::V1 { id, plus: SynNvLSepListOptI(n) } => { self.add_value(id, n.to_string()); }
            // nv_l_sep_list_opt -> Id "=" ";"
            CtxNvLSepListOpt::V2 { id } => { self.add_value(id, "-".to_string()); }
        }
        SynNvLSepListOpt()
    }

    fn init_nv_l_sep_list_opt_i(&mut self) -> SynNvLSepListOptI {
        SynNvLSepListOptI(0)
    }

    fn exit_nv_l_sep_list_opt_i(&mut self, acc: &mut SynNvLSepListOptI, ctx: CtxNvLSepListOptI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_sep_list_opt_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // `<L> "*" / "," "then"` iteration in `nv_l_sep_list_opt -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        let CtxNvLSepListOptI::V1 = ctx;
        acc.0 += 1;
    }

    fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) -> SynNvRrecI {
        self.spans.push(format!("exit_nv_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        SynNvRrecI(match ctx {
            // nv_rrec_i -> "," "*" nv_rrec_i
            CtxNvRrecI::V1 { nv_rrec_i: SynNvRrecI(n) } => n + 1,
            // nv_rrec_i -> ";"
            CtxNvRrecI::V2 => 0,
        })
    }

    fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) -> SynNvLrecI {
        self.spans.push(format!("exit_nv_lrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        SynNvLrecI(match ctx {
            // nv_lrec_i -> nv_lrec_i "," "*"
            CtxNvLrecI::V1 { nv_lrec_i: SynNvLrecI(n) } => n + 1,
            // nv_lrec_i -> "+"
            CtxNvLrecI::V2 => 0,
        })
    }
}

// -------------------------------------------------------------------------
// User types used in the listener interface:
// (initially copied/uncommented from the generated parser code)

pub mod listener_types {
    use crate::level_string::LevelString;

    /// User-defined type for `text`
    #[derive(Debug, PartialEq)] pub struct SynText();
    /// User-defined type for `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )* ";" (<L> nv_example)*`
    #[derive(Debug, PartialEq)] pub struct SynI();
    /// User-defined type for `<L> nv_example` iteration in `text -> (<L> example)* ";" ( ►► <L> nv_example ◄◄ )*`
    #[derive(Debug, PartialEq)] pub struct SynNvI();
    /// User-defined type for `example`
    #[derive(Debug, PartialEq)] pub struct SynExample();
    /// User-defined type for `nv_example`
    #[derive(Debug, PartialEq)] pub struct SynNvExample();
    /// User-defined type for `star`
    #[derive(Debug, PartialEq)] pub struct SynStar();
    /// User-defined type for `plus`
    #[derive(Debug, PartialEq)] pub struct SynPlus();
    /// User-defined type for `l_star`
    #[derive(Debug, PartialEq)] pub struct SynLStar();
    /// User-defined type for `<L> "," Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," Num ◄◄ )* ";"`
    #[derive(Debug, PartialEq)] pub struct SynLStarI(pub Vec<String>);
    /// User-defined type for `l_plus`
    #[derive(Debug, PartialEq)] pub struct SynLPlus();
    /// User-defined type for `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
    #[derive(Debug, PartialEq)] pub struct SynLPlusI(pub Vec<String>);
    /// User-defined type for `rrec`
    #[derive(Debug, PartialEq)] pub struct SynRrec();
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
    /// User-defined type for `<L> Id` iteration in `l_star_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)* "]" ";"`
    #[derive(Debug, PartialEq)] pub struct SynLStarAI(pub Vec<String>);
    /// User-defined type for `l_plus_a`
    #[derive(Debug, PartialEq)] pub struct SynLPlusA();
    /// User-defined type for `<L> Id` iteration in `l_plus_a -> Id "=" "[" ( ►► <L> Id ◄◄  | Num ":" Id)+ "]" ";"`
    #[derive(Debug, PartialEq)] pub struct SynLPlusAI(pub Vec<String>);
    /// User-defined type for `sep_list`
    #[derive(Debug, PartialEq)] pub struct SynSepList();
    /// User-defined type for `sep_list_opt`
    #[derive(Debug, PartialEq)] pub struct SynSepListOpt();
    /// User-defined type for `l_sep_list`
    #[derive(Debug, PartialEq)] pub struct SynLSepList();
    /// User-defined type for `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
    #[derive(Debug, PartialEq)] pub struct SynLSepListI(pub Vec<String>);
    /// User-defined type for `l_sep_list_opt`
    #[derive(Debug, PartialEq)] pub struct SynLSepListOpt();
    /// User-defined type for `<L> Id ":" Num / "," "then"` iteration in `l_sep_list_opt -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
    #[derive(Debug, PartialEq)] pub struct SynLSepListOptI(pub Vec<String>);
    /// User-defined type for `rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynRrecI(pub Vec<String>);
    /// User-defined type for `lrec_i`
    #[derive(Debug, PartialEq)] pub struct SynLrecI(pub Vec<String>);
    /// User-defined type for `amb_i`
    #[derive(Debug, PartialEq)] pub struct SynAmbI(pub LevelString);

    /// User-defined type for `nv_star`
    #[derive(Debug, PartialEq)] pub struct SynNvStar();
    /// User-defined type for `nv_plus`
    #[derive(Debug, PartialEq)] pub struct SynNvPlus();
    /// User-defined type for `nv_l_star`
    #[derive(Debug, PartialEq)] pub struct SynNvLStar();
    /// User-defined type for `<L> "," "*"` iteration in `nv_l_star -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )* ";"`
    #[derive(Debug, PartialEq)] pub struct SynNvLStarI(pub usize);
    /// User-defined type for `nv_l_plus`
    #[derive(Debug, PartialEq)] pub struct SynNvLPlus();
    /// User-defined type for `<L> "," "*"` iteration in `nv_l_plus -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )+ ";"`
    #[derive(Debug, PartialEq)] pub struct SynNvLPlusI(pub Vec<bool>);
    /// User-defined type for `nv_rrec`
    #[derive(Debug, PartialEq)] pub struct SynNvRrec();
    /// User-defined type for `nv_lrec`
    #[derive(Debug, PartialEq)] pub struct SynNvLrec();
    /// User-defined type for `nv_star_a`
    #[derive(Debug, PartialEq)] pub struct SynNvStarA();
    /// User-defined type for `nv_plus_a`
    #[derive(Debug, PartialEq)] pub struct SynNvPlusA();
    /// User-defined type for `nv_l_star_a`
    #[derive(Debug, PartialEq)] pub struct SynNvLStarA();
    /// User-defined type for `<L> "+"` iteration in `nv_l_star_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" ":" Id)* "]" ";"`
    #[derive(Debug, PartialEq)] pub struct SynNvLStarAI(pub Vec<bool>);
    /// User-defined type for `nv_l_plus_a`
    #[derive(Debug, PartialEq)] pub struct SynNvLPlusA();
    /// User-defined type for `<L> "+"` iteration in `nv_l_plus_a -> Id "=" "[" ( ►► <L> "+" ◄◄  | "*" ":" Id)+ "]" ";"`
    #[derive(Debug, PartialEq)] pub struct SynNvLPlusAI(pub Vec<bool>);
    /// User-defined type for `nv_sep_list`
    #[derive(Debug, PartialEq)] pub struct SynNvSepList();
    /// User-defined type for `nv_sep_list_opt`
    #[derive(Debug, PartialEq)] pub struct SynNvSepListOpt();
    /// User-defined type for `nv_l_sep_list`
    #[derive(Debug, PartialEq)] pub struct SynNvLSepList();
    /// User-defined type for `<L> "*" / "," "then"` iteration in `nv_l_sep_list -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";"`
    #[derive(Debug, PartialEq)] pub struct SynNvLSepListI(pub usize);
    /// User-defined type for `nv_l_sep_list_opt`
    #[derive(Debug, PartialEq)] pub struct SynNvLSepListOpt();
    /// User-defined type for `<L> "*" / "," "then"` iteration in `nv_l_sep_list_opt -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";" | Id "=" ";"`
    #[derive(Debug, PartialEq)] pub struct SynNvLSepListOptI(pub usize);
    /// User-defined type for `nv_rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynNvRrecI(pub usize);
    /// User-defined type for `nv_lrec_i`
    #[derive(Debug, PartialEq)] pub struct SynNvLrecI(pub usize);
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

    use lexigram_core::{AltId, LALR, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::{LogMsg, Logger}, parser::{Call, ListenerWrapper, RecoveryNt, Symbol, Terminate, lr::{LRAction::{self, Accept as LRA, Error as LRE, Reduce as LRR, Shift as LRS}, LRParser, LRStateId, WrapperLRErrorRecovery}}};
    use super::listener_types::*;

    static SYMBOLS_T: [(&str, Option<&str>); 31] = [
        ("Add", Some("+")),("Div", Some("/")),("Equal", Some("=")),("Exp", Some("^")),("Lpar", Some("(")),("Lsbracket", Some("[")),("Mul", Some("*")),("Rpar", Some(")")),("Rsbracket", Some("]")),("Sub", Some("-")),
        ("Colon", Some(":")),("Comma", Some(",")),("Semi", Some(";")),("Then", Some("then")),("Star", Some("star")),("Plus", Some("plus")),("L_Star", Some("l-star")),("L_Plus", Some("l-plus")),("Rrec", Some("rrec")),("Lrec", Some("lrec")),
        ("Amb", Some("amb")),("Star_A", Some("star-a")),("Plus_A", Some("plus-a")),("L_Star_A", Some("l-star-a")),("L_Plus_A", Some("l-plus-a")),("SepList", Some("sep-list")),("SepList_Opt", Some("sep-list-opt")),("L_SepList", Some("l-sep-list")),("L_SepList_Opt", Some("l-sep-list-opt")),("Id", None),
        ("Num", None)];

    static NUM_NT: usize = 63;
    static NUM_T_FULL: usize = 32;
    static ACTION: [LRAction; 11328] = [
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(2),LRE,LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRR(2),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(2),LRE,LRS(3),LRS(4),LRS(5),LRS(6),LRS(7),LRS(8),LRS(9),LRS(10),LRS(11),LRS(12),LRS(13),LRS(14),LRS(15),LRS(16),LRS(17),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRE,LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRR(4),LRE,LRE,LRR(4),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(73),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(75),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(77),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(79),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(81),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(83),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(85),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(87),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(89),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(91),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(93),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(95),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(97),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(99),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(101),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(19),LRS(20),LRS(21),LRS(22),LRS(23),
        LRS(24),LRE,LRS(25),LRS(26),LRS(27),LRS(28),LRS(29),LRS(30),LRS(31),LRS(32),LRE,LRE,LRR(0),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(113),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(115),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(119),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(121),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(123),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(125),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(127),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(129),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(131),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(133),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(135),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(137),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(139),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(141),LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(143),LRS(144),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(146),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(148),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(149),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(151),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(153),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(154),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(115),
        LRR(115),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(166),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(24),LRR(24),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(169),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(171),LRS(172),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(143),LRS(144),LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(143),
        LRS(144),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(120),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(120),LRR(120),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(180),LRS(181),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(36),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(36),LRR(36),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(184),LRS(185),LRE,LRS(199),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(201),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(203),LRE,LRE,LRE,LRE,LRE,LRS(204),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(206),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(208),LRE,LRE,LRE,LRE,LRE,LRS(209),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(143),LRS(144),LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(143),LRS(144),LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(143),LRS(144),
        LRE,LRE,LRE,LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(143),LRS(144),LRE,LRE,LRE,
        LRE,LRE,LRS(44),LRE,LRE,LRE,LRE,LRS(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(143),LRS(144),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(130),LRR(130),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(251),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(83),LRR(83),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(254),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(256),LRS(257),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(135),LRE,LRE,LRE,LRE,LRE,LRR(135),LRE,LRR(135),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(262),LRE,LRE,LRE,LRE,LRE,LRS(263),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(94),LRE,LRE,LRE,LRE,LRE,LRR(94),LRE,LRR(94),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(266),LRE,LRE,LRE,LRE,LRE,LRS(267),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(171),LRS(172),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(256),LRS(257),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRA,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(1),LRE,LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRR(1),LRE,LRE,LRE,LRE,LRE,LRS(104),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(5),LRE,LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRR(5),LRE,LRE,LRE,LRE,LRE,LRS(105),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(6),LRE,LRR(6),LRR(6),LRR(6),LRR(6),
        LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRR(6),LRE,LRE,LRE,LRE,LRE,LRS(106),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(7),LRE,LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),
        LRR(7),LRR(7),LRR(7),LRR(7),LRR(7),LRE,LRE,LRE,LRE,LRE,LRS(107),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(8),LRE,LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRR(8),LRE,
        LRE,LRE,LRE,LRE,LRS(108),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(9),LRE,LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRR(9),LRE,LRE,LRE,LRE,LRE,LRS(33),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(10),LRE,LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRR(10),LRE,LRE,LRE,LRE,LRE,LRS(34),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(11),
        LRE,LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRR(11),LRE,LRE,LRE,LRE,LRE,LRS(109),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(12),LRE,LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),
        LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRR(12),LRE,LRE,LRE,LRE,LRE,LRS(110),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(13),LRE,LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),LRR(13),
        LRR(13),LRR(13),LRR(13),LRR(13),LRE,LRE,LRE,LRE,LRE,LRS(111),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(14),LRE,LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRR(14),LRE,LRE,
        LRE,LRE,LRE,LRS(112),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(15),LRE,LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRR(15),LRE,LRE,LRE,LRE,LRE,LRS(35),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(16),LRE,LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRR(16),LRE,LRE,LRE,LRE,LRE,LRS(36),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(17),LRE,
        LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRR(17),LRE,LRE,LRE,LRE,LRE,LRS(37),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(18),LRE,LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),
        LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRR(18),LRE,LRE,LRE,LRE,LRE,LRS(38),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(19),LRE,LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),LRR(19),
        LRR(19),LRR(19),LRR(19),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRE,LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),LRR(3),
        LRE,LRE,LRR(3),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(39),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(40),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(41),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(42),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(43),LRE,LRE,LRE,LRE,LRE,LRE,LRS(46),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(47),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(48),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(49),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(156),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRE,LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),LRR(65),
        LRR(65),LRR(65),LRE,LRE,LRR(65),LRE,LRE,LRS(157),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRE,LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRR(66),LRE,LRE,LRR(66),LRE,
        LRE,LRS(158),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRE,LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRR(67),LRE,LRE,LRR(67),LRE,LRE,LRS(159),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRE,LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRR(68),LRE,LRE,LRR(68),LRE,LRE,LRS(160),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(69),LRR(69),
        LRR(69),LRR(69),LRR(69),LRR(69),LRE,LRR(69),LRR(69),LRR(69),LRR(69),LRR(69),LRR(69),LRR(69),LRR(69),LRE,LRE,LRR(69),LRE,LRE,LRS(50),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRE,LRR(70),
        LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRR(70),LRE,LRE,LRR(70),LRE,LRE,LRS(161),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRE,LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),LRR(71),
        LRR(71),LRE,LRE,LRR(71),LRE,LRE,LRS(162),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRE,LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRR(72),LRE,LRE,LRR(72),LRE,LRE,
        LRS(163),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRE,LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRR(73),LRE,LRE,LRR(73),LRE,LRE,LRS(164),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRE,LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRR(74),LRE,LRE,LRR(74),LRE,LRE,LRS(51),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(75),LRR(75),LRR(75),
        LRR(75),LRR(75),LRR(75),LRE,LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),LRR(75),LRE,LRE,LRR(75),LRE,LRE,LRS(52),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRE,LRR(76),LRR(76),
        LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRR(76),LRE,LRE,LRR(76),LRE,LRE,LRS(53),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRE,LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),LRR(77),
        LRE,LRE,LRR(77),LRE,LRE,LRS(54),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRE,LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRR(78),LRE,LRE,LRR(78),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(55),LRR(55),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(174),LRS(175),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(63),LRR(63),LRE,LRR(63),LRE,LRE,LRR(63),LRR(63),LRE,
        LRR(63),LRE,LRE,LRR(63),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(64),LRR(64),LRE,LRR(64),LRE,LRE,LRR(64),LRR(64),LRE,LRR(64),LRE,LRE,
        LRR(64),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(55),LRS(56),LRE,LRS(57),LRE,LRE,LRS(58),LRE,LRE,LRS(59),LRE,LRE,LRS(178),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(187),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(188),LRS(189),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(44),LRE,LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),
        LRR(44),LRR(44),LRR(44),LRR(44),LRR(44),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(190),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(191),LRS(192),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(193),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(194),LRS(195),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(49),LRE,LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRR(49),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(196),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(197),LRS(198),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(60),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(61),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(62),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(63),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(64),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(65),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(66),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(67),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(68),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(211),LRS(212),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(213),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(214),LRS(215),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(216),LRS(217),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(218),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(219),LRS(220),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(69),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(53),LRE,LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),LRR(53),
        LRR(53),LRR(53),LRR(53),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(28),LRE,LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),LRR(28),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(221),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(29),LRE,LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRR(29),LRE,LRE,LRE,LRS(55),LRS(56),LRE,
        LRS(57),LRE,LRE,LRS(58),LRS(222),LRE,LRS(59),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(56),LRR(56),LRE,LRR(56),LRE,LRE,
        LRR(56),LRR(56),LRE,LRR(56),LRE,LRE,LRR(56),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(30),LRE,LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRR(30),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(228),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(229),LRS(230),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(122),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(122),LRR(122),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(231),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(232),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(233),LRS(234),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(235),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(236),LRS(237),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(39),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(39),LRR(39),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(238),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(239),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(240),LRS(241),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(242),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(243),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRR(42),LRE,LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRR(42),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(244),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(245),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(43),LRE,LRR(43),LRR(43),
        LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRR(43),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(246),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(247),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(45),LRE,LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),LRR(45),
        LRR(45),LRR(45),LRR(45),LRR(45),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRS(248),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(249),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(48),LRE,LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRR(48),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(113),LRR(113),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(259),LRS(260),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(141),LRR(141),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(269),LRS(270),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(143),LRR(143),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(102),LRR(102),LRR(102),
        LRR(102),LRR(102),LRR(102),LRE,LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRR(102),LRE,LRE,LRR(102),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(271),LRS(272),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(105),LRR(105),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(273),LRS(274),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(109),LRR(109),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRE,LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRR(107),LRE,LRE,LRR(107),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(275),LRS(276),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(277),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(20),LRE,LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRR(20),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(117),LRR(117),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(278),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(21),LRE,LRR(21),
        LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRR(21),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(279),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(22),LRE,LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),
        LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRR(22),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(27),LRR(27),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(280),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(25),LRE,LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRR(25),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(54),LRR(54),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(62),
        LRR(62),LRE,LRR(62),LRE,LRE,LRR(62),LRR(62),LRE,LRR(62),LRE,LRE,LRR(62),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(60),LRS(56),LRE,LRS(57),
        LRE,LRE,LRS(58),LRR(60),LRE,LRR(60),LRE,LRE,LRR(60),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(59),LRR(59),LRE,LRS(57),LRE,LRE,LRR(59),
        LRR(59),LRE,LRR(59),LRE,LRE,LRR(59),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(57),LRR(57),LRE,LRS(57),LRE,LRE,LRR(57),LRR(57),LRE,LRR(57),
        LRE,LRE,LRR(57),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(58),LRR(58),LRE,LRS(57),LRE,LRE,LRR(58),LRR(58),LRE,LRR(58),LRE,LRE,LRR(58),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(61),LRS(56),LRE,LRS(57),LRE,LRE,LRS(58),LRR(61),LRE,LRR(61),LRE,LRE,LRR(61),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(282),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(118),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(118),LRR(118),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(283),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(284),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(285),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(121),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(121),LRR(121),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(286),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(287),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRR(34),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(34),LRR(34),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(288),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(289),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(290),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(38),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(38),LRR(38),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(291),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(126),LRR(126),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(292),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(128),LRR(128),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(293),LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(47),LRR(47),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(294),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(51),LRR(51),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(295),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(296),LRS(297),LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(298),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(299),LRS(300),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(301),LRS(302),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(303),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(304),LRS(305),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(70),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRE,LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRR(111),LRE,LRE,LRR(111),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRE,LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRR(87),LRE,LRE,LRR(87),LRE,LRE,LRE,LRE,LRE,LRE,LRS(306),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRE,LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRR(88),LRE,LRE,LRR(88),LRS(307),LRE,LRE,LRE,LRE,LRE,LRS(308),LRE,LRS(309),LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(137),LRE,LRE,LRE,LRE,LRE,LRR(137),LRE,LRR(137),LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(310),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(311),LRE,LRE,LRE,LRE,LRE,LRS(312),LRE,LRS(313),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(314),LRE,LRE,LRE,LRE,LRE,LRS(315),LRE,LRS(316),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(97),LRE,LRE,LRE,LRE,LRE,LRR(97),LRE,LRR(97),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(317),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRS(318),LRE,LRE,LRE,LRE,LRE,LRS(319),LRE,LRS(320),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(321),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRE,LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRR(100),LRE,LRE,LRR(100),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(322),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRE,LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRR(101),LRE,LRE,LRR(101),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(323),
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(103),LRR(103),LRR(103),
        LRR(103),LRR(103),LRR(103),LRE,LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRR(103),LRE,LRE,LRR(103),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(324),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRE,LRR(106),LRR(106),
        LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRR(106),LRE,LRE,LRR(106),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(114),LRR(114),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(116),LRR(116),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(23),LRR(23),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(26),LRR(26),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(52),LRE,LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRR(52),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRR(31),LRE,LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRR(31),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(325),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(124),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(124),LRR(124),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(32),LRE,LRR(32),
        LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRR(32),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(326),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(33),LRE,LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),
        LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRR(33),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRS(327),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(41),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRR(41),LRR(41),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(37),LRE,LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRR(37),LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(328),LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(329),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRS(330),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRS(331),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRS(332),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(333),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(79),LRR(79),
        LRR(79),LRR(79),LRR(79),LRR(79),LRE,LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRR(79),LRE,LRE,LRR(79),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(132),LRR(132),LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRS(334),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRR(80),LRE,LRR(80),LRR(80),LRR(80),LRR(80),
        LRR(80),LRR(80),LRR(80),LRR(80),LRE,LRE,LRR(80),LRE,LRE,LRE,LRE,LRE,LRE,LRS(335),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRE,LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRR(81),LRE,LRE,
        LRR(81),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(86),LRR(86),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRS(336),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
        LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRE,LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRR(84),LRE,LRE,LRR(84),LRE,LRE,LRE,LRE,LRE,LRE,LRE,LRE,
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
    static GOTO: [LRStateId; 4473] = [
        71,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,72,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,74,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,76,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,78,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,80,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,82,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,84,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,86,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,88,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,90,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,92,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,94,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,103,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,114,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,116,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,118,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,122,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,124,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,126,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,130,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,132,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,134,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,136,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,138,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,140,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,142,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,145,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,147,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,150,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,152,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,155,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,165,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,167,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,168,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,170,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,173,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,176,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,177,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,179,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,182,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        183,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,186,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,202,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,205,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,210,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,223,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,225,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,226,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,227,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,250,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,252,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,253,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,258,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,261,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,264,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,265,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,268,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,281,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,337,0,0,0,0,0,0,0,0,0,0,0,0,0];
    static ALT_NT_LEN: [(VarId, u16, u16); 145] = [
        (0, 3, 0),(1, 2, 0),(1, 0, 0),(2, 2, 0),(2, 0, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(3, 2, 0),(4, 5, 2),(5, 5, 2),(6, 5, 2),(7, 3, 1),(7, 0, 0),
        (8, 5, 2),(9, 3, 1),(9, 2, 1),(10, 4, 2),(11, 4, 1),(12, 4, 1),(13, 6, 1),(14, 6, 1),(15, 6, 1),(16, 2, 1),(16, 4, 2),(16, 0, 0),(17, 6, 1),(18, 2, 1),(18, 1, 1),(18, 4, 2),(18, 3, 2),(19, 4, 1),(20, 4, 1),(20, 3, 1),(21, 4, 1),(22, 6, 2),(22, 3, 2),(23, 4, 1),(23, 3, 1),
        (24, 6, 2),(24, 3, 2),(25, 3, 1),(25, 1, 0),(26, 3, 1),(26, 1, 1),(27, 2, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 3, 0),(27, 1, 1),(27, 1, 1),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),
        (28, 2, 0),(28, 2, 0),(28, 2, 0),(28, 2, 0),(29, 5, 1),(30, 5, 1),(31, 5, 1),(32, 3, 0),(32, 0, 0),(33, 5, 1),(34, 3, 0),(34, 2, 0),(35, 4, 1),(36, 4, 1),(37, 6, 1),(38, 6, 1),(39, 6, 1),(40, 2, 0),(40, 3, 0),(40, 0, 0),(41, 6, 1),(42, 2, 0),(42, 1, 0),(42, 3, 0),(42, 2, 0),
        (43, 4, 1),(44, 4, 1),(44, 3, 1),(45, 4, 1),(46, 4, 0),(46, 1, 0),(47, 4, 1),(47, 3, 1),(48, 4, 0),(48, 1, 0),(49, 3, 0),(49, 1, 0),(50, 3, 0),(50, 1, 0),(51, 3, 1),(51, 0, 0),(52, 3, 1),(52, 2, 1),(53, 2, 1),(53, 4, 2),(53, 0, 0),(54, 2, 1),(54, 1, 1),(54, 4, 2),(54, 3, 2),
        (55, 6, 2),(55, 3, 2),(56, 6, 2),(56, 3, 2),(57, 3, 0),(57, 0, 0),(58, 3, 0),(58, 2, 0),(59, 2, 0),(59, 3, 0),(59, 0, 0),(60, 2, 0),(60, 1, 0),(60, 3, 0),(60, 2, 0),(61, 4, 0),(61, 1, 0),(62, 4, 0),(62, 1, 0),(63, 1, 0)];
    static SYMBOLS_NT: [&str; 64] = [
        "text","i","nv_i","example","star","plus","l_star","l_star_i","l_plus","l_plus_i","rrec","lrec","amb","star_a","plus_a","l_star_a","l_star_a_i","l_plus_a","l_plus_a_i","sep_list",
        "sep_list_opt","l_sep_list","l_sep_list_i","l_sep_list_opt","l_sep_list_opt_i","rrec_i","lrec_i","amb_i","nv_example","nv_star","nv_plus","nv_l_star","nv_l_star_i","nv_l_plus","nv_l_plus_i","nv_rrec","nv_lrec","nv_star_a","nv_plus_a","nv_l_star_a",
        "nv_l_star_a_i","nv_l_plus_a","nv_l_plus_a_i","nv_sep_list","nv_sep_list_opt","nv_l_sep_list","nv_l_sep_list_i","nv_l_sep_list_opt","nv_l_sep_list_opt_i","nv_rrec_i","nv_lrec_i","star_1","plus_1","star_a_1","plus_a_1","sep_list_1","sep_list_opt_1","nv_star_1","nv_plus_1","nv_star_a_1",
        "nv_plus_a_1","nv_sep_list_1","nv_sep_list_opt_1","<goal>"];

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum Term {
        #[doc = "'+'"]              Add = 0,
        #[doc = "'/'"]              Div = 1,
        #[doc = "'='"]              Equal = 2,
        #[doc = "'^'"]              Exp = 3,
        #[doc = "'('"]              Lpar = 4,
        #[doc = "'['"]              Lsbracket = 5,
        #[doc = "'*'"]              Mul = 6,
        #[doc = "')'"]              Rpar = 7,
        #[doc = "']'"]              Rsbracket = 8,
        #[doc = "'-'"]              Sub = 9,
        #[doc = "':'"]              Colon = 10,
        #[doc = "','"]              Comma = 11,
        #[doc = "';'"]              Semi = 12,
        #[doc = "'then'"]           Then = 13,
        #[doc = "'star'"]           Star = 14,
        #[doc = "'plus'"]           Plus = 15,
        #[doc = "'l-star'"]         L_Star = 16,
        #[doc = "'l-plus'"]         L_Plus = 17,
        #[doc = "'rrec'"]           Rrec = 18,
        #[doc = "'lrec'"]           Lrec = 19,
        #[doc = "'amb'"]            Amb = 20,
        #[doc = "'star-a'"]         Star_A = 21,
        #[doc = "'plus-a'"]         Plus_A = 22,
        #[doc = "'l-star-a'"]       L_Star_A = 23,
        #[doc = "'l-plus-a'"]       L_Plus_A = 24,
        #[doc = "'sep-list'"]       SepList = 25,
        #[doc = "'sep-list-opt'"]   SepList_Opt = 26,
        #[doc = "'l-sep-list'"]     L_SepList = 27,
        #[doc = "'l-sep-list-opt'"] L_SepList_Opt = 28,
        #[doc = "(variable)"]       Id = 29,
        #[doc = "(variable)"]       Num = 30,
    }

    // Unfortunately, Rust has no way to safely convert to enum constants...
    impl From<TokenId> for Term {
        fn from(value: TokenId) -> Self {
            match value {
                _ if value == Term::Add as TokenId => Term::Add,
                _ if value == Term::Div as TokenId => Term::Div,
                _ if value == Term::Equal as TokenId => Term::Equal,
                _ if value == Term::Exp as TokenId => Term::Exp,
                _ if value == Term::Lpar as TokenId => Term::Lpar,
                _ if value == Term::Lsbracket as TokenId => Term::Lsbracket,
                _ if value == Term::Mul as TokenId => Term::Mul,
                _ if value == Term::Rpar as TokenId => Term::Rpar,
                _ if value == Term::Rsbracket as TokenId => Term::Rsbracket,
                _ if value == Term::Sub as TokenId => Term::Sub,
                _ if value == Term::Colon as TokenId => Term::Colon,
                _ if value == Term::Comma as TokenId => Term::Comma,
                _ if value == Term::Semi as TokenId => Term::Semi,
                _ if value == Term::Then as TokenId => Term::Then,
                _ if value == Term::Star as TokenId => Term::Star,
                _ if value == Term::Plus as TokenId => Term::Plus,
                _ if value == Term::L_Star as TokenId => Term::L_Star,
                _ if value == Term::L_Plus as TokenId => Term::L_Plus,
                _ if value == Term::Rrec as TokenId => Term::Rrec,
                _ if value == Term::Lrec as TokenId => Term::Lrec,
                _ if value == Term::Amb as TokenId => Term::Amb,
                _ if value == Term::Star_A as TokenId => Term::Star_A,
                _ if value == Term::Plus_A as TokenId => Term::Plus_A,
                _ if value == Term::L_Star_A as TokenId => Term::L_Star_A,
                _ if value == Term::L_Plus_A as TokenId => Term::L_Plus_A,
                _ if value == Term::SepList as TokenId => Term::SepList,
                _ if value == Term::SepList_Opt as TokenId => Term::SepList_Opt,
                _ if value == Term::L_SepList as TokenId => Term::L_SepList,
                _ if value == Term::L_SepList_Opt as TokenId => Term::L_SepList_Opt,
                _ if value == Term::Id as TokenId => Term::Id,
                _ if value == Term::Num as TokenId => Term::Num,
                _ => panic!("cannot convert terminal index #{value} to Term"),
            }
        }
    }

    #[derive(Clone, Copy, PartialEq, Debug)]
    #[repr(u16)]
    pub enum NTerm {
        #[doc = "`text`"]                                             Text = 0,
        #[doc = "`i`, parent: `text`"]                                I = 1,
        #[doc = "`nv_i`, parent: `text`"]                             NvI = 2,
        #[doc = "`example`"]                                          Example = 3,
        #[doc = "`star`"]                                             Star = 4,
        #[doc = "`plus`"]                                             Plus = 5,
        #[doc = "`l_star`"]                                           LStar = 6,
        #[doc = "`l_star_i`, parent: `l_star`"]                       LStarI = 7,
        #[doc = "`l_plus`"]                                           LPlus = 8,
        #[doc = "`l_plus_i`, parent: `l_plus`"]                       LPlusI = 9,
        #[doc = "`rrec`"]                                             Rrec = 10,
        #[doc = "`lrec`"]                                             Lrec = 11,
        #[doc = "`amb`"]                                              Amb = 12,
        #[doc = "`star_a`"]                                           StarA = 13,
        #[doc = "`plus_a`"]                                           PlusA = 14,
        #[doc = "`l_star_a`"]                                         LStarA = 15,
        #[doc = "`l_star_a_i`, parent: `l_star_a`"]                   LStarAI = 16,
        #[doc = "`l_plus_a`"]                                         LPlusA = 17,
        #[doc = "`l_plus_a_i`, parent: `l_plus_a`"]                   LPlusAI = 18,
        #[doc = "`sep_list`"]                                         SepList = 19,
        #[doc = "`sep_list_opt`"]                                     SepListOpt = 20,
        #[doc = "`l_sep_list`"]                                       LSepList = 21,
        #[doc = "`l_sep_list_i`, parent: `l_sep_list`"]               LSepListI = 22,
        #[doc = "`l_sep_list_opt`"]                                   LSepListOpt = 23,
        #[doc = "`l_sep_list_opt_i`, parent: `l_sep_list_opt`"]       LSepListOptI = 24,
        #[doc = "`rrec_i`"]                                           RrecI = 25,
        #[doc = "`lrec_i`"]                                           LrecI = 26,
        #[doc = "`amb_i`"]                                            AmbI = 27,
        #[doc = "`nv_example`"]                                       NvExample = 28,
        #[doc = "`nv_star`"]                                          NvStar = 29,
        #[doc = "`nv_plus`"]                                          NvPlus = 30,
        #[doc = "`nv_l_star`"]                                        NvLStar = 31,
        #[doc = "`nv_l_star_i`, parent: `nv_l_star`"]                 NvLStarI = 32,
        #[doc = "`nv_l_plus`"]                                        NvLPlus = 33,
        #[doc = "`nv_l_plus_i`, parent: `nv_l_plus`"]                 NvLPlusI = 34,
        #[doc = "`nv_rrec`"]                                          NvRrec = 35,
        #[doc = "`nv_lrec`"]                                          NvLrec = 36,
        #[doc = "`nv_star_a`"]                                        NvStarA = 37,
        #[doc = "`nv_plus_a`"]                                        NvPlusA = 38,
        #[doc = "`nv_l_star_a`"]                                      NvLStarA = 39,
        #[doc = "`nv_l_star_a_i`, parent: `nv_l_star_a`"]             NvLStarAI = 40,
        #[doc = "`nv_l_plus_a`"]                                      NvLPlusA = 41,
        #[doc = "`nv_l_plus_a_i`, parent: `nv_l_plus_a`"]             NvLPlusAI = 42,
        #[doc = "`nv_sep_list`"]                                      NvSepList = 43,
        #[doc = "`nv_sep_list_opt`"]                                  NvSepListOpt = 44,
        #[doc = "`nv_l_sep_list`"]                                    NvLSepList = 45,
        #[doc = "`nv_l_sep_list_i`, parent: `nv_l_sep_list`"]         NvLSepListI = 46,
        #[doc = "`nv_l_sep_list_opt`"]                                NvLSepListOpt = 47,
        #[doc = "`nv_l_sep_list_opt_i`, parent: `nv_l_sep_list_opt`"] NvLSepListOptI = 48,
        #[doc = "`nv_rrec_i`"]                                        NvRrecI = 49,
        #[doc = "`nv_lrec_i`"]                                        NvLrecI = 50,
        #[doc = "`star_1`, parent: `star`"]                           Star1 = 51,
        #[doc = "`plus_1`, parent: `plus`"]                           Plus1 = 52,
        #[doc = "`star_a_1`, parent: `star_a`"]                       StarA1 = 53,
        #[doc = "`plus_a_1`, parent: `plus_a`"]                       PlusA1 = 54,
        #[doc = "`sep_list_1`, parent: `sep_list`"]                   SepList1 = 55,
        #[doc = "`sep_list_opt_1`, parent: `sep_list_opt`"]           SepListOpt1 = 56,
        #[doc = "`nv_star_1`, parent: `nv_star`"]                     NvStar1 = 57,
        #[doc = "`nv_plus_1`, parent: `nv_plus`"]                     NvPlus1 = 58,
        #[doc = "`nv_star_a_1`, parent: `nv_star_a`"]                 NvStarA1 = 59,
        #[doc = "`nv_plus_a_1`, parent: `nv_plus_a`"]                 NvPlusA1 = 60,
        #[doc = "`nv_sep_list_1`, parent: `nv_sep_list`"]             NvSepList1 = 61,
        #[doc = "`nv_sep_list_opt_1`, parent: `nv_sep_list_opt`"]     NvSepListOpt1 = 62,
    }

    impl TryFrom<TokenId> for NTerm {
        type Error = String;
        fn try_from(value: VarId) -> Result<Self, Self::Error> {
            match value {
                _ if value == NTerm::Text as VarId => Ok(NTerm::Text),
                _ if value == NTerm::I as VarId => Ok(NTerm::I),
                _ if value == NTerm::NvI as VarId => Ok(NTerm::NvI),
                _ if value == NTerm::Example as VarId => Ok(NTerm::Example),
                _ if value == NTerm::Star as VarId => Ok(NTerm::Star),
                _ if value == NTerm::Plus as VarId => Ok(NTerm::Plus),
                _ if value == NTerm::LStar as VarId => Ok(NTerm::LStar),
                _ if value == NTerm::LStarI as VarId => Ok(NTerm::LStarI),
                _ if value == NTerm::LPlus as VarId => Ok(NTerm::LPlus),
                _ if value == NTerm::LPlusI as VarId => Ok(NTerm::LPlusI),
                _ if value == NTerm::Rrec as VarId => Ok(NTerm::Rrec),
                _ if value == NTerm::Lrec as VarId => Ok(NTerm::Lrec),
                _ if value == NTerm::Amb as VarId => Ok(NTerm::Amb),
                _ if value == NTerm::StarA as VarId => Ok(NTerm::StarA),
                _ if value == NTerm::PlusA as VarId => Ok(NTerm::PlusA),
                _ if value == NTerm::LStarA as VarId => Ok(NTerm::LStarA),
                _ if value == NTerm::LStarAI as VarId => Ok(NTerm::LStarAI),
                _ if value == NTerm::LPlusA as VarId => Ok(NTerm::LPlusA),
                _ if value == NTerm::LPlusAI as VarId => Ok(NTerm::LPlusAI),
                _ if value == NTerm::SepList as VarId => Ok(NTerm::SepList),
                _ if value == NTerm::SepListOpt as VarId => Ok(NTerm::SepListOpt),
                _ if value == NTerm::LSepList as VarId => Ok(NTerm::LSepList),
                _ if value == NTerm::LSepListI as VarId => Ok(NTerm::LSepListI),
                _ if value == NTerm::LSepListOpt as VarId => Ok(NTerm::LSepListOpt),
                _ if value == NTerm::LSepListOptI as VarId => Ok(NTerm::LSepListOptI),
                _ if value == NTerm::RrecI as VarId => Ok(NTerm::RrecI),
                _ if value == NTerm::LrecI as VarId => Ok(NTerm::LrecI),
                _ if value == NTerm::AmbI as VarId => Ok(NTerm::AmbI),
                _ if value == NTerm::NvExample as VarId => Ok(NTerm::NvExample),
                _ if value == NTerm::NvStar as VarId => Ok(NTerm::NvStar),
                _ if value == NTerm::NvPlus as VarId => Ok(NTerm::NvPlus),
                _ if value == NTerm::NvLStar as VarId => Ok(NTerm::NvLStar),
                _ if value == NTerm::NvLStarI as VarId => Ok(NTerm::NvLStarI),
                _ if value == NTerm::NvLPlus as VarId => Ok(NTerm::NvLPlus),
                _ if value == NTerm::NvLPlusI as VarId => Ok(NTerm::NvLPlusI),
                _ if value == NTerm::NvRrec as VarId => Ok(NTerm::NvRrec),
                _ if value == NTerm::NvLrec as VarId => Ok(NTerm::NvLrec),
                _ if value == NTerm::NvStarA as VarId => Ok(NTerm::NvStarA),
                _ if value == NTerm::NvPlusA as VarId => Ok(NTerm::NvPlusA),
                _ if value == NTerm::NvLStarA as VarId => Ok(NTerm::NvLStarA),
                _ if value == NTerm::NvLStarAI as VarId => Ok(NTerm::NvLStarAI),
                _ if value == NTerm::NvLPlusA as VarId => Ok(NTerm::NvLPlusA),
                _ if value == NTerm::NvLPlusAI as VarId => Ok(NTerm::NvLPlusAI),
                _ if value == NTerm::NvSepList as VarId => Ok(NTerm::NvSepList),
                _ if value == NTerm::NvSepListOpt as VarId => Ok(NTerm::NvSepListOpt),
                _ if value == NTerm::NvLSepList as VarId => Ok(NTerm::NvLSepList),
                _ if value == NTerm::NvLSepListI as VarId => Ok(NTerm::NvLSepListI),
                _ if value == NTerm::NvLSepListOpt as VarId => Ok(NTerm::NvLSepListOpt),
                _ if value == NTerm::NvLSepListOptI as VarId => Ok(NTerm::NvLSepListOptI),
                _ if value == NTerm::NvRrecI as VarId => Ok(NTerm::NvRrecI),
                _ if value == NTerm::NvLrecI as VarId => Ok(NTerm::NvLrecI),
                _ if value == NTerm::Star1 as VarId => Ok(NTerm::Star1),
                _ if value == NTerm::Plus1 as VarId => Ok(NTerm::Plus1),
                _ if value == NTerm::StarA1 as VarId => Ok(NTerm::StarA1),
                _ if value == NTerm::PlusA1 as VarId => Ok(NTerm::PlusA1),
                _ if value == NTerm::SepList1 as VarId => Ok(NTerm::SepList1),
                _ if value == NTerm::SepListOpt1 as VarId => Ok(NTerm::SepListOpt1),
                _ if value == NTerm::NvStar1 as VarId => Ok(NTerm::NvStar1),
                _ if value == NTerm::NvPlus1 as VarId => Ok(NTerm::NvPlus1),
                _ if value == NTerm::NvStarA1 as VarId => Ok(NTerm::NvStarA1),
                _ if value == NTerm::NvPlusA1 as VarId => Ok(NTerm::NvPlusA1),
                _ if value == NTerm::NvSepList1 as VarId => Ok(NTerm::NvSepList1),
                _ if value == NTerm::NvSepListOpt1 as VarId => Ok(NTerm::NvSepListOpt1),
                _ => Err(format!("cannot convert nonterminal index #{value} to NTerm")),
            }
        }
    }

    pub fn get_term_name(t: TokenId) -> (&'static str, Option<&'static str>) {
        SYMBOLS_T[t as usize]
    }

    pub fn build_parser() -> LRParser<'static, LALR> {
        LRParser::new(
            NUM_NT, NUM_T_FULL, &ACTION, &GOTO, &ALT_NT_LEN,
            FixedSymTable::new(
                SYMBOLS_T.into_iter().map(|(t, v)| (t.to_string(), v.map(|s| s.to_string()))).collect(),
                SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
            ),
            false
        )
    }

    static NT_VALUE: [bool; 64] = [
        true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,
        true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,
        true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,
        false,false,false,true];
    static STATE_SYMBOL: [Symbol; 354] = [
        Symbol::Empty,Symbol::NT(1),Symbol::T(12),Symbol::T(14),Symbol::T(15),Symbol::T(16),Symbol::T(17),Symbol::T(18),Symbol::T(19),Symbol::T(20),Symbol::T(21),Symbol::T(22),Symbol::T(23),Symbol::T(24),Symbol::T(25),Symbol::T(26),Symbol::T(27),Symbol::T(28),Symbol::NT(2),Symbol::T(14),Symbol::T(15),Symbol::T(16),Symbol::T(17),Symbol::T(18),Symbol::T(19),
        Symbol::T(21),Symbol::T(22),Symbol::T(23),Symbol::T(24),Symbol::T(25),Symbol::T(26),Symbol::T(27),Symbol::T(28),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(29),Symbol::T(30),Symbol::T(30),Symbol::T(30),Symbol::T(30),Symbol::T(4),Symbol::T(9),Symbol::T(5),Symbol::T(5),Symbol::T(5),Symbol::T(5),
        Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(0),Symbol::T(1),Symbol::T(3),Symbol::T(6),Symbol::T(9),Symbol::T(0),Symbol::T(0),Symbol::T(0),Symbol::T(0),Symbol::T(0),Symbol::T(5),Symbol::T(5),Symbol::T(5),Symbol::T(5),Symbol::T(30),Symbol::T(6),Symbol::NT(0),Symbol::NT(3),Symbol::T(29),Symbol::NT(4),
        Symbol::T(29),Symbol::NT(5),Symbol::T(29),Symbol::NT(6),Symbol::T(29),Symbol::NT(8),Symbol::T(29),Symbol::NT(10),Symbol::T(29),Symbol::NT(11),Symbol::T(29),Symbol::NT(12),Symbol::T(29),Symbol::NT(13),Symbol::T(29),Symbol::NT(14),Symbol::T(29),Symbol::NT(15),Symbol::T(29),Symbol::NT(17),Symbol::T(29),Symbol::NT(19),Symbol::T(29),Symbol::NT(20),Symbol::T(29),
        Symbol::NT(21),Symbol::T(29),Symbol::NT(23),Symbol::NT(28),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(29),Symbol::NT(29),Symbol::T(29),Symbol::NT(30),Symbol::T(29),Symbol::NT(31),Symbol::T(29),Symbol::NT(33),Symbol::T(29),Symbol::NT(35),Symbol::T(29),Symbol::NT(36),
        Symbol::T(29),Symbol::NT(37),Symbol::T(29),Symbol::NT(38),Symbol::T(29),Symbol::NT(39),Symbol::T(29),Symbol::NT(41),Symbol::T(29),Symbol::NT(43),Symbol::T(29),Symbol::NT(44),Symbol::T(29),Symbol::NT(45),Symbol::T(29),Symbol::NT(47),Symbol::T(30),Symbol::NT(26),Symbol::T(29),Symbol::T(30),Symbol::NT(27),Symbol::T(29),Symbol::NT(55),Symbol::T(12),Symbol::T(29),
        Symbol::NT(56),Symbol::T(29),Symbol::NT(22),Symbol::T(12),Symbol::T(29),Symbol::NT(24),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::T(2),Symbol::NT(51),Symbol::T(11),Symbol::NT(52),Symbol::NT(7),Symbol::T(11),Symbol::NT(9),Symbol::T(11),Symbol::T(12),Symbol::NT(25),Symbol::T(11),
        Symbol::T(12),Symbol::NT(27),Symbol::NT(27),Symbol::T(12),Symbol::NT(53),Symbol::T(29),Symbol::T(30),Symbol::NT(54),Symbol::NT(16),Symbol::T(29),Symbol::T(30),Symbol::NT(18),Symbol::T(10),Symbol::T(11),Symbol::T(12),Symbol::T(10),Symbol::T(11),Symbol::T(12),Symbol::T(10),Symbol::T(11),Symbol::T(12),Symbol::T(10),Symbol::T(11),Symbol::T(12),Symbol::T(0),
        Symbol::NT(50),Symbol::T(6),Symbol::NT(61),Symbol::T(6),Symbol::T(12),Symbol::NT(62),Symbol::T(6),Symbol::NT(46),Symbol::T(6),Symbol::T(12),Symbol::NT(48),Symbol::T(11),Symbol::T(12),Symbol::T(30),Symbol::T(11),Symbol::T(12),Symbol::T(11),Symbol::T(12),Symbol::T(30),Symbol::T(11),Symbol::T(12),Symbol::T(30),Symbol::T(7),Symbol::NT(27),Symbol::NT(27),
        Symbol::NT(27),Symbol::NT(27),Symbol::NT(27),Symbol::T(8),Symbol::T(29),Symbol::T(30),Symbol::T(10),Symbol::T(8),Symbol::T(29),Symbol::T(30),Symbol::T(8),Symbol::T(29),Symbol::T(30),Symbol::T(10),Symbol::T(8),Symbol::T(29),Symbol::T(30),Symbol::T(30),Symbol::T(13),Symbol::T(30),Symbol::T(13),Symbol::T(30),Symbol::T(13),Symbol::T(30),Symbol::T(13),
        Symbol::NT(57),Symbol::T(11),Symbol::NT(58),Symbol::NT(32),Symbol::T(11),Symbol::NT(34),Symbol::T(11),Symbol::T(12),Symbol::NT(49),Symbol::T(11),Symbol::T(12),Symbol::NT(59),Symbol::T(0),Symbol::T(6),Symbol::NT(60),Symbol::NT(40),Symbol::T(0),Symbol::T(6),Symbol::NT(42),Symbol::T(11),Symbol::T(12),Symbol::T(11),Symbol::T(12),Symbol::T(11),Symbol::T(12),
        Symbol::T(11),Symbol::T(12),Symbol::T(30),Symbol::T(30),Symbol::T(30),Symbol::T(30),Symbol::NT(25),Symbol::T(12),Symbol::T(10),Symbol::T(29),Symbol::T(12),Symbol::T(10),Symbol::T(12),Symbol::T(10),Symbol::T(29),Symbol::T(12),Symbol::T(10),Symbol::T(29),Symbol::T(29),Symbol::T(29),Symbol::T(29),Symbol::T(11),Symbol::T(12),Symbol::T(6),Symbol::T(11),
        Symbol::T(12),Symbol::T(11),Symbol::T(12),Symbol::T(6),Symbol::T(11),Symbol::T(12),Symbol::T(6),Symbol::T(0),Symbol::T(6),Symbol::T(8),Symbol::T(9),Symbol::T(0),Symbol::T(6),Symbol::T(8),Symbol::T(0),Symbol::T(6),Symbol::T(8),Symbol::T(9),Symbol::T(0),Symbol::T(6),Symbol::T(8),Symbol::T(13),Symbol::T(13),Symbol::T(13),Symbol::T(13),
        Symbol::T(29),Symbol::T(29),Symbol::T(29),Symbol::T(29),Symbol::T(10),Symbol::T(10),Symbol::T(10),Symbol::T(10),Symbol::T(6),Symbol::T(6),Symbol::T(6),Symbol::T(6),Symbol::NT(49),Symbol::T(9),Symbol::T(12),Symbol::T(9),Symbol::T(12),Symbol::T(9),Symbol::T(12),Symbol::T(9),Symbol::T(12),Symbol::T(6),Symbol::T(6),Symbol::T(6),Symbol::T(6),
        Symbol::T(30),Symbol::T(30),Symbol::T(30),Symbol::T(30)];

    #[derive(Debug)]
    pub enum CtxText {
        /// `text -> (<L> example)* ";" (<L> nv_example)*`
        V1 { star: SynI, star1: SynNvI },
    }
    #[derive(Debug)]
    pub enum CtxI {
        /// `<L> example` iteration in `text -> ( ►► <L> example ◄◄ )* ";" (<L> nv_example)*`
        V1 { example: SynExample },
    }
    #[derive(Debug)]
    pub enum CtxNvI {
        /// `<L> nv_example` iteration in `text -> (<L> example)* ";" ( ►► <L> nv_example ◄◄ )*`
        V1 { nv_example: SynNvExample },
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
        /// `example -> "lrec" lrec`
        V6 { lrec: SynLrec },
        /// `example -> "amb" amb`
        V7 { amb: SynAmb },
        /// `example -> "star-a" star_a`
        V8 { star_a: SynStarA },
        /// `example -> "plus-a" plus_a`
        V9 { plus_a: SynPlusA },
        /// `example -> "l-star-a" l_star_a`
        V10 { l_star_a: SynLStarA },
        /// `example -> "l-plus-a" l_plus_a`
        V11 { l_plus_a: SynLPlusA },
        /// `example -> "sep-list" sep_list`
        V12 { sep_list: SynSepList },
        /// `example -> "sep-list-opt" sep_list_opt`
        V13 { sep_list_opt: SynSepListOpt },
        /// `example -> "l-sep-list" l_sep_list`
        V14 { l_sep_list: SynLSepList },
        /// `example -> "l-sep-list-opt" l_sep_list_opt`
        V15 { l_sep_list_opt: SynLSepListOpt },
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
        V1 { id: String, num: String, star: SynLStarI },
    }
    #[derive(Debug)]
    pub enum CtxLStarI {
        /// `<L> "," Num` iteration in `l_star -> Id "=" Num ( ►► <L> "," Num ◄◄ )* ";"`
        V1 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxLPlus {
        /// `l_plus -> Id "=" Num (<L> "," Num)+ ";"`
        V1 { id: String, num: String, plus: SynLPlusI },
    }
    #[derive(Debug)]
    pub enum CtxLPlusI {
        /// `<L> "," Num` iteration in `l_plus -> Id "=" Num ( ►► <L> "," Num ◄◄ )+ ";"`
        V1 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxRrec {
        /// `rrec -> Id "=" Num rrec_i`
        V1 { id: String, num: String, rrec_i: SynRrecI },
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
        V1 { id: String, star: SynLStarAI },
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
        V1 { id: String, plus: SynLPlusAI },
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
        V1 { id: String, plus: SynLSepListI },
    }
    #[derive(Debug)]
    pub enum CtxLSepListI {
        /// `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
        V1 { id: String, num: String },
    }
    #[derive(Debug)]
    pub enum CtxLSepListOpt {
        /// `l_sep_list_opt -> Id "=" (<L> Id ":" Num / "," "then")+ ";"`
        V1 { id: String, plus: SynLSepListOptI },
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
        V1 { num: String, rrec_i: SynRrecI },
        /// `rrec_i -> ";"`
        V2,
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
        /// `amb_i -> "-" amb_i`
        V1 { amb_i: SynAmbI },
        /// `amb_i -> <R> amb_i "^" amb_i`
        V2 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i "*" amb_i`
        V3 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i <P> "/" amb_i`
        V4 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i "+" amb_i`
        V5 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> amb_i <P> "-" amb_i`
        V6 { amb_i: [SynAmbI; 2] },
        /// `amb_i -> "(" amb_i ")"`
        V7 { amb_i: SynAmbI },
        /// `amb_i -> Id`
        V8 { id: String },
        /// `amb_i -> Num`
        V9 { num: String },
    }
    #[derive(Debug)]
    pub enum CtxNvExample {
        /// `nv_example -> "star" nv_star`
        V1 { nv_star: SynNvStar },
        /// `nv_example -> "plus" nv_plus`
        V2 { nv_plus: SynNvPlus },
        /// `nv_example -> "l-star" nv_l_star`
        V3 { nv_l_star: SynNvLStar },
        /// `nv_example -> "l-plus" nv_l_plus`
        V4 { nv_l_plus: SynNvLPlus },
        /// `nv_example -> "rrec" nv_rrec`
        V5 { nv_rrec: SynNvRrec },
        /// `nv_example -> "lrec" nv_lrec`
        V6 { nv_lrec: SynNvLrec },
        /// `nv_example -> "star-a" nv_star_a`
        V7 { nv_star_a: SynNvStarA },
        /// `nv_example -> "plus-a" nv_plus_a`
        V8 { nv_plus_a: SynNvPlusA },
        /// `nv_example -> "l-star-a" nv_l_star_a`
        V9 { nv_l_star_a: SynNvLStarA },
        /// `nv_example -> "l-plus-a" nv_l_plus_a`
        V10 { nv_l_plus_a: SynNvLPlusA },
        /// `nv_example -> "sep-list" nv_sep_list`
        V11 { nv_sep_list: SynNvSepList },
        /// `nv_example -> "sep-list-opt" nv_sep_list_opt`
        V12 { nv_sep_list_opt: SynNvSepListOpt },
        /// `nv_example -> "l-sep-list" nv_l_sep_list`
        V13 { nv_l_sep_list: SynNvLSepList },
        /// `nv_example -> "l-sep-list-opt" nv_l_sep_list_opt`
        V14 { nv_l_sep_list_opt: SynNvLSepListOpt },
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
        V1 { id: String, star: SynNvLStarI },
    }
    #[derive(Debug)]
    pub enum CtxNvLStarI {
        /// `<L> "," "*"` iteration in `nv_l_star -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )* ";"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvLPlus {
        /// `nv_l_plus -> Id "=" "+" (<L> "," "*")+ ";"`
        V1 { id: String, plus: SynNvLPlusI },
    }
    #[derive(Debug)]
    pub enum CtxNvLPlusI {
        /// `<L> "," "*"` iteration in `nv_l_plus -> Id "=" "+" ( ►► <L> "," "*" ◄◄ )+ ";"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvRrec {
        /// `nv_rrec -> Id "=" "+" nv_rrec_i`
        V1 { id: String, nv_rrec_i: SynNvRrecI },
    }
    #[derive(Debug)]
    pub enum CtxNvLrec {
        /// `nv_lrec -> Id "=" nv_lrec_i ";"`
        V1 { id: String, nv_lrec_i: SynNvLrecI },
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
        V1 { id: String, star: SynNvLStarAI },
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
        V1 { id: String, plus: SynNvLPlusAI },
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
        V1 { id: String, plus: SynNvLSepListI },
    }
    #[derive(Debug)]
    pub enum CtxNvLSepListI {
        /// `<L> "*" / "," "then"` iteration in `nv_l_sep_list -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";"`
        V1,
    }
    #[derive(Debug)]
    pub enum CtxNvLSepListOpt {
        /// `nv_l_sep_list_opt -> Id "=" (<L> "*" / "," "then")+ ";"`
        V1 { id: String, plus: SynNvLSepListOptI },
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
        V1 { nv_rrec_i: SynNvRrecI },
        /// `nv_rrec_i -> ";"`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxNvLrecI {
        /// `nv_lrec_i -> nv_lrec_i "," "*"`
        V1 { nv_lrec_i: SynNvLrecI },
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

    #[derive(Debug)]
    pub enum EnumSynValue { Text(SynText), I(SynI), NvI(SynNvI), Example(SynExample), Star(SynStar), Plus(SynPlus), LStar(SynLStar), LStarI(SynLStarI), LPlus(SynLPlus), LPlusI(SynLPlusI), Rrec(SynRrec), Lrec(SynLrec), Amb(SynAmb), StarA(SynStarA), PlusA(SynPlusA), LStarA(SynLStarA), LStarAI(SynLStarAI), LPlusA(SynLPlusA), LPlusAI(SynLPlusAI), SepList(SynSepList), SepListOpt(SynSepListOpt), LSepList(SynLSepList), LSepListI(SynLSepListI), LSepListOpt(SynLSepListOpt), LSepListOptI(SynLSepListOptI), RrecI(SynRrecI), LrecI(SynLrecI), AmbI(SynAmbI), NvExample(SynNvExample), NvStar(SynNvStar), NvPlus(SynNvPlus), NvLStar(SynNvLStar), NvLStarI(SynNvLStarI), NvLPlus(SynNvLPlus), NvLPlusI(SynNvLPlusI), NvRrec(SynNvRrec), NvLrec(SynNvLrec), NvStarA(SynNvStarA), NvPlusA(SynNvPlusA), NvLStarA(SynNvLStarA), NvLStarAI(SynNvLStarAI), NvLPlusA(SynNvLPlusA), NvLPlusAI(SynNvLPlusAI), NvSepList(SynNvSepList), NvSepListOpt(SynNvSepListOpt), NvLSepList(SynNvLSepList), NvLSepListI(SynNvLSepListI), NvLSepListOpt(SynNvLSepListOpt), NvLSepListOptI(SynNvLSepListOptI), NvRrecI(SynNvRrecI), NvLrecI(SynNvLrecI), Star1(SynStar1), Plus1(SynPlus1), StarA1(SynStarA1), PlusA1(SynPlusA1), SepList1(SynSepList1), SepListOpt1(SynSepListOpt1) }

    impl EnumSynValue {
        fn get_text(self) -> SynText {
            if let EnumSynValue::Text(val) = self { val } else { panic!() }
        }
        fn get_i(self) -> SynI {
            if let EnumSynValue::I(val) = self { val } else { panic!() }
        }
        fn get_nv_i(self) -> SynNvI {
            if let EnumSynValue::NvI(val) = self { val } else { panic!() }
        }
        fn get_example(self) -> SynExample {
            if let EnumSynValue::Example(val) = self { val } else { panic!() }
        }
        fn get_star(self) -> SynStar {
            if let EnumSynValue::Star(val) = self { val } else { panic!() }
        }
        fn get_plus(self) -> SynPlus {
            if let EnumSynValue::Plus(val) = self { val } else { panic!() }
        }
        fn get_l_star(self) -> SynLStar {
            if let EnumSynValue::LStar(val) = self { val } else { panic!() }
        }
        fn get_l_star_i(self) -> SynLStarI {
            if let EnumSynValue::LStarI(val) = self { val } else { panic!() }
        }
        fn get_l_plus(self) -> SynLPlus {
            if let EnumSynValue::LPlus(val) = self { val } else { panic!() }
        }
        fn get_l_plus_i(self) -> SynLPlusI {
            if let EnumSynValue::LPlusI(val) = self { val } else { panic!() }
        }
        fn get_rrec(self) -> SynRrec {
            if let EnumSynValue::Rrec(val) = self { val } else { panic!() }
        }
        fn get_lrec(self) -> SynLrec {
            if let EnumSynValue::Lrec(val) = self { val } else { panic!() }
        }
        fn get_amb(self) -> SynAmb {
            if let EnumSynValue::Amb(val) = self { val } else { panic!() }
        }
        fn get_star_a(self) -> SynStarA {
            if let EnumSynValue::StarA(val) = self { val } else { panic!() }
        }
        fn get_plus_a(self) -> SynPlusA {
            if let EnumSynValue::PlusA(val) = self { val } else { panic!() }
        }
        fn get_l_star_a(self) -> SynLStarA {
            if let EnumSynValue::LStarA(val) = self { val } else { panic!() }
        }
        fn get_l_star_a_i(self) -> SynLStarAI {
            if let EnumSynValue::LStarAI(val) = self { val } else { panic!() }
        }
        fn get_l_plus_a(self) -> SynLPlusA {
            if let EnumSynValue::LPlusA(val) = self { val } else { panic!() }
        }
        fn get_l_plus_a_i(self) -> SynLPlusAI {
            if let EnumSynValue::LPlusAI(val) = self { val } else { panic!() }
        }
        fn get_sep_list(self) -> SynSepList {
            if let EnumSynValue::SepList(val) = self { val } else { panic!() }
        }
        fn get_sep_list_opt(self) -> SynSepListOpt {
            if let EnumSynValue::SepListOpt(val) = self { val } else { panic!() }
        }
        fn get_l_sep_list(self) -> SynLSepList {
            if let EnumSynValue::LSepList(val) = self { val } else { panic!() }
        }
        fn get_l_sep_list_i(self) -> SynLSepListI {
            if let EnumSynValue::LSepListI(val) = self { val } else { panic!() }
        }
        fn get_l_sep_list_opt(self) -> SynLSepListOpt {
            if let EnumSynValue::LSepListOpt(val) = self { val } else { panic!() }
        }
        fn get_l_sep_list_opt_i(self) -> SynLSepListOptI {
            if let EnumSynValue::LSepListOptI(val) = self { val } else { panic!() }
        }
        fn get_rrec_i(self) -> SynRrecI {
            if let EnumSynValue::RrecI(val) = self { val } else { panic!() }
        }
        fn get_lrec_i(self) -> SynLrecI {
            if let EnumSynValue::LrecI(val) = self { val } else { panic!() }
        }
        fn get_amb_i(self) -> SynAmbI {
            if let EnumSynValue::AmbI(val) = self { val } else { panic!() }
        }
        fn get_nv_example(self) -> SynNvExample {
            if let EnumSynValue::NvExample(val) = self { val } else { panic!() }
        }
        fn get_nv_star(self) -> SynNvStar {
            if let EnumSynValue::NvStar(val) = self { val } else { panic!() }
        }
        fn get_nv_plus(self) -> SynNvPlus {
            if let EnumSynValue::NvPlus(val) = self { val } else { panic!() }
        }
        fn get_nv_l_star(self) -> SynNvLStar {
            if let EnumSynValue::NvLStar(val) = self { val } else { panic!() }
        }
        fn get_nv_l_star_i(self) -> SynNvLStarI {
            if let EnumSynValue::NvLStarI(val) = self { val } else { panic!() }
        }
        fn get_nv_l_plus(self) -> SynNvLPlus {
            if let EnumSynValue::NvLPlus(val) = self { val } else { panic!() }
        }
        fn get_nv_l_plus_i(self) -> SynNvLPlusI {
            if let EnumSynValue::NvLPlusI(val) = self { val } else { panic!() }
        }
        fn get_nv_rrec(self) -> SynNvRrec {
            if let EnumSynValue::NvRrec(val) = self { val } else { panic!() }
        }
        fn get_nv_lrec(self) -> SynNvLrec {
            if let EnumSynValue::NvLrec(val) = self { val } else { panic!() }
        }
        fn get_nv_star_a(self) -> SynNvStarA {
            if let EnumSynValue::NvStarA(val) = self { val } else { panic!() }
        }
        fn get_nv_plus_a(self) -> SynNvPlusA {
            if let EnumSynValue::NvPlusA(val) = self { val } else { panic!() }
        }
        fn get_nv_l_star_a(self) -> SynNvLStarA {
            if let EnumSynValue::NvLStarA(val) = self { val } else { panic!() }
        }
        fn get_nv_l_star_a_i(self) -> SynNvLStarAI {
            if let EnumSynValue::NvLStarAI(val) = self { val } else { panic!() }
        }
        fn get_nv_l_plus_a(self) -> SynNvLPlusA {
            if let EnumSynValue::NvLPlusA(val) = self { val } else { panic!() }
        }
        fn get_nv_l_plus_a_i(self) -> SynNvLPlusAI {
            if let EnumSynValue::NvLPlusAI(val) = self { val } else { panic!() }
        }
        fn get_nv_sep_list(self) -> SynNvSepList {
            if let EnumSynValue::NvSepList(val) = self { val } else { panic!() }
        }
        fn get_nv_sep_list_opt(self) -> SynNvSepListOpt {
            if let EnumSynValue::NvSepListOpt(val) = self { val } else { panic!() }
        }
        fn get_nv_l_sep_list(self) -> SynNvLSepList {
            if let EnumSynValue::NvLSepList(val) = self { val } else { panic!() }
        }
        fn get_nv_l_sep_list_i(self) -> SynNvLSepListI {
            if let EnumSynValue::NvLSepListI(val) = self { val } else { panic!() }
        }
        fn get_nv_l_sep_list_opt(self) -> SynNvLSepListOpt {
            if let EnumSynValue::NvLSepListOpt(val) = self { val } else { panic!() }
        }
        fn get_nv_l_sep_list_opt_i(self) -> SynNvLSepListOptI {
            if let EnumSynValue::NvLSepListOptI(val) = self { val } else { panic!() }
        }
        fn get_nv_rrec_i(self) -> SynNvRrecI {
            if let EnumSynValue::NvRrecI(val) = self { val } else { panic!() }
        }
        fn get_nv_lrec_i(self) -> SynNvLrecI {
            if let EnumSynValue::NvLrecI(val) = self { val } else { panic!() }
        }
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
        #[allow(unused)]
        fn nt(&self) -> VarId {
            match &self {
                EnumSynValue::Text(_) => 0,
                EnumSynValue::I(_) => 1,
                EnumSynValue::NvI(_) => 2,
                EnumSynValue::Example(_) => 3,
                EnumSynValue::Star(_) => 4,
                EnumSynValue::Plus(_) => 5,
                EnumSynValue::LStar(_) => 6,
                EnumSynValue::LStarI(_) => 7,
                EnumSynValue::LPlus(_) => 8,
                EnumSynValue::LPlusI(_) => 9,
                EnumSynValue::Rrec(_) => 10,
                EnumSynValue::Lrec(_) => 11,
                EnumSynValue::Amb(_) => 12,
                EnumSynValue::StarA(_) => 13,
                EnumSynValue::PlusA(_) => 14,
                EnumSynValue::LStarA(_) => 15,
                EnumSynValue::LStarAI(_) => 16,
                EnumSynValue::LPlusA(_) => 17,
                EnumSynValue::LPlusAI(_) => 18,
                EnumSynValue::SepList(_) => 19,
                EnumSynValue::SepListOpt(_) => 20,
                EnumSynValue::LSepList(_) => 21,
                EnumSynValue::LSepListI(_) => 22,
                EnumSynValue::LSepListOpt(_) => 23,
                EnumSynValue::LSepListOptI(_) => 24,
                EnumSynValue::RrecI(_) => 25,
                EnumSynValue::LrecI(_) => 26,
                EnumSynValue::AmbI(_) => 27,
                EnumSynValue::NvExample(_) => 28,
                EnumSynValue::NvStar(_) => 29,
                EnumSynValue::NvPlus(_) => 30,
                EnumSynValue::NvLStar(_) => 31,
                EnumSynValue::NvLStarI(_) => 32,
                EnumSynValue::NvLPlus(_) => 33,
                EnumSynValue::NvLPlusI(_) => 34,
                EnumSynValue::NvRrec(_) => 35,
                EnumSynValue::NvLrec(_) => 36,
                EnumSynValue::NvStarA(_) => 37,
                EnumSynValue::NvPlusA(_) => 38,
                EnumSynValue::NvLStarA(_) => 39,
                EnumSynValue::NvLStarAI(_) => 40,
                EnumSynValue::NvLPlusA(_) => 41,
                EnumSynValue::NvLPlusAI(_) => 42,
                EnumSynValue::NvSepList(_) => 43,
                EnumSynValue::NvSepListOpt(_) => 44,
                EnumSynValue::NvLSepList(_) => 45,
                EnumSynValue::NvLSepListI(_) => 46,
                EnumSynValue::NvLSepListOpt(_) => 47,
                EnumSynValue::NvLSepListOptI(_) => 48,
                EnumSynValue::NvRrecI(_) => 49,
                EnumSynValue::NvLrecI(_) => 50,
                EnumSynValue::Star1(_) => 51,
                EnumSynValue::Plus1(_) => 52,
                EnumSynValue::StarA1(_) => 53,
                EnumSynValue::PlusA1(_) => 54,
                EnumSynValue::SepList1(_) => 55,
                EnumSynValue::SepListOpt1(_) => 56,
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
        fn drop_nt_value(&mut self, value: &EnumSynValue) {}
        #[allow(unused_variables)]
        fn get_recovery_value(&mut self, nt: VarId, last_dropped: Option<EnumSynValue>) -> RecoveryNtValue { RecoveryNtValue::Abort }
        fn syntax_error_recovered(&mut self) {}
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, text: SynText, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText;
        fn init_i(&mut self) -> SynI;
        fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>);
        fn init_nv_i(&mut self) -> SynNvI;
        fn exit_nv_i(&mut self, acc: &mut SynNvI, ctx: CtxNvI, spans: Vec<PosSpan>);
        fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample;
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar;
        fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus;
        fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar;
        fn init_l_star_i(&mut self) -> SynLStarI;
        fn exit_l_star_i(&mut self, acc: &mut SynLStarI, ctx: CtxLStarI, spans: Vec<PosSpan>);
        fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus;
        fn init_l_plus_i(&mut self) -> SynLPlusI;
        fn exit_l_plus_i(&mut self, acc: &mut SynLPlusI, ctx: CtxLPlusI, spans: Vec<PosSpan>);
        fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec;
        fn exit_lrec(&mut self, ctx: CtxLrec, spans: Vec<PosSpan>) -> SynLrec;
        fn exit_amb(&mut self, ctx: CtxAmb, spans: Vec<PosSpan>) -> SynAmb;
        fn exit_star_a(&mut self, ctx: CtxStarA, spans: Vec<PosSpan>) -> SynStarA;
        fn exit_plus_a(&mut self, ctx: CtxPlusA, spans: Vec<PosSpan>) -> SynPlusA;
        fn exit_l_star_a(&mut self, ctx: CtxLStarA, spans: Vec<PosSpan>) -> SynLStarA;
        fn init_l_star_a_i(&mut self) -> SynLStarAI;
        fn exit_l_star_a_i(&mut self, acc: &mut SynLStarAI, ctx: CtxLStarAI, spans: Vec<PosSpan>);
        fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA;
        fn init_l_plus_a_i(&mut self) -> SynLPlusAI;
        fn exit_l_plus_a_i(&mut self, acc: &mut SynLPlusAI, ctx: CtxLPlusAI, spans: Vec<PosSpan>);
        fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) -> SynSepList;
        fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) -> SynSepListOpt;
        fn exit_l_sep_list(&mut self, ctx: CtxLSepList, spans: Vec<PosSpan>) -> SynLSepList;
        fn init_l_sep_list_i(&mut self) -> SynLSepListI;
        fn exit_l_sep_list_i(&mut self, acc: &mut SynLSepListI, ctx: CtxLSepListI, spans: Vec<PosSpan>);
        fn exit_l_sep_list_opt(&mut self, ctx: CtxLSepListOpt, spans: Vec<PosSpan>) -> SynLSepListOpt;
        fn init_l_sep_list_opt_i(&mut self) -> SynLSepListOptI;
        fn exit_l_sep_list_opt_i(&mut self, acc: &mut SynLSepListOptI, ctx: CtxLSepListOptI, spans: Vec<PosSpan>);
        fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI;
        fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI;
        fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI;
        fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) -> SynNvExample;
        fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) -> SynNvStar;
        fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) -> SynNvPlus;
        fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) -> SynNvLStar;
        fn init_nv_l_star_i(&mut self) -> SynNvLStarI;
        fn exit_nv_l_star_i(&mut self, acc: &mut SynNvLStarI, ctx: CtxNvLStarI, spans: Vec<PosSpan>);
        fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) -> SynNvLPlus;
        fn init_nv_l_plus_i(&mut self) -> SynNvLPlusI;
        fn exit_nv_l_plus_i(&mut self, acc: &mut SynNvLPlusI, ctx: CtxNvLPlusI, spans: Vec<PosSpan>);
        fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) -> SynNvRrec;
        fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) -> SynNvLrec;
        fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) -> SynNvStarA;
        fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) -> SynNvPlusA;
        fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) -> SynNvLStarA;
        fn init_nv_l_star_a_i(&mut self) -> SynNvLStarAI;
        fn exit_nv_l_star_a_i(&mut self, acc: &mut SynNvLStarAI, ctx: CtxNvLStarAI, spans: Vec<PosSpan>);
        fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) -> SynNvLPlusA;
        fn init_nv_l_plus_a_i(&mut self) -> SynNvLPlusAI;
        fn exit_nv_l_plus_a_i(&mut self, acc: &mut SynNvLPlusAI, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>);
        fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) -> SynNvSepList;
        fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) -> SynNvSepListOpt;
        fn exit_nv_l_sep_list(&mut self, ctx: CtxNvLSepList, spans: Vec<PosSpan>) -> SynNvLSepList;
        fn init_nv_l_sep_list_i(&mut self) -> SynNvLSepListI;
        fn exit_nv_l_sep_list_i(&mut self, acc: &mut SynNvLSepListI, ctx: CtxNvLSepListI, spans: Vec<PosSpan>);
        fn exit_nv_l_sep_list_opt(&mut self, ctx: CtxNvLSepListOpt, spans: Vec<PosSpan>) -> SynNvLSepListOpt;
        fn init_nv_l_sep_list_opt_i(&mut self) -> SynNvLSepListOptI;
        fn exit_nv_l_sep_list_opt_i(&mut self, acc: &mut SynNvLSepListOptI, ctx: CtxNvLSepListOptI, spans: Vec<PosSpan>);
        fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) -> SynNvRrecI;
        fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) -> SynNvLrecI;
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
                            let val = self.stack.pop().unwrap().get_text();
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

    impl<T: PandemoniumListener> WrapperLRErrorRecovery for Wrapper<T> {
        fn pop_nt_value(&mut self) {
            self.last_dropped_nt_value = self.stack.pop();
            if self.verbose { println!("dropped {:?} value", self.last_dropped_nt_value.as_ref().unwrap()); }
            self.listener.drop_nt_value(self.last_dropped_nt_value.as_ref().unwrap());
        }

        fn push_nt_recovery_value(&mut self, nt: VarId) -> RecoveryNt {
            match self.listener.get_recovery_value(nt, self.last_dropped_nt_value.take()) {
                RecoveryNtValue::Abort => RecoveryNt::Abort,
                RecoveryNtValue::Skip => RecoveryNt::Skip,
                RecoveryNtValue::Value(val) => {
                    self.stack.push(val);
                    RecoveryNt::Done
                }
            }
        }

        fn get_state_symbol_and_value(state: LRStateId) -> (Symbol, bool) {
            let sym = STATE_SYMBOL[state as usize];
            let has_value = match sym {
                Symbol::T(t) => SYMBOLS_T[t as usize].1.is_none(),
                Symbol::NT(nt) => NT_VALUE[nt as usize],
                Symbol::Empty => false,
                Symbol::End => panic!(),
            };
            (sym, has_value)
        }

        fn syntax_error_recovered(&mut self) {
            self.listener.syntax_error_recovered();
        }
    }

    impl<T: PandemoniumListener> Wrapper<T> {
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

        fn exit_text(&mut self) {
            let star1 = self.stack.pop().unwrap().get_nv_i();
            let star = self.stack.pop().unwrap().get_i();
            let ctx = CtxText::V1 { star, star1 };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_text(ctx, spans);
            self.stack.push(EnumSynValue::Text(val));
        }

        fn init_i(&mut self) {
            let val = self.listener.init_i();
            self.stack.push(EnumSynValue::I(val));
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_i(&mut self) {
            let example = self.stack.pop().unwrap().get_example();
            let ctx = CtxI::V1 { example };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::I(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_i(acc, ctx, spans);
        }

        fn init_nv_i(&mut self) {
            let val = self.listener.init_nv_i();
            self.stack.push(EnumSynValue::NvI(val));
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_nv_i(&mut self) {
            let nv_example = self.stack.pop().unwrap().get_nv_example();
            let ctx = CtxNvI::V1 { nv_example };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_i(acc, ctx, spans);
        }

        fn exit_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                5 => {
                    let star = self.stack.pop().unwrap().get_star();
                    (2, CtxExample::V1 { star })
                }
                6 => {
                    let plus = self.stack.pop().unwrap().get_plus();
                    (2, CtxExample::V2 { plus })
                }
                7 => {
                    let l_star = self.stack.pop().unwrap().get_l_star();
                    (2, CtxExample::V3 { l_star })
                }
                8 => {
                    let l_plus = self.stack.pop().unwrap().get_l_plus();
                    (2, CtxExample::V4 { l_plus })
                }
                9 => {
                    let rrec = self.stack.pop().unwrap().get_rrec();
                    (2, CtxExample::V5 { rrec })
                }
                10 => {
                    let lrec = self.stack.pop().unwrap().get_lrec();
                    (2, CtxExample::V6 { lrec })
                }
                11 => {
                    let amb = self.stack.pop().unwrap().get_amb();
                    (2, CtxExample::V7 { amb })
                }
                12 => {
                    let star_a = self.stack.pop().unwrap().get_star_a();
                    (2, CtxExample::V8 { star_a })
                }
                13 => {
                    let plus_a = self.stack.pop().unwrap().get_plus_a();
                    (2, CtxExample::V9 { plus_a })
                }
                14 => {
                    let l_star_a = self.stack.pop().unwrap().get_l_star_a();
                    (2, CtxExample::V10 { l_star_a })
                }
                15 => {
                    let l_plus_a = self.stack.pop().unwrap().get_l_plus_a();
                    (2, CtxExample::V11 { l_plus_a })
                }
                16 => {
                    let sep_list = self.stack.pop().unwrap().get_sep_list();
                    (2, CtxExample::V12 { sep_list })
                }
                17 => {
                    let sep_list_opt = self.stack.pop().unwrap().get_sep_list_opt();
                    (2, CtxExample::V13 { sep_list_opt })
                }
                18 => {
                    let l_sep_list = self.stack.pop().unwrap().get_l_sep_list();
                    (2, CtxExample::V14 { l_sep_list })
                }
                19 => {
                    let l_sep_list_opt = self.stack.pop().unwrap().get_l_sep_list_opt();
                    (2, CtxExample::V15 { l_sep_list_opt })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_example")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_example(ctx, spans);
            self.stack.push(EnumSynValue::Example(val));
        }

        fn exit_star(&mut self) {
            let star = self.stack.pop().unwrap().get_star1();
            let id_2 = self.stack_t.pop().unwrap();
            let id_1 = self.stack_t.pop().unwrap();
            let ctx = CtxStar::V1 { id: [id_1, id_2], star };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_star(ctx, spans);
            self.stack.push(EnumSynValue::Star(val));
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
            let val = self.listener.exit_plus(ctx, spans);
            self.stack.push(EnumSynValue::Plus(val));
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
            let star = self.stack.pop().unwrap().get_l_star_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStar::V1 { id, num, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_star(ctx, spans);
            self.stack.push(EnumSynValue::LStar(val));
        }

        fn init_l_star_i(&mut self) {
            let val = self.listener.init_l_star_i();
            self.stack.push(EnumSynValue::LStarI(val));
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_l_star_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLStarI::V1 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LStarI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_star_i(acc, ctx, spans);
        }

        fn exit_l_plus(&mut self) {
            let plus = self.stack.pop().unwrap().get_l_plus_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlus::V1 { id, num, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_plus(ctx, spans);
            self.stack.push(EnumSynValue::LPlus(val));
        }

        fn init_l_plus_i(&mut self) {
            let val = self.listener.init_l_plus_i();
            self.stack.push(EnumSynValue::LPlusI(val));
            self.stack_span.insert(self.stack_span.len() - 2, PosSpan::empty());
        }

        fn exit_l_plus_i(&mut self, alt_id: AltId) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusI::V1 { num };
            if matches!(alt_id, 27) { self.init_l_plus_i(); }
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LPlusI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_plus_i(acc, ctx, spans);
        }

        fn exit_rrec(&mut self) {
            let rrec_i = self.stack.pop().unwrap().get_rrec_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxRrec::V1 { id, num, rrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_rrec(ctx, spans);
            self.stack.push(EnumSynValue::Rrec(val));
        }

        fn exit_lrec(&mut self) {
            let lrec_i = self.stack.pop().unwrap().get_lrec_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLrec::V1 { id, lrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec(ctx, spans);
            self.stack.push(EnumSynValue::Lrec(val));
        }

        fn exit_amb(&mut self) {
            let amb_i = self.stack.pop().unwrap().get_amb_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxAmb::V1 { id, amb_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_amb(ctx, spans);
            self.stack.push(EnumSynValue::Amb(val));
        }

        fn exit_star_a(&mut self) {
            let star = self.stack.pop().unwrap().get_star_a1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_star_a(ctx, spans);
            self.stack.push(EnumSynValue::StarA(val));
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
            let val = self.listener.exit_plus_a(ctx, spans);
            self.stack.push(EnumSynValue::PlusA(val));
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
            let star = self.stack.pop().unwrap().get_l_star_a_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_star_a(ctx, spans);
            self.stack.push(EnumSynValue::LStarA(val));
        }

        fn init_l_star_a_i(&mut self) {
            let val = self.listener.init_l_star_a_i();
            self.stack.push(EnumSynValue::LStarAI(val));
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
            let Some(EnumSynValue::LStarAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_star_a_i(acc, ctx, spans);
        }

        fn exit_l_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_l_plus_a_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusA::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_plus_a(ctx, spans);
            self.stack.push(EnumSynValue::LPlusA(val));
        }

        fn init_l_plus_a_i(&mut self, alt_id: AltId) {
            let val = self.listener.init_l_plus_a_i();
            self.stack.push(EnumSynValue::LPlusAI(val));
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
            let Some(EnumSynValue::LPlusAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_plus_a_i(acc, ctx, spans);
        }

        fn exit_sep_list(&mut self) {
            let plus = self.stack.pop().unwrap().get_sep_list1();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxSepList::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_sep_list(ctx, spans);
            self.stack.push(EnumSynValue::SepList(val));
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
            let val = self.listener.exit_sep_list_opt(ctx, spans);
            self.stack.push(EnumSynValue::SepListOpt(val));
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
            let plus = self.stack.pop().unwrap().get_l_sep_list_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepList::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_sep_list(ctx, spans);
            self.stack.push(EnumSynValue::LSepList(val));
        }

        fn init_l_sep_list_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let mut val = self.listener.init_l_sep_list_i();
            self.listener.exit_l_sep_list_i(&mut val, ctx, spans);
            self.stack.push(EnumSynValue::LSepListI(val));
        }

        fn exit_l_sep_list_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListI::V1 { id, num };
            let mut spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            let Some(EnumSynValue::LSepListI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_sep_list_i(acc, ctx, spans);
        }

        fn exit_l_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                48 => {
                    let plus = self.stack.pop().unwrap().get_l_sep_list_opt_i();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxLSepListOpt::V1 { id, plus })
                }
                49 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxLSepListOpt::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_sep_list_opt(ctx, spans);
            self.stack.push(EnumSynValue::LSepListOpt(val));
        }

        fn init_l_sep_list_opt_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListOptI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let mut val = self.listener.init_l_sep_list_opt_i();
            self.listener.exit_l_sep_list_opt_i(&mut val, ctx, spans);
            self.stack.push(EnumSynValue::LSepListOptI(val));
        }

        fn exit_l_sep_list_opt_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListOptI::V1 { id, num };
            let mut spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            let Some(EnumSynValue::LSepListOptI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_sep_list_opt_i(acc, ctx, spans);
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                52 => {
                    let rrec_i = self.stack.pop().unwrap().get_rrec_i();
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num, rrec_i })
                }
                53 => {
                    (1, CtxRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_rrec_i(ctx, spans);
            self.stack.push(EnumSynValue::RrecI(val));
        }

        fn exit_lrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                54 => {
                    let num = self.stack_t.pop().unwrap();
                    let lrec_i = self.stack.pop().unwrap().get_lrec_i();
                    (3, CtxLrecI::V1 { lrec_i, num })
                }
                55 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxLrecI::V2 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_lrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(EnumSynValue::LrecI(val));
        }

        fn exit_amb_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                56 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (2, CtxAmbI::V1 { amb_i })
                }
                57 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V2 { amb_i: [amb_i_1, amb_i_2] })
                }
                58 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V3 { amb_i: [amb_i_1, amb_i_2] })
                }
                59 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V4 { amb_i: [amb_i_1, amb_i_2] })
                }
                60 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V5 { amb_i: [amb_i_1, amb_i_2] })
                }
                61 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V6 { amb_i: [amb_i_1, amb_i_2] })
                }
                62 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V7 { amb_i })
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
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(EnumSynValue::AmbI(val));
        }

        fn exit_nv_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                65 => {
                    let nv_star = self.stack.pop().unwrap().get_nv_star();
                    (2, CtxNvExample::V1 { nv_star })
                }
                66 => {
                    let nv_plus = self.stack.pop().unwrap().get_nv_plus();
                    (2, CtxNvExample::V2 { nv_plus })
                }
                67 => {
                    let nv_l_star = self.stack.pop().unwrap().get_nv_l_star();
                    (2, CtxNvExample::V3 { nv_l_star })
                }
                68 => {
                    let nv_l_plus = self.stack.pop().unwrap().get_nv_l_plus();
                    (2, CtxNvExample::V4 { nv_l_plus })
                }
                69 => {
                    let nv_rrec = self.stack.pop().unwrap().get_nv_rrec();
                    (2, CtxNvExample::V5 { nv_rrec })
                }
                70 => {
                    let nv_lrec = self.stack.pop().unwrap().get_nv_lrec();
                    (2, CtxNvExample::V6 { nv_lrec })
                }
                71 => {
                    let nv_star_a = self.stack.pop().unwrap().get_nv_star_a();
                    (2, CtxNvExample::V7 { nv_star_a })
                }
                72 => {
                    let nv_plus_a = self.stack.pop().unwrap().get_nv_plus_a();
                    (2, CtxNvExample::V8 { nv_plus_a })
                }
                73 => {
                    let nv_l_star_a = self.stack.pop().unwrap().get_nv_l_star_a();
                    (2, CtxNvExample::V9 { nv_l_star_a })
                }
                74 => {
                    let nv_l_plus_a = self.stack.pop().unwrap().get_nv_l_plus_a();
                    (2, CtxNvExample::V10 { nv_l_plus_a })
                }
                75 => {
                    let nv_sep_list = self.stack.pop().unwrap().get_nv_sep_list();
                    (2, CtxNvExample::V11 { nv_sep_list })
                }
                76 => {
                    let nv_sep_list_opt = self.stack.pop().unwrap().get_nv_sep_list_opt();
                    (2, CtxNvExample::V12 { nv_sep_list_opt })
                }
                77 => {
                    let nv_l_sep_list = self.stack.pop().unwrap().get_nv_l_sep_list();
                    (2, CtxNvExample::V13 { nv_l_sep_list })
                }
                78 => {
                    let nv_l_sep_list_opt = self.stack.pop().unwrap().get_nv_l_sep_list_opt();
                    (2, CtxNvExample::V14 { nv_l_sep_list_opt })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_example")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_example(ctx, spans);
            self.stack.push(EnumSynValue::NvExample(val));
        }

        fn exit_nv_star(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvStar::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_star(ctx, spans);
            self.stack.push(EnumSynValue::NvStar(val));
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
            let val = self.listener.exit_nv_plus(ctx, spans);
            self.stack.push(EnumSynValue::NvPlus(val));
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
            let star = self.stack.pop().unwrap().get_nv_l_star_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLStar::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_star(ctx, spans);
            self.stack.push(EnumSynValue::NvLStar(val));
        }

        fn init_nv_l_star_i(&mut self) {
            let val = self.listener.init_nv_l_star_i();
            self.stack.push(EnumSynValue::NvLStarI(val));
            self.stack_span.push(PosSpan::empty());
        }

        fn exit_nv_l_star_i(&mut self) {
            let ctx = CtxNvLStarI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLStarI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_star_i(acc, ctx, spans);
        }

        fn exit_nv_l_plus(&mut self) {
            let plus = self.stack.pop().unwrap().get_nv_l_plus_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLPlus::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 5 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_plus(ctx, spans);
            self.stack.push(EnumSynValue::NvLPlus(val));
        }

        fn init_nv_l_plus_i(&mut self) {
            let val = self.listener.init_nv_l_plus_i();
            self.stack.push(EnumSynValue::NvLPlusI(val));
            self.stack_span.insert(self.stack_span.len() - 2, PosSpan::empty());
        }

        fn exit_nv_l_plus_i(&mut self, alt_id: AltId) {
            let ctx = CtxNvLPlusI::V1;
            if matches!(alt_id, 86) { self.init_nv_l_plus_i(); }
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLPlusI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_plus_i(acc, ctx, spans);
        }

        fn exit_nv_rrec(&mut self) {
            let nv_rrec_i = self.stack.pop().unwrap().get_nv_rrec_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvRrec::V1 { id, nv_rrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_rrec(ctx, spans);
            self.stack.push(EnumSynValue::NvRrec(val));
        }

        fn exit_nv_lrec(&mut self) {
            let nv_lrec_i = self.stack.pop().unwrap().get_nv_lrec_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLrec::V1 { id, nv_lrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_lrec(ctx, spans);
            self.stack.push(EnumSynValue::NvLrec(val));
        }

        fn exit_nv_star_a(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvStarA::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_star_a(ctx, spans);
            self.stack.push(EnumSynValue::NvStarA(val));
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
            let val = self.listener.exit_nv_plus_a(ctx, spans);
            self.stack.push(EnumSynValue::NvPlusA(val));
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
            let star = self.stack.pop().unwrap().get_nv_l_star_a_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLStarA::V1 { id, star };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_star_a(ctx, spans);
            self.stack.push(EnumSynValue::NvLStarA(val));
        }

        fn init_nv_l_star_a_i(&mut self) {
            let val = self.listener.init_nv_l_star_a_i();
            self.stack.push(EnumSynValue::NvLStarAI(val));
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
            let Some(EnumSynValue::NvLStarAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_star_a_i(acc, ctx, spans);
        }

        fn exit_nv_l_plus_a(&mut self) {
            let plus = self.stack.pop().unwrap().get_nv_l_plus_a_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLPlusA::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_plus_a(ctx, spans);
            self.stack.push(EnumSynValue::NvLPlusA(val));
        }

        fn init_nv_l_plus_a_i(&mut self, alt_id: AltId) {
            let val = self.listener.init_nv_l_plus_a_i();
            self.stack.push(EnumSynValue::NvLPlusAI(val));
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
            let Some(EnumSynValue::NvLPlusAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_plus_a_i(acc, ctx, spans);
        }

        fn exit_nv_sep_list(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvSepList::V1 { id };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_sep_list(ctx, spans);
            self.stack.push(EnumSynValue::NvSepList(val));
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
            let val = self.listener.exit_nv_sep_list_opt(ctx, spans);
            self.stack.push(EnumSynValue::NvSepListOpt(val));
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
            let plus = self.stack.pop().unwrap().get_nv_l_sep_list_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLSepList::V1 { id, plus };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_sep_list(ctx, spans);
            self.stack.push(EnumSynValue::NvLSepList(val));
        }

        fn init_nv_l_sep_list_i(&mut self) {
            let ctx = CtxNvLSepListI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let mut val = self.listener.init_nv_l_sep_list_i();
            self.listener.exit_nv_l_sep_list_i(&mut val, ctx, spans);
            self.stack.push(EnumSynValue::NvLSepListI(val));
        }

        fn exit_nv_l_sep_list_i(&mut self) {
            let ctx = CtxNvLSepListI::V1;
            let mut spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            let Some(EnumSynValue::NvLSepListI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_sep_list_i(acc, ctx, spans);
        }

        fn exit_nv_l_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                106 => {
                    let plus = self.stack.pop().unwrap().get_nv_l_sep_list_opt_i();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvLSepListOpt::V1 { id, plus })
                }
                107 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxNvLSepListOpt::V2 { id })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_sep_list_opt")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_sep_list_opt(ctx, spans);
            self.stack.push(EnumSynValue::NvLSepListOpt(val));
        }

        fn init_nv_l_sep_list_opt_i(&mut self) {
            let ctx = CtxNvLSepListOptI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let mut val = self.listener.init_nv_l_sep_list_opt_i();
            self.listener.exit_nv_l_sep_list_opt_i(&mut val, ctx, spans);
            self.stack.push(EnumSynValue::NvLSepListOptI(val));
        }

        fn exit_nv_l_sep_list_opt_i(&mut self) {
            let ctx = CtxNvLSepListOptI::V1;
            let mut spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            spans.drain(..3);
            let Some(EnumSynValue::NvLSepListOptI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_sep_list_opt_i(acc, ctx, spans);
        }

        fn exit_nv_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                110 => {
                    let nv_rrec_i = self.stack.pop().unwrap().get_nv_rrec_i();
                    (3, CtxNvRrecI::V1 { nv_rrec_i })
                }
                111 => {
                    (1, CtxNvRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_rrec_i(ctx, spans);
            self.stack.push(EnumSynValue::NvRrecI(val));
        }

        fn exit_nv_lrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                112 => {
                    let nv_lrec_i = self.stack.pop().unwrap().get_nv_lrec_i();
                    (3, CtxNvLrecI::V1 { nv_lrec_i })
                }
                113 => {
                    (1, CtxNvLrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_lrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_lrec_i(ctx, spans);
            self.stack.push(EnumSynValue::NvLrecI(val));
        }
    }

    // [pandemonium_parser]
}
