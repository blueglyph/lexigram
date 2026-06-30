// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use std::collections::BTreeMap;
use lexigram_core::CollectJoin;
use lexigram_core::char_reader::CharReader;
use lexigram_core::lexer::{Lexer, PosSpan, TokenSpliterator};
use lexigram_core::log::{BufLog, LogStatus, Logger};
use lexigram_core::parser::{LLParser, Terminate};
use lexigram_core::text_span::{GetLine, GetTextSpan};
use crate::level_string::{ls_binary_op, ls_prefix_op, LevelString};
use listener_types::*;
use pandemonium_lexer::build_lexer;
use pandemonium_parser::*;
use crate::{SPANS1, SPANS2, TXT1, TXT2};

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
    "[Foxtrot][106,160,650,<end>]",
    "[Golf][(107),(170),(750)]",
    "[Hotel][(5 - (2 * (- 6))) + ((3 ^ (2 ^ 4)) / 81)]",
    "[India][1:Alpha/Beta/4:Delta/Echo/10:Juliet]",
    "[Juliet][11:Kilo/Lima/Mike/26:Zoulou]",
    "[Kilo][2:Beta|Charlie|5:Echo]",
    "[Lima][21:Uniform||Victor||25:Yankee]",
    "[Mike][x]",
    "[November][202]",
    "[Oscar][203]",
    "[Papa][204,<end>]",
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
    "[Delta][false,false,true]",
    "[Echo][2]",
    "[Foxtrot][false,false,false,true]",
    "[Golf][2]",
    "[India][+*-*]",
    "[Juliet][+*-+]",
    "[Kilo][false|true|false|true|false]",
    "[Lima][false||true||false||true||false]",
    "[Mike][*]",
    "[November][0]",
    "[Oscar][0]",
    "[Papa][true]",
    "[Quebec][0]",
    "[Romeo][*,then+]",
    "[Sierra][*,then+]",
    "[Tango][a]",
    "[Uniform][b]",
    "[Victor][2]",
    "[Whiskey][0]",
    "[Xray][1]",
    "[Yankee][-]",
];

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
            CtxExample::V6 { l_rrec: SynLRrec() } => {}
            CtxExample::V7 { lrec: SynLrec() } => {}
            CtxExample::V8 { amb: SynAmb() } => {}
            CtxExample::V9 { star_a: SynStarA() } => {}
            CtxExample::V10 { plus_a: SynPlusA() } => {}
            CtxExample::V11 { l_star_a: SynLStarA() } => {}
            CtxExample::V12 { l_plus_a: SynLPlusA() } => {}
            CtxExample::V13 { sep_list: SynSepList() } => {}
            CtxExample::V14 { sep_list_opt: SynSepListOpt() } => {}
            CtxExample::V15 { l_sep_list: SynLSepList() } => {}
            CtxExample::V16 { l_sep_list_opt: SynLSepListOpt() } => {}
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
        let CtxLPlusI::V1 { num, last_iteration } = ctx;
        acc.0.push(num);
    }

    fn exit_rrec(&mut self, ctx: CtxRrec, spans: Vec<PosSpan>) -> SynRrec {
        self.spans.push(format!("exit_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxRrec::V1 { id, num, rrec_i: SynRrecI(mut items) } = ctx;
        items.push(num);
        self.add_value(id, items.iter().rev().join(";"));
        SynRrec()
    }

    fn exit_l_rrec(&mut self, ctx: CtxLRrec, spans: Vec<PosSpan>) -> SynLRrec {
        self.spans.push(format!("exit_l_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        let CtxLRrec::V1 { id, num, l_rrec_i: SynLRrecI(mut list) } = ctx;
        list.insert(0, num);
        self.add_value(id, list.join(","));
        SynLRrec()
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
            CtxLPlusAI::V1 { id, last_iteration } => id,
            // `Num ":" Id` iteration in `l_plus_a -> Id "=" "[" (<L> Id |  ►► Num ":" Id ◄◄ )+ "]" ";"`
            CtxLPlusAI::V2 { num, id, last_iteration } => format!("{num}:{id}"),
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

    fn init_l_sep_list_i(&mut self, ctx: InitCtxLSepListI, spans: Vec<PosSpan>) -> SynLSepListI {
        // value of `Id Num` before `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
        let InitCtxLSepListI::V1 { id, num } = ctx;
        SynLSepListI(vec![format!("<{id}:{num}>")])
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

    fn init_l_sep_list_opt_i(&mut self, ctx: InitCtxLSepListOptI, spans: Vec<PosSpan>) -> SynLSepListOptI {
        // value of `Id Num` before `<L> Id ":" Num / "," "then"` iteration in `l_sep_list_opt -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        let InitCtxLSepListOptI::V1 { id, num } = ctx;
        SynLSepListOptI(vec![format!("<{id}/{num}>")])
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

    fn init_l_rrec_i(&mut self) -> SynLRrecI {
        SynLRrecI(vec![])
    }

    fn exit_l_rrec_i(&mut self, acc: &mut SynLRrecI, ctx: CtxLRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_l_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxLRrecI::V1 { num } => {
                acc.0.push(num);
            }
            CtxLRrecI::V2 => {
                acc.0.push("<end>".to_string());
            }
        }
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

    fn exitloop_lrec_i(&mut self, _lrec_i: &mut SynLrecI) {
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

    // TODO:

    fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) -> SynNvExample {
        self.spans.push(format!("exit_nv_example({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            CtxNvExample::V1 { nv_star } => {}              // nv_example -> "star" nv_star
            CtxNvExample::V2 { nv_plus } => {}              // nv_example -> "plus" nv_plus
            CtxNvExample::V3 { nv_l_star } => {}            // nv_example -> "l-star" nv_l_star
            CtxNvExample::V4 { nv_l_plus } => {}            // nv_example -> "l-plus" nv_l_plus
            CtxNvExample::V5 { nv_rrec } => {}              // nv_example -> "rrec" nv_rrec
            CtxNvExample::V6 { nv_l_rrec } => {}            // nv_example -> "l-rrec" nv_l_rrec
            CtxNvExample::V7 { nv_lrec } => {}              // nv_example -> "lrec" nv_lrec
            CtxNvExample::V8 { nv_star_a } => {}            // nv_example -> "star-a" nv_star_a
            CtxNvExample::V9 { nv_plus_a } => {}            // nv_example -> "plus-a" nv_plus_a
            CtxNvExample::V10 { nv_l_star_a } => {}         // nv_example -> "l-star-a" nv_l_star_a
            CtxNvExample::V11 { nv_l_plus_a } => {}         // nv_example -> "l-plus-a" nv_l_plus_a
            CtxNvExample::V12 { nv_sep_list } => {}         // nv_example -> "sep-list" nv_sep_list
            CtxNvExample::V13 { nv_sep_list_opt } => {}     // nv_example -> "sep-list-opt" nv_sep_list_opt
            CtxNvExample::V14 { nv_l_sep_list } => {}       // nv_example -> "l-sep-list" nv_l_sep_list
            CtxNvExample::V15 { nv_l_sep_list_opt } => {}   // nv_example -> "l-sep-list-opt" nv_l_sep_list_opt
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
        let CtxNvLPlusI::V1 { last_iteration } = ctx;
        acc.0.push(last_iteration);
    }

    fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) -> SynNvRrec {
        self.spans.push(format!("exit_nv_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_rrec -> Id "=" "+" nv_rrec_i
        let CtxNvRrec::V1 { id, nv_rrec_i: SynNvRrecI(n) } = ctx;
        self.add_value(id, n.to_string());
        SynNvRrec()
    }

    fn exit_nv_l_rrec(&mut self, ctx: CtxNvLRrec, spans: Vec<PosSpan>) -> SynNvLRrec {
        self.spans.push(format!("exit_nv_l_rrec({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        // nv_l_rrec -> Id "=" "+" nv_l_rrec_i
        let CtxNvLRrec::V1 { id, nv_l_rrec_i: SynNvLRrecI(items) } = ctx;
        self.add_value(id, items.iter().map(bool::to_string).join(","));
        SynNvLRrec()
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
            CtxNvLPlusAI::V1 { last_iteration } => true,
            // `"*" "-"` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )+ "]" ";"`
            CtxNvLPlusAI::V2 { last_iteration } => false,
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

    fn init_nv_l_sep_list_i(&mut self, ctx: InitCtxNvLSepListI, spans: Vec<PosSpan>) -> SynNvLSepListI {
        let InitCtxNvLSepListI::V1 = ctx;
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

    fn init_nv_l_sep_list_opt_i(&mut self, ctx: InitCtxNvLSepListOptI, spans: Vec<PosSpan>) -> SynNvLSepListOptI {
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

    fn init_nv_l_rrec_i(&mut self) -> SynNvLRrecI {
        SynNvLRrecI(vec![])
    }

    fn exit_nv_l_rrec_i(&mut self, acc: &mut SynNvLRrecI, ctx: CtxNvLRrecI, spans: Vec<PosSpan>) {
        self.spans.push(format!("exit_nv_l_rrec_i({})", spans.into_iter().map(|s| format!("{:?}", self.extract_text(&s))).join(", ")));
        match ctx {
            // nv_l_rrec_i -> <L> "," "*" nv_l_rrec_i
            CtxNvLRrecI::V1 => { acc.0.push(false); }
            // nv_l_rrec_i -> ";"
            CtxNvLRrecI::V2 => { acc.0.push(true); }
        }
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
    /// User-defined type for `l_rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynLRrecI(pub Vec<String>);
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
    /// User-defined type for `nv_l_rrec`
    #[derive(Debug, PartialEq)] pub struct SynNvLRrec();
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
    /// User-defined type for `nv_l_rrec_i`
    #[derive(Debug, PartialEq)] pub struct SynNvLRrecI(pub Vec<bool>);
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
    const FIRST_END_STATE: LexStateId = 33;
    const NBR_STATES: LexStateId = 87;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         29,  29,  29,  29,  29,  29,  29,  29,  29,   0,  32,  29,  29,  32,  29,  29,   // 0-15
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,   // 16-31
          0,  29,  29,  29,  29,  29,  29,  29,   1,   2,   3,   4,   5,   6,  29,   7,   // 32-47
         22,   8,   8,   8,   8,   8,   8,   8,   8,   8,   9,  10,  29,  11,  29,  29,   // 48-63
         29,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,   // 64-79
         27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,  13,  29,  14,  15,  28,   // 80-95
         29,  16,  34,  24,  27,  12,  27,  27,  31,  25,  27,  27,  17,  30,  33,  26,   // 96-111
         18,  27,  19,  20,  21,  23,  27,  27,  27,  27,  27,  29,  29,  29,  29,  29,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 29),
        (Seg(57344, 1114111), 29),
    ];
    static TERMINAL_TABLE: [Terminal;54] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(31), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [LexStateId; 3046] = [
         33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  87,  45,  45,  45,  45,  45,  87,  87,  45,  45,  33,  45,  45, // state 0
          1,   1,   1,  25,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 1
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  77,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 2
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  78,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 3
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,   5,   6,   7,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 4
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  12,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 5
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  15,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 6
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,   8,  87,  87,  87,  87,  87,  87,  87,  87,   9,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 7
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  22,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 8
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  10,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 9
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  67,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 10
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  79,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 11
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  13,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 12
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  68,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 13
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  80,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 14
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  16,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 15
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  69,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 16
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  18,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 17
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  19,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 18
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  82,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 19
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  27,  87,  87,  87,  87,  87,  87,  87,  87, // state 20
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  83,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 21
         87,  87,  87,  87,  87,  87,  32,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 22
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  84,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 23
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  85,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 24
          1,   1,   1,  25,   1,   1,   1,  86,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, // state 25
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  17,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 26
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  21,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 27
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  24,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 28
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  23,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 29
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  29,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 30
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  28,  87,  87,  87,  87,  87,  87,  87,  87, // state 31
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  30,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 32
         33,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  33,  87,  87, // state 33 <skip>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 34 <end:4>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 35 <end:7>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 36 <end:6>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 37 <end:0>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 38 <end:11>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 39 <end:9>
         87,  87,  87,   1,  87,  87,  87,  55,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 40 <end:1>
         87,  87,  87,  87,  87,  87,  87,  87,  41,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  41,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 41 <end:31>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 42 <end:10>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 43 <end:12>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 44 <end:2>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 45 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 46 <end:5>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 47 <end:8>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 48 <end:3>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  75,  45,  87,  45,  45, // state 49 <end:30>
         87,  87,  87,  87,  87,  87,   4,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  66,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 50 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  63,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 51 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  70,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 52 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  59,  87,  87,  87,  45,  45,  45,  45,  45,  60,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 53 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  56,  87,  45,  45, // state 54 <end:30>
         55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  55,  87,  55,  55, // state 55 <skip>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  57,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 56 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  58,  45, // state 57 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 58 <end:13>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  81,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 59 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  61,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 60 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  62,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 61 <end:30>
         87,  87,  87,  87,  87,  87,   2,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 62 <end:14>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  64,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 63 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  65,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 64 <end:30>
         87,  87,  87,  87,  87,  87,   3,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 65 <end:15>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  73,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 66 <end:30>
         87,  87,  87,  87,  87,  87,  11,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 67 <end:16>
         87,  87,  87,  87,  87,  87,  14,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 68 <end:17>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 69 <end:18>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  71,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 70 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  72,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 71 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 72 <end:19>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  74,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 73 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 74 <end:20>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  76, // state 75 <end:30>
         87,  87,  87,  87,  87,  87,  87,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 76 <end:21>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 77 <end:22>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 78 <end:23>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 79 <end:24>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 80 <end:25>
         87,  87,  87,  87,  87,  87,  26,  87,  45,  87,  87,  87,  45,  87,  87,  87,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  87,  45,  45,  87,  45,  45, // state 81 <end:30>
         87,  87,  87,  87,  87,  87,  20,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 82 <end:26>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 83 <end:27>
         87,  87,  87,  87,  87,  87,  31,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 84 <end:28>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 85 <end:29>
         87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87, // state 86 <skip>
         87 // error group in [nbr_state * nbr_group + nbr_group]
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
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 32;
    const PARSER_NUM_NT: usize = 91;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Add", Some("+")), ("Div", Some("/")), ("Equal", Some("=")), ("Exp", Some("^")), ("Lpar", Some("(")), ("Lsbracket", Some("[")), ("Mul", Some("*")), ("Rpar", Some(")")), ("Rsbracket", Some("]")), ("Sub", Some("-")), ("Colon", Some(":")), ("Comma", Some(",")), ("Semi", Some(";")), ("Then", Some("then")), ("Star", Some("star")), ("Plus", Some("plus")), ("L_Star", Some("l-star")), ("L_Plus", Some("l-plus")), ("L_Rrec", Some("l-rrec")), ("Rrec", Some("rrec")), ("Lrec", Some("lrec")), ("Amb", Some("amb")), ("Star_A", Some("star-a")), ("Plus_A", Some("plus-a")), ("L_Star_A", Some("l-star-a")), ("L_Plus_A", Some("l-plus-a")), ("SepList", Some("sep-list")), ("SepList_Opt", Some("sep-list-opt")), ("L_SepList", Some("l-sep-list")), ("L_SepList_Opt", Some("l-sep-list-opt")), ("Id", None), ("Num", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["text", "i", "nv_i", "example", "star", "plus", "l_star", "l_star_i", "l_plus", "l_plus_i", "rrec", "l_rrec", "lrec", "amb", "star_a", "plus_a", "l_star_a", "l_star_a_i", "l_plus_a", "l_plus_a_i", "sep_list", "sep_list_opt", "l_sep_list", "l_sep_list_i", "l_sep_list_opt", "l_sep_list_opt_i", "rrec_i", "l_rrec_i", "lrec_i", "amb_i", "nv_example", "nv_star", "nv_plus", "nv_l_star", "nv_l_star_i", "nv_l_plus", "nv_l_plus_i", "nv_rrec", "nv_l_rrec", "nv_lrec", "nv_star_a", "nv_plus_a", "nv_l_star_a", "nv_l_star_a_i", "nv_l_plus_a", "nv_l_plus_a_i", "nv_sep_list", "nv_sep_list_opt", "nv_l_sep_list", "nv_l_sep_list_i", "nv_l_sep_list_opt", "nv_l_sep_list_opt_i", "nv_rrec_i", "nv_l_rrec_i", "nv_lrec_i", "star_1", "plus_1", "star_a_1", "plus_a_1", "sep_list_1", "sep_list_opt_1", "nv_star_1", "nv_plus_1", "nv_star_a_1", "nv_plus_a_1", "nv_sep_list_1", "nv_sep_list_opt_1", "lrec_i_1", "amb_i_1", "amb_i_2", "amb_i_3", "amb_i_4", "amb_i_5", "amb_i_6", "nv_lrec_i_1", "l_plus_i_1", "l_plus_a_i_1", "l_plus_a_i_2", "sep_list_opt_2", "l_sep_list_opt_1", "nv_l_plus_i_1", "nv_l_plus_a_i_1", "nv_l_plus_a_i_2", "nv_sep_list_opt_2", "nv_l_sep_list_opt_1", "plus_2", "plus_a_2", "plus_a_3", "nv_plus_2", "nv_plus_a_2", "nv_plus_a_3"];
    static ALT_VAR: [VarId; 180] = [0, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 17, 17, 18, 19, 19, 20, 21, 22, 23, 23, 24, 25, 25, 26, 26, 27, 27, 28, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 32, 33, 34, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 43, 43, 44, 45, 45, 46, 47, 48, 49, 49, 50, 51, 51, 52, 52, 53, 53, 54, 55, 55, 56, 57, 57, 57, 58, 58, 59, 59, 60, 60, 61, 61, 62, 63, 63, 63, 64, 64, 65, 65, 66, 66, 67, 67, 68, 68, 68, 68, 68, 68, 69, 70, 70, 70, 70, 71, 72, 72, 73, 73, 73, 73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 79, 79, 80, 80, 81, 81, 82, 82, 83, 83, 84, 84, 85, 85, 86, 86, 87, 87, 88, 88, 89, 89, 90, 90];
    static PARSING_TABLE: [AltId; 3003] = [180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 0, 180, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 2, 180, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 3, 3, 3, 3, 3, 3, 3, 180, 3, 3, 3, 3, 3, 3, 3, 3, 180, 180, 4, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 5, 6, 7, 8, 10, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 21, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 22, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 23, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 24, 25, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 26, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 27, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 28, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 29, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 30, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 31, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 32, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 33, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 34, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 37, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 35, 36, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 38, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 39, 40, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 41, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 42, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 43, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 44, 45, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 46, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 47, 48, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 49, 50, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 51, 52, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 53, 180, 180, 180, 180, 180, 54, 180, 180, 181, 180, 54, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 54, 54, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 55, 56, 57, 58, 60, 59, 61, 180, 62, 63, 64, 65, 66, 67, 68, 69, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 70, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 71, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 72, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 73, 74, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 75, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 76, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 77, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 78, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 79, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 80, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 81, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 82, 180, 181, 83, 180, 180, 180, 180, 180, 84, 180, 85, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 86, 180, 181, 87, 180, 180, 180, 180, 180, 88, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 89, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 90, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 91, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 92, 93, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 94, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 95, 96, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 97, 98, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 99, 100, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 181, 101, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 102, 103, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 104, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 107, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 105, 106, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 108, 109, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 110, 111, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 112, 113, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 114, 115, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 116, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 117, 180, 180, 180, 180, 180, 118, 180, 119, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 120, 180, 180, 180, 180, 180, 121, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 122, 123, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 124, 125, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 126, 127, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 131, 130, 180, 128, 180, 180, 129, 133, 180, 132, 180, 180, 133, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 180, 181, 134, 180, 181, 181, 180, 134, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 134, 134, 180, 138, 137, 180, 135, 180, 180, 136, 138, 180, 138, 180, 180, 138, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 180, 181, 139, 180, 181, 181, 180, 139, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 139, 139, 180, 141, 141, 180, 140, 180, 180, 141, 141, 180, 141, 180, 180, 141, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 181, 181, 180, 181, 143, 180, 181, 181, 180, 142, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 144, 145, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 146, 147, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 148, 149, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 151, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 150, 150, 180, 180, 180, 180, 180, 180, 180, 180, 180, 153, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 152, 152, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 154, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 155, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 156, 180, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 157, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 158, 159, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 160, 180, 180, 180, 180, 180, 160, 180, 161, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 162, 180, 180, 180, 180, 180, 162, 180, 163, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 164, 180, 180, 180, 180, 180, 165, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 181, 180, 180, 180, 180, 180, 180, 166, 180, 180, 180, 180, 180, 167, 180, 181, 181, 181, 181, 181, 181, 181, 180, 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 168, 169, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 171, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 170, 170, 180, 180, 180, 180, 180, 180, 180, 180, 180, 173, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 172, 172, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 174, 175, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 176, 180, 180, 180, 180, 180, 176, 180, 177, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 178, 180, 180, 180, 180, 180, 178, 180, 179, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180];
    static OPCODES: [&[OpCode]; 180] = [&[OpCode::Exit(0), OpCode::NT(2), OpCode::T(12), OpCode::NT(1)], &[OpCode::Loop(1), OpCode::Exit(1), OpCode::NT(3)], &[OpCode::Exit(2)], &[OpCode::Loop(2), OpCode::Exit(3), OpCode::NT(30)], &[OpCode::Exit(4)], &[OpCode::Exit(5), OpCode::NT(4), OpCode::T(14)], &[OpCode::Exit(6), OpCode::NT(5), OpCode::T(15)], &[OpCode::Exit(7), OpCode::NT(6), OpCode::T(16)], &[OpCode::Exit(8), OpCode::NT(8), OpCode::T(17)], &[OpCode::Exit(9), OpCode::NT(10), OpCode::T(19)], &[OpCode::Exit(10), OpCode::NT(11), OpCode::T(18)], &[OpCode::Exit(11), OpCode::NT(12), OpCode::T(20)], &[OpCode::Exit(12), OpCode::NT(13), OpCode::T(21)], &[OpCode::Exit(13), OpCode::NT(14), OpCode::T(22)], &[OpCode::Exit(14), OpCode::NT(15), OpCode::T(23)], &[OpCode::Exit(15), OpCode::NT(16), OpCode::T(24)], &[OpCode::Exit(16), OpCode::NT(18), OpCode::T(25)], &[OpCode::Exit(17), OpCode::NT(20), OpCode::T(26)], &[OpCode::Exit(18), OpCode::NT(21), OpCode::T(27)], &[OpCode::Exit(19), OpCode::NT(22), OpCode::T(28)], &[OpCode::Exit(20), OpCode::NT(24), OpCode::T(29)], &[OpCode::Exit(21), OpCode::T(12), OpCode::NT(55), OpCode::T(30), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(22), OpCode::T(12), OpCode::NT(56), OpCode::T(31), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(23), OpCode::T(12), OpCode::NT(7), OpCode::T(31), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(7), OpCode::Exit(24), OpCode::T(31), OpCode::T(11)], &[OpCode::Exit(25)], &[OpCode::Exit(26), OpCode::T(12), OpCode::NT(9), OpCode::T(31), OpCode::T(2), OpCode::T(30)], &[OpCode::NT(75), OpCode::T(31), OpCode::T(11)], &[OpCode::Exit(28), OpCode::NT(26), OpCode::T(31), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(29), OpCode::NT(27), OpCode::T(31), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(30), OpCode::T(12), OpCode::NT(28), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(31), OpCode::T(12), OpCode::NT(29), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(32), OpCode::T(12), OpCode::T(8), OpCode::NT(57), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(33), OpCode::T(12), OpCode::T(8), OpCode::NT(58), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(34), OpCode::T(12), OpCode::T(8), OpCode::NT(17), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(17), OpCode::Exit(35), OpCode::T(30)], &[OpCode::Loop(17), OpCode::Exit(36), OpCode::T(30), OpCode::T(10), OpCode::T(31)], &[OpCode::Exit(37)], &[OpCode::Exit(38), OpCode::T(12), OpCode::T(8), OpCode::NT(19), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::NT(76), OpCode::T(30)], &[OpCode::NT(77), OpCode::T(30), OpCode::T(10), OpCode::T(31)], &[OpCode::Exit(41), OpCode::T(12), OpCode::NT(59), OpCode::T(31), OpCode::T(10), OpCode::T(30), OpCode::T(2), OpCode::T(30)], &[OpCode::NT(78), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(43), OpCode::T(12), OpCode::NT(23), OpCode::T(31), OpCode::T(10), OpCode::T(30), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(23), OpCode::Exit(44), OpCode::T(31), OpCode::T(10), OpCode::T(30), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(45)], &[OpCode::NT(79), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(25), OpCode::Exit(47), OpCode::T(31), OpCode::T(10), OpCode::T(30), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(48)], &[OpCode::Exit(49), OpCode::NT(26), OpCode::T(31), OpCode::T(11)], &[OpCode::Exit(50), OpCode::T(12)], &[OpCode::Loop(27), OpCode::Exit(51), OpCode::T(31), OpCode::T(11)], &[OpCode::Exit(52), OpCode::T(12)], &[OpCode::NT(67), OpCode::Exit(53), OpCode::T(31)], &[OpCode::NT(68), OpCode::Exit(54), OpCode::NT(73)], &[OpCode::Exit(55), OpCode::NT(31), OpCode::T(14)], &[OpCode::Exit(56), OpCode::NT(32), OpCode::T(15)], &[OpCode::Exit(57), OpCode::NT(33), OpCode::T(16)], &[OpCode::Exit(58), OpCode::NT(35), OpCode::T(17)], &[OpCode::Exit(59), OpCode::NT(37), OpCode::T(19)], &[OpCode::Exit(60), OpCode::NT(38), OpCode::T(18)], &[OpCode::Exit(61), OpCode::NT(39), OpCode::T(20)], &[OpCode::Exit(62), OpCode::NT(40), OpCode::T(22)], &[OpCode::Exit(63), OpCode::NT(41), OpCode::T(23)], &[OpCode::Exit(64), OpCode::NT(42), OpCode::T(24)], &[OpCode::Exit(65), OpCode::NT(44), OpCode::T(25)], &[OpCode::Exit(66), OpCode::NT(46), OpCode::T(26)], &[OpCode::Exit(67), OpCode::NT(47), OpCode::T(27)], &[OpCode::Exit(68), OpCode::NT(48), OpCode::T(28)], &[OpCode::Exit(69), OpCode::NT(50), OpCode::T(29)], &[OpCode::Exit(70), OpCode::T(12), OpCode::NT(61), OpCode::T(0), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(71), OpCode::T(12), OpCode::NT(62), OpCode::T(0), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(72), OpCode::T(12), OpCode::NT(34), OpCode::T(0), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(34), OpCode::Exit(73), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(74)], &[OpCode::Exit(75), OpCode::T(12), OpCode::NT(36), OpCode::T(0), OpCode::T(2), OpCode::T(30)], &[OpCode::NT(80), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(77), OpCode::NT(52), OpCode::T(0), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(78), OpCode::NT(53), OpCode::T(0), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(79), OpCode::T(12), OpCode::NT(54), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(80), OpCode::T(12), OpCode::T(8), OpCode::NT(63), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(81), OpCode::T(12), OpCode::T(8), OpCode::NT(64), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(82), OpCode::T(12), OpCode::T(8), OpCode::NT(43), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(43), OpCode::Exit(83), OpCode::T(0)], &[OpCode::Loop(43), OpCode::Exit(84), OpCode::T(9), OpCode::T(6)], &[OpCode::Exit(85)], &[OpCode::Exit(86), OpCode::T(12), OpCode::T(8), OpCode::NT(45), OpCode::T(5), OpCode::T(2), OpCode::T(30)], &[OpCode::NT(81), OpCode::T(0)], &[OpCode::NT(82), OpCode::T(9), OpCode::T(6)], &[OpCode::Exit(89), OpCode::T(12), OpCode::NT(65), OpCode::T(6), OpCode::T(2), OpCode::T(30)], &[OpCode::NT(83), OpCode::T(2), OpCode::T(30)], &[OpCode::Exit(91), OpCode::T(12), OpCode::NT(49), OpCode::T(6), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(49), OpCode::Exit(92), OpCode::T(6), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(93)], &[OpCode::NT(84), OpCode::T(2), OpCode::T(30)], &[OpCode::Loop(51), OpCode::Exit(95), OpCode::T(6), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(96)], &[OpCode::Exit(97), OpCode::NT(52), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(98), OpCode::T(12)], &[OpCode::Loop(53), OpCode::Exit(99), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(100), OpCode::T(12)], &[OpCode::NT(74), OpCode::Exit(101), OpCode::T(0)], &[OpCode::Loop(55), OpCode::Exit(102), OpCode::T(31), OpCode::T(11)], &[OpCode::Exit(103)], &[OpCode::NT(85), OpCode::T(31), OpCode::T(11)], &[OpCode::Loop(57), OpCode::Exit(105), OpCode::T(30)], &[OpCode::Loop(57), OpCode::Exit(106), OpCode::T(30), OpCode::T(10), OpCode::T(31)], &[OpCode::Exit(107)], &[OpCode::NT(86), OpCode::T(30)], &[OpCode::NT(87), OpCode::T(30), OpCode::T(10), OpCode::T(31)], &[OpCode::Loop(59), OpCode::Exit(110), OpCode::T(31), OpCode::T(10), OpCode::T(30), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(111)], &[OpCode::Loop(60), OpCode::Exit(112), OpCode::T(31), OpCode::T(10), OpCode::T(30), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(113)], &[OpCode::Loop(61), OpCode::Exit(114), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(115)], &[OpCode::NT(88), OpCode::T(6), OpCode::T(11)], &[OpCode::Loop(63), OpCode::Exit(117), OpCode::T(0)], &[OpCode::Loop(63), OpCode::Exit(118), OpCode::T(9), OpCode::T(6)], &[OpCode::Exit(119)], &[OpCode::NT(89), OpCode::T(0)], &[OpCode::NT(90), OpCode::T(9), OpCode::T(6)], &[OpCode::Loop(65), OpCode::Exit(122), OpCode::T(6), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(123)], &[OpCode::Loop(66), OpCode::Exit(124), OpCode::T(6), OpCode::T(13), OpCode::T(11)], &[OpCode::Exit(125)], &[OpCode::Loop(67), OpCode::Exit(126), OpCode::T(31), OpCode::T(11)], &[OpCode::Exit(127)], &[OpCode::Loop(68), OpCode::Exit(128), OpCode::NT(71), OpCode::T(3)], &[OpCode::Loop(68), OpCode::Exit(129), OpCode::NT(71), OpCode::T(6)], &[OpCode::Loop(68), OpCode::Exit(130), OpCode::NT(71), OpCode::T(1)], &[OpCode::Loop(68), OpCode::Exit(131), OpCode::NT(69), OpCode::T(0)], &[OpCode::Loop(68), OpCode::Exit(132), OpCode::NT(69), OpCode::T(9)], &[OpCode::Exit(133)], &[OpCode::NT(70), OpCode::Exit(134), OpCode::NT(73)], &[OpCode::Loop(70), OpCode::Exit(135), OpCode::NT(71), OpCode::T(3)], &[OpCode::Loop(70), OpCode::Exit(136), OpCode::NT(71), OpCode::T(6)], &[OpCode::Loop(70), OpCode::Exit(137), OpCode::NT(71), OpCode::T(1)], &[OpCode::Exit(138)], &[OpCode::NT(72), OpCode::Exit(139), OpCode::NT(73)], &[OpCode::Loop(72), OpCode::Exit(140), OpCode::NT(71), OpCode::T(3)], &[OpCode::Exit(141)], &[OpCode::Exit(142), OpCode::NT(73), OpCode::T(9)], &[OpCode::Exit(143), OpCode::T(7), OpCode::NT(29), OpCode::T(4)], &[OpCode::Exit(144), OpCode::T(30)], &[OpCode::Exit(145), OpCode::T(31)], &[OpCode::Loop(74), OpCode::Exit(146), OpCode::T(6), OpCode::T(11)], &[OpCode::Exit(147)], &[OpCode::Loop(9), OpCode::Exit(148)], &[OpCode::Exit(149)], &[OpCode::Loop(19), OpCode::Exit(150)], &[OpCode::Exit(151)], &[OpCode::Loop(19), OpCode::Exit(152)], &[OpCode::Exit(153)], &[OpCode::Exit(154), OpCode::T(12)], &[OpCode::Exit(155), OpCode::T(12), OpCode::NT(60), OpCode::T(31), OpCode::T(10), OpCode::T(30)], &[OpCode::Exit(156), OpCode::T(12)], &[OpCode::Exit(157), OpCode::T(12), OpCode::NT(25), OpCode::T(31), OpCode::T(10), OpCode::T(30)], &[OpCode::Loop(36), OpCode::Exit(158)], &[OpCode::Exit(159)], &[OpCode::Loop(45), OpCode::Exit(160)], &[OpCode::Exit(161)], &[OpCode::Loop(45), OpCode::Exit(162)], &[OpCode::Exit(163)], &[OpCode::Exit(164), OpCode::T(12), OpCode::NT(66), OpCode::T(6)], &[OpCode::Exit(165), OpCode::T(12)], &[OpCode::Exit(166), OpCode::T(12), OpCode::NT(51), OpCode::T(6)], &[OpCode::Exit(167), OpCode::T(12)], &[OpCode::Loop(56), OpCode::Exit(168)], &[OpCode::Exit(169)], &[OpCode::Loop(58), OpCode::Exit(170)], &[OpCode::Exit(171)], &[OpCode::Loop(58), OpCode::Exit(172)], &[OpCode::Exit(173)], &[OpCode::Loop(62), OpCode::Exit(174)], &[OpCode::Exit(175)], &[OpCode::Loop(64), OpCode::Exit(176)], &[OpCode::Exit(177)], &[OpCode::Loop(64), OpCode::Exit(178)], &[OpCode::Exit(179)]];
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
        /// `example -> "sep-list" sep_list`
        V13 { sep_list: SynSepList },
        /// `example -> "sep-list-opt" sep_list_opt`
        V14 { sep_list_opt: SynSepListOpt },
        /// `example -> "l-sep-list" l_sep_list`
        V15 { l_sep_list: SynLSepList },
        /// `example -> "l-sep-list-opt" l_sep_list_opt`
        V16 { l_sep_list_opt: SynLSepListOpt },
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
    pub enum CtxLSepList {
        /// `l_sep_list -> Id "=" (<L> Id ":" Num / "," "then")+ ";"`
        V1 { id: String, plus: SynLSepListI },
    }
    #[derive(Debug)]
    pub enum InitCtxLSepListI {
        /// first `<L> Id ":" Num / "," "then"` iteration in `l_sep_list -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";"`
        V1 { id: String, num: String },
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
    pub enum InitCtxLSepListOptI {
        /// first `<L> Id ":" Num / "," "then"` iteration in `l_sep_list_opt -> Id "=" ( ►► <L> Id ":" Num / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        V1 { id: String, num: String },
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
    pub enum CtxLRrecI {
        /// `l_rrec_i -> <L> "," Num l_rrec_i`
        V1 { num: String },
        /// `l_rrec_i -> ";"`
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
        /// `nv_example -> "l-rrec" nv_l_rrec`
        V6 { nv_l_rrec: SynNvLRrec },
        /// `nv_example -> "lrec" nv_lrec`
        V7 { nv_lrec: SynNvLrec },
        /// `nv_example -> "star-a" nv_star_a`
        V8 { nv_star_a: SynNvStarA },
        /// `nv_example -> "plus-a" nv_plus_a`
        V9 { nv_plus_a: SynNvPlusA },
        /// `nv_example -> "l-star-a" nv_l_star_a`
        V10 { nv_l_star_a: SynNvLStarA },
        /// `nv_example -> "l-plus-a" nv_l_plus_a`
        V11 { nv_l_plus_a: SynNvLPlusA },
        /// `nv_example -> "sep-list" nv_sep_list`
        V12 { nv_sep_list: SynNvSepList },
        /// `nv_example -> "sep-list-opt" nv_sep_list_opt`
        V13 { nv_sep_list_opt: SynNvSepListOpt },
        /// `nv_example -> "l-sep-list" nv_l_sep_list`
        V14 { nv_l_sep_list: SynNvLSepList },
        /// `nv_example -> "l-sep-list-opt" nv_l_sep_list_opt`
        V15 { nv_l_sep_list_opt: SynNvLSepListOpt },
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
        V1 { last_iteration: bool },
    }
    #[derive(Debug)]
    pub enum CtxNvRrec {
        /// `nv_rrec -> Id "=" "+" nv_rrec_i`
        V1 { id: String, nv_rrec_i: SynNvRrecI },
    }
    #[derive(Debug)]
    pub enum CtxNvLRrec {
        /// `nv_l_rrec -> Id "=" "+" nv_l_rrec_i`
        V1 { id: String, nv_l_rrec_i: SynNvLRrecI },
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
        V1 { last_iteration: bool },
        /// `"*" "-"` iteration in `nv_l_plus_a -> Id "=" "[" (<L> "+" |  ►► "*" "-" ◄◄ )+ "]" ";"`
        V2 { last_iteration: bool },
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
    pub enum InitCtxNvLSepListI {
        /// first `<L> "*" / "," "then"` iteration in `nv_l_sep_list -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";"`
        V1,
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
    pub enum InitCtxNvLSepListOptI {
        /// first `<L> "*" / "," "then"` iteration in `nv_l_sep_list_opt -> Id "=" ( ►► <L> "*" / "," "then" ◄◄ )+ ";" | Id "=" ";"`
        V1,
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
    pub enum CtxNvLRrecI {
        /// `nv_l_rrec_i -> <L> "," "*" nv_l_rrec_i`
        V1,
        /// `nv_l_rrec_i -> ";"`
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
    enum EnumSynValue { Text(SynText), I(SynI), NvI(SynNvI), Example(SynExample), Star(SynStar), Plus(SynPlus), LStar(SynLStar), LStarI(SynLStarI), LPlus(SynLPlus), LPlusI(SynLPlusI), Rrec(SynRrec), LRrec(SynLRrec), Lrec(SynLrec), Amb(SynAmb), StarA(SynStarA), PlusA(SynPlusA), LStarA(SynLStarA), LStarAI(SynLStarAI), LPlusA(SynLPlusA), LPlusAI(SynLPlusAI), SepList(SynSepList), SepListOpt(SynSepListOpt), LSepList(SynLSepList), LSepListI(SynLSepListI), LSepListOpt(SynLSepListOpt), LSepListOptI(SynLSepListOptI), RrecI(SynRrecI), LRrecI(SynLRrecI), LrecI(SynLrecI), AmbI(SynAmbI), NvExample(SynNvExample), NvStar(SynNvStar), NvPlus(SynNvPlus), NvLStar(SynNvLStar), NvLStarI(SynNvLStarI), NvLPlus(SynNvLPlus), NvLPlusI(SynNvLPlusI), NvRrec(SynNvRrec), NvLRrec(SynNvLRrec), NvLrec(SynNvLrec), NvStarA(SynNvStarA), NvPlusA(SynNvPlusA), NvLStarA(SynNvLStarA), NvLStarAI(SynNvLStarAI), NvLPlusA(SynNvLPlusA), NvLPlusAI(SynNvLPlusAI), NvSepList(SynNvSepList), NvSepListOpt(SynNvSepListOpt), NvLSepList(SynNvLSepList), NvLSepListI(SynNvLSepListI), NvLSepListOpt(SynNvLSepListOpt), NvLSepListOptI(SynNvLSepListOptI), NvRrecI(SynNvRrecI), NvLRrecI(SynNvLRrecI), NvLrecI(SynNvLrecI), Star1(SynStar1), Plus1(SynPlus1), StarA1(SynStarA1), PlusA1(SynPlusA1), SepList1(SynSepList1), SepListOpt1(SynSepListOpt1) }

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
        fn get_l_rrec(self) -> SynLRrec {
            if let EnumSynValue::LRrec(val) = self { val } else { panic!() }
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
        fn get_l_rrec_i(self) -> SynLRrecI {
            if let EnumSynValue::LRrecI(val) = self { val } else { panic!() }
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
        fn get_nv_l_rrec(self) -> SynNvLRrec {
            if let EnumSynValue::NvLRrec(val) = self { val } else { panic!() }
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
        fn get_nv_l_rrec_i(self) -> SynNvLRrecI {
            if let EnumSynValue::NvLRrecI(val) = self { val } else { panic!() }
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
        fn exit(&mut self, text: SynText, span: PosSpan) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_text(&mut self) {}
        fn exit_text(&mut self, ctx: CtxText, spans: Vec<PosSpan>) -> SynText;
        fn init_i(&mut self) -> SynI;
        fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_i(&mut self, acc: &mut SynI) {}
        fn init_nv_i(&mut self) -> SynNvI;
        fn exit_nv_i(&mut self, acc: &mut SynNvI, ctx: CtxNvI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_nv_i(&mut self, acc: &mut SynNvI) {}
        fn init_example(&mut self) {}
        fn exit_example(&mut self, ctx: CtxExample, spans: Vec<PosSpan>) -> SynExample;
        fn init_star(&mut self) {}
        fn exit_star(&mut self, ctx: CtxStar, spans: Vec<PosSpan>) -> SynStar;
        fn init_plus(&mut self) {}
        fn exit_plus(&mut self, ctx: CtxPlus, spans: Vec<PosSpan>) -> SynPlus;
        fn init_l_star(&mut self) {}
        fn exit_l_star(&mut self, ctx: CtxLStar, spans: Vec<PosSpan>) -> SynLStar;
        fn init_l_star_i(&mut self) -> SynLStarI;
        fn exit_l_star_i(&mut self, acc: &mut SynLStarI, ctx: CtxLStarI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_l_star_i(&mut self, acc: &mut SynLStarI) {}
        fn init_l_plus(&mut self) {}
        fn exit_l_plus(&mut self, ctx: CtxLPlus, spans: Vec<PosSpan>) -> SynLPlus;
        fn init_l_plus_i(&mut self) -> SynLPlusI;
        fn exit_l_plus_i(&mut self, acc: &mut SynLPlusI, ctx: CtxLPlusI, spans: Vec<PosSpan>);
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
        fn init_l_star_a_i(&mut self) -> SynLStarAI;
        fn exit_l_star_a_i(&mut self, acc: &mut SynLStarAI, ctx: CtxLStarAI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_l_star_a_i(&mut self, acc: &mut SynLStarAI) {}
        fn init_l_plus_a(&mut self) {}
        fn exit_l_plus_a(&mut self, ctx: CtxLPlusA, spans: Vec<PosSpan>) -> SynLPlusA;
        fn init_l_plus_a_i(&mut self) -> SynLPlusAI;
        fn exit_l_plus_a_i(&mut self, acc: &mut SynLPlusAI, ctx: CtxLPlusAI, spans: Vec<PosSpan>);
        fn init_sep_list(&mut self) {}
        fn exit_sep_list(&mut self, ctx: CtxSepList, spans: Vec<PosSpan>) -> SynSepList;
        fn init_sep_list_opt(&mut self) {}
        fn exit_sep_list_opt(&mut self, ctx: CtxSepListOpt, spans: Vec<PosSpan>) -> SynSepListOpt;
        fn init_l_sep_list(&mut self) {}
        fn exit_l_sep_list(&mut self, ctx: CtxLSepList, spans: Vec<PosSpan>) -> SynLSepList;
        fn init_l_sep_list_i(&mut self, ctx: InitCtxLSepListI, spans: Vec<PosSpan>) -> SynLSepListI;
        fn exit_l_sep_list_i(&mut self, acc: &mut SynLSepListI, ctx: CtxLSepListI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_l_sep_list_i(&mut self, acc: &mut SynLSepListI) {}
        fn init_l_sep_list_opt(&mut self) {}
        fn exit_l_sep_list_opt(&mut self, ctx: CtxLSepListOpt, spans: Vec<PosSpan>) -> SynLSepListOpt;
        fn init_l_sep_list_opt_i(&mut self, ctx: InitCtxLSepListOptI, spans: Vec<PosSpan>) -> SynLSepListOptI;
        fn exit_l_sep_list_opt_i(&mut self, acc: &mut SynLSepListOptI, ctx: CtxLSepListOptI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_l_sep_list_opt_i(&mut self, acc: &mut SynLSepListOptI) {}
        fn init_rrec_i(&mut self) {}
        fn exit_rrec_i(&mut self, ctx: CtxRrecI, spans: Vec<PosSpan>) -> SynRrecI;
        fn init_l_rrec_i(&mut self) -> SynLRrecI;
        fn exit_l_rrec_i(&mut self, acc: &mut SynLRrecI, ctx: CtxLRrecI, spans: Vec<PosSpan>);
        fn init_lrec_i(&mut self) {}
        fn exit_lrec_i(&mut self, ctx: CtxLrecI, spans: Vec<PosSpan>) -> SynLrecI;
        #[allow(unused_variables)]
        fn exitloop_lrec_i(&mut self, lrec_i: &mut SynLrecI) {}
        fn init_amb_i(&mut self) {}
        fn exit_amb_i(&mut self, ctx: CtxAmbI, spans: Vec<PosSpan>) -> SynAmbI;
        fn init_nv_example(&mut self) {}
        fn exit_nv_example(&mut self, ctx: CtxNvExample, spans: Vec<PosSpan>) -> SynNvExample;
        fn init_nv_star(&mut self) {}
        fn exit_nv_star(&mut self, ctx: CtxNvStar, spans: Vec<PosSpan>) -> SynNvStar;
        fn init_nv_plus(&mut self) {}
        fn exit_nv_plus(&mut self, ctx: CtxNvPlus, spans: Vec<PosSpan>) -> SynNvPlus;
        fn init_nv_l_star(&mut self) {}
        fn exit_nv_l_star(&mut self, ctx: CtxNvLStar, spans: Vec<PosSpan>) -> SynNvLStar;
        fn init_nv_l_star_i(&mut self) -> SynNvLStarI;
        fn exit_nv_l_star_i(&mut self, acc: &mut SynNvLStarI, ctx: CtxNvLStarI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_nv_l_star_i(&mut self, acc: &mut SynNvLStarI) {}
        fn init_nv_l_plus(&mut self) {}
        fn exit_nv_l_plus(&mut self, ctx: CtxNvLPlus, spans: Vec<PosSpan>) -> SynNvLPlus;
        fn init_nv_l_plus_i(&mut self) -> SynNvLPlusI;
        fn exit_nv_l_plus_i(&mut self, acc: &mut SynNvLPlusI, ctx: CtxNvLPlusI, spans: Vec<PosSpan>);
        fn init_nv_rrec(&mut self) {}
        fn exit_nv_rrec(&mut self, ctx: CtxNvRrec, spans: Vec<PosSpan>) -> SynNvRrec;
        fn init_nv_l_rrec(&mut self) {}
        fn exit_nv_l_rrec(&mut self, ctx: CtxNvLRrec, spans: Vec<PosSpan>) -> SynNvLRrec;
        fn init_nv_lrec(&mut self) {}
        fn exit_nv_lrec(&mut self, ctx: CtxNvLrec, spans: Vec<PosSpan>) -> SynNvLrec;
        fn init_nv_star_a(&mut self) {}
        fn exit_nv_star_a(&mut self, ctx: CtxNvStarA, spans: Vec<PosSpan>) -> SynNvStarA;
        fn init_nv_plus_a(&mut self) {}
        fn exit_nv_plus_a(&mut self, ctx: CtxNvPlusA, spans: Vec<PosSpan>) -> SynNvPlusA;
        fn init_nv_l_star_a(&mut self) {}
        fn exit_nv_l_star_a(&mut self, ctx: CtxNvLStarA, spans: Vec<PosSpan>) -> SynNvLStarA;
        fn init_nv_l_star_a_i(&mut self) -> SynNvLStarAI;
        fn exit_nv_l_star_a_i(&mut self, acc: &mut SynNvLStarAI, ctx: CtxNvLStarAI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_nv_l_star_a_i(&mut self, acc: &mut SynNvLStarAI) {}
        fn init_nv_l_plus_a(&mut self) {}
        fn exit_nv_l_plus_a(&mut self, ctx: CtxNvLPlusA, spans: Vec<PosSpan>) -> SynNvLPlusA;
        fn init_nv_l_plus_a_i(&mut self) -> SynNvLPlusAI;
        fn exit_nv_l_plus_a_i(&mut self, acc: &mut SynNvLPlusAI, ctx: CtxNvLPlusAI, spans: Vec<PosSpan>);
        fn init_nv_sep_list(&mut self) {}
        fn exit_nv_sep_list(&mut self, ctx: CtxNvSepList, spans: Vec<PosSpan>) -> SynNvSepList;
        fn init_nv_sep_list_opt(&mut self) {}
        fn exit_nv_sep_list_opt(&mut self, ctx: CtxNvSepListOpt, spans: Vec<PosSpan>) -> SynNvSepListOpt;
        fn init_nv_l_sep_list(&mut self) {}
        fn exit_nv_l_sep_list(&mut self, ctx: CtxNvLSepList, spans: Vec<PosSpan>) -> SynNvLSepList;
        fn init_nv_l_sep_list_i(&mut self, ctx: InitCtxNvLSepListI, spans: Vec<PosSpan>) -> SynNvLSepListI;
        fn exit_nv_l_sep_list_i(&mut self, acc: &mut SynNvLSepListI, ctx: CtxNvLSepListI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_nv_l_sep_list_i(&mut self, acc: &mut SynNvLSepListI) {}
        fn init_nv_l_sep_list_opt(&mut self) {}
        fn exit_nv_l_sep_list_opt(&mut self, ctx: CtxNvLSepListOpt, spans: Vec<PosSpan>) -> SynNvLSepListOpt;
        fn init_nv_l_sep_list_opt_i(&mut self, ctx: InitCtxNvLSepListOptI, spans: Vec<PosSpan>) -> SynNvLSepListOptI;
        fn exit_nv_l_sep_list_opt_i(&mut self, acc: &mut SynNvLSepListOptI, ctx: CtxNvLSepListOptI, spans: Vec<PosSpan>);
        #[allow(unused_variables)]
        fn exitloop_nv_l_sep_list_opt_i(&mut self, acc: &mut SynNvLSepListOptI) {}
        fn init_nv_rrec_i(&mut self) {}
        fn exit_nv_rrec_i(&mut self, ctx: CtxNvRrecI, spans: Vec<PosSpan>) -> SynNvRrecI;
        fn init_nv_l_rrec_i(&mut self) -> SynNvLRrecI;
        fn exit_nv_l_rrec_i(&mut self, acc: &mut SynNvLRrecI, ctx: CtxNvLRrecI, spans: Vec<PosSpan>);
        fn init_nv_lrec_i(&mut self) {}
        fn exit_nv_lrec_i(&mut self, ctx: CtxNvLrecI, spans: Vec<PosSpan>) -> SynNvLrecI;
        #[allow(unused_variables)]
        fn exitloop_nv_lrec_i(&mut self, nv_lrec_i: &mut SynNvLrecI) {}
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
                    if matches!(nt, 1 | 2 | 7 | 9 | 17 | 19 | 27 | 34 | 36 | 43 | 45 | 53 | 55 ..= 58 | 61 ..= 64) {
                        self.stack_span.push(PosSpan::empty());
                    }
                    match nt {
                        0 => self.listener.init_text(),               // text
                        1 => self.init_i(),                           // i
                        2 => self.init_nv_i(),                        // nv_i
                        3 => self.listener.init_example(),            // example
                        4 => self.listener.init_star(),               // star
                        55 => self.init_star1(),                      // star_1
                        5 => self.listener.init_plus(),               // plus
                        56 => self.init_plus1(),                      // plus_1
                        85 => {}                                      // plus_2
                        6 => self.listener.init_l_star(),             // l_star
                        7 => self.init_l_star_i(),                    // l_star_i
                        8 => self.listener.init_l_plus(),             // l_plus
                        9 => self.init_l_plus_i(),                    // l_plus_i
                        75 => {}                                      // l_plus_i_1
                        10 => self.listener.init_rrec(),              // rrec
                        11 => self.listener.init_l_rrec(),            // l_rrec
                        12 => self.listener.init_lrec(),              // lrec
                        13 => self.listener.init_amb(),               // amb
                        14 => self.listener.init_star_a(),            // star_a
                        57 => self.init_star_a1(),                    // star_a_1
                        15 => self.listener.init_plus_a(),            // plus_a
                        58 => self.init_plus_a1(),                    // plus_a_1
                        86 | 87 => {}                                 // plus_a_2, plus_a_3
                        16 => self.listener.init_l_star_a(),          // l_star_a
                        17 => self.init_l_star_a_i(),                 // l_star_a_i
                        18 => self.listener.init_l_plus_a(),          // l_plus_a
                        19 => self.init_l_plus_a_i(),                 // l_plus_a_i
                        76 | 77 => {}                                 // l_plus_a_i_1, l_plus_a_i_2
                        20 => self.listener.init_sep_list(),          // sep_list
                        59 => self.init_sep_list1(),                  // sep_list_1
                        21 => self.listener.init_sep_list_opt(),      // sep_list_opt
                        60 => self.init_sep_list_opt1(),              // sep_list_opt_1
                        78 => {}                                      // sep_list_opt_2
                        22 => self.listener.init_l_sep_list(),        // l_sep_list
                        23 => self.init_l_sep_list_i(),               // l_sep_list_i
                        24 => self.listener.init_l_sep_list_opt(),    // l_sep_list_opt
                        25 => self.init_l_sep_list_opt_i(),           // l_sep_list_opt_i
                        79 => {}                                      // l_sep_list_opt_1
                        26 => self.listener.init_rrec_i(),            // rrec_i
                        27 => self.init_l_rrec_i(),                   // l_rrec_i
                        28 => self.listener.init_lrec_i(),            // lrec_i
                        67 => {}                                      // lrec_i_1
                        29 => self.listener.init_amb_i(),             // amb_i
                        68 ..= 73 => {}                               // amb_i_1, amb_i_2, amb_i_3, amb_i_4, amb_i_5, amb_i_6
                        30 => self.listener.init_nv_example(),        // nv_example
                        31 => self.listener.init_nv_star(),           // nv_star
                        61 => {}                                      // nv_star_1
                        32 => self.listener.init_nv_plus(),           // nv_plus
                        62 => {}                                      // nv_plus_1
                        88 => {}                                      // nv_plus_2
                        33 => self.listener.init_nv_l_star(),         // nv_l_star
                        34 => self.init_nv_l_star_i(),                // nv_l_star_i
                        35 => self.listener.init_nv_l_plus(),         // nv_l_plus
                        36 => self.init_nv_l_plus_i(),                // nv_l_plus_i
                        80 => {}                                      // nv_l_plus_i_1
                        37 => self.listener.init_nv_rrec(),           // nv_rrec
                        38 => self.listener.init_nv_l_rrec(),         // nv_l_rrec
                        39 => self.listener.init_nv_lrec(),           // nv_lrec
                        40 => self.listener.init_nv_star_a(),         // nv_star_a
                        63 => {}                                      // nv_star_a_1
                        41 => self.listener.init_nv_plus_a(),         // nv_plus_a
                        64 => {}                                      // nv_plus_a_1
                        89 | 90 => {}                                 // nv_plus_a_2, nv_plus_a_3
                        42 => self.listener.init_nv_l_star_a(),       // nv_l_star_a
                        43 => self.init_nv_l_star_a_i(),              // nv_l_star_a_i
                        44 => self.listener.init_nv_l_plus_a(),       // nv_l_plus_a
                        45 => self.init_nv_l_plus_a_i(),              // nv_l_plus_a_i
                        81 | 82 => {}                                 // nv_l_plus_a_i_1, nv_l_plus_a_i_2
                        46 => self.listener.init_nv_sep_list(),       // nv_sep_list
                        65 => self.init_nv_sep_list1(),               // nv_sep_list_1
                        47 => self.listener.init_nv_sep_list_opt(),   // nv_sep_list_opt
                        66 => self.init_nv_sep_list_opt1(),           // nv_sep_list_opt_1
                        83 => {}                                      // nv_sep_list_opt_2
                        48 => self.listener.init_nv_l_sep_list(),     // nv_l_sep_list
                        49 => self.init_nv_l_sep_list_i(),            // nv_l_sep_list_i
                        50 => self.listener.init_nv_l_sep_list_opt(), // nv_l_sep_list_opt
                        51 => self.init_nv_l_sep_list_opt_i(),        // nv_l_sep_list_opt_i
                        84 => {}                                      // nv_l_sep_list_opt_1
                        52 => self.listener.init_nv_rrec_i(),         // nv_rrec_i
                        53 => self.init_nv_l_rrec_i(),                // nv_l_rrec_i
                        54 => self.listener.init_nv_lrec_i(),         // nv_lrec_i
                        74 => {}                                      // nv_lrec_i_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.exit_text(),                      // text -> i ";" nv_i
                        1 => self.exit_i(),                         // i -> <L> example i
                        2 => self.exitloop_i(),                     // i -> <L> ε
                        3 => self.exit_nv_i(),                      // nv_i -> <L> nv_example nv_i
                        4 => self.exitloop_nv_i(),                  // nv_i -> <L> ε
                        5 |                                         // example -> "star" star
                        6 |                                         // example -> "plus" plus
                        7 |                                         // example -> "l-star" l_star
                        8 |                                         // example -> "l-plus" l_plus
                        9 |                                         // example -> "rrec" rrec
                        10 |                                        // example -> "l-rrec" l_rrec
                        11 |                                        // example -> "lrec" lrec
                        12 |                                        // example -> "amb" amb
                        13 |                                        // example -> "star-a" star_a
                        14 |                                        // example -> "plus-a" plus_a
                        15 |                                        // example -> "l-star-a" l_star_a
                        16 |                                        // example -> "l-plus-a" l_plus_a
                        17 |                                        // example -> "sep-list" sep_list
                        18 |                                        // example -> "sep-list-opt" sep_list_opt
                        19 |                                        // example -> "l-sep-list" l_sep_list
                        20 => self.exit_example(alt_id),            // example -> "l-sep-list-opt" l_sep_list_opt
                        21 => self.exit_star(),                     // star -> Id "=" Id star_1 ";"
                        102 => self.exit_star1(),                   // star_1 -> "," Num star_1
                        103 => {}                                   // star_1 -> ε
                        22 => self.exit_plus(),                     // plus -> Id "=" Num plus_1 ";"
                        168 |                                       // plus_2 -> plus_1
                        169 => self.exit_plus1(),                   // plus_2 -> ε
                     /* 104 */                                      // plus_1 -> "," Num plus_2 (never called)
                        23 => self.exit_l_star(),                   // l_star -> Id "=" Num l_star_i ";"
                        24 => self.exit_l_star_i(),                 // l_star_i -> <L> "," Num l_star_i
                        25 => self.exitloop_l_star_i(),             // l_star_i -> <L> ε
                        26 => self.exit_l_plus(),                   // l_plus -> Id "=" Num l_plus_i ";"
                        148 |                                       // l_plus_i_1 -> l_plus_i
                        149 => self.exit_l_plus_i(alt_id),          // l_plus_i_1 -> ε
                     /* 27 */                                       // l_plus_i -> <L> "," Num l_plus_i_1 (never called)
                        28 => self.exit_rrec(),                     // rrec -> Id "=" Num rrec_i
                        29 => self.exit_l_rrec(),                   // l_rrec -> Id "=" Num l_rrec_i
                        30 => self.exit_lrec(),                     // lrec -> Id "=" lrec_i ";"
                        31 => self.exit_amb(),                      // amb -> Id "=" amb_i ";"
                        32 => self.exit_star_a(),                   // star_a -> Id "=" "[" star_a_1 "]" ";"
                        105 |                                       // star_a_1 -> Id star_a_1
                        106 => self.exit_star_a1(alt_id),           // star_a_1 -> Num ":" Id star_a_1
                        107 => {}                                   // star_a_1 -> ε
                        33 => self.exit_plus_a(),                   // plus_a -> Id "=" "[" plus_a_1 "]" ";"
                        170 |                                       // plus_a_2 -> plus_a_1
                        171 |                                       // plus_a_2 -> ε
                        172 |                                       // plus_a_3 -> plus_a_1
                        173 => self.exit_plus_a1(alt_id),           // plus_a_3 -> ε
                     /* 108 */                                      // plus_a_1 -> Id plus_a_2 (never called)
                     /* 109 */                                      // plus_a_1 -> Num ":" Id plus_a_3 (never called)
                        34 => self.exit_l_star_a(),                 // l_star_a -> Id "=" "[" l_star_a_i "]" ";"
                        35 |                                        // l_star_a_i -> <L> Id l_star_a_i
                        36 => self.exit_l_star_a_i(alt_id),         // l_star_a_i -> <L> Num ":" Id l_star_a_i
                        37 => self.exitloop_l_star_a_i(),           // l_star_a_i -> <L> ε
                        38 => self.exit_l_plus_a(),                 // l_plus_a -> Id "=" "[" l_plus_a_i "]" ";"
                        150 |                                       // l_plus_a_i_1 -> l_plus_a_i
                        151 |                                       // l_plus_a_i_1 -> ε
                        152 |                                       // l_plus_a_i_2 -> l_plus_a_i
                        153 => self.exit_l_plus_a_i(alt_id),        // l_plus_a_i_2 -> ε
                     /* 39 */                                       // l_plus_a_i -> <L> Id l_plus_a_i_1 (never called)
                     /* 40 */                                       // l_plus_a_i -> <L> Num ":" Id l_plus_a_i_2 (never called)
                        41 => self.exit_sep_list(),                 // sep_list -> Id "=" Id ":" Num sep_list_1 ";"
                        110 => self.exit_sep_list1(),               // sep_list_1 -> "," "then" Id ":" Num sep_list_1
                        111 => {}                                   // sep_list_1 -> ε
                        154 |                                       // sep_list_opt_2 -> ";"
                        155 => self.exit_sep_list_opt(alt_id),      // sep_list_opt_2 -> Id ":" Num sep_list_opt_1 ";"
                        112 => self.exit_sep_list_opt1(),           // sep_list_opt_1 -> "," "then" Id ":" Num sep_list_opt_1
                        113 => {}                                   // sep_list_opt_1 -> ε
                     /* 42 */                                       // sep_list_opt -> Id "=" sep_list_opt_2 (never called)
                        43 => self.exit_l_sep_list(),               // l_sep_list -> Id "=" Id ":" Num l_sep_list_i ";"
                        44 => self.exit_l_sep_list_i(),             // l_sep_list_i -> <L> "," "then" Id ":" Num l_sep_list_i
                        45 => self.exitloop_l_sep_list_i(),         // l_sep_list_i -> <L> ε
                        156 |                                       // l_sep_list_opt_1 -> ";"
                        157 => self.exit_l_sep_list_opt(alt_id),    // l_sep_list_opt_1 -> Id ":" Num l_sep_list_opt_i ";"
                        47 => self.exit_l_sep_list_opt_i(),         // l_sep_list_opt_i -> <L> "," "then" Id ":" Num l_sep_list_opt_i
                        48 => self.exitloop_l_sep_list_opt_i(),     // l_sep_list_opt_i -> <L> ε
                     /* 46 */                                       // l_sep_list_opt -> Id "=" l_sep_list_opt_1 (never called)
                        49 |                                        // rrec_i -> "," Num rrec_i
                        50 => self.exit_rrec_i(alt_id),             // rrec_i -> ";"
                        51 |                                        // l_rrec_i -> <L> "," Num l_rrec_i
                        52 => self.exit_l_rrec_i(alt_id),           // l_rrec_i -> <L> ";"
                        53 => self.inter_lrec_i(),                  // lrec_i -> Num lrec_i_1
                        126 => self.exit_lrec_i1(),                 // lrec_i_1 -> "," Num lrec_i_1
                        127 => self.exitloop_lrec_i1(),             // lrec_i_1 -> ε
                        128 |                                       // amb_i_1 -> <R> "^" amb_i_4 amb_i_1
                        129 |                                       // amb_i_1 -> "*" amb_i_4 amb_i_1
                        130 |                                       // amb_i_1 -> "/" amb_i_4 amb_i_1
                        131 |                                       // amb_i_1 -> "+" amb_i_2 amb_i_1
                        132 => self.exit_amb_i1(alt_id),            // amb_i_1 -> "-" amb_i_2 amb_i_1
                        135 |                                       // amb_i_3 -> <R> "^" amb_i_4 amb_i_3 (duplicate of 128)
                        140 => self.exit_amb_i1(128),               // amb_i_5 -> <R> "^" amb_i_4 amb_i_5 (duplicate of 128)
                        136 => self.exit_amb_i1(129),               // amb_i_3 -> "*" amb_i_4 amb_i_3 (duplicate of 129)
                        137 => self.exit_amb_i1(130),               // amb_i_3 -> "/" amb_i_4 amb_i_3 (duplicate of 130)
                        142 |                                       // amb_i_6 -> "-" amb_i_6
                        143 |                                       // amb_i_6 -> "(" amb_i ")"
                        144 |                                       // amb_i_6 -> Id
                        145 => self.exit_amb_i6(alt_id),            // amb_i_6 -> Num
                        54 => {}                                    // amb_i -> amb_i_6 amb_i_1 (not used)
                        133 => {}                                   // amb_i_1 -> ε (not used)
                        134 => {}                                   // amb_i_2 -> amb_i_6 amb_i_3 (not used)
                        138 => {}                                   // amb_i_3 -> ε (not used)
                        139 => {}                                   // amb_i_4 -> amb_i_6 amb_i_5 (not used)
                        141 => {}                                   // amb_i_5 -> ε (not used)
                        55 |                                        // nv_example -> "star" nv_star
                        56 |                                        // nv_example -> "plus" nv_plus
                        57 |                                        // nv_example -> "l-star" nv_l_star
                        58 |                                        // nv_example -> "l-plus" nv_l_plus
                        59 |                                        // nv_example -> "rrec" nv_rrec
                        60 |                                        // nv_example -> "l-rrec" nv_l_rrec
                        61 |                                        // nv_example -> "lrec" nv_lrec
                        62 |                                        // nv_example -> "star-a" nv_star_a
                        63 |                                        // nv_example -> "plus-a" nv_plus_a
                        64 |                                        // nv_example -> "l-star-a" nv_l_star_a
                        65 |                                        // nv_example -> "l-plus-a" nv_l_plus_a
                        66 |                                        // nv_example -> "sep-list" nv_sep_list
                        67 |                                        // nv_example -> "sep-list-opt" nv_sep_list_opt
                        68 |                                        // nv_example -> "l-sep-list" nv_l_sep_list
                        69 => self.exit_nv_example(alt_id),         // nv_example -> "l-sep-list-opt" nv_l_sep_list_opt
                        70 => self.exit_nv_star(),                  // nv_star -> Id "=" "+" nv_star_1 ";"
                        114 => self.exit_nv_star1(),                // nv_star_1 -> "," "*" nv_star_1
                        115 => {}                                   // nv_star_1 -> ε
                        71 => self.exit_nv_plus(),                  // nv_plus -> Id "=" "+" nv_plus_1 ";"
                        174 |                                       // nv_plus_2 -> nv_plus_1
                        175 => self.exit_nv_plus1(),                // nv_plus_2 -> ε
                     /* 116 */                                      // nv_plus_1 -> "," "*" nv_plus_2 (never called)
                        72 => self.exit_nv_l_star(),                // nv_l_star -> Id "=" "+" nv_l_star_i ";"
                        73 => self.exit_nv_l_star_i(),              // nv_l_star_i -> <L> "," "*" nv_l_star_i
                        74 => self.exitloop_nv_l_star_i(),          // nv_l_star_i -> <L> ε
                        75 => self.exit_nv_l_plus(),                // nv_l_plus -> Id "=" "+" nv_l_plus_i ";"
                        158 |                                       // nv_l_plus_i_1 -> nv_l_plus_i
                        159 => self.exit_nv_l_plus_i(alt_id),       // nv_l_plus_i_1 -> ε
                     /* 76 */                                       // nv_l_plus_i -> <L> "," "*" nv_l_plus_i_1 (never called)
                        77 => self.exit_nv_rrec(),                  // nv_rrec -> Id "=" "+" nv_rrec_i
                        78 => self.exit_nv_l_rrec(),                // nv_l_rrec -> Id "=" "+" nv_l_rrec_i
                        79 => self.exit_nv_lrec(),                  // nv_lrec -> Id "=" nv_lrec_i ";"
                        80 => self.exit_nv_star_a(),                // nv_star_a -> Id "=" "[" nv_star_a_1 "]" ";"
                        117 |                                       // nv_star_a_1 -> "+" nv_star_a_1
                        118 => self.exit_nv_star_a1(alt_id),        // nv_star_a_1 -> "*" "-" nv_star_a_1
                        119 => {}                                   // nv_star_a_1 -> ε
                        81 => self.exit_nv_plus_a(),                // nv_plus_a -> Id "=" "[" nv_plus_a_1 "]" ";"
                        176 |                                       // nv_plus_a_2 -> nv_plus_a_1
                        177 |                                       // nv_plus_a_2 -> ε
                        178 |                                       // nv_plus_a_3 -> nv_plus_a_1
                        179 => self.exit_nv_plus_a1(alt_id),        // nv_plus_a_3 -> ε
                     /* 120 */                                      // nv_plus_a_1 -> "+" nv_plus_a_2 (never called)
                     /* 121 */                                      // nv_plus_a_1 -> "*" "-" nv_plus_a_3 (never called)
                        82 => self.exit_nv_l_star_a(),              // nv_l_star_a -> Id "=" "[" nv_l_star_a_i "]" ";"
                        83 |                                        // nv_l_star_a_i -> <L> "+" nv_l_star_a_i
                        84 => self.exit_nv_l_star_a_i(alt_id),      // nv_l_star_a_i -> <L> "*" "-" nv_l_star_a_i
                        85 => self.exitloop_nv_l_star_a_i(),        // nv_l_star_a_i -> <L> ε
                        86 => self.exit_nv_l_plus_a(),              // nv_l_plus_a -> Id "=" "[" nv_l_plus_a_i "]" ";"
                        160 |                                       // nv_l_plus_a_i_1 -> nv_l_plus_a_i
                        161 |                                       // nv_l_plus_a_i_1 -> ε
                        162 |                                       // nv_l_plus_a_i_2 -> nv_l_plus_a_i
                        163 => self.exit_nv_l_plus_a_i(alt_id),     // nv_l_plus_a_i_2 -> ε
                     /* 87 */                                       // nv_l_plus_a_i -> <L> "+" nv_l_plus_a_i_1 (never called)
                     /* 88 */                                       // nv_l_plus_a_i -> <L> "*" "-" nv_l_plus_a_i_2 (never called)
                        89 => self.exit_nv_sep_list(),              // nv_sep_list -> Id "=" "*" nv_sep_list_1 ";"
                        122 => self.exit_nv_sep_list1(),            // nv_sep_list_1 -> "," "then" "*" nv_sep_list_1
                        123 => {}                                   // nv_sep_list_1 -> ε
                        164 |                                       // nv_sep_list_opt_2 -> "*" nv_sep_list_opt_1 ";"
                        165 => self.exit_nv_sep_list_opt(alt_id),   // nv_sep_list_opt_2 -> ";"
                        124 => self.exit_nv_sep_list_opt1(),        // nv_sep_list_opt_1 -> "," "then" "*" nv_sep_list_opt_1
                        125 => {}                                   // nv_sep_list_opt_1 -> ε
                     /* 90 */                                       // nv_sep_list_opt -> Id "=" nv_sep_list_opt_2 (never called)
                        91 => self.exit_nv_l_sep_list(),            // nv_l_sep_list -> Id "=" "*" nv_l_sep_list_i ";"
                        92 => self.exit_nv_l_sep_list_i(),          // nv_l_sep_list_i -> <L> "," "then" "*" nv_l_sep_list_i
                        93 => self.exitloop_nv_l_sep_list_i(),      // nv_l_sep_list_i -> <L> ε
                        166 |                                       // nv_l_sep_list_opt_1 -> "*" nv_l_sep_list_opt_i ";"
                        167 => self.exit_nv_l_sep_list_opt(alt_id), // nv_l_sep_list_opt_1 -> ";"
                        95 => self.exit_nv_l_sep_list_opt_i(),      // nv_l_sep_list_opt_i -> <L> "," "then" "*" nv_l_sep_list_opt_i
                        96 => self.exitloop_nv_l_sep_list_opt_i(),  // nv_l_sep_list_opt_i -> <L> ε
                     /* 94 */                                       // nv_l_sep_list_opt -> Id "=" nv_l_sep_list_opt_1 (never called)
                        97 |                                        // nv_rrec_i -> "," "*" nv_rrec_i
                        98 => self.exit_nv_rrec_i(alt_id),          // nv_rrec_i -> ";"
                        99 |                                        // nv_l_rrec_i -> <L> "," "*" nv_l_rrec_i
                        100 => self.exit_nv_l_rrec_i(alt_id),       // nv_l_rrec_i -> <L> ";"
                        101 => self.inter_nv_lrec_i(),              // nv_lrec_i -> "+" nv_lrec_i_1
                        146 => self.exit_nv_lrec_i1(),              // nv_lrec_i_1 -> "," "*" nv_lrec_i_1
                        147 => self.exitloop_nv_lrec_i1(),          // nv_lrec_i_1 -> ε
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
        }

        fn exit_i(&mut self) {
            let example = self.stack.pop().unwrap().get_example();
            let ctx = CtxI::V1 { example };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::I(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_i(acc, ctx, spans);
        }

        fn exitloop_i(&mut self) {
            let EnumSynValue::I(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_i(acc);
        }

        fn init_nv_i(&mut self) {
            let val = self.listener.init_nv_i();
            self.stack.push(EnumSynValue::NvI(val));
        }

        fn exit_nv_i(&mut self) {
            let nv_example = self.stack.pop().unwrap().get_nv_example();
            let ctx = CtxNvI::V1 { nv_example };
            let spans = self.stack_span.drain(self.stack_span.len() - 2 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_i(acc, ctx, spans);
        }

        fn exitloop_nv_i(&mut self) {
            let EnumSynValue::NvI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_nv_i(acc);
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
                    let l_rrec = self.stack.pop().unwrap().get_l_rrec();
                    (2, CtxExample::V6 { l_rrec })
                }
                11 => {
                    let lrec = self.stack.pop().unwrap().get_lrec();
                    (2, CtxExample::V7 { lrec })
                }
                12 => {
                    let amb = self.stack.pop().unwrap().get_amb();
                    (2, CtxExample::V8 { amb })
                }
                13 => {
                    let star_a = self.stack.pop().unwrap().get_star_a();
                    (2, CtxExample::V9 { star_a })
                }
                14 => {
                    let plus_a = self.stack.pop().unwrap().get_plus_a();
                    (2, CtxExample::V10 { plus_a })
                }
                15 => {
                    let l_star_a = self.stack.pop().unwrap().get_l_star_a();
                    (2, CtxExample::V11 { l_star_a })
                }
                16 => {
                    let l_plus_a = self.stack.pop().unwrap().get_l_plus_a();
                    (2, CtxExample::V12 { l_plus_a })
                }
                17 => {
                    let sep_list = self.stack.pop().unwrap().get_sep_list();
                    (2, CtxExample::V13 { sep_list })
                }
                18 => {
                    let sep_list_opt = self.stack.pop().unwrap().get_sep_list_opt();
                    (2, CtxExample::V14 { sep_list_opt })
                }
                19 => {
                    let l_sep_list = self.stack.pop().unwrap().get_l_sep_list();
                    (2, CtxExample::V15 { l_sep_list })
                }
                20 => {
                    let l_sep_list_opt = self.stack.pop().unwrap().get_l_sep_list_opt();
                    (2, CtxExample::V16 { l_sep_list_opt })
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
        }

        fn exit_l_star_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLStarI::V1 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LStarI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_star_i(acc, ctx, spans);
        }

        fn exitloop_l_star_i(&mut self) {
            let EnumSynValue::LStarI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_l_star_i(acc);
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
        }

        fn exit_l_plus_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 149;
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLPlusI::V1 { num, last_iteration };
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

        fn exit_l_rrec(&mut self) {
            let l_rrec_i = self.stack.pop().unwrap().get_l_rrec_i();
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLRrec::V1 { id, num, l_rrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_l_rrec(ctx, spans);
            self.stack.push(EnumSynValue::LRrec(val));
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
        }

        fn exit_star_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                105 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynStarA1Item::V1 { id })
                }
                106 => {
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

        fn init_plus_a1(&mut self) {
            let val = SynPlusA1(Vec::new());
            self.stack.push(EnumSynValue::PlusA1(val));
        }

        fn exit_plus_a1(&mut self, alt_id: AltId) {
            let (n, val) = match alt_id {
                170 | 171 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, SynPlusA1Item::V1 { id })
                }
                172 | 173 => {
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
        }

        fn exit_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                35 => {
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLStarAI::V1 { id })
                }
                36 => {
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

        fn exitloop_l_star_a_i(&mut self) {
            let EnumSynValue::LStarAI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_l_star_a_i(acc);
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

        fn init_l_plus_a_i(&mut self) {
            let val = self.listener.init_l_plus_a_i();
            self.stack.push(EnumSynValue::LPlusAI(val));
        }

        fn exit_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                150 | 151 => {
                    let last_iteration = alt_id == 151;
                    let id = self.stack_t.pop().unwrap();
                    (2, CtxLPlusAI::V1 { id, last_iteration })
                }
                152 | 153 => {
                    let last_iteration = alt_id == 153;
                    let id = self.stack_t.pop().unwrap();
                    let num = self.stack_t.pop().unwrap();
                    (4, CtxLPlusAI::V2 { num, id, last_iteration })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_plus_a_i")
            };
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
                154 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxSepListOpt::V2 { id })
                }
                155 => {
                    let plus = self.stack.pop().unwrap().get_sep_list_opt1();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxSepListOpt::V1 { id, plus })
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
            let ctx = InitCtxLSepListI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_l_sep_list_i(ctx, spans);
            self.stack.push(EnumSynValue::LSepListI(val));
        }

        fn exit_l_sep_list_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LSepListI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_sep_list_i(acc, ctx, spans);
        }

        fn exitloop_l_sep_list_i(&mut self) {
            let EnumSynValue::LSepListI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_l_sep_list_i(acc);
        }

        fn exit_l_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                156 => {
                    let id = self.stack_t.pop().unwrap();
                    (3, CtxLSepListOpt::V2 { id })
                }
                157 => {
                    let plus = self.stack.pop().unwrap().get_l_sep_list_opt_i();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxLSepListOpt::V1 { id, plus })
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
            let ctx = InitCtxLSepListOptI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_l_sep_list_opt_i(ctx, spans);
            self.stack.push(EnumSynValue::LSepListOptI(val));
        }

        fn exit_l_sep_list_opt_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxLSepListOptI::V1 { id, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 6 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LSepListOptI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_sep_list_opt_i(acc, ctx, spans);
        }

        fn exitloop_l_sep_list_opt_i(&mut self) {
            let EnumSynValue::LSepListOptI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_l_sep_list_opt_i(acc);
        }

        fn exit_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                49 => {
                    let rrec_i = self.stack.pop().unwrap().get_rrec_i();
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxRrecI::V1 { num, rrec_i })
                }
                50 => {
                    (1, CtxRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_rrec_i(ctx, spans);
            self.stack.push(EnumSynValue::RrecI(val));
        }

        fn init_l_rrec_i(&mut self) {
            let val = self.listener.init_l_rrec_i();
            self.stack.push(EnumSynValue::LRrecI(val));
        }

        fn exit_l_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                51 => {
                    let num = self.stack_t.pop().unwrap();
                    (3, CtxLRrecI::V1 { num })
                }
                52 => {
                    (2, CtxLRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_l_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::LRrecI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_l_rrec_i(acc, ctx, spans);
        }

        fn inter_lrec_i(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let ctx = CtxLrecI::V2 { num };
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(EnumSynValue::LrecI(val));
        }

        fn exit_lrec_i1(&mut self) {
            let num = self.stack_t.pop().unwrap();
            let lrec_i = self.stack.pop().unwrap().get_lrec_i();
            let ctx = CtxLrecI::V1 { lrec_i, num };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_lrec_i(ctx, spans);
            self.stack.push(EnumSynValue::LrecI(val));
        }

        fn exitloop_lrec_i1(&mut self) {
            let EnumSynValue::LrecI(lrec_i) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_lrec_i(lrec_i);
        }

        fn exit_amb_i1(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                128 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V2 { amb_i: [amb_i_1, amb_i_2] })
                }
                129 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V3 { amb_i: [amb_i_1, amb_i_2] })
                }
                130 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V4 { amb_i: [amb_i_1, amb_i_2] })
                }
                131 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V5 { amb_i: [amb_i_1, amb_i_2] })
                }
                132 => {
                    let amb_i_2 = self.stack.pop().unwrap().get_amb_i();
                    let amb_i_1 = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V6 { amb_i: [amb_i_1, amb_i_2] })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_amb_i1")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(EnumSynValue::AmbI(val));
        }

        fn exit_amb_i6(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                142 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (2, CtxAmbI::V1 { amb_i })
                }
                143 => {
                    let amb_i = self.stack.pop().unwrap().get_amb_i();
                    (3, CtxAmbI::V7 { amb_i })
                }
                144 => {
                    let id = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V8 { id })
                }
                145 => {
                    let num = self.stack_t.pop().unwrap();
                    (1, CtxAmbI::V9 { num })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_amb_i6")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_amb_i(ctx, spans);
            self.stack.push(EnumSynValue::AmbI(val));
        }

        fn exit_nv_example(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                55 => {
                    let nv_star = self.stack.pop().unwrap().get_nv_star();
                    (2, CtxNvExample::V1 { nv_star })
                }
                56 => {
                    let nv_plus = self.stack.pop().unwrap().get_nv_plus();
                    (2, CtxNvExample::V2 { nv_plus })
                }
                57 => {
                    let nv_l_star = self.stack.pop().unwrap().get_nv_l_star();
                    (2, CtxNvExample::V3 { nv_l_star })
                }
                58 => {
                    let nv_l_plus = self.stack.pop().unwrap().get_nv_l_plus();
                    (2, CtxNvExample::V4 { nv_l_plus })
                }
                59 => {
                    let nv_rrec = self.stack.pop().unwrap().get_nv_rrec();
                    (2, CtxNvExample::V5 { nv_rrec })
                }
                60 => {
                    let nv_l_rrec = self.stack.pop().unwrap().get_nv_l_rrec();
                    (2, CtxNvExample::V6 { nv_l_rrec })
                }
                61 => {
                    let nv_lrec = self.stack.pop().unwrap().get_nv_lrec();
                    (2, CtxNvExample::V7 { nv_lrec })
                }
                62 => {
                    let nv_star_a = self.stack.pop().unwrap().get_nv_star_a();
                    (2, CtxNvExample::V8 { nv_star_a })
                }
                63 => {
                    let nv_plus_a = self.stack.pop().unwrap().get_nv_plus_a();
                    (2, CtxNvExample::V9 { nv_plus_a })
                }
                64 => {
                    let nv_l_star_a = self.stack.pop().unwrap().get_nv_l_star_a();
                    (2, CtxNvExample::V10 { nv_l_star_a })
                }
                65 => {
                    let nv_l_plus_a = self.stack.pop().unwrap().get_nv_l_plus_a();
                    (2, CtxNvExample::V11 { nv_l_plus_a })
                }
                66 => {
                    let nv_sep_list = self.stack.pop().unwrap().get_nv_sep_list();
                    (2, CtxNvExample::V12 { nv_sep_list })
                }
                67 => {
                    let nv_sep_list_opt = self.stack.pop().unwrap().get_nv_sep_list_opt();
                    (2, CtxNvExample::V13 { nv_sep_list_opt })
                }
                68 => {
                    let nv_l_sep_list = self.stack.pop().unwrap().get_nv_l_sep_list();
                    (2, CtxNvExample::V14 { nv_l_sep_list })
                }
                69 => {
                    let nv_l_sep_list_opt = self.stack.pop().unwrap().get_nv_l_sep_list_opt();
                    (2, CtxNvExample::V15 { nv_l_sep_list_opt })
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

        fn exit_nv_plus1(&mut self) {
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
        }

        fn exit_nv_l_star_i(&mut self) {
            let ctx = CtxNvLStarI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLStarI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_star_i(acc, ctx, spans);
        }

        fn exitloop_nv_l_star_i(&mut self) {
            let EnumSynValue::NvLStarI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_nv_l_star_i(acc);
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
        }

        fn exit_nv_l_plus_i(&mut self, alt_id: AltId) {
            let last_iteration = alt_id == 159;
            let ctx = CtxNvLPlusI::V1 { last_iteration };
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

        fn exit_nv_l_rrec(&mut self) {
            let nv_l_rrec_i = self.stack.pop().unwrap().get_nv_l_rrec_i();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxNvLRrec::V1 { id, nv_l_rrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_l_rrec(ctx, spans);
            self.stack.push(EnumSynValue::NvLRrec(val));
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

        fn exit_nv_star_a1(&mut self, alt_id: AltId) {
            let n = match alt_id {
                117 => {
                    2
                }
                118 => {
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

        fn exit_nv_plus_a1(&mut self, alt_id: AltId) {
            let n = match alt_id {
                176 | 177 => {
                    2
                }
                178 | 179 => {
                    3
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_plus_a1"),
            };
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
        }

        fn exit_nv_l_star_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                83 => {
                    (2, CtxNvLStarAI::V1)
                }
                84 => {
                    (3, CtxNvLStarAI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_star_a_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLStarAI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_star_a_i(acc, ctx, spans);
        }

        fn exitloop_nv_l_star_a_i(&mut self) {
            let EnumSynValue::NvLStarAI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_nv_l_star_a_i(acc);
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

        fn init_nv_l_plus_a_i(&mut self) {
            let val = self.listener.init_nv_l_plus_a_i();
            self.stack.push(EnumSynValue::NvLPlusAI(val));
        }

        fn exit_nv_l_plus_a_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                160 | 161 => {
                    let last_iteration = alt_id == 161;
                    (2, CtxNvLPlusAI::V1 { last_iteration })
                }
                162 | 163 => {
                    let last_iteration = alt_id == 163;
                    (3, CtxNvLPlusAI::V2 { last_iteration })
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_plus_a_i")
            };
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
                164 => {
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvSepListOpt::V1 { id })
                }
                165 => {
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
            let ctx = InitCtxNvLSepListI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_nv_l_sep_list_i(ctx, spans);
            self.stack.push(EnumSynValue::NvLSepListI(val));
        }

        fn exit_nv_l_sep_list_i(&mut self) {
            let ctx = CtxNvLSepListI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLSepListI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_sep_list_i(acc, ctx, spans);
        }

        fn exitloop_nv_l_sep_list_i(&mut self) {
            let EnumSynValue::NvLSepListI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_nv_l_sep_list_i(acc);
        }

        fn exit_nv_l_sep_list_opt(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                166 => {
                    let plus = self.stack.pop().unwrap().get_nv_l_sep_list_opt_i();
                    let id = self.stack_t.pop().unwrap();
                    (4, CtxNvLSepListOpt::V1 { id, plus })
                }
                167 => {
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
            let ctx = InitCtxNvLSepListOptI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.init_nv_l_sep_list_opt_i(ctx, spans);
            self.stack.push(EnumSynValue::NvLSepListOptI(val));
        }

        fn exit_nv_l_sep_list_opt_i(&mut self) {
            let ctx = CtxNvLSepListOptI::V1;
            let spans = self.stack_span.drain(self.stack_span.len() - 4 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLSepListOptI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_sep_list_opt_i(acc, ctx, spans);
        }

        fn exitloop_nv_l_sep_list_opt_i(&mut self) {
            let EnumSynValue::NvLSepListOptI(acc) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_nv_l_sep_list_opt_i(acc);
        }

        fn exit_nv_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                97 => {
                    let nv_rrec_i = self.stack.pop().unwrap().get_nv_rrec_i();
                    (3, CtxNvRrecI::V1 { nv_rrec_i })
                }
                98 => {
                    (1, CtxNvRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_rrec_i(ctx, spans);
            self.stack.push(EnumSynValue::NvRrecI(val));
        }

        fn init_nv_l_rrec_i(&mut self) {
            let val = self.listener.init_nv_l_rrec_i();
            self.stack.push(EnumSynValue::NvLRrecI(val));
        }

        fn exit_nv_l_rrec_i(&mut self, alt_id: AltId) {
            let (n, ctx) = match alt_id {
                99 => {
                    (3, CtxNvLRrecI::V1)
                }
                100 => {
                    (2, CtxNvLRrecI::V2)
                }
                _ => panic!("unexpected alt id {alt_id} in method exit_nv_l_rrec_i")
            };
            let spans = self.stack_span.drain(self.stack_span.len() - n ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let Some(EnumSynValue::NvLRrecI(acc)) = self.stack.last_mut() else { panic!() };
            self.listener.exit_nv_l_rrec_i(acc, ctx, spans);
        }

        fn inter_nv_lrec_i(&mut self) {
            let ctx = CtxNvLrecI::V2;
            let spans = self.stack_span.drain(self.stack_span.len() - 1 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_lrec_i(ctx, spans);
            self.stack.push(EnumSynValue::NvLrecI(val));
        }

        fn exit_nv_lrec_i1(&mut self) {
            let nv_lrec_i = self.stack.pop().unwrap().get_nv_lrec_i();
            let ctx = CtxNvLrecI::V1 { nv_lrec_i };
            let spans = self.stack_span.drain(self.stack_span.len() - 3 ..).collect::<Vec<_>>();
            self.stack_span.push(spans.iter().fold(PosSpan::empty(), |acc, sp| acc + sp));
            let val = self.listener.exit_nv_lrec_i(ctx, spans);
            self.stack.push(EnumSynValue::NvLrecI(val));
        }

        fn exitloop_nv_lrec_i1(&mut self) {
            let EnumSynValue::NvLrecI(nv_lrec_i) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_nv_lrec_i(nv_lrec_i);
        }
    }

    // [pandemonium_parser]
}
