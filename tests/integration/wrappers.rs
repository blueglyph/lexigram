// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(crate) mod level_string {
    use std::cmp::max;

    #[derive(Debug, PartialEq)]
    pub struct LevelString(pub u32, pub String);

    impl LevelString {
        pub fn get_string(self) -> String {
            self.1
        }
    }

    pub fn par(ls: LevelString) -> String {
        if ls.0 > 0 {
            format!("({})", ls.1)
        } else {
            ls.1
        }
    }

    pub fn ls_prefix_op(op: &str, ls: LevelString) -> LevelString {
        LevelString(ls.0 + 1, format!("{op} {}", par(ls)))
    }

    pub fn ls_suffix_op(op: &str, ls: LevelString) -> LevelString {
        LevelString(ls.0 + 1, format!("{} {op}", par(ls)))
    }

    pub fn ls_binary_op(op: &str, lsleft: LevelString, lsright: LevelString) -> LevelString {
        LevelString(max(lsleft.0, lsright.0) + 1, format!("{} {op} {}", par(lsleft), par(lsright)))
    }
}

mod rules_153_1 {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::CollectJoin;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::{BufLog, LogStatus, Logger};
    use crate::out::wrapper_source::rules_153_1::*;

    struct ChoiceListener {
        log: BufLog,
        result: Option<Vec<String>>,
    }

    impl ChoiceListener {
        fn new() -> Self {
            ChoiceListener {
                log: BufLog::new(),
                result: None,
            }
        }
    }

    impl TestListener for ChoiceListener {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, a: SynA, _span: PosSpan) {
            self.result = Some(a.0);
        }

        fn exit_a(&mut self, ctx: CtxA, _spans: Vec<PosSpan>) -> SynA {
            let CtxA::V1 { a, plus, f } = ctx;
            let mut val = vec![];
            val.push(a);
            val.extend(plus.0.into_iter()
                .map(|choice| {
                    match choice {
                        SynA1Item::V1 { b } => format!("b({b})"),
                        SynA1Item::V2 { b: [SynB(b_1), SynB(b_2)], c: [c_1, c_2], b1 } => format!("d({b_1}), c({c_1}), d({b_2}), b({b1}), c({c_2})", ),
                        SynA1Item::V3 { e } => format!("e({e})"),
                    }
            }));
            val.push(f);
            SynA(val)
        }

        fn exit_b(&mut self, ctx: CtxB, _spans: Vec<PosSpan>) -> SynB {
            let CtxB::V1 { d } = ctx;
            SynB(d)
        }
    }

    #[test]
    fn test() {
        let sequences = vec![
            // a -> A (B | b C b B C | E)+ F; b -> D;
            (
                "alpha echo delta charlie delta2 bravo charlie2 foxtrot",
                Some(vec!["alpha", "e(echo)", "d(delta), c(charlie), d(delta2), b(bravo), c(charlie2)", "foxtrot"])
            ),
            ("A C B F", None),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_LISTENER: bool = false;

        let mut parser = build_parser();
        let table = parser.get_symbol_table().unwrap();
        let symbols = (0..table.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(Some(table)).chars().next().unwrap(), t))
            .collect::<HashMap<_, _>>();
        for (input, expected_result) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let mut stop_lexer = false;
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).filter_map(|(i, w)| {
                if !stop_lexer {
                    // use the first letter to find a terminal
                    let pos = Pos(1, i);
                    let pos_span = PosSpan::new(pos, pos);
                    let first = w.chars().next().unwrap_or('?').to_ascii_uppercase();
                    if let Some(s) = symbols.get(&first) {
                        Some((*s, w.to_string(), pos_span))
                    } else {
                        stop_lexer = true;
                        None
                    }
                } else {
                    None
                }
            });
            let listener = ChoiceListener::new();
            let mut wrapper = Wrapper::new(listener, VERBOSE_LISTENER);
            match parser.parse_stream(&mut wrapper, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully: {:?}", wrapper.get_listener().result); }
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                }
            };
            if VERBOSE {
                let msg = wrapper.get_listener().log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let listener = wrapper.get_listener();
            assert_eq!(listener.result, expected_result.map(|v| v.into_iter().map(|s| s.to_string()).to_vec()), "test failed for input {input}");
        }
    }
}

mod rules_580_1 {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::CollectJoin;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::{BufLog, LogStatus, Logger};
    use crate::out::wrapper_source::rules_580_1::*;
    use crate::out::wrapper_code::code_580_1::*;
    use crate::integration::wrappers::level_string::LevelString;
    use crate::integration::parser_examples::listener1::build_parser;
    use crate::integration::wrappers::level_string::{ls_prefix_op, ls_suffix_op};

    struct EListener {
        log: BufLog,
        result: Option<String>,
    }

    impl EListener {
        fn new() -> Self {
            EListener {
                log: BufLog::new(),
                result: None,
            }
        }
    }

    impl TestListener for EListener {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, e: SynE, _span: PosSpan) {
            self.result = Some(e.0.1);
        }

        fn init_e(&mut self) {
            self.result = None;
        }

        fn exit_e(&mut self, ctx: CtxE, _spans: Vec<PosSpan>) -> SynE {
            SynE(match ctx {
                // e -> e "!"
                CtxE::V1 { e: SynE(ls) } => ls_suffix_op("!", ls),
                // e -> "-" e
                CtxE::V2 { e: SynE(ls) } => ls_prefix_op("-", ls),
                // e -> Num
                CtxE::V3 { num } => LevelString(0, num),
            })
        }
    }

    #[test]
    fn test() {
        let sequences = vec![
            // priority: e -> e "!" | "-" e | Num
            ("- - 0 ! !", Some("- (- ((0 !) !))")),
            ("0 !", Some("0 !")),
            ("- 0", Some("- 0")),
            ("0", Some("0")),
            ("- !", None),
            ("-", None),
            ("!", None),
            ("", None),
            ("- 0 0 !", None),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_LISTENER: bool = false;
        let num_id = 2;

        let mut parser = build_parser();
        let table = parser.get_symbol_table().unwrap();
        let symbols = (0..table.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(Some(table)), t))
            .collect::<HashMap<_, _>>();
        for (input, expected_result) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        (num_id, w.to_string(), pos_span)
                    } else {
                        panic!("IDs not supported")
                    }
                }
            });
            let listener = EListener::new();
            let mut wrapper = Wrapper::new(listener, VERBOSE_LISTENER);
            match parser.parse_stream(&mut wrapper, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully: {:?}", wrapper.get_listener().result); }
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                }
            };
            if VERBOSE {
                let msg = wrapper.get_listener().log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let listener = wrapper.get_listener();
            assert_eq!(listener.result, expected_result.map(|s| s.to_string()), "test failed for input {input}");
        }
    }
}

pub mod precedence_type {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::BufLog;
    use lexigram_lib::parser::{ListenerWrapper, Parser, ParserError, ParserToken};
    use crate::integration::wrappers::level_string::LevelString;

    const TOK_NUM: TokenId = 3;
    const TOK_ID: TokenId = 4;

    /// User-defined type for `e`
    #[derive(Debug, PartialEq)] pub struct SynE(pub LevelString);

    pub fn get_stream(input: &str, symbols: &HashMap<String, TokenId>) -> impl Iterator<Item=ParserToken> {
        const VERBOSE: bool = false;
        input.chars().index_start::<CaretCol>(1).filter(|(_, c)| !c.is_ascii_whitespace())
            .map(|(i, w)| {
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(&w.to_string()) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.is_ascii_digit() {
                        (TOK_NUM, w.to_string(), pos_span)
                    } else {
                        (TOK_ID, w.to_string(), pos_span)
                    }
                }
            })
            .inspect(|(tok, s, pos_span)| { if VERBOSE { println!("STREAM: pos={pos_span}, tok={tok}, s={s:?}"); } })
    }

    pub struct Tester<W: ListenerWrapper> {
        pub parser: Parser<'static>,
        pub wrapper: W,
        pub symbols: HashMap<String, TokenId>
    }

    pub trait TestApi {
        fn new() -> Self where Self: Sized;
        fn get_symbols(parser: &Parser) -> HashMap<String, TokenId> where Self: Sized {
            let table = parser.get_symbol_table().unwrap();
            (0..table.get_num_t() as TokenId)
                .map(|t| (Symbol::T(t).to_str(Some(table)), t))
                .collect::<HashMap<_, _>>()
        }
        fn parse(&mut self, input: &str) -> Result<(Option<String>, BufLog), (ParserError, BufLog)>;
    }
}

mod test_precedence {
    use lexigram_lib::parser::ParserError;
    use super::precedence_type::{TestApi, Tester};
    #[allow(unused_imports)]
    use crate::out::wrapper_source::{rules_603_1, rules_604_1, rules_605_1, rules_606_1, rules_607_1, rules_608_1, rules_609_1, rules_610_1,
                                     rules_611_1, rules_612_1, rules_613_1, rules_614_1, rules_630_1, rules_631_1, rules_632_1};

    #[test]
    fn test() {
        let listeners: Vec<(Box<dyn TestApi>, Vec<(&str, Result<Option<&str>, ParserError>)>)> = vec![
            (   // 603: e -> e "*" e | e "+" e |   "!" e | Num
                Box::new(Tester::<rules_603_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("!!2", Ok(Some("! (! 2)"))),
                    ("2 * 3 + 4", Ok(Some("(2 * 3) + 4"))),
                    ("2 + 3 * 4", Ok(Some("2 + (3 * 4)"))),
                    ("2 * !3", Ok(Some("2 * (! 3)"))),
                    ("!2 * 3", Ok(Some("! (2 * 3)"))),
                    ("2 + !3", Ok(Some("2 + (! 3)"))),
                    ("!2 + 3", Ok(Some("! (2 + 3)"))),

                    ("2 * a", Err(ParserError::ExtraSymbol)),
                ]),
            (   // 604: e -> e "*" e |   "!" e | e "+" e | Num
                Box::new(Tester::<rules_604_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 * 3 + 4", Ok(Some("(2 * 3) + 4"))),
                    ("2 + 3 * 4", Ok(Some("2 + (3 * 4)"))),
                    ("2 * !3", Ok(Some("2 * (! 3)"))),
                    ("!2 * 3", Ok(Some("! (2 * 3)"))),
                    ("2 + !3", Ok(Some("2 + (! 3)"))),
                    ("!2 + 3", Ok(Some("(! 2) + 3"))),
                ]),
            (   // 605: e ->   "!" e | e "*" e | e "+" e | Num
                Box::new(Tester::<rules_605_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 * 3 + 4", Ok(Some("(2 * 3) + 4"))),
                    ("2 + 3 * 4", Ok(Some("2 + (3 * 4)"))),
                    ("2 * !3", Ok(Some("2 * (! 3)"))),
                    ("!2 * 3", Ok(Some("(! 2) * 3"))),
                    ("2 + !3", Ok(Some("2 + (! 3)"))),
                    ("!2 + 3", Ok(Some("(! 2) + 3"))),
                ]),
            (   // 606: e ->     e "*" e |     e "+" e | <R> e "!" e | Num
                Box::new(Tester::<rules_606_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 ! 3 ! 4", Ok(Some("2 ! (3 ! 4)"))),
                    ("2 * 3 + 4 ! 5", Ok(Some("((2 * 3) + 4) ! 5"))),
                    ("1 ! 2 + 3 * 4", Ok(Some("1 ! (2 + (3 * 4))"))),
                    ("1 * 2 ! 3 + 4", Ok(Some("(1 * 2) ! (3 + 4)"))),
                    ("1 + 2 ! 3 * 4", Ok(Some("(1 + 2) ! (3 * 4)"))),
                ]),
            (   // 607: e ->     e "*" e | <R> e "!" e |     e "+" e | Num
                Box::new(Tester::<rules_607_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 ! 3 ! 4", Ok(Some("2 ! (3 ! 4)"))),
                    ("2 * 3 + 4 ! 5", Ok(Some("(2 * 3) + (4 ! 5)"))),
                    ("1 ! 2 + 3 * 4", Ok(Some("(1 ! 2) + (3 * 4)"))),
                    ("1 * 2 ! 3 + 4", Ok(Some("((1 * 2) ! 3) + 4"))),
                    ("1 + 2 ! 3 * 4", Ok(Some("1 + (2 ! (3 * 4))"))),
                ]),
            (   // 608: e -> <R> e "!" e |     e "*" e |     e "+" e | Num
                Box::new(Tester::<rules_608_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 ! 3 ! 4", Ok(Some("2 ! (3 ! 4)"))),
                    ("2 * 3 + 4 ! 5", Ok(Some("(2 * 3) + (4 ! 5)"))),
                    ("1 ! 2 + 3 * 4", Ok(Some("(1 ! 2) + (3 * 4)"))),
                    ("1 * 2 ! 3 + 4", Ok(Some("(1 * (2 ! 3)) + 4"))),
                    ("1 + 2 ! 3 * 4", Ok(Some("1 + ((2 ! 3) * 4)"))),
                ]),
            (   // 609: e -> e "*" e | e "+" e | e "!"   | Num
                Box::new(Tester::<rules_609_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 * 3 + 4", Ok(Some("(2 * 3) + 4"))),
                    ("2 + 3 * 4", Ok(Some("2 + (3 * 4)"))),
                    ("2 * 3!", Ok(Some("(2 * 3) !"))),
                    ("2! * 3", Ok(Some("(2 !) * 3"))),
                    ("2 + 3!", Ok(Some("(2 + 3) !"))),
                    ("2! + 3", Ok(Some("(2 !) + 3"))),
                ]),
            (   // 610: e -> e "*" e | e "!"   | e "+" e | Num
                Box::new(Tester::<rules_610_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 * 3 + 4", Ok(Some("(2 * 3) + 4"))),
                    ("2 + 3 * 4", Ok(Some("2 + (3 * 4)"))),
                    ("2 * 3!", Ok(Some("(2 * 3) !"))),
                    ("2! * 3", Ok(Some("(2 !) * 3"))),
                    ("2 + 3!", Ok(Some("2 + (3 !)"))),
                    ("2! + 3", Ok(Some("(2 !) + 3"))),
                ]),
            (   // 611: e -> e "!"   | e "*" e | e "+" e | Num
                Box::new(Tester::<rules_611_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 * 3 + 4", Ok(Some("(2 * 3) + 4"))),
                    ("2 + 3 * 4", Ok(Some("2 + (3 * 4)"))),
                    ("2 * 3!", Ok(Some("2 * (3 !)"))),
                    ("2! * 3", Ok(Some("(2 !) * 3"))),
                    ("2 + 3!", Ok(Some("2 + (3 !)"))),
                    ("2! + 3", Ok(Some("(2 !) + 3"))),
                ]),
            (   // 612: e -> e "!" e |     e "*" e |     e "+" e | Num
                Box::new(Tester::<rules_612_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 ! 3 ! 4", Ok(Some("(2 ! 3) ! 4"))),
                    ("2 * 3 + 4 ! 5", Ok(Some("(2 * 3) + (4 ! 5)"))),
                    ("1 ! 2 + 3 * 4", Ok(Some("(1 ! 2) + (3 * 4)"))),
                    ("1 * 2 ! 3 + 4", Ok(Some("(1 * (2 ! 3)) + 4"))),
                    ("1 + 2 ! 3 * 4", Ok(Some("1 + ((2 ! 3) * 4)"))),
                ]),
            (   // 613: e -> e "*" e |     e "+" e | <P> e "!" e | Num
                Box::new(Tester::<rules_613_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 ! 3 ! 4", Ok(Some("(2 ! 3) ! 4"))),
                    ("2 * 3 + 4 ! 5", Ok(Some("((2 * 3) + 4) ! 5"))),
                    ("1 ! 2 + 3 * 4", Ok(Some("(1 ! 2) + (3 * 4)"))),
                    ("1 * 2 ! 3 + 4", Ok(Some("((1 * 2) ! 3) + 4"))),
                    ("1 + 2 ! 3 * 4", Ok(Some("(1 + 2) ! (3 * 4)"))),
                ]),
            (   // 614: e -> e "*" e | <P> e "!" e |     e "+" e | Num
                Box::new(Tester::<rules_614_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2 + 3 + 4", Ok(Some("(2 + 3) + 4"))),
                    ("2 ! 3 ! 4", Ok(Some("(2 ! 3) ! 4"))),
                    ("2 * 3 + 4 ! 5", Ok(Some("(2 * 3) + (4 ! 5)"))),
                    ("1 ! 2 + 3 * 4", Ok(Some("(1 ! 2) + (3 * 4)"))),
                    ("1 * 2 ! 3 + 4", Ok(Some("((1 * 2) ! 3) + 4"))),
                    ("1 + 2 ! 3 * 4", Ok(Some("1 + ((2 ! 3) * 4)"))),
                ]),
            (   // 630: e -> e "*" e |     e "+" |     "!" e | Num
                Box::new(Tester::<rules_630_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2++", Ok(Some("(2 +) +"))),
                    ("!!2", Ok(Some("! (! 2)"))),
                    ("2 * 3+", Ok(Some("(2 * 3) +"))),
                    ("2+ * 4", Ok(Some("(2 +) * 4"))),
                    ("2 * !3", Ok(Some("2 * (! 3)"))),
                    ("!2 * 4", Ok(Some("! (2 * 4)"))),
                    ("!2+", Ok(Some("! (2 +)"))),
                ]),
            (   // 631: e -> e "*" e |     e "+" | <R> "!" e | Num
                Box::new(Tester::<rules_631_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2++", Ok(Some("(2 +) +"))),
                    ("!!2", Ok(Some("! (! 2)"))),
                    ("2 * 3+", Ok(Some("(2 * 3) +"))),
                    ("2+ * 4", Ok(Some("(2 +) * 4"))),
                    ("2 * !3", Ok(Some("2 * (! 3)"))),
                    ("!2 * 4", Ok(Some("! (2 * 4)"))),
                    ("!2+", Ok(Some("! (2 +)"))),
                ]),
            (   // 632: e -> e "*" e | <R> e "+" |     "!" e | Num
                Box::new(Tester::<rules_632_1::Wrapper<_>>::new()),
                vec![
                    ("2 * 3 * 4", Ok(Some("(2 * 3) * 4"))),
                    ("2++", Ok(Some("(2 +) +"))),
                    ("!!2", Ok(Some("! (! 2)"))),
                    ("2 * 3+", Ok(Some("(2 * 3) +"))),
                    ("2+ * 4", Ok(Some("(2 +) * 4"))),
                    ("2 * !3", Ok(Some("2 * (! 3)"))),
                    ("!2 * 4", Ok(Some("! (2 * 4)"))),
                    ("!2+", Ok(Some("! (2 +)"))),
                ]),
        ];
        const VERBOSE: bool = false;

        for (test_id, (mut tester, tests)) in listeners.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\nTest {test_id}", ""); }
            for (input, expected) in tests {
                if VERBOSE { println!("input: {input}"); }
                let (result, log) = match tester.parse(input) {
                    Ok((output, log)) => {
                        if VERBOSE { println!("-> output: {output:?}"); }
                        (Ok(output), log)
                    }
                    Err((error, log)) => {
                        if VERBOSE { println!("-> error: {error:?}"); }
                        (Err(error), log)
                    }
                };
                if VERBOSE && !log.is_empty() { println!("Log:\n{log}"); }
                assert_eq!(result, expected.map(|s_maybe| s_maybe.map(|s| s.to_string())));
            }
        }

    }
}

mod rules_640_1 {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::CollectJoin;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::{BufLog, LogStatus, Logger};
    use crate::out::wrapper_source::rules_640_1::*;
    use crate::integration::parser_examples::listener2::build_parser;
    use crate::integration::wrappers::level_string::{ls_binary_op, ls_prefix_op, LevelString};

    struct EListener {
        log: BufLog,
        result: Option<String>,
    }

    impl EListener {
        fn new() -> Self {
            EListener {
                log: BufLog::new(),
                result: None,
            }
        }
    }

    impl TestListener for EListener {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, e: SynE, _span: PosSpan) {
            self.result = Some(e.0.get_string());
        }

        fn exit_e(&mut self, ctx: CtxE, _spans: Vec<PosSpan>) -> SynE {
            SynE(match ctx {
                // `E -> - E`
                CtxE::V1 { e: SynE(lsleft) } => ls_prefix_op("-", lsleft),
                // `E -> E * E`
                CtxE::V2 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("*", lsleft, lsright),
                // `E -> E / E`
                CtxE::V3 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("/", lsleft, lsright),
                // `E -> E + E`
                CtxE::V4 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("+", lsleft, lsright),
                // `E -> E - E`
                CtxE::V5 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("-", lsleft, lsright),
                // `E -> ID`
                CtxE::V6 { id } => LevelString(0, id),
            })
        }
    }

    #[test]
    fn test() {
        let sequences = vec![
            // E -> - E | E * E | <P> E / E | E + E | <P> E - E | ID
            ("- a", Some("- a")),
            ("a * b", Some("a * b")),
            ("a / b", Some("a / b")),
            ("a + b", Some("a + b")),
            ("a - b", Some("a - b")),
            ("- - - a", Some("- (- (- a))")),
            ("a * b * c * d", Some("((a * b) * c) * d")),
            ("a / b / c / d", Some("((a / b) / c) / d")),
            ("a + b + c + d", Some("((a + b) + c) + d")),
            ("a - b - c - d", Some("((a - b) - c) - d")),
            ("a * b + c", Some("(a * b) + c")),
            ("a + b * c", Some("a + (b * c)")),
            ("a + b - c", Some("(a + b) - c")),
            ("a - b + c", Some("(a - b) + c")),
            ("a * b / c", Some("(a * b) / c")),
            ("a / b * c", Some("(a / b) * c")),
            ("a + b / c", Some("a + (b / c)")),
            ("a / b + c", Some("(a / b) + c")),
            ("- a * b", Some("(- a) * b")),
            ("a + - b + c", Some("(a + (- b)) + c")),
            ("a * - b", Some("a * (- b)")),
            ("a * * b", None),
            ("a / / b", None),
            ("a + + b", None),
            ("a - - b", Some("a - (- b)")),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_LISTENER: bool = false;
        let id_id = 4;

        let mut parser = build_parser();
        let table = parser.get_symbol_table().unwrap();
        let symbols = (0..table.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(Some(table)), t))
            .collect::<HashMap<_, _>>();
        for (input, expected_result) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        // (num_id, w.to_string(), pos_span)
                        panic!("numbers not supported")
                    } else {
                        (id_id, w.to_string(), pos_span)
                    }
                }
            });
            let listener = EListener::new();
            let mut wrapper = Wrapper::new(listener, VERBOSE_LISTENER);
            let errors = match parser.parse_stream(&mut wrapper, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully: {:?}", wrapper.get_listener().result); }
                    None
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    Some(wrapper.get_listener().log.get_errors().map(|s| s.as_str()).to_vec())
                }
            };
            if VERBOSE {
                let msg = wrapper.get_listener().log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let listener = wrapper.get_listener();
            assert_eq!(errors.is_some(), listener.result.is_none(), "listener.result of unexpected variant for input {input}: {:?}", listener.result);
            assert_eq!(listener.result, expected_result.map(|s| s.to_string()), "test failed for input {input}");
        }
    }
}

mod rules_641_1 {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::CollectJoin;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::{BufLog, LogStatus, Logger};
    use crate::out::wrapper_source::rules_641_1::*;
    use crate::integration::parser_examples::listener3::build_parser;
    use crate::integration::wrappers::level_string::{ls_binary_op, ls_prefix_op, LevelString};

    struct EListener {
        log: BufLog,
        result: Option<String>,
    }

    impl EListener {
        fn new() -> Self {
            EListener {
                log: BufLog::new(),
                result: None,
            }
        }
    }

    impl TestListener for EListener {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, e: SynE, _span: PosSpan) {
            self.result = Some(e.0.get_string());
        }

        fn exit_e(&mut self, ctx: CtxE, _spans: Vec<PosSpan>) -> SynE {
            SynE(match ctx {
                // `E -> - E`
                CtxE::V1 { e: SynE(lsleft) } => ls_prefix_op("-", lsleft),
                // `E -> E * E`
                CtxE::V2 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("*", lsleft, lsright),
                // `E -> E / E`
                CtxE::V3 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("/", lsleft, lsright),
                // `E -> E + E`
                CtxE::V4 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("+", lsleft, lsright),
                // `E -> E - E`
                CtxE::V5 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("-", lsleft, lsright),
                // `E -> ID`
                CtxE::V6 { id } => LevelString(0, id),
            })
        }
    }

    #[test]
    fn test() {
        let sequences = vec![
            // E -> - E | <R> E * E | <R> E / E <P> | <R> E + E | <R> E - E <P> | ID
            ("- a", Some("- a")),
            ("a * b", Some("a * b")),
            ("a / b", Some("a / b")),
            ("a + b", Some("a + b")),
            ("a - b", Some("a - b")),
            ("- - - a", Some("- (- (- a))")),
            ("a * b * c * d", Some("a * (b * (c * d))")),
            ("a / b / c / d", Some("a / (b / (c / d))")),
            ("a + b + c + d", Some("a + (b + (c + d))")),
            ("a - b - c - d", Some("a - (b - (c - d))")),
            ("a * b + c", Some("(a * b) + c")),
            ("a + b * c", Some("a + (b * c)")),
            ("a + b - c", Some("a + (b - c)")),
            ("a - b + c", Some("a - (b + c)")),
            ("a * b / c", Some("a * (b / c)")),
            ("a / b * c", Some("a / (b * c)")),
            ("a + b / c", Some("a + (b / c)")),
            ("a / b + c", Some("(a / b) + c")),
            ("- a * b", Some("(- a) * b")),
            ("a + - b + c", Some("a + ((- b) + c)")),
            ("a * - b", Some("a * (- b)")),
            ("a * * b", None),
            ("a / / b", None),
            ("a + + b", None),
            ("a - - b", Some("a - (- b)")),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_LISTENER: bool = false;
        let id_id = 4;

        let mut parser = build_parser();
        let table = parser.get_symbol_table().unwrap();
        let symbols = (0..table.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(Some(table)), t))
            .collect::<HashMap<_, _>>();
        for (input, expected_result) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        // (num_id, w.to_string(), pos_span)
                        panic!("numbers not supported")
                    } else {
                        (id_id, w.to_string(), pos_span)
                    }
                }
            });
            let listener = EListener::new();
            let mut wrapper = Wrapper::new(listener, VERBOSE_LISTENER);
            let errors = match parser.parse_stream(&mut wrapper, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully: {:?}", wrapper.get_listener().result); }
                    None
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    Some(wrapper.get_listener().log.get_errors().map(|s| s.as_str()).to_vec())
                }
            };
            if VERBOSE {
                let msg = wrapper.get_listener().log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let listener = wrapper.get_listener();
            assert_eq!(errors.is_some(), listener.result.is_none(), "listener.result of unexpected variant for input {input}: {:?}", listener.result);
            assert_eq!(listener.result, expected_result.map(|s| s.to_string()), "test failed for input {input}");
        }
    }
}

mod rules_642_1 {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::CollectJoin;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::{BufLog, LogStatus, Logger};
    use crate::out::wrapper_source::rules_642_1::*;
    use crate::integration::parser_examples::listener4::build_parser;
    use crate::integration::wrappers::level_string::{ls_binary_op, ls_prefix_op, LevelString};

    struct EListener {
        log: BufLog,
        result: Option<String>,
    }

    impl EListener {
        fn new() -> Self {
            EListener {
                log: BufLog::new(),
                result: None,
            }
        }
    }

    impl TestListener for EListener {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, e: SynE, _span: PosSpan) {
            self.result = Some(e.0.get_string());
        }

        fn exit_e(&mut self, ctx: CtxE, _spans: Vec<PosSpan>) -> SynE {
            SynE(match ctx {
                // `E -> - E`
                CtxE::V1 { e: SynE(lsleft) } => ls_prefix_op("-", lsleft),
                // `E -> E * E`
                CtxE::V2 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("*", lsleft, lsright),
                // `E -> E / E`
                CtxE::V3 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("/", lsleft, lsright),
                // `E -> E + E`
                CtxE::V4 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("+", lsleft, lsright),
                // `E -> E - E`
                CtxE::V5 { e: [SynE(lsleft), SynE(lsright)] } => ls_binary_op("-", lsleft, lsright),
                // `E -> ID`
                CtxE::V6 { id } => LevelString(0, id),
            })
        }
    }

    #[test]
    fn test() {
        let sequences = vec![
            // E -> - E | <R> E * E | <R> E / E <P> | E + E | E - E <P> | ID
            ("- a", Some("- a")),
            ("a * b", Some("a * b")),
            ("a / b", Some("a / b")),
            ("a + b", Some("a + b")),
            ("a - b", Some("a - b")),
            ("- - - a", Some("- (- (- a))")),
            ("a * b * c * d", Some("a * (b * (c * d))")),
            ("a / b / c / d", Some("a / (b / (c / d))")),
            ("a + b + c + d", Some("((a + b) + c) + d")),
            ("a - b - c - d", Some("((a - b) - c) - d")),
            ("a * b + c", Some("(a * b) + c")),
            ("a + b * c", Some("a + (b * c)")),
            ("a + b - c", Some("(a + b) - c")),
            ("a - b + c", Some("(a - b) + c")),
            ("a * b / c", Some("a * (b / c)")),
            ("a / b * c", Some("a / (b * c)")),
            ("a + b / c", Some("a + (b / c)")),
            ("a / b + c", Some("(a / b) + c")),
            ("- a * b", Some("(- a) * b")),
            ("a + - b + c", Some("(a + (- b)) + c")),
            ("a * - b", Some("a * (- b)")),
            ("a * * b", None),
            ("a / / b", None),
            ("a + + b", None),
            ("a - - b", Some("a - (- b)")),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_LISTENER: bool = false;
        let id_id = 4;

        let mut parser = build_parser();
        let table = parser.get_symbol_table().unwrap();
        let symbols = (0..table.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(Some(table)), t))
            .collect::<HashMap<_, _>>();
        for (input, expected_result) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        // (num_id, w.to_string(), pos_span)
                        panic!("numbers not supported")
                    } else {
                        (id_id, w.to_string(), pos_span)
                    }
                }
            });
            let listener = EListener::new();
            let mut wrapper = Wrapper::new(listener, VERBOSE_LISTENER);
            let errors = match parser.parse_stream(&mut wrapper, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully: {:?}", wrapper.get_listener().result); }
                    None
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    Some(wrapper.get_listener().log.get_errors().map(|s| s.as_str()).to_vec())
                }
            };
            if VERBOSE {
                let msg = wrapper.get_listener().log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let listener = wrapper.get_listener();
            assert_eq!(errors.is_some(), listener.result.is_none(), "listener.result of unexpected variant for input {input}: {:?}", listener.result);
            assert_eq!(listener.result, expected_result.map(|s| s.to_string()), "test failed for input {input}");
        }
    }
}


mod rules_862_1 {
    use std::collections::HashMap;
    use iter_index::IndexerIterator;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::grammar::Symbol;
    use lexigram_lib::lexer::{CaretCol, Pos, PosSpan};
    use lexigram_lib::log::{BufLog, Logger};
    use crate::out::wrapper_source::rules_862_1::*;
    use crate::out::wrapper_code::code_862_1::*;
    use crate::integration::parser_examples::listener5::build_parser;

    struct ExprListener {
        log: BufLog,
        result: Option<String>,
    }

    impl ExprListener {
        fn new() -> Self {
            ExprListener {
                log: BufLog::new(),
                result: None,
            }
        }
    }

    impl TestListener for ExprListener {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self, expr: SynExpr, _span: PosSpan) {
            let SynExpr(mut result) = expr;
            while result.len() > 1 {
                let r = result.pop().unwrap();
                let l = result.pop().unwrap();
                result.push(if result.len() > 0 { format!("({l} ^ {r})") } else { format!("{l} ^ {r}") });
            }
            self.result = result.pop();
        }

        fn init_expr(&mut self) -> SynExpr {
            self.result = None;
            SynExpr(vec![])
        }

        fn exit_expr(&mut self, ctx: CtxExpr, _spans: Vec<PosSpan>) -> SynExpr {
            let (mut e, num) = match ctx {
                // expr -> <L> Num "^" expr
                CtxExpr::V1 { expr: SynExpr(e), num } => (e, num),
                // expr -> Num
                CtxExpr::V2 { expr: SynExpr(e), num } => (e, num),
            };
            e.push(num);
            SynExpr(e)
        }
    }

    #[test]
    fn test() {
        let sequences = vec![
            // expr -> <L=expr> Num "^" expr | Num
            ("1", Some("1")),
            ("1 ^ 2", Some("1 ^ 2")),
            ("1 ^ 2 ^ 3", Some("1 ^ (2 ^ 3)")),
            ("1 ^ 2 ^ 3 ^ 4", Some("1 ^ (2 ^ (3 ^ 4))")),
            ("", None),
            ("^ 1", None),
            ("1 ^", None),
            ("1 ^ ^", None),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_LISTENER: bool = false;
        let num_id = 0;

        let mut parser = build_parser();
        let table = parser.get_symbol_table().unwrap();
        let symbols = (0..table.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(Some(table)), t))
            .collect::<HashMap<_, _>>();
        println!("symbols = {symbols:?}");
        for (input, expected_result) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        (num_id, w.to_string(), pos_span)
                    } else {
                        panic!("'{w}' input not recognized")
                    }
                }
            });
            let listener = ExprListener::new();
            let mut wrapper = Wrapper::new(listener, VERBOSE_LISTENER);
            let result = match parser.parse_stream(&mut wrapper, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully: {:?}", wrapper.get_listener().result); }
                    wrapper.get_listener_mut().result.take()
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    None
                }
            };
            if VERBOSE {
                let msg = wrapper.get_listener().log.to_string();
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let expected_result = expected_result.map(|s| s.to_string());
            assert_eq!(result, expected_result, "test failed for input: {input}");
        }
    }
}

