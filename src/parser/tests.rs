// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::collections::HashMap;
use iter_index::IndexerIterator;
use crate::{CollectJoin, LL1};
use crate::dfa::TokenId;
use crate::grammar::{Alternative, ProdRuleSet, Symbol, VarId};
use crate::grammar::tests::old_build_rts_prs::T;
use crate::grammar::tests::old_build_rts_prs::build_prs;
use crate::lexer::CaretCol;
use crate::log::{BufLog, BuildFrom, LogStatus, Logger};
use crate::parser::{ListenerWrapper, OpCode, Parser};
use crate::parsergen::{ParserGen, ParserTables};


impl<'a> Parser<'a> {
    pub(crate) fn get_alt_var(&self) -> &[VarId] {
        self.alt_var
    }

    pub(crate) fn get_alts(&self) -> &Vec<Alternative> {
        &self.alts
    }

    pub(crate) fn get_opcodes(&self) -> &Vec<Vec<OpCode>> {
        &self.opcodes
    }
}

impl OpCode {
    pub fn to_macro_item(&self) -> String {
        match self {
            OpCode::Empty => "e".to_string(),
            OpCode::T(t) => format!("t {t}"),
            OpCode::NT(v) => format!("nt {v}"),
            OpCode::Loop(v) => format!("loop {v}"),
            OpCode::Exit(v) => format!("exit {v}"),
            OpCode::End => "end".to_string(),
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates an `OpCode` instance.
    ///
    /// # Examples
    /// ```
    /// # use lexigram_lib::dfa::TokenId;
    /// # use lexigram_lib::opcode;
    /// # use lexigram_lib::grammar::VarId;
    /// # use lexigram_lib::parser::OpCode;
    /// assert_eq!(opcode!(e), OpCode::Empty);
    /// assert_eq!(opcode!(t 2), OpCode::T(2 as TokenId));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1));
    /// assert_eq!(opcode!(end), OpCode::End);
    #[macro_export()]
    macro_rules! opcode {
        (e) => { $crate::parser::OpCode::Empty };
        (t $id:expr) => { $crate::parser::OpCode::T($id as $crate::dfa::TokenId) };
        (nt $id:expr) => { $crate::parser::OpCode::NT($id as $crate::grammar::VarId) };
        (loop $id:expr) => { $crate::parser::OpCode::Loop($id as $crate::grammar::VarId) };
        (exit $id:expr) => { $crate::parser::OpCode::Exit($id as $crate::grammar::VarId) };
        (nt $id:expr) => { $crate::parser::OpCode::NT($id as $crate::grammar::VarId, 0) };
        (loop $id:expr) => { $crate::parser::OpCode::Loop($id as $crate::grammar::VarId, 0) };
        (exit $id:expr) => { $crate::parser::OpCode::Exit($id as $crate::grammar::VarId, 0) };
        (end) => { $crate::parser::OpCode::End };
    }

    /// Generates an opcode strip. A strip is made up of `OpCode` items separated by a comma.
    ///
    /// # Example
    /// ```
    /// # use lexigram_lib::dfa::TokenId;
    /// # use lexigram_lib::grammar::{Alternative, Symbol, VarId};
    /// # use lexigram_lib::{strip, opcode};
    /// # use lexigram_lib::parser::OpCode;
    /// assert_eq!(strip!(nt 1, loop 5, t 3, e), vec![opcode!(nt 1), opcode!(loop 5), opcode!(t 3), opcode!(e)]);
    /// ```
    #[macro_export()]
    macro_rules! strip {
        () => { std::vec![] };
        ($($a:ident $($b:expr)?,)+) => { strip![$($a $($b)?),+] };
        ($($a:ident $($b:expr)?),*) => { std::vec![$($crate::opcode!($a $($b)?)),*] };
    }
}

// ---------------------------------------------------------------------------------------------

#[test]
fn parser_parse_stream() {

    struct Stub(BufLog);
    impl ListenerWrapper for Stub {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.0
        }
    }

    let tests = vec![
        (5, 0, vec![
            ("++;;", true),
            ("--+;;", true),
            ("+-;;", false),
            ("++;;-", false),
            ("++;-", false),
            ("-", false),
        ]),
        (4, 0, vec![
            ("I*I", true),
            ("(N)", true),
            ("((N))", true),
        ]),
        (8, 0, vec![ // ambiguous grammar but that should work
            ("b a b a b", true),
            ("b", true),
        ]),
        (16, 0, vec![ // A -> B A | b ; B -> a
            ("aaab", true),
        ]),
        (17, 0, vec![
            ("(((a)))", true),
            ("((a)", false),
            ("((a)))", false),
        ]),
        (18, 0, vec![
            ("a", true),
            ("", false),
            ("aa", false),
        ]),
        (19, 0, vec![
            ("a", true),
            ("", true),
            ("aa", false),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, sequences)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id}/{start}", ""); }
        let mut ll1 = ProdRuleSet::<LL1>::build_from(build_prs(ll_id, false));
        ll1.set_start(start);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
            .collect::<HashMap<_, _>>();
        let parser_tables = ParserTables::build_from(ParserGen::build_from_rules(ll1, "Test".to_string()));
        let mut parser = parser_tables.make_parser();
        for (input, expected_success) in sequences {
            if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
            let stream = input.chars().into_iter().index_start::<CaretCol>(1).filter_map(|(i, c)| {
                if c.is_ascii_whitespace() {
                    None
                } else {
                    let c_str = c.to_string();
                    if let Some(s) = symbols.get(&c_str) {
                        // println!("stream: '{}' -> sym!({})", c, symbol_to_macro(s));
                        Some((*s, c_str, 1, i))
                    } else {
                        panic!("unrecognized test input '{c}' in test {test_id}/{ll_id}/{start}, input {input}");
                    }
                }
            });
            let mut listener = Stub(BufLog::new());
            let success = match parser.parse_stream(&mut listener, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully"); }
                    true
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    false
                }
            };
            if VERBOSE {
                let msg = listener.0.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            assert_eq!(success, expected_success, "test {test_id}/{ll_id}/{start} failed for input {input}");
        }
    }
}

#[test]
fn parser_parse_stream_id() {

    struct Stub(BufLog);
    impl ListenerWrapper for Stub {
        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.0
        }
    }

    let tests = vec![
        (T::RTS(9), 0, 2, 999, vec![
            ("var a , b ,", None),
        ]),
        (T::PRS(20), 0, 5, 999, vec![
            ("struct test1 { a : int ; b : string ; c : bool ; }", None),
            ("struct test2 { a : int ; b : string ; c : bool }", Some(vec![
                "syntax error: found input '}' instead of ';', line 1, col 15"
            ])),
        ]),
        (T::PRS(100), 0, 999, 999, vec![
            ("c a c a c b b", None),
            ("c a c b a c b", None),
        ]),
        (T::RTS(23), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", None),
        ]),
        (T::RTS(27), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", None),
        ]),
        (T::PRS(33), 0, 999, 999, vec![
            // A -> A a | b c | b d
            ("b c a a", None),
            ("b d a a", None),
            ("b c", None),
            ("b d", None),
        ]),
        (T::PRS(43), 0, 7, 6, vec![
            // BATCH -> GROUP ';' BATCH <L> | ε
            // GROUP -> '[' EXPR ']' | '(' EXPR ')'
            // EXPR -> FACTOR '*' FACTOR;
            // FACTOR -> id | int | '(' EXPR ')';
            ("[ 1 * 2 ] ;", None),
            ("[ ( 1 * 2 * 3 ] ;", Some(vec![
                "syntax error: found input '*' instead of ')', line 1, col 6"
            ])),
            ("[ 1 * 2 ; [ ( 3 * 4 ) * ] ; [ 5 * 6 ] ;", Some(vec![
                "syntax error: found input ';' instead of ']', line 1, col 5",
                "syntax error: found input ']' instead of '(', 'id', 'int' while parsing '►FACTOR', line 1, col 13"
            ])),
        ]),
        (T::PRS(61), 0, 99, 99, vec![
            ("- 0 +", None),
            ("- - 1 + +", None),
            ("- 0", None),
            ("1 +", None),
            ("0", None),
            ("1", None),
        ]),
        (T::PRS(63), 0, 4, 99, vec![
            ("a * b", None),
            ("a + b", None),
            ("- a", None),
            ("a * b + c", None),
            ("a + b * c", None),
            ("- a * b", None),
            ("a * - b", None),
        ]),
        (T::PRS(51), 0, 8, 7, vec![
            // E -> 'abs' E | E '^' E | E '*' E | '-' E | E '+' E | F;
            // F -> ( E ) | NUM | ID
            ("1 ^ 2", None),
            ("1 + 2 * 3 + 4 ^ 5 * 6 + 7 ^ 8", None),
            ("2 ' ^ 3", None),
            ("- 4 * 3", None),
            ("3 * - 4", None),
            ("( 1 + 2 ) * ( 3 + - abs i * - 5 + 6 ) ^ 2", None)
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, id_id, num_id, sequences)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id:?}/{start}", ""); }
        let ll1 = ll_id.build_prs(test_id, start, false);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
            .collect::<HashMap<_, _>>();
        let parser_tables = ParserTables::build_from(ParserGen::build_from_rules(ll1, "Test".to_string()));
        let mut parser = parser_tables.make_parser();
        for (input, expected_errors) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = input.split_ascii_whitespace().index_start::<CaretCol>(1).map(|(i, w)| {
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), 1, i)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        (num_id, w.to_string(), 1, i)
                    } else {
                        (id_id, w.to_string(), 1, i)
                    }
                }
            });
            let mut listener = Stub(BufLog::new());
            let errors = match parser.parse_stream(&mut listener, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully"); }
                    None
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    Some(listener.0.get_errors().map(|s| s.as_str()).to_vec())
                }
            };
            if VERBOSE {
                let msg = listener.0.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            assert_eq!(errors, expected_errors, "test {test_id}/{ll_id:?}/{start} failed for input {input}");
        }
    }
}

mod listener {
    use crate::grammar::tests::old_build_rts_prs::build_prs;
    use crate::grammar::{AltId, VarId};
    use crate::lexer::CaretCol;
    use crate::log::{BufLog, LogStatus};
    use crate::parser::{Call, ListenerWrapper};
    use super::*;

    // Code generated by rparser -------------------------------------

    // - 0: E -> T E_1
    // - 1: T -> F T_1
    // - 2: F -> ( E )
    // - 3: F -> NUM
    // - 4: F -> ID
    // - 5: E_1 -> + T E_1
    // - 6: E_1 -> - T E_1
    // - 7: E_1 -> ε
    // - 8: T_1 -> * F T_1
    // - 9: T_1 -> / F T_1
    // - 10: T_1 -> ε
    pub enum CtxF { LpRp, Num(String), Id(String) }
    pub enum CtxE1 { Add, Sub, Empty }
    pub enum CtxT1 { Mul, Div, Empty }

    pub trait ExprListenerTrait {
        fn get_log(&mut self) -> &mut impl Logger;
        fn exit(&mut self) {}
        fn enter_e(&mut self) {}
        fn enter_t(&mut self) {}
        fn enter_f(&mut self) {}
        fn enter_e_1(&mut self) {}
        fn enter_t_1(&mut self) {}
        fn exit_e(&mut self) {}
        fn exit_t(&mut self) {}
        fn exit_f(&mut self, _ctx: CtxF) {}
        fn exit_e_1(&mut self, _ctx: CtxE1) {}
        fn exit_t_1(&mut self, _ctx: CtxT1) {}
    }

    struct Wrapper<T> {
        listener: T,
        stack_t: Vec<String>
    }

    impl<T: ExprListenerTrait> Wrapper<T> {
        pub fn new(listener: T) -> Self {
            Self { listener, stack_t: Vec::new() }
        }

        pub fn give_listener(self) -> T {
            self.listener
        }
    }

    // `Parser::parse_stream_hook` requires a type implementing `Listener`, but we can only implement
    // `Listener` on a local type, not as a blanket implementation on any type implementing `ExprListenerTrait`,
    // so we must have the `ListenerWrapper` wrapper type above.
    impl<T: ExprListenerTrait> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Loop | Call::Enter => {
                    match nt {
                        0 => self.listener.enter_e(),
                        1 => self.listener.enter_t(),
                        2 => self.listener.enter_f(),
                        3 => self.listener.enter_e_1(),
                        4 => self.listener.enter_t_1(),
                        _ => panic!("unexpected nt exit value: {nt}")
                    }
                }
                Call::Exit => {
                    match alt_id {
                        0 => {}
                        1 => {}
                        2 => self.listener.exit_f(CtxF::LpRp),
                        3 => self.listener.exit_f(CtxF::Num(self.stack_t.pop().unwrap())),
                        4 => self.listener.exit_f(CtxF::Id(self.stack_t.pop().unwrap())),
                        5 => self.listener.exit_e_1(CtxE1::Add),
                        6 => self.listener.exit_e_1(CtxE1::Sub),
                        7 => self.exit_e_1(),
                        8 => self.listener.exit_t_1(CtxT1::Mul),
                        9 => self.listener.exit_t_1(CtxT1::Div),
                        10 => self.exit_t_1(),
                        _ => panic!("unexpected nt exit alternative id: {nt}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            // false
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            self.listener.get_log()
        }
    }

    impl<T: ExprListenerTrait> Wrapper<T> {
        fn exit(&mut self) {
            self.listener.exit();
        }

        fn exit_e_1(&mut self) {
            self.listener.exit_e_1(CtxE1::Empty);
            self.listener.exit_e();
        }

        fn exit_t_1(&mut self) {
            self.listener.exit_t_1(CtxT1::Empty);
            self.listener.exit_t();
        }
    }

    // User code -----------------------------------------------------

    struct TestListener {
        result: Vec<String>,
        level: usize,
        verbose: bool,
        log: BufLog
    }

    impl TestListener {
        pub fn new(verbose: bool) -> Self {
            Self { result: Vec::new(), level: 0, verbose, log: BufLog::new() }
        }
    }

    impl ExprListenerTrait for TestListener {
        fn get_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

        fn exit(&mut self) {
            if self.verbose { println!("{: <1$}$", "", self.level * 4); }
            self.result.push("$".to_string());
        }

        fn enter_e(&mut self) {
            if self.verbose { println!("{: <1$}(E", "", self.level * 4); }
            self.result.push("(E".to_string());
            self.level += 1;
        }

        fn enter_t(&mut self) {
            if self.verbose { println!("{: <1$}(T", "", self.level * 4); }
            self.result.push("(T".to_string());
            self.level += 1;
        }

        fn enter_f(&mut self) {
            if self.verbose { println!("{: <1$}(F", "", self.level * 4); }
            self.result.push("(F".to_string());
            self.level += 1;
        }

        // we're not interested in enter_e_1

        fn enter_t_1(&mut self) {
            if self.verbose { println!("{: <1$}(T_1", "", self.level * 4); }
            self.result.push("(T_1".to_string());
            self.level += 1;
        }

        fn exit_e(&mut self) {
            self.level -= 1;
            if self.verbose { println!("{: <1$} E)", "", self.level * 4); }
            self.result.push("E)".to_string());
        }

        fn exit_t(&mut self) {
            self.level -= 1;
            if self.verbose { println!("{: <1$} T)", "", self.level * 4); }
            self.result.push("T)".to_string());
        }

        fn exit_f(&mut self, ctx: CtxF) {
            self.level -= 1;
            let output = match ctx {
                CtxF::LpRp => format!("F)"),
                CtxF::Num(n) => format!("F)=#{n}"),
                CtxF::Id(i) => format!("F)='{i}'"),
            };
            if self.verbose { println!("{: <1$} {output}", "", self.level * 4); }
            self.result.push(output);
        }

        // we're not interested in exit_e_1

        fn exit_t_1(&mut self, _ctx: CtxT1) {
            self.level -= 1;
            if self.verbose { println!("{: <1$} T_1)", "", self.level * 4); }
            self.result.push("T_1)".to_string());
        }
    }

    #[test]
    fn parser_parse_stream() {
        let tests = vec![
            (4, 0, vec![
                // E -> T E_1
                // T -> F T_1
                // F -> ( E ) | NUM | ID
                // E_1 -> + T E_1 | - T E_1 | ε
                // T_1 -> * F T_1 | / F T_1 | ε
                ("a+2*b", true, vec![
                    "(E", "(T", "(F", "F)='a'", "(T_1", "T_1)", "T)", "(T", "(F", "F)=#2", "(T_1", "(F", "F)='b'", "T_1)", "(T_1", "T_1)", "T)", "E)", "$"]),
                ("a*(4+5)", true, vec![
                    "(E", "(T", "(F", "F)='a'", "(T_1", "(F", "(E", "(T", "(F", "F)=#4", "(T_1", "T_1)", "T)",
                    "(T", "(F", "F)=#5", "(T_1", "T_1)", "T)", "E)", "F)", "T_1)", "(T_1", "T_1)", "T)", "E)", "$"]),
            ])
        ];
        const VERBOSE: bool = false;
        for (test_id, (ll_id, start, sequences)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id}/{start}", ""); }
            let mut ll1 = ProdRuleSet::<LL1>::build_from(build_prs(ll_id, false));
            ll1.set_start(start);
            let symbols = (0..ll1.get_num_t() as TokenId)
                .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
                .collect::<HashMap<_, _>>();
            let parser_tables = ParserTables::build_from(ParserGen::build_from_rules(ll1, "Test".to_string()));
            let mut parser = parser_tables.make_parser();
            for (input, expected_success, expected_result) in sequences {
                if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
                let stream = input.chars().into_iter().index_start::<CaretCol>(1).filter_map(|(i, c)| {
                    let c_str = c.to_string();
                    if c.is_ascii_whitespace() {
                        None
                    } else {
                        Some(match c {
                            '0'..='9' => (6, c_str, 1, i),
                            'a'..='z' => (7, c_str, 1, i),
                            _ => {
                                if let Some(s) = symbols.get(&c_str) {
                                    // println!("stream: '{}' -> sym!({})", c, symbol_to_macro(s));
                                    (*s, c_str, 1, i)
                                } else {
                                    panic!("unrecognized test input '{c}' in test {test_id}/{ll_id}/{start}, input {input}");
                                }
                            }
                        })
                    }
                });

                // User code -----------------------------------------

                let listener = TestListener::new(VERBOSE);
                let mut wrapper = Wrapper::new(listener);
                let success = match parser.parse_stream(&mut wrapper, stream) {
                    Ok(_) => {
                        if VERBOSE { println!("parsing completed successfully"); }
                        true
                    }
                    Err(e) => {
                        if VERBOSE { println!("parsing failed: {e}"); }
                        false
                    }
                };
                let listener = wrapper.give_listener();
                if VERBOSE {
                    let msg = listener.log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                    if !msg.is_empty() {
                        println!("Messages:\n{msg}");
                    }
                }

                // ---------------------------------------------------

                assert_eq!(success, expected_success, "test {test_id}/{ll_id}/{start} failed for input {input}");
                if VERBOSE { println!("listener data: {}", listener.result.join(" -> ")); }
                assert_eq!(listener.result, expected_result, "test {test_id}/{ll_id}/{start} failed for input {input}");
            }
        }
    }
}