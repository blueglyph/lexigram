#![cfg(test)]

use std::collections::HashMap;
use iter_index::IndexerIterator;
use crate::{CollectJoin, LL1};
use crate::dfa::TokenId;
use crate::grammar::{ProdRuleSet, Symbol, VarId};
use crate::grammar::tests::{build_prs, build_rts, complete_symbol_table, T};
use crate::lexer::{CaretCol, LexerToken};
use crate::parser::Listener;
use crate::parsergen::ParserGen;
use crate::symbol_table::SymbolTable;

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates an `OpCode` instance.
    ///
    /// # Examples
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::opcode;
    /// # use rlexer::grammar::VarId;
    /// # use rlexer::parser::OpCode;
    /// assert_eq!(opcode!(e), OpCode::Empty);
    /// assert_eq!(opcode!(t 2), OpCode::T(2 as TokenId));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1));
    /// assert_eq!(opcode!(end), OpCode::End);
    #[macro_export(local_inner_macros)]
    macro_rules! opcode {
        (e) => { OpCode::Empty };
        (t $id:expr) => { OpCode::T($id as TokenId) };
        (nt $id:expr) => { OpCode::NT($id as VarId) };
        (loop $id:expr) => { OpCode::Loop($id as VarId) };
        (exit $id:expr) => { OpCode::Exit($id as VarId) };
        (nt $id:expr) => { OpCode::NT($id as VarId, 0) };
        (loop $id:expr) => { OpCode::Loop($id as VarId, 0) };
        (exit $id:expr) => { OpCode::Exit($id as VarId, 0) };
        (end) => { OpCode::End };
    }

    /// Generates an opcode strip. A strip is made up of `OpCode` items separated by a comma.
    ///
    /// # Example
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::grammar::{ProdFactor, Symbol, VarId};
    /// # use rlexer::{strip, opcode};
    /// # use rlexer::parser::OpCode;
    /// assert_eq!(strip!(nt 1, loop 5, t 3, e), vec![opcode!(nt 1), opcode!(loop 5), opcode!(t 3), opcode!(e)]);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! strip {
        () => { std::vec![] };
        ($($a:ident $($b:expr)?,)+) => { strip![$($a $($b)?),+] };
        ($($a:ident $($b:expr)?),*) => { std::vec![$(opcode!($a $($b)?)),*] };
    }
}

// ---------------------------------------------------------------------------------------------

#[test]
fn parser_parse_stream() {

    struct Stub();
    impl Listener for Stub {}

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
        let mut ll1 = ProdRuleSet::<LL1>::from(build_prs(ll_id, false));
        ll1.set_start(start);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
            .collect::<HashMap<_, _>>();
        let mut parser = ParserGen::from_rules(ll1, "Test".to_string()).make_parser();
        for (input, expected_success) in sequences {
            if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
            let mut stream = input.chars().into_iter().index_start::<CaretCol>(1).filter_map(|(i, c)| {
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
            let success = match parser.parse_stream(&mut Stub(), stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully"); }
                    true
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    false
                }
            };
            assert_eq!(success, expected_success, "test {test_id}/{ll_id}/{start} failed for input {input}");
        }
    }
}

#[test]
fn parser_parse_stream_id() {

    struct Stub();
    impl Listener for Stub {}

    let tests = vec![
        (T::RTS(9), 0, 2, 999, vec![
            ("var a , b ,", true),
        ]),
        (T::PRS(20), 0, 5, 999, vec![
            ("struct test1 { a : int ; b : string ; c : bool ; }", true),
            ("struct test2 { a : int ; b : string ; c : bool }", false),
        ]),
        (T::PRS(22), 0, 3, 4, vec![
            // - 0: E -> id E_1
            // - 1: E_1 -> ε
            // - 2: E_1 -> * id E_1
            // - 3: E_1 -> + id E_1
            // - 4: E_1 -> & E_2
            // - 5: E_2 -> * id E_1
            // - 6: E_2 -> + id E_1
            ("a + b * c", true),
            ("a + b & * c * d", true),
        ]),
        (T::PRS(23), 0, 3, 4, vec![
            // - 0: E -> F E_1
            // - 1: F -> id
            // - 2: F -> num
            // - 3: E_1 -> ε
            // - 4: E_1 -> * F E_1
            // - 5: E_1 -> + F E_1
            // - 6: E_1 -> & E_2
            // - 7: E_2 -> * F E_1
            // - 8: E_2 -> + F E_1
            ("1 + 2 * 3", true),
            ("a + 1 & * b * 2", true),
        ]),
        (T::PRS(100), 0, 999, 999, vec![
            ("c a c a c b b", true),
            ("c a c b a c b", true),
        ]),
        (T::RTS(23), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", true),
        ]),
        (T::RTS(27), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", true),
        ]),
        (T::PRS(33), 0, 999, 999, vec![
            // A -> A a | b c | b d
            ("b c a a", true),
            ("b d a a", true),
            ("b c", true),
            ("b d", true),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, id_id, num_id, sequences)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id:?}/{start}", ""); }
        let mut ll1 = ll_id.get_prs(test_id, start, false);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
            .collect::<HashMap<_, _>>();
        let mut parser = ParserGen::from_rules(ll1, "Test".to_string()).make_parser();
        for (input, expected_success) in sequences {
            if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
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
            let success = match parser.parse_stream(&mut Stub(), stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully"); }
                    true
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    false
                }
            };
            assert_eq!(success, expected_success, "test {test_id}/{ll_id:?}/{start} failed for input {input}");
        }
    }
}

mod listener {
    use crate::grammar::tests::build_prs;
    use crate::grammar::{FactorId, VarId};
    use crate::lexer::{CaretCol, LexerToken};
    use crate::parser::{Call, Listener};
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

    struct ListenerWrapper<T> {
        listener: T,
        stack_t: Vec<String>
    }

    impl<T: ExprListenerTrait> ListenerWrapper<T> {
        pub fn new(listener: T) -> Self {
            Self { listener, stack_t: Vec::new() }
        }

        pub fn listener(self) -> T {
            self.listener
        }
    }

    // `Parser::parse_stream_hook` requires a type implementing `Listener`, but we can only implement
    // `Listener` on a local type, not as a blanket implementation on any type implementing `ExprListenerTrait`,
    // so we must have the `ListenerWrapper` wrapper type above.
    impl<T: ExprListenerTrait> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: FactorId, t_data: Option<Vec<String>>) {
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
                    match factor_id {
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
                        _ => panic!("unexpected nt exit factor id: {nt}")
                    }
                }
                Call::End => {
                    self.exit();
                }
            }
            // false
        }
    }

    impl<T: ExprListenerTrait> ListenerWrapper<T> {
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
        verbose: bool
    }

    impl TestListener {
        pub fn new(verbose: bool) -> Self {
            Self { result: Vec::new(), level: 0, verbose }
        }
    }

    impl ExprListenerTrait for TestListener {
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

        fn exit_t_1(&mut self, ctx: CtxT1) {
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
            let mut ll1 = ProdRuleSet::<LL1>::from(build_prs(ll_id, false));
            ll1.set_start(start);
            let symbols = (0..ll1.get_num_t() as TokenId)
                .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
                .collect::<HashMap<_, _>>();
            let mut parser = ParserGen::from_rules(ll1, "Test".to_string()).make_parser();
            for (input, expected_success, expected_result) in sequences {
                if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
                let mut stream = input.chars().into_iter().index_start::<CaretCol>(1).filter_map(|(i, c)| {
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
                let mut wrapper = ListenerWrapper::new(listener);
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
                let listener = wrapper.listener();

                // ---------------------------------------------------

                assert_eq!(success, expected_success, "test {test_id}/{ll_id}/{start} failed for input {input}");
                if VERBOSE { println!("listener data: {}", listener.result.join(" -> ")); }
                assert_eq!(listener.result, expected_result, "test {test_id}/{ll_id}/{start} failed for input {input}");
            }
        }
    }
}