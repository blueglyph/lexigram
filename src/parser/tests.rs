#![cfg(test)]

use std::collections::HashMap;
use crate::{CollectJoin, LL1};
use crate::dfa::TokenId;
use crate::grammar::{ProdRuleSet, Symbol, VarId};
use crate::grammar::tests::{build_prs, build_rts, complete_symbol_table, T};
use crate::parser::Listener;
use crate::parsergen::ParserBuilder;
use crate::symbol_table::SymbolTable;

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
        let mut ll1 = ProdRuleSet::<LL1>::from(build_prs(ll_id));
        ll1.set_start(start);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| Symbol::T(t))
            .map(|s| (s.to_str(ll1.get_symbol_table()), s))
            .collect::<HashMap<_, _>>();
        let mut parser = ParserBuilder::from_rules(ll1).make_parser();
        for (input, expected_success) in sequences {
            if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
            let mut stream = input.chars().into_iter().filter_map(|c| {
                if c.is_ascii_whitespace() {
                    None
                } else {
                    let c_str = c.to_string();
                    if let Some(s) = symbols.get(&c_str) {
                        // println!("stream: '{}' -> sym!({})", c, symbol_to_macro(s));
                        Some((*s, c_str))
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
        ])
    ];
    const VERBOSE: bool = true;
    for (test_id, (ll_id, start, id_id, num_id, sequences)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id:?}/{start}", ""); }
        let mut ll1 = ll_id.get_prs(test_id, start);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| Symbol::T(t))
            .map(|s| (s.to_str(ll1.get_symbol_table()), s))
            .collect::<HashMap<_, _>>();
        let mut parser = ParserBuilder::from_rules(ll1).make_parser();
        for (input, expected_success) in sequences {
            if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
            let stream = input.split_ascii_whitespace().map(|w| {
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string() )
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        (Symbol::T(num_id), w.to_string())
                    } else {
                        (Symbol::T(id_id), w.to_string())
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

// #[cfg(disabled)]
mod opcodes {
    use crate::grammar::{ProdRuleSet, Symbol, VarId};
    use crate::grammar::tests::{build_prs, build_rts, complete_symbol_table, print_logs, print_prs_summary, T};
    use crate::{CollectJoin, LL1};
    use crate::parser::Parser;
    use crate::parsergen::ParserBuilder;
    use crate::symbol_table::SymbolTable;

    fn get_factors_str(parser: &Parser) -> Vec<String> {
        parser.factors.iter().enumerate().map(|(id, (v, f))|
            format!("{id:2}: {} -> {}", Symbol::NT(*v).to_str(Some(&parser.symbol_table)), f.iter().map(|s| s.to_str(Some(&parser.symbol_table))).join(" "))
        ).collect()
    }

    fn print_opcodes(parser: &Parser) {
        let factors = get_factors_str(&parser);
        let width = factors.iter().map(|f| f.len()).max().unwrap();
        let opcodes = factors.into_iter().zip(&parser.opcodes).map(|(s, ops)|
            format!("- {s:width$} - {}", ops.into_iter().map(|s| s.to_str(Some(&parser.symbol_table))).join(" "), width=width)
        ).join("\n");
        println!("{}", opcodes)
    }

    #[test]
    fn parser_opcodes() {
        let tests = vec![
            // A -> b (c d)+
            // =>
            // A -> b B
            // B -> c d B | c d
            // =>
            // A -> b B
            // B -> c d B_1
            // B_1 -> B
            // B_1 -> ε
            (T::RTS(9), 0),
            // A -> b (c d)*
            // =>
            // A -> b B
            // B -> c d B
            // B -> ε
            (T::RTS(12), 0),
            (T::RTS(16), 0),
            // E -> E * E | E '&' '*' E | E + E | E '&' '+' E | id
            // -  0: E -> id E_1     - ◄0 E_1 id
            // -  1: E_1 -> ε        -
            // -  2: E_1 -> * id E_1 - ●E_1 id *
            // -  3: E_1 -> + id E_1 - ●E_1 id +
            // -  4: E_1 -> & E_2    - E_2 &
            // -  5: E_2 -> * id E_1 - ◄5 E_1 id *
            // -  6: E_2 -> + id E_1 - ◄6 E_1 id +
            (T::PRS(22), 0),
            (T::PRS(26), 1),
        ];
        const VERBOSE: bool = true;
        const VERBOSE_DETAILS: bool = true;
        for (test_id, (rule_id, start_nt)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
            let mut ll1 = rule_id.get_prs(test_id, start_nt);
            // let parsing_table = ll1.create_parsing_table();
            // if VERBOSE && (ll1.get_log().num_warnings() > 0) || (ll1.get_log().num_notes() > 0) {
            //     print_logs(&ll1);
            // }
            // let result_flags = ll1.flags.iter().enumerate().filter_map(|(v, &f)| if f != 0 { Some((v as VarId, f)) } else { None }).collect::<BTreeMap<_, _>>();
            // let result_fflags = ll1.prods.iter().flat_map(|p| p.iter().map(|f| f.flags)).enumerate().filter_map(|(i, f)| if f != 0 { Some((i, f)) } else { None }).collect::<BTreeMap<_, _>>();
            // let result_parent = ll1.parent.iter().enumerate().filter_map(|(v, &par)| if let Some(p) = par { Some((v as VarId, p)) } else { None }).collect::<BTreeMap<_, _>>();
            if VERBOSE {
                print!("- ");
                print_prs_summary(&ll1);
                // println!("=>");
                // println!("        (T::{rule_id:?}, {start_nt}, btreemap![{}],", result_flags.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
                // println!("         btreemap![{}],", result_fflags.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
                // println!("         btreemap![{}]),", result_parent.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            }
            let mut parser = ParserBuilder::from_rules(ll1).make_parser();
            if VERBOSE {
                println!("Final factors and opcodes:");
                print_opcodes(&parser);
            }
        }
    }
}

mod listener {
    use crate::grammar::tests::build_prs;
    use crate::grammar::VarId;
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

    struct ListenerWrapper<T>(T);

    impl<T: ExprListenerTrait> ListenerWrapper<T> {
        pub fn new(listener: T) -> Self {
            Self(listener)
        }

        pub fn listener(self) -> T {
            self.0
        }
    }

    // `Parser::parse_stream_hook` requires a type implementing `Listener`, but we can only implement
    // `Listener` on a local type, not as a blanket implementation on any type implementing `ExprListenerTrait`,
    // so we must have the `ListenerWrapper` wrapper type above.
    impl<T: ExprListenerTrait> Listener for ListenerWrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, mut t_str: Vec<String>) {
            match call {
                Call::Loop | Call::Enter => {
                    match nt {
                        0 => self.0.enter_e(),
                        1 => self.0.enter_t(),
                        2 => self.0.enter_f(),
                        3 => self.0.enter_e_1(),
                        4 => self.0.enter_t_1(),
                        _ => panic!("unexpected nt exit value: {nt}")
                    }
                }
                Call::Exit => {
                    match factor_id {
                        0 => self.0.exit_e(),
                        1 => self.0.exit_t(),
                        2 => self.0.exit_f(CtxF::LpRp),
                        3 => self.0.exit_f(CtxF::Num(t_str.pop().unwrap())),
                        4 => self.0.exit_f(CtxF::Id(t_str.pop().unwrap())),
                        5 => self.0.exit_e_1(CtxE1::Add),
                        6 => self.0.exit_e_1(CtxE1::Sub),
                        7 => self.0.exit_e_1(CtxE1::Empty),
                        8 => self.0.exit_t_1(CtxT1::Mul),
                        9 => self.0.exit_t_1(CtxT1::Div),
                        10 => self.0.exit_t_1(CtxT1::Empty),
                        _ => panic!("unexpected nt exit factor id: {nt}")
                    }
                }
                Call::Asm => panic!("unexepected Call::Asm in this test"),
            }
            // false
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
                    "(E", "(T", "(F", "F)='a'", "(T_1", "T_1)", "T)", "(T", "(F", "F)=#2", "(T_1", "(F", "F)='b'", "T_1)", "(T_1", "T_1)", "T)", "E)"]),
                ("a*(4+5)", true, vec![
                    "(E", "(T", "(F", "F)='a'", "(T_1", "(F", "(E", "(T", "(F", "F)=#4", "(T_1", "T_1)", "T)",
                    "(T", "(F", "F)=#5", "(T_1", "T_1)", "T)", "E)", "F)", "T_1)", "(T_1", "T_1)", "T)", "E)"]),
            ])
        ];
        const VERBOSE: bool = false;
        for (test_id, (ll_id, start, sequences)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id}/{start}", ""); }
            let mut ll1 = ProdRuleSet::<LL1>::from(build_prs(ll_id));
            ll1.set_start(start);
            let symbols = (0..ll1.get_num_t() as TokenId)
                .map(|t| Symbol::T(t))
                .map(|s| (s.to_str(ll1.get_symbol_table()), s))
                .collect::<HashMap<_, _>>();
            let mut parser = ParserBuilder::from_rules(ll1).make_parser();
            for (input, expected_success, expected_result) in sequences {
                if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
                let mut stream = input.chars().into_iter().filter_map(|c| {
                    let c_str = c.to_string();
                    if c.is_ascii_whitespace() {
                        None
                    } else {
                        Some(match c {
                            '0'..='9' => (Symbol::T(6), c_str),
                            'a'..='z' => (Symbol::T(7), c_str),
                            _ => {
                                if let Some(s) = symbols.get(&c_str) {
                                    // println!("stream: '{}' -> sym!({})", c, symbol_to_macro(s));
                                    (*s, c_str)
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