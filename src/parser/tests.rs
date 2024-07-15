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
    const VERBOSE: bool = false;
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
    use crate::grammar::{ProdFactor, ProdRuleSet, Symbol, VarId};
    use crate::grammar::tests::{build_prs, build_rts, complete_symbol_table, print_logs, print_prs_summary, symbol_to_macro, T};
    use crate::{CollectJoin, LL1, strip};
    use crate::parser::{OpCode, Parser};
    use crate::parsergen::ParserBuilder;
    use crate::symbol_table::SymbolTable;
    use crate::dfa::TokenId;

    fn get_factors_str(parser: &Parser) -> Vec<String> {
        parser.factors.iter().enumerate().map(|(id, (v, f))|
            format!("{id:2}: {} -> {}", Symbol::NT(*v).to_str(Some(&parser.symbol_table)), f.iter().map(|s| s.to_str(Some(&parser.symbol_table))).join(" "))
        ).collect()
    }

    pub(crate) fn opcode_to_macro(s: &OpCode) -> String {
        match s {
            OpCode::Empty => "e".to_string(),
            OpCode::T(t) => format!("t {t}"),
            OpCode::NT(v, n) => if *n > 0 { format!("nt {v}/{n}") } else { format!("nt {v}") },
            OpCode::Loop(v, n) => if *n > 0 { format!("loop {v}/{n}") } else { format!("loop {v}") },
            OpCode::Exit(v, n) => if *n > 0 { format!("exit {v}/{n}") } else { format!("exit {v}") },
            OpCode::End => "end".to_string(),
        }
    }

    fn print_opcodes(parser: &Parser) {
        let factors = get_factors_str(&parser);
        if !factors.is_empty() {
            let indent = 16;
            let opcodes = factors.into_iter().zip(&parser.opcodes).map(|(s, ops)|
                (
                    format!("strip![{}],", ops.iter().map(|o| opcode_to_macro(o)).join(", ")),
                    s,
                    ops.into_iter().map(|s| s.to_str(Some(&parser.symbol_table))).join(" ")
                )
            ).to_vec();
            let width = opcodes.iter().fold((0, 0), |acc, s| (acc.0.max(s.0.len()), acc.1.max(s.1.len())));
            let code = opcodes.into_iter()
                .map(|(a, b, c)| format!("{:indent$}{a:width_a$} // {b:width_b$} - {c}", "", indent=indent, width_a=width.0, width_b=width.1))
                .join("\n");
            println!("{}", code);
        }
    }

    #[test]
    fn parser_opcodes() {
        // terminal:     t (static) or t! (contains a string)
        // non-terminal: ►A
        // exit:         ◄2/1 (factor #2, takes 1 string from lexer stack)
        // loop:         ●1 (factor #1)
        let tests: Vec<(T, VarId, Vec<Vec<OpCode>>)> = vec![
            (T::RTS(9), 0, vec![                        // A -> var (id ,)+
                strip![exit 0, nt 1, t 1],              //  0: A -> var A_1    - ◄0 ►A_1 var
                strip![nt 2, t 3, t 2],                 //  1: A_1 -> id , A_2 - ►A_2 , id!
                strip![exit 2/1],                       //  2: A_2 -> ε        - ◄2/1
                strip![loop 1, exit 3/1],               //  3: A_2 -> A_1      - ●1 ◄3/1
            ]),
            (T::RTS(12), 0, vec![                       // A -> b (c d)*
                strip![exit 0/1, nt 1, t 1],            //  0: A -> b B   - ◄0/1 ►B b!
                strip![loop 1, exit 1/2, t 3, t 2],     //  1: B -> c d B - ●1 ◄1/2 d! c!
                strip![exit 2],                         //  2: B -> ε     - ◄2
            ]),
            (T::RTS(16), 0, vec![                       // A -> A a+ b | c
                strip![exit 0/1, nt 2, t 2],            //  0: A -> c A_1     - ◄0/1 ►A_1 c!
                strip![nt 3, t 0],                      //  1: B -> a B_1     - ►B_1 a!
                strip![loop 2, exit 2/1, t 1, nt 1],    //  2: A_1 -> B b A_1 - ●2 ◄2/1 b! ►B
                strip![exit 3],                         //  3: A_1 -> ε       - ◄3
                strip![exit 4/1],                       //  4: B_1 -> ε       - ◄4/1
                strip![loop 1, exit 5/1],               //  5: B_1 -> B       - ●1 ◄5/1
            ]),
            (T::PRS(4), 0, vec![
                strip![exit 0, nt 3, nt 1],             //  0: E -> T E_1     - ◄0 ►E_1 ►T
                strip![exit 1, nt 4, nt 2],             //  1: T -> F T_1     - ◄1 ►T_1 ►F
                strip![exit 2, t 5, nt 0, t 4],         //  2: F -> ( E )     - ◄2 ) ►E (
                strip![exit 3/1, t 6],                  //  3: F -> N         - ◄3/1 N!
                strip![exit 4/1, t 7],                  //  4: F -> I         - ◄4/1 I!
                strip![loop 3, exit 5, nt 1, t 0],      //  5: E_1 -> - T E_1 - ●3 ◄5 ►T -
                strip![loop 3, exit 6, nt 1, t 1],      //  6: E_1 -> + T E_1 - ●3 ◄6 ►T +
                strip![exit 7],                         //  7: E_1 -> ε       - ◄7
                strip![loop 4, exit 8, nt 2, t 2],      //  8: T_1 -> / F T_1 - ●4 ◄8 ►F /
                strip![loop 4, exit 9, nt 2, t 3],      //  9: T_1 -> * F T_1 - ●4 ◄9 ►F *
                strip![exit 10],                        // 10: T_1 -> ε       - ◄10
            ]),
            (T::PRS(22), 0, vec![                       // E -> E * E | E & * E | E + E | E & + E | id
                strip![exit 0/1, nt 1, t 3],            //  0: E -> id E_1     - ◄0/1 ►E_1 id!
                strip![exit 1],                         //  1: E_1 -> ε        - ◄1
                strip![loop 1, exit 2/1, t 3, t 0],     //  2: E_1 -> * id E_1 - ●1 ◄2/1 id! *
                strip![loop 1, exit 3/1, t 3, t 1],     //  3: E_1 -> + id E_1 - ●1 ◄3/1 id! +
                strip![nt 2, t 2],                      //  4: E_1 -> & E_2    - ►E_2 &
                strip![loop 1, exit 5/1, t 3, t 0],     //  5: E_2 -> * id E_1 - ●1 ◄5/1 id! *
                strip![loop 1, exit 6/1, t 3, t 1],     //  6: E_2 -> + id E_1 - ●1 ◄6/1 id! +
            ]),
            (T::PRS(26), 0, vec![                       // A -> A a | b
                strip![exit 0/1, nt 1, t 1],            //  0: A -> b A_1   - ◄0/1 ►A_1 b!
                strip![loop 1, exit 1/1, t 0],          //  1: A_1 -> a A_1 - ●1 ◄1/1 a!
                strip![exit 2],                         //  2: A_1 -> ε     - ◄2
            ]),
            (T::PRS(26), 1, vec![                       // B -> B a B | b
                strip![exit 0/1, nt 1, t 1],            //  0: B -> b B_1     - ◄0/1 ►B_1 b!
                strip![loop 1, exit 1/2, t 1, t 0],     //  1: B_1 -> a b B_1 - ●1 ◄1/2 b! a!
                strip![exit 2],                         //  2: B_1 -> ε       - ◄2
            ]),
        ];
        const VERBOSE: bool = true;
        for (test_id, (rule_id, start_nt, expected_opcodes)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
            let mut ll1 = rule_id.get_prs(test_id, start_nt);
            if VERBOSE {
                print!("- ");
                print_prs_summary(&ll1);
            }
            let mut parser = ParserBuilder::from_rules(ll1).make_parser();
            if VERBOSE {
                println!("Final factors and opcodes:");
                print_opcodes(&parser);
            }
            assert_eq!(parser.opcodes, expected_opcodes, "test {test_id} {rule_id:?}/{start_nt} failed");
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
        fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId) {
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
                        0 => self.listener.exit_e(),
                        1 => self.listener.exit_t(),
                        2 => self.listener.exit_f(CtxF::LpRp),
                        3 => self.listener.exit_f(CtxF::Num(self.stack_t.pop().unwrap())),
                        4 => self.listener.exit_f(CtxF::Id(self.stack_t.pop().unwrap())),
                        5 => self.listener.exit_e_1(CtxE1::Add),
                        6 => self.listener.exit_e_1(CtxE1::Sub),
                        7 => self.listener.exit_e_1(CtxE1::Empty),
                        8 => self.listener.exit_t_1(CtxT1::Mul),
                        9 => self.listener.exit_t_1(CtxT1::Div),
                        10 => self.listener.exit_t_1(CtxT1::Empty),
                        _ => panic!("unexpected nt exit factor id: {nt}")
                    }
                }
                Call::Asm => panic!("unexepected Call::Asm in this test"),
            }
            // false
        }

        fn t_data(&mut self, t: TokenId, data: String) {
            self.stack_t.push(data);
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