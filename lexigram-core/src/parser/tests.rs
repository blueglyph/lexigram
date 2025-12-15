// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::collections::HashMap;
use iter_index::IndexerIterator;
use crate::alt::Alternative;
use crate::CollectJoin;
use crate::LL1;
use crate::{TokenId, VarId};
use crate::grammar::ProdRuleSet;
use crate::grammar::tests::old_build_rts_prs::T;
use crate::grammar::tests::old_build_rts_prs::build_prs;
use crate::lexer::{CaretCol, Pos, PosSpan};
use crate::log::{BufLog, BuildFrom, LogStatus, Logger};
use crate::parser::{ListenerWrapper, OpCode, Parser, Symbol};
use lexigram_lib::parsergen::{ParserGen, ParserTables};

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
        (4, 0, vec![
            ("I*I", true),
            ("(N)", true),
            ("((N))", true),
        ]),
        (5, 0, vec![
            ("++;;", true),
            ("--+;;", true),
            ("+-;;", false),
            ("++;;-", false),
            ("++;-", false),
            ("-", false),
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
                        let pos = Pos(1, i);
                        Some((*s, c_str, PosSpan::new(pos, pos)))
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
        (T::RTS(23), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", None),
        ]),
        (T::RTS(27), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", None),
        ]),
        (T::PRS(20), 0, 5, 999, vec![
            ("struct test1 { a : int ; b : string ; c : bool ; }", None),
            ("struct test2 { a : int ; b : string ; c : bool }", Some(vec![
                "syntax error: found input '}' instead of ';', line 1, col 15"
            ])),
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
        (T::PRS(100), 0, 999, 999, vec![
            ("c a c a c b b", None),
            ("c a c b a c b", None),
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
                let pos = Pos(1, i);
                let pos_span = PosSpan::new(pos, pos);
                if let Some(s) = symbols.get(w) {
                    (*s, w.to_string(), pos_span)
                } else {
                    if w.chars().next().unwrap().is_ascii_digit() {
                        (num_id, w.to_string(), pos_span)
                    } else {
                        (id_id, w.to_string(), pos_span)
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
