use std::collections::HashMap;
use crate::CollectJoin;
use crate::dfa::TokenId;
use crate::grammar::{LL1, ProdRuleSet, Symbol};
use crate::grammar::tests::{build_prs, symbol_to_macro};
use crate::parsergen::ParserBuilder;

#[test]
fn parser_parse_stream() {
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
        ])
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, sequences)) in tests.into_iter().enumerate() {
if ll_id != 4 { continue; }
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id}/{start}", ""); }
        let mut ll1 = ProdRuleSet::<LL1>::from(build_prs(ll_id));
        ll1.set_start(start);
        let symbols = (0..ll1.get_num_terminals() as TokenId)
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
            let success = match parser.parse_stream(stream) {
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