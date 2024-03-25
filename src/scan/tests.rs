#![cfg(test)]

use std::collections::BTreeMap;
use std::io::Cursor;
use crate::{btreemap, escape_string};
use crate::dfa::{DfaBuilder, Token, TokenId};
use crate::dfa::tests::{build_re, print_graph};
use crate::lexgen::tests::print_source_code;
use super::*;

#[test]
fn scanner() {
    const VERBOSE: bool = false;

    fn eval(result: &Result<Token, LexScanError>, verbose: bool) -> Option<Token> {
        match result {
            Ok(token) => {
                if verbose { println!("=> OK {}", token.0); }
                Some(token.clone())
            }
            Err(e) => {
                if verbose { print!("## {e}"); }
                if e.token.is_some() {
                    if verbose { println!(" => OK"); }
                    e.token.clone()
                } else {
                    if verbose { println!(" => Error"); }
                    None
                }
            }
        }
    }

    let tests: Vec<(usize, BTreeMap<TokenId, Vec<&str>>, Vec<(&str, u64)>, Vec<(&str, Vec<TokenId>)>)> = vec![
        // [ \t\n\r]*[0-9]+(<end:0>|\.[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
        (10, btreemap![
            0 => vec!["0", "0 ", "10", "9876543210"],
            1 => vec!["0.5", "0.1 ", "9876543210.0123456789"],
            2 => vec!["0x0", "0xF ", "0x0123456789abcdef", "0x0123456789ABCDEF", "0xff"]
         ],
         vec![("a", 0), (".5", 0), ("()", 0), ("0xy", 2), ("0.a", 2), ("", 0)],
         vec![("0 1 2 ", vec![0, 0, 0]), ("0x1 0.5 15", vec![2, 1, 0])],
        ),

        // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>)
        (11, btreemap![
            0 => vec!["a", "id", "id05b", "ifa", "printa"],
            1 => vec!["if", "if x"],
            2 => vec!["print", "print "],
            3 => vec!["=", "=5", "=a"],
            4 => vec!["+", "+5", "+a"],
            5 => vec![";", ";a"]
         ],
         vec![("0", 0), ("", 0), ("-", 0), ("*", 0)],
         vec![("\ta = x; if i=j print b;\n", vec![0, 3, 0, 5, 1, 0, 3, 0, 2, 0, 5])]
        ),
    ];
    for (test_id, token_tests, err_tests, stream_tests) in tests {
        let mut dfa = DfaBuilder::new(build_re(test_id)).build();
        dfa.normalize();
        if VERBOSE { print_graph(&dfa); }
        let lexgen = LexGen::new(dfa);
        if VERBOSE { print_source_code(&lexgen); }
        let mut interpret = Scanner::new(lexgen);
        for (exp_token, inputs) in token_tests {
            for input in inputs {
                if VERBOSE { println!("\"{}\": (should succeed)", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                interpret.attach_steam(stream);
                let result = interpret.get_token();
                let token = eval(&result, VERBOSE);
                assert_eq!(token, Some(Token(exp_token)), "test {} failed for input '{}'", test_id, escape_string(input));
                interpret.detach_stream();
            }
        }
        for (input, expected_pos) in err_tests {
            if VERBOSE { println!("\"{}\": (should fail)", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            interpret.attach_steam(stream);
            let result = interpret.get_token();
            let token = eval(&result, VERBOSE);
            assert_eq!(token, None, "test {} failed for input '{}'", test_id, escape_string(input));
            if let Err(e) = result {
                assert_eq!(e.pos, expected_pos)
            }
        }
        // gathering all the tokens, one at a time:
        for (input, expected_tokens) in stream_tests.clone() {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            interpret.attach_steam(stream);
            let mut result = Vec::new();
            while interpret.is_open() {
                let token = &interpret.get_token();
                let t = eval(token, false);
                if let Some(token) = t {
                    if VERBOSE { print!(" {}", token.0); }
                    result.push(token.0);
                } else {
                    assert_eq!(t, None);
                    break;
                }
            }
            if VERBOSE { println!(); }
            assert_eq!(result, expected_tokens, "test {} failed for input '{}'", test_id, escape_string(input));
        }
        // gathering all the tokens with an iterator:
        for (input, expected_tokens) in stream_tests {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            interpret.attach_steam(stream);
            let result = interpret.tokens().map(|t| t.0).collect::<Vec<_>>();
            assert_eq!(result, expected_tokens, "test {} failed for input '{}'", test_id, escape_string(input));
            assert!(interpret.get_error() == None || interpret.get_error().unwrap().is_eos, "test {} failed for input '{}'",
                    test_id, escape_string(input));
            // or:
            // assert!(!matches!(interpret.get_error(), Some(LexScanError { is_eos: false, .. })), "test {} failed for input '{}'",
            //         test_id, escape_string(input));
        }
    }
}
