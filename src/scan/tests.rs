#![cfg(test)]

use std::collections::BTreeMap;
use std::io::Cursor;
use crate::{btreemap, escape_string};
use crate::dfa::{DfaBuilder, Token};
use crate::dfa::tests::{build_re, print_dfa};
use crate::lexgen::tests::print_source_code;
use super::*;

#[test]
fn scanner() {
    const VERBOSE: bool = false;

    fn eval(result: &Result<(Token, ChannelId), &LexScanError>, verbose: bool) -> Option<(Token, ChannelId)> {
        match result {
            Ok(token_ch) => {
                if verbose { println!("=> OK {}, #{}", token_ch.0.0, token_ch.1); }
                Some(token_ch.clone())
            }
            Err(e) => {
                if verbose { print!("## {e}"); }
                if e.token_ch.is_some() {
                    if verbose { println!(" => OK"); }
                    e.token_ch.clone()
                } else {
                    if verbose { println!(" => Error"); }
                    None
                }
            }
        }
    }

    let tests = vec![
        // [ \t\n\r]*[0-9]+(<end:0>|\.[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
        (10, btreemap![
            0 => vec!["0", "0 ", "10", "9876543210"],
            1 => vec!["0.5", "0.1 ", "9876543210.0123456789"],
            2 => vec!["0x0", "0xF ", "0x0123456789abcdef", "0x0123456789ABCDEF", "0xff"]
         ],
         vec![("a", 0, false), (".5", 0, false), ("()", 0, false), ("0xy", 2, false), ("0.a", 2, false), ("", 0, true), (" ", 1, true)],
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
         vec![("0", 0, false), ("", 0, true), ("-", 0, false), ("*", 0, false)],
         vec![("\ta = x; if i=j print b;\n", vec![0, 3, 0, 5, 1, 0, 3, 0, 2, 0, 5])]
        ),
    ];
    for (test_id, token_tests, err_tests, stream_tests) in tests {
        let mut dfa = DfaBuilder::new(build_re(test_id)).build();
        dfa.normalize();
        if VERBOSE { print_dfa(&dfa); }
        let lexgen = LexGen::from_dfa(&dfa);
        if VERBOSE { print_source_code(&lexgen); }
        let mut scanner = Scanner::new(lexgen);
        for (exp_token, inputs) in token_tests {
            for input in inputs {
                if VERBOSE { println!("\"{}\": (should succeed)", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                scanner.attach_steam(stream);
                let result = scanner.get_token();
                let token = eval(&result, VERBOSE);
                assert_eq!(token, Some((Token(exp_token), 0)), "test {} failed for input '{}'", test_id, escape_string(input));
                scanner.detach_stream();
            }
        }
        for (input, expected_pos, expected_eos) in err_tests {
            if VERBOSE { println!("\"{}\": (should fail)", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            scanner.attach_steam(stream);
            let result = scanner.get_token();
            let token = eval(&result, VERBOSE);
            assert_eq!(token, None, "test {} failed for input '{}'", test_id, escape_string(input));
            if let Err(e) = result {
                assert_eq!(e.pos, expected_pos, "test {} failed for input '{}', {:?}", test_id, escape_string(input), e);
                assert_eq!(e.is_eos, expected_eos, "test {} failed for input '{}', {:?}", test_id, escape_string(input), e);
            }
        }
        // gathering all the tokens, one at a time:
        for (input, expected_tokens) in stream_tests.clone() {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            scanner.attach_steam(stream);
            let mut result = Vec::new();
            while scanner.is_open() {
                let token = &scanner.get_token();
                let t = eval(token, false);
                if let Some(token_ch) = t {
                    if VERBOSE { print!(" {}, #{}", token_ch.0.0, token_ch.1); }
                    result.push(token_ch.0.0);
                    assert_eq!(token_ch.1, 0, "test {} failed for input {}", test_id, escape_string(input));
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
            scanner.attach_steam(stream);
            let result = scanner.tokens().map(|t| {
                assert_eq!(t.1, 0, "test {} failed for input {}", test_id, escape_string(input));
                t.0.0
            }).collect::<Vec<_>>();
            assert_eq!(result, expected_tokens, "test {} failed for input '{}'", test_id, escape_string(input));
            assert!(scanner.get_error() == None || scanner.get_error().unwrap().is_eos, "test {} failed for input '{}'",
                    test_id, escape_string(input));
            // or:
            // assert!(!matches!(interpret.get_error(), Some(LexScanError { is_eos: false, .. })), "test {} failed for input '{}'",
            //         test_id, escape_string(input));
        }
    }
}
