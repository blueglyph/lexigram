#![cfg(test)]

use std::collections::{BTreeMap, BTreeSet};
use std::io::Cursor;
use crate::{btreemap, escape_string, node, term};
use crate::dfa::*;
use crate::segments::*;
use crate::dfa::tests::{build_re, print_dfa};
use crate::lexgen::tests::print_source_code;
use crate::vectree::VecTree;
use super::*;

#[test]
fn scanner() {
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
                    panic!();
                    // e.token_ch.clone()
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
    const VERBOSE: bool = true;
    for (test_id, token_tests, err_tests, stream_tests) in tests {
        let mut dfa = DfaBuilder::from_re(build_re(test_id)).build();
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

fn build_scanner<R: Read>(test: usize) -> Scanner<R> {
    let mut re = VecTree::<ReNode>::new();
    let trees = match test {
        1 => {
            // mode 0: ([ \t\n\r]+<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add(None, node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let p0 = re.add(Some(cc0), node!(+));
            let or0 = re.add(Some(p0), node!(|));
            re.add(Some(or0), node!([' ', '\t', '\n', '\r']));
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1) + term!(skip))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add(Some(or2), node!(['0'-'9']));
            re.add(Some(cc2), node!(=0));

            // mode 1: ('*'/<pop>|.+<skip>)
            let mut re1 = VecTree::new();
            let or = re1.add(None, node!(|));
            let cc1 = re1.add(Some(or), node!(&));
            re1.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(pop))]);
            let cc2 = re1.add(Some(or), node!(&));
            let s2 = re1.add(Some(cc2), node!(+));
            re1.add(Some(s2), node!([' ', '\t', '\n', '\r', 'a'-'z']));
            re1.add(Some(cc2), node!(term!(skip)));

            btreemap![0 => re, 1 => re1]
        },

        2 => {
            // mode 0: ([ \t\n\r]+<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add(None, node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let p0 = re.add(Some(cc0), node!(+));
            let or0 = re.add(Some(p0), node!(|));
            re.add(Some(or0), node!([' ', '\t', '\n', '\r']));
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1) + term!(skip))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add(Some(or2), node!(['0'-'9']));
            re.add(Some(cc2), node!(=0));

            // mode 1: ('*'/<pop>|.+<skip>)
            let mut re1 = VecTree::new();
            let or = re1.add(None, node!(|));
            let cc1 = re1.add(Some(or), node!(&));
            re1.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(pop))]);
            let cc2 = re1.add(Some(or), node!(&));
            let s2 = re1.add(Some(cc2), node!(+));
            re1.add(Some(s2), node!([DOT]));
            re1.add(Some(cc2), node!(term!(skip)));

            btreemap![0 => re, 1 => re1]
        },

        _ => btreemap![],
    };
    const VERBOSE: bool = true;
    let dfas = trees.into_iter().enumerate().map(|(dfa_id, (mode, re))| {
        if VERBOSE { println!("creating dfa for mode {mode}"); }
        // let dfa = DfaBuilder::from_re(re).build();
        let mut dfa_builder = DfaBuilder::from_re(re);
        let dfa = dfa_builder.build();
        assert!(dfa_builder.get_messages().is_empty(), "warnings/errors when building scanner {test}: (DFA #{dfa_id})\n{}", dfa_builder.get_messages());
        if VERBOSE {
            print_dfa(&dfa);
        }
        (mode as u16, dfa)
    }).collect::<Vec<_>>();
    let mut dfa_builder = DfaBuilder::new();
    if VERBOSE { println!("merging dfa modes"); }
    let mut dfa = dfa_builder.build_from_dfa_modes(dfas).expect(&format!("failed to build lexer #{test}\n{}", dfa_builder.get_messages()));
    assert!(dfa_builder.get_messages().is_empty(), "warnings/errors when building scanner {test} (merging DFAs):\n{}", dfa_builder.get_messages());
    if VERBOSE {
        print_dfa(&dfa);
        println!("normalizing");
    }
    dfa.normalize();
    if VERBOSE {
        print_dfa(&dfa);
        println!("creating lexer");
    }
    let mut lexgen = LexGen::new();
    // lexgen.max_utf8_chars = 10;
    lexgen.build(&dfa);
    // if VERBOSE {
    //     print_source_code(&lexgen);
    //     println!("creating scanner");
    // }
    Scanner::new(lexgen)
}

#[test]
fn scanner_modes() {
    let tests = vec![
        (1, vec![
            // no error
            (" 10 20 30", vec![0, 0, 0]),
            (" 10 20 ", vec![0, 0]),
            (" 5 /* blabla */ 6", vec![0, 0]),
        ]),
        (2, vec![
            // no error
            (" 10 20 30", vec![0, 0, 0]),
            (" 10 20 ", vec![0, 0]),
            (" 5 /* blabla */ 6", vec![0, 0]),
        ]),
    ];
    const VERBOSE: bool = true;
    for (test_id, (scan_id, inputs)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("test {test_id}:"); }
        let mut scanner = build_scanner(scan_id);
        for (input, expected_tokens) in inputs {
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

        }
        if VERBOSE { println!("--------------------------------------\n"); }
    }
}
