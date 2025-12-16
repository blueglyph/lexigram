// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Cursor;
use vectree::VecTree;
use lexigram_core::char_reader::{escape_string, CharReader};
use lexigram_core::lexer::{ChannelId, LexerError, PosSpan};
use lexigram_core::log::{LogReader, LogStatus};
use lexigram_core::{CollectJoin, TokenId};
use crate::{btreemap, node, term, General};
use crate::build::{BuildFrom, TryBuildInto};
use crate::dfa::{Dfa, DfaBuilder, DfaBundle, ReNode};
use crate::dfa::tests::build_re;
use crate::lexergen::{LexerGen, LexerTables};

#[test]
fn todo() {
    todo!("bring back some of those tests in lexer with generated lexers (lexer_simple, build_lexer_tables)");
}

#[test]
fn lexer_simple() {
    fn eval(result: &Result<Option<(TokenId, ChannelId, String, PosSpan)>, LexerError>, verbose: bool)
        -> Option<(TokenId, ChannelId, String, PosSpan)>
    {
        match result {
            Ok(Some(token_ch)) => {
                if verbose { println!("=> OK {}, #{}", token_ch.0, token_ch.1); }
                Some(token_ch.clone())
            }
            Ok(None) => None,
            Err(e) => {
                if verbose { println!("## Error: {e}"); }
                None
                // if e.token_ch.is_some() {
                //     if verbose { println!(" => OK"); }
                //     panic!();
                //     // e.token_ch.clone()
                // } else {
                //     if verbose { println!(" => Error"); }
                //     None
                // }
            }
        }
    }

    let tests = vec![
        // [ \t\n\r]*[0-9]+(<end:0>|'.'[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
        (10, btreemap![
            0 => (vec!["0", "0 ", "10", "9876543210", " 0"], vec!["0", "0", "10", "9876543210", " 0"]),
            1 => (vec!["0.5", "0.1 ", "9876543210.0123456789"], vec!["0.5", "0.1", "9876543210.0123456789"]),
            2 => (vec!["0x0", "0xF ", "0x0123456789abcdef", "0x0123456789ABCDEF", "0xff"], vec!["0x0", "0xF", "0x0123456789abcdef", "0x0123456789ABCDEF", "0xff"]),
         ],
         vec![("a", Some(0), false), (".5", Some(0), false), ("()", Some(0), false), ("0xy", Some(2), false),
              ("0.a", Some(2), false), ("", Some(0), true), (" ", Some(1), true)],
         vec![
             // note: col values are set on the first space character [ \t\n\t] since we don't skip them in another channel
             ("0 1 2 ", vec![(0, 1, 1), (0, 1, 2), (0, 1, 4)]),
             ("0x1 0.5 15", vec![(2, 1, 1), (1, 1, 4), (0, 1, 8)]),
             //        12-90-789
             ("1\n2 3\n4\t5\t6 7", vec![(0, 1, 1), (0, 1, 2), (0, 2, 2), (0, 2, 4), (0, 3, 2), (0, 3, 10), (0, 3, 18)]),
         ],
        ),

        // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|'+'<end:4>|;<end:5>)
        (11, btreemap![
            0 => (vec!["a", "id", "id05b", "ifa", "printa", "\t\nx "], vec!["a", "id", "id05b", "ifa", "printa", "\t\nx"]),
            1 => (vec!["if", "if x"], vec!["if", "if"]),
            2 => (vec!["print", "print "], vec!["print", "print"]),
            3 => (vec!["=", "=5", "=a"], vec!["=", "=", "="]),
            4 => (vec!["+", "+5", "+a"], vec!["+", "+", "+"]),
            5 => (vec![";", ";a"], vec![";", ";"]),
         ],
         vec![("0", Some(0), false), ("", Some(0), true), ("-", Some(0), false), ("*", Some(0), false)],
         //     1-9012345678901234567890
         vec![("\ta = x; if i=j print b;\n", vec![(0, 1, 1), (3, 1, 10), (0, 1, 12), (5, 1, 14), (1, 1, 15), (0, 1, 18), (3, 1, 20), (0, 1, 21),
                                                  (2, 1, 22), (0, 1, 28), (5, 1, 30)])]
        ),
    ];
    const VERBOSE: bool = false;
    for (test_id, token_tests, err_tests, stream_tests) in tests {
        let dfa = Dfa::<General>::build_from(DfaBuilder::build_from(build_re(test_id))).normalize();
        if VERBOSE { dfa.print(12); }
        let lexgen = LexerGen::build_from(dfa);
        if VERBOSE { lexgen.write_source_code(None, 0).expect("Couldn't output the source code"); }
        let lexer_tables = LexerTables::build_from(lexgen);
        let mut lexer = lexer_tables.make_lexer();
        lexer.set_tab_width(8);
        for (exp_token, (inputs, outputs)) in token_tests {
            for (input, output) in inputs.into_iter().zip(outputs.into_iter()) {
                if VERBOSE { println!("\"{}\": (should succeed)", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let result = lexer.get_token();
                let token = eval(&result, VERBOSE).map(|(id, ch, str, span)| (id, ch, str, span.first_forced().0, span.first_forced().1));
                assert_eq!(token, Some((exp_token, 0, output.to_string(), 1, 1)), "test {} failed for input '{}'", test_id, escape_string(input));
                lexer.detach_stream();
            }
        }
        for (input, expected_pos, expected_eos) in err_tests {
            if VERBOSE { println!("\"{}\": (should fail)", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let result = lexer.get_token();
            let token = eval(&result, VERBOSE).map(|(id, ch, str, span)| (id, ch, str, span.first_forced().0, span.first_forced().1));
            assert_eq!(token, None, "test {} failed for input '{}'", test_id, escape_string(input));
            if let Err(e) = result {
                assert_eq!(e.get_pos(), expected_pos, "test {} failed for input '{}', wrong pos, {:?}", test_id, escape_string(input), e);
                assert_eq!(lexer.is_eos(), expected_eos, "test {} failed for input '{}', wrong eos flag, {:?}", test_id, escape_string(input), e);
            }
        }
        // gathering all the tokens, one at a time:
        for (input, expected_tokens) in stream_tests.clone() {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let mut result = Vec::new();
            while lexer.is_open() {
                let token = &lexer.get_token();
                let t = eval(token, false);
                if let Some((tok, ch, _text, span)) = t {
                    if VERBOSE { print!(" {}, #{}", tok, ch); }
                    let start = span.first_forced();
                    result.push((tok, start.line(), start.col()));
                    assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
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
            lexer.attach_stream(stream);
            let result = lexer.tokens().map(|(tok, ch, _text, span)| {
                if VERBOSE { print!(" {}, #{}", tok, ch); }
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                let start = span.first_forced();
                (tok, start.line(), start.col())
            }).to_vec();
            if VERBOSE { println!(); }
            assert_eq!(result, expected_tokens, "test {} failed for input '{}'", test_id, escape_string(input));
            assert!(!lexer.has_error() || lexer.is_eos(), "test {} failed for input '{}'",
                    test_id, escape_string(input));
            // or:
            // assert!(!matches!(interpret.get_error(), Some(LexerError { is_eos: false, .. })), "test {} failed for input '{}'",
            //         test_id, escape_string(input));
        }
    }
}

fn build_lexer_tables(test: usize) -> LexerTables {
    let mut re = VecTree::<ReNode>::new();
    let trees = match test {
        1 => {
            // mode 0: ([ \t\n\r]+<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add_root(node!(|));
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

            // mode 1: ('*/'<pop>|[' ', '\t', '\n', '\r', 'a'-'z']+<skip>)
            let mut re1 = VecTree::new();
            let or = re1.add_root(node!(|));
            let cc1 = re1.add(Some(or), node!(&));
            re1.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(pop))]);
            let cc2 = re1.add(Some(or), node!(&));
            let s2 = re1.add(Some(cc2), node!(+));
            re1.add(Some(s2), node!([' ', '\t', '\n', '\r', 'a'-'z']));
            re1.add(Some(cc2), node!(term!(skip)));

            btreemap![0 => re, 1 => re1]
        },
        2 => {
            // mode 0: ([ \t\n\r]+<skip>|'/*'<push(1)>|[0-9]+<end:0>)
            let or = re.add_root(node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let p0 = re.add(Some(cc0), node!(+));
            let or0 = re.add(Some(p0), node!(|));
            re.add(Some(or0), node!([' ', '\t', '\n', '\r']));
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add(Some(or2), node!(['0'-'9']));
            re.add(Some(cc2), node!(=0));

            // mode 1: .*?'*/'<pop>
            let mut re1 = VecTree::new();
            let cc = re1.add_root(node!(&));
            let l0 = re1.add(Some(cc), node!(??));
            let s2 = re1.add(Some(l0), node!(*));
            re1.add(Some(s2), node!([DOT]));
            re1.add_iter(Some(cc), [node!(str "*/"), node!(term!(pop))]);

            btreemap![0 => re, 1 => re1]
        },
        3 => {
            // mode 0: '\''<more,push(1)>|'.'<end:0>
            let or = re.add_root(node!(|));
            re.addc_iter(Some(or), node!(&), [node!(chr '\''), node!(term!(push 1) + term!(more))]);
            re.addc_iter(Some(or), node!(&), [node!(chr '.'), node!(term!(= 0))]);

            // mode 1: '\''<end:1,pop>|[' ', 'a'-'z']<more>
            let mut re1 = VecTree::new();
            let or = re1.add_root(node!(|));
            re1.addc_iter(Some(or), node!(&), [node!(chr '\''), node!(term!(= 1) + term!(pop))]);
            re1.addc_iter(Some(or), node!(&), [node!([' ', 'a'-'z']), node!(term!(more))]);

            btreemap![0 => re, 1 => re1]
        }
     _ => btreemap![],
    };
    const VERBOSE: bool = false;
    let dfas = trees.into_iter().enumerate().map(|(dfa_id, (mode, re))| {
        if VERBOSE { println!("creating dfa for mode {mode}"); }
        // let dfa = DfaBuilder::from_re(re).build();
        let dfa_builder = DfaBuilder::build_from(re);
        let dfa = Dfa::<General>::build_from(dfa_builder);
        let log = dfa.get_log();
        assert!(log.has_no_errors() && log.has_no_warnings(),
                "warnings/errors when building lexer #{test}: (DFA #{dfa_id})\n{log}");
        if VERBOSE {
            dfa.print(4);
        }
        (mode as u16, dfa)
    }).to_vec();
    if VERBOSE { println!("merging dfa modes"); }
    let dfa = Dfa::<General>::build_from(DfaBundle::new(dfas));
    assert!(dfa.get_log().has_no_errors() && dfa.get_log().has_no_warnings(),
            "warnings/errors when building lexer #{test} (merging DFAs):\n{}", dfa.get_log().get_messages_str());
    if VERBOSE {
        dfa.print(4);
        println!("normalizing");
    }
    let dfa = dfa.normalize();
    if VERBOSE {
        dfa.print(4);
        println!("creating lexer");
    }
    let lexgen = LexerGen::build_from(dfa);
    if VERBOSE {
        lexgen.write_source_code(None, 0).expect("Couldn't output the source code");
        println!("creating lexer");
    }
    match lexgen.try_build_into() {
        Ok(tables) => tables,
        Err(build_error) => panic!("{build_error}"),
    }
}

#[test]
fn lexer_modes() {
    let tests = vec![
        (1, vec![
            // no error
            (" 10 20 30", vec![(0, 1, 2, 3), (0, 1, 5, 6), (0, 1, 8, 9)], vec!["10", "20", "30"]),
            (" 10 20 ", vec![(0, 1, 2, 3), (0, 1, 5, 6)], vec!["10", "20"]),
            (" 5 /* bla bla */ 6\n \n 75", vec![(0, 1, 2, 2), (0, 1, 18, 18), (0, 3, 2, 3)], vec!["5", "6", "75"]),
        ]),
        (2, vec![
            // no error
            (" 10 20 30", vec![(0, 1, 2, 3), (0, 1, 5, 6), (0, 1, 8, 9)], vec!["10", "20", "30"]),
            (" 10 20 ", vec![(0, 1, 2, 3), (0, 1, 5, 6)], vec!["10", "20"]),
            (" 5 /* bla bla */ 6\n \n 75", vec![(0, 1, 2, 2), (0, 1, 18, 18), (0, 3, 2, 3)], vec!["5", "6", "75"]),
        ]),
        (3, vec![
            ("'hello'.", vec![(1, 1, 1, 7), (0, 1, 8, 8)], vec!["'hello'", "."]),
            (".", vec![(0, 1, 1, 1)], vec!["."]),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (lexer_id, data)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("test {test_id}:"); }
        let lexer_tables = build_lexer_tables(lexer_id);
        let mut lexer = lexer_tables.make_lexer();
        for (input, expected_tokens, expected_texts) in data {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let (tokens, texts): (Vec<(TokenId, i32, i32, i32)>, Vec<String>) = lexer.tokens().map(|(tok, ch, text, span)| {
                if VERBOSE { print!(" ({tok}, \"{text}\", {span})") }
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                let (start, end) = (span.first_forced(), span.last_forced());
                ((tok, start.line() as i32, start.col() as i32, end.col() as i32), text)
            }).unzip();
            if VERBOSE { println!(); }
            assert_eq!(tokens, expected_tokens, "test {} failed for input '{}'", test_id, escape_string(input));
            assert_eq!(texts, expected_texts, "test {} failed for input '{}'", test_id, escape_string(input));
            assert!(!lexer.has_error() || lexer.is_eos(), "test {} failed for input '{}'",
                    test_id, escape_string(input));

        }
        if VERBOSE { println!("--------------------------------------\n"); }
    }
}
