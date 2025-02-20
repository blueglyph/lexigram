#![cfg(test)]

use std::io::Cursor;
use crate::{btreemap, CollectJoin, escape_string, node, term};
use crate::dfa::*;
use crate::segments::*;
use crate::dfa::tests::{build_re, print_dfa};
use crate::lexergen::LexerGen;
use vectree::VecTree;
use super::*;

#[test]
fn lexer_simple() {
    fn eval(result: &Result<(TokenId, ChannelId, String, CaretCol, CaretLine), LexerError>, verbose: bool)
        -> Option<(TokenId, ChannelId, String, CaretCol, CaretLine)>
    {
        match result {
            Ok(token_ch) => {
                if verbose { println!("=> OK {}, #{}", token_ch.0, token_ch.1); }
                Some(token_ch.clone())
            }
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
         vec![("a", 0, false), (".5", 0, false), ("()", 0, false), ("0xy", 2, false), ("0.a", 2, false), ("", 0, true), (" ", 1, true)],
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
         vec![("0", 0, false), ("", 0, true), ("-", 0, false), ("*", 0, false)],
         //     1-9012345678901234567890
         vec![("\ta = x; if i=j print b;\n", vec![(0, 1, 1), (3, 1, 10), (0, 1, 12), (5, 1, 14), (1, 1, 15), (0, 1, 18), (3, 1, 20), (0, 1, 21),
                                                  (2, 1, 22), (0, 1, 28), (5, 1, 30)])]
        ),
    ];
    const VERBOSE: bool = false;
    for (test_id, token_tests, err_tests, stream_tests) in tests {
        let dfa = DfaBuilder::from_re(build_re(test_id)).build();
        let dfa = dfa.normalize();
        if VERBOSE { print_dfa(&dfa); }
        let lexgen = LexerGen::from_dfa(&dfa);
        if VERBOSE { lexgen.write_source_code(None, 0).expect("Couldn't output the source code"); }
        let mut lexer = lexgen.make_lexer();
        for (exp_token, (inputs, outputs)) in token_tests {
            for (input, output) in inputs.into_iter().zip(outputs.into_iter()) {
                if VERBOSE { println!("\"{}\": (should succeed)", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let result = lexer.get_token();
                let token = eval(&result, VERBOSE);
                assert_eq!(token, Some((exp_token, 0, output.to_string(), 1, 1)), "test {} failed for input '{}'", test_id, escape_string(input));
                lexer.detach_stream();
            }
        }
        for (input, expected_pos, expected_eos) in err_tests {
            if VERBOSE { println!("\"{}\": (should fail)", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let result = lexer.get_token();
            let token = eval(&result, VERBOSE);
            assert_eq!(token, None, "test {} failed for input '{}'", test_id, escape_string(input));
            if let Err(e) = result {
                assert_eq!(lexer.pos, expected_pos, "test {} failed for input '{}', {:?}", test_id, escape_string(input), e);
                assert_eq!(lexer.is_eos(), expected_eos, "test {} failed for input '{}', {:?}", test_id, escape_string(input), e);
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
                if let Some((tok, ch, _text, line, col)) = t {
                    if VERBOSE { print!(" {}, #{}", tok, ch); }
                    result.push((tok, line, col));
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
            let result = lexer.tokens().map(|(tok, ch, _text, line, col)| {
                if VERBOSE { print!(" {}, #{}", tok, ch); }
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                (tok, line, col)
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

fn build_lexer<R: Read>(test: usize) -> Lexer<R> {
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
        let mut dfa_builder = DfaBuilder::from_re(re);
        let dfa = dfa_builder.build();
        assert!(dfa_builder.get_messages().is_empty(), "warnings/errors when building lexer #{test}: (DFA #{dfa_id})\n{}", dfa_builder.get_messages());
        if VERBOSE {
            print_dfa(&dfa);
        }
        (mode as u16, dfa)
    }).to_vec();
    let mut dfa_builder = DfaBuilder::new();
    if VERBOSE { println!("merging dfa modes"); }
    let dfa = dfa_builder.build_from_dfa_modes(dfas).expect(&format!("failed to build lexer #{test}\n{}", dfa_builder.get_messages()));
    assert!(dfa_builder.get_messages().is_empty(), "warnings/errors when building lexer #{test} (merging DFAs):\n{}", dfa_builder.get_messages());
    if VERBOSE {
        print_dfa(&dfa);
        println!("normalizing");
    }
    let dfa = dfa.normalize();
    if VERBOSE {
        print_dfa(&dfa);
        println!("creating lexer");
    }
    let mut lexgen = LexerGen::new();
    lexgen.build_tables(&dfa);
    if VERBOSE {
        lexgen.write_source_code(None, 0).expect("Couldn't output the source code");
        println!("creating lexer");
    }
    lexgen.make_lexer()
}

#[test]
fn lexer_modes() {
    let tests = vec![
        (1, vec![
            // no error
            (" 10 20 30", vec![(0, 1, 2), (0, 1, 5), (0, 1, 8)], vec!["10", "20", "30"]),
            (" 10 20 ", vec![(0, 1, 2), (0, 1, 5)], vec!["10", "20"]),
            (" 5 /* bla bla */ 6\n \n 7", vec![(0, 1, 2), (0, 1, 18), (0, 3, 2)], vec!["5", "6", "7"]),
        ]),
        (2, vec![
            // no error
            (" 10 20 30", vec![(0, 1, 2), (0, 1, 5), (0, 1, 8)], vec!["10", "20", "30"]),
            (" 10 20 ", vec![(0, 1, 2), (0, 1, 5)], vec!["10", "20"]),
            (" 5 /* bla bla */ 6\n \n 7", vec![(0, 1, 2), (0, 1, 18), (0, 3, 2)], vec!["5", "6", "7"]),
        ]),
        (3, vec![
            ("'hello'.", vec![(1, 1, 1), (0, 1, 8)], vec!["'hello'", "."]),
            (".", vec![(0, 1, 1)], vec!["."]),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (lexer_id, data)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("test {test_id}:"); }
        let mut lexer = build_lexer(lexer_id);
        for (input, expected_tokens, expected_texts) in data {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let (tokens, texts): (Vec<(TokenId, i32, i32)>, Vec<String>) = lexer.tokens().map(|(tok, ch, text, line, col)| {
                if VERBOSE { print!(" ({tok}, \"{text}\")") }
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                ((tok, line as i32, col as i32), text, )
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

// tests files generated by print_source_code(&lexergen)
mod lexer_source1 {
    use std::collections::HashMap;
    use std::io::{Cursor, Read};
    use crate::dfa::{ModeOption, StateId, Terminal, TokenId};
    use crate::escape_string;
    use crate::io::CharReader;
    use crate::lexergen::GroupId;
    use crate::segments::{Seg, SegMap};
    use crate::lexer::Lexer;

    // -------------------------------------------------------------------------
    // Copied from a print_source_code(&lexergen)

    const NBR_GROUPS: u32 = 5;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 4;
    const NBR_STATES: StateId = 9;
    const ASCII_TO_GROUP: [GroupId; 128] = [
          5,   5,   5,   5,   5,   5,   5,   5,   5,   0,   0,   5,   5,   0,   5,   5,   // 0-15
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   // 16-31
          0,   5,   5,   5,   5,   5,   5,   5,   5,   5,   3,   5,   5,   5,   5,   1,   // 32-47
          2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   5,   5,   5,   5,   5,   5,   // 48-63
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   // 64-79
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   // 80-95
          5,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   // 96-111
          4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   5,   5,   5,   5,   5,   // 112-127
    ];
    const UTF8_TO_GROUP: [(char, GroupId); 0] = [];
    // const UTF8_TO_GROUP: [(char, GroupId); 3] = [('Δ', 4),('∑', 5),('𝆕', 2),];
    const SEG_TO_GROUP: [(Seg, GroupId); 0] = [];
    // const SEG_TO_GROUP: [(Seg, GroupId); 8] = [
    //     (Seg(256, 915), 6),
    //     (Seg(916, 916), 4),
    //     (Seg(917, 8720), 6),
    //     (Seg(8721, 8721), 5),
    //     (Seg(8722, 55295), 6),
    //     (Seg(57344, 119188), 6),
    //     (Seg(119189, 119189), 2),
    //     (Seg(119190, 1114111), 6),];
    const TERMINAL_TABLE: [Terminal;5] = [
        Terminal { action: crate::dfa::TermAction::Skip,     channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: crate::dfa::TermAction::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: crate::dfa::TermAction::Skip,     channel: 0, mode: ModeOption::Push(1), mode_state: Some(2), pop: false },
        Terminal { action: crate::dfa::TermAction::Skip,     channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: crate::dfa::TermAction::Skip,     channel: 0, mode: ModeOption::None, mode_state: None, pop: true }];
    const STATE_TABLE: [StateId; 45] = [
          4,   1,   5,   9,   9, // state 0
          9,   9,   9,   6,   9, // state 1
          7,   9,   9,   3,   7, // state 2
          9,   8,   9,   9,   9, // state 3
          4,   9,   9,   9,   9, // state 4 <skip>
          9,   9,   5,   9,   9, // state 5 <end:0>
          9,   9,   9,   9,   9, // state 6 <skip,push(mode 1,state 2)>
          7,   9,   9,   9,   7, // state 7 <skip>
          9,   9,   9,   9,   9, // state 8 <skip,pop>
    ];

    fn build_lexer<R: Read>() -> Lexer<R> {
        Lexer::new(
            // parameters
            NBR_GROUPS,
            INITIAL_STATE,
            FIRST_END_STATE,
            NBR_STATES,
            // tables
            Box::new(ASCII_TO_GROUP),
            HashMap::<char, GroupId>::from(UTF8_TO_GROUP),
            SegMap::<GroupId>::from_iter(SEG_TO_GROUP),
            Box::new(STATE_TABLE),
            Box::new(TERMINAL_TABLE)
        )
    }

    // -------------------------------------------------------------------------

    #[test]
    fn lexer_modes() {
        let tests = vec![
            (1, vec![
                // no error
                (" 10 20 30", vec![0, 0, 0], vec!["10", "20", "30"]),
                (" 10 20 ", vec![0, 0], vec!["10", "20"]),
                (" 5 /* bla bla */ 6", vec![0, 0], vec!["5", "6"]),
            ]),
        ];
        const VERBOSE: bool = false;
        for (test_id, inputs) in tests.into_iter() {
            if VERBOSE { println!("test {test_id}:"); }
            let mut lexer = build_lexer();
            for (input, expected_tokens, expected_texts) in inputs {
                if VERBOSE { print!("\"{}\":", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text, _col, _line)| {
                    if VERBOSE { print!(" ({tok}, \"{text}\")")}
                    assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                    (tok, text)
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

}