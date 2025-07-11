// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

const TXT1: &str = r#"
    lexicon A;
    fragment ALPHANUM: [0-9A-Za-z_];
    fragment N_ALPHANUM: ~ALPHANUM;
    A: 'a:' ALPHANUM+;
    B: 'b:' (~N_ALPHANUM)+;
    C: 'c:' ([0-9] | [ABC-Z] | 'a'..'z' | '_')+;
    D: 'd:' \w+;
    E: 'e:' [\w]+;
    F: 'f' ':' (\d | [a-zA-Z_])+;
    G: 'g:' ([\d] | [A-Za-z_])+;
    H: 'h:' .;
    I: 'i:' [a-z] [0-9]*? ('0' | '.');
    J: 'j:' [a-z] [0-9]*  ('0' | '.');
    K: 'k:' [a-z] [0-9]+? ('0' | '.');
    L: 'l:' [a-z] [0-9]+  ('0' | '.');
    M: 'm:' [a-z] [0-9]?;
    WHITESPACE: [ \n\r\t]+ -> skip;
"#;

const TXT2: &str = r#"
    lexicon B;
    channels { REF }

    OPEN_TAG: '<' -> push(TAG_MODE), skip;
    OPEN_SPEC: '<?' -> push(SPEC_MODE), skip;
    OPEN_ARRAY: '[' -> mode(ARRAY_MODE), more;

    mode TAG_MODE;
    CLOSE_TAG: '>' -> pop, skip;
    SEP_TAG: [ \t,] -> skip;
    TAG: \w+;
    REF_TAG: '@' \w+ -> channel(REF);

    mode SPEC_MODE;
    CLOSE_SPEC: '?>' -> pop, skip;
    SEP_SPEC: [ \t,] -> skip;
    SPEC: \w+;

    mode ARRAY_MODE;
    CLOSE_ARRAY: ']' -> mode(DEFAULT_MODE), type(ARRAY);
    NUM_ARRAY: \d+ | ',' -> more;
    SEP_ARRAY: [ \t\n] -> skip;
"#;

const TXT3: &str = r#"
    lexicon C;
    ID1: [a-z]+ -> type(ID);
    A:   'a';
    ID:  [A-Z]+;
    B:   'b';
"#;

const TXT4: &str = r#"
    lexicon D;
    A: 'a' -> type(D);
    B: 'b';
    C: 'c' -> skip;
    D: 'd';
    E: 'e' -> type(E);
    F: 'f';
"#;

const TXT5: &str = r#"
    lexicon E;
    fragment X1: 'abc';
    R: 'a' -> type(P);
    Q: 'b' -> skip;
    P: 'c' -> type(R);
    O: 'd' -> skip;
    N: 'e' -> type(O);
    M: 'f' -> type(O);
    L: 'g' -> type(M);
    K: 'h';
    J: 'i' -> mode(ONE), skip;
    I: 'j' -> type(E);
    fragment X0: 'def';
    H: 'k';

    mode ONE;
    fragment Y0: 'ghi';
    G: 'l';
    F: 'm' -> type(I);
    E: 'n' -> type(H);
    D: 'o' -> type(C);
    C: 'p' -> type(A);
    fragment Y1: 'jkl';
    B: 'q' -> mode(DEFAULT_MODE);
    A: 'r' -> skip;
"#;

mod listener {
    use crate::listener::decode_str;

    #[test]
    fn decode_str_is_ok() {
        let tests = vec![
            ("abc", (true, "abc")),
            ("\\n\\r\\t\\'\\\\", (true, "\n\r\t'\\")),
            ("012顠abc©345𠃐ab", (true, "012顠abc©345𠃐ab")),
            ("\\u{20}\\u{00A9}", (true, " ©")),
            ("\\", (false, "'\\' incomplete escape code in string literal '\\'")),
            ("\\a", (false, "unknown escape code '\\a' in string literal '\\a'")),
            ("\\u", (false, "malformed unicode literal in string literal '\\u' (missing '{')")),
            ("\\u{20", (false, "malformed unicode literal in string literal '\\u{20' (missing '}')")),
            ("\\u{2x0}", (false, "'2x0' isn't a valid hexadecimal value")),
            ("\\u{d800}", (false, "'d800' isn't a valid unicode hexadecimal value")),
        ];
        for (input, (ok, s)) in tests {
            let result = match decode_str(input) {
                Ok(s) => (true, s),
                Err(s) => (false, s)
            };
            assert_eq!(result, (ok, s.to_string()), "failed for input '{input}'");
        }
    }
}

mod simple {
    use std::collections::BTreeMap;
    use std::hint::black_box;
    use std::io::Cursor;
    use lexigram::{branch, btreemap, term};
    use lexigram::dfa::{print_dfa, tree_to_string, ActionOption, ReType};
    use lexigram::io::CharReader;
    use lexigram::lexer::LexerError;
    use lexigram::lexergen::LexerGen;
    use lexigram::CollectJoin;
    use crate::lexi::Lexi;
    use crate::listener::RuleType;
    use super::*;

    #[test]
    fn lexiparser() {
        let tests = vec![
            (
                TXT1,
                btreemap![
                    0 => branch!('\t'-'\n', '\r', ' ' => 33, 'a' => 1, 'b' => 17, 'c' => 18, 'd' => 19, 'e' => 20, 'f' => 21, 'g' => 22, 'h' => 23, 'i' => 24, 'j' => 29, 'k' => 30, 'l' => 32, 'm' => 25),
                    1 => branch!(':' => 2),
                    2 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 34),
                    3 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 35),
                    4 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 36),
                    5 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 37),
                    6 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 38),
                    7 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 39),
                    8 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 40),
                    9 => branch!(DOT => 41),
                    10 => branch!('a'-'z' => 11),
                    11 => branch!('.' => 42, '0' => 43, '1'-'9' => 11),
                    12 => branch!('.' => 44, '0' => 45, '1'-'9' => 12),
                    13 => branch!('0'-'9' => 14),
                    14 => branch!('.' => 46, '0' => 47, '1'-'9' => 14),
                    15 => branch!('.' => 48, '0' => 49, '1'-'9' => 15),
                    16 => branch!('a'-'z' => 50),
                    17 => branch!(':' => 3),
                    18 => branch!(':' => 4),
                    19 => branch!(':' => 5),
                    20 => branch!(':' => 6),
                    21 => branch!(':' => 7),
                    22 => branch!(':' => 8),
                    23 => branch!(':' => 9),
                    24 => branch!(':' => 10),
                    25 => branch!(':' => 16),
                    26 => branch!('a'-'z' => 12),
                    27 => branch!('a'-'z' => 13),
                    28 => branch!('0'-'9' => 15),
                    29 => branch!(':' => 26),
                    30 => branch!(':' => 27),
                    31 => branch!('a'-'z' => 28),
                    32 => branch!(':' => 31),
                    33 => branch!('\t'-'\n', '\r', ' ' => 33), // <skip>
                    34 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 34), // <end:0>
                    35 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 35), // <end:1>
                    36 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 36), // <end:2>
                    37 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 37), // <end:3>
                    38 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 38), // <end:4>
                    39 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 39), // <end:5>
                    40 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 40), // <end:6>
                    41 => branch!(), // <end:7>
                    42 => branch!(), // <end:8>
                    43 => branch!(), // <end:8>
                    44 => branch!(), // <end:9>
                    45 => branch!('.' => 44, '0' => 45, '1'-'9' => 12), // <end:9>
                    46 => branch!(), // <end:10>
                    47 => branch!(), // <end:10>
                    48 => branch!(), // <end:11>
                    49 => branch!('.' => 48, '0' => 49, '1'-'9' => 15), // <end:11>
                    50 => branch!('0'-'9' => 51), // <end:12>
                    51 => branch!(), // <end:12>
                ], btreemap![
                    33 => term!(skip), 34 => term!(=0), 35 => term!(=1), 36 => term!(=2), 37 => term!(=3), 38 => term!(=4), 39 => term!(=5),
                    40 => term!(=6), 41 => term!(=7), 42 => term!(=8), 43 => term!(=8), 44 => term!(=9), 45 => term!(=9), 46 => term!(=10),
                    47 => term!(=10), 48 => term!(=11), 49 => term!(=11), 50 => term!(=12), 51 => term!(=12)
                ], vec![
                (
                    "a:aA_0 \t\nb:bB_1 c:cC_2 d:dD_3 e:eE_4 f:fF_5 g:gG_6 h:α i:i123. i:i120 j:j123. j:j1200 j:j120 k:k123. k:k120 l:l123. l:l1200 l:l120. m:m1",
                    vec![
                        (0, 0, "a:aA_0"), (0, 1, "b:bB_1"), (0, 2, "c:cC_2"), (0, 3, "d:dD_3"), (0, 4, "e:eE_4"), (0, 5, "f:fF_5"), (0, 6, "g:gG_6"), (0, 7, "h:α"),
                        (0, 8, "i:i123."), (0, 8, "i:i120"), (0, 9, "j:j123."), (0, 9, "j:j1200"), (0, 9, "j:j120"), (0, 10, "k:k123."), (0, 10, "k:k120"),
                        (0, 11, "l:l123."), (0, 11, "l:l1200"), (0, 11, "l:l120."), (0, 12, "m:m1")
                    ]
                )
            ]
            ),
            (
                TXT2,
                btreemap![
                    0 => branch!('<' => 6, '[' => 7),
                    1 => branch!('\t', ' ', ',' => 9, '0'-'9', 'A'-'Z', '_', 'a'-'z' => 10, '>' => 11, '@' => 2),
                    2 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 12),
                    3 => branch!('\t', ' ', ',' => 13, '0'-'9', 'A'-'Z', '_', 'a'-'z' => 14, '?' => 4),
                    4 => branch!('>' => 15),
                    5 => branch!('\t'-'\n', ' ' => 16, ',' => 17, '0'-'9' => 18, ']' => 19),
                    6 => branch!('?' => 8), // <skip,push(1,state 1)>
                    7 => branch!(), // <more,mode(3,state 5)>
                    8 => branch!(), // <skip,push(2,state 3)>
                    9 => branch!(), // <skip>
                    10 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 10), // <end:5>
                    11 => branch!(), // <skip,pop>
                    12 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 12), // <end:6,ch 1>
                    13 => branch!(), // <skip>
                    14 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 14), // <end:9>
                    15 => branch!(), // <skip,pop>
                    16 => branch!(), // <skip>
                    17 => branch!(), // <more>
                    18 => branch!('0'-'9' => 18), // <more>
                    19 => branch!(), // <end:10,mode(0,state 0)>
                ],
                btreemap![
                    6 => term!(skip) + term!(push 1) + term!(pushst 1), 7 => term!(more) + term!(mode 3) + term!(pushst 5),
                    8 => term!(skip) + term!(push 2) + term!(pushst 3), 9 => term!(skip), 10 => term!(=0), 11 => term!(skip) + term!(pop),
                    12 => term!(=1) + term!(#1), 13 => term!(skip), 14 => term!(=2), 15 => term!(skip) + term!(pop), 16 => term!(skip),
                    17 => term!(more), 18 => term!(more), 19 => term!(=3) + term!(mode 0) + term!(pushst 0)
               ], vec![
                (
                    "<a,A,\t_, 0><b><><?c,C,\t_, 1?><?d?><??>[0,\t1, 2][3][]",
                    vec![
                        (0, 0, "a"), (0, 0, "A"), (0, 0, "_"), (0, 0, "0"), (0, 0, "b"), (0, 2, "c"), (0, 2, "C"),
                        (0, 2, "_"), (0, 2, "1"), (0, 2, "d"), (0, 3, "[0,1,2]"), (0, 3, "[3]"), (0, 3, "[]")
                    ]
                ),
                (
                    "<a@1,b@my_b_1>",
                    vec![(0, 0, "a"), (1, 1, "@1"), (0, 0, "b"), (1, 1, "@my_b_1")]
                )
                ]
            ),
            (
                TXT3, btreemap![
                    0 => branch!('A'-'Z' => 1, 'a' => 2, 'b' => 3, 'c'-'z' => 4),
                    1 => branch!('A'-'Z' => 1), // <end:1>
                    2 => branch!('a'-'z' => 4), // <end:0>
                    3 => branch!('a'-'z' => 4), // <end:2>
                    4 => branch!('a'-'z' => 4), // <end:1>
                ], btreemap![
                    1 => term!(=1), 2 => term!(=0), 3 => term!(=2), 4 => term!(=1)
                ], vec![]
            ),
            (
                TXT4, btreemap![
                    0 => branch!('a' => 1, 'b' => 2, 'c' => 3, 'd' => 4, 'e' => 5, 'f' => 6),
                    1 => branch!(), // <end:1>
                    2 => branch!(), // <end:0>
                    3 => branch!(), // <skip>
                    4 => branch!(), // <end:1>
                    5 => branch!(), // <end:2>
                    6 => branch!(), // <end:3>
                ], btreemap![
                    1 => term!(=1), 2 => term!(=0), 3 => term!(skip), 4 => term!(=1), 5 => term!(=2), 6 => term!(=3)
                ], vec![]
            ),
            (
                TXT5, btreemap![
                    0 => branch!('a' => 2, 'b' => 3, 'c' => 4, 'd' => 5, 'e' => 6, 'f' => 7, 'g' => 8, 'h' => 9, 'i' => 10, 'j' => 11, 'k' => 12),
                    1 => branch!('l' => 13, 'm' => 14, 'n' => 15, 'o' => 16, 'p' => 17, 'q' => 18, 'r' => 19),
                    2 => branch!(), // <end:1>
                    3 => branch!(), // <skip>
                    4 => branch!(), // <end:0>
                    5 => branch!(), // <skip>
                    6 => branch!(), // <end:2>
                    7 => branch!(), // <end:2>
                    8 => branch!(), // <end:3>
                    9 => branch!(), // <end:4>
                    10 => branch!(), // <skip,mode(1,state 1)>
                    11 => branch!(), // <end:8>
                    12 => branch!(), // <end:6>
                    13 => branch!(), // <end:7>
                    14 => branch!(), // <end:5>
                    15 => branch!(), // <end:6>
                    16 => branch!(), // <end:9>
                    17 => branch!(), // <end:11>
                    18 => branch!(), // <end:10,mode(0,state 0)>
                    19 => branch!(), // <skip>
                ], btreemap![
                    2 => term!(=1), 3 => term!(skip), 4 => term!(=0), 5 => term!(skip), 6 => term!(=2), 7 => term!(=2), 8 => term!(=3), 9 => term!(=4), 10 => term!(skip) + term!(mode 1) + term!(pushst 1), 11 => term!(=8), 12 => term!(=6), 13 => term!(=7), 14 => term!(=5), 15 => term!(=6), 16 => term!(=9), 17 => term!(=11), 18 => term!(=10) + term!(mode 0) + term!(pushst 0), 19 => term!(skip)
                ], vec![]
            ),
        ];
        const VERBOSE: bool = false;

        for (test_id, (input, expected_graph, expected_end_states, test_strs)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}", ""); }
            let stream = CharReader::new(Cursor::new(input));
            let mut lexi = Lexi::new();
            let result = lexi.build(stream);
            let mut listener = lexi.wrapper.listener();
            if VERBOSE {
                let msg = listener.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let text = format!("test {test_id} failed");
            assert_eq!(result, Ok(()), "{text}");
            if VERBOSE {
                println!("Rules:");
                let mut rules = listener.rules.iter().to_vec();
                rules.sort_by(|a, b| (&a.1, &a.0).cmp(&(&b.1, &b.0)));
                // rules.sort_by_key(|(s, rt)| (rt, s));
                for (i, (s, rt)) in rules.into_iter().enumerate() {
                    let (t, lit) = match rt {
                        RuleType::Fragment(id) => (listener.fragments.get(*id as usize).unwrap(), listener.fragment_literals.get(*id as usize).unwrap()),
                        RuleType::Terminal(id) => (listener.terminals.get(*id as usize).unwrap(), listener.terminal_literals.get(*id as usize).unwrap()),
                    };
                    println!("- [{i:3}] {rt:?} {s}: {}    {lit:?}", tree_to_string(t, None, true));
                }
            }
            let dfa = listener.make_dfa().optimize();
            if VERBOSE {
                println!("Final optimized Dfa:");
                print_dfa(&dfa, 20);
            }
            assert_eq!(dfa.get_state_graph(), &expected_graph, "{text}");
            assert_eq!(dfa.get_end_states(), &expected_end_states, "{text}");

            let lexer_tables = LexerGen::from(dfa).make_lexer_tables();
            let mut lexer = lexer_tables.make_lexer();
            for (input_id, (input, expected_tokens)) in test_strs.into_iter().enumerate() {
                if VERBOSE {
                    println!("Testing input '{input}'")
                }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let tokens = lexer.tokens().to_vec();
                if VERBOSE {
                    for (tokenid, channelid, string, caretline, caretcol) in &tokens {
                        println!("- {tokenid}, {channelid}, {string}, {caretline}, {caretcol}")
                    }
                    if lexer.has_error() {
                        println!("{:?}", lexer.get_error());
                    } else if lexer.is_eos() {
                        println!("=> OK");
                    } else {
                        println!("stream not entirely consumed");
                    }
                }
                lexer.detach_stream();
                let result_tokens = tokens.into_iter()
                    .map(|(tokenid, channelid, string, _, _)| (channelid, tokenid, string))
                    .to_vec();
                if VERBOSE {
                    println!("{}", result_tokens.iter().map(|(c, t, s)| format!("({c}, {t}, \"{s}\")")).join(", "));
                }
                let expected_tokens = expected_tokens.into_iter().map(|(c, i, s)| (c, i, s.to_string())).to_vec();
                assert_eq!(result_tokens, expected_tokens, "{text}, input {input_id}");
            }
        }
    }

    #[test]
    fn lexiparser_errors() {
        let tests = vec![
            (
                TXT1,
                vec![
                    // input    lexer OK: None, lexer error: Some(column)
                    ("a",       Some(2), Some("':'")),
                    ("a:",      Some(3), Some("'0'-'9', 'A'-'Z', '_', 'a'-'z'")),
                    ("a:aA_0",  None, None),
                ]
            ),
            (
                TXT2,
                vec![
                    ("<a>", None, None),
                    ("<a><b,", None, None),
                    ("<a,A,_, 0><b><><?c,C,_, 1!?><?d?><??>[0,\t1, 2][3][]", Some(26), Some("'\\t', ' ', ',', '0'-'9', '?', 'A'-'Z', '_', 'a'-'z'")),
                ]
            )
        ];
        const VERBOSE: bool = false;
        const JUST_SHOW_ANSWERS: bool = false;

        for (test_id, (lexicon, inputs)) in tests.into_iter().enumerate() {
            if VERBOSE || JUST_SHOW_ANSWERS { println!("// {:=<80}\n// Test {test_id}", ""); }
            let text = format!("test {test_id} failed");
            let stream = CharReader::new(Cursor::new(lexicon));
            let mut lexi = Lexi::new();
            let result = lexi.build(stream);
            let mut listener = lexi.wrapper.listener();
            if VERBOSE {
                let msg = listener.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            assert_eq!(result, Ok(()), "{text}: couldn't parse the lexicon");

            // - builds the dfa from the reg tree
            let dfa = listener.make_dfa().optimize();

            // - builds the lexer
            let lexer_tables = LexerGen::from(dfa).make_lexer_tables();
            let mut lexer = lexer_tables.make_lexer();

            // 2) tests the lexer on inputs
            for (input_id, (input, expected_error, expected_valid)) in inputs.into_iter().enumerate() {
                let text = format!("{text} in input {input_id}");
                if VERBOSE {
                    println!("Testing input '{input}'")
                }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let tokens = lexer.tokens().to_vec();
                let (result_error, valid_segments) = match lexer.get_error() {
                    LexerError::None => (None, None),
                    LexerError::InvalidChar { info }
                    | LexerError::UnrecognizedChar { info }
                    | LexerError::EndOfStream { info } => (Some(info.col), Some(lexer.get_valid_segments(info.state))),
                    LexerError::EmptyStateStack { info } => (Some(info.col), None),
                    LexerError::NoStreamAttached | LexerError::InfiniteLoop { .. } => panic!("{text}: unexpected error {:?}", lexer.get_error()),
                };
                let result_valid = valid_segments.and_then(|s| Some(s.to_string()));
                if VERBOSE {
                    for (tokenid, channelid, string, caretline, caretcol) in &tokens {
                        println!("- {tokenid}, {channelid}, {string}, {caretline}, {caretcol}")
                    }
                    println!(" => {:?}", lexer.get_error());
                    if let Some(s) = &result_valid {
                        println!("    valid characters: {s}")
                    }
                }
                lexer.detach_stream();
                let result_tokens = tokens.into_iter()
                    .map(|(tokenid, channelid, string, _, _)| (channelid, tokenid, string))
                    .to_vec();
                if VERBOSE {
                    println!("{}", result_tokens.iter().map(|(c, t, s)| format!("({c}, {t}, \"{s}\")")).join(", "));
                }
                if JUST_SHOW_ANSWERS {
                    println!("                    (\"{input}\", {result_error:?}, {result_valid:?}),");
                } else {
                    assert_eq!(result_error, expected_error, "{text}: wrong parsing outcome");
                    assert_eq!(result_valid, expected_valid.map(|s| s.to_string()), "{text}: wrong set of valid characters at this state");
                }
            }
        }
    }

    #[test]
    fn listener_data() {
        let tests = vec![
            (TXT1,
             vec!["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"],
             btreemap![0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8, 9 => 9, 10 => 10, 11 => 11, 12 => 12],
            ),
            (TXT2,
             vec!["TAG", "REF_TAG", "SPEC", "ARRAY"],
             btreemap![5 => 0, 6 => 1, 9 => 2, 10 => 3],
            ),
            (TXT3,
             vec!["A", "ID", "B"],
             btreemap![0 => 1, 1 => 0, 2 => 1, 3 => 2],
            ),
            (TXT4,
             vec!["B", "D", "E", "F"],
             btreemap![0 => 1, 1 => 0, 3 => 1, 4 => 2, 5 => 3],
            ),
            (TXT5,
             vec!["R", "P", "O", "M", "K", "I", "H", "G", "E", "C", "B", "A"],
             btreemap![0 => 1, 2 => 0, 4 => 2, 5 => 2, 6 => 3, 7 => 4, 9 => 8, 10 => 6, 11 => 7, 12 => 5, 13 => 6, 14 => 9, 15 => 11, 16 => 10],
            ),
        ];
        const VERBOSE: bool = false;
        const JUST_SHOW_ANSWERS: bool = false;

        for (test_id, (input, expected_sym, expected_end)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}", ""); }
            let stream = CharReader::new(Cursor::new(input));
            let mut lexi = Lexi::new();
            let result = lexi.build(stream);
            let mut listener = lexi.wrapper.listener();
            if VERBOSE {
                let msg = listener.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let text = format!("test {test_id} failed");
            assert_eq!(result, Ok(()), "{text}");
            if VERBOSE {
                println!("Rules lexicon {}:\n{}", listener.get_name(), listener.rules_to_string(0));
            }
            let symbol_table = listener.build_symbol_table();
            let expected_sym = expected_sym.into_iter().map(|s| s.to_string()).to_vec();
            let result_sym = symbol_table.get_terminals().map(|(s, _)| s.to_string()).to_vec();
            let result_end = listener.terminals.iter().enumerate().filter_map(|(id, t)| {
                t.iter_depth_simple().find_map(|n|
                    // unfortunately, we can't destructure entirely because term is a Box
                    if let ReType::End(term) = n.get_type() {
                        if let ActionOption::Token(end) = term.action {
                            Some((id, end))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                )
            }).collect::<BTreeMap<_,_>>();
            if JUST_SHOW_ANSWERS {
                println!("             vec![{}],", result_sym.iter().map(|s| format!("\"{s}\"")).join(", "));
                println!("             btreemap![{}],", result_end.iter().map(|(k, v)| format!("{k} => {v}")).join(", "));
            } else {
                assert_eq!(result_sym, expected_sym, "{text}: symbol table");
                assert_eq!(result_end, expected_end, "{text}: end values");
            }
            // checks that the Dfa can be built and optimized
            black_box(listener.make_dfa().optimize());
        }
    }
}

mod lexicon {
    use std::io::Cursor;
    use lexigram::CollectJoin;
    use lexigram::io::CharReader;
    use crate::lexi::Lexi;

    #[test]
    fn errors() {
        let tests = vec![
            (
                r#"lexicon test0; channels { CH1, CH1 }"#,
                vec!["channel 'CH1' defined twice"]
            ),
            (
                r"lexicon test1; fragment A: 'a1'; fragment A: 'a2';",
                vec!["symbol 'A' is already defined"]
            ),
            (
                r"lexicon test2; fragment A: 'a1'; A: 'a2';",
                vec!["symbol 'A' is already defined"]
            ),
            (
                r"lexicon test3; A: 'a1'; A: 'a2';",
                vec!["symbol 'A' is already defined"]
            ),
            (
                r#"lexicon test4;
                    R: 'fake';
                    S: 'dummy';
                    A: 'a' -> type(R), type(S);
                    B: 'b' -> skip, type(R);
                    C: 'c' -> skip, more, push(MODE_A);
                    D: 'd' -> type(R), more, pop;
                    E: 'e' -> mode(X), mode(Y), skip;"#,
                vec![
                    "can't add actions 'type(R)' and 'type(S)'",
                    "can't add actions 'skip' and 'type(R)'",
                    "can't add actions 'skip' and 'more'",
                    "can't add actions 'type(R)' and 'more'",
                    "can't add actions 'mode(X)' and 'mode(Y)",
                ]
            ),
            (
                r"lexicon test5; fragment A: 'a'; B: 'b' -> type(A);",
                vec!["rule B: 'A' is not a terminal; it's a fragment"]
            ),
            (
                r"lexicon test6; channels { CH1 } A: 'a' -> channel(CH2);",
                vec!["rule A: channel 'CH2' undefined"]
            ),
            (
                r"lexicon test7; A: ~'abc'; B: ~('ab'|'cd');",
                vec![
                    "rule A: ~ can only be applied to a char set, not to 'abc'",
                    "rule B: ~ can only be applied to a char set, not to |", // not a great message...
                ]
            ),
            (
                r"lexicon test8; fragment A: B;",
                vec!["rule A: unknown fragment 'B'"]
            ),
            (
                r#"lexicon test9; A: ':\u{d800}'; B: '\u{110000}'; C: 'a'..'\u{d800}';"#,
                vec![
                    "rule A: cannot decode the string literal ':\\u{d800}': 'd800' isn't a valid unicode",
                    "rule B: cannot decode the character literal '\\u{110000}': '110000' isn't a valid unicode",
                    "rule C: cannot decode the character literal ''\\u{d800}'': 'd800' isn't a valid unicode",
                ]
            ),
            (
                r#"lexicon test10; A: 'bad:\a'; B: 'b';"#,
                vec!["lexical error: invalid character 'a'"]
            ),
            (
                r#"lexicon test11; A: 'bad:\u100'; B: 'b';"#,
                vec!["lexical error: invalid character '1'"]
            ),
        ];
        const VERBOSE: bool = false;
        const CHECK_ALL_ERRORS: bool = false;

        for (test_id, (lexicon, mut expected_errors)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("\n// {:=<80}\n// Test {test_id}\n{lexicon}\n", ""); }
            let stream = CharReader::new(Cursor::new(lexicon));
            let mut lexi = Lexi::new();
            let _result = lexi.build(stream);
            let listener = lexi.wrapper.listener();
            let text = format!("test {test_id} failed");
            let msg = listener.get_log().get_messages().map(|s| format!("\n- {s}")).join("");
            if VERBOSE {
                if !msg.is_empty() {
                    println!("Messages:{msg}");
                }
            }
            let mut extra_errors = vec![];
            for m in listener.get_log().get_errors() {
                let mut i = 0;
                let mut found = false;
                while i < expected_errors.len() {
                    if m.contains(expected_errors[i]) {
                        expected_errors.remove(i);
                        found = true;
                        break;
                    } else {
                        i += 1;
                    }
                }
                if !found {
                    extra_errors.push(m.to_string());
                }
            }
            assert!(expected_errors.is_empty(), "{text} was expecting to find those errors while parsing the lexicon:{}\nbut got those messages:{msg}",
                    expected_errors.iter().map(|s| format!("\n- {s}")).join(""));
            if CHECK_ALL_ERRORS {
                assert!(extra_errors.is_empty(), "{text} generated unforseen errors:{}",
                        extra_errors.iter().map(|s| format!("\n- {s}")).join(""));
            }
        }
    }
}