// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::io::Read;
use lexigram::CollectJoin;
use lexigram::io::CharReader;
use lexigram::lexer::{Lexer, TokenSpliterator};
use lexigram::log::Logger;
use lexigram::parser::{Parser, ParserError};
use crate::lexi::LexiListener;
use crate::out::build_lexer;
use crate::out::lexiparser::lexiparser::{build_parser, Wrapper};

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

struct TestLexi<R: Read> {
    lexilexer: Lexer<R>,
    lexiparser: Parser,
    wrapper: Wrapper<LexiListener>
}

#[allow(unused)]
impl<R: Read> TestLexi<R> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const VERBOSE_LISTENER: bool = false;

    fn new() -> Self {
        let listener = LexiListener::new();
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_mut_listener().set_verbose(Self::VERBOSE_LISTENER);
        let mut lexilexer = build_lexer();
        lexilexer.set_tab_width(4);
        TestLexi {
            lexilexer,
            lexiparser: build_parser(),
            wrapper
        }
    }

    fn get_mut_listener(&mut self) -> &mut LexiListener {
        self.wrapper.get_mut_listener()
    }

    fn get_listener(&self) -> &LexiListener {
        self.wrapper.get_listener()
    }

    fn build(&mut self, lexicon: CharReader<R>) -> Result<(), ParserError> {
        self.lexilexer.attach_stream(lexicon);
        let mut result_tokens = 0;
        let tokens = self.lexilexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
            panic!("no channel {ch} in this test, line {line} col {col}, \"{text}\"")
        ).inspect(|(tok, text, line, col)| {
            result_tokens += 1;
            if Self::VERBOSE_DETAILS {
                println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\"");
            }
        });
        let result = self.lexiparser.parse_stream(&mut self.wrapper, tokens);
        result.and_then(|r| if self.wrapper.get_listener().log.num_errors() > 0 {
            // in case the parser hasn't reported any error but the listener has
            Err(ParserError::EncounteredErrors)
        } else {
            Ok(r)
        } )
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
    use crate::lexi::RuleType;
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
        ];
        const VERBOSE: bool = false;

        for (test_id, (input, expected_graph, expected_end_states, test_strs)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}", ""); }
            let stream = CharReader::new(Cursor::new(input));
            let mut lexi = TestLexi::new();
            let result = lexi.build(stream);
            let mut listener = lexi.wrapper.listener();
            if VERBOSE {
                let msg = listener.log.get_messages().map(|s| format!("- {s:?}")).join("\n");
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
            // assert_eq!(result_tokens, expected_tokens, "{text}");

            let mut lexer = LexerGen::from(dfa).make_lexer();
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
            let mut lexi = TestLexi::new();
            let result = lexi.build(stream);
            let mut listener = lexi.wrapper.listener();
            if VERBOSE {
                let msg = listener.log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            assert_eq!(result, Ok(()), "{text}: couldn't parse the lexicon");

            // - builds the dfa from the reg tree
            let dfa = listener.make_dfa().optimize();

            // - builds the lexer
            let mut lexer = LexerGen::from(dfa).make_lexer();

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
            let mut lexi = TestLexi::new();
            let result = lexi.build(stream);
            let mut listener = lexi.wrapper.listener();
            if VERBOSE {
                let msg = listener.log.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            let text = format!("test {test_id} failed");
            assert_eq!(result, Ok(()), "{text}");
            if VERBOSE {
                println!("Rules lexicon {}:\n{}", listener.name, listener.rules_to_string(0));
            }
            let symbol_table = listener.build_symbol_table();
            let expected_sym = expected_sym.into_iter().map(|s| s.to_string()).to_vec();
            let result_sym = symbol_table.get_terminals().iter().map(|(s, _)| s.to_string()).to_vec();
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

mod stability {
    use std::fs::File;
    use std::io::BufReader;
    use lexigram::CollectJoin;
    use lexigram::io::CharReader;
    use lexigram::lexergen::LexerGen;
    use lexigram::test_tools::{get_tagged_source, replace_tagged_source};
    use crate::lexi::tests::TestLexi;

    const LEXICON_FILENAME: &str = "tests/lexi/lexicon.l";
    const LEXILEXER_FILENAME: &str = "tests/out/lexilexer.rs";
    const LEXILEXER_TAG: &str = "lexilexer";

    #[ignore]
    #[test]
    /// Checks if lexi can rebuild itself identically from the lexicon (or at least produce the same output
    /// after rebuilding itself).
    ///
    /// Set `REPLACE_SOURCE` to `true`, launch the test, launch the test again.
    ///
    /// On the 2nd run, it should be successful (once it runs with the sources it has generated in the first step).
    ///
    /// Revert the changes in `lexilexer.rs` after the test.
    ///
    /// * This test replaces the source in `lexilexer.rs`, so it must be reverted if something goes wrong.
    /// * If the terminal list has changed, it must be updated in `src/lexi/mod.rs`, and the parser must be
    ///   regenerated with the test `lexiparser_source()` in `src/lexi/tests.rs` (a series of other unit tests will
    ///   have to be fixed, too).
    fn lexilexer() {
        const VERBOSE: bool = false;

        const REPLACE_SOURCE: bool = false;

        let file = File::open(LEXICON_FILENAME).expect(&format!("couldn't open lexicon file {LEXICON_FILENAME}"));
        let reader = BufReader::new(file);
        let stream = CharReader::new(reader);
        let mut lexi = TestLexi::new();
        let result = lexi.build(stream);
        let mut listener = lexi.wrapper.listener();

        if VERBOSE {
            let msg = listener.log.get_messages().map(|s| format!("- {s:?}")).join("\n");
            if !msg.is_empty() {
                println!("Parser messages:\n{msg}");
            }
            let msg = listener.log.get_messages().map(|s| format!("- {s:?}")).join("\n");
            if !msg.is_empty() {
                println!("Listener messages:\n{msg}");
            }
        }
        assert_eq!(result, Ok(()), "couldn't parse the lexicon");
        let symbol_table = listener.build_symbol_table();
        if VERBOSE {
            println!("Rules lexicon {}:\n{}", listener.name, listener.rules_to_string(0));

        }

        // - builds the dfa from the reg tree
        let dfa = listener.make_dfa().optimize();

        // - builds the lexer
        let mut lexgen = LexerGen::new();
        lexgen.max_utf8_chars = 0;
        lexgen.from_dfa(dfa);
        lexgen.symbol_table = Some(symbol_table);
        if VERBOSE {
            // terminals to replace in src/lexi/mod.rs (copy/paste)
            let sym_src = lexgen.build_symbols_source_code(0).expect("symbol source code");
            println!("Terminals:\n{sym_src}");
        }

        let result_src = lexgen.build_source_code(4);
        let expected_src = get_tagged_source(LEXILEXER_FILENAME, LEXILEXER_TAG).unwrap_or(String::new());
        if result_src != expected_src {
            if REPLACE_SOURCE {
                replace_tagged_source(LEXILEXER_FILENAME, LEXILEXER_TAG, &result_src).expect("source replacement failed");
            }
            assert_eq!(result_src, expected_src);
        }
    }
}