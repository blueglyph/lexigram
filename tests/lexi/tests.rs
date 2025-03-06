#![cfg(test)]

use rlexer::CollectJoin;

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

mod simple {
    use std::io::Cursor;
    use rlexer::{branch, btreemap, term};
    use rlexer::dfa::{print_dfa, tree_to_string};
    use rlexer::io::CharReader;
    use crate::out::build_lexer;
    use crate::out::lexiparser::lexiparser::{build_parser, ListenerWrapper};
    use rlexer::lexer::TokenSpliterator;
    use rlexer::lexergen::LexerGen;
    use crate::lexi::{LexiListener, RuleType};
    use super::*;

    #[test]
    fn lexer_parser() {
        let tests = [
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
                    8 => term!(skip) + term!(push 2) + term!(pushst 3), 9 => term!(skip), 10 => term!(=5), 11 => term!(skip) + term!(pop),
                    12 => term!(=6) + term!(#1), 13 => term!(skip), 14 => term!(=9), 15 => term!(skip) + term!(pop), 16 => term!(skip),
                    17 => term!(more), 18 => term!(more), 19 => term!(=10) + term!(mode 0) + term!(pushst 0)
               ], vec![
                (
                    "<a,A,\t_, 0><b><><?c,C,\t_, 1?><?d?><??>[0,\t1, 2][3][]",
                    vec![
                        (0, 5, "a"), (0, 5, "A"), (0, 5, "_"), (0, 5, "0"), (0, 5, "b"), (0, 9, "c"), (0, 9, "C"),
                        (0, 9, "_"), (0, 9, "1"), (0, 9, "d"), (0, 10, "2]"), (0, 10, "[3]"), (0, 10, "[]")
                    ]
                ),
                (
                    "<a@1,b@my_b_1>",
                    vec![(0, 5, "a"), (1, 6, "@1"), (0, 5, "b"), (1, 6, "@my_b_1")]
                )
                ]
            ),
        ];
        const VERBOSE: bool = true;
        const VERBOSE_WRAPPER: bool = false;
        const VERBOSE_LISTENER: bool = false;
        const VERBOSE_DETAILS: bool = false;

        for (test_id, (input, expected_graph, expected_end_states, test_strs)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}", ""); }
            let stream = CharReader::new(Cursor::new(input));
            let mut lexer = build_lexer();
            lexer.set_tab_width(4);
            lexer.attach_stream(stream);

            let mut parser = build_parser();
            let mut listener = LexiListener::new();
            listener.verbose = VERBOSE_LISTENER;
            let mut wrapper = ListenerWrapper::new(listener, false);
            wrapper.set_verbose(VERBOSE_WRAPPER);
            let mut result_tokens = 0;
            let tokens = lexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
                panic!("no channel {ch} in this test, line {line} col {col}, \"{text}\"")
            ).inspect(|(tok, text, line, col)| {
                result_tokens += 1;
                if VERBOSE_DETAILS {
                    println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\"");
                }
            });
            let result = parser.parse_stream(&mut wrapper, tokens);
            let text = format!("test {test_id} failed");
            assert_eq!(result, Ok(()), "{text}");
            listener = wrapper.listener();
            if VERBOSE {
                println!("Rules:");
                let mut rules = listener.rules.iter().to_vec();
                rules.sort_by(|a, b| (&a.1, &a.0).cmp(&(&b.1, &b.0)));
                // rules.sort_by_key(|(s, rt)| (rt, s));
                for (s, rt) in rules {
                    let t = match rt {
                        RuleType::Fragment(id) => listener.fragments.get(*id as usize).unwrap(),
                        RuleType::Terminal(id) => listener.terminals.get(*id as usize).unwrap(),
                    };
                    println!("- {rt:?} {s}: {}", tree_to_string(t, None, true));
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
                    if lexer.is_eos() {
                        println!("=> OK");
                    } if lexer.has_error() {
                        println!("{:?}", lexer.get_error());
                    } else {
                        println!("stream not entirely consummed");
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
}
