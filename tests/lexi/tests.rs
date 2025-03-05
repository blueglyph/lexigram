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
    OPEN_TAG: '<' -> push(TAG_MODE), skip;
    OPEN_SPEC: '<?' -> push(SPEC_MODE), skip;
    OPEN_ARRAY: '[' -> mode(ARRAY_MODE), more;

    mode TAG_MODE;
    CLOSE_TAG: '>' -> pop, skip;
    SEP_TAG: [ \t,] -> skip;
    TAG: \w+;

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
                ]
            ),
            (
                TXT2,
                btreemap![
                    0 => branch!('<' => 5, '[' => 6),
                    1 => branch!('\t', ' ', ',' => 8, '0'-'9', 'A'-'Z', '_', 'a'-'z' => 9, '>' => 10),
                    2 => branch!('\t', ' ', ',' => 11, '0'-'9', 'A'-'Z', '_', 'a'-'z' => 12, '?' => 3),
                    3 => branch!('>' => 13),
                    4 => branch!(',' => 14, '0'-'9' => 15, ']' => 16),
                    5 => branch!('?' => 7), // <skip,push(1,state 1)>
                    6 => branch!(), // <more,mode(3,state 4)>
                    7 => branch!(), // <skip,push(2,state 2)>
                    8 => branch!(), // <skip>
                    9 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 9), // <end:5>
                    10 => branch!(), // <skip,pop>
                    11 => branch!(), // <skip>
                    12 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 12), // <end:8>
                    13 => branch!(), // <skip,pop>
                    14 => branch!(), // <more>
                    15 => branch!('0'-'9' => 15), // <more>
                    16 => branch!(), // <end:9,mode(0,state 0)>
                ],
                btreemap![
                    5 => term!(skip) + term!(push 1) + term!(pushst 1), 6 => term!(more) + term!(mode 3) + term!(pushst 4),
                    7 => term!(skip) + term!(push 2) + term!(pushst 2), 8 => term!(skip), 9 => term!(=5), 10 => term!(skip) + term!(pop),
                    11 => term!(skip), 12 => term!(=8), 13 => term!(skip) + term!(pop), 14 => term!(more), 15 => term!(more),
                    16 => term!(=9) + term!(mode 0) + term!(pushst 0)
                ]
            ),
        ];
        const VERBOSE: bool = true;
        // const VERBOSE_DETAILS: bool = false;
        const VERBOSE_WRAPPER: bool = false;
        const VERBOSE_LISTENER: bool = true;

        for (test_id, (input, expected_graph, expected_end_states)) in tests.into_iter().enumerate() {
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
                if VERBOSE {
                    println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\"");
                }
            });
            let result = parser.parse_stream(&mut wrapper, tokens);
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
            let text = format!("test {test_id} failed");
            assert_eq!(result, Ok(()), "{text}");
            assert_eq!(dfa.get_state_graph(), &expected_graph, "{text}");
            assert_eq!(dfa.get_end_states(), &expected_end_states, "{text}");
            // assert_eq!(result_tokens, expected_tokens, "{text}");
        }
    }
}
