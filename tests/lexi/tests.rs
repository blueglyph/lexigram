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
    WHITESPACE: [ \n\r\t]+ -> skip;
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
                    0 => branch!('\t'-'\n', '\r', ' ' => 15, 'a' => 1, 'b' => 9, 'c' => 10, 'd' => 11, 'e' => 12, 'f' => 13, 'g' => 14),
                    1 => branch!(':' => 2),
                    2 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 16),
                    3 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 17),
                    4 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 18),
                    5 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 19),
                    6 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 20),
                    7 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 21),
                    8 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 22),
                    9 => branch!(':' => 3),
                    10 => branch!(':' => 4),
                    11 => branch!(':' => 5),
                    12 => branch!(':' => 6),
                    13 => branch!(':' => 7),
                    14 => branch!(':' => 8),
                    15 => branch!('\t'-'\n', '\r', ' ' => 15), // <skip>
                    16 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 16), // <end:0>
                    17 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 17), // <end:1>
                    18 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 18), // <end:2>
                    19 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 19), // <end:3>
                    20 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 20), // <end:4>
                    21 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 21), // <end:5>
                    22 => branch!('0'-'9', 'A'-'Z', '_', 'a'-'z' => 22), // <end:6>
                ], btreemap![
                    15 => term!(skip), 16 => term!(=0), 17 => term!(=1), 18 => term!(=2), 19 => term!(=3), 20 => term!(=4), 21 => term!(=5), 22 => term!(=6)
            ]),
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

/*
    grammar LexiParser;

    file: header? file_item*;

    file_item:
        option | declaration | rule
    ;

    header:
        LEXICON ID SEMICOLON
    ;

    declaration:
        MODE ID SEMICOLON
    ;

    option:
        CHANNELS LBRACKET ID (COMMA ID)* RBRACKET
    ;

    rule:
        FRAGMENT ID COLON match SEMICOLON
    |   ID COLON match (ARROW actions)? SEMICOLON
    ;

    actions:
        action (COMMA action)*
    ;

    action:
        MODE LPAREN ID RPAREN
    |   PUSH LPAREN ID RPAREN
    |   POP
    |   SKiP
    |   MORE
    |   TYPE LPAREN ID RPAREN
    |   CHANNEL LPAREN ID RPAREN
    ;

    match:
        alt_items
    ;

    alt_items:
v       alt_items OR alt_item
v   |   alt_item
    ;

    alt_item:
v       repeat_item+
    ;

    repeat_item:
        item STAR QUESTION?
    |   item PLUS QUESTION?
    |   item QUESTION?
    ;

    item:
v       ID
v   |   CHAR_LIT (ELLIPSIS CHAR_LIT)?
v   |   STR_LIT
v   |   char_set
v   |   LPAREN alt_items RPAREN
v   |   NEGATE item
    ;

    char_set:
v       LSBRACKET (char_set_one)+ RSBRACKET
    |   DOT
v   |   FIXED_SET;
    ;

    char_set_one:
v       SET_CHAR MINUS SET_CHAR
v   |   SET_CHAR
v   |   FIXED_SET;

*/