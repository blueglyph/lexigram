#![cfg(test)]

use rlexer::CollectJoin;

const TXT1: &str = r#"
    lexicon A;
    fragment ALPHANUM: [0-9A-Za-z_];
    LABEL: 'code:' ALPHANUM+;
    WHITESPACE: [ \n\r\t]+ -> skip;
"#;

mod simple {
    use std::io::Cursor;
    use rlexer::{branch, btreemap, term};
    use rlexer::dfa::{print_dfa, tree_to_string};
    use rlexer::io::CharReader;
    use crate::gen::build_lexer;
    use crate::gen::lexiparser::lexiparser::{build_parser, ListenerWrapper};
    use rlexer::lexer::TokenSpliterator;
    use crate::lexi::{LexiListener, RuleType};
    use super::*;

    #[test]
    fn lexer_parser() {
        let tests = [
            (
                TXT1,
                btreemap![
                    0 => branch!('\t'-'\n', '\r', ' ' => 6, 'c' => 1),
                    1 => branch!('o' => 2),
                    2 => branch!('d' => 3),
                    3 => branch!('e' => 4),
                    4 => branch!(':' => 5),
                    5 => branch!('-', '0', '9', 'A', 'Z', '_', 'a', 'z' => 7),
                    6 => branch!('\t'-'\n', '\r', ' ' => 6), // <skip>
                    7 => branch!('-', '0', '9', 'A', 'Z', '_', 'a', 'z' => 7), // <end:0>
                ], btreemap![
                    6 => term!(skip), 7 => term!(=0)
            ]),
        ];
        const VERBOSE: bool = true;
        // const VERBOSE_DETAILS: bool = false;
        const VERBOSE_WRAPPER: bool = false;
        const VERBOSE_LISTENER: bool = false;

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
