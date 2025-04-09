// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

const TXT_LEXI1: &str = r#"
    lexicon A;
    fragment ID     : [a-zA-Z][a-zA-Z_0-9]*;
    fragment NUM    : [0-9]+;
    fragment WS     : [ \n\r\t]+;
    WhiteSpace      : WS -> skip;
    Add             : '+';
    Equal           : '=';
    Lparen          : '(';
    Rparen          : ')';
    Semicolon       : ';';
    Sub             : '-';
    Let             : 'let';
    Print           : 'print';
    Id              : ID;
    Int             : NUM;
"#;

const TXT_GRAM1: &str = r#"
    grammar A;
    prog:
        instr+;
    instr:
        Let Id Equal expr Semicolon
    |   Print expr Semicolon
    |   empty_instr Semicolon;
    empty_instr:
        Sub empty_instr<L>
    |   ;
    expr:
        term ((Add|Sub) term <L>)*;
    term:
        Id | Sub? Int | Lparen expr Rparen;
"#;

const TXT_GRAM1b: &str = r#"
    grammar A;
    prog:
        instr+;
    instr:
        Let Id Equal expr Semicolon
    |   Print expr Semicolon
    |   empty_instr Semicolon;
    empty_instr:
        Sub empty_instr<L>
    |   ;
    expr:
        term ((Add|Sub) term <L=expr_item>)*;
    term:
        Id | Sub? Int | Lparen expr Rparen;
"#;

const TXT_GRAM2: &str = r#"
    grammar B;
    a: <L=Id> Id;
"#;

mod listener {
    use super::*;
    use crate::gram::Gram;
    use lexigram::log::{BufLog, Logger};
    use lexigram::parser::{Call, ListenerWrapper};
    use lexi::lexi::Lexi;
    use lexigram::grammar::{print_ll1_table, print_prs_factors, FactorId, VarId};
    use lexigram::io::CharReader;
    use lexigram::lexer::{Lexer, TokenSpliterator};
    use lexigram::lexergen::LexerGen;
    use lexigram::parsergen::ParserGen;
    use lexigram::{CollectJoin, LL1};
    use std::io::Cursor;

    struct Stub {
        log: BufLog,
        verbose: bool
    }

    impl Stub {
        pub fn new() -> Self {
            Stub { log: BufLog::new(), verbose: false }
        }

        pub fn set_verbose(&mut self, verbose: bool) {
            self.verbose = verbose;
        }

        pub fn get_log(&self) -> &BufLog {
            &self.log
        }
    }

    impl ListenerWrapper for Stub {
        fn switch(&mut self, call: Call, nt: VarId, factor_id: FactorId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!(":: {call:?} nt={nt} factor_id={factor_id}{}",
                         if let Some(t) = t_data { format!(" - {}", t.iter().join(",")) } else { String::new() });
            }
        }

        fn get_mut_log(&mut self) -> &mut impl Logger {
            &mut self.log
        }

    }

    #[test]
    fn just_parsing() {
        let tests = vec![
            (
                TXT_GRAM1,
                vec![], false,
                vec![
                    ("let a = 1; let b = 2; print a + (b - 5);", true),
                    ("let c = 10 + 11 + -12; ----;;", true),
                    ("let d = a; let e = d + 1 print e; print d; print a", false),
                ]
            ),
            (
                TXT_GRAM1b,
                vec![], false,
                vec![
                    ("let a = 1; let b = 2; print a + (b - 5);", true),
                    ("let c = 10 + 11 + -12; ----;;", true),
                    ("let d = a; let e = d + 1 print e; print d; print a", false),
                ]
            ),
           (
                TXT_GRAM2,
                vec!["rule name in <L=Id> is already defined as terminal", "abort request"], false,
                vec![]
            )
         ];
        const VERBOSE: bool = true;
        const VERBOSE_WRAPPER: bool = false;

        // parses the test lexicon
        let lexicon_stream = CharReader::new(Cursor::new(TXT_LEXI1));
        let mut lexi = Lexi::new();
        let result = lexi.build(lexicon_stream);
        let mut listener = lexi.wrapper.listener();
        if VERBOSE {
            let msg = listener.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
            if !msg.is_empty() {
                println!("Messages:\n{msg}");
            }
        }
        assert_eq!(result, Ok(()), "couldn't parse the lexicon");

        // builds the lexer for the test lexicon
        let dfa = listener.make_dfa().optimize();
        let sym_table = listener.build_symbol_table();
        let mut lexer: Lexer<Cursor<&str>> = LexerGen::from(dfa).make_lexer();

        for (test_id, (grammar, mut expected_grammar_errors, expected_warnings, inputs)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("\ntest {test_id}"); }
            let text = format!("test {test_id} failed");

            // grammar parser
            let gram = Gram::<LL1, _>::new(sym_table.clone());
            let grammar_stream = CharReader::new(Cursor::new(grammar));
            let (ll1, name) = gram.build_ll1(grammar_stream);
            let msg = ll1.get_log().get_messages().map(|s| format!("\n- {s:?}")).join("");
            let should_succeed = expected_grammar_errors.is_empty();
            if VERBOSE {
                print_prs_factors(&ll1);
                if !should_succeed {
                    if !msg.is_empty() {
                        println!("Messages:{msg}");
                    }
                }
            }
            assert_eq!(ll1.get_log().has_no_errors() && ll1.get_log().has_no_warnings(), should_succeed,
                       "{text}: was expecting grammar parsing to {}:{msg}", if expected_grammar_errors.is_empty() { "succeed" } else { "fail" });
            for m in ll1.get_log().get_errors() {
                let mut i = 0;
                while i < expected_grammar_errors.len() {
                    if m.contains(expected_grammar_errors[i]) {
                        expected_grammar_errors.remove(i);
                    } else {
                        i += 1;
                    }
                }
                if expected_grammar_errors.is_empty() { break }
            }
            assert!(expected_grammar_errors.is_empty(), "was expecting to find those errors while parsing the grammar:{}\nbut got those messages:{msg}",
                    expected_grammar_errors.iter().map(|s| format!("\n- {s}")).join(""));
            if should_succeed {
                let builder = ParserGen::from_rules(ll1, name.clone());
                let msg = builder.get_log().get_messages().map(|s| format!("\n- {s:?}")).join("");
                if VERBOSE {
                    println!("Parsing table of grammar '{name}':");
                    print_ll1_table(builder.get_symbol_table(), builder.get_parsing_table(), 4);
                    if !builder.get_log().is_empty() {
                        println!("Messages:{msg}");
                    }
                }
                assert_eq!(builder.get_log().num_warnings() > 0, expected_warnings, "{} warnings:{msg}", if expected_warnings { "Expected" } else { "Didn't expect"} );
                let mut parser = builder.make_parser();

                for (input, expected_success) in inputs {
                    if VERBOSE { println!("- input '{input}'"); }
                    let input_stream = CharReader::new(Cursor::new(input));
                    lexer.attach_stream(input_stream);
                    let mut stub = Stub::new();
                    stub.set_verbose(VERBOSE_WRAPPER);
                    let result_parser = parser.parse_stream(&mut stub, lexer.tokens().keep_channel0());
                    let msg = format!("{text}, input '{input}'");
                    if VERBOSE && !stub.get_log().is_empty() {
                        println!("  Messages:{}", stub.get_log().get_messages().map(|m| format!("\n  - {m:?}")).join(""));
                    }
                    assert_eq!(result_parser.is_ok(), expected_success, "{msg}: {result_parser:?} instead of {}", if expected_success { "success" } else { "failure" });
                    assert!(!lexer.has_error(), "{msg}: lexer errors: {}", lexer.get_error());
                }
            }
        }
    }
}