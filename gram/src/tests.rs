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
        Let Id Equal expr
    |   Print expr;
    expr:
        term (Add term)*;
    term:
        Id | Int;
"#;

mod listener {
    use super::*;
    use crate::gram::Gram;
    // use lexigram::log::{BufLog, Logger};
    // use lexigram::parser::ListenerWrapper;
    use lexi::lexi::Lexi;
    use lexigram::grammar::Symbol;
    use lexigram::io::CharReader;
    use lexigram::lexer::Lexer;
    use lexigram::lexergen::LexerGen;
    use lexigram::log::Logger;
    use lexigram::{CollectJoin, LL1};
    use std::io::Cursor;

    // struct Stub(BufLog);
    // impl ListenerWrapper for Stub {
    //     fn get_mut_log(&mut self) -> &mut impl Logger {
    //         &mut self.0
    //     }
    // }

    #[test]
    fn just_parsing() {
        let tests = vec![
            (
                TXT_GRAM1,
                vec![
                    "let a = 1; let b = 2; print a + b + 5;"
                ]
            )
        ];
        const VERBOSE: bool = true;

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

        for (test_id, (grammar, inputs)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("test {test_id}"); }
            let text = format!("test {test_id} failed");

            // grammar parser
            let gram = Gram::<LL1, _>::new(sym_table.clone());
            let grammar_stream = CharReader::new(Cursor::new(grammar));
            let ll1 = gram.build_ll1(grammar_stream);
            let msg = ll1.get_log().get_messages().map(|s| format!("\n- {s:?}")).join("");
            if VERBOSE {
                if !msg.is_empty() {
                    println!("Messages:{msg}");
                }
            }
            assert!(ll1.get_log().has_no_errors(), "{text}: couldn't build the production rules:{msg}");

            for input in inputs {
                if VERBOSE { println!("- input '{input}'"); }
                let input_stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(input_stream);
                let tokens = lexer.tokens().to_vec();
                let n = tokens.len();
                if VERBOSE { println!("  {n} tokens: {}", tokens.iter().map(|t| Symbol::T(t.0).to_str(Some(&sym_table))).join(" ")); }
                assert!(n > 0, "{text}: no token extracted from input '{input}'");
                assert!(!lexer.has_error(), "lexer errors: {}", lexer.get_error());

                // we don't have the test parser yet
            }
        }
    }
}