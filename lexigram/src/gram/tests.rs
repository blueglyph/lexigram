// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

const TXT_LEXI1: &str = r#"
    lexicon A;
    //
    fragment ID     : [a-zA-Z][a-zA-Z_0-9]*;
    fragment NUM    : [0-9]+;
    fragment WS     : [ \n\r\t]+;
    //
    WhiteSpace      : WS -> skip;
    Comment         : '/*' .*? '*/' -> skip;
    LineComment     : '//' ~[\r\n]* -> skip;
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
    /**---- comments ----**/
    prog:
        instr+;                             // [Plus]
    instr:
        Let Id Equal expr Semicolon
    |   Print expr Semicolon
    |   empty_instr Semicolon;
    empty_instr:
        Sub empty_instr<L>                  // [<L>] on right recursion
    |   ;                                   // [prodFactor: prodTerm*] when empty -> ε
    expr:
        term ((Add|Sub) term)*;             // [Star] and [|] inside Plus/Star (RTS to PRS)
    term:
        Id | Sub? Int | Lparen expr Rparen; // [?]
"#;

const TXT_GRAM2: &str = r#"
    grammar B;
    tests: (test Semicolon)*;
    test: a | b | c;
    a: <L=a> Add Id a |;                    // [<L=own Id>] on right recursion
    b: (<L=b_iter> Sub Id)+;                // [<L=Id>] inside Plus/Star
    c: Let Id Equal Int <R>;                // [<R>] (not used yet in ParserGen)
"#;

mod listener {
    use super::*;
    use crate::{Lexi, Gram};
    use lexigram_lib::log::{BufLog, Logger};
    use lexigram_lib::parser::{Call, ListenerWrapper};
    use lexigram_lib::grammar::{FactorId, VarId};
    use lexigram_lib::io::CharReader;
    use lexigram_lib::lexer::{Lexer, TokenSpliterator};
    use lexigram_lib::lexergen::LexerGen;
    use lexigram_lib::parsergen::{print_flags, ParserGen};
    use lexigram_lib::{CollectJoin, LL1};
    use std::io::Cursor;
    use iter_index::IndexerIterator;

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
                    // (input, lexer ok, parser ok)
                    ("let a = 1; let b = 2; print a + (b - 5);", true, true),
                    ("let c = 10 + 11 + -12; ----;;", true, true),
                    ("let d = a; let e = d + 1 print e; print d; print a", true, false),
                    ("", false, false),
                    ("( print x;", true, false),
                ]
            ),
            (
                TXT_GRAM2,
                vec![], true,
                vec![
                    ("/* a */; + a; + a + b; /* b */ - a; -a-b; // 1st line\n/* c */ let x=2; // end", true, true),
                ]
            ),
            (
                "grammar C; a: <L=Id> Id;           // error: [<L=Id>] with Id of terminal",
                vec!["rule name in <L=Id> is already defined as terminal", "abort request"], false,
                vec![]
            ),
            (
                "grammar D; a: b;                   // error: undefined symbol",
                vec!["has been used but is not defined, neither as terminal or non-terminal"], false,
                vec![]
            ),
            (
                "grammar E; Id: Int Plus Int;       // error: terminal used as rule name",
                vec!["is a terminal and cannot be used as a rule name"], false,
                vec![]
            ),
            (
                "grammar E; a: Int; a: Plus Int;    // error: non-terminal already defined",
                vec!["non-terminal 'a' already defined"], false,
                vec![]
            ),
            (
                "grammar E; EOF: Plus Int;          // EOF as rule name, should be interesting",
                vec!["found input 'EOF' instead of 'Id'", "found input 'Id' instead of ':'"], false,
                vec![]
            ),
            (
                "grammar E; a: b* EOF; b: Id ( Equal Int )? Semicolon;",
                vec![], true,
                vec![
                    ("a; b = 1; c = 2;", true, true),
                ]
            ),
            (
                "grammar E; a: b*; b: Id EOF;       // EOF can only be in the first rule",
                vec!["EOF can only be put in the top rule"], false,
                vec![]
            ),
        ];
        const VERBOSE: bool = false;
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
        if VERBOSE {
            println!("Lexer symbol table:\n- T: {}",
                     sym_table.get_terminals().index::<VarId>().map(|(v, (t, maybe))| format!("{v}:{t}{}", if let Some(s) = maybe { format!("='{s}'") } else { String::new() })).join(", "));
        }
        let lexer_tables = LexerGen::from(dfa).make_lexer_tables();
        let mut lexer: Lexer<Cursor<&str>> = lexer_tables.make_lexer();

        for (test_id, (grammar, mut expected_grammar_errors, expected_warnings, inputs)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\ntest {test_id}", ""); }
            let text = format!("test {test_id} failed");

            // grammar parser
            let gram = Gram::<LL1, _>::new(sym_table.clone());
            let grammar_stream = CharReader::new(Cursor::new(grammar));
            let ll1 = gram.build_ll1(grammar_stream);
            let msg = ll1.get_log().get_messages().map(|s| format!("\n- {s}")).join("");
            let should_succeed = expected_grammar_errors.is_empty();
            if VERBOSE {
                ll1.print_factors();
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
                        break;
                    } else {
                        i += 1;
                    }
                }
                if expected_grammar_errors.is_empty() { break }
            }
            assert!(expected_grammar_errors.is_empty(), "was expecting to find those errors while parsing the grammar:{}\nbut got those messages:{msg}",
                    expected_grammar_errors.iter().map(|s| format!("\n- {s}")).join(""));
            if should_succeed {
                let builder = ParserGen::from(ll1);
                let msg = builder.get_log().get_messages().map(|s| format!("\n- {s}")).join("");
                if VERBOSE {
                    print_flags(&builder, 4);
                    println!("Parsing table of grammar '{}':", builder.get_name());
                    builder.get_parsing_table().print(builder.get_symbol_table(), 4);
                    if !builder.get_log().is_empty() {
                        println!("Messages:{msg}");
                    }
                }
                assert_eq!(builder.get_log().num_warnings() > 0, expected_warnings, "{} warnings:{msg}", if expected_warnings { "Expected" } else { "Didn't expect"} );
                let parser_table = builder.make_parser_tables();
                let mut parser = parser_table.make_parser();

                for (input, expected_lexer_success, expected_parser_success) in inputs {
                    if VERBOSE { println!("- input '{input}'"); }
                    let input_stream = CharReader::new(Cursor::new(input));
                    lexer.attach_stream(input_stream);
                    let mut stub = Stub::new();
                    stub.set_verbose(VERBOSE_WRAPPER);
                    let result_parser = parser.parse_stream(&mut stub, lexer.tokens().keep_channel0());
                    let msg = format!("{text}, input '{input}'");
                    if VERBOSE && !stub.get_log().is_empty() {
                        println!("  Messages:{}", stub.get_log().get_messages().map(|m| format!("\n  - {m}")).join(""));
                    }
                    assert_eq!(result_parser.is_ok(), expected_parser_success, "{msg}: {result_parser:?} instead of {}", if expected_parser_success { "success" } else { "failure" });
                    assert_eq!(!lexer.has_error(), expected_lexer_success, "{msg}: lexer errors: {}", lexer.get_error());
                }
            }
        }
    }
}