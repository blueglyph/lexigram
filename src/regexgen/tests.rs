#![cfg(test)]

use std::io::{Cursor, Read};
use crate::dfa::{DfaBuilder, TokenId, tree_to_string};
use crate::escape_string;
use crate::io::CharReader;
use crate::lexer::Lexer;
use crate::lexgen::LexGen;
use super::*;
use crate::dfa::tests::print_dfa;
use crate::lexgen::tests::print_source_code;

fn make_lexer<R: Read>() -> Lexer<R> {
    const VERBOSE: bool = false;
    let re = build_re();
    let mut dfa_builder = DfaBuilder::from_re(re);
    let mut dfa = dfa_builder.build();
    if VERBOSE {
        println!("Tree: {}", tree_to_string(&dfa_builder.get_re(), true));
        println!("Messages:\n{}", dfa_builder.get_messages());
    }
    assert_eq!(dfa_builder.get_errors().len(), 0);
    dfa.normalize();
    if VERBOSE { print_dfa(&dfa); }
    let lexgen = LexGen::from_dfa(&dfa);
    if VERBOSE { println!("Sources:"); print_source_code(&lexgen); }
    lexgen.make_lexer()
}

#[test]
fn regexgen_re() {
    let re = build_re();
    println!("{}", crate::dfa::tree_to_string(&re, true));

}

#[test]
fn regexgen_lexer() {
    let mut lexer = make_lexer();
    let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
        (1, vec![
            // no error
            ("-> : , .. { ( ~ + | ? } ) ; * channels fragment grammar lexer mode pop push return skip",
             vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22],
             vec!["->", ":", ",", "..", "{", "(", "~", "+", "|", "?", "}", ")", ";", "*",
                  "channels", "fragment", "grammar", "lexer", "mode", "pop", "push", "return", "skip"]),
        ]),
        (2, vec![(LEXICON, LEXICON_TOKENS.to_vec(), LEXICON_TEXT.to_vec())]),

    ];
    const VERBOSE: bool = false;

    for (test_id, inputs) in tests.into_iter() {
        if VERBOSE { println!("test {test_id}:"); }
        for (input, expected_tokens, expected_texts) in inputs {
            //let expected_texts = expected_texts.iter().map(|s| s.escape_default());
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text)| {
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                (tok.0, text)
            }).unzip();
            assert_eq!(tokens, expected_tokens, "test {} failed for input '{}'", test_id, escape_string(input));
            assert_eq!(texts, expected_texts, "test {} failed for input '{}'", test_id, escape_string(input));
            assert!(lexer.get_error() == None || lexer.get_error().unwrap().is_eos, "test {} failed for input '{}'",
                    test_id, escape_string(input));

        }
        if VERBOSE { println!("--------------------------------------\n"); }
    }
}
