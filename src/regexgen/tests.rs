#![cfg(test)]

use std::collections::BTreeSet;
use std::io::{Cursor, Read};
use std::mem::size_of_val;
use crate::dfa::{Dfa, DfaBuilder, TokenId, tree_to_string};
use crate::escape_string;
use crate::io::CharReader;
use crate::lexer::Lexer;
use crate::lexgen::LexGen;
use super::*;
use crate::dfa::tests::print_dfa;
use crate::lexgen::tests::print_source_code;

fn make_dfa() -> Dfa {
    const VERBOSE: bool = false;
    let re = build_re();
    let mut dfa_builder = DfaBuilder::from_re(re);
    let dfa = dfa_builder.build();
    if VERBOSE {
        println!("Tree: {}", tree_to_string(&dfa_builder.get_re(), true));
        println!("Messages:\n{}", dfa_builder.get_messages());
    }
    assert_eq!(dfa_builder.get_errors().len(), 0);
    dfa
}

fn make_lexer<R: Read>(optimize: bool) -> Lexer<R> {
    const VERBOSE: bool = false;
    let mut dfa = make_dfa();
    if optimize {
        dfa.optimize();
    } else {
        dfa.normalize();
    }
    if VERBOSE { print_dfa(&dfa); }
    let lexgen = LexGen::from_dfa(&dfa);
    if VERBOSE { println!("Sources:"); print_source_code(&lexgen); }
    lexgen.make_lexer()
}

#[ignore]
#[test]
fn regexgen_re() {
    let re = build_re();
    println!("{}", crate::dfa::tree_to_string(&re, true));
}

#[test]
/// We scan source files and check the tokens and the source text they cover.
fn regexgen_lexer() {
    const VERBOSE: bool = false;
    for opt in [false, true] {
        let mut lexer = make_lexer(opt);
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
        for (test_id, inputs) in tests {
            if VERBOSE { println!("test {test_id}, opt={opt}:"); }
            for (input, expected_tokens, expected_texts) in inputs {
                //let expected_texts = expected_texts.iter().map(|s| s.escape_default());
                if VERBOSE { print!("\"{}\":", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text)| {
                    assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                    (tok.0, text)
                }).unzip();
                assert_eq!(tokens, expected_tokens, "test {} failed for opt={opt}, input '{}'", test_id, escape_string(input));
                assert_eq!(texts, expected_texts, "test {} failed for opt={opt}, input '{}'", test_id, escape_string(input));
                assert!(lexer.get_error() == None || lexer.get_error().unwrap().is_eos, "test {} failed for opt={opt}, input '{}'",
                        test_id, escape_string(input));
            }
            if VERBOSE { println!("--------------------------------------\n"); }
        }
    }
}

#[test]
/// We take the text output of each token and re-inject them to the lexer, then we compare both token streams.
fn regexgen_stability() {
    for opt in [false, true] {
        let mut lexer = make_lexer(opt);
        let stream = CharReader::new(Cursor::new(LEXICON));
        lexer.attach_stream(stream);
        let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer
            .tokens()
            .filter_map(|(tok, ch, text)| if ch == 0 { Some((tok.0, text)) } else { None })
            .unzip();
        let source2 = texts.join(" ");
        let stream2 = CharReader::new(Cursor::new(source2.as_str()));
        lexer.detach_stream();
        lexer.attach_stream(stream2);
        let (tokens2, texts2): (Vec<TokenId>, Vec<String>) = lexer
            .tokens()
            .filter_map(|(tok, ch, text)| if ch == 0 { Some((tok.0, text)) } else { None })
            .unzip();
        assert_eq!(tokens, tokens2, "failed for opt={opt}");
        assert_eq!(texts, texts2, "failed for opt={opt}");
    }
}

#[ignore]
#[test]
fn regexgen_optimize() {
    const VERBOSE: bool = true;
    for opt in [false, true] {
        println!("-----------------------------------------\n{} DFA:", if opt { "optimized" } else { "normalized" });
        let mut dfa = make_dfa();
        if opt {
            dfa.optimize();
        } else {
            dfa.normalize();
        }
        println!("DFA:\n- {} states\n- {} terminals\n- {} end states",
                 dfa.state_graph.len(),
                 dfa.end_states.iter().map(|(_, t)| t.clone()).collect::<BTreeSet<Terminal>>().len(),
                 dfa.end_states.len()
        );
        if VERBOSE { print_dfa(&dfa); }
        let mut lexgen = LexGen::new();
        lexgen.max_utf8_chars = 0;
        lexgen.build_tables(&dfa);
        let size_tables = size_of_val(lexgen.state_table.as_ref()) +
                size_of_val(lexgen.ascii_to_group.as_ref()) +
                size_of_val(&lexgen.utf8_to_group) +
                size_of_val(&lexgen.seg_to_group) +
                size_of_val(lexgen.terminal_table.as_ref());
        println!("Lexer:\n- {} states\n- {} groups\n- {} segments\n- {} terminals (table)\n- {:.1}k tables",
                 lexgen.nbr_states,
                 lexgen.nbr_groups,
                 lexgen.seg_to_group.len(),
                 lexgen.terminal_table.len(),
                 size_tables as f64 * 0.001);
        if VERBOSE {
            println!("Sources:");
            print_source_code(&lexgen);
        }
    }
}

#[ignore]
#[test]
fn type_size() {
    println!("Size of main types:");
    println!("- Terminal   : {:4} bytes", std::mem::size_of::<crate::dfa::Terminal>());
    println!("- ReType     : {:4} bytes", std::mem::size_of::<crate::dfa::ReType>());
    println!("- ReNode     : {:4} bytes", std::mem::size_of::<crate::dfa::ReNode>());
    println!("- StateId    : {:4} bytes", std::mem::size_of::<crate::dfa::StateId>());
    println!("- TokenId    : {:4} bytes", std::mem::size_of::<crate::dfa::TokenId>());
    println!("- ModeId     : {:4} bytes", std::mem::size_of::<crate::dfa::ModeId>());
    println!("- ChannelId  : {:4} bytes", std::mem::size_of::<crate::dfa::ChannelId>());
    println!("- Seg        : {:4} bytes", std::mem::size_of::<crate::segments::Seg>());
    println!("- Segments   : {:4} bytes", std::mem::size_of::<crate::segments::Segments>());
}
