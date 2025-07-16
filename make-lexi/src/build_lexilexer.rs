// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexigram::Lexi;
use lexigram_lib::CollectJoin;
use lexigram_lib::dfa::print_dfa;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::parser::ParserError;
use lexigram_lib::test_tools::replace_tagged_source;
use crate::{BUILD_LEXIPARSER_FILENAME, LEXILEXER_FILENAME, LEXILEXER_LEXICON, LEXILEXER_TAG, LEXI_SYM_T_TAG};

fn lexilexer_source(lexicon_filename: &str, indent: usize, verbose: bool) -> Result<(String, String), ParserError> {
    let file = File::open(lexicon_filename).expect(&format!("couldn't open lexicon file {lexicon_filename}"));
    let reader = BufReader::new(file);
    let stream = CharReader::new(reader);
    let mut lexi = Lexi::new();
    let result = lexi.build(stream);
    let mut listener = lexi.wrapper.listener();
    if verbose {
        let msg = listener.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Parser messages:\n{msg}");
        }
        let msg = listener.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Listener messages:\n{msg}");
        }
    }
    if let Err(error) = result {
        return Err(error);
    }
    let symbol_table = listener.build_symbol_table();
    if verbose {
        println!("Rules lexicon {}:\n{}", listener.get_name(), listener.rules_to_string(0));
    }
    // - builds the dfa from the reg tree
    let dfa = listener.make_dfa().optimize();
    if verbose {
        println!("Dfa:");
        print_dfa(&dfa, 4);
    }
    let sym_src = symbol_table.build_source_code_t(0, false, true);

    // - builds the lexer
    let mut lexgen = LexerGen::new();
    lexgen.max_utf8_chars = 0;
    lexgen.build_from_dfa(dfa);
    lexgen.symbol_table = Some(symbol_table);
    if verbose {
        // terminals to replace in src/lexigram/lexiparser.rs (copy/paste)
        println!("Terminals:\n{sym_src}");
    }
    Ok((sym_src, lexgen.build_source_code(indent)))
}

pub fn write_lexilexer() {
    let (result_sym, result_src) = lexilexer_source(LEXILEXER_LEXICON, 4, true)
        .inspect_err(|e| eprintln!("Failed to parse lexicon: {e:?}"))
        .unwrap();
    replace_tagged_source(BUILD_LEXIPARSER_FILENAME, LEXI_SYM_T_TAG, &result_sym)
        .expect("parser symbol replacement failed");
    replace_tagged_source(LEXILEXER_FILENAME, LEXILEXER_TAG, &result_src)
        .expect("lexer source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;

        let (_result_sym, _result_src) = lexilexer_source(LEXILEXER_LEXICON, 4, VERBOSE)
            .inspect_err(|e| eprintln!("Failed to parse lexicon: {e:?}"))
            .unwrap();
        if !cfg!(miri) {
            let expected_sym = get_tagged_source(BUILD_LEXIPARSER_FILENAME, LEXI_SYM_T_TAG).unwrap_or(String::new());
            let expected_src = get_tagged_source(LEXILEXER_FILENAME, LEXILEXER_TAG).unwrap_or(String::new());
            assert_eq!(_result_sym, expected_sym);
            assert_eq!(_result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_lexilexer();
    }
}
