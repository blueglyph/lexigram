// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

use std::fs::File;
use std::io::BufReader;
use lexigram::{Gram, Lexi};
use lexigram::lexi::SymbolicDfa;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::{BuildError, LL1};
use lexigram_lib::log::{BufLog, LogStatus, TryBuildFrom, TryBuildInto};
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::file_utils::{get_tagged_source, replace_tagged_source};

static LEXICON_FILENAME: &str = "examples/pandemonium.l";
static GRAMMAR_FILENAME: &str = "examples/pandemonium.g";
static SOURCE_FILENAME: &str = "examples/pandemonium.rs";
static LEXER_TAG: &str = "pandemonium_lexer";
static PARSER_TAG: &str = "pandemonium_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

#[allow(unused)]
enum Action { Verify, Generate }

fn main() {
    match gen_source(Action::Generate) {
        Ok(log) => println!("Code successfully generated in {SOURCE_FILENAME}\n{log}"),
        Err(build_error) => panic!("{build_error}"),
    }
}

fn gen_source(action: Action) -> Result<BufLog, BuildError> {
    // 1. Lexer

    let file = File::open(LEXICON_FILENAME)
        .expect(&format!("couldn't open lexicon file {LEXICON_FILENAME}"));
    let lexicon_stream = CharReader::new(BufReader::new(file));
    let lexi = Lexi::new(lexicon_stream);

    // - reads the lexicon and builds the DFA
    let SymbolicDfa { dfa, symbol_table } = lexi.try_build_into()?;

    // - builds the lexer
    let mut lexgen = LexerGen::try_build_from(dfa)?;
    lexgen.symbol_table = Some(symbol_table.clone());
    let (mut log, lexer_source) = lexgen.try_gen_source_code(LEXER_INDENT)?;

    // - writes the source code between existing tags:
    replace_tagged_source(SOURCE_FILENAME, LEXER_TAG, &lexer_source)
        .expect(&format!("lexer source replacement failed; check that file {SOURCE_FILENAME} exists and has the tags {LEXER_TAG}"));

    // 2. Parser

    let file = File::open(GRAMMAR_FILENAME)
        .expect(&format!("couldn't open lexicon file {GRAMMAR_FILENAME}"));
    let grammar_stream = CharReader::new(BufReader::new(file));

    // - parses the grammar
    let gram = Gram::new(symbol_table, grammar_stream);
    let ll1 = ProdRuleSet::<LL1>::try_build_from(gram)?;

    // - generates Lexi's parser source code (parser + listener):
    let mut builder = ParserGen::try_build_from(ll1)?;
    builder.set_parents_have_value();
    builder.add_lib("super::listener_types::*");
    builder.set_gen_span_params(true);
    let (parser_log, parser_source) = builder.try_gen_source_code(PARSER_INDENT, true)?;
    log.extend(parser_log);

     match action {
        Action::Verify => {
            assert!(log.has_no_errors(), "errors in the log:\n{log}");
            let expected = get_tagged_source(SOURCE_FILENAME, PARSER_TAG);
            match expected {
                Ok(expected_source) => assert_eq!(parser_source, expected_source),
                Err(e) => panic!("didn't find the expected sources for comparison: {e}"),
            }
        }
        Action::Generate => {
            // - writes the source code between existing tags:
            replace_tagged_source(SOURCE_FILENAME, PARSER_TAG, &parser_source)
                .expect(&format!("parser source replacement failed; check that file {SOURCE_FILENAME} exists and has the tags {PARSER_TAG}"));
        }
    }

    Ok(log)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn test_gen_source() {
        main();
    }

    #[test]
    fn test_check_source() {
        match gen_source(Action::Verify) {
            Ok(log) => println!("Code successfully generated in {SOURCE_FILENAME}\n{log}"),
            Err(build_error) => panic!("{build_error}"),
       }
    }
}