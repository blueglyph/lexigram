// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the microcalc parser

use std::fs::File;
use std::io::BufReader;
use lexigram::{Gram, Lexi};
use lexigram::lexi::SymbolicDfa;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::LL1;
use lexigram_lib::log::{BufLog, LogStatus, TryBuildFrom, TryBuildInto};
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::test_tools::replace_tagged_source;

static LEXICON_FILENAME: &str = "examples/microcalc.l";
static GRAMMAR_FILENAME: &str = "examples/microcalc.g";
static SOURCE_FILENAME: &str = "examples/microcalc.rs";
static LEXER_TAG: &str = "microcalc_lexer";
static PARSER_TAG: &str = "microcalc_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn main() {
    match gen_microcalc_source() {
        Ok(()) => println!("Code successfully generated in {SOURCE_FILENAME}"),
        Err(log) => println!("Error(s):\n{}", log.get_messages_str()),
    }
}

fn gen_microcalc_source() -> Result<(), BufLog> {
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
    let lexer_source = lexgen.try_gen_source_code(LEXER_INDENT)?;

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
    let parser_source = builder.try_gen_source_code(PARSER_INDENT, true)?;

    // - writes the source code between existing tags:
    replace_tagged_source(SOURCE_FILENAME, PARSER_TAG, &parser_source)
        .expect(&format!("parser source replacement failed; check that file {SOURCE_FILENAME} exists and has the tags {PARSER_TAG}"));

    Ok(())
}

