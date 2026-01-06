// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the typedef parser

#![allow(unused)]

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::log::LogStatus;
use lexigram_lib::parsergen::NTValue;

static LEXICON_FILENAME: &str = "src/typedef.lg";
static LEXICON_TAGS: [&str; 2] = ["typedef_type_lexicon", "typedef_id_type_lexicon"];
static GRAMMAR_FILENAME: &str = "src/typedef.lg";
static GRAMMAR_TAGS: [&str; 2] = ["typedef_type_grammar", "typedef_id_type_grammar"];
static SOURCE_FILENAMES: [&str; 2] = ["../typedef/src/typedef_type.rs", "../typedef/src/typedef_id_type.rs"];
static LEXER_TAGS: [&str; 2] = ["typedef_type_lexer", "typedef_id_type_lexer"];
static PARSER_TAGS: [&str; 2] = ["typedef_type_parser", "typedef_id_type_parser"];
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_typedef_source(action: Action) {
    for i in 0..2 {
        let (lexicon_tag, grammar_tag) = (LEXICON_TAGS[i], GRAMMAR_TAGS[i]);
        let (lexer_tag, parser_tag, source_filename) = (LEXER_TAGS[i], PARSER_TAGS[i], SOURCE_FILENAMES[i]);
        let options = OptionsBuilder::new()
            .lexer(genspec!(filename: LEXICON_FILENAME, tag: lexicon_tag), gencode!(filename: source_filename, tag: lexer_tag))
            .indent(LEXER_INDENT)
            .parser(genspec!(filename: GRAMMAR_FILENAME, tag: grammar_tag), gencode!(filename: source_filename, tag: parser_tag))
            .indent(PARSER_INDENT)
            .extra_libs([format!("super::listener{}_type_types::*", if i == 0 { "" } else { "_id" })])
            .span_params(true)
            .set_nt_value(NTValue::Parents)
            .build()
            .expect("should have no error");
        match try_gen_parser(action, options) {
            Ok(log) => {
                println!("Code generated in {source_filename} [{lexer_tag}] / [{parser_tag}]\n{log}");
                println!("{} note(s)\n{} warning(s)\n", log.num_notes(), log.num_warnings());
                assert!(log.has_no_warnings(), "no warning expected");
            }
            Err(build_error) => panic!("[{lexicon_tag}] / [{grammar_tag}]: {build_error}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn gen_source() {
        gen_typedef_source(Action::Generate);
    }

    #[test]
    fn check_source() {
        gen_typedef_source(Action::Verify);
    }
}