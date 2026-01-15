// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the terminate parser

#![cfg(test)]

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::log::LogStatus;

static LEXICON_FILENAME: &str = "src/terminate.lg";
static LEXICON_TAG: &str = "terminate_lexicon";
static GRAMMAR_FILENAME: &str = "src/terminate.lg";
static GRAMMAR_TAG: &str = "terminate_grammar";
static SOURCE_FILENAME: &str = "../terminate/src/terminate.rs";
static LEXER_TAG: &str = "terminate_lexer";
static PARSER_TAG: &str = "terminate_parser";
static LIBS: &str = "super::listener_terminate_types::*";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_terminate_source(action: Action) {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME, tag: LEXICON_TAG), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME, tag: GRAMMAR_TAG), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs([LIBS])
        .span_params(true)
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            println!("Code generated in {SOURCE_FILENAME} [{LEXER_TAG}] / [{PARSER_TAG}]\n{log}");
            println!("{} note(s)\n{} warning(s)\n", log.num_notes(), log.num_warnings());
            assert!(log.has_no_warnings(), "no warning expected");
        }
        Err(build_error) => panic!("[{LEXICON_TAG}] / [{GRAMMAR_TAG}]: {build_error}"),
    }
}


mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn gen_source() {
        gen_terminate_source(Action::Generate);
    }

    #[test]
    fn check_source() {
        gen_terminate_source(Action::Verify);
    }
}
