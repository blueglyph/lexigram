// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the microcalc parser

#![cfg(test)]

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::log::LogStatus;

static LEXICON_FILENAME: &str = "src/microcalc.l";
static GRAMMAR_FILENAME: &str = "src/microcalc.g";
static LEXICON_GRAMMAR_FILENAME: &str = "src/microcalc.lg";
static SOURCE_FILENAME: &str = "../microcalc/src/main.rs";
static LEXER_TAG: &str = "microcalc_lexer";
static PARSER_TAG: &str = "microcalc_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn write_microcalc_l_g_source(action: Action) {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            println!("Code generated in {SOURCE_FILENAME}\n{log}");
            assert!(log.has_no_warnings(), "no warning expected");
        }
        Err(build_error) => panic!("{build_error}"),
    }
}

fn write_microcalc_lg_source(action: Action) {
    let options = OptionsBuilder::new()
        .combined_spec(genspec!(filename: LEXICON_GRAMMAR_FILENAME))
        .lexer_code(gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser_code(gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            println!("Code generated in {SOURCE_FILENAME}\n{log}");
            assert!(log.has_no_warnings(), "no warning expected");
        }
        Err(build_error) => panic!("{build_error}"),
    }
}

mod tests_l_g {
    use super::*;

    #[test]
    fn check_source() {
        write_microcalc_l_g_source(Action::Verify);
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_microcalc_l_g_source(Action::Generate);
    }
}

mod test_lg {
    use super::*;

    #[test]
    fn check_source() {
        write_microcalc_lg_source(Action::Verify);
    }

}