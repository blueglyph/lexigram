// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

#![cfg(test)]

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::log::LogStatus;
use lexigram_lib::parsergen::NTValue;

static LEXICON_FILENAME: &str = "src/pandemonium.l";
static GRAMMAR_FILENAME: &str = "src/pandemonium.g";
static SOURCE_FILENAME: &str = "../pandemonium/src/lib.rs";
static LEXER_TAG: &str = "pandemonium_lexer";
static PARSER_TAG: &str = "pandemonium_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn write_pandemonium_source(action: Action) {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .span_params(true)
        .set_nt_value(NTValue::Default)
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

mod tests {
    use super::*;

    #[test]
    fn check_source() {
        write_pandemonium_source(Action::Verify);
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_pandemonium_source(Action::Generate);
    }
}