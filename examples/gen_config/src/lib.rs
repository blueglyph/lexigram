// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the microcalc parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::lexigram_lib::parsergen::NTValue;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;

static LEXICON_GRAMMAR_FILENAME: &str = "src/config.lg";
static SOURCE_FILENAME: &str = "../config/src/parser.rs";
static LEXER_TAG: &str = "config_lexer";
static PARSER_TAG: &str = "config_parser";
static SOURCE_TEMPLATES: &str = "../config/templates.txt";
static USERS_TAG: &str = "template_user_types";
static LISTENER_TAG: &str = "template_listener_impl";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;
const INDENT_TEMPLATES: usize = 4;
static NT_NAMES: [&str; 7] = [
        "<default>",
        "-config",
        "-definitions",
        "-i_def",
        "-lexer",
        "-parser",
        "-options",
];


// -------------------------------------------------------------------------

fn gen_source_config_lg(action: Action) {
    let options = OptionsBuilder::new()
        .headers(["use lexi_gram::lexigram_lib::lexigram_core;"])
        .combined_spec(genspec!(filename: LEXICON_GRAMMAR_FILENAME))
        .lexer_code(gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser_code(gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .types_code(gencode!(filename: SOURCE_TEMPLATES, tag: USERS_TAG))
        .indent(INDENT_TEMPLATES)
        .listener_code(gencode!(filename: SOURCE_TEMPLATES, tag: LISTENER_TAG))
        .indent(INDENT_TEMPLATES)
        .libs(["super::listener_types::*"])
        .span_params(true)
        .set_nt_value(NTValue::SetNames(NT_NAMES.into_iter().map(|s| s.to_string()).to_vec()))
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            if action == Action::Generate {
                println!("Code generated in {SOURCE_FILENAME}\n{log}");
            }
            assert!(log.has_no_warnings(), "no warning expected");
        }
        Err(build_error) => panic!("{build_error}"),
    }
}

mod test_lg {
    use super::*;

    #[test]
    fn check_source() {
        gen_source_config_lg(Action::Verify);
    }

    #[ignore]
    #[test]
    fn write_source() {
        gen_source_config_lg(Action::Generate);
    }
}
