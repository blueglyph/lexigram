// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the microcalc parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::lexigram_lib::parsergen::{NTValue, ParserType};
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;

static LEXICON_GRAMMAR_FILENAME: &str = "src/config.lg";
static SOURCE_LL1_FILENAME: &str = "../config/src/parser_ll1.rs";
static SOURCE_LALR_FILENAME: &str = "../config/src/parser_lalr.rs";
static LEXER_TAG: &str = "config_lexer";
static PARSER_TAG: &str = "config_parser";
static SOURCE_LL1_TEMPLATES: &str = "../config/templates_ll1.txt";
static SOURCE_LALR_TEMPLATES: &str = "../config/templates_lalr.txt";
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

fn gen_source_config_lg(action: Action, parser_type: ParserType) {
    let (source_filename, source_templates) = match parser_type {
        ParserType::LL1 => (SOURCE_LL1_FILENAME, SOURCE_LL1_TEMPLATES),
        ParserType::LALR => (SOURCE_LALR_FILENAME, SOURCE_LALR_TEMPLATES),
    };
    let options = OptionsBuilder::new()
        .headers(["use lexi_gram::lexigram_lib::lexigram_core;"])
        .combined_spec(genspec!(filename: LEXICON_GRAMMAR_FILENAME))
        .lexer_code(gencode!(filename: source_filename, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser_code(gencode!(filename: source_filename, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .types_code(gencode!(filename: source_templates, tag: USERS_TAG))
        .indent(INDENT_TEMPLATES)
        .listener_code(gencode!(filename: source_templates, tag: LISTENER_TAG))
        .indent(INDENT_TEMPLATES)
        .libs(["super::listener_types::*"])
        .span_params(true)
        .set_nt_value(NTValue::SetNames(NT_NAMES.into_iter().map(|s| s.to_string()).to_vec()))
        .parser_type(parser_type)
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            if action == Action::Generate {
                println!("Code generated in {source_filename}\n{log}");
            }
            assert!(log.has_no_warnings(), "no warning expected");
        }
        Err(build_error) => panic!("{build_error}"),
    }
}

mod test_lg {
    use super::*;

    #[test]
    fn check_source_ll1() {
        gen_source_config_lg(Action::Verify, ParserType::LL1);
    }

    #[test]
    fn check_source_lalr() {
        gen_source_config_lg(Action::Verify, ParserType::LALR);
    }

    #[ignore]
    #[test]
    fn write_source_ll1() {
        gen_source_config_lg(Action::Generate, ParserType::LL1);
    }

    #[ignore]
    #[test]
    fn write_source_lalr() {
        gen_source_config_lg(Action::Generate, ParserType::LALR);
    }
}
