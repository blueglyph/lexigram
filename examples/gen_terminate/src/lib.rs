// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the terminate parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::lexigram_lib::parsergen::ParserType;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;

static GRAMMAR_FILENAME: &str = "src/terminate.lg";
static SOURCE_FILENAME_LL1: &str = "../terminate/src/ll1.rs";
static TEMPLATE_FILENAME_LL1: &str = "../terminate/src/tpl_ll1.txt";
static SOURCE_FILENAME_LALR: &str = "../terminate/src/lalr.rs";
static TEMPLATE_FILENAME_LALR: &str = "../terminate/src/tpl_lalr.txt";
static LEXER_TAG: &str = "terminate_lexer";
static PARSER_TAG: &str = "terminate_parser";
static LIBS: &str = "super::listener_terminate_types::*";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source_terminate(action: Action, parser_type: ParserType) {
    let (source_filename, template_filename) = match parser_type {
        ParserType::LL1 => (SOURCE_FILENAME_LL1, TEMPLATE_FILENAME_LL1),
        ParserType::LALR => (SOURCE_FILENAME_LALR, TEMPLATE_FILENAME_LALR),
    };
    let options = OptionsBuilder::new()
        .combined_spec(genspec!(filename: GRAMMAR_FILENAME))
        .lexer_code(gencode!(filename: source_filename, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser_code(gencode!(filename: source_filename, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .listener_code(gencode!(filename: template_filename))
        .indent(PARSER_INDENT)
        .libs([LIBS])
        .span_params(true)
        .parser_type(parser_type)
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            if action == Action::Generate {
                println!("Code generated in {source_filename} [{LEXER_TAG}] / [{PARSER_TAG}]\n{log}");
            }
            assert!(log.has_no_warnings(), "unexpected warning(s):\n{}", log.get_warnings().join("\n"));
        }
        Err(build_error) => panic!("{build_error}"),
    }
}


mod tests {
    use super::*;

    #[test]
    fn check_source_ll1() {
        gen_source_terminate(Action::Verify, ParserType::LL1);
    }

    #[test]
    fn check_source_lalr() {
        gen_source_terminate(Action::Verify, ParserType::LALR);
    }

    #[ignore]
    #[test]
    fn write_source_ll1() {
        gen_source_terminate(Action::Generate, ParserType::LL1);
    }

    #[ignore]
    #[test]
    fn write_source_lalr() {
        gen_source_terminate(Action::Generate, ParserType::LALR);
    }
}
