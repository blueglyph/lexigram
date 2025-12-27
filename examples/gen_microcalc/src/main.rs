// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the microcalc parser

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::log::LogStatus;

static LEXICON_FILENAME: &str = "src/microcalc.l";
static GRAMMAR_FILENAME: &str = "src/microcalc.g";
static SOURCE_FILENAME: &str = "../microcalc/src/main.rs";
static LEXER_TAG: &str = "microcalc_lexer";
static PARSER_TAG: &str = "microcalc_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn main() {
    std::env::set_current_dir("examples/gen_microcalc").expect("couldn't change directory");
    gen_microcalc_source(Action::Generate);
}

fn gen_microcalc_source(action: Action) {
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
            println!("{} note(s)\n{} warning(s)\n", log.num_notes(), log.num_warnings());
            assert!(log.has_no_warnings(), "no warning expected");
        }
        Err(build_error) => panic!("{build_error}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn test_gen_source() {
        gen_microcalc_source(Action::Generate);
    }

    #[test]
    fn test_check_source() {
        gen_microcalc_source(Action::Verify);
    }
}