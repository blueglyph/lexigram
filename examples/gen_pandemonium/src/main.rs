// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

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

fn main() {
    std::env::set_current_dir("examples/gen_pandemonium").expect("couldn't change directory");
    gen_pandemonium_source(Action::Generate);
}

fn gen_pandemonium_source(action: Action) {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .span_params(true)
        .set_nt_value(NTValue::Parents)
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            println!("Code generated in {SOURCE_FILENAME}\n{log}");
            println!("{} note(s)\n{} warning(s)\n{} error(s)", log.num_notes(), log.num_warnings(), log.num_errors());
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
    fn gen_source() {
        gen_pandemonium_source(Action::Generate);
    }

    #[test]
    fn check_source() {
        gen_pandemonium_source(Action::Verify);
    }
}