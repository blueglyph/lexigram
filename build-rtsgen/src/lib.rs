// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexi_gram::{gencode, genspec, lexigram_lib};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::parsergen::NTValue;

static LEXICON_FILENAME: &str = "src/rtsgen.l";
static GRAMMAR_FILENAME: &str = "src/rtsgen.g";
static SOURCE_FILENAME: &str = "../src/rtsgen/mod.rs";
static LEXER_TAG: &str = "rtsgen_lexer";
static PARSER_TAG: &str = "rtsgen_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source_rtsgen(action: Action) {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .use_full_lib(true)
        .set_nt_value(NTValue::Parents)
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            if action == Action::Generate {
                println!("Code generated in {SOURCE_FILENAME}\n{log}");
            }
            assert!(log.has_no_warnings(), "unexpected warning(s):\n{}", log.get_warnings().join("\n"));
        },
        Err(gen_error) => panic!("{gen_error}"),
    }
}

mod tests {
    use super::*;

    #[test]
    fn check_source() {
        gen_source_rtsgen(Action::Verify);
    }

    #[ignore]
    #[test]
    fn write_source() {
        gen_source_rtsgen(Action::Generate);
    }
}