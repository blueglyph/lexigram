// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the terminate parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;
use lexigram_lib::parsergen::NTValue;

static LEXICON_FILENAME: &str = "src/watcher.lg";
static SOURCE_FILENAME: &str = "../watcher/src/lib.rs";
static LEXER_TAG: &str = "watcher_lexer";
static PARSER_TAG: &str = "watcher_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source_watcher(action: Action) {
    let options = OptionsBuilder::new()
        .combined_spec(genspec!(filename: LEXICON_FILENAME))
        .lexer_code(gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        // grammar is combined with lexicon, no need to define parser_spec
        .parser_code(gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .set_nt_value(NTValue::None)
        .span_params(true)
        .build()
        .expect("should have no error");
    match try_gen_parser(action, options) {
        Ok(log) => {
            if action == Action::Generate {
                println!("Code generated in {SOURCE_FILENAME} [{LEXER_TAG}] / [{PARSER_TAG}]\n{log}");
            }
            assert!(log.has_no_warnings(), "unexpected warning(s):\n{}", log.get_warnings().join("\n"));
        }
        Err(build_error) => panic!("{build_error}"),
    }
}


mod tests {
    use super::*;

    #[test]
    fn check_source() {
        gen_source_watcher(Action::Verify);
    }

    #[ignore]
    #[test]
    fn write_source() {
        gen_source_watcher(Action::Generate);
    }
}
