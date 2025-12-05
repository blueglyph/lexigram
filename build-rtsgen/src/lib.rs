// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexigram::{lexigram_lib, genspec, gencode};
use lexigram::gen_parser::{try_gen_parser, GenParserError};
use lexigram_lib::log::{BufLog, LogStatus};
use lexigram::options::{Action, OptionsBuilder};

static LEXICON_FILENAME: &str = "src/rtsgen.l";
static GRAMMAR_FILENAME: &str = "src/rtsgen.g";
static SOURCE_FILENAME: &str = "../src/rtsgen/mod.rs";
static LEXER_TAG: &str = "rtsgen_lexer";
static PARSER_TAG: &str = "rtsgen_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source(action: Action) -> Result<BufLog, GenParserError> {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .build();
    try_gen_parser(action, options)
}

mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn test_gen_source() {
        match gen_source(Action::Generate) {
            Ok(log) => {
                println!("Code successfully generated in {SOURCE_FILENAME}");
                println!("\n{} warning(s), {} note(s):\n", log.num_warnings(), log.num_notes());
                println!("{log}")
            },
            Err(gen_error) => println!("{gen_error}"),
        }
    }

    #[test]
    fn test_check_source() {
        match gen_source(Action::Verify) {
            Ok(log) => println!("Code successfully generated in {SOURCE_FILENAME}\n{log}"),
            Err(gen_error) => println!("{gen_error}"),
       }
    }
}