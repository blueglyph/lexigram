// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the microcalc parser

use lexigram::{gencode, genspec};
use lexigram::gen_parser::{try_gen_parser, GenParserError};
use lexigram::options::{Action, OptionsBuilder};
use lexigram_lib::log::{BufLog};

static LEXICON_FILENAME: &str = "examples/microcalc.l";
static GRAMMAR_FILENAME: &str = "examples/microcalc.g";
static SOURCE_FILENAME: &str = "examples/microcalc.rs";
static LEXER_TAG: &str = "microcalc_lexer";
static PARSER_TAG: &str = "microcalc_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn main() {
    match gen_microcalc_source(Action::Generate) {
        Ok(log) => println!("Code successfully generated in {SOURCE_FILENAME}\n{log}"),
        Err(build_error) => println!("{build_error}"),
    }
}

fn gen_microcalc_source(action: Action) -> Result<BufLog, GenParserError> {
    let options = OptionsBuilder::new()
        .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
        .indent(LEXER_INDENT)
        .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
        .indent(PARSER_INDENT)
        .extra_libs(["super::listener_types::*"])
        .build()
        .expect("should have no error");
    try_gen_parser(action, options)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn test_gen_source() {
        main();
    }

    #[test]
    fn test_check_source() {
        match gen_microcalc_source(Action::Verify) {
            Ok(log) => println!("Code successfully generated in {SOURCE_FILENAME}\n{log}"),
            Err(gen_error) => println!("{gen_error}"),
       }
    }
}