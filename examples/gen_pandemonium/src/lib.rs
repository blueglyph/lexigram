// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;
use lexigram_lib::parsergen::NTValue;

static LEXICON_FILENAME: &str = "src/pandemonium.l";
static GRAMMAR_FILENAME: &str = "src/pandemonium.g";
static NT_VALUES: [NTValue; 2] = [NTValue::Default, NTValue::None];
static SOURCE_FILENAMES: [&str; 2] = ["../pandemonium/src/ll1/value.rs", "../pandemonium/src/ll1/no_value.rs"];
static TMPL_FILENAMES: [&str; 2] = ["../pandemonium/src/ll1/tpl_value.txt", "../pandemonium/src/ll1/tpl_no_value.txt"];
static LIBS: [&[&str]; 2] = [&["super::listener_types::*"], &[]];
static LEXER_TAG: &str = "pandemonium_lexer";
static PARSER_TAG: &str = "pandemonium_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source_pandemonium(action: Action) {
    for i in 0..2 {
        let (nt_value, source_filename, tpl, lib) = (&NT_VALUES[i], SOURCE_FILENAMES[i], TMPL_FILENAMES[i], LIBS[i].iter().map(|s| s.to_string()).to_vec());
        if action == Action::Generate {
            println!("\n{:-<80}\nGenerating {source_filename}", "");
        }
        let options = OptionsBuilder::new()
            .lexer(genspec!(filename: LEXICON_FILENAME), gencode!(filename: source_filename, tag: LEXER_TAG))
            .indent(LEXER_INDENT)
            .parser(genspec!(filename: GRAMMAR_FILENAME), gencode!(filename: source_filename, tag: PARSER_TAG))
            .indent(PARSER_INDENT)
            .libs(lib)
            .span_params(true)
            .set_nt_value(nt_value.clone())
            .listener_code(gencode!(filename: tpl))
            .build()
            .expect("should have no error");
        match try_gen_parser(action, options) {
            Ok(log) => {
                if action == Action::Generate {
                    println!("Code generated in {source_filename}\n{log}");
                }
                assert!(log.has_no_warnings(), "unexpected warning(s):\n{}", log.get_warnings().join("\n"));
            }
            Err(build_error) => panic!("{build_error} with {source_filename}"),
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn check_source() {
        gen_source_pandemonium(Action::Verify);
    }

    #[ignore]
    #[test]
    fn write_source() {
        gen_source_pandemonium(Action::Generate);
    }
}