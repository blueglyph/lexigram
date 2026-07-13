// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::lexigram_lib::parsergen::ParserType;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;
use lexigram_lib::parsergen::NTValue;

static LEXICON_LL1_FILENAME: &str = "src/ll1/pandemonium.l";
static GRAMMAR_LL1_FILENAME: &str = "src/ll1/pandemonium.g";
static LEXICON_LR_FILENAME: &str = "src/lalr/pandemonium.l";
static GRAMMAR_LR_FILENAME: &str = "src/lalr/pandemonium.g";
static NT_VALUES: [NTValue; 2] = [NTValue::Default, NTValue::None];
static SOURCE_LL1_FILENAMES: [&str; 2] = ["../pandemonium/src/ll1/value.rs", "../pandemonium/src/ll1/no_value.rs"];
static TMPL_LL1_FILENAMES: [&str; 2] = ["../pandemonium/src/ll1/tpl_value.txt", "../pandemonium/src/ll1/tpl_no_value.txt"];
static SOURCE_LALR_FILENAMES: [&str; 2] = ["../pandemonium/src/lalr/value.rs", "../pandemonium/src/lalr/no_value.rs"];
static TMPL_LALR_FILENAMES: [&str; 2] = ["../pandemonium/src/lalr/tpl_value.txt", "../pandemonium/src/lalr/tpl_no_value.txt"];
static LIBS: [&[&str]; 2] = [&["super::listener_types::*"], &[]];
static LEXER_TAG: &str = "pandemonium_lexer";
static PARSER_TAG: &str = "pandemonium_parser";
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source_pandemonium(action: Action, parser_type: ParserType) {
    let (lexicon, grammar, src, tmpl) = match parser_type {
        ParserType::LL1 => (LEXICON_LL1_FILENAME, GRAMMAR_LL1_FILENAME, SOURCE_LL1_FILENAMES, TMPL_LL1_FILENAMES),
        ParserType::LALR => (LEXICON_LR_FILENAME, GRAMMAR_LR_FILENAME, SOURCE_LALR_FILENAMES, TMPL_LALR_FILENAMES),
    };
    for i in 0..2 {
        let (nt_value, source_filename, tpl, lib) = (&NT_VALUES[i], src[i], tmpl[i], LIBS[i].iter().map(|s| s.to_string()).to_vec());
        if action == Action::Generate {
            println!("\n{:-<80}\nGenerating {source_filename}", "");
        }
        let options = OptionsBuilder::new()
            .lexer(genspec!(filename: lexicon), gencode!(filename: source_filename, tag: LEXER_TAG))
            .indent(LEXER_INDENT)
            .parser(genspec!(filename: grammar), gencode!(filename: source_filename, tag: PARSER_TAG))
            .indent(PARSER_INDENT)
            .libs(lib)
            .span_params(true)
            .set_nt_value(nt_value.clone())
            .listener_code(gencode!(filename: tpl))
            .parser_type(parser_type)
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
    fn check_source_ll1() {
        gen_source_pandemonium(Action::Verify, ParserType::LL1);
    }

    #[test]
    fn check_source_lalr() {
        gen_source_pandemonium(Action::Verify, ParserType::LALR);
    }

    #[ignore]
    #[test]
    fn write_source_ll1() {
        gen_source_pandemonium(Action::Generate, ParserType::LL1);
    }

    #[ignore]
    #[test]
    fn write_source_lalr() {
        gen_source_pandemonium(Action::Generate, ParserType::LALR);
    }
}