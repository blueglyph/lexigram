// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the typedef parser

#![cfg(test)]

use lexi_gram::lexigram_lib;

use lexi_gram::{gencode, genspec};
use lexi_gram::gen_parser::try_gen_parser;
use lexi_gram::lexigram_lib::parsergen::ParserType;
use lexi_gram::options::{Action, OptionsBuilder};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::LogStatus;
use lexigram_lib::parsergen::NTValue;

static LEXICON_FILENAME: &str = "src/typedef.lg";
static LEXICON_TAGS: [&str; 3] = ["typedef_type_lexicon", "typedef_id_type_lexicon", "typedef_match_lexicon"];
static GRAMMAR_FILENAME: &str = "src/typedef.lg";
static GRAMMAR_TAGS: [&str; 3] = ["typedef_type_grammar", "typedef_id_type_grammar", "typedef_match_grammar"];
static SOURCE_LL1_FILENAMES: [&str; 3] = [
    "../typedef/src/ll1/typedef_type.rs",
    "../typedef/src/ll1/typedef_id_type.rs",
    "../typedef/src/ll1/typedef_match.rs"];
static SOURCE_LALR_FILENAMES: [&str; 3] = [
    "../typedef/src/lalr/typedef_type.rs",
    "../typedef/src/lalr/typedef_id_type.rs",
    "../typedef/src/lalr/typedef_match.rs"];
static TPL_TAGS: [&str; 3] = ["typedef", "typedef-id", "typedef-match"];
static TPL_FILENAMES: [&str; 2] = ["../typedef/src/ll1/tpl.txt", "../typedef/src/lalr/tpl.txt"];
static LEXER_TAGS: [&str; 3] = ["typedef_type_lexer", "typedef_id_type_lexer", "typedef_match_lexer"];
static PARSER_TAGS: [&str; 3] = ["typedef_type_parser", "typedef_id_type_parser", "typedef_match_parser"];
static LIBS: [&str; 3] = ["super::listener_type_types::*", "super::listener_id_type_types::*", "super::listener_match_types::*"];
const LEXER_INDENT: usize = 4;
const PARSER_INDENT: usize = 4;

// -------------------------------------------------------------------------

fn gen_source_typedef(action: Action, parser_type: ParserType) {
    let (source_filenames, tpl_filename) = match parser_type {
        ParserType::LL1 => (SOURCE_LL1_FILENAMES, TPL_FILENAMES[0]),
        ParserType::LALR => (SOURCE_LALR_FILENAMES, TPL_FILENAMES[1]),
    };
    for i in 0..LEXICON_TAGS.len() {
        let (lexicon_tag, grammar_tag) = (LEXICON_TAGS[i], GRAMMAR_TAGS[i]);
        let (lexer_tag, parser_tag, source_filename) = (LEXER_TAGS[i], PARSER_TAGS[i], source_filenames[i]);
        let options = OptionsBuilder::new()
            .lexer(genspec!(filename: LEXICON_FILENAME, tag: lexicon_tag), gencode!(filename: source_filename, tag: lexer_tag))
            .indent(LEXER_INDENT)
            .parser(genspec!(filename: GRAMMAR_FILENAME, tag: grammar_tag), gencode!(filename: source_filename, tag: parser_tag))
            .indent(PARSER_INDENT)
            .listener_code(gencode!(filename: tpl_filename, tag: TPL_TAGS[i]))
            .indent(PARSER_INDENT)
            .libs([LIBS[i]])
            .span_params(true)
            .set_nt_value(NTValue::SetNames(vec![
                NTValue::PARENTS.to_string(),
                "id_i".to_string()
            ]))
            .token_enums(true)
            .parser_type(parser_type)
            .build()
            .expect("should have no error");
        match try_gen_parser(action, options) {
            Ok(log) => {
                if action == Action::Generate {
                    println!("Code generated in {source_filename} [{lexer_tag}] / [{parser_tag}]\n{log}");
                }
                assert!(log.has_no_warnings(), "unexpected warning(s):\n{}", log.get_warnings().join("\n"));
            }
            Err(build_error) => panic!("[{lexicon_tag}] / [{grammar_tag}]: {build_error}"),
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn check_source_ll1() {
        gen_source_typedef(Action::Verify, ParserType::LL1);
    }

    #[test]
    fn check_source_lalr() {
        gen_source_typedef(Action::Verify, ParserType::LALR);
    }

    #[ignore]
    #[test]
    fn write_source_ll1() {
        gen_source_typedef(Action::Generate, ParserType::LL1);
    }

    #[ignore]
    #[test]
    fn write_source_lalr() {
        gen_source_typedef(Action::Generate, ParserType::LALR);
    }
}
