// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexigram::{lexigram_lib, Lexi};
use lexigram::lexi::SymbolicDfa;
use lexigram::lexigram_lib::log::{BuildInto, LogReader, LogStatus};
use lexigram_lib::io::CharReader;
use lexigram_lib::test_tools::replace_tagged_source;
use super::{BUILD_GRAMPARSER_FILENAME, GRAMLEXER_STAGE2_FILENAME, GRAMLEXER_LEXICON, GRAMLEXER_STAGE2_TAG, GRAM_SYM_T_TAG, VERSIONS_TAG};

/// Generates Gram's lexer source code from the lexicon file.
fn gramlexer_source(lexicon_filename: &str, verbose: bool) -> Result<(String, String), impl LogStatus> {
    let file = File::open(lexicon_filename).expect(&format!("couldn't open lexicon file {lexicon_filename}"));
    let reader = BufReader::new(file);
    let stream = CharReader::new(reader);
    let lexi = Lexi::new(stream);
    let SymbolicDfa { dfa, symbol_table } = lexi.build_into();
    if verbose {
        let msg = dfa.get_log().get_messages_str();
        if !msg.is_empty() {
            println!("Parser messages:\n{msg}");
        }
    }
    if !dfa.get_log().has_no_errors() {
        return Err(dfa.give_log());
    }
    if verbose {
        println!("Dfa:");
        dfa.print(4);
    }

    // - exports data to stage 2
    let sym_src = symbol_table.gen_source_code_t(0, false, true);
    let dfa_src = dfa.gen_tables_source_code(4);

    Ok((sym_src, dfa_src))
}

fn get_versions() -> String {
    format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
            lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
            lexigram::LEXIGRAM_PKG_NAME, lexigram::LEXIGRAM_PKG_VERSION,
            crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION)
}

pub fn write_gramlexer() {
    let (result_sym, result_src) = gramlexer_source(GRAMLEXER_LEXICON, true)
        .inspect_err(|e| eprintln!("Failed to parse lexicon:\n{}", e.get_messages_str()))
        .unwrap();
    replace_tagged_source(BUILD_GRAMPARSER_FILENAME, GRAM_SYM_T_TAG, &result_sym)
        .expect("parser symbol replacement failed");
    replace_tagged_source(GRAMLEXER_STAGE2_FILENAME, GRAM_SYM_T_TAG, &result_sym)
        .expect("parser symbol replacement failed");
    replace_tagged_source(GRAMLEXER_STAGE2_FILENAME, GRAMLEXER_STAGE2_TAG, &result_src)
        .expect("lexer source replacement failed");
    let versions = get_versions();
    replace_tagged_source(GRAMLEXER_STAGE2_FILENAME, VERSIONS_TAG, &versions)
        .expect("versions replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;

        let (result_sym, result_src) = gramlexer_source(GRAMLEXER_LEXICON, VERBOSE)
            .inspect_err(|e| eprintln!("Failed to parse lexicon: {e:?}"))
            .unwrap();
        if !cfg!(miri) {
            let expected_sym1 = get_tagged_source(BUILD_GRAMPARSER_FILENAME, GRAM_SYM_T_TAG).unwrap_or(String::new());
            let expected_sym2 = get_tagged_source(GRAMLEXER_STAGE2_FILENAME, GRAM_SYM_T_TAG).unwrap_or(String::new());
            assert_eq!(expected_sym1, expected_sym2, "T symbols are different in {BUILD_GRAMPARSER_FILENAME} and {GRAMLEXER_STAGE2_FILENAME}");
            let expected_src = get_tagged_source(GRAMLEXER_STAGE2_FILENAME, GRAMLEXER_STAGE2_TAG).unwrap_or(String::new());
            assert_eq!(result_sym, expected_sym1);
            assert_eq!(result_src, expected_src);
            let result_ver = get_versions();
            let expected_ver = get_tagged_source(GRAMLEXER_STAGE2_FILENAME, VERSIONS_TAG).unwrap_or(String::new());
            assert_eq!(result_ver, expected_ver);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_gramlexer();
    }
}
