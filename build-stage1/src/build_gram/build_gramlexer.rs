// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexigram::{lexigram_lib, Lexi};
use lexigram::lexi::SymbolicDfa;
use lexigram::lexigram_lib::log::{BufLog, BuildInto, LogReader, LogStatus, Logger};
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::file_utils::replace_tagged_source;
use super::{BUILD_GRAMPARSER_FILENAME, GRAMLEXER_STAGE2_FILENAME, GRAMLEXER_LEXICON, GRAMLEXER_STAGE2_TAG, GRAM_SYM_T_TAG, VERSIONS_TAG};

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Gram's lexer source code from the lexicon file.
fn gramlexer_source(lexicon_filename: &str, verbose: bool) -> Result<(BufLog, String, String), BufLog> {
    let file = File::open(lexicon_filename).expect(&format!("couldn't open lexicon file {lexicon_filename}"));
    let reader = BufReader::new(file);
    let stream = CharReader::new(reader);
    let lexi = Lexi::new(stream);
    let SymbolicDfa { dfa, symbol_table } = lexi.build_into();
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
    let mut log = dfa.give_log();
    if EXPECTED_NBR_WARNINGS != log.num_warnings() {
        log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        Err(log)
    } else {
        Ok((log, sym_src, dfa_src))
    }
}

fn get_versions() -> String {
    format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
            lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
            lexigram::LEXIGRAM_PKG_NAME, lexigram::LEXIGRAM_PKG_VERSION,
            crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION)
}

pub fn write_gramlexer() {
    let (log, result_sym, result_src) = gramlexer_source(GRAMLEXER_LEXICON, true)
        .inspect_err(|log| panic!("Failed to parse lexicon:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
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
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;

        let (log, result_sym, result_src) = gramlexer_source(GRAMLEXER_LEXICON, VERBOSE)
            .inspect_err(|log| panic!("Failed to parse lexicon:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
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
