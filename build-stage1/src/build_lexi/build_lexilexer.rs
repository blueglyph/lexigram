// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexi_gram::{lexigram_lib, Lexi};
use lexigram_lib::build::BuildInto;
use lexi_gram::lexi::SymbolicDfa;
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::file_utils::replace_tagged_source;
use super::{BUILD_LEXIPARSER_FILENAME, LEXILEXER_STAGE2_FILENAME, LEXILEXER_LEXICON, LEXILEXER_STAGE2_TAG, LEXI_SYM_T_TAG, VERSIONS_TAG};

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's lexer source code from the lexicon file.
fn lexilexer_source(lexicon_filename: &str, verbose: bool) -> Result<(BufLog, String, String), BufLog> {
    let file = File::open(lexicon_filename).expect(&format!("couldn't open lexicon file {lexicon_filename}"));
    let reader = BufReader::new(file);
    let stream = CharReader::new(reader);
    let lexi = Lexi::new(stream);
    let SymbolicDfa { dfa, symbol_table } = lexi.build_into();
    if !dfa.get_log().has_no_errors() || dfa.get_log().num_warnings() != EXPECTED_NBR_WARNINGS {
        let mut log = dfa.give_log();
        if log.num_warnings() != EXPECTED_NBR_WARNINGS {
            log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        }
        return Err(log);
    }
    if verbose {
        println!("Dfa:");
        dfa.print(4);
    }

    // - exports data to stage 2
    let sym_src = symbol_table.gen_source_code_t(0, false, true);
    let dfa_src = dfa.gen_tables_source_code(4);
    Ok((dfa.give_log(), sym_src, dfa_src))
}

fn get_versions() -> String {
    format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
            lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
            lexi_gram::LEXIGRAM_PKG_NAME, lexi_gram::LEXIGRAM_PKG_VERSION,
            crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION)
}

pub fn write_lexilexer() {
    let (log, result_sym, result_src) = lexilexer_source(LEXILEXER_LEXICON, true)
        .inspect_err(|log| panic!("Failed to parse lexicon:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(BUILD_LEXIPARSER_FILENAME, LEXI_SYM_T_TAG, &result_sym)
        .expect("parser symbol replacement failed");
    replace_tagged_source(LEXILEXER_STAGE2_FILENAME, LEXI_SYM_T_TAG, &result_sym)
        .expect("parser symbol replacement failed");
    replace_tagged_source(LEXILEXER_STAGE2_FILENAME, LEXILEXER_STAGE2_TAG, &result_src)
        .expect("lexer source replacement failed");
    let versions = get_versions();
    replace_tagged_source(LEXILEXER_STAGE2_FILENAME, VERSIONS_TAG, &versions)
        .expect("versions replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;

        let (log, result_sym, result_src) = lexilexer_source(LEXILEXER_LEXICON, VERBOSE)
            .inspect_err(|log| panic!("Failed to parse lexicon:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
            let expected_sym1 = get_tagged_source(BUILD_LEXIPARSER_FILENAME, LEXI_SYM_T_TAG).unwrap_or(String::new());
            let expected_sym2 = get_tagged_source(LEXILEXER_STAGE2_FILENAME, LEXI_SYM_T_TAG).unwrap_or(String::new());
            assert_eq!(expected_sym1, expected_sym2, "T symbols are different in {BUILD_LEXIPARSER_FILENAME} and {LEXILEXER_STAGE2_FILENAME}");
            let expected_src = get_tagged_source(LEXILEXER_STAGE2_FILENAME, LEXILEXER_STAGE2_TAG).unwrap_or(String::new());
            assert_eq!(result_sym, expected_sym1);
            assert_eq!(result_src, expected_src);
            let result_ver = get_versions();
            let expected_ver = get_tagged_source(LEXILEXER_STAGE2_FILENAME, VERSIONS_TAG).unwrap_or(String::new());
            assert_eq!(result_ver, expected_ver);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_lexilexer();
    }
}
