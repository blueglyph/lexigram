// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexigram::{lexigram_lib, Gram};
use lexigram::lexigram_lib::grammar::ProdRuleSet;
use lexigram_core::log::{BufLog, BuildInto, LogReader, LogStatus, Logger};
use lexigram_lib::{SymbolTable, LL1};
use lexigram_lib::build::{BuildError, HasBuildErrorSource};
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::file_utils::replace_tagged_source;
use super::{GRAMPARSER_GRAMMAR, GRAMPARSER_STAGE2_FILENAME, GRAMPARSER_STAGE2_TAG, VERSIONS_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 14] = [
    ("Colon",    Some(":")),       // 0
    ("Lparen",   Some("(")),       // 1
    ("Or",       Some("|")),       // 2
    ("Plus",     Some("+")),       // 3
    ("Question", Some("?")),       // 4
    ("Rparen",   Some(")")),       // 5
    ("Semicolon",Some(";")),       // 6
    ("Star",     Some("*")),       // 7
    ("Grammar",  Some("grammar")), // 8
    ("SymEof",   Some("EOF")),     // 9
    ("Lform",    None),            // 10
    ("Rform",    Some("<R>")),     // 11
    ("Pform",    Some("<P>")),     // 12
    ("Id",       None),            // 13
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

fn gramparser_source(grammar_filename: &str, _verbose: bool) -> Result<(BufLog, String), BuildError> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);
    let file = File::open(grammar_filename).expect(&format!("couldn't open lexicon file {grammar_filename}"));
    let reader = BufReader::new(file);
    let grammar_stream = CharReader::new(reader);
    let gram = Gram::new(symbol_table, grammar_stream);
    let ll1: ProdRuleSet<LL1> = gram.build_into();
    if !ll1.get_log().has_no_errors() || ll1.get_log().num_warnings() != EXPECTED_NBR_WARNINGS {
        let mut log = ll1.give_log();
        if log.num_warnings() != EXPECTED_NBR_WARNINGS {
            log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        }
        return Err(BuildError::new(log, ProdRuleSet::<LL1>::get_build_error_source()));
    }

    // - exports data to stage 2
    let ll1_src = ll1.gen_tables_source_code(4);
    let log = ll1.give_log();
    Ok((log, ll1_src))
}

fn get_versions() -> String {
    format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
            lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
            lexigram::LEXIGRAM_PKG_NAME, lexigram::LEXIGRAM_PKG_VERSION,
            crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION)
}

pub fn write_gramparser() {
    let (log, result_src) = gramparser_source(GRAMPARSER_GRAMMAR, true)
        .inspect_err(|e| panic!("Failed to parse grammar:\n{e}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(GRAMPARSER_STAGE2_FILENAME, GRAMPARSER_STAGE2_TAG, &result_src)
        .expect("parser source replacement failed");
    let versions = get_versions();
    replace_tagged_source(GRAMPARSER_STAGE2_FILENAME, VERSIONS_TAG, &versions)
        .expect("versions replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;
        let (log, result_src) = gramparser_source(GRAMPARSER_GRAMMAR, VERBOSE)
            .inspect_err(|e| panic!("Failed to parse grammar:\n{e}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
            let expected_src = get_tagged_source(GRAMPARSER_STAGE2_FILENAME, GRAMPARSER_STAGE2_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
            let result_ver = get_versions();
            let expected_ver = get_tagged_source(GRAMPARSER_STAGE2_FILENAME, VERSIONS_TAG).unwrap_or(String::new());
            assert_eq!(result_ver, expected_ver);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_gramparser();
    }
}
