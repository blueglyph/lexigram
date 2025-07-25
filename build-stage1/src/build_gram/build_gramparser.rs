// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexigram::{lexigram_lib, Gram};
use lexigram_lib::{CollectJoin, LL1, SymbolTable};
use lexigram_lib::io::CharReader;
use lexigram_lib::log::Logger;
use lexigram_lib::test_tools::replace_tagged_source;
use super::{GRAMPARSER_STAGE2_FILENAME, GRAMPARSER_GRAMMAR, GRAMPARSER_STAGE2_TAG, VERSIONS_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 13] = [
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
    ("Id",       None),            // 12
];

// [terminal_symbols]
// -------------------------------------------------------------------------

fn gramparser_source(grammar_filename: &str, verbose: bool) -> Result<String, String> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);
    let file = File::open(grammar_filename).expect(&format!("couldn't open lexicon file {grammar_filename}"));
    let reader = BufReader::new(file);
    let grammar_stream = CharReader::new(reader);
    let gram = Gram::<LL1, _>::new(symbol_table);
    let ll1 = gram.into_ll1(grammar_stream);
    let msg = ll1.get_log().get_messages().map(|s| format!("\n- {s}")).join("");
    if verbose {
        let msg = ll1.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Parser messages:\n{msg}");
        }
    }
    if !ll1.get_log().has_no_errors() {
        return Err(msg);
    }

    // - exports data to stage 2
    let ll1_src = ll1.build_tables_source_code(4);
    Ok(ll1_src)
}

pub fn write_gramparser() {
    let result_src = gramparser_source(GRAMPARSER_GRAMMAR, true)
        .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
        .unwrap();
    replace_tagged_source(GRAMPARSER_STAGE2_FILENAME, GRAMPARSER_STAGE2_TAG, &result_src)
        .expect("parser source replacement failed");
    let versions = format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
        lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
        lexigram::LEXIGRAM_PKG_NAME, lexigram::LEXIGRAM_PKG_VERSION,
        crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION);
    replace_tagged_source(GRAMPARSER_STAGE2_FILENAME, VERSIONS_TAG, &versions)
        .expect("versions replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;
        let result_src = gramparser_source(GRAMPARSER_GRAMMAR, VERBOSE)
            .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
            .unwrap();
        if !cfg!(miri) {
            let expected_src = get_tagged_source(GRAMPARSER_STAGE2_FILENAME, GRAMPARSER_STAGE2_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_gramparser();
    }
}
