// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexi_gram::{lexigram_lib, Gram};
use lexigram_lib::build::BuildInto;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::{LL1, SymbolTable};
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::file_utils::replace_tagged_source;
use super::{LEXIPARSER_GRAMMAR, LEXIPARSER_STAGE2_FILENAME, LEXIPARSER_STAGE2_TAG, VERSIONS_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 33] = [
    ("Arrow",    Some("->")),       // 0
    ("Colon",    Some(":")),        // 1
    ("Comma",    Some(",")),        // 2
    ("Dot",      Some(".")),        // 3
    ("Ellipsis", Some("..")),       // 4
    ("Lbracket", Some("{")),        // 5
    ("Lparen",   Some("(")),        // 6
    ("Negate",   Some("~")),        // 7
    ("Minus",    Some("-")),        // 8
    ("Plus",     Some("+")),        // 9
    ("Or",       Some("|")),        // 10
    ("Question", Some("?")),        // 11
    ("Rbracket", Some("}")),        // 12
    ("Rparen",   Some(")")),        // 13
    ("Semicolon",Some(";")),        // 14
    ("Star",     Some("*")),        // 15
    ("Channels", Some("channels")), // 16
    ("Fragment", Some("fragment")), // 17
    ("Lexicon",  Some("lexicon")),  // 18
    ("Mode",     Some("mode")),     // 19
    ("Pop",      Some("pop")),      // 20
    ("Push",     Some("push")),     // 21
    ("More",     Some("more")),     // 22
    ("Skip",     Some("skip")),     // 23
    ("Type",     Some("type")),     // 24
    ("Channel",  Some("channel")),  // 25
    ("Id",       None),             // 26
    ("CharLit",  None),             // 27
    ("StrLit",   None),             // 28
    ("FixedSet", None),             // 29
    ("LSbracket",Some("[")),        // 30
    ("RSbracket",Some("]")),        // 31
    ("SetChar",  None),             // 32
];

// [terminal_symbols]
// -------------------------------------------------------------------------

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn lexiparser_source(grammar_filename: &str, _indent: usize, _verbose: bool) -> Result<(BufLog, String), BufLog> {
    // - builds initial symbol table from symbols above extracted by lexi (in build_lexilexer, which updated this source file)
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);
    let file = File::open(grammar_filename).expect(&format!("couldn't open lexicon file {grammar_filename}"));
    let reader = BufReader::new(file);
    let grammar_stream = CharReader::new(reader);

    // - parses the grammar
    let gram = Gram::new(symbol_table, grammar_stream);
    let ll1: ProdRuleSet<LL1> = gram.build_into();
    if !ll1.get_log().has_no_errors() || ll1.get_log().num_warnings() != EXPECTED_NBR_WARNINGS {
        let mut log = ll1.give_log();
        if log.num_warnings() != EXPECTED_NBR_WARNINGS {
            log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        }
        return Err(log);
    }

    // - exports data to stage 2
    let ll1_src = ll1.gen_tables_source_code(4);
    let log = ll1.give_log();
    Ok((log, ll1_src))
}

fn get_versions() -> String {
    format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
            lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
            lexi_gram::LEXIGRAM_PKG_NAME, lexi_gram::LEXIGRAM_PKG_VERSION,
            crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION)
}

pub fn write_lexiparser() {
    let (log, result_src) = lexiparser_source(LEXIPARSER_GRAMMAR, 0, true)
        .inspect_err(|log| panic!("Failed to parse grammar:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(LEXIPARSER_STAGE2_FILENAME, LEXIPARSER_STAGE2_TAG, &result_src)
        .expect("parser source replacement failed");
    let versions = get_versions();
    replace_tagged_source(LEXIPARSER_STAGE2_FILENAME, VERSIONS_TAG, &versions)
        .expect("versions replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;
        let (log, result_src) = lexiparser_source(LEXIPARSER_GRAMMAR, 0, VERBOSE)
            .inspect_err(|log| panic!("Failed to parse grammar:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
            let expected_src = get_tagged_source(LEXIPARSER_STAGE2_FILENAME, LEXIPARSER_STAGE2_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
            let result_ver = get_versions();
            let expected_ver = get_tagged_source(LEXIPARSER_STAGE2_FILENAME, VERSIONS_TAG).unwrap_or(String::new());
            assert_eq!(result_ver, expected_ver);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_lexiparser();
    }
}
