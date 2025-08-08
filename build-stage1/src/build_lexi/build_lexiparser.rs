// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use lexigram::{lexigram_lib, Gram};
use lexigram::lexigram_lib::grammar::ProdRuleSet;
use lexigram::lexigram_lib::log::{BufLog, LogReader, LogStatus};
use lexigram_lib::{CollectJoin, LL1, SymbolTable};
use lexigram_lib::io::CharReader;
use lexigram_lib::test_tools::replace_tagged_source;
use super::{LEXIPARSER_GRAMMAR, LEXIPARSER_STAGE2_FILENAME, LEXIPARSER_STAGE2_TAG, VERSIONS_TAG};

// -------------------------------------------------------------------------
// [terminal_symbols]

static TERMINALS: [(&str, Option<&str>); 34] = [
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
    ("SymEof",   Some("EOF")),      // 26
    ("Id",       None),             // 27
    ("CharLit",  None),             // 28
    ("StrLit",   None),             // 29
    ("FixedSet", None),             // 30
    ("LSbracket",Some("[")),        // 31
    ("RSbracket",Some("]")),        // 32
    ("SetChar",  None),             // 33
];

// [terminal_symbols]
// -------------------------------------------------------------------------

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn lexiparser_source(grammar_filename: &str, _indent: usize, verbose: bool) -> Result<String, BufLog> {
    // - builds initial symbol table from symbols above extracted by lexi (in build_lexilexer, which updated this source file)
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);
    let file = File::open(grammar_filename).expect(&format!("couldn't open lexicon file {grammar_filename}"));
    let reader = BufReader::new(file);
    let grammar_stream = CharReader::new(reader);

    // - parses the grammar
    let gram = Gram::new(symbol_table, grammar_stream);
    let ll1: ProdRuleSet<LL1> = gram.into();
    if verbose {
        let msg = ll1.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Parser messages:\n{msg}");
        }
    }
    if !ll1.get_log().has_no_errors() {
        return Err(ll1.give_log());
    }

    // - exports data to stage 2
    let ll1_src = ll1.build_tables_source_code(4);
    Ok(ll1_src)
}

pub fn write_lexiparser() {
    let result_src = lexiparser_source(LEXIPARSER_GRAMMAR, 0, true)
        .inspect_err(|log| eprintln!("Failed to parse grammar: {}", log.get_messages_str()))
        .unwrap();
    replace_tagged_source(LEXIPARSER_STAGE2_FILENAME, LEXIPARSER_STAGE2_TAG, &result_src)
        .expect("parser source replacement failed");
    let versions = format!("    // {}: {}\n    // {}: {}\n    // {}: {}\n",
        lexigram_lib::LIB_PKG_NAME, lexigram_lib::LIB_PKG_VERSION,
        lexigram::LEXIGRAM_PKG_NAME, lexigram::LEXIGRAM_PKG_VERSION,
        crate::STAGE1_PKG_NAME, crate::STAGE1_PKG_VERSION);
    replace_tagged_source(LEXIPARSER_STAGE2_FILENAME, VERSIONS_TAG, &versions)
        .expect("versions replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;
        let result_src = lexiparser_source(LEXIPARSER_GRAMMAR, 0, VERBOSE)
            .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
            .unwrap();
        if !cfg!(miri) {
            let expected_src = get_tagged_source(LEXIPARSER_STAGE2_FILENAME, LEXIPARSER_STAGE2_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_lexiparser();
    }
}
