// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fs::File;
use std::io::BufReader;
use gram::gram::Gram;
use lexigram::grammar::{print_ll1_table};
use lexigram::{CollectJoin, LL1, SymbolTable};
use lexigram::io::CharReader;
use lexigram::log::Logger;
use lexigram::parsergen::{print_flags, ParserGen};
use lexigram::test_tools::replace_tagged_source;
use crate::{LEXIPARSER_GRAMMAR, LEXIPARSER_FILENAME, LEXIPARSER_TAG};

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

fn lexiparser_source(grammar_filename: &str, indent: usize, verbose: bool) -> Result<String, String> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(TERMINALS);
    let file = File::open(grammar_filename).expect(&format!("couldn't open lexicon file {grammar_filename}"));
    let reader = BufReader::new(file);
    let grammar_stream = CharReader::new(reader);
    let gram = Gram::<LL1, _>::new(symbol_table);
    let (ll1, name) = gram.build_ll1(grammar_stream);
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
    let mut builder = ParserGen::from_rules(ll1, name.clone());
    let msg = builder.get_log().get_messages().map(|s| format!("\n- {s}")).join("");
    if verbose {
        print_flags(&builder, 4);
        println!("Parsing table of grammar '{name}':");
        print_ll1_table(builder.get_symbol_table(), builder.get_parsing_table(), 4);
        if !builder.get_log().is_empty() {
            println!("Messages:{msg}");
        }
    }
    if !builder.get_log().has_no_errors() {
        return Err(msg);
    }
    builder.set_parents_have_value();
    builder.add_lib("super::lexiparser_types::*");
    Ok(builder.build_source_code(indent, true))
}

pub fn write_lexiparser() {
    let result_src = lexiparser_source(LEXIPARSER_GRAMMAR, 4, true)
        .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
        .unwrap();
    replace_tagged_source(LEXIPARSER_FILENAME, LEXIPARSER_TAG, &result_src)
        .expect("parser source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        let result_src = lexiparser_source(LEXIPARSER_GRAMMAR, 4, false)
            .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
            .unwrap();
        if !cfg!(miri) {
            let expected_src = get_tagged_source(LEXIPARSER_FILENAME, LEXIPARSER_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_lexiparser();
    }
}