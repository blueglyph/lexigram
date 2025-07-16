// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use iter_index::IndexerIterator;
use std::fs::File;
use std::io::BufReader;
use gram::gram::Gram;
use lexigram::grammar::{print_ll1_table, ruleflag, Symbol};
use lexigram::{CollectJoin, LL1, SymbolTable};
use lexigram::grammar::VarId;
use lexigram::io::CharReader;
use lexigram::log::Logger;
use lexigram::parsergen::ParserGen;
use lexigram::test_tools::replace_tagged_source;
use crate::{GRAMPARSER_FILENAME, GRAMPARSER_GRAMMAR, GRAMPARSER_TAG};

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

fn set_nt_has_value(builder: &mut ParserGen) {
    for v in 0..builder.get_symbol_table().unwrap().get_num_nt() as VarId {
        // print!("- {}: ", Symbol::NT(v).to_str(builder.get_symbol_table()));
        if builder.get_nt_parent(v).is_none() {
            builder.set_nt_has_value(v, true);
            // println!("has no parent, has value");
        } else {
            // println!("has parents, has no value");
        }
    }
}

fn gramparser_source(grammar_filename: &str, indent: usize, verbose: bool) -> Result<String, String> {
    pub fn print_flags(builder: &ParserGen, indent: usize) {
        let tbl = builder.get_symbol_table();
        let prefix = format!("{:width$}//", "", width = indent);
        let nt_flags = builder.get_parsing_table().flags.iter().index().filter_map(|(nt, &f)|
            if f != 0 { Some(format!("{prefix}  - {}: {} ({})", Symbol::NT(nt).to_str(tbl), ruleflag::to_string(f).join(" | "), f)) } else { None }
        ).join("\n");
        let parents = builder.get_parsing_table().parent.iter().index().filter_map(|(c, &par)|
            if let Some(p) = par { Some(format!("{prefix}  - {} -> {}", Symbol::NT(c).to_str(tbl), Symbol::NT(p).to_str(tbl))) } else { None }
        ).join("\n");
        println!("{prefix} NT flags:\n{}", if nt_flags.is_empty() { format!("{prefix}  - (nothing)") } else { nt_flags });
        println!("{prefix} parents:\n{}", if parents.is_empty() { format!("{prefix}  - (nothing)") } else { parents });
    }

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
    set_nt_has_value(&mut builder);
    builder.add_lib("super::gramparser_types::*");
    Ok(builder.build_source_code(indent, true))
}

pub fn write_gramparser() {
    let result_src = gramparser_source(GRAMPARSER_GRAMMAR, 4, true)
        .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
        .unwrap();
    replace_tagged_source(GRAMPARSER_FILENAME, GRAMPARSER_TAG, &result_src)
        .expect("parser source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        let result_src = gramparser_source(GRAMPARSER_GRAMMAR, 4, false)
            .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
            .unwrap();
        if !cfg!(miri) {
            let expected_src = get_tagged_source(GRAMPARSER_FILENAME, GRAMPARSER_TAG).unwrap_or(String::new());
            assert_eq!(result_src, expected_src);
        }
    }

    #[ignore]
    #[test]
    fn write_source() {
        write_gramparser();
    }
}