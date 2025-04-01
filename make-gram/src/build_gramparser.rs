// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

use lexigram::grammar::GrTreeExt;
use lexigram::{gnode, CollectJoin, General, LL1};
use lexigram::grammar::{print_production_rules, ProdRuleSet, RuleTreeSet, VarId};
use lexigram::log::Logger;
use lexigram::parsergen::ParserGen;
use lexigram::symbol_table::SymbolTable;
use lexigram::test_tools::replace_tagged_source;
use crate::*;

// -------------------------------------------------------------------------
// [terminal_symbols]

#[repr(u16)]
enum T {
}

pub const TERMINALS: [(&str, Option<&str>); 0] = [
];

// [terminal_symbols]
// -------------------------------------------------------------------------

// -------------------------------------------------------------------------
// [non_terminal_symbols]

#[repr(u16)]
enum NT {
    File = 0,           // 0
}

const NON_TERMINALS: [&str; 1] = [
    "file",             // 0
];

// [non_terminal_symbols]
// -------------------------------------------------------------------------

pub(crate) fn build_rts() -> RuleTreeSet<General> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_non_terminals(NON_TERMINALS);
    symbol_table.extend_terminals(TERMINALS);
    let mut rules = RuleTreeSet::new();
    rules.set_symbol_table(symbol_table);

    // grammar GramParser;
    //
    // file: header rule+ SymEOF?;
    //
    let tree = rules.get_tree_mut(NT::File as VarId);
    let cc = tree.add_root(gnode!(&));
    todo!("parser rules");

    //
    // header:
    //     Grammar Id Semicolon
    // ;
    //
    // rule:
    //     Id Colon prod Semicolon
    // ;
    //
    // prod:
    //     prodFactor
    // |   prod Or prodFactor
    // ;
    //
    // prodFactor:
    //     prodTerm*
    // ;
    //
    // prodTerm:
    //     termItem (Plus | Star | Question)?
    // ;
    //
    // termItem:
    //     Id
    // |   Lparen prod Rparen
    // |   Lform
    // |   Rform
    // ;

    rules
}

fn gramparser_source(indent: usize, verbose: bool) -> String {
    let mut rts = build_rts();
    rts.set_start(0);
    if verbose {
        println!("rules, num_nt = {}, NT symbols: {}", rts.get_trees_iter().count(), rts.get_symbol_table().unwrap().get_num_nt());
        let printable = std::collections::BTreeMap::from_iter(rts.get_trees_iter().map(|(id, t)| (id, format!("{}", t.to_str(None, None)))));
        for (id, s) in printable {
            println!("{id} => {s}");
        }
    }
    let rules = ProdRuleSet::from(rts);
    if !rules.get_log().is_empty() {
        println!("messages PRS<General>: {}", rules.get_log().get_messages().map(|l| format!("\n  {l:?}")).join(""));
    }
    if verbose {
        let st_num_nt = rules.get_symbol_table().unwrap().get_num_nt();
        println!("rules, num_nt = {}, NT symbols: {}", rules.get_num_nt(), st_num_nt);
        println!("- {}", (0..st_num_nt).map(|i| rules.get_symbol_table().unwrap().get_nt_name(i as VarId)).join(", "));
        print_production_rules(&rules, true);
        let msg = rules.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Messages:\n{msg}");
        }
    }
    assert_eq!(rules.get_log().num_errors(), 0);
    let ll1 = ProdRuleSet::<LL1>::from(rules);
    if !ll1.get_log().is_empty() {
        println!("messages PRS<LL1>: {}", ll1.get_log().get_messages().map(|l| format!("\n  {l:?}")).join(""));
    }
    if verbose {
        println!("LL1, num_nt = {}, NT symbols: {}", ll1.get_num_nt(), ll1.get_symbol_table().unwrap().get_num_nt());
        print_production_rules(&ll1, true);
        let msg = ll1.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Messages:\n{msg}");
        }
    }
    assert_eq!(ll1.get_log().num_errors(), 0);
    let mut builder = ParserGen::from_rules(ll1, "GramParser".to_string());
    for v in 0..builder.get_symbol_table().unwrap().get_num_nt() as VarId {
        // print!("- {}: ", Symbol::NT(v).to_str(builder.get_symbol_table()));
        if builder.get_nt_parent(v).is_none() {
            builder.set_nt_has_value(v, true);
            // println!("has no parent, has value");
        } else {
            // println!("has parents, has no value");
        }
    }
    builder.add_lib("super::gramparser_types::*");
    builder.build_source_code(indent, true)
}

fn write_gramparser() {
    let result_src = gramparser_source(4, false);
    replace_tagged_source(GRAMPARSER_FILENAME, GRAMPARSER_TAG, &result_src)
        .expect("parser source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram::test_tools::{get_tagged_source, replace_tagged_source};
    use crate::*;
    use super::*;

    #[test]
    fn test_source() {
        let result_src = gramparser_source(4, false);
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