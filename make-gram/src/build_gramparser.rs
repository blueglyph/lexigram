// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

use lexigram::grammar::{print_ll1_table, GrTreeExt};
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
    Colon     = 0, // 0
    Lparen   ,     // 1
    Or       ,     // 2
    Plus     ,     // 3
    Question ,     // 4
    Rparen   ,     // 5
    Semicolon,     // 6
    Star     ,     // 7
    Grammar  ,     // 8
    SymEof   ,     // 9
    Lform    ,     // 10
    Rform    ,     // 11
    Id       ,     // 12
}

pub const TERMINALS: [(&str, Option<&str>); 13] = [
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

// -------------------------------------------------------------------------
// [non_terminal_symbols]

#[repr(u16)]
enum NT {
    File = 0,           // 0
    Header,             // 1
    Rules,              // 2
    Rule,               // 3
    RuleName,           // 4
    Prod,               // 5
    ProdFactor,         // 6
    ProdTerm,           // 7
    TermItem,           // 8
}

const NON_TERMINALS: [&str; 9] = [
    "file",             // 0
    "header",           // 1
    "rules",            // 2
    "rule",             // 3
    "rule_name",        // 4
    "prod",             // 5
    "prod_factor",      // 6
    "prod_term",        // 7
    "term_item",        // 8
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
    // file: header rules SymEOF?;
    //
    let tree = rules.get_tree_mut(NT::File as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add(Some(cc), gnode!(nt NT::Header));
    tree.add(Some(cc), gnode!(nt NT::Rules));
    tree.addc(Some(cc), gnode!(?), gnode!(t T::SymEof));

    // header:
    //     Grammar Id Semicolon
    // ;
    //
    let tree = rules.get_tree_mut(NT::Header as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Grammar), gnode!(t T::Id), gnode!(t T::Semicolon)]);

    // rules:
    //     rule
    // |   rules rule
    // ;
    //
    let tree = rules.get_tree_mut(NT::Rules as VarId);
    let or = tree.add_root(gnode!(|));
    tree.add(Some(or), gnode!(nt NT::Rule));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Rules), gnode!(nt NT::Rule)]);

    // rule:
    //     ruleName Colon prod Semicolon
    // ;
    //
    let tree = rules.get_tree_mut(NT::Rule as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(nt NT::RuleName), gnode!(t T::Colon), gnode!(nt NT::Prod), gnode!(t T::Semicolon)]);

    // ruleName:
    //     Id
    // ;
    let tree = rules.get_tree_mut(NT::RuleName as VarId);
    tree.add_root(gnode!(t T::Id));

    // prod:
    //     prodFactor
    // |   prod Or prodFactor
    // ;
    //
    let tree = rules.get_tree_mut(NT::Prod as VarId);
    let or = tree.add_root(gnode!(|));
    tree.add(Some(or), gnode!(nt NT::ProdFactor));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Prod), gnode!(t T::Or), gnode!(nt NT::ProdFactor)]);

    // prodFactor:
    //     prodTerm*
    // ;
    //
    let tree = rules.get_tree_mut(NT::ProdFactor as VarId);
    let star = tree.add_root(gnode!(*));
    tree.add(Some(star), gnode!(nt NT::ProdTerm));

    // prodTerm:
    //     termItem (Plus | Star | Question)?
    // ;
    //
    let tree = rules.get_tree_mut(NT::ProdTerm as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add(Some(cc), gnode!(nt NT::TermItem));
    let maybe = tree.add(Some(cc), gnode!(?));
    tree.addc_iter(Some(maybe), gnode!(|), [gnode!(t T::Plus), gnode!(t T::Star), gnode!(t T::Question)]);

    // termItem:
    //     Id
    // |   Lform
    // |   Rform
    // |   Lparen prod Rparen
    // ;
    //
    let tree = rules.get_tree_mut(NT::TermItem as VarId);
    let or = tree.add_root(gnode!(|));
    tree.add_iter(Some(or), [gnode!(t T::Id), gnode!(t T::Lform), gnode!(t T::Rform)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Lparen), gnode!(nt NT::Prod), gnode!(t T::Rparen)]);

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
    if verbose {
        println!("Parsing table:");
        print_ll1_table(builder.get_symbol_table(), builder.get_parsing_table(), 4);
    }
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
        const VERBOSE: bool = true;
        let result_src = gramparser_source(4, VERBOSE);
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