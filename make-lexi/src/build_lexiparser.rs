// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

use lexigram_mlexi::grammar::GrTreeExt;
use lexigram_mlexi::{gnode, CollectJoin, General, LL1};
use lexigram_mlexi::grammar::{print_production_rules, ProdRuleSet, RuleTreeSet, VarId};
use lexigram_mlexi::log::Logger;
use lexigram_mlexi::parsergen::ParserGen;
use lexigram_mlexi::symbol_table::SymbolTable;
use lexigram_mlexi::test_tools::replace_tagged_source;
use crate::*;

// -------------------------------------------------------------------------
// [terminal_symbols]

#[repr(u16)]
enum T {
    Arrow     = 0, // 0
    Colon    ,     // 1
    Comma    ,     // 2
    Dot      ,     // 3
    Ellipsis ,     // 4
    Lbracket ,     // 5
    Lparen   ,     // 6
    Negate   ,     // 7
    Minus    ,     // 8
    Plus     ,     // 9
    Or       ,     // 10
    Question ,     // 11
    Rbracket ,     // 12
    Rparen   ,     // 13
    Semicolon,     // 14
    Star     ,     // 15
    Channels ,     // 16
    Fragment ,     // 17
    Lexicon  ,     // 18
    Mode     ,     // 19
    Pop      ,     // 20
    Push     ,     // 21
    More     ,     // 22
    Skip     ,     // 23
    Type     ,     // 24
    Channel  ,     // 25
    SymEof   ,     // 26
    Id       ,     // 27
    CharLit  ,     // 28
    StrLit   ,     // 29
    FixedSet ,     // 30
    LSbracket,     // 31
    RSbracket,     // 32
    SetChar  ,     // 33
}

pub const TERMINALS: [(&str, Option<&str>); 34] = [
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

// -------------------------------------------------------------------------
// [non_terminal_symbols]

#[repr(u16)]
enum NT {
    File = 0,           // 0
    FileItem,           // 1
    Header,             // 2
    Declaration,        // 3
    Option,             // 4
    Rule,               // 5
    Actions,            // 6
    Action,             // 7
    Match,              // 8
    AltItems,           // 9
    AltItem,            // 10
    RepeatItem,         // 11
    Item,               // 12
    CharSet,            // 13
    CharSetOne,         // 14
}

const NON_TERMINALS: [&str; 15] = [
    "file",             // 0
    "file_item",        // 1
    "header",           // 2
    "declaration",      // 3
    "option",           // 4
    "rule",             // 5
    "actions",          // 6
    "action",           // 7
    "match",            // 8
    "alt_items",        // 9
    "alt_item",         // 10
    "repeat_item",      // 11
    "item",             // 12
    "char_set",         // 13
    "char_set_one",     // 14
];

// [non_terminal_symbols]
// -------------------------------------------------------------------------

pub(crate) fn build_rts() -> RuleTreeSet<General> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_non_terminals(NON_TERMINALS);
    symbol_table.extend_terminals(TERMINALS);
    let mut rules = RuleTreeSet::new();
    rules.set_symbol_table(symbol_table);

    // grammar LexiParser;
    //
    // file: header? file_item* EOF;
    //
    let tree = rules.get_tree_mut(NT::File as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.addc(Some(cc), gnode!(?), gnode!(nt NT::Header));
    tree.addc(Some(cc), gnode!(*), gnode!(nt NT::FileItem));

    // file_item:
    //     option | declaration | rule
    // ;
    //
    let tree = rules.get_tree_mut(NT::FileItem as VarId);
    let or = tree.add_root(gnode!(|));
    tree.add_iter(Some(or), [gnode!(nt NT::Option), gnode!(nt NT::Declaration), gnode!(nt NT::Rule)]);

    // header:
    //     LEXICON ID SEMICOLON
    // ;
    //
    let tree = rules.get_tree_mut(NT::Header as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Lexicon), gnode!(t T::Id), gnode!(t T::Semicolon)]);

    // declaration:
    //     MODE ID SEMICOLON
    // ;
    //
    let tree = rules.get_tree_mut(NT::Declaration as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Mode), gnode!(t T::Id), gnode!(t T::Semicolon)]);

    // option:
    //     CHANNELS LBRACKET ID (COMMA ID)* RBRACKET
    // ;
    //
    let tree = rules.get_tree_mut(NT::Option as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Channels), gnode!(t T::Lbracket), gnode!(t T::Id)]);
    let star1 = tree.add(Some(cc), gnode!(*));
    tree.addc_iter(Some(star1), gnode!(&), [gnode!(t T::Comma), gnode!(t T::Id)]);
    tree.add(Some(cc), gnode!(t T::Rbracket));

    // rule:
    //     FRAGMENT ID COLON match SEMICOLON
    // |   ID COLON match (ARROW actions)? SEMICOLON
    // ;
    //
    let tree = rules.get_tree_mut(NT::Rule as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Fragment), gnode!(t T::Id), gnode!(t T::Colon), gnode!(nt NT::Match), gnode!(t T::Semicolon)]);
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Id), gnode!(t T::Colon), gnode!(nt NT::Match)]);
    let maybe2 = tree.add(Some(cc1), gnode!(?));
    tree.addc_iter(Some(maybe2), gnode!(&), [gnode!(t T::Arrow), gnode!(nt NT::Actions)]);
    tree.add(Some(cc1), gnode!(t T::Semicolon));

    // actions:
    //     action (COMMA action)*
    // ;
    //
    let tree = rules.get_tree_mut(NT::Actions as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add(Some(cc), gnode!(nt NT::Action));
    let star1 = tree.add(Some(cc), gnode!(*));
    tree.addc_iter(Some(star1), gnode!(&), [gnode!(t T::Comma), gnode!(nt NT::Action)]);

    // action:
    //     MODE LPAREN ID RPAREN
    // |   PUSH LPAREN ID RPAREN
    // |   POP
    // |   SKiP
    // |   MORE
    // |   TYPE LPAREN ID RPAREN
    // |   CHANNEL LPAREN ID RPAREN
    // ;
    //
    let tree = rules.get_tree_mut(NT::Action as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Mode), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Push), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);
    tree.add_iter(Some(or), [gnode!(t T::Pop), gnode!(t T::Skip), gnode!(t T::More)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Type), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Channel), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);

    // match:
    //     alt_items
    // ;
    //
    let tree = rules.get_tree_mut(NT::Match as VarId);
    tree.add_root(gnode!(nt NT::AltItems));

    // alt_items:
    //     alt_item (OR alt_item)*
    // ;
    let tree = rules.get_tree_mut(NT::AltItems as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add(Some(cc), gnode!(nt NT::AltItem));
    let star = tree.add(Some(cc), gnode!(*));
    tree.addc_iter(Some(star), gnode!(&), [gnode!(t T::Or), gnode!(nt NT::AltItem)]);

    // alt_item:
    // 	repeat_item+
    // ;
    //
    let tree = rules.get_tree_mut(NT::AltItem as VarId);
    let plus = tree.add_root(gnode!(+));
    tree.add(Some(plus), gnode!(nt NT::RepeatItem));

    // repeat_item:
    //     item STAR QUESTION?
    // |   item PLUS QUESTION?
    // |   item QUESTION?
    // ;
    let tree = rules.get_tree_mut(NT::RepeatItem as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item), gnode!(t T::Star)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item), gnode!(t T::Plus)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));

    //
    // item:
    //     ID
    // |   CHAR_LIT (ELLIPSIS CHAR_LIT)?
    // |   STR_LIT
    // |   char_set
    // |   LPAREN alt_item RPAREN
    // |   NEGATE item
    // ;
    let tree = rules.get_tree_mut(NT::Item as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1s = tree.add_iter(Some(or), [
        gnode!(t T::Id),        // 0: ID
        gnode!(&),              // 2: CHAR_LIT (ELLIPSIS CHAR_LIT)?
        gnode!(t T::StrLit),    // 3: STR_LIT
        gnode!(nt NT::CharSet), // 4: char_set
        gnode!(&),              // 5: LPAREN alt_items RPAREN
        gnode!(&),              // 6: NEGATE item
    ]);
    tree.add(Some(cc1s[1]), gnode!(t T::CharLit));
    let maybe2 = tree.add(Some(cc1s[1]), gnode!(?));
    tree.addc_iter(Some(maybe2), gnode!(&), [gnode!(t T::Ellipsis), gnode!(t T::CharLit)]);
    tree.add_iter(Some(cc1s[4]), [gnode!(t T::Lparen), gnode!(nt NT::AltItems), gnode!(t T::Rparen)]);
    tree.add_iter(Some(cc1s[5]), [gnode!(t T::Negate), gnode!(nt NT::Item)]);

    // char_set:
    //     LSBRACKET (char_set_one)+ RSBRACKET
    // |   DOT
    // |   FIXED_SET;
    let tree = rules.get_tree_mut(NT::CharSet as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1 = tree.addc(Some(or), gnode!(&), gnode!(t T::LSbracket));
    tree.addc(Some(cc1), gnode!(+), gnode!(nt NT::CharSetOne));
    tree.add(Some(cc1), gnode!(t T::RSbracket));
    tree.add(Some(or), gnode!(t T::Dot));
    tree.add(Some(or), gnode!(t T::FixedSet));

    // char_set_one:
    //     SET_CHAR MINUS SET_CHAR | SET_CHAR | FIXED_SET;
    let tree = rules.get_tree_mut(NT::CharSetOne as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::SetChar), gnode!(t T::Minus), gnode!(t T::SetChar)]);
    tree.add_iter(Some(or), [gnode!(t T::SetChar), gnode!(t T::FixedSet)]);

    rules
}

fn lexiparser_source(indent: usize, verbose: bool) -> String {
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
    let mut builder = ParserGen::from_rules(ll1, "LexiParser".to_string());
    for v in 0..builder.get_symbol_table().unwrap().get_num_nt() as VarId {
        // print!("- {}: ", Symbol::NT(v).to_str(builder.get_symbol_table()));
        if builder.get_nt_parent(v).is_none() {
            builder.set_nt_has_value(v, true);
            // println!("has no parent, has value");
        } else {
            // println!("has parents, has no value");
        }
    }
    builder.add_lib("super::lexiparser_types::*");
    builder.build_source_code(indent, true)
}

fn write_lexiparser() {
    let result_src = lexiparser_source(4, false);
    replace_tagged_source(LEXIPARSER_FILENAME, LEXIPARSER_TAG, &result_src)
        .expect("parser source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_mlexi::test_tools::{get_tagged_source, replace_tagged_source};
    use crate::*;
    use super::*;

    #[test]
    fn test_source() {
        let result_src = lexiparser_source(4, false);
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