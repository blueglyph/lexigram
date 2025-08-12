// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use std::collections::BTreeSet;
use std::io::Cursor;
use std::mem::size_of_val;
use crate::dfa::{Dfa, DfaBuilder, TokenId, Terminal};
use crate::{escape_string, gnode, CollectJoin, General, SymbolTable, LL1};
use crate::SymInfoTable;
use crate::io::CharReader;
use crate::lexer::Lexer;
use crate::lexergen::{LexerGen, LexerTables};
use super::*;
use crate::grammar::{ProdRuleSet, GrTreeExt, VarId, RuleTreeSet};
use crate::log::{BuildFrom, LogReader, LogStatus, TryBuildInto};
use crate::parsergen::{print_flags, print_items, ParserGen};
use crate::test_tools::{get_tagged_source, replace_tagged_source};

// ---------------------------------------------------------------------------------------------
// Lexer

#[derive(Debug, Clone, Copy)]
pub enum LexerType { Normalized, Optimized }

fn make_dfa() -> Dfa<General> {
    const VERBOSE: bool = false;
    let regs = build_re();
    let mut dfas = vec![];
    for (n, re) in regs {
        let dfa_builder = DfaBuilder::build_from(re);
        let dfa = Dfa::<General>::build_from(dfa_builder);
        if VERBOSE {
            println!("Mode {n}:");
            println!("Messages:\n{}", dfa.get_log().get_messages_str());
        }
        assert!(dfa.get_log().has_no_errors(), "Failed to build DFA:\n{}", dfa.get_log().get_messages_str());
        dfas.push((n, dfa));
    }
    let dfa = Dfa::<General>::build_from(dfas);
    if VERBOSE {
        println!("Messages:\n{}", dfa.get_log().get_messages_str());
    }
    assert!(dfa.get_log().has_no_errors(), "failed to build Dfa:\n{}", dfa.get_log().get_messages_str());
    dfa
}

fn make_lexer_tables(ltype: LexerType) -> LexerTables {
    const VERBOSE: bool = false;
    let dfa = make_dfa();
    let dfa = if let LexerType::Normalized = ltype {
        dfa.normalize()
    } else {
        dfa.optimize()
    };
    if VERBOSE { dfa.print(4); }
    let lexgen = LexerGen::build_from(dfa);
    if VERBOSE {
        println!("Sources:");
        lexgen.write_source_code(None, 0).expect("Couldn't output the source code");
    }
    match lexgen.try_build_into() {
        Ok(tables) => tables,
        Err(build_error) => panic!("{build_error}"),
    }
}

#[test]
#[cfg(not(miri))]
fn lexilexer_source() {
    // CAUTION! Setting this to 'true' modifies the validation file with the current result
    const REPLACE_SOURCE: bool = false;

    const FILENAME: &str = "tests/out/lexilexer.rs";
    const TAG: &str = "lexilexer";
    let dfa = make_dfa();
    let dfa = dfa.optimize();
    let lexgen = LexerGen::build_from(dfa);
    let result_src = lexgen.gen_source_code(4);
    let expected_src = get_tagged_source(FILENAME, TAG).unwrap_or(String::new());
    if result_src != expected_src {
        if REPLACE_SOURCE {
            replace_tagged_source(FILENAME, TAG, &result_src).expect("source replacement failed");
        }
        assert_eq!(result_src, expected_src, "failed");
    }
}

#[test]
fn lexilexer_tokens() {
    for opt in [LexerType::Normalized, LexerType::Optimized] {
        let lexer_tables = make_lexer_tables(opt);
        let mut lexer: Lexer<Cursor<&str>> = lexer_tables.make_lexer();
        check_lexer_tokens(&mut lexer, opt);
    }
}

/// We scan source files and check the tokens and the source text they cover.
pub fn check_lexer_tokens(lexer: &mut Lexer<Cursor<&str>>, opt: LexerType) {
    const VERBOSE: bool = false;
    let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
        (1, vec![
            // no error
            ("-> : , . .. { ( ~ - + | ? } ) ; * channels fragment lexicon mode pop push more skip type channel \\w[a-z.\\t\\w]",
             vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 30, 31, 33, 8, 33, 33, 33, 30, 32],
             vec!["->", ":", ",", ".", "..", "{", "(", "~", "-", "+", "|", "?", "}", ")", ";", "*",
                  "channels", "fragment", "lexicon", "mode", "pop", "push", "more", "skip", "type", "channel", "\\w", "[", "a", "-", "z", ".", "\\t", "\\w", "]"]),
        ]),
        (2, vec![(LEXICON, LEXICON_TOKENS.to_vec(), LEXICON_TEXT.to_vec())]),
    ];
    for (test_id, inputs) in tests {
        if VERBOSE { println!("test {test_id}, opt={opt:?}:"); }
        for (input, expected_tokens, expected_texts) in inputs {
            //let expected_texts = expected_texts.iter().map(|s| s.escape_default());
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            lexer.attach_stream(stream);
            let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text, _col, _line)| {
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                (tok, text)
            }).unzip();
            if VERBOSE {
                if lexer.has_error() {
                    println!("ERROR: {:?}", lexer.get_error());
                }
            }
            let txt = format!("test {} failed for opt={opt:?}, input '{}'{}", test_id, escape_string(input),
                              if lexer.has_error() { format!(", error: {:?}", lexer.get_error()) } else { String::new() });
            assert_eq!(tokens, expected_tokens, "{txt}");
            assert_eq!(texts, expected_texts, "txt");
            assert!(!lexer.has_error() || lexer.is_eos(), "{txt}");
        }
        if VERBOSE { println!("--------------------------------------\n"); }
    }
}

#[test]
/// We take the text output of each token and re-inject them to the lexer, then we compare both token streams.
fn regexgen_stability() {
    const VERBOSE: bool = false;
    for opt in [LexerType::Normalized, LexerType::Optimized] {
        let lexer_tables = make_lexer_tables(opt);
        let mut lexer = lexer_tables.make_lexer();
        let stream = CharReader::new(Cursor::new(LEXICON));
        lexer.attach_stream(stream);
        let mut source2 = String::new();
        let mut mode1 = false;
        let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer
            .tokens()
            .filter_map(|(tok, ch, text, _col, _line)| if ch == 0 {
                source2.push_str(&text);
                if VERBOSE { println!("{} {text}", if mode1 { "1" } else { " " }); }
                if &text == "[" {
                    mode1 = true; // '[' doesn't need escaping within mode1, so we don't check the mode here
                } else if &text == "]" {
                    assert!(mode1);
                    mode1 = false;
                }
                if !mode1 {
                    source2.push(' ');
                }
                Some((tok, text))
            } else {
                None
            })
            .unzip();
        source2.pop(); // remove trailing space
        if VERBOSE { println!("{source2}"); }
        let stream2 = CharReader::new(Cursor::new(source2.as_str()));
        lexer.detach_stream();
        lexer.attach_stream(stream2);
        let (tokens2, texts2): (Vec<TokenId>, Vec<String>) = lexer
            .tokens()
            .filter_map(|(tok, ch, text, _col, _line)| if ch == 0 { Some((tok, text)) } else { None })
            .unzip();
        assert_eq!(tokens, tokens2, "failed for opt={opt:?}");
        assert_eq!(texts, texts2, "failed for opt={opt:?}");
    }
}

#[ignore]
#[test]
// Not a test. Only shows the improvements in table size between the two versions.
fn regexgen_optimize() {
    const VERBOSE: bool = false;
    for opt in [false, true] {
        println!("-----------------------------------------\n{} DFA:", if opt { "optimized" } else { "normalized" });
        let dfa = make_dfa();
        let dfa = if opt {
            dfa.optimize()
        } else {
            dfa.normalize()
        };
        println!("DFA:\n- {} states\n- {} terminals\n- {} end states",
                 dfa.get_state_graph().len(),
                 dfa.get_end_states().iter().map(|(_, t)| t.clone()).collect::<BTreeSet<Terminal>>().len(),
                 dfa.get_end_states().len()
        );
        if VERBOSE { dfa.print(4); }
        let lexgen = LexerGen::build_from_dfa(dfa, 0);
        let size_tables = size_of_val(&lexgen.state_table) +
                size_of_val(&lexgen.ascii_to_group) +
                size_of_val(&lexgen.utf8_to_group) +
                size_of_val(&lexgen.seg_to_group) +
                size_of_val(&lexgen.terminal_table);
        println!("Lexer:\n- {} states\n- {} groups\n- {} segments\n- {} terminals (table)\n- {:.1}k tables",
                 lexgen.nbr_states,
                 lexgen.nbr_groups,
                 lexgen.seg_to_group.len(),
                 lexgen.terminal_table.len(),
                 size_tables as f64 * 0.001);
        if VERBOSE {
            println!("Sources:");
            lexgen.write_source_code(None, 0).expect("Couldn't output the source code");
        }
    }
}

#[ignore]
#[test]
// Not a test. Only shows the size of a few types.
fn type_size() {
    println!("Size of main types:");
    println!("- Terminal   : {:4} bytes", std::mem::size_of::<crate::dfa::Terminal>());
    println!("- ReType     : {:4} bytes", std::mem::size_of::<crate::dfa::ReType>());
    println!("- ReNode     : {:4} bytes", std::mem::size_of::<crate::dfa::ReNode>());
    println!("- StateId    : {:4} bytes", std::mem::size_of::<crate::dfa::StateId>());
    println!("- TokenId    : {:4} bytes", std::mem::size_of::<crate::dfa::TokenId>());
    println!("- ModeId     : {:4} bytes", std::mem::size_of::<crate::dfa::ModeId>());
    println!("- ChannelId  : {:4} bytes", std::mem::size_of::<crate::dfa::ChannelId>());
    println!("- Seg        : {:4} bytes", std::mem::size_of::<crate::segments::Seg>());
    println!("- Segments   : {:4} bytes", std::mem::size_of::<crate::segments::Segments>());
}

// ---------------------------------------------------------------------------------------------
// Parser

#[test]
fn lexiparser_source() {
    // CAUTION! Setting this to 'true' modifies the validation file with the current result
    const REPLACE_SOURCE: bool = false;

    const VERBOSE: bool = false;
    const FILENAME: &str = "tests/out/lexiparser.rs";
    const TAG: &str = "lexiparser";
    let mut rts = build_rts();
    rts.set_start(0);
    if VERBOSE {
        println!("rules, num_nt = {}, NT symbols: {}", rts.get_trees_iter().count(), rts.get_symbol_table().unwrap().get_num_nt());
        let printable = std::collections::BTreeMap::from_iter(rts.get_trees_iter().map(|(id, t)| (id, format!("{}", t.to_str(None, None)))));
        for (id, s) in printable {
            println!("{id} => {s}");
        }
    }
    let rules = ProdRuleSet::build_from(rts);
    if !rules.get_log().is_empty() {
        println!("messages PRS<General>: {}", rules.get_log().get_messages().map(|l| format!("\n  {l:?}")).join(""));
    }
    if VERBOSE {
        let st_num_nt = rules.get_symbol_table().unwrap().get_num_nt();
        println!("rules, num_nt = {}, NT symbols: {}", rules.get_num_nt(), st_num_nt);
        println!("- {}", (0..st_num_nt).map(|i| rules.get_symbol_table().unwrap().get_nt_name(i as VarId)).join(", "));
        rules.print_rules(true, false);
        let msg = rules.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Messages:\n{msg}");
        }
    }
    assert_eq!(rules.get_log().num_errors(), 0);
    let ll1 = ProdRuleSet::<LL1>::build_from(rules);
    if !ll1.get_log().is_empty() {
        println!("messages PRS<LL1>: {}", ll1.get_log().get_messages().map(|l| format!("\n  {l:?}")).join(""));
    }
    if VERBOSE {
        println!("LL1, num_nt = {}, NT symbols: {}", ll1.get_num_nt(), ll1.get_symbol_table().unwrap().get_num_nt());
        ll1.print_rules(true, false);
        let msg = ll1.get_log().get_messages().map(|s| format!("- {s:?}")).join("\n");
        if !msg.is_empty() {
            println!("Messages:\n{msg}");
        }
    }
    assert_eq!(ll1.get_log().num_errors(), 0);
    let mut builder = ParserGen::build_from_rules(ll1, "LexiParser".to_string());
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
    if VERBOSE {
        builder.make_item_ops();
        print_flags(&builder, 0);
        print_items(&builder, 0, false);
    }
    let result_src = builder.gen_source_code(4, true);
    if !cfg!(miri) {
        let expected_src = get_tagged_source(FILENAME, TAG).unwrap_or(String::new());
        if result_src != expected_src {
            if REPLACE_SOURCE {
                replace_tagged_source(FILENAME, TAG, &result_src).expect("source replacement failed");
            }
            assert_eq!(result_src, expected_src, "failed");
        }
    }
}

// ---------------------------------------------------------------------------------------------

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

pub(crate) fn build_rts() -> RuleTreeSet<General> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_nonterminals(NON_TERMINALS);
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