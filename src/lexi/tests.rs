#![cfg(test)]

use std::collections::BTreeSet;
use std::io::{Cursor, Read};
use std::mem::size_of_val;
use crate::dfa::{Dfa, DfaBuilder, TokenId, tree_to_string};
use crate::{escape_string, CollectJoin, General, LL1};
use crate::io::CharReader;
use crate::lexer::Lexer;
use crate::lexergen::LexerGen;
use super::*;
use crate::dfa::tests::print_dfa;
use crate::grammar::ProdRuleSet;
use crate::grammar::tests::print_production_rules;
use crate::parsergen::ParserGen;
use crate::parsergen::tests::{print_flags, print_items};
use crate::test_tools::{get_tagged_source, replace_tagged_source};

// ---------------------------------------------------------------------------------------------
// Lexer

#[derive(Debug, Clone, Copy)]
pub enum LexerType { Normalized, Optimized }

fn make_dfa() -> Dfa<General> {
    const VERBOSE: bool = false;
    let re = build_re();
    let mut dfa_builder = DfaBuilder::from_re(re);
    let dfa = dfa_builder.build();
    if VERBOSE {
        println!("Tree: {}", tree_to_string(&dfa_builder.get_re(), true));
        println!("Messages:\n{}", dfa_builder.get_messages());
    }
    assert_eq!(dfa_builder.get_errors().len(), 0);
    dfa
}

fn make_lexer<R: Read>(ltype: LexerType) -> Lexer<R> {
    const VERBOSE: bool = false;
    let dfa = make_dfa();
    let dfa = if let LexerType::Normalized = ltype {
        dfa.normalize()
    } else {
        dfa.optimize()
    };
    if VERBOSE { print_dfa(&dfa); }
    let lexgen = LexerGen::from_dfa(&dfa);
    if VERBOSE {
        println!("Sources:");
        lexgen.write_source_code(None, 0).expect("Couldn't output the source code");
    }
    lexgen.make_lexer()
}

#[test]
fn lexilexer_source() {
    // CAUTION! Setting this to 'true' modifies the validation file with the current result
    const REPLACE_SOURCE: bool = false;

    const FILENAME: &str = "tests/gen/lexilexer.rs";
    const TAG: &str = "lexilexer";
    let dfa = make_dfa();
    let dfa = dfa.optimize();
    let mut lexgen = LexerGen::new();
    lexgen.max_utf8_chars = 0;
    lexgen.build_tables(&dfa);
    let result_src = lexgen.build_source_code(4);
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
        let mut lexer: Lexer<Cursor<&str>> = make_lexer(opt);
        check_lexer_tokens(&mut lexer, opt);
    }
}

/// We scan source files and check the tokens and the source text they cover.
pub fn check_lexer_tokens(lexer: &mut Lexer<Cursor<&str>>, opt: LexerType) {
    const VERBOSE: bool = false;
    let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
        (1, vec![
            // no error
            ("-> : , .. { ( ~ + | ? } ) ; * channels fragment lexicon mode pop push return skip",
             vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21],
             vec!["->", ":", ",", "..", "{", "(", "~", "+", "|", "?", "}", ")", ";", "*",
                  "channels", "fragment", "lexicon", "mode", "pop", "push", "return", "skip"]),
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
            let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text)| {
                assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                (tok, text)
            }).unzip();
            assert_eq!(tokens, expected_tokens, "test {} failed for opt={opt:?}, input '{}'", test_id, escape_string(input));
            assert_eq!(texts, expected_texts, "test {} failed for opt={opt:?}, input '{}'", test_id, escape_string(input));
            assert!(!lexer.has_error() || lexer.is_eos(), "test {} failed for opt={opt:?}, input '{}'",
                    test_id, escape_string(input));
        }
        if VERBOSE { println!("--------------------------------------\n"); }
    }
}

#[test]
/// We take the text output of each token and re-inject them to the lexer, then we compare both token streams.
fn regexgen_stability() {
    for opt in [LexerType::Normalized, LexerType::Optimized] {
        let mut lexer = make_lexer(opt);
        let stream = CharReader::new(Cursor::new(LEXICON));
        lexer.attach_stream(stream);
        let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer
            .tokens()
            .filter_map(|(tok, ch, text)| if ch == 0 { Some((tok, text)) } else { None })
            .unzip();
        let source2 = texts.join(" ");
        let stream2 = CharReader::new(Cursor::new(source2.as_str()));
        lexer.detach_stream();
        lexer.attach_stream(stream2);
        let (tokens2, texts2): (Vec<TokenId>, Vec<String>) = lexer
            .tokens()
            .filter_map(|(tok, ch, text)| if ch == 0 { Some((tok, text)) } else { None })
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
        if VERBOSE { print_dfa(&dfa); }
        let mut lexgen = LexerGen::new();
        lexgen.max_utf8_chars = 0;
        lexgen.build_tables(&dfa);
        let size_tables = size_of_val(lexgen.state_table.as_ref()) +
                size_of_val(lexgen.ascii_to_group.as_ref()) +
                size_of_val(&lexgen.utf8_to_group) +
                size_of_val(&lexgen.seg_to_group) +
                size_of_val(lexgen.terminal_table.as_ref());
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
    const FILENAME: &str = "tests/gen/lexiparser.rs";
    const TAG: &str = "lexiparser";
    let mut rts = build_rts();
    rts.set_start(0);
    if VERBOSE {
        println!("rules, num_nt = {}, NT symbols: {}", rts.get_trees_iter().count(), rts.get_symbol_table().unwrap().get_num_nt());
        let printable = std::collections::BTreeMap::from_iter(rts.get_trees_iter().map(|(id, t)| (id, format!("{t}"))));
        for (id, s) in printable {
            println!("{id} => {s}");
        }
    }
    let rules = ProdRuleSet::from(rts);
    println!("messages PRS<General>: {}", rules.get_log().get_messages().map(|l| format!("\n  {l:?}")).join(""));
    if VERBOSE {
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
    println!("messages PRS<LL1>: {}", ll1.get_log().get_messages().map(|l| format!("\n  {l:?}")).join(""));
    if VERBOSE {
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
    if VERBOSE {
        builder.build_item_ops();
        print_flags(&builder, 0);
        print_items(&builder, 0, false);
    }
    let result_src = builder.build_source_code(4, true);
    let expected_src = get_tagged_source(FILENAME, TAG).unwrap_or(String::new());
    if result_src != expected_src {
        if REPLACE_SOURCE {
            replace_tagged_source(FILENAME, TAG, &result_src).expect("source replacement failed");
        }
        assert_eq!(result_src, expected_src, "failed");
    }

}