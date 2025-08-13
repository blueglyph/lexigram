// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::LL1;
use lexigram_lib::log::{BufLog, BuildFrom, LogReader, LogStatus, Logger};
use lexigram_lib::parsergen::{print_flags, ParserGen};
use lexigram_lib::test_tools::replace_tagged_source;
use lexigram_lib::grammar::{ProdRuleSet, ProdRuleSetTables};
use lexigram_lib::{hashmap, prod};
use super::{LEXIPARSER_FILENAME, LEXIPARSER_TAG};

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn lexiparser_source(indent: usize, verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram_lib: 0.5.2
    // lexigram: 0.5.2
    // build-stage1: 0.5.2

    // [versions]

    // -------------------------------------------------------------------------
    // [lexiparser_stage_2]

    let ll1_tables = ProdRuleSetTables::new(
        Some("LexiParser"),
        vec![
            prod!(nt 2, nt 17; nt 17),
            prod!(nt 4; nt 3; nt 5),
            prod!(t 18, t 26, t 14),
            prod!(t 19, t 26, t 14),
            prod!(t 16, t 5, t 26, nt 18, t 12),
            prod!(nt 6, t 1, nt 10, t 14; nt 7, t 1, nt 10, nt 23),
            prod!(t 17, t 26),
            prod!(t 26),
            prod!(nt 9, nt 19),
            prod!(t 19, t 6, t 26, t 13; t 21, t 6, t 26, t 13; t 20; t 23; t 22; t 24, t 6, t 26, t 13; t 25, t 6, t 26, t 13),
            prod!(nt 11),
            prod!(nt 12, nt 20),
            prod!(nt 21),
            prod!(nt 14, nt 24),
            prod!(t 6, nt 11, t 13; t 7, nt 14; t 26; t 27, nt 25; t 28; nt 15),
            prod!(t 30, nt 22, t 31; t 3; t 29),
            prod!(t 29; t 32, nt 26),
            prod!(nt 1, nt 17; e),
            prod!(t 2, t 26, nt 18; e),
            prod!(t 2, nt 9, nt 19; e),
            prod!(t 10, nt 12, nt 20; e),
            prod!(nt 13, nt 27),
            prod!(nt 16, nt 28),
            prod!(t 0, nt 8, t 14; t 14),
            prod!(t 9, nt 29; t 11; t 15, nt 30; e),
            prod!(t 4, t 27; e),
            prod!(t 8, t 32; e),
            prod!(nt 21; e),
            prod!(nt 22; e),
            prod!(t 11; e),
            prod!(t 11; e),
        ],
        vec![
        ],
        vec![("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Dot", Some(".")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Minus", Some("-")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("More", Some("more")), ("Skip", Some("skip")), ("Type", Some("type")), ("Channel", Some("channel")), ("Id", None), ("CharLit", None), ("StrLit", None), ("FixedSet", None), ("LSbracket", Some("[")), ("RSbracket", Some("]")), ("SetChar", None)],
        vec!["file", "file_item", "header", "declaration", "option", "rule", "rule_fragment_name", "rule_terminal_name", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "char_set", "char_set_one", "file_1", "option_1", "actions_1", "alt_items_1", "alt_item_1", "char_set_1", "rule_1", "repeat_item_1", "item_1", "char_set_one_1", "alt_item_2", "char_set_2", "repeat_item_2", "repeat_item_3"],
        vec![2048, 0, 0, 0, 2048, 32, 0, 0, 2048, 0, 0, 2048, 6144, 32, 34, 6144, 32, 1, 1, 1, 1, 4129, 4129, 64, 96, 64, 64, 64, 64, 64, 64],
        vec![None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, Some(0), Some(4), Some(8), Some(11), Some(12), Some(15), Some(5), Some(13), Some(14), Some(16), Some(21), Some(22), Some(24), Some(24)],
        Some(0),
        hashmap![]
    );

    // [lexiparser_stage_2]
    // -------------------------------------------------------------------------

    // - gets data from stage 1
    let ll1 = ProdRuleSet::<LL1>::build_from(ll1_tables);

    // - generates Lexi's parser source code (parser + listener):
    let mut builder = ParserGen::build_from(ll1);
    if verbose {
        print_flags(&builder, 4);
        println!("Parsing table of grammar '{}':", builder.get_name());
        builder.get_parsing_table().print(builder.get_symbol_table(), 4);
    }
    if !builder.get_log().has_no_errors() {
        return Err(builder.give_log());
    }
    builder.set_parents_have_value();
    builder.add_lib("lexiparser_types::*");
    let src = builder.gen_source_code(indent, true);
    let mut log = builder.give_log();
    if EXPECTED_NBR_WARNINGS != log.num_warnings() {
        log.add_error(format!("Unexpected number of warnings: {} instead of {EXPECTED_NBR_WARNINGS}", log.num_warnings()));
        Err(log)
    } else {
        Ok((log, src))
    }
}

pub fn write_lexiparser() {
    let (log, result_src) = lexiparser_source(0, true)
        .inspect_err(|log| panic!("Failed to build parser:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(LEXIPARSER_FILENAME, LEXIPARSER_TAG, &result_src)
        .expect("parser source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::test_tools::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;
        let (log, result_src) = lexiparser_source(0, VERBOSE)
            .inspect_err(|log| panic!("Failed to build parser:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
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
