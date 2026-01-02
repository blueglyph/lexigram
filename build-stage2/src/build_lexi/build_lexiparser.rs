// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashMap;
use lexigram_lib::{gnode, LL1, VarId, TokenId};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::BuildFrom;
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::file_utils::replace_tagged_source;
use lexigram_lib::grammar::{GrNode, GrTree, ProdRuleSet, ProdRuleSetTables};
use lexigram_lib::{hashmap, prule};
use lexigram_lib::grammar::origin::Origin;
use super::{LEXIPARSER_FILENAME, LEXIPARSER_TAG};

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn lexiparser_source(indent: usize, verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram-lib: 0.8.3
    // lexi-gram: 0.8.3
    // build-stage1: 0.8.3

    // [versions]

    // -------------------------------------------------------------------------
    // [lexiparser_stage_2]

    static ORIGIN: [(Option<usize>, &[(GrNode, &[usize])]); 18] = [
        (Some(7), &[(gnode!(*), &[1]), (gnode!(nt 1), &[]), (gnode!(nt 2), &[]), (gnode!(inst), &[0]), (gnode!(&), &[2,3]), (gnode!(inst), &[0]), (gnode!(&), &[5]), (gnode!(|), &[4,6])]),
        (Some(3), &[(gnode!(nt 4), &[]), (gnode!(nt 3), &[]), (gnode!(nt 5), &[]), (gnode!(|), &[0,1,2])]),
        (Some(3), &[(gnode!(t 18), &[]), (gnode!(t 27), &[]), (gnode!(t 14), &[]), (gnode!(&), &[0,1,2])]),
        (Some(3), &[(gnode!(t 19), &[]), (gnode!(t 27), &[]), (gnode!(t 14), &[]), (gnode!(&), &[0,1,2])]),
        (Some(9), &[(gnode!(*), &[3]), (gnode!(t 2), &[]), (gnode!(t 27), &[]), (gnode!(&), &[1,2]), (gnode!(t 16), &[]), (gnode!(t 5), &[]), (gnode!(t 27), &[]), (gnode!(inst), &[0]), (gnode!(t 12), &[]), (gnode!(&), &[4,5,6,7,8])]),
        (Some(31), &[(gnode!(nt 7), &[]), (gnode!(t 1), &[]), (gnode!(nt 11), &[]), (gnode!(t 14), &[]), (gnode!(&), &[0,1,2,3]), (gnode!(nt 8), &[]), (gnode!(t 1), &[]), (gnode!(nt 11), &[]), (gnode!(t 0), &[]), (gnode!(nt 9), &[]), (gnode!(t 14), &[]), (gnode!(&), &[5,6,7,8,9,10]), (gnode!(nt 8), &[]), (gnode!(t 1), &[]), (gnode!(nt 11), &[]), (gnode!(t 14), &[]), (gnode!(&), &[12,13,14,15]), (gnode!(t 6), &[]), (gnode!(nt 8), &[]), (gnode!(t 13), &[]), (gnode!(nt 6), &[]), (gnode!(t 0), &[]), (gnode!(t 26), &[]), (gnode!(t 14), &[]), (gnode!(&), &[17,18,19,20,21,22,23]), (gnode!(t 6), &[]), (gnode!(nt 8), &[]), (gnode!(t 13), &[]), (gnode!(nt 6), &[]), (gnode!(t 14), &[]), (gnode!(&), &[25,26,27,28,29]), (gnode!(|), &[4,11,16,24,30])]),
        (Some(4), &[(gnode!(t 1), &[]), (gnode!(t 29), &[]), (gnode!(&), &[0,1]), (gnode!(e), &[]), (gnode!(|), &[2,3])]),
        (Some(2), &[(gnode!(t 17), &[]), (gnode!(t 27), &[]), (gnode!(&), &[0,1])]),
        (Some(0), &[(gnode!(t 27), &[])]),
        (Some(6), &[(gnode!(*), &[3]), (gnode!(t 2), &[]), (gnode!(nt 10), &[]), (gnode!(&), &[1,2]), (gnode!(nt 10), &[]), (gnode!(inst), &[0]), (gnode!(&), &[4,5])]),
        (Some(24), &[(gnode!(t 19), &[]), (gnode!(t 6), &[]), (gnode!(t 27), &[]), (gnode!(t 13), &[]), (gnode!(&), &[0,1,2,3]), (gnode!(t 21), &[]), (gnode!(t 6), &[]), (gnode!(t 27), &[]), (gnode!(t 13), &[]), (gnode!(&), &[5,6,7,8]), (gnode!(t 20), &[]), (gnode!(t 23), &[]), (gnode!(t 22), &[]), (gnode!(t 24), &[]), (gnode!(t 6), &[]), (gnode!(t 27), &[]), (gnode!(t 13), &[]), (gnode!(&), &[13,14,15,16]), (gnode!(t 25), &[]), (gnode!(t 6), &[]), (gnode!(t 27), &[]), (gnode!(t 13), &[]), (gnode!(&), &[18,19,20,21]), (gnode!(t 26), &[]), (gnode!(|), &[4,9,10,11,12,17,22,23])]),
        (Some(0), &[(gnode!(nt 12), &[])]),
        (Some(6), &[(gnode!(*), &[3]), (gnode!(t 10), &[]), (gnode!(nt 13), &[]), (gnode!(&), &[1,2]), (gnode!(nt 13), &[]), (gnode!(inst), &[0]), (gnode!(&), &[4,5])]),
        (Some(2), &[(gnode!(+), &[1]), (gnode!(nt 14), &[]), (gnode!(inst), &[0])]),
        (Some(19), &[(gnode!(nt 15), &[]), (gnode!(t 15), &[]), (gnode!(t 11), &[]), (gnode!(&), &[0,1,2]), (gnode!(nt 15), &[]), (gnode!(t 15), &[]), (gnode!(&), &[4,5]), (gnode!(nt 15), &[]), (gnode!(t 9), &[]), (gnode!(t 11), &[]), (gnode!(&), &[7,8,9]), (gnode!(nt 15), &[]), (gnode!(t 9), &[]), (gnode!(&), &[11,12]), (gnode!(nt 15), &[]), (gnode!(t 11), &[]), (gnode!(&), &[14,15]), (gnode!(nt 15), &[]), (gnode!(&), &[17]), (gnode!(|), &[3,6,10,13,16,18])]),
        (Some(16), &[(gnode!(t 27), &[]), (gnode!(t 28), &[]), (gnode!(t 4), &[]), (gnode!(t 28), &[]), (gnode!(&), &[1,2,3]), (gnode!(t 28), &[]), (gnode!(&), &[5]), (gnode!(t 29), &[]), (gnode!(nt 16), &[]), (gnode!(t 6), &[]), (gnode!(nt 12), &[]), (gnode!(t 13), &[]), (gnode!(&), &[9,10,11]), (gnode!(t 7), &[]), (gnode!(nt 15), &[]), (gnode!(&), &[13,14]), (gnode!(|), &[0,4,6,7,8,12,15])]),
        (Some(8), &[(gnode!(+), &[1]), (gnode!(nt 17), &[]), (gnode!(t 31), &[]), (gnode!(inst), &[0]), (gnode!(t 32), &[]), (gnode!(&), &[2,3,4]), (gnode!(t 3), &[]), (gnode!(t 30), &[]), (gnode!(|), &[5,6,7])]),
        (Some(6), &[(gnode!(t 33), &[]), (gnode!(t 8), &[]), (gnode!(t 33), &[]), (gnode!(&), &[0,1,2]), (gnode!(t 33), &[]), (gnode!(t 30), &[]), (gnode!(|), &[3,4,5])]),
    ];
    static MAP: [(VarId, (VarId, usize)); 6] = [
        (18, (0, 0)), (19, (4, 0)), (20, (9, 0)), (21, (12, 0)), (22, (13, 0)),
        (23, (16, 0)),
    ];
    let origin = Origin::from_data(
        ORIGIN.into_iter().map(|(root, nodes)| GrTree::from((root, nodes.to_vec()))).collect(),
        HashMap::from(MAP));

    let ll1_tables = ProdRuleSetTables::new(
        Some("LexiParser"),
        vec![
            prule!(%(0, 4), nt 2, nt 18; %(0, 6), nt 18),
            prule!(%(1, 0), nt 4; %(1, 1), nt 3; %(1, 2), nt 5),
            prule!(%(2, 3), t 18, t 27, t 14),
            prule!(%(3, 3), t 19, t 27, t 14),
            prule!(%(4, 9), t 16, t 5, t 27, nt 19, t 12),
            prule!(t 6, nt 8, t 13, nt 6, nt 24; %(5, 4), nt 7, t 1, nt 11, t 14; nt 8, t 1, nt 11, nt 25),
            prule!(%(6, 2), t 1, t 29; %(6, 3), e),
            prule!(%(7, 2), t 17, t 27),
            prule!(%(8, 0), t 27),
            prule!(%(9, 6), nt 10, nt 20),
            prule!(%(10, 4), t 19, t 6, t 27, t 13; %(10, 9), t 21, t 6, t 27, t 13; %(10, 10), t 20; %(10, 11), t 23; %(10, 12), t 22; %(10, 17), t 24, t 6, t 27, t 13; %(10, 22), t 25, t 6, t 27, t 13; %(10, 23), t 26),
            prule!(%(11, 0), nt 12),
            prule!(%(12, 6), nt 13, nt 21),
            prule!(%(13, 2), nt 22),
            prule!(nt 15, nt 26),
            prule!(%(15, 12), t 6, nt 12, t 13; %(15, 15), t 7, nt 15; %(15, 0), t 27; t 28, nt 27; %(15, 7), t 29; %(15, 8), nt 16),
            prule!(%(16, 5), t 31, nt 23, t 32; %(16, 6), t 3; %(16, 7), t 30),
            prule!(%(17, 5), t 30; t 33, nt 28),
            prule!(%(0, 1), nt 1, nt 18; e),
            prule!(%(4, 3), t 2, t 27, nt 19; e),
            prule!(%(9, 3), t 2, nt 10, nt 20; e),
            prule!(%(12, 3), t 10, nt 13, nt 21; e),
            prule!(%(13, 1), nt 14, nt 29),
            prule!(%(16, 1), nt 17, nt 30),
            prule!(%(5, 24), t 0, t 26, t 14; %(5, 30), t 14),
            prule!(%(5, 11), t 0, nt 9, t 14; %(5, 16), t 14),
            prule!(t 9, nt 31; %(14, 16), t 11; t 15, nt 32; %(14, 18), e),
            prule!(%(15, 4), t 4, t 28; %(15, 6), e),
            prule!(%(17, 3), t 8, t 33; %(17, 4), e),
            prule!(%(13, 1), nt 22; %(13, 1), e),
            prule!(%(16, 1), nt 23; %(16, 1), e),
            prule!(%(14, 10), t 11; %(14, 13), e),
            prule!(%(14, 3), t 11; %(14, 6), e),
        ],
        origin,
        vec![("Arrow", Some("->")), ("Colon", Some(":")), ("Comma", Some(",")), ("Dot", Some(".")), ("Ellipsis", Some("..")), ("Lbracket", Some("{")), ("Lparen", Some("(")), ("Negate", Some("~")), ("Minus", Some("-")), ("Plus", Some("+")), ("Or", Some("|")), ("Question", Some("?")), ("Rbracket", Some("}")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Channels", Some("channels")), ("Fragment", Some("fragment")), ("Lexicon", Some("lexicon")), ("Mode", Some("mode")), ("Pop", Some("pop")), ("Push", Some("push")), ("More", Some("more")), ("Skip", Some("skip")), ("Type", Some("type")), ("Channel", Some("channel")), ("Hook", Some("hook")), ("Id", None), ("CharLit", None), ("StrLit", None), ("FixedSet", None), ("LSbracket", Some("[")), ("RSbracket", Some("]")), ("SetChar", None)],
        vec!["file", "file_item", "header", "declaration", "option", "rule", "opt_str_lit", "rule_fragment_name", "rule_terminal_name", "actions", "action", "match", "alt_items", "alt_item", "repeat_item", "item", "char_set", "char_set_one", "file_1", "option_1", "actions_1", "alt_items_1", "alt_item_1", "char_set_1", "rule_1", "rule_2", "repeat_item_1", "item_1", "char_set_one_1", "alt_item_2", "char_set_2", "repeat_item_2", "repeat_item_3"],
        vec![2048, 0, 0, 0, 2048, 32, 0, 0, 0, 2048, 0, 0, 2048, 6144, 32, 34, 6144, 32, 1, 1, 1, 1, 4129, 4129, 64, 64, 96, 64, 64, 64, 64, 64, 64],
        vec![None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, Some(0), Some(4), Some(9), Some(12), Some(13), Some(16), Some(5), Some(5), Some(14), Some(15), Some(17), Some(22), Some(23), Some(26), Some(26)],
        Some(0),
        hashmap![]
    );

    // [lexiparser_stage_2]
    // -------------------------------------------------------------------------
    // [lexiparser_stage_2_hooks]

    static TERMINAL_HOOKS: [TokenId; 0] = [
    ];

    // [lexiparser_stage_2_hooks]
    // -------------------------------------------------------------------------

    // - gets data from stage 1
    let ll1 = ProdRuleSet::<LL1>::build_from(ll1_tables);

    // - generates Lexi's parser source code (parser + listener):
    let mut builder = ParserGen::build_from(ll1);
    builder.set_terminal_hooks(TERMINAL_HOOKS.to_vec());
    if verbose {
        builder.print_flags(4);
        println!("Parsing table of grammar '{}':", builder.get_name());
        builder.get_parsing_table().print(builder.get_symbol_table(), 4);
    }
    if !builder.get_log().has_no_errors() {
        return Err(builder.give_log());
    }
    builder.set_parents_have_value();
    builder.add_lib("lexiparser_types::*");
    builder.use_full_lib(true);
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
    use lexigram_lib::file_utils::get_tagged_source;
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
