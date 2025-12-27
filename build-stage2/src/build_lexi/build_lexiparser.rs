// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashMap;
use lexigram_lib::{gnode, LL1, VarId};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::BuildFrom;
use lexigram_lib::parsergen::{print_flags, ParserGen};
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

    static ORIGIN: [(Option<usize>, &[(GrNode, &[usize])]); 17] = [
        (Some(7), &[(gnode!(*), &[1]), (gnode!(nt 1), &[]), (gnode!(nt 2), &[]), (gnode!(inst), &[0]), (gnode!(&), &[2,3]), (gnode!(inst), &[0]), (gnode!(&), &[5]), (gnode!(|), &[4,6])]),
        (Some(3), &[(gnode!(nt 4), &[]), (gnode!(nt 3), &[]), (gnode!(nt 5), &[]), (gnode!(|), &[0,1,2])]),
        (Some(3), &[(gnode!(t 18), &[]), (gnode!(t 26), &[]), (gnode!(t 14), &[]), (gnode!(&), &[0,1,2])]),
        (Some(3), &[(gnode!(t 19), &[]), (gnode!(t 26), &[]), (gnode!(t 14), &[]), (gnode!(&), &[0,1,2])]),
        (Some(9), &[(gnode!(*), &[3]), (gnode!(t 2), &[]), (gnode!(t 26), &[]), (gnode!(&), &[1,2]), (gnode!(t 16), &[]), (gnode!(t 5), &[]), (gnode!(t 26), &[]), (gnode!(inst), &[0]), (gnode!(t 12), &[]), (gnode!(&), &[4,5,6,7,8])]),
        (Some(17), &[(gnode!(nt 6), &[]), (gnode!(t 1), &[]), (gnode!(nt 10), &[]), (gnode!(t 14), &[]), (gnode!(&), &[0,1,2,3]), (gnode!(nt 7), &[]), (gnode!(t 1), &[]), (gnode!(nt 10), &[]), (gnode!(t 0), &[]), (gnode!(nt 8), &[]), (gnode!(t 14), &[]), (gnode!(&), &[5,6,7,8,9,10]), (gnode!(nt 7), &[]), (gnode!(t 1), &[]), (gnode!(nt 10), &[]), (gnode!(t 14), &[]), (gnode!(&), &[12,13,14,15]), (gnode!(|), &[4,11,16])]),
        (Some(2), &[(gnode!(t 17), &[]), (gnode!(t 26), &[]), (gnode!(&), &[0,1])]),
        (Some(0), &[(gnode!(t 26), &[])]),
        (Some(6), &[(gnode!(*), &[3]), (gnode!(t 2), &[]), (gnode!(nt 9), &[]), (gnode!(&), &[1,2]), (gnode!(nt 9), &[]), (gnode!(inst), &[0]), (gnode!(&), &[4,5])]),
        (Some(23), &[(gnode!(t 19), &[]), (gnode!(t 6), &[]), (gnode!(t 26), &[]), (gnode!(t 13), &[]), (gnode!(&), &[0,1,2,3]), (gnode!(t 21), &[]), (gnode!(t 6), &[]), (gnode!(t 26), &[]), (gnode!(t 13), &[]), (gnode!(&), &[5,6,7,8]), (gnode!(t 20), &[]), (gnode!(t 23), &[]), (gnode!(t 22), &[]), (gnode!(t 24), &[]), (gnode!(t 6), &[]), (gnode!(t 26), &[]), (gnode!(t 13), &[]), (gnode!(&), &[13,14,15,16]), (gnode!(t 25), &[]), (gnode!(t 6), &[]), (gnode!(t 26), &[]), (gnode!(t 13), &[]), (gnode!(&), &[18,19,20,21]), (gnode!(|), &[4,9,10,11,12,17,22])]),
        (Some(0), &[(gnode!(nt 11), &[])]),
        (Some(6), &[(gnode!(*), &[3]), (gnode!(t 10), &[]), (gnode!(nt 12), &[]), (gnode!(&), &[1,2]), (gnode!(nt 12), &[]), (gnode!(inst), &[0]), (gnode!(&), &[4,5])]),
        (Some(2), &[(gnode!(+), &[1]), (gnode!(nt 13), &[]), (gnode!(inst), &[0])]),
        (Some(19), &[(gnode!(nt 14), &[]), (gnode!(t 15), &[]), (gnode!(t 11), &[]), (gnode!(&), &[0,1,2]), (gnode!(nt 14), &[]), (gnode!(t 15), &[]), (gnode!(&), &[4,5]), (gnode!(nt 14), &[]), (gnode!(t 9), &[]), (gnode!(t 11), &[]), (gnode!(&), &[7,8,9]), (gnode!(nt 14), &[]), (gnode!(t 9), &[]), (gnode!(&), &[11,12]), (gnode!(nt 14), &[]), (gnode!(t 11), &[]), (gnode!(&), &[14,15]), (gnode!(nt 14), &[]), (gnode!(&), &[17]), (gnode!(|), &[3,6,10,13,16,18])]),
        (Some(16), &[(gnode!(t 26), &[]), (gnode!(t 27), &[]), (gnode!(t 4), &[]), (gnode!(t 27), &[]), (gnode!(&), &[1,2,3]), (gnode!(t 27), &[]), (gnode!(&), &[5]), (gnode!(t 28), &[]), (gnode!(nt 15), &[]), (gnode!(t 6), &[]), (gnode!(nt 11), &[]), (gnode!(t 13), &[]), (gnode!(&), &[9,10,11]), (gnode!(t 7), &[]), (gnode!(nt 14), &[]), (gnode!(&), &[13,14]), (gnode!(|), &[0,4,6,7,8,12,15])]),
        (Some(8), &[(gnode!(+), &[1]), (gnode!(nt 16), &[]), (gnode!(t 30), &[]), (gnode!(inst), &[0]), (gnode!(t 31), &[]), (gnode!(&), &[2,3,4]), (gnode!(t 3), &[]), (gnode!(t 29), &[]), (gnode!(|), &[5,6,7])]),
        (Some(6), &[(gnode!(t 32), &[]), (gnode!(t 8), &[]), (gnode!(t 32), &[]), (gnode!(&), &[0,1,2]), (gnode!(t 32), &[]), (gnode!(t 29), &[]), (gnode!(|), &[3,4,5])]),
    ];
    static MAP: [(VarId, (VarId, usize)); 6] = [
        (17, (0, 0)), (18, (4, 0)), (19, (8, 0)), (20, (11, 0)), (21, (12, 0)),
        (22, (15, 0)),
    ];
    let origin = Origin::from_data(
        ORIGIN.into_iter().map(|(root, nodes)| GrTree::from((root, nodes.to_vec()))).collect(),
        HashMap::from(MAP));

    let ll1_tables = ProdRuleSetTables::new(
        Some("LexiParser"),
        vec![
            prule!(%(0, 4), nt 2, nt 17; %(0, 6), nt 17),
            prule!(%(1, 0), nt 4; %(1, 1), nt 3; %(1, 2), nt 5),
            prule!(%(2, 3), t 18, t 26, t 14),
            prule!(%(3, 3), t 19, t 26, t 14),
            prule!(%(4, 9), t 16, t 5, t 26, nt 18, t 12),
            prule!(%(5, 4), nt 6, t 1, nt 10, t 14; nt 7, t 1, nt 10, nt 23),
            prule!(%(6, 2), t 17, t 26),
            prule!(%(7, 0), t 26),
            prule!(%(8, 6), nt 9, nt 19),
            prule!(%(9, 4), t 19, t 6, t 26, t 13; %(9, 9), t 21, t 6, t 26, t 13; %(9, 10), t 20; %(9, 11), t 23; %(9, 12), t 22; %(9, 17), t 24, t 6, t 26, t 13; %(9, 22), t 25, t 6, t 26, t 13),
            prule!(%(10, 0), nt 11),
            prule!(%(11, 6), nt 12, nt 20),
            prule!(%(12, 2), nt 21),
            prule!(nt 14, nt 24),
            prule!(%(14, 12), t 6, nt 11, t 13; %(14, 15), t 7, nt 14; %(14, 0), t 26; t 27, nt 25; %(14, 7), t 28; %(14, 8), nt 15),
            prule!(%(15, 5), t 30, nt 22, t 31; %(15, 6), t 3; %(15, 7), t 29),
            prule!(%(16, 5), t 29; t 32, nt 26),
            prule!(%(0, 1), nt 1, nt 17; e),
            prule!(%(4, 3), t 2, t 26, nt 18; e),
            prule!(%(8, 3), t 2, nt 9, nt 19; e),
            prule!(%(11, 3), t 10, nt 12, nt 20; e),
            prule!(%(12, 1), nt 13, nt 27),
            prule!(%(15, 1), nt 16, nt 28),
            prule!(%(5, 11), t 0, nt 8, t 14; %(5, 16), t 14),
            prule!(t 9, nt 29; %(13, 16), t 11; t 15, nt 30; %(13, 18), e),
            prule!(%(14, 4), t 4, t 27; %(14, 6), e),
            prule!(%(16, 3), t 8, t 32; %(16, 4), e),
            prule!(%(12, 1), nt 21; %(12, 1), e),
            prule!(%(15, 1), nt 22; %(15, 1), e),
            prule!(%(13, 10), t 11; %(13, 13), e),
            prule!(%(13, 3), t 11; %(13, 6), e),
        ],
        origin,
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
