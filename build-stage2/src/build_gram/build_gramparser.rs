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
use super::{GRAMPARSER_FILENAME, GRAMPARSER_TAG};

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn gramparser_source(indent: usize, verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram-lib: 0.8.3
    // lexi-gram: 0.8.3
    // build-stage1: 0.8.3

    // [versions]

    // -------------------------------------------------------------------------
    // [gramparser_stage_2]

    static ORIGIN: [(Option<usize>, &[(GrNode, &[usize])]); 9] = [
        (Some(2), &[(gnode!(nt 1), &[]), (gnode!(nt 2), &[]), (gnode!(&), &[0,1])]),
        (Some(3), &[(gnode!(t 8), &[]), (gnode!(t 14), &[]), (gnode!(t 6), &[]), (gnode!(&), &[0,1,2])]),
        (Some(4), &[(gnode!(nt 3), &[]), (gnode!(nt 2), &[]), (gnode!(nt 3), &[]), (gnode!(&), &[1,2]), (gnode!(|), &[0,3])]),
        (Some(11), &[(gnode!(nt 4), &[]), (gnode!(t 0), &[]), (gnode!(nt 5), &[]), (gnode!(t 9), &[]), (gnode!(t 6), &[]), (gnode!(&), &[0,1,2,3,4]), (gnode!(nt 4), &[]), (gnode!(t 0), &[]), (gnode!(nt 5), &[]), (gnode!(t 6), &[]), (gnode!(&), &[6,7,8,9]), (gnode!(|), &[5,10])]),
        (Some(0), &[(gnode!(t 14), &[])]),
        (Some(5), &[(gnode!(nt 6), &[]), (gnode!(nt 5), &[]), (gnode!(t 2), &[]), (gnode!(nt 6), &[]), (gnode!(&), &[1,2,3]), (gnode!(|), &[0,4])]),
        (Some(2), &[(gnode!(*), &[1]), (gnode!(nt 7), &[]), (gnode!(inst), &[0])]),
        (Some(11), &[(gnode!(nt 8), &[]), (gnode!(t 3), &[]), (gnode!(&), &[0,1]), (gnode!(nt 8), &[]), (gnode!(t 7), &[]), (gnode!(&), &[3,4]), (gnode!(nt 8), &[]), (gnode!(t 4), &[]), (gnode!(&), &[6,7]), (gnode!(nt 8), &[]), (gnode!(&), &[9]), (gnode!(|), &[2,5,8,10])]),
        (Some(9), &[(gnode!(t 14), &[]), (gnode!(t 10), &[]), (gnode!(t 11), &[]), (gnode!(t 12), &[]), (gnode!(t 13), &[]), (gnode!(t 1), &[]), (gnode!(nt 5), &[]), (gnode!(t 5), &[]), (gnode!(&), &[5,6,7]), (gnode!(|), &[0,1,2,3,4,8])]),
    ];
    static MAP: [(VarId, (VarId, usize)); 1] = [
        (9, (6, 0)),
    ];
    let origin = Origin::from_data(
        ORIGIN.into_iter().map(|(root, nodes)| GrTree::from((root, nodes.to_vec()))).collect(),
        HashMap::from(MAP));

    let ll1_tables = ProdRuleSetTables::new(
        Some("GramParser"),
        vec![
            prule!(%(0, 2), nt 1, nt 2),
            prule!(%(1, 3), t 8, t 14, t 6),
            prule!(%(2, 0), nt 3, nt 10),
            prule!(nt 4, t 0, nt 5, nt 12),
            prule!(%(4, 0), t 14),
            prule!(%(5, 0), nt 6, nt 11),
            prule!(%(6, 2), nt 9),
            prule!(nt 8, nt 13),
            prule!(%(8, 0), t 14; %(8, 1), t 10; %(8, 2), t 11; %(8, 3), t 12; %(8, 4), t 13; %(8, 8), t 1, nt 5, t 5),
            prule!(%(6, 1), nt 7, nt 9; e),
            prule!(#(0, 0), %(2, 3), nt 3, nt 10; e),
            prule!(#(0, 1), %(5, 4), t 2, nt 6, nt 11; e),
            prule!(%(3, 10), t 6; %(3, 5), t 9, t 6),
            prule!(%(7, 2), t 3; %(7, 8), t 4; %(7, 5), t 7; %(7, 10), e),
        ],
        origin,
        vec![("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Pform", Some("<P>")), ("Greedy", Some("<G>")), ("Id", None)],
        vec!["file", "header", "rules", "rule", "rule_name", "prod", "prod_term", "prod_factor", "prod_atom", "prod_term_1", "rules_1", "prod_1", "rule_1", "prod_factor_1"],
        vec![0, 0, 512, 32, 0, 512, 2048, 32, 0, 1, 4, 4, 64, 64],
        vec![None, None, None, None, None, None, None, None, None, Some(6), Some(2), Some(5), Some(3), Some(7)],
        Some(0),
        hashmap![]
    );

    // [gramparser_stage_2]
    // -------------------------------------------------------------------------

    // - gets data from stage 1
    let ll1 = ProdRuleSet::<LL1>::build_from(ll1_tables);

    // - generates Gram's parser source code (parser + listener):
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
    builder.add_lib("gramparser_types::*");
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

pub fn write_gramparser() {
    let (log, result_src) = gramparser_source(0, true)
        .inspect_err(|log| panic!("Failed to build parser:\n{log}"))
        .unwrap();
    println!("Log:\n{log}");
    replace_tagged_source(GRAMPARSER_FILENAME, GRAMPARSER_TAG, &result_src)
        .expect("parser source replacement failed");
}

#[cfg(test)]
mod tests {
    use lexigram_lib::file_utils::get_tagged_source;
    use super::*;

    #[test]
    fn test_source() {
        const VERBOSE: bool = false;
        let (log, result_src) = gramparser_source(0, VERBOSE)
            .inspect_err(|log| panic!("Failed to build parser:\n{log}"))
            .unwrap();
        if !cfg!(miri) {
            if VERBOSE { println!("Log:\n{log}"); }
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
