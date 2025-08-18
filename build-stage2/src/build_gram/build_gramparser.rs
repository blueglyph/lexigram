// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::LL1;
use lexigram_lib::log::{BufLog, BuildFrom, LogReader, LogStatus, Logger};
use lexigram_lib::parsergen::{print_flags, ParserGen};
use lexigram_lib::test_tools::replace_tagged_source;
use lexigram_lib::grammar::{ProdRuleSet, ProdRuleSetTables};
use lexigram_lib::{hashmap, prod, prodf};
use super::{GRAMPARSER_FILENAME, GRAMPARSER_TAG};

const EXPECTED_NBR_WARNINGS: usize = 0;

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn gramparser_source(indent: usize, verbose: bool) -> Result<(BufLog, String), BufLog> {
    // [versions]

    // lexigram_lib: 0.5.2
    // lexigram: 0.5.2
    // build-stage1: 0.5.2

    // [versions]

    // -------------------------------------------------------------------------
    // [gramparser_stage_2]

    let ll1_tables = ProdRuleSetTables::new(
        Some("GramParser"),
        vec![
            prod!(nt 1, nt 2),
            prod!(t 8, t 13, t 6),
            prod!(nt 3, nt 10),
            prod!(nt 4, t 0, nt 5, nt 12),
            prod!(t 13),
            prod!(nt 6, nt 11),
            prod!(nt 9),
            prod!(nt 8, nt 13),
            prod!(t 13; t 10; t 11; t 12; t 1, nt 5, t 5),
            prod!(nt 7, nt 9; e),
            prod!(#(0, 0), nt 3, nt 10; e),
            prod!(#(0, 1), t 2, nt 6, nt 11; e),
            prod!(t 6; t 9, t 6),
            prod!(t 3; t 4; t 7; e),
        ],
        vec![
            prodf!(nt 2, nt 3),
            prodf!(nt 5, t 2, nt 6),
        ],
        vec![("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Pform", Some("<P>")), ("Id", None)],
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
    use lexigram_lib::test_tools::get_tagged_source;
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
