// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use iter_index::IndexerIterator;
use lexigram_lib::grammar::{ruleflag, Symbol};
use lexigram_lib::CollectJoin;
use lexigram_lib::log::Logger;
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::test_tools::replace_tagged_source;
use lexigram_lib::grammar::ProdRuleSetTables;
use lexigram_lib::{hashmap, prod, prodf};
use super::{GRAMPARSER_FILENAME, GRAMPARSER_TAG};

/// Generates Lexi's parser source code from the grammar file and from the symbols in the lexicon
/// (extracted in [`build_lexilexer`](crate::build_lexilexer::lexilexer_source())).
fn gramparser_source(indent: usize, verbose: bool) -> Result<String, String> {
    pub fn print_flags(builder: &ParserGen, indent: usize) {
        let tbl = builder.get_symbol_table();
        let prefix = format!("{:width$}//", "", width = indent);
        let nt_flags = builder.get_parsing_table().flags.iter().index().filter_map(|(nt, &f)|
            if f != 0 { Some(format!("{prefix}  - {}: {} ({})", Symbol::NT(nt).to_str(tbl), ruleflag::to_string(f).join(" | "), f)) } else { None }
        ).join("\n");
        let parents = builder.get_parsing_table().parent.iter().index().filter_map(|(c, &par)|
            if let Some(p) = par { Some(format!("{prefix}  - {} -> {}", Symbol::NT(c).to_str(tbl), Symbol::NT(p).to_str(tbl))) } else { None }
        ).join("\n");
        println!("{prefix} NT flags:\n{}", if nt_flags.is_empty() { format!("{prefix}  - (nothing)") } else { nt_flags });
        println!("{prefix} parents:\n{}", if parents.is_empty() { format!("{prefix}  - (nothing)") } else { parents });
    }

    // [versions]

    // lexigram_lib: 0.3.0
    // lexigram: 0.3.0
    // build-stage1: 0.3.0

    // [versions]

    // -------------------------------------------------------------------------
    // [gramparser_stage_2]

    let ll1_tables = ProdRuleSetTables::new(
        "GramParser",
        vec![
            prod!(nt 1, nt 2),
            prod!(t 8, t 12, t 6),
            prod!(nt 3, nt 10),
            prod!(nt 4, t 0, nt 5, nt 12),
            prod!(t 12),
            prod!(nt 6, nt 11),
            prod!(nt 9),
            prod!(nt 8, nt 13),
            prod!(t 12; t 10; t 11; t 1, nt 5, t 5),
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
        vec![("Colon", Some(":")), ("Lparen", Some("(")), ("Or", Some("|")), ("Plus", Some("+")), ("Question", Some("?")), ("Rparen", Some(")")), ("Semicolon", Some(";")), ("Star", Some("*")), ("Grammar", Some("grammar")), ("SymEof", Some("EOF")), ("Lform", None), ("Rform", Some("<R>")), ("Id", None)],
        vec!["file", "header", "rules", "rule", "rule_name", "prod", "prod_factor", "prod_term", "term_item", "prod_factor_1", "rules_1", "prod_1", "rule_1", "prod_term_1"],
        vec![0, 0, 512, 32, 0, 512, 2048, 32, 0, 1, 4, 4, 64, 64],
        vec![None, None, None, None, None, None, None, None, None, Some(6), Some(2), Some(5), Some(3), Some(7)],
        Some(0),
        hashmap![]
    );

    // [gramparser_stage_2]
    // -------------------------------------------------------------------------

    // - gets data from stage 1
    let name = ll1_tables.get_name().to_string();
    let ll1 = ll1_tables.make_prod_rule_set();

    // - generates Gram's parser source code (parser + listener):
    let mut builder = ParserGen::from_rules(ll1, name.clone());
    let msg = builder.get_log().get_messages().map(|s| format!("\n- {s}")).join("");
    if verbose {
        print_flags(&builder, 4);
        println!("Parsing table of grammar '{name}':");
        builder.get_parsing_table().print(builder.get_symbol_table(), 4);
        if !builder.get_log().is_empty() {
            println!("Messages:{msg}");
        }
    }
    if !builder.get_log().has_no_errors() {
        return Err(msg);
    }
    builder.set_parents_have_value();
    builder.add_lib("gramparser_types::*");
    Ok(builder.build_source_code(indent, true))
}

pub fn write_gramparser() {
    let result_src = gramparser_source(0, true)
        .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
        .unwrap();
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
        let result_src = gramparser_source(0, VERBOSE)
            .inspect_err(|e| eprintln!("Failed to parse grammar: {e:?}"))
            .unwrap();
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
