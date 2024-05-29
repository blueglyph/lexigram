#![cfg(test)]

use crate::grammar::ProdRuleSet;
use crate::grammar::tests::build_prs;
use crate::LL1;
use crate::parsergen::ParserBuilder;

#[ignore]
#[test]
fn write_source_code_from_ll1() {
    let rules = build_prs(13);
    let ll1 = ProdRuleSet::<LL1>::from(rules);
    let builder = ParserBuilder::from_rules(ll1);
    match builder.write_source_code(None) {
        Ok(_) => {}
        Err(e) => { println!("Error: {e}"); }
    }
}

#[ignore]
#[test]
fn write_source_code_from_lr() {
    let rules = build_prs(4);
    let builder = ParserBuilder::from_rules(rules);
    match builder.write_source_code(None) {
        Ok(_) => {}
        Err(e) => { println!("Error: {e}"); }
    }
}

#[cfg(disabled)]
#[test]
fn test_source_code_manual() {
    let _parser = source_code_manual::build_parser();
}

#[cfg(disabled)]
mod source_code_manual {
    // -------------------------------------------------------------------------
    // Automatically generated

    use crate::grammar::{Symbol, VarId};
    use crate::parser::Parser;
    use crate::symbol_table::SymbolTable;

    const SYMBOLS_T: [(&str, Option<&str>); 8] = [("PLUS", Some("+")), ("MINUS", Some("-")), ("MUL", Some("*")), ("DIV", Some("/")), ("LPAREN", Some("(")), ("RPAREN", Some(")")), ("NUM", None), ("ID", None)];
    const SYMBOLS_NT: [&str; 5] = ["E", "T", "F", "E_1", "T_1"];
    const SYMBOLS_NAMES: [(&str, VarId); 2] = [("T_1", 4), ("E_1", 3)];
    const PARSING_FACTORS: [(VarId, &[Symbol]); 11] = [(0, &[Symbol::T(1), Symbol::T(3)]), (1, &[Symbol::T(2), Symbol::T(4)]), (2, &[Symbol::T(4), Symbol::T(0), Symbol::T(5)]), (2, &[Symbol::T(6)]), (2, &[Symbol::T(7)]), (3, &[Symbol::T(0), Symbol::T(1), Symbol::T(3)]), (3, &[Symbol::T(1), Symbol::T(1), Symbol::T(3)]), (3, &[Symbol::Empty]), (4, &[Symbol::T(2), Symbol::T(2), Symbol::T(4)]), (4, &[Symbol::T(3), Symbol::T(2), Symbol::T(4)]), (4, &[Symbol::Empty])];
    const PARSING_TABLE: [VarId; 45] = [11, 11, 11, 11, 0, 11, 0, 0, 11, 11, 11, 11, 11, 1, 11, 1, 1, 11, 11, 11, 11, 11, 2, 11, 3, 4, 11, 5, 6, 11, 11, 11, 7, 11, 11, 11, 10, 10, 8, 9, 11, 10, 11, 11, 11];
    const START_SYMBOL: VarId = 0;

    pub(super) fn build_parser() -> Parser {
        let mut symbol_table = SymbolTable::new();
        symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));
        symbol_table.extend_non_terminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));
        symbol_table.extend_names(SYMBOLS_NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));
        let factors: Vec<(VarId, Vec<Symbol>)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, s.to_vec())).collect();
        let table: Vec<VarId> = PARSING_TABLE.into();
        let parsing_table = crate::grammar::LLParsingTable {
            num_nt: 5,
            num_t: 9,
            factors,
            table
        };
        Parser::new(parsing_table, symbol_table, START_SYMBOL)
    }
}