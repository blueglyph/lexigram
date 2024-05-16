#[cfg(test)]

use crate::grammar::{Symbol, VarId};
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;

const T: [(&str, Option<&str>); 8] = [("PLUS", Some("+")), ("MINUS", Some("-")), ("MUL", Some("*")), ("DIV", Some("/")), ("LPAREN", Some("(")), ("RPAREN", Some(")")), ("NUM", None), ("ID", None)];
const NT: [&str; 5] = ["E", "T", "F", "E_1", "T_1"];
const NAMES: [(&str, VarId); 2] = [("T_1", 4), ("E_1", 3)];
const FACTORS: [(VarId, &[Symbol]); 11] = [(0, &[Symbol::NT(1), Symbol::NT(3)]), (1, &[Symbol::NT(2), Symbol::NT(4)]), (2, &[Symbol::T(4), Symbol::NT(0), Symbol::T(5)]), (2, &[Symbol::T(6)]), (2, &[Symbol::T(7)]), (3, &[Symbol::T(0), Symbol::NT(1), Symbol::NT(3)]), (3, &[Symbol::T(1), Symbol::NT(1), Symbol::NT(3)]), (3, &[Symbol::Empty]), (4, &[Symbol::T(2), Symbol::NT(2), Symbol::NT(4)]), (4, &[Symbol::T(3), Symbol::NT(2), Symbol::NT(4)]), (4, &[Symbol::Empty])];
const TABLE: [VarId; 45] = [11, 11, 11, 11, 0, 11, 0, 0, 11, 11, 11, 11, 11, 1, 11, 1, 1, 11, 11, 11, 11, 11, 2, 11, 3, 4, 11, 5, 6, 11, 11, 11, 7, 11, 11, 11, 10, 10, 8, 9, 11, 10, 11, 11, 11];

fn parser() -> Parser {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_terminals(T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));
    symbol_table.extend_non_terminals(NT.into_iter().map(|s| s.to_string()));
    symbol_table.extend_names(NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));
    let factors: Vec<(VarId, Vec<Symbol>)> = FACTORS.into_iter().map(|(v, s)| (v, s.to_vec())).collect();
    let table: Vec<VarId> = TABLE.into();
    let parsing_table = crate::grammar::LLParsingTable {
        num_nt: 5,
        num_t: 9,
        factors,
        table
    };
    Parser::new(parsing_table, symbol_table)
}

#[test]
fn write_source_code() {
    let p = parser();
}
