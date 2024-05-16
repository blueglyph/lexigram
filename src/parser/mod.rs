use crate::grammar::{LLParsingTable, Symbol, VarId};
use crate::symbol_table::SymbolTable;

mod tests;

pub struct Parser {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
    start: VarId
}

impl Parser {
    pub fn new(parsing_table: LLParsingTable, symbol_table: SymbolTable, start: VarId) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        Parser { parsing_table, symbol_table, start }
    }
}