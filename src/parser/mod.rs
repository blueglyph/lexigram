use crate::grammar::LLParsingTable;
use crate::symbol_table::SymbolTable;

mod tests;

pub struct Parser {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
}

impl Parser {
    pub fn new(parsing_table: LLParsingTable, symbol_table: SymbolTable) -> Self {
        Parser { parsing_table, symbol_table }
    }
}