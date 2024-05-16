use std::fs::File;
use std::io::BufWriter;
use crate::grammar::{LL1, LLParsingTable, ProdRuleSet, RuleTreeSet};
use crate::{General, Normalized};
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;

mod tests;

// ---------------------------------------------------------------------------------------------

#[allow(unused)]
pub struct ParserBuilder {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
}

impl ParserBuilder {
    pub fn from_rules(rules: RuleTreeSet<General>) -> Self {
        let normalized = RuleTreeSet::<Normalized>::from(rules);
        let lr_rules = ProdRuleSet::from(normalized);
        let ll1_rules = ProdRuleSet::<LL1>::from(lr_rules);
        let parsing_table = ll1_rules.create_parsing_table();
        let symbol_table = ll1_rules.symbol_table().expect(stringify!("symbol table is requires to create a {}", std::any::type_name::<Self>()));
        ParserBuilder {
            parsing_table,
            symbol_table
        }
    }

    pub fn make_parser(self) -> Parser {
        Parser::new(self.parsing_table, self.symbol_table)
    }

    pub fn write_source_code(self, file: Option<File>) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn std::io::Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };
        Ok(())
    }
}

