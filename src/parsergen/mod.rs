use std::fs::File;
use std::io::{BufWriter, Write};
use crate::grammar::{LL1, LLParsingTable, ProdRuleSet, RuleTreeSet, Symbol};
use crate::{CollectJoin, General, Normalized};
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
    pub fn from_tree(tree: RuleTreeSet<General>) -> Self {
        let normalized = RuleTreeSet::<Normalized>::from(tree);
        let lr_rules = ProdRuleSet::from(normalized);
        Self::from_rules(lr_rules)
    }

    pub fn from_rules<T>(rules: ProdRuleSet<T>) -> Self where ProdRuleSet<LL1>: From<ProdRuleSet<T>>, T: std::fmt::Debug {
        let ll1_rules = ProdRuleSet::<LL1>::from(rules);
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

    fn symbol_to_code(s: &Symbol) -> String {
        match s {
            Symbol::Empty => "Symbol::Empty".to_string(),
            Symbol::T(t) => format!("Symbol::T({t})"),
            Symbol::NT(nt) => format!("Symbol::T({nt})"),
            Symbol::End => "Symbol::End".to_string(),
        }
    }

    pub fn write_source_code(self, file: Option<File>) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn std::io::Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };

        // Create source code:
        writeln!(out, "// -------------------------------------------------------------------------")?;
        writeln!(out, "// Automatically generated\n")?;
        writeln!(out, "use crate::grammar::{{Symbol, VarId}};")?;
        writeln!(out, "use crate::parser::Parser;")?;
        writeln!(out, "use crate::symbol_table::SymbolTable;\n")?;

        writeln!(out, "const SYMBOLS_T: [(&str, Option<&str>); {}] = [{}];",
                 self.symbol_table.get_terminals().len(),
                 self.symbol_table.get_terminals().iter().map(|(s, os)|
                     format!("(\"{s}\", {})", os.as_ref().map(|s| format!("Some(\"{s}\")")).unwrap_or("None".to_string()))).join(", "))?;
        writeln!(out, "const SYMBOLS_NT: [&str; {}] = [{}];",
                 self.symbol_table.get_non_terminals().len(),
                 self.symbol_table.get_non_terminals().iter().map(|s| format!("\"{s}\"")).join(", "))?;
        writeln!(out, "const SYMBOLS_NAMES: [(&str, VarId); {}] = [{}];",
                 self.symbol_table.get_names().count(),
                 self.symbol_table.get_names().map(|(s, v)| format!("(\"{s}\", {v})")).join(", "))?;
        writeln!(out, "const PARSING_FACTORS: [(VarId, &[Symbol]); {}] = [{}];",
                 self.parsing_table.factors.len(),
                 self.parsing_table.factors.iter().map(|(v, f)| format!("({v}, &[{}])", f.iter().map(|s| Self::symbol_to_code(s)).join(", "))).join(", "))?;
        writeln!(out, "const PARSING_TABLE: [VarId; {}] = [{}];\n",
                 self.parsing_table.table.len(),
                 self.parsing_table.table.iter().map(|v| format!("{v}")).join(", "))?;

        writeln!(out, "pub(super) fn build_parser() -> Parser {{")?;
        writeln!(out, "    let mut symbol_table = SymbolTable::new();")?;
        writeln!(out, "    symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));")?;
        writeln!(out, "    symbol_table.extend_non_terminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));")?;
        writeln!(out, "    symbol_table.extend_names(SYMBOLS_NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));")?;
        writeln!(out, "    let factors: Vec<(VarId, Vec<Symbol>)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, s.to_vec())).collect();")?;
        writeln!(out, "    let table: Vec<VarId> = PARSING_TABLE.into();")?;
        writeln!(out, "    let parsing_table = crate::grammar::LLParsingTable {{")?;
        writeln!(out, "        num_nt: 5,")?;
        writeln!(out, "        num_t: 9,")?;
        writeln!(out, "        factors,")?;
        writeln!(out, "        table")?;
        writeln!(out, "    }};")?;
        writeln!(out, "    Parser::new(parsing_table, symbol_table)")?;
        writeln!(out, "}}")?;

        Ok(())
    }
}
