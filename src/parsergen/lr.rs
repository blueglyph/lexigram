use std::marker::PhantomData;
use lexigram_core::fixed_sym_table::{FixedSymTable, SymInfoTable};
use lexigram_core::log::LogStatus;
use lexigram_core::parser::lr_parser::{LRAction, LRParser, LRStateId};
use lexigram_core::{CollectJoin, VarId};
use crate::build::BuildFrom;
use crate::grammar::ProdRuleSet;
use crate::{LALR, LR};

/// Tables and parameters used to create a [`LRParser`]. This type is used as a return object from the parser generator,
/// when the LRParser must be created dynamically; for example, in tests or in situations where the grammar isn't
/// known in advance. In those situations, the LRParserTables object must live as long as the parser it generates.
///
/// The LRParser itself uses references to tables whenever possible because, in most situations, the tables are
/// static in generated source files. A few fields must still be created dynamically from (possibly) static
/// tables because they don't exist in static form.
#[derive(Default, Debug)]
pub struct LRParserTables<T> {
    num_nt: usize,
    num_t_full: usize,                  // includes the end symbol
    action: Vec<LRAction>,
    goto: Vec<LRStateId>,
    alt_nt_len: Vec<(VarId, u16, u16)>, // alt_id -> (nt, # symbols in alt, # terminals in alt)
    symbol_table: FixedSymTable,        // must include terminals <$> and <empty> at the end
    _phantom: PhantomData<T>,
}

impl<T> LRParserTables<T> {
    pub fn new(
        num_nt: usize,
        num_t_full: usize,
        action: Vec<LRAction>,
        goto: Vec<LRStateId>,
        alt_nt_len: Vec<(VarId, u16, u16)>,
        symbol_table: FixedSymTable
    ) -> Self {
        LRParserTables { num_nt, num_t_full, action, goto, alt_nt_len, symbol_table, _phantom: PhantomData }
    }

    pub fn make_parser(&self) -> LRParser<'_, T> {
        LRParser::new(
            self.num_nt,
            self.num_t_full,
            &self.action,
            &self.goto,
            &self.alt_nt_len,
            self.symbol_table.clone())
    }
}

// ---------------------------------------------------------------------------------------------

impl BuildFrom<ProdRuleSet<LR>> for LRParserTables<LALR> {
    fn build_from(mut source: ProdRuleSet<LR>) -> Self {
        let table = source.make_parsing_table_lalr();
        if !source.has_no_errors() {
            panic!("creation of LALR parsing table failed:{}", source.log);
        }
        let Some(mut symtable) = source.symbol_table.clone() else {
            panic!("no symbol table in ProdRuleSet<LR>");
        };
        symtable.add_terminal("<$>", Some("<$>"));
        symtable.add_terminal("<empty>", Some("<empty>"));
        LRParserTables::new(
            table.num_nt,
            table.num_t_full,
            table.action,
            table.goto,
            table.alts.into_iter()
                .enumerate()
                .map(|(i, (nt, alt))| (
                    // nonterminal index:
                    nt,
                    // number of symbols in production alternative:
                    u16::try_from(if alt.is_sym_empty() { 0 } else { alt.len() })
                        .expect(&format!("alt[{i}] too long:\n{}", alt.to_str(source.get_symbol_table()))),
                    // number of (variable) terminals containing data:
                    alt.iter().filter(|s| symtable.is_symbol_t_data(s)).count() as u16
                ))
                .to_vec(),
            symtable.to_fixed_sym_table(),
        )
    }
}
