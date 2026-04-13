use std::marker::PhantomData;
use lexigram_core::log::LogStatus;
use crate::build::BuildFrom;
use crate::{General, LALR};
use crate::grammar::ProdRuleSet;

impl ProdRuleSet<LALR> {

}

impl BuildFrom<ProdRuleSet<General>> for ProdRuleSet<LALR> {
    fn build_from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.remove_ambiguity();
            rules.transfer_alt_flags();
            rules.check_flags();
        }
        ProdRuleSet::<LALR> {
            prules: rules.prules,
            origin: rules.origin,
            num_nt: rules.num_nt,
            num_t: rules.num_t,
            symbol_table: rules.symbol_table,
            flags: rules.flags,
            parent: rules.parent,
            start: rules.start,
            name: rules.name,
            nt_conversion: rules.nt_conversion,
            log: rules.log,
            options: rules.options,
            _phantom: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct LALRParsingTable {
}

impl LALRParsingTable {
    pub fn new() -> Self {
        LALRParsingTable { }
    }
}

impl Default for LALRParsingTable {
    fn default() -> Self {
        Self::new()
    }
}
