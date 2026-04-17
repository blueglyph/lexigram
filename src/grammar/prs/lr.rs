use std::marker::PhantomData;
use lexigram_core::log::LogStatus;
use crate::build::BuildFrom;
use crate::{General, LR};
use crate::grammar::ProdRuleSet;

impl ProdRuleSet<LR> {

}

impl BuildFrom<ProdRuleSet<General>> for ProdRuleSet<LR> {
    fn build_from(mut rules: ProdRuleSet<General>) -> Self {
        if rules.log.has_no_errors() {
            rules.remove_ambiguity();
            rules.transfer_alt_flags();
            rules.check_flags();
        }
        ProdRuleSet::<LR> {
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
pub struct LRParsingTable {
}

impl LRParsingTable {
    pub fn new() -> Self {
        LRParsingTable { }
    }
}

impl Default for LRParsingTable {
    fn default() -> Self {
        Self::new()
    }
}
