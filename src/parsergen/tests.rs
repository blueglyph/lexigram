#![cfg(test)]

use crate::grammar::ProdRuleSet;
use crate::grammar::tests::build_prs;
use crate::LL1;
use crate::parsergen::ParserBuilder;

#[ignore]
#[test]
fn write_source_code_from_ll1() {
    // Copy the output into tests/integration.rs, module parser_gen:
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
