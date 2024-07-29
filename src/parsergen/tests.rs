#![cfg(test)]

mod gen_integration {
    use crate::grammar::ProdRuleSet;
    use crate::grammar::tests::build_prs;
    use crate::LL1;
    use crate::parsergen::ParserBuilder;

    #[ignore]
    #[test]
    fn write_source_code_from_ll1() {
        // Copy the output into tests/integration.rs, module parser_gen:
        let rules = build_prs(13, false);
        let ll1 = ProdRuleSet::<LL1>::from(rules);
        let builder = ParserBuilder::from_rules(ll1);
        match builder.write_source_code(None, 8) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener() {
        // Copy the output into tests/integration.rs, module listener:
        let rules = build_prs(4, false);
        let ll1 = ProdRuleSet::<LL1>::from(rules);
        let builder = ParserBuilder::from_rules(ll1);
        match builder.write_source_code(None, 4) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener2() {
        // Copy the output into tests/integration.rs, module listener2:
        let rules = build_prs(13, false);
        let builder = ParserBuilder::from_rules(rules);
        match builder.write_source_code(None, 4) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener3() {
        // Copy the output into tests/integration.rs, module listener3:
        let rules = build_prs(20, false);
        let builder = ParserBuilder::from_rules(rules);
        match builder.write_source_code(None, 4) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener4() {
        // Copy the output into tests/integration.rs, module listener4:
        let rules = build_prs(30, false);
        let builder = ParserBuilder::from_rules(rules);
        match builder.write_source_code(None, 4) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener5() {
        // Copy the output into tests/integration.rs, module listener5:
        let rules = build_prs(31, false);
        let builder = ParserBuilder::from_rules(rules);
        match builder.write_source_code(None, 4) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener6() {
        // Copy the output into tests/integration.rs, module listener6:
        let rules = build_prs(32, false);
        let builder = ParserBuilder::from_rules(rules);
        match builder.write_source_code(None, 4) {
            Ok(_) => {}
            Err(e) => { println!("Error: {e}"); }
        }
    }
}