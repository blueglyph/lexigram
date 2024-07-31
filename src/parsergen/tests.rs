#![cfg(test)]

mod gen_integration {
    use std::fs::File;
    use std::io::Read;
    use crate::grammar::ProdRuleSet;
    use crate::grammar::tests::build_prs;
    use crate::{CollectJoin, LL1};
    use crate::parsergen::ParserBuilder;

    fn get_source(prs_id: u32, indent: usize) -> String {
        let rules = build_prs(prs_id, false);
        let ll1 = ProdRuleSet::<LL1>::from(rules);
        let builder = ParserBuilder::from_rules(ll1);
        builder.build_source_code(indent, false)
    }

    fn get_integration_source(tag: &str) -> String {
        const FILENAME: &str = "tests/integration/main.rs";
        let file_tag = format!("[{tag}]");
        let mut file = File::open(FILENAME).expect("Couldn't open source file");
        let mut buffer = String::new();
        file.read_to_string(&mut buffer).expect("Couldn't read source file");
        let mut result = buffer.lines()
            .skip_while(|l| !l.contains(&file_tag))
            .skip(1)
            .take_while(|l| !l.contains(&file_tag))
            .join("\n");
        result.push('\n');
        result
    }

    fn get_test_data<'a>(id: u32) -> Option<(u32, usize, &'a str)> {
        match id {
            0 => Some((13, 8, "write_source_code_from_ll1")),
            1 => Some(( 4, 4, "write_source_code_for_integration_listener")),
            2 => Some((13, 4, "write_source_code_for_integration_listener2")),
            3 => Some((20, 4, "write_source_code_for_integration_listener3")),
            4 => Some((30, 4, "write_source_code_for_integration_listener4")),
            5 => Some((31, 4, "write_source_code_for_integration_listener5")),
            6 => Some((32, 4, "write_source_code_for_integration_listener6")),
            _ => None
        }
    }

    fn do_test(id: u32, verbose: bool) -> bool {
        if let Some((prs_id, indent, tag)) = get_test_data(id) {
            let expected = get_source(prs_id, indent);
            if verbose {
                let s = String::from_utf8(vec![32; indent]).unwrap();
                println!("{s}// [{tag}]\n{expected}{s}// [{tag}]");
            }
            let result = get_integration_source(tag);
            assert_eq!(result, expected, "test failed for {id} / {prs_id} / {tag}");
            true
        } else {
            false
        }
    }

    #[test]
    fn verify_integration_sources() {
        for i in 0_u32.. {
            if !do_test(i, false) { break }
            // println!("{i} OK");
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_from_ll1() {
        do_test(0, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener() {
        do_test(1, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener2() {
        do_test(2, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener3() {
        do_test(3, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener4() {
        do_test(4, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener5() {
        do_test(5, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener6() {
        do_test(6, true);
    }
}