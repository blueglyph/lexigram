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

mod wrapper_source {
    use std::collections::BTreeMap;
    use crate::grammar::{Symbol, VarId};
    use crate::grammar::tests::{symbol_to_macro, T};
    use crate::{btreemap, CollectJoin, symbols};
    use crate::grammar::tests::T::PRS;
    use crate::parsergen::ParserBuilder;
    use crate::dfa::TokenId;

    fn print_items(builder: &ParserBuilder, result_items: &BTreeMap<VarId, Vec<Symbol>>, indent: usize) {
        let fields = (0..builder.parsing_table.factors.len())
            .filter_map(|f| {
                let f_id = f as VarId;
                if let Some(it) = result_items.get(&f_id) {
                    Some((
                        format!("{f_id} => symbols![{}],", it.iter().map(|s| symbol_to_macro(s)).join(", ")),
                        format!("// {f_id}: {}", it.iter().map(|s| s.to_str(builder.get_symbol_table())).join(" ")),
                    ))
                } else {
                    None
                }
            }).to_vec();
        let width = fields.iter().fold(39, |acc, s| acc.max(s.0.len()));
        for (symbols, comment) in fields {
            println!("{:indent$}{symbols:width_a$} {comment}", "", width_a = width, indent = indent);
        }
    }

    #[test]
    #[allow(unused_doc_comments)]
    fn build_items() {
        let tests: Vec<(T, VarId, BTreeMap<VarId, Vec<Symbol>>)> = vec![
            /// A -> a | a b | a b c | a b d | e
            //  0: A -> a A_1   - ►A_1 a
            //  1: A -> e       - ◄1 e
            //  2: A_1 -> b A_2 - ►A_2 b
            //  3: A_1 -> ε     - ◄3
            //  4: A_2 -> c     - ◄4 c
            //  5: A_2 -> d     - ◄5 d
            //  6: A_2 -> ε     - ◄6
            // - NT flags:
            //   - A: parent_left_fact (32)
            //   - A_1: parent_left_fact | child_left_fact (96)
            //   - A_2: child_left_fact (64)
            // - parents:
            //   - A_1 -> A
            //   - A_2 -> A_1
            (PRS(28), 0, btreemap![
                0 => symbols![],                        // 0:
                1 => symbols![t 4],                     // 1: e
                2 => symbols![],                        // 2:
                3 => symbols![t 0],                     // 3: a
                4 => symbols![t 0, t 1, t 2],           // 4: a b c
                5 => symbols![t 0, t 1, t 3],           // 5: a b d
                6 => symbols![t 0, t 1],                // 6: a b
            ]),
            /// A -> A a | b c | b d
            //  0: A -> b A_2   - ►A_2 b
            //  1: A_1 -> a A_1 - ●A_1 ◄1 a
            //  2: A_1 -> ε     - ◄2
            //  3: A_2 -> c A_1 - ◄3 ►A_1 c
            //  4: A_2 -> d A_1 - ◄4 ►A_1 d
            // - NT flags:
            //   - A: parent_left_fact | parent_left_rec (544)
            //   - A_1: child_left_rec (4)
            //   - A_2: child_left_fact (64)
            // - parents:
            //   - A_1 -> A
            //   - A_2 -> A
            (PRS(33), 0, btreemap![
                0 => symbols![],                        // 0:
                1 => symbols![t 0],                     // 1: a
                2 => symbols![],                        // 2:
                3 => symbols![t 1, t 2],                // 3: b c
                4 => symbols![t 1, t 3],                // 4: b d
            ]),
            /*
            (PRS(), 0, btreemap![]),
            */
        ];
        const VERBOSE: bool = true;
        for (test_id, (rule_id, start_nt, expected_items)) in tests.into_iter().enumerate() {
            // if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
            let ll1 = rule_id.get_prs(test_id, start_nt, true);
            let builder = ParserBuilder::from_rules(ll1);
            let items = builder.build_item_ops();
            let result_items = items.into_iter().collect::<BTreeMap<VarId, Vec<Symbol>>>();
            if VERBOSE {
                println!("            ({rule_id:?}, {start_nt}, btreemap![", );
                print_items(&builder, &result_items, 16);
                println!("            ]),");
            }
            assert_eq!(result_items, expected_items, "test {test_id} {rule_id:?}/{start_nt} failed ");
        }
    }
}