// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use iter_index::IndexerIterator;
use crate::grammar::{grtree_to_str, ProdRuleSet, Symbol, VarId};
use crate::{CollectJoin, LL1};

fn get_original_str(ll1: &ProdRuleSet<LL1>, indent: usize) -> String {
    let symtab = ll1.get_symbol_table();
    ll1.origin.trees.iter().index::<VarId>()
        .filter_map(|(v, t)|
            if !t.is_empty() {
                Some(format!("{: <w$}// {} -> {}", "", Symbol::NT(v).to_str(symtab), grtree_to_str(t, None, None, Some(v), symtab, false), w=indent))
            } else {
                None
            })
        .join("\n")
}

mod gen_integration {
    use crate::grammar::ProdRuleSet;
    use crate::{CollectJoin, LL1};
    use crate::grammar::tests::TestRules;
    use crate::log::{BuildFrom, LogReader, LogStatus};
    use crate::parsergen::ParserGen;
    use crate::test_tools::{get_tagged_source, replace_tagged_source};

    fn get_source(tr_id: u32, indent: usize, include_alts: bool, name: String) -> String {
        let rules = TestRules(tr_id).to_prs_general().expect(&format!("invalid test rule ID #{tr_id}"));
        assert_eq!(rules.get_log().num_errors(), 0, "building {tr_id} failed:\n- {}", rules.get_log().get_errors().join("\n- "));
        let ll1 = ProdRuleSet::<LL1>::build_from(rules);
        let mut builder = ParserGen::build_from_rules(ll1, name);
        builder.set_include_alts(include_alts);
        builder.gen_source_code(indent, false)
    }

    fn get_test_data<'a>(id: u32) -> Option<(u32, usize, bool, &'a str, &'a str)> {
        match id {
            //          rules indent alts     tag name                                      listener name
            // those parsers are also used in other tests:
            1 => Some((580,    4,  true,    "write_source_code_for_integration_listener1", "Expr")),
            2 => Some((640,    4,  true,    "write_source_code_for_integration_listener2", "Expr")),
            3 => Some((641,    4,  true,    "write_source_code_for_integration_listener3", "Expr")),
            4 => Some((642,    4,  true,    "write_source_code_for_integration_listener4", "Expr")),
            5 => Some((862,    4,  true,    "write_source_code_for_integration_listener5", "Expr")),
            _ => None
        }
    }

    #[derive(Debug, Clone, Copy)]
    enum Action { VerifySource, WriteSource }
    #[derive(Debug, Clone, Copy)]
    enum SourceTestError { NoSuchTest, SourceNotFound, SourceDiffer }

    fn do_test(id: u32, action: Action, verbose: bool) -> Result<(), SourceTestError> {
        const FILENAME: &str = "tests/integration/parser_examples.rs";
        if let Some((tr_id, indent, include_alts, tag, name)) = get_test_data(id) {
            let source = get_source(tr_id, indent, include_alts, name.to_string());
            if verbose {
                let s = String::from_utf8(vec![32; indent]).unwrap();
                println!("{s}// [{tag}]\n{source}{s}// [{tag}]");
            }
            match action {
                Action::VerifySource => {
                    let result = get_tagged_source(FILENAME, tag).ok_or_else(|| {
                        println!("source not found for {id} / {tr_id} / {tag} ({name})");
                        SourceTestError::SourceNotFound
                    })?;
                    if result != source {
                        println!("source mismatch for {id} / {tr_id} / {tag} ({name})");
                        return Err(SourceTestError::SourceDiffer);
                    }
                }
                Action::WriteSource => {
                    replace_tagged_source(FILENAME, tag, source.as_str()).expect(&format!("couldn't write {FILENAME} / {tag}"));
                }
            }
            Ok(())
        } else {
            Err(SourceTestError::NoSuchTest)
        }
    }

    #[test]
    #[cfg(not(miri))]
    fn verify_integration_sources() {
        let mut nbr_error = 0;
        for i in 1_u32.. {
            match do_test(i, Action::VerifySource, false) {
                Err(SourceTestError::NoSuchTest) => break,
                Err(_) => nbr_error += 1,
                Ok(_) => {}
            }
        }
        if nbr_error > 0 { panic!("verification failed with {nbr_error} error(s)"); }
    }

    #[ignore]
    #[test]
    #[cfg(not(miri))]
    fn write_all_sources() {
        for i in 1_u32.. {
            match do_test(i, Action::WriteSource, false) {
                Ok(_) => println!("writing source for test {i}"),
                Err(SourceTestError::NoSuchTest) => break,
                Err(e) => panic!("error while writing source for test {i}: {e:?}"),
            }
        }
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener1() {
        do_test(1, Action::WriteSource, true).expect("couldn't write source #1");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener2() {
        do_test(2, Action::WriteSource, true).expect("couldn't write source #2");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener3() {
        do_test(3, Action::WriteSource, true).expect("couldn't write source #3");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener4() {
        do_test(4, Action::WriteSource, true).expect("couldn't write source #4");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener5() {
        do_test(5, Action::WriteSource, true).expect("couldn't write source #5");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener6() {
        do_test(6, Action::WriteSource, true).expect("couldn't write source #6");
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener7() {
        do_test(7, Action::WriteSource, true).expect("couldn't write source #7");
    }
}

pub mod opcodes {
    use crate::log::BuildFrom;
    use crate::grammar::{Symbol, VarId};
    use crate::{columns_to_str, strip, CollectJoin};
    use crate::grammar::tests::TestRules;
    use crate::parser::{OpCode, Parser};
    use crate::parsergen::{ParserGen, ParserTables};
    use crate::parsergen::tests::get_original_str;

    fn get_alts_str(parser: &Parser) -> Vec<String> {
        let pv = parser.get_alt_var();
        let pf = parser.get_alts();
        pv.iter().enumerate().map(|(id, v)|
            format!("{id:2}: {} -> {}",
                    Symbol::NT(*v).to_str(parser.get_symbol_table()),
                    if let Some(a) = pf.get(id) { a.iter().map(|s| s.to_str(parser.get_symbol_table())).join(" ") } else { "(alternative)".to_string() }
            )
        ).collect()
    }

    fn print_opcodes(parser: &Parser) {
        let alts = get_alts_str(&parser);
        if !alts.is_empty() {
            let indent = 16;
            let opcodes = alts.into_iter().zip(parser.get_opcodes()).map(|(s, ops)|
                vec![
                    format!("strip![{}],", ops.iter().map(|o| o.to_macro_item()).join(", ")),
                    format!("// {s}"),
                    format!("- {}", ops.into_iter().map(|s| s.to_str(parser.get_symbol_table())).join(" ")),
                ]
            ).to_vec();
            for l in columns_to_str(opcodes, Some(vec![40, 0, 0])) {
                println!("{:indent$}{l}", "", indent = indent)
            }
        }
    }

    #[test]
    fn parser_opcodes() {
        // terminal:     t (static) or t! (contains a string)
        // non-terminal: ►A
        // exit:         ◄2 (alternative #2)
        // loop:         ●1 (alternative #1)
        let tests: Vec<(u32, VarId, Vec<Vec<OpCode>>)> = vec![
            // ----------------------------------------------------------------- basic rules
            // a -> A b
            // b -> B
            (11, 0, vec![
                strip![exit 0, nt 1, t 0],              //  0: a -> A b - ◄0 ►b A!
                strip![exit 1, t 1],                    //  1: b -> B   - ◄1 B!
            ]),
            // a -> A | B
            (2, 0, vec![
                strip![exit 0, t 0],                    //  0: a -> A - ◄0 A!
                strip![exit 1, t 1],                    //  1: a -> B - ◄1 B!
            ]),
            // ----------------------------------------------------------------- +_or_*
            // a -> A B* C
            (102, 0, vec![
                strip![exit 0, t 2, nt 1, t 0],         //  0: a -> A a_1 C - ◄0 C! ►a_1 A!
                strip![loop 1, exit 1, t 1],            //  1: a_1 -> B a_1 - ●a_1 ◄1 B!
                strip![exit 2],                         //  2: a_1 -> ε     - ◄2
            ]),
            // a -> A B+ C
            (103, 0, vec![
                strip![exit 0, t 2, nt 1, t 0],         //  0: a -> A a_1 C - ◄0 C! ►a_1 A!
                strip![nt 2, t 1],                      //  1: a_1 -> B a_2 - ►a_2 B!
                strip![loop 1, exit 2],                 //  2: a_2 -> a_1   - ●a_1 ◄2
                strip![exit 3],                         //  3: a_2 -> ε     - ◄3
            ]),
            // a -> (A (b ",")* ";")* C
            // b -> B
            (106, 0, vec![
                strip![exit 0, t 3, nt 3],              //  0: a -> a_2 C         - ◄0 C! ►a_2
                strip![exit 1, t 4],                    //  1: b -> B             - ◄1 B!
                strip![loop 2, exit 2, t 1, nt 1],      //  2: a_1 -> b , a_1     - ●a_1 ◄2 , ►b
                strip![exit 3],                         //  3: a_1 -> ε           - ◄3
                strip![loop 3, exit 4, t 2, nt 2, t 0], //  4: a_2 -> A a_1 ; a_2 - ●a_2 ◄4 ; ►a_1 A!
                strip![exit 5],                         //  5: a_2 -> ε           - ◄5
            ]),
            // a -> (A (b ",")+ ";")+ C
            // b -> B
            (107, 0, vec![
                strip![exit 0, t 3, nt 3],              //  0: a -> a_2 C         - ◄0 C! ►a_2
                strip![exit 1, t 4],                    //  1: b -> B             - ◄1 B!
                strip![nt 4, t 1, nt 1],                //  2: a_1 -> b , a_3     - ►a_3 , ►b
                strip![nt 5, t 2, nt 2, t 0],           //  3: a_2 -> A a_1 ; a_4 - ►a_4 ; ►a_1 A!
                strip![loop 2, exit 4],                 //  4: a_3 -> a_1         - ●a_1 ◄4
                strip![exit 5],                         //  5: a_3 -> ε           - ◄5
                strip![loop 3, exit 6],                 //  6: a_4 -> a_2         - ●a_2 ◄6
                strip![exit 7],                         //  7: a_4 -> ε           - ◄7
            ]),
            // a -> (A | B)*
            (150, 0, vec![
                strip![exit 0, nt 1],                   //  0: a -> a_1     - ◄0 ►a_1
                strip![loop 1, exit 1, t 0],            //  1: a_1 -> A a_1 - ●a_1 ◄1 A!
                strip![loop 1, exit 2, t 1],            //  2: a_1 -> B a_1 - ●a_1 ◄2 B!
                strip![exit 3],                         //  3: a_1 -> ε     - ◄3
            ]),
            // ----------------------------------------------------------------- +_or_* <L>
            // a -> A (<L=i> B)* C
            (200, 0, vec![
                strip![exit 0, t 2, nt 1, t 0],         //  0: a -> A i C - ◄0 C! ►i A!
                strip![loop 1, exit 1, t 1],            //  1: i -> B i   - ●i ◄1 B!
                strip![exit 2],                         //  2: i -> ε     - ◄2
            ]),
            // a -> A (<L=i> B)+ C
            (201, 0, vec![
                strip![exit 0, t 2, nt 1, t 0],         //  0: a -> A i C - ◄0 C! ►i A!
                strip![nt 2, t 1],                      //  1: i -> B a_1 - ►a_1 B!
                strip![loop 1, exit 2],                 //  2: a_1 -> i   - ●i ◄2
                strip![exit 3],                         //  3: a_1 -> ε   - ◄3
            ]),
            // ----------------------------------------------------------------- right_rec
            // expr -> Num "^" expr | Num
            (302, 0, vec![
                strip![nt 1, t 0],                      //  0: expr -> Num expr_1 - ►expr_1 Num!
                strip![exit 1, nt 0, t 1],              //  1: expr_1 -> ^ expr   - ◄1 ►expr ^
                strip![exit 2],                         //  2: expr_1 -> ε        - ◄2
            ]),
            // ----------------------------------------------------------------- right_rec <L>
            // expr -> <L> Num "^" expr | Num
            (402, 0, vec![
                strip![nt 1, t 0],                      //  0: expr -> Num expr_1 - ►expr_1 Num!
                strip![loop 0, exit 1, t 1],            //  1: expr_1 -> ^ expr   - ●expr ◄1 ^
                strip![exit 2],                         //  2: expr_1 -> ε        - ◄2
            ]),
            // ----------------------------------------------------------------- left_rec
            // a -> a "b" | a "c" | "a"
            (501, 0, vec![
                strip![nt 1, exit 0, t 2],              //  0: a -> a a_1   - ►a_1 ◄0 a
                strip![loop 1, exit 1, t 0],            //  1: a_1 -> b a_1 - ●a_1 ◄1 b
                strip![loop 1, exit 2, t 1],            //  2: a_1 -> c a_1 - ●a_1 ◄2 c
                strip![exit 3],                         //  3: a_1 -> ε     - ◄3
            ]),
            // ----------------------------------------------------------------- left_rec + right_rec
            // e -> e "!" | "-" e | Num
            (580, 0, vec![
                strip![exit 0, nt 0, t 1],              //  0: e -> - e     - ◄0 ►e -
                strip![nt 1, exit 1, t 2],              //  1: e -> Num e_1 - ►e_1 ◄1 Num!
                strip![loop 1, exit 2, t 0],            //  2: e_1 -> ! e_1 - ●e_1 ◄2 !
                strip![exit 3],                         //  3: e_1 -> ε     - ◄3
            ]),
            // ----------------------------------------------------------------- left_rec + ambig
            // e -> e "+" e | Num
            (600, 0, vec![
                strip![nt 1, exit 0, nt 2],             //  0: e -> e_2 e_1     - ►e_1 ◄0 ►e_2
                strip![loop 1, exit 1, nt 2, t 0],      //  1: e_1 -> + e_2 e_1 - ●e_1 ◄1 ►e_2 +
                strip![exit 2],                         //  2: e_1 -> ε         - ◄2
                strip![exit 3, t 1],                    //  3: e_2 -> Num       - ◄3 Num!
            ]),
            // e -> e "*" e | e "+" e | "!" e | Num
            (603, 0, vec![
                strip![nt 1, exit 0, nt 4],             //  0: e -> e_4 e_1     - ►e_1 ◄0 ►e_4
                strip![loop 1, exit 1, nt 4, t 0],      //  1: e_1 -> * e_4 e_1 - ●e_1 ◄1 ►e_4 *
                strip![loop 1, exit 2, nt 2, t 1],      //  2: e_1 -> + e_2 e_1 - ●e_1 ◄2 ►e_2 +
                strip![exit 3],                         //  3: e_1 -> ε         - ◄3
                strip![nt 3, exit 4, nt 4],             //  4: e_2 -> e_4 e_3   - ►e_3 ◄4 ►e_4
                strip![loop 3, exit 5, nt 4, t 0],      //  5: e_3 -> * e_4 e_3 - ●e_3 ◄5 ►e_4 *
                strip![exit 6],                         //  6: e_3 -> ε         - ◄6
                strip![exit 7, nt 0, t 2],              //  7: e_4 -> ! e       - ◄7 ►e !
                strip![exit 8, t 3],                    //  8: e_4 -> Num       - ◄8 Num!
            ]),
            // e -> e "*" e | e "+" e | e "!" | Num
            (609, 0, vec![
                strip![nt 1, exit 0, nt 4],             //  0: e -> e_4 e_1     - ►e_1 ◄0 ►e_4
                strip![loop 1, exit 1, nt 4, t 0],      //  1: e_1 -> * e_4 e_1 - ●e_1 ◄1 ►e_4 *
                strip![loop 1, exit 2, nt 2, t 1],      //  2: e_1 -> + e_2 e_1 - ●e_1 ◄2 ►e_2 +
                strip![loop 1, exit 3, t 2],            //  3: e_1 -> ! e_1     - ●e_1 ◄3 !
                strip![exit 4],                         //  4: e_1 -> ε         - ◄4
                strip![nt 3, exit 5, nt 4],             //  5: e_2 -> e_4 e_3   - ►e_3 ◄5 ►e_4
                strip![loop 3, exit 6, nt 4, t 0],      //  6: e_3 -> * e_4 e_3 - ●e_3 ◄6 ►e_4 *
                strip![exit 7],                         //  7: e_3 -> ε         - ◄7
                strip![exit 8, t 3],                    //  8: e_4 -> Num       - ◄8 Num!
            ]),
            // ----------------------------------------------------------------- left_fact
            // a -> A | A B
            (700, 0, vec![
                strip![nt 1, t 0],                      //  0: a -> A a_1 - ►a_1 A!
                strip![exit 1, t 1],                    //  1: a_1 -> B   - ◄1 B!
                strip![exit 2],                         //  2: a_1 -> ε   - ◄2
            ]),
            // a -> A B C | B B C | B C | B B A
            (704, 0, vec![
                strip![exit 0, t 2, t 1, t 0],          //  0: a -> A B C   - ◄0 C! B! A!
                strip![nt 1, t 1],                      //  1: a -> B a_1   - ►a_1 B!
                strip![nt 2, t 1],                      //  2: a_1 -> B a_2 - ►a_2 B!
                strip![exit 3, t 2],                    //  3: a_1 -> C     - ◄3 C!
                strip![exit 4, t 0],                    //  4: a_2 -> A     - ◄4 A!
                strip![exit 5, t 2],                    //  5: a_2 -> C     - ◄5 C!
            ]),
            // ----------------------------------------------------------------- mix
            // a -> A* B a | C
            (810, 0, vec![
                strip![exit 0, nt 0, t 1, nt 1],        //  0: a -> a_1 B a - ◄0 ►a B! ►a_1
                strip![exit 1, t 2],                    //  1: a -> C       - ◄1 C!
                strip![loop 1, exit 2, t 0],            //  2: a_1 -> A a_1 - ●a_1 ◄2 A!
                strip![exit 3],                         //  3: a_1 -> ε     - ◄3
            ]),
            // a -> A+ B a | C
            (811, 0, vec![
                strip![exit 0, nt 0, t 1, nt 1],        //  0: a -> a_1 B a - ◄0 ►a B! ►a_1
                strip![exit 1, t 2],                    //  1: a -> C       - ◄1 C!
                strip![nt 2, t 0],                      //  2: a_1 -> A a_2 - ►a_2 A!
                strip![loop 1, exit 3],                 //  3: a_2 -> a_1   - ●a_1 ◄3
                strip![exit 4],                         //  4: a_2 -> ε     - ◄4
            ]),
            // a -> a A* C | B
            (820, 0, vec![
                strip![nt 2, exit 0, t 2],              //  0: a -> B a_2       - ►a_2 ◄0 B!
                strip![loop 1, exit 1, t 0],            //  1: a_1 -> A a_1     - ●a_1 ◄1 A!
                strip![exit 2],                         //  2: a_1 -> ε         - ◄2
                strip![loop 2, exit 3, t 1, nt 1],      //  3: a_2 -> a_1 C a_2 - ●a_2 ◄3 C! ►a_1
                strip![exit 4],                         //  4: a_2 -> ε         - ◄4
            ]),
            // a -> a A+ C | B
            (821, 0, vec![
                strip![nt 2, exit 0, t 2],              //  0: a -> B a_2       - ►a_2 ◄0 B!
                strip![nt 3, t 0],                      //  1: a_1 -> A a_3     - ►a_3 A!
                strip![loop 2, exit 2, t 1, nt 1],      //  2: a_2 -> a_1 C a_2 - ●a_2 ◄2 C! ►a_1
                strip![exit 3],                         //  3: a_2 -> ε         - ◄3
                strip![loop 1, exit 4],                 //  4: a_3 -> a_1       - ●a_1 ◄4
                strip![exit 5],                         //  5: a_3 -> ε         - ◄5
            ]),
            // a -> (A B | A C)*
            (840, 0, vec![
                strip![exit 0, nt 1],                   //  0: a -> a_1     - ◄0 ►a_1
                strip![nt 2, t 0],                      //  1: a_1 -> A a_2 - ►a_2 A!
                strip![exit 2],                         //  2: a_1 -> ε     - ◄2
                strip![loop 1, exit 3, t 1],            //  3: a_2 -> B a_1 - ●a_1 ◄3 B!
                strip![loop 1, exit 4, t 2],            //  4: a_2 -> C a_1 - ●a_1 ◄4 C!
            ]),
            // a -> A B a | A C a | D
            (860, 0, vec![
                strip![nt 1, t 0],                      //  0: a -> A a_1 - ►a_1 A!
                strip![exit 1, t 3],                    //  1: a -> D     - ◄1 D!
                strip![exit 2, nt 0, t 1],              //  2: a_1 -> B a - ◄2 ►a B!
                strip![exit 3, nt 0, t 2],              //  3: a_1 -> C a - ◄3 ►a C!
            ]),
            // a -> A B a <L> | A C a <L> | D
            (861, 0, vec![
                strip![nt 1, t 0],                      //  0: a -> A a_1 - ►a_1 A!
                strip![exit 1, t 3],                    //  1: a -> D     - ◄1 D!
                strip![loop 0, exit 2, t 1],            //  2: a_1 -> B a - ●a ◄2 B!
                strip![loop 0, exit 3, t 2],            //  3: a_1 -> C a - ●a ◄3 C!
            ]),
            // a -> a A | B C | B D
            (870, 0, vec![
                strip![nt 2, t 1],                      //  0: a -> B a_2   - ►a_2 B!
                strip![loop 1, exit 1, t 0],            //  1: a_1 -> A a_1 - ●a_1 ◄1 A!
                strip![exit 2],                         //  2: a_1 -> ε     - ◄2
                strip![nt 1, exit 3, t 2],              //  3: a_2 -> C a_1 - ►a_1 ◄3 C!
                strip![nt 1, exit 4, t 3],              //  4: a_2 -> D a_1 - ►a_1 ◄4 D!
            ]),
            // a -> a A B | a A C | D
            (871, 0, vec![
                strip![nt 1, exit 0, t 3],              //  0: a -> D a_1   - ►a_1 ◄0 D!
                strip![nt 2, t 0],                      //  1: a_1 -> A a_2 - ►a_2 A!
                strip![exit 2],                         //  2: a_1 -> ε     - ◄2
                strip![loop 1, exit 3, t 1],            //  3: a_2 -> B a_1 - ●a_1 ◄3 B!
                strip![loop 1, exit 4, t 2],            //  4: a_2 -> C a_1 - ●a_1 ◄4 C!
            ]),
            /* template:
            (, 0, vec![
            ]),
            */
        ];
        const VERBOSE: bool = false;
        const TESTS_ALL: bool = false;
        let mut num_errors = 0;
        for (test_id, (tr_id, start_nt, expected_opcodes)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {tr_id}, start {start_nt}:", ""); }
            let ll1 = TestRules(tr_id).to_prs_ll1().unwrap();
            if VERBOSE {
                ll1.print_prs_summary();
            }
            let original_str = get_original_str(&ll1, 12);
            let parser_tables = ParserTables::build_from(ParserGen::build_from_rules(ll1, "Test".to_string()));
            let parser = parser_tables.make_parser();
            if VERBOSE {
                println!("Final alts and opcodes:\n{original_str}");
                println!("            ({tr_id}, {start_nt}, vec![");
                print_opcodes(&parser);
                println!("            ]),");
            }
            let err_msg = format!("test {test_id} {tr_id}/{start_nt} failed");
            if TESTS_ALL {
                if parser.get_opcodes() != &expected_opcodes {
                    num_errors += 1;
                    println!("## ERROR: {err_msg}");
                }
            } else {
                assert_eq!(parser.get_opcodes(), &expected_opcodes, "{err_msg}");
            }
        }
        if TESTS_ALL {
            assert_eq!(num_errors, 0, "{num_errors} tests have failed");
        }
    }
}

mod parser_source {
    use crate::grammar::tests::TestRules;
    use crate::log::{BuildFrom, LogReader, LogStatus};
    use crate::parsergen::{ParserGen, ParserTables};

    #[test]
    fn alternatives() {
        for include_alts in [false, true] {
            // let rules = build_prs(9, true);
            let ll1 = TestRules(900).to_prs_ll1().unwrap();
            assert_eq!(ll1.get_log().num_errors(), 0, "building the LL(1) failed:\n{}", ll1.get_log());
            let mut builder = ParserGen::build_from_rules(ll1, "simple".to_string());
            builder.set_include_alts(include_alts);
            let src = builder.gen_source_code(0, false);
            let alt_present = src.contains("static ALTERNATIVES");
            assert_eq!(alt_present, include_alts, "unexpected source code: include_alts = {include_alts}, code = \n{src}");
            let pt = ParserTables::build_from(builder);
            let parser = pt.make_parser();
            let alts = parser.get_alts();
            assert_eq!(alts.is_empty(), !include_alts, "unexpected: include_alts = {include_alts}, alts = {alts:?}");
        }
    }
}

mod wrapper_source {
    use std::collections::{BTreeMap, HashMap};
    use iter_index::IndexerIterator;
    use crate::grammar::{alt_to_rule_str, ruleflag, AltId, Symbol, VarId};
    use crate::grammar::tests::TestRules;
    use crate::{btreemap, columns_to_str, indent_source, symbols, CollectJoin};
    use crate::parsergen::{print_flags, print_items, ParserGen};
    use crate::log::{LogReader, LogStatus};
    use crate::parsergen::tests::get_original_str;
    use crate::parsergen::tests::wrapper_source::HasValue::{All, Default, Set};
    use crate::test_tools::{get_tagged_source, replace_tagged_source};

    #[allow(dead_code)] // All not used for now
    #[derive(Clone)]
    enum HasValue { Set(Vec<Symbol>), All, Default }

    fn set_has_value(builder: &mut ParserGen, has_value: HasValue) {
        match has_value {
            Set(symbols) => {
                for s in symbols {
                    match s {
                        Symbol::NT(v) => { builder.nt_value[v as usize] = true; }
                        _ => {}
                    }
                }
            },
            All | Default => {
                for v in 0..builder.parsing_table.num_nt {
                    if builder.parsing_table.parent[v].is_none() || builder.nt_has_all_flags(v as VarId, ruleflag::CHILD_REPEAT | ruleflag::L_FORM) {
                        builder.nt_value[v] = true
                    }
                }
            }
        }
    }

    #[test]
    #[allow(unused_doc_comments)]
    /// Tests [ParserGen::source_build_parser], [ParserGen::source_wrapper], [ParserGen::source_use], and [ParserGen::make_item_ops].
    fn build_items() {
        let tests: Vec<(
            u32,                            // TestRules #
            bool,                           // test sources?
            bool,                           // test sources include parser?
            u16,                            // start NT
            BTreeMap<VarId, String>,        // NT types
            BTreeMap<u16, Vec<Symbol>>,     // expected items
            HasValue,                       // which symbols have a value
            BTreeMap<VarId, Vec<AltId>>, // expected alt groups
        )> = vec![
            // -----------------------------------------------------------------------------
            // a -> A B
            (1, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, t 1],                //  0: a -> A B | ◄0 B! A! | A B
            ], Default, btreemap![0 => vec![0]]),
            // --------------------------------------------------------------------------- NT/T simple mix
            // s -> Id "=" val | "exit" | "return" val
            // val -> Id | Num
            (13, true, false, 0, btreemap![
                0 => "SynS".to_string(),
                1 => "SynVal".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1],               //  0: s -> Id "=" val   | ◄0 ►val "=" Id!  | Id val
                1 => symbols![],                        //  1: s -> "exit"       | ◄1 "exit"        |
                2 => symbols![nt 1],                    //  2: s -> "return" val | ◄2 ►val "return" | val
                3 => symbols![t 0],                     //  3: val -> Id         | ◄3 Id!           | Id
                4 => symbols![t 4],                     //  4: val -> Num        | ◄4 Num!          | Num
            ], Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),

            // --------------------------------------------------------------------------- +_or_*
            // a -> A B* C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (102, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | A a_1 C
                1 => symbols![nt 1, t 1],               //  1: a_1 -> B a_1 | ●a_1 ◄1 B!    | a_1 B
                2 => symbols![nt 1],                    //  2: a_1 -> ε     | ◄2            | a_1
            ], Default, btreemap![0 => vec![0]]),

            // a -> A B+ C
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (103, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | A a_1 C
                1 => symbols![],                        //  1: a_1 -> B a_2 | ►a_2 B!       |
                2 => symbols![nt 1, t 1],               //  2: a_2 -> a_1   | ●a_1 ◄2       | a_1 B
                3 => symbols![nt 1, t 1],               //  3: a_2 -> ε     | ◄3            | a_1 B
            ], Default, btreemap![0 => vec![0]]),

            // a -> (b A b B A)*
            // b -> C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (104, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 2],                            //  0: a -> a_1             | ◄0 ►a_1                | a_1
                1 => symbols![t 2],                             //  1: b -> C               | ◄1 C!                  | C
                2 => symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0], //  2: a_1 -> b A b B A a_1 | ●a_1 ◄2 A! B! ►b A! ►b | a_1 b A b B A
                3 => symbols![nt 2],                            //  3: a_1 -> ε             | ◄3                     | a_1
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),

            // a -> (b A b B A)+
            // b -> C
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (105, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 2],                            //  0: a -> a_1             | ◄0 ►a_1             | a_1
                1 => symbols![t 2],                             //  1: b -> C               | ◄1 C!               | C
                2 => symbols![],                                //  2: a_1 -> b A b B A a_2 | ►a_2 A! B! ►b A! ►b |
                3 => symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0], //  3: a_2 -> a_1           | ●a_1 ◄3             | a_1 b A b B A
                4 => symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0], //  4: a_2 -> ε             | ◄4                  | a_1 b A b B A
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),

            // a -> (A (b ",")* ";")* C
            // b -> B
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            //  - a_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - a_1 -> a_2
            //  - a_2 -> a
            (106, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 3, t 3],               //  0: a -> a_2 C           | ◄0 C! ►a_2          | a_2 C
                1 => symbols![t 4],                     //  1: b -> B               | ◄1 B!               | B
                2 => symbols![nt 2, nt 1],              //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | a_1 b
                3 => symbols![nt 2],                    //  3: a_1 -> ε             | ◄3                  | a_1
                4 => symbols![nt 3, t 0, nt 2],         //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | a_2 A a_1
                5 => symbols![nt 3],                    //  5: a_2 -> ε             | ◄5                  | a_2
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),
            // (106, true, false, 0, btreemap![
            // ], btreemap![
            //     0 => symbols![nt 3, t 3],               //  0: a -> a_2 C           | ◄0 C! ►a_2          | a_2 C
            //     1 => symbols![t 4],                     //  1: b -> B               | ◄1 B!               | B
            //     2 => symbols![],                        //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      |
            //     3 => symbols![],                        //  3: a_1 -> ε             | ◄3                  |
            //     4 => symbols![nt 3, t 0],               //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | a_2 A
            //     5 => symbols![nt 3],                    //  5: a_2 -> ε             | ◄5                  | a_2
            // ], Set(symbols![nt 0]), btreemap![0 => vec![0], 1 => vec![1]]),

            // a -> A "B"* C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (108, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: a -> A a_1 C   | ◄0 C! ►a_1 A! | A C
                1 => symbols![],                        //  1: a_1 -> "B" a_1 | ●a_1 ◄1 "B"   |
                2 => symbols![],                        //  2: a_1 -> ε       | ◄2            |
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- norm+/* alternatives
            // a -> (A | B)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (150, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> a_1     | ◄0 ►a_1    | a_1
                1 => symbols![nt 1, t 0],               //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | a_1 A
                2 => symbols![nt 1, t 1],               //  2: a_1 -> B a_1 | ●a_1 ◄2 B! | a_1 B
                3 => symbols![nt 1],                    //  3: a_1 -> ε     | ◄3         | a_1
            ], Default, btreemap![0 => vec![0]]),

            // a -> (A | B)+
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_fact (64)
            //  - a_3: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            //  - a_3 -> a_1
            (151, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> a_1     | ◄0 ►a_1 | a_1
                1 => symbols![],                        //  1: a_1 -> A a_2 | ►a_2 A! |
                2 => symbols![],                        //  2: a_1 -> B a_3 | ►a_3 B! |
                3 => symbols![nt 1, t 0],               //  3: a_2 -> a_1   | ●a_1 ◄3 | a_1 A
                4 => symbols![nt 1, t 0],               //  4: a_2 -> ε     | ◄4      | a_1 A
                5 => symbols![nt 1, t 1],               //  5: a_3 -> a_1   | ●a_1 ◄5 | a_1 B
                6 => symbols![nt 1, t 1],               //  6: a_3 -> ε     | ◄6      | a_1 B
            ], Default, btreemap![0 => vec![0]]),

            // a -> A (B | b C b B C | E)* F
            // b -> D
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (152, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, nt 2, t 4],                  //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!          | A a_1 F
                1 => symbols![t 5],                             //  1: b -> D               | ◄1 D!                  | D
                2 => symbols![nt 2, t 1],                       //  2: a_1 -> B a_1         | ●a_1 ◄2 B!             | a_1 B
                3 => symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2], //  3: a_1 -> b C b B C a_1 | ●a_1 ◄3 C! B! ►b C! ►b | a_1 b C b B C
                4 => symbols![nt 2, t 3],                       //  4: a_1 -> E a_1         | ●a_1 ◄4 E!             | a_1 E
                5 => symbols![nt 2],                            //  5: a_1 -> ε             | ◄5                     | a_1
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),

            // a -> A (B | b C b B C | E)+ F
            // b -> D
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_fact (64)
            //  - a_3: child_left_fact (64)
            //  - a_4: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            //  - a_3 -> a_1
            //  - a_4 -> a_1
            (153, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, nt 2, t 4],                   //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!       | A a_1 F
                1 => symbols![t 5],                              //  1: b -> D               | ◄1 D!               | D
                2 => symbols![],                                 //  2: a_1 -> B a_2         | ►a_2 B!             |
                3 => symbols![],                                 //  3: a_1 -> E a_3         | ►a_3 E!             |
                4 => symbols![],                                 //  4: a_1 -> b C b B C a_4 | ►a_4 C! B! ►b C! ►b |
                5 => symbols![nt 2, t 1],                        //  5: a_2 -> a_1           | ●a_1 ◄5             | a_1 B
                6 => symbols![nt 2, t 1],                        //  6: a_2 -> ε             | ◄6                  | a_1 B
                7 => symbols![nt 2, t 3],                        //  7: a_3 -> a_1           | ●a_1 ◄7             | a_1 E
                8 => symbols![nt 2, t 3],                        //  8: a_3 -> ε             | ◄8                  | a_1 E
                9 => symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2],  //  9: a_4 -> a_1           | ●a_1 ◄9             | a_1 b C b B C
                10 => symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2], // 10: a_4 -> ε             | ◄10                 | a_1 b C b B C
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),

            // a -> (A | A B | C)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* | parent_left_fact (33)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (154, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> a_1     | ◄0 ►a_1    | a_1
                1 => symbols![],                        //  1: a_1 -> A a_2 | ►a_2 A!    |
                2 => symbols![nt 1, t 2],               //  2: a_1 -> C a_1 | ●a_1 ◄2 C! | a_1 C
                3 => symbols![nt 1],                    //  3: a_1 -> ε     | ◄3         | a_1
                4 => symbols![nt 1, t 0, t 1],          //  4: a_2 -> B a_1 | ●a_1 ◄4 B! | a_1 A B
                5 => symbols![nt 1, t 0],               //  5: a_2 -> a_1   | ●a_1 ◄5    | a_1 A
            ], Default, btreemap![0 => vec![0]]),

            // a -> (A | A B | C)+
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: parent_left_fact | child_left_fact (96)
            //  - a_3: child_left_fact (64)
            //  - a_4: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            //  - a_3 -> a_1
            //  - a_4 -> a_2
            (155, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> a_1     | ◄0 ►a_1 | a_1
                1 => symbols![],                        //  1: a_1 -> A a_2 | ►a_2 A! |
                2 => symbols![],                        //  2: a_1 -> C a_3 | ►a_3 C! |
                3 => symbols![],                        //  3: a_2 -> B a_4 | ►a_4 B! |
                4 => symbols![nt 1, t 0],               //  4: a_2 -> a_1   | ●a_1 ◄4 | a_1 A
                5 => symbols![nt 1, t 0],               //  5: a_2 -> ε     | ◄5      | a_1 A
                6 => symbols![nt 1, t 2],               //  6: a_3 -> a_1   | ●a_1 ◄6 | a_1 C
                7 => symbols![nt 1, t 2],               //  7: a_3 -> ε     | ◄7      | a_1 C
                8 => symbols![nt 1, t 0, t 1],          //  8: a_4 -> a_1   | ◄8 ►a_1 | a_1 A B
                9 => symbols![nt 1, t 0, t 1],          //  9: a_4 -> ε     | ◄9      | a_1 A B
            ], Default, btreemap![0 => vec![0]]),

            // a -> A ((B C | D)* E | F)* G
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            //  - a_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - a_1 -> a_2
            //  - a_2 -> a
            (156, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, nt 2, t 6],          //  0: a -> A a_2 G     | ◄0 G! ►a_2 A!   | A a_2 G
                1 => symbols![nt 1, t 1, t 2],          //  1: a_1 -> B C a_1   | ●a_1 ◄1 C! B!   | a_1 B C
                2 => symbols![nt 1, t 3],               //  2: a_1 -> D a_1     | ●a_1 ◄2 D!      | a_1 D
                3 => symbols![nt 1],                    //  3: a_1 -> ε         | ◄3              | a_1
                4 => symbols![nt 2, nt 1, t 4],         //  4: a_2 -> a_1 E a_2 | ●a_2 ◄4 E! ►a_1 | a_2 a_1 E
                5 => symbols![nt 2, t 5],               //  5: a_2 -> F a_2     | ●a_2 ◄5 F!      | a_2 F
                6 => symbols![nt 2],                    //  6: a_2 -> ε         | ◄6              | a_2
            ], Default, btreemap![0 => vec![0]]),

            // a -> A ((B C | D)+ E | F)+ G
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - a_3: child_left_fact (64)
            //  - a_4: child_left_fact (64)
            //  - a_5: child_left_fact (64)
            //  - a_6: child_left_fact (64)
            // parents:
            //  - a_1 -> a_2
            //  - a_2 -> a
            //  - a_3 -> a_1
            //  - a_4 -> a_1
            //  - a_5 -> a_2
            //  - a_6 -> a_2
            (157, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, nt 2, t 6],          //  0: a -> A a_2 G     | ◄0 G! ►a_2 A! | A a_2 G
                1 => symbols![],                        //  1: a_1 -> B C a_3   | ►a_3 C! B!    |
                2 => symbols![],                        //  2: a_1 -> D a_4     | ►a_4 D!       |
                3 => symbols![],                        //  3: a_2 -> F a_5     | ►a_5 F!       |
                4 => symbols![],                        //  4: a_2 -> a_1 E a_6 | ►a_6 E! ►a_1  |
                5 => symbols![nt 1, t 1, t 2],          //  5: a_3 -> a_1       | ●a_1 ◄5       | a_1 B C
                6 => symbols![nt 1, t 1, t 2],          //  6: a_3 -> ε         | ◄6            | a_1 B C
                7 => symbols![nt 1, t 3],               //  7: a_4 -> a_1       | ●a_1 ◄7       | a_1 D
                8 => symbols![nt 1, t 3],               //  8: a_4 -> ε         | ◄8            | a_1 D
                9 => symbols![nt 2, t 5],               //  9: a_5 -> a_2       | ●a_2 ◄9       | a_2 F
                10 => symbols![nt 2, t 5],              // 10: a_5 -> ε         | ◄10           | a_2 F
                11 => symbols![nt 2, nt 1, t 4],        // 11: a_6 -> a_2       | ●a_2 ◄11      | a_2 a_1 E
                12 => symbols![nt 2, nt 1, t 4],        // 12: a_6 -> ε         | ◄12           | a_2 a_1 E
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- +_or_* <L>
            // a -> A (<L=i> B)* C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            (200, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynI".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: a -> A i C | ◄0 C! ►i A! | A i C
                1 => symbols![nt 1, t 1],               //  1: i -> B i   | ●i ◄1 B!    | i B
                2 => symbols![nt 1],                    //  2: i -> ε     | ◄2          | i
            ], Default, btreemap![0 => vec![0]]),
            (200, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: a -> A i C | ◄0 C! ►i A! | A C
                1 => symbols![t 1],                     //  1: i -> B i   | ●i ◄1 B!    | B
                2 => symbols![],                        //  2: i -> ε     | ◄2          |
            ], Set(symbols![nt 0]), btreemap![0 => vec![0]]),

            // a -> A (<L=i> B)+ C
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - a_1: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - a_1 -> i
            (201, true, false, 0, btreemap![
                0 => "SynMyA".to_string(),
                1 => "SynMyI".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: a -> A i C | ◄0 C! ►i A! | A i C
                1 => symbols![],                        //  1: i -> B a_1 | ►a_1 B!     |
                2 => symbols![nt 1, t 1],               //  2: a_1 -> i   | ●i ◄2       | i B
                3 => symbols![nt 1, t 1],               //  3: a_1 -> ε   | ◄3          | i B
            ], Default, btreemap![0 => vec![0]]),
            (201, true, false, 0, btreemap![
                0 => "SynMyA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: a -> A i C | ◄0 C! ►i A! | A C
                1 => symbols![],                        //  1: i -> B a_1 | ►a_1 B!     |
                2 => symbols![t 1],                     //  2: a_1 -> i   | ●i ◄2       | B
                3 => symbols![t 1],                     //  3: a_1 -> ε   | ◄3          | B
            ], Set(symbols![nt 0]), btreemap![0 => vec![0]]),
            (201, true, false, 0, btreemap![
                0 => "SynMyA".to_string(),
                1 => "SynMyI".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: a -> A i C | ◄0 C! ►i A! | A i C
                1 => symbols![],                        //  1: i -> B a_1 | ►a_1 B!     |
                2 => symbols![nt 1, t 1],               //  2: a_1 -> i   | ●i ◄2       | i B
                3 => symbols![nt 1, t 1],               //  3: a_1 -> ε   | ◄3          | i B
            ], Set(symbols![nt 1]), btreemap![0 => vec![0]]),

            // a -> (<L=i> b A b B A)*
            // b -> C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            (202, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                            //  0: a -> i           | ◄0 ►i                | i
                1 => symbols![nt 1, nt 2, t 0, nt 2, t 1, t 0], //  1: i -> b A b B A i | ●i ◄1 A! B! ►b A! ►b | i b A b B A
                2 => symbols![nt 1],                            //  2: i -> ε           | ◄2                   | i
                3 => symbols![t 2],                             //  3: b -> C           | ◄3 C!                | C
            ], Default, btreemap![0 => vec![0], 2 => vec![3]]),

            // a -> (A (<L=j> B ",")* ";")* C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - j: child_+_or_* | L-form (129)
            //  - a_1: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - j -> a_1
            //  - a_1 -> a
            (206, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynAiter".to_string(),
                2 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![nt 2, t 4],               //  0: a -> a_1 C         | ◄0 C! ►a_1        | a_1 C
                1 => symbols![nt 1, t 1],               //  1: j -> B "," j       | ●j ◄1 "," B!      | j B
                2 => symbols![nt 1],                    //  2: j -> ε             | ◄2                | j
                3 => symbols![nt 2, t 0, nt 1],         //  3: a_1 -> A j ";" a_1 | ●a_1 ◄3 ";" ►j A! | a_1 A j
                4 => symbols![nt 2],                    //  4: a_1 -> ε           | ◄4                | a_1
            ], Default, btreemap![0 => vec![0]]),

            // a -> (<L=i> A (<L=j> b ",")* ";")* C
            // b -> B
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form | parent_+_or_* (2177)
            //  - j: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            //  - j -> i
            //
            // 1) All nonterminals have a value:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1, t 3],               //  0: a -> i C       | ◄0 C! ►i        | i C
                1 => symbols![nt 1, t 0, nt 2],         //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | i A j
                2 => symbols![nt 1],                    //  2: i -> ε         | ◄2              | i
                3 => symbols![nt 2, nt 3],              //  3: j -> b "," j   | ●j ◄3 "," ►b    | j b
                4 => symbols![nt 2],                    //  4: j -> ε         | ◄4              | j
                5 => symbols![t 4],                     //  5: b -> B         | ◄5 B!           | B
            ], Default, btreemap![0 => vec![0], 3 => vec![5]]),
            //
            // 2) Here, 'i' needs to be in the list of valued nonterminals, or it'll generate the same
            // code as the 3rd example:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1, t 3],               //  0: a -> i C       | ◄0 C! ►i        | i C
                1 => symbols![nt 1, t 0],               //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | i A
                2 => symbols![nt 1],                    //  2: i -> ε         | ◄2              | i
                3 => symbols![],                        //  3: j -> b "," j   | ●j ◄3 "," ►b    |
                4 => symbols![],                        //  4: j -> ε         | ◄4              |
                5 => symbols![t 4],                     //  5: b -> B         | ◄5 B!           | B
            ], Set(symbols![nt 0, nt 1]), btreemap![0 => vec![0], 3 => vec![5]]),
            //
            // 3) Only 'a' has a value, the other exit (exit_i, exit_j, exit_b) don't return any value:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 3],                     //  0: a -> i C       | ◄0 C! ►i        | C
                1 => symbols![t 0],                     //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | A
                2 => symbols![],                        //  2: i -> ε         | ◄2              |
                3 => symbols![],                        //  3: j -> b "," j   | ●j ◄3 "," ►b    |
                4 => symbols![],                        //  4: j -> ε         | ◄4              |
                5 => symbols![t 4],                     //  5: b -> B         | ◄5 B!           | B
            ], Set(symbols![nt 0]), btreemap![0 => vec![0], 3 => vec![5]]),
            //
            // 4) Same items, but 'a' doesn't have any value, so there's no SynA nor any value for 'a'
            // on the stack:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 3],                     //  0: a -> i C       | ◄0 C! ►i        | C
                1 => symbols![t 0],                     //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | A
                2 => symbols![],                        //  2: i -> ε         | ◄2              |
                3 => symbols![],                        //  3: j -> b "," j   | ●j ◄3 "," ►b    |
                4 => symbols![],                        //  4: j -> ε         | ◄4              |
                5 => symbols![t 4],                     //  5: b -> B         | ◄5 B!           | B
            ], Set(symbols![]), btreemap![0 => vec![0], 3 => vec![5]]),

            // a -> A (<L=i> "B")* C
            (210, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: a -> A i C | ◄0 C! ►i A! | A C
                1 => symbols![],                        //  1: i -> "B" i | ●i ◄1 "B"   |
                2 => symbols![],                        //  2: i -> ε     | ◄2          |
            ], Set(symbols![nt 0]), btreemap![0 => vec![0]]),

            // a -> A A (B <L=i>)* C | A C (B <L=i>)* C
            // NT flags:
            //  - a: parent_left_fact | parent_+_or_* (2080)
            //  - i: child_+_or_* | L-form (129)
            //  - a_1: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - a_1 -> a
            (211, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynI".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: a -> A a_1   | ►a_1 A!     |
                1 => symbols![nt 1, t 2],               //  1: i -> B i     | ●i ◄1 B!    | i B
                2 => symbols![nt 1],                    //  2: i -> ε       | ◄2          | i
                3 => symbols![t 0, t 0, nt 1, t 1],     //  3: a_1 -> A i C | ◄3 C! ►i A! | A A i C
                4 => symbols![t 0, t 1, nt 1, t 1],     //  4: a_1 -> C i C | ◄4 C! ►i C! | A C i C
            ], Default, btreemap![0 => vec![3, 4]]),

            // --------------------------------------------------------------------------- norm+/* <L> alternatives
            // a -> (<L=i> A | B)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            (250, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> i   | ◄0 ►i    | i
                1 => symbols![nt 1, t 0],               //  1: i -> A i | ●i ◄1 A! | i A
                2 => symbols![nt 1, t 1],               //  2: i -> B i | ●i ◄2 B! | i B
                3 => symbols![nt 1],                    //  3: i -> ε   | ◄3       | i
            ], Default, btreemap![0 => vec![0]]),

            // a -> (<L=i> A | B)+
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - a_1: child_left_fact (64)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - a_1 -> i
            //  - a_2 -> i
            (251, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> i     | ◄0 ►i   | i
                1 => symbols![],                        //  1: i -> A a_1 | ►a_1 A! |
                2 => symbols![],                        //  2: i -> B a_2 | ►a_2 B! |
                3 => symbols![nt 1, t 0],               //  3: a_1 -> i   | ●i ◄3   | i A
                4 => symbols![nt 1, t 0],               //  4: a_1 -> ε   | ◄4      | i A
                5 => symbols![nt 1, t 1],               //  5: a_2 -> i   | ●i ◄5   | i B
                6 => symbols![nt 1, t 1],               //  6: a_2 -> ε   | ◄6      | i B
            ], Default, btreemap![0 => vec![0]]),

            // a -> A (<L=i> B A | B A C b | D)+ E
            // b -> F
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - a_1: parent_left_fact | child_left_fact (96)
            //  - a_2: child_left_fact (64)
            //  - a_3: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - a_1 -> i
            //  - a_2 -> i
            //  - a_3 -> a_1
            (256, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, nt 1, t 4],             //  0: a -> A i E     | ◄0 E! ►i A! | A i E
                1 => symbols![],                           //  1: i -> B A a_1   | ►a_1 A! B!  |
                2 => symbols![],                           //  2: i -> D a_2     | ►a_2 D!     |
                3 => symbols![t 5],                        //  3: b -> F         | ◄3 F!       | F
                4 => symbols![],                           //  4: a_1 -> C b a_3 | ►a_3 ►b C!  |
                5 => symbols![nt 1, t 1, t 0],             //  5: a_1 -> i       | ●i ◄5       | i B A
                6 => symbols![nt 1, t 1, t 0],             //  6: a_1 -> ε       | ◄6          | i B A
                7 => symbols![nt 1, t 3],                  //  7: a_2 -> i       | ●i ◄7       | i D
                8 => symbols![nt 1, t 3],                  //  8: a_2 -> ε       | ◄8          | i D
                9 => symbols![nt 1, t 1, t 0, t 2, nt 2],  //  9: a_3 -> i       | ●i ◄9       | i B A C b
                10 => symbols![nt 1, t 1, t 0, t 2, nt 2], // 10: a_3 -> ε       | ◄10         | i B A C b
            ], Default, btreemap![0 => vec![0], 2 => vec![3]]),

            // a -> (<L=i> A | A B A | C)+
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - a_1: parent_left_fact | child_left_fact (96)
            //  - a_2: child_left_fact (64)
            //  - a_3: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - a_1 -> i
            //  - a_2 -> i
            //  - a_3 -> a_1
            (257, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> i         | ◄0 ►i      | i
                1 => symbols![],                        //  1: i -> A a_1     | ►a_1 A!    |
                2 => symbols![],                        //  2: i -> C a_2     | ►a_2 C!    |
                3 => symbols![],                        //  3: a_1 -> B A a_3 | ►a_3 A! B! |
                4 => symbols![nt 1, t 0],               //  4: a_1 -> i       | ●i ◄4      | i A
                5 => symbols![nt 1, t 0],               //  5: a_1 -> ε       | ◄5         | i A
                6 => symbols![nt 1, t 2],               //  6: a_2 -> i       | ●i ◄6      | i C
                7 => symbols![nt 1, t 2],               //  7: a_2 -> ε       | ◄7         | i C
                8 => symbols![nt 1, t 0, t 1, t 0],     //  8: a_3 -> i       | ●i ◄8      | i A B A
                9 => symbols![nt 1, t 0, t 1, t 0],     //  9: a_3 -> ε       | ◄9         | i A B A
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- right_rec
            // expr -> Id "." expr | "(" Num ")"
            // NT flags:
            //  - expr: right_rec (2)
            (301, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0, nt 0],               //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | Id expr
                1 => symbols![t 3],                     //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | Num
            ], Default, btreemap![0 => vec![0, 1]]),
            (301, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0],                     //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | Id
                1 => symbols![t 3],                     //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | Num
            ], Set(symbols![]), btreemap![0 => vec![0, 1]]),

            // Code: without the <L>, the right-recursive loop starts with `Num` and proceeds right to left,
            //       handing the current `expr` and the previous `Id` to generate the updated `expr`:
            //
            // fn exit_expr(&mut self, alt_id: AltId) {
            //     let ctx = match alt_id {
            //         0 => {
            //             let expr = self.stack.pop().unwrap().get_expr();
            //             let id = self.stack_t.pop().unwrap();
            //             CtxExpr::Expr1 { id, expr }
            //         }
            //         1 => {
            //             let num = self.stack_t.pop().unwrap();
            //             CtxExpr::Expr2 { num }
            //         }
            //         _ => panic!("unexpected alt id {alt_id} in fn exit_expr")
            //     };
            //     let val = self.listener.exit_expr(ctx);
            //     self.stack.push(SynValue::Expr(val));
            // }

            // --------------------------------------------------------------------------- right_rec <L>
            // expr -> <L> Id "." expr | "(" Num ")"
            // NT flags:
            //  - expr: right_rec | L-form (130)
            (401, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0, t 0],               //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | expr Id
                1 => symbols![nt 0, t 3],               //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | expr Num
            ], Default, btreemap![0 => vec![0, 1]]),
            (401, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![t 0],                     //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | Id
                1 => symbols![t 3],                     //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | Num
            ], Set(symbols![]), btreemap![0 => vec![0, 1]]),

            // Code: with <L>, the right-recursive loop starts with an initialization of `expr`, then all
            //       the `Id` are scanned from left to right, handing the current `expr` and the next `Id`,
            //       and the loop ends with the accumulated value of `expr` and the final `Num`:
            //
            // fn init_expr(&mut self) {
            //     let val = self.listener.init_expr();
            //     self.stack.push(SynValue::Expr(val));
            // }
            //
            // fn exit_expr(&mut self, alt_id: AltId) {
            //     let ctx = match alt_id {
            //         0 => {
            //             let id = self.stack_t.pop().unwrap();
            //             let expr = self.stack.pop().unwrap().get_expr();
            //             CtxExpr::Expr1 { expr, id }
            //         }
            //         1 => {
            //             let num = self.stack_t.pop().unwrap();
            //             let expr = self.stack.pop().unwrap().get_expr();
            //             CtxExpr::Expr2 { expr, num }
            //         }
            //         _ => panic!("unexpected alt id {alt_id} in fn exit_expr")
            //     };
            //     let val = self.listener.exit_expr(ctx);
            //     self.stack.push(SynValue::Expr(val));
            // }

            // --------------------------------------------------------------------------- left_rec
            // e -> f | e "." Id
            // f -> Id
            // NT flags:
            //  - e: parent_left_rec (512)
            //  - e_1: child_left_rec (4)
            // parents:
            //  - e_1 -> e
            (502, true, false, 0, btreemap![
                0 => "SynE".to_string(),
                1 => "SynF".to_string(),
            ], btreemap![
                0 => symbols![nt 1],                    //  0: e -> f e_1        | ►e_1 ◄0 ►f      | f
                1 => symbols![t 1],                     //  1: f -> Id           | ◄1 Id!          | Id
                2 => symbols![nt 0, t 1],               //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | e Id
                3 => symbols![nt 0],                    //  3: e_1 -> ε          | ◄3              | e
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),
            (502, true, false, 0, btreemap![
                1 => "SynF".to_string(),
            ], btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F    | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!        | id
                2 => symbols![t 1],                     //  2: E_1 -> . id E_1 | ●E_1 ◄2 id! . | id
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3            |
            ], Set(symbols![nt 1]), btreemap![0 => vec![0], 1 => vec![1]]),


            // --------------------------------------------------------------------------- right_rec + left_rec
            // e -> e "!" | "-" e | Num
            // NT flags:
            //  - e: right_rec | parent_left_rec (514)
            //  - e_1: child_left_rec (4)
            // parents:
            //  - e_1 -> e
            (580, true, false, 0, btreemap![
                0 => "SynE".to_string(),
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> "-" e     | ◄0 ►e "-"    | e
                1 => symbols![t 2],                     //  1: e -> Num e_1   | ►e_1 ◄1 Num! | Num
                2 => symbols![nt 0],                    //  2: e_1 -> "!" e_1 | ●e_1 ◄2 "!"  | e
                3 => symbols![nt 0],                    //  3: e_1 -> ε       | ◄3           | e
            ], Default, btreemap![0 => vec![0, 1]]),

            // e -> e "!" | <L=e> "-" e | Num
            // NT flags:
            //  - e: right_rec | L-form | parent_left_rec (642)
            //  - e_1: child_left_rec (4)
            // parents:
            //  - e_1 -> e
            (581, true, false, 0, btreemap![
                0 => "SynE".to_string(),
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> "-" e     | ●e ◄0 "-"    | e
                1 => symbols![nt 0, t 2],               //  1: e -> Num e_1   | ►e_1 ◄1 Num! | e Num
                2 => symbols![nt 0],                    //  2: e_1 -> "!" e_1 | ●e_1 ◄2 "!"  | e
                3 => symbols![nt 0],                    //  3: e_1 -> ε       | ◄3           | e
            ], Default, btreemap![0 => vec![0, 1]]),

            // --------------------------------------------------------------------------- left_rec ambig

            // e -> e "+" e | Num
            // NT flags:
            //  - e: parent_left_rec | parent_amb (1536)
            //  - e_1: child_left_rec (4)
            // parents:
            //  - e_1 -> e
            //  - e_2 -> e
            (600, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "+" e_2 e_1 | ●e_1 ◄1 ►e_2 "+" | e e
                2 => symbols![nt 0],                    //  2: e_1 -> ε           | ◄2               | e
                3 => symbols![t 1],                     //  3: e_2 -> Num         | ◄3 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),

            // e -> e "*" e | e "+" e | "!" e | Num
            (603, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | e e
                3 => symbols![nt 0],                    //  3: e_1 -> ε           | ◄3               | e
                4 => symbols![nt 0],                    //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | e
                5 => symbols![nt 0, nt 0],              //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | e e
                6 => symbols![nt 0],                    //  6: e_3 -> ε           | ◄6               | e
                7 => symbols![nt 0],                    //  7: e_4 -> "!" e       | ◄7 ►e "!"        | e
                8 => symbols![t 3],                     //  8: e_4 -> Num         | ◄8 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | "!" e | e "+" e | Num
            (604, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | e e
                3 => symbols![nt 0],                    //  3: e_1 -> ε           | ◄3               | e
                4 => symbols![nt 0],                    //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | e
                5 => symbols![nt 0, nt 0],              //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | e e
                6 => symbols![nt 0],                    //  6: e_3 -> ε           | ◄6               | e
                7 => symbols![nt 0],                    //  7: e_4 -> "!" e_2     | ◄7 ►e_2 "!"      | e
                8 => symbols![t 3],                     //  8: e_4 -> Num         | ◄8 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> "!" e | e "*" e | e "+" e | Num
            (605, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | e e
                3 => symbols![nt 0],                    //  3: e_1 -> ε           | ◄3               | e
                4 => symbols![nt 0],                    //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | e
                5 => symbols![nt 0, nt 0],              //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | e e
                6 => symbols![nt 0],                    //  6: e_3 -> ε           | ◄6               | e
                7 => symbols![nt 0],                    //  7: e_4 -> "!" e_4     | ◄7 ►e_4 "!"      | e
                8 => symbols![t 3],                     //  8: e_4 -> Num         | ◄8 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" e | <R> e "!" e | Num
            (606, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "!" e e_1   | ●e_1 ◄3 ►e "!"   | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
                7 => symbols![nt 0],                    //  7: e_3 -> ε           | ◄7               | e
                8 => symbols![t 3],                     //  8: e_4 -> Num         | ◄8 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | <R> e "!" e | e "+" e | Num
            (607, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "!" e_2 e_1 | ●e_1 ◄2 ►e_2 "!" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "!" e_2 e_3 | ●e_3 ◄7 ►e_2 "!" | e e
                8 => symbols![nt 0],                    //  8: e_3 -> ε           | ◄8               | e
                9 => symbols![t 3],                     //  9: e_4 -> Num         | ◄9 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> <R> e "!" e | e "*" e | e "+" e | Num
            (608, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "!" e_4 e_1 | ●e_1 ◄1 ►e_4 "!"  | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4                | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "!" e_4 e_3 | ●e_3 ◄6 ►e_4 "!"  | e e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | e e
                8 => symbols![nt 0],                    //  8: e_3 -> ε           | ◄8                | e
                9 => symbols![nt 0],                    //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | e
                10 => symbols![nt 0, nt 0],             // 10: e_5 -> "!" e_4 e_5 | ●e_5 ◄10 ►e_4 "!" | e e
                11 => symbols![nt 0],                   // 11: e_5 -> ε           | ◄11               | e
                12 => symbols![t 3],                    // 12: e_6 -> Num         | ◄12 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" e | e "!" | Num
            (609, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | e e
                3 => symbols![nt 0],                    //  3: e_1 -> "!" e_1     | ●e_1 ◄3 "!"      | e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
                7 => symbols![nt 0],                    //  7: e_3 -> ε           | ◄7               | e
                8 => symbols![t 3],                     //  8: e_4 -> Num         | ◄8 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "!" | e "+" e | Num
            (610, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0],                    //  2: e_1 -> "!" e_1     | ●e_1 ◄2 "!"      | e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
                7 => symbols![nt 0],                    //  7: e_3 -> "!" e_3     | ●e_3 ◄7 "!"      | e
                8 => symbols![nt 0],                    //  8: e_3 -> ε           | ◄8               | e
                9 => symbols![t 3],                     //  9: e_4 -> Num         | ◄9 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "!" | e "*" e | e "+" e | Num
            (611, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6     | e
                1 => symbols![nt 0],                    //  1: e_1 -> "!" e_1     | ●e_1 ◄1 "!"      | e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6     | e
                6 => symbols![nt 0],                    //  6: e_3 -> "!" e_3     | ●e_3 ◄6 "!"      | e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | e e
                8 => symbols![nt 0],                    //  8: e_3 -> ε           | ◄8               | e
                9 => symbols![nt 0],                    //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6     | e
                10 => symbols![nt 0],                   // 10: e_5 -> "!" e_5     | ●e_5 ◄10 "!"     | e
                11 => symbols![nt 0],                   // 11: e_5 -> ε           | ◄11              | e
                12 => symbols![t 3],                    // 12: e_6 -> Num         | ◄12 Num!         | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "!" e | e "*" e | e "+" e | Num
            (612, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "!" e_6 e_1 | ●e_1 ◄1 ►e_6 "!"  | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4                | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "!" e_6 e_3 | ●e_3 ◄6 ►e_6 "!"  | e e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | e e
                8 => symbols![nt 0],                    //  8: e_3 -> ε           | ◄8                | e
                9 => symbols![nt 0],                    //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | e
                10 => symbols![nt 0, nt 0],             // 10: e_5 -> "!" e_6 e_5 | ●e_5 ◄10 ►e_6 "!" | e e
                11 => symbols![nt 0],                   // 11: e_5 -> ε           | ◄11               | e
                12 => symbols![t 3],                    // 12: e_6 -> Num         | ◄12 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" e | <P> e "!" e | Num
            (613, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "!" e_2 e_1 | ●e_1 ◄3 ►e_2 "!" | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
                7 => symbols![nt 0],                    //  7: e_3 -> ε           | ◄7               | e
                8 => symbols![t 3],                     //  8: e_4 -> Num         | ◄8 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | <P> e "!" e | e "+" e | Num
            (614, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "!" e_4 e_1 | ●e_1 ◄2 ►e_4 "!" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
                4 => symbols![nt 0],                    //  4: e_1 -> ε           | ◄4               | e
                5 => symbols![nt 0],                    //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | e
                6 => symbols![nt 0, nt 0],              //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | e e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "!" e_4 e_3 | ●e_3 ◄7 ►e_4 "!" | e e
                8 => symbols![nt 0],                    //  8: e_3 -> ε           | ◄8               | e
                9 => symbols![t 3],                     //  9: e_4 -> Num         | ◄9 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" | "!" e | Num
            (630, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | e e
                2 => symbols![nt 0],                    //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | e
                3 => symbols![nt 0],                    //  3: e_1 -> ε           | ◄3               | e
                4 => symbols![nt 0],                    //  4: e_2 -> "!" e       | ◄4 ►e "!"        | e
                5 => symbols![t 3],                     //  5: e_2 -> Num         | ◄5 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" | <R> "!" e | Num
            (631, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | e e
                2 => symbols![nt 0],                    //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | e
                3 => symbols![nt 0],                    //  3: e_1 -> ε           | ◄3               | e
                4 => symbols![nt 0],                    //  4: e_2 -> "!" e       | ◄4 ►e "!"        | e
                5 => symbols![t 3],                     //  5: e_2 -> Num         | ◄5 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | <R> e "+" | "!" e | Num
            (632, true, true, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | e e
                2 => symbols![nt 0],                    //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | e
                3 => symbols![nt 0],                    //  3: e_1 -> ε           | ◄3               | e
                4 => symbols![nt 0],                    //  4: e_2 -> "!" e       | ◄4 ►e "!"        | e
                5 => symbols![t 3],                     //  5: e_2 -> Num         | ◄5 Num!          | Num
            ], Default, btreemap![0 => vec![0]]),

            // e -> "-" e | e "*" e | e "/" <P> e | e "+" e | e "-" <P> e | Id
            // NT flags:
            //  - e: parent_left_rec | parent_amb (1536)
            //  - e_1: child_left_rec (4)
            //  - e_2: parent_left_rec (512)
            //  - e_3: child_left_rec (4)
            //  - e_4: right_rec (2)
            // parents:
            //  - e_1 -> e
            //  - e_2 -> e
            //  - e_3 -> e_2
            //  - e_4 -> e
            (640, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "/" e_4 e_1 | ●e_1 ◄2 ►e_4 "/" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
                4 => symbols![nt 0, nt 0],              //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | e e
                5 => symbols![nt 0],                    //  5: e_1 -> ε           | ◄5               | e
                6 => symbols![nt 0],                    //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | e e
                8 => symbols![nt 0, nt 0],              //  8: e_3 -> "/" e_4 e_3 | ●e_3 ◄8 ►e_4 "/" | e e
                9 => symbols![nt 0],                    //  9: e_3 -> ε           | ◄9               | e
                10 => symbols![nt 0],                   // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | e
                11 => symbols![t 4],                    // 11: e_4 -> Id          | ◄11 Id!          | Id
            ], Default, btreemap![0 => vec![0]]),

            // e -> "-" e | <R> e "*" e | <R> e "/" <P> e | <R> e "+" e | <R> e "-" <P> e | Id
            // NT flags:
            //  - e: parent_left_rec | parent_amb (1536)
            //  - e_1: child_left_rec (4)
            //  - e_2: parent_left_rec (512)
            //  - e_3: child_left_rec (4)
            //  - e_4: right_rec (2)
            // parents:
            //  - e_1 -> e
            //  - e_2 -> e
            //  - e_3 -> e_2
            //  - e_4 -> e
            (641, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e e_1   | ●e_1 ◄3 ►e "+"   | e e
                4 => symbols![nt 0, nt 0],              //  4: e_1 -> "-" e e_1   | ●e_1 ◄4 ►e "-"   | e e
                5 => symbols![nt 0],                    //  5: e_1 -> ε           | ◄5               | e
                6 => symbols![nt 0],                    //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | e e
                8 => symbols![nt 0, nt 0],              //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | e e
                9 => symbols![nt 0],                    //  9: e_3 -> ε           | ◄9               | e
                10 => symbols![nt 0],                   // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | e
                11 => symbols![t 4],                    // 11: e_4 -> Id          | ◄11 Id!          | Id
            ], Default, btreemap![0 => vec![0]]),

            // e -> "-" e | <R> e "*" e | <R> e "/" <P> e | e "+" e | e "-" <P> e | Id
            // NT flags:
            //  - e: parent_left_rec | parent_amb (1536)
            //  - e_1: child_left_rec (4)
            //  - e_2: parent_left_rec (512)
            //  - e_3: child_left_rec (4)
            //  - e_4: right_rec (2)
            // parents:
            //  - e_1 -> e
            //  - e_2 -> e
            //  - e_3 -> e_2
            //  - e_4 -> e
            (642, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | e
                1 => symbols![nt 0, nt 0],              //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | e e
                2 => symbols![nt 0, nt 0],              //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | e e
                3 => symbols![nt 0, nt 0],              //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | e e
                4 => symbols![nt 0, nt 0],              //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | e e
                5 => symbols![nt 0],                    //  5: e_1 -> ε           | ◄5               | e
                6 => symbols![nt 0],                    //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | e
                7 => symbols![nt 0, nt 0],              //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | e e
                8 => symbols![nt 0, nt 0],              //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | e e
                9 => symbols![nt 0],                    //  9: e_3 -> ε           | ◄9               | e
                10 => symbols![nt 0],                   // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | e
                11 => symbols![t 4],                    // 11: e_4 -> Id          | ◄11 Id!          | Id
            ], Default, btreemap![0 => vec![0]]),

            // a -> a A a a | B
            // NT flags:
            //  - a: parent_left_rec | parent_amb (1536)
            //  - a_1: child_left_rec (4)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            (650, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: a -> a_2 a_1       | ►a_1 ◄0 ►a_2       | a
                1 => symbols![nt 0, t 0, nt 0, nt 0],   //  1: a_1 -> A a a_2 a_1 | ●a_1 ◄1 ►a_2 ►a A! | a A a a
                2 => symbols![nt 0],                    //  2: a_1 -> ε           | ◄2                 | a
                3 => symbols![t 1],                     //  3: a_2 -> B           | ◄3 B!              | B
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- left_fact
            // a -> A | A B | A B C | A B D | E
            // NT flags:
            //  - a: parent_left_fact (32)
            //  - a_1: parent_left_fact | child_left_fact (96)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (705, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: a -> A a_1   | ►a_1 A! |
                1 => symbols![t 4],                     //  1: a -> E       | ◄1 E!   | E
                2 => symbols![],                        //  2: a_1 -> B a_2 | ►a_2 B! |
                3 => symbols![t 0],                     //  3: a_1 -> ε     | ◄3      | A
                4 => symbols![t 0, t 1, t 2],           //  4: a_2 -> C     | ◄4 C!   | A B C
                5 => symbols![t 0, t 1, t 3],           //  5: a_2 -> D     | ◄5 D!   | A B D
                6 => symbols![t 0, t 1],                //  6: a_2 -> ε     | ◄6      | A B
            ], Default, btreemap![0 => vec![1, 3, 4, 5, 6]]),

            // --------------------------------------------------------------------------- combinations

            // --------------------------------------------------------------------------- +_or_* and right_rec
            // a -> A* B a | C
            // NT flags:
            //  - a: right_rec | parent_+_or_* (2050)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (810, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1, t 1, nt 0],         //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | a_1 B a
                1 => symbols![t 2],                     //  1: a -> C       | ◄1 C!         | C
                2 => symbols![nt 1, t 0],               //  2: a_1 -> A a_1 | ●a_1 ◄2 A!    | a_1 A
                3 => symbols![nt 1],                    //  3: a_1 -> ε     | ◄3            | a_1
            ], Default, btreemap![0 => vec![0, 1]]),

            // a -> A+ B a | C
            // NT flags:
            //  - a: right_rec | parent_+_or_* | plus (6146)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (811, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1, t 1, nt 0],         //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | a_1 B a
                1 => symbols![t 2],                     //  1: a -> C       | ◄1 C!         | C
                2 => symbols![],                        //  2: a_1 -> A a_2 | ►a_2 A!       |
                3 => symbols![nt 1, t 0],               //  3: a_2 -> a_1   | ●a_1 ◄3       | a_1 A
                4 => symbols![nt 1, t 0],               //  4: a_2 -> ε     | ◄4            | a_1 A
            ], Default, btreemap![0 => vec![0, 1]]),

            // --------------------------------------------------------------------------- +_or_* and left_rec
            // a -> a A* C | B
            // NT flags:
            //  - a: parent_left_rec | parent_+_or_* (2560)
            //  - a_1: child_+_or_* (1)
            //  - a_2: child_left_rec (4)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            (820, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 2],                     //  0: a -> B a_2       | ►a_2 ◄0 B!      | B
                1 => symbols![nt 1, t 0],               //  1: a_1 -> A a_1     | ●a_1 ◄1 A!      | a_1 A
                2 => symbols![nt 1],                    //  2: a_1 -> ε         | ◄2              | a_1
                3 => symbols![nt 0, nt 1, t 1],         //  3: a_2 -> a_1 C a_2 | ●a_2 ◄3 C! ►a_1 | a a_1 C
                4 => symbols![nt 0],                    //  4: a_2 -> ε         | ◄4              | a
            ], Default, btreemap![0 => vec![0]]),

            // a -> a A+ C | B
            // NT flags:
            //  - a: parent_left_rec | parent_+_or_* | plus (6656)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_rec (4)
            //  - a_3: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            //  - a_3 -> a_1
            (821, true, false, 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 2],                     //  0: a -> B a_2       | ►a_2 ◄0 B!      | B
                1 => symbols![],                        //  1: a_1 -> A a_3     | ►a_3 A!         |
                2 => symbols![nt 0, nt 1, t 1],         //  2: a_2 -> a_1 C a_2 | ●a_2 ◄2 C! ►a_1 | a a_1 C
                3 => symbols![nt 0],                    //  3: a_2 -> ε         | ◄3              | a
                4 => symbols![nt 1, t 0],               //  4: a_3 -> a_1       | ●a_1 ◄4         | a_1 A
                5 => symbols![nt 1, t 0],               //  5: a_3 -> ε         | ◄5              | a_1 A
            ], Default, btreemap![0 => vec![0]]),

            // a -> a "x" a | a "*" "[" Num+ "]" | "-" a | Id
            // NT flags:
            //  - a: parent_left_rec | parent_amb | parent_+_or_* | plus (7680)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - a_2: child_left_rec (4)
            //  - a_3: right_rec (2)
            //  - a_4: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            //  - a_3 -> a
            //  - a_4 -> a_1
            (835, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 0],                    //  0: a -> a_3 a_2               | ►a_2 ◄0 ►a_3             | a
                1 => symbols![],                        //  1: a_1 -> Num a_4             | ►a_4 Num!                |
                2 => symbols![nt 0, nt 0],              //  2: a_2 -> "x" a_3 a_2         | ●a_2 ◄2 ►a_3 "x"         | a a
                3 => symbols![nt 0, nt 1],              //  3: a_2 -> "*" "[" a_1 "]" a_2 | ●a_2 ◄3 "]" ►a_1 "[" "*" | a a_1
                4 => symbols![nt 0],                    //  4: a_2 -> ε                   | ◄4                       | a
                5 => symbols![nt 0],                    //  5: a_3 -> "-" a               | ◄5 ►a "-"                | a
                6 => symbols![t 6],                     //  6: a_3 -> Id                  | ◄6 Id!                   | Id
                7 => symbols![nt 1, t 3],               //  7: a_4 -> a_1                 | ●a_1 ◄7                  | a_1 Num
                8 => symbols![nt 1, t 3],               //  8: a_4 -> ε                   | ◄8                       | a_1 Num
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- +_or_* and left_fact
            // a -> (A B | A C)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* | parent_left_fact (33)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (840, false, false, 0, btreemap![
            ], btreemap![
                0 => symbols![nt 1],                    //  0: a -> a_1     | ◄0 ►a_1    | a_1
                1 => symbols![],                        //  1: a_1 -> A a_2 | ►a_2 A!    |
                2 => symbols![nt 1],                    //  2: a_1 -> ε     | ◄2         | a_1
                3 => symbols![nt 1, t 0, t 1],          //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | a_1 A B
                4 => symbols![nt 1, t 0, t 2],          //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | a_1 A C
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- right_rec + left_fact
            // expr -> <L> Num "^" expr | Num
            // NT flags:
            //  - expr: right_rec | parent_left_fact | L-form (162)
            //  - expr_1: child_left_fact (64)
            // parents:
            //  - expr_1 -> expr
            (862, true, false, 0, btreemap![
            ], btreemap![
                0 => symbols![],                        //  0: expr -> Num expr_1 | ►expr_1 Num! |
                1 => symbols![nt 0, t 0],               //  1: expr_1 -> "^" expr | ●expr ◄1 "^" | expr Num
                2 => symbols![nt 0, t 0],               //  2: expr_1 -> ε        | ◄2           | expr Num
            ], Default, btreemap![0 => vec![1, 2]]),

            // --------------------------------------------------------------------------- left_rec [left_fact]
            // a -> a A | B C | B D
            // NT flags:
            //  - a: parent_left_fact | parent_left_rec (544)
            //  - a_1: child_left_rec (4)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            (870, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: a -> B a_2   | ►a_2 B!    |
                1 => symbols![nt 0, t 0],               //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | a A
                2 => symbols![nt 0],                    //  2: a_1 -> ε     | ◄2         | a
                3 => symbols![t 1, t 2],                //  3: a_2 -> C a_1 | ►a_1 ◄3 C! | B C
                4 => symbols![t 1, t 3],                //  4: a_2 -> D a_1 | ►a_1 ◄4 D! | B D
            ], Default, btreemap![0 => vec![3, 4]]),

            // a -> a A B | a A C | D
            // NT flags:
            //  - a: parent_left_rec (512)
            //  - a_1: child_left_rec | parent_left_fact (36)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a_1
            (871, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 3],                     //  0: a -> D a_1   | ►a_1 ◄0 D! | D
                1 => symbols![],                        //  1: a_1 -> A a_2 | ►a_2 A!    |
                2 => symbols![nt 0],                    //  2: a_1 -> ε     | ◄2         | a
                3 => symbols![nt 0, t 0, t 1],          //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | a A B
                4 => symbols![nt 0, t 0, t 2],          //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | a A C
            ], Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- misc
            // NT flags:
            //  - file: parent_+_or_* (2048)
            //  - option: parent_+_or_* (2048)
            //  - rule: parent_left_fact (32)
            //  - actions: parent_+_or_* (2048)
            //  - alt_items: parent_+_or_* (2048)
            //  - alt_item: parent_+_or_* | plus (6144)
            //  - repeat_item: parent_left_fact (32)
            //  - item: right_rec | parent_left_fact (34)
            //  - char_set: parent_+_or_* | plus (6144)
            //  - char_set_one: parent_left_fact (32)
            //  - file_1: child_+_or_* (1)
            //  - option_1: child_+_or_* (1)
            //  - actions_1: child_+_or_* (1)
            //  - alt_items_1: child_+_or_* (1)
            //  - alt_item_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - char_set_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - rule_1: child_left_fact (64)
            //  - repeat_item_1: parent_left_fact | child_left_fact (96)
            //  - item_1: child_left_fact (64)
            //  - char_set_one_1: child_left_fact (64)
            //  - alt_item_2: child_left_fact (64)
            //  - char_set_2: child_left_fact (64)
            //  - repeat_item_2: child_left_fact (64)
            //  - repeat_item_3: child_left_fact (64)
            // parents:
            //  - file_1 -> file
            //  - option_1 -> option
            //  - actions_1 -> actions
            //  - alt_items_1 -> alt_items
            //  - alt_item_1 -> alt_item
            //  - char_set_1 -> char_set
            //  - rule_1 -> rule
            //  - repeat_item_1 -> repeat_item
            //  - item_1 -> item
            //  - char_set_one_1 -> char_set_one
            //  - alt_item_2 -> alt_item_1
            //  - char_set_2 -> char_set_1
            //  - repeat_item_2 -> repeat_item_1
            //  - repeat_item_3 -> repeat_item_1
            (901, true, false, 0, btreemap![
                0 => "SynFile".to_string(),
                1 => "SynFileItem".to_string(),
                2 => "SynHeader".to_string(),
                3 => "SynDeclaration".to_string(),
                4 => "SynOption".to_string(),
                5 => "SynRule".to_string(),
                6 => "SynActions".to_string(),
                7 => "SynAction".to_string(),
                8 => "SynMatch".to_string(),
                9 => "SynAltItems".to_string(),
                10 => "SynAltItem".to_string(),
                11 => "SynRepeatItem".to_string(),
                12 => "SynItem".to_string(),
                13 => "SynCharSet".to_string(),
                14 => "SynCharSetOne".to_string(),
                15 => "SynFile1".to_string(),
                16 => "SynOption1".to_string(),
                17 => "SynActions1".to_string(),
                18 => "SynAltItems1".to_string(),
                19 => "SynAltItem1".to_string(),
                20 => "SynCharSet1".to_string(),
            ], btreemap![
                0 => symbols![nt 2, nt 15],             //  0: file -> header file_1                  | ◄0 ►file_1 ►header            | header file_1
                1 => symbols![nt 15],                   //  1: file -> file_1                         | ◄1 ►file_1                    | file_1
                2 => symbols![nt 4],                    //  2: file_item -> option                    | ◄2 ►option                    | option
                3 => symbols![nt 3],                    //  3: file_item -> declaration               | ◄3 ►declaration               | declaration
                4 => symbols![nt 5],                    //  4: file_item -> rule                      | ◄4 ►rule                      | rule
                5 => symbols![t 27],                    //  5: header -> lexicon Id ;                 | ◄5 ; Id! lexicon              | Id
                6 => symbols![t 27],                    //  6: declaration -> mode Id ;               | ◄6 ; Id! mode                 | Id
                7 => symbols![t 27, nt 16],             //  7: option -> channels { Id option_1 }     | ◄7 } ►option_1 Id! { channels | Id option_1
                8 => symbols![t 27, nt 8],              //  8: rule -> fragment Id : match ;          | ◄8 ; ►match : Id! fragment    | Id match
                9 => symbols![],                        //  9: rule -> Id : match rule_1              | ►rule_1 ►match : Id!          |
                10 => symbols![nt 7, nt 17],            // 10: actions -> action actions_1            | ◄10 ►actions_1 ►action        | action actions_1
                11 => symbols![t 27],                   // 11: action -> mode ( Id )                  | ◄11 ) Id! ( mode              | Id
                12 => symbols![t 27],                   // 12: action -> push ( Id )                  | ◄12 ) Id! ( push              | Id
                13 => symbols![],                       // 13: action -> pop                          | ◄13 pop                       |
                14 => symbols![],                       // 14: action -> skip                         | ◄14 skip                      |
                15 => symbols![],                       // 15: action -> more                         | ◄15 more                      |
                16 => symbols![t 27],                   // 16: action -> type ( Id )                  | ◄16 ) Id! ( type              | Id
                17 => symbols![t 27],                   // 17: action -> channel ( Id )               | ◄17 ) Id! ( channel           | Id
                18 => symbols![nt 9],                   // 18: match -> alt_items                     | ◄18 ►alt_items                | alt_items
                19 => symbols![nt 10, nt 18],           // 19: alt_items -> alt_item alt_items_1      | ◄19 ►alt_items_1 ►alt_item    | alt_item alt_items_1
                20 => symbols![nt 19],                  // 20: alt_item -> alt_item_1                 | ◄20 ►alt_item_1               | alt_item_1
                21 => symbols![],                       // 21: repeat_item -> item repeat_item_1      | ►repeat_item_1 ►item          |
                22 => symbols![nt 9],                   // 22: item -> ( alt_items )                  | ◄22 ) ►alt_items (            | alt_items
                23 => symbols![nt 12],                  // 23: item -> ~ item                         | ◄23 ►item ~                   | item
                24 => symbols![t 27],                   // 24: item -> Id                             | ◄24 Id!                       | Id
                25 => symbols![],                       // 25: item -> CharLit item_1                 | ►item_1 CharLit!              |
                26 => symbols![t 29],                   // 26: item -> StrLit                         | ◄26 StrLit!                   | StrLit
                27 => symbols![nt 13],                  // 27: item -> char_set                       | ◄27 ►char_set                 | char_set
                28 => symbols![nt 20],                  // 28: char_set -> [ char_set_1 ]             | ◄28 ] ►char_set_1 [           | char_set_1
                29 => symbols![],                       // 29: char_set -> .                          | ◄29 .                         |
                30 => symbols![t 30],                   // 30: char_set -> FixedSet                   | ◄30 FixedSet!                 | FixedSet
                31 => symbols![t 30],                   // 31: char_set_one -> FixedSet               | ◄31 FixedSet!                 | FixedSet
                32 => symbols![],                       // 32: char_set_one -> SetChar char_set_one_1 | ►char_set_one_1 SetChar!      |
                33 => symbols![nt 15, nt 1],            // 33: file_1 -> file_item file_1             | ●file_1 ◄33 ►file_item        | file_1 file_item
                34 => symbols![nt 15],                  // 34: file_1 -> ε                            | ◄34                           | file_1
                35 => symbols![nt 16, t 27],            // 35: option_1 -> , Id option_1              | ●option_1 ◄35 Id! ,           | option_1 Id
                36 => symbols![nt 16],                  // 36: option_1 -> ε                          | ◄36                           | option_1
                37 => symbols![nt 17, nt 7],            // 37: actions_1 -> , action actions_1        | ●actions_1 ◄37 ►action ,      | actions_1 action
                38 => symbols![nt 17],                  // 38: actions_1 -> ε                         | ◄38                           | actions_1
                39 => symbols![nt 18, nt 10],           // 39: alt_items_1 -> | alt_item alt_items_1  | ●alt_items_1 ◄39 ►alt_item |  | alt_items_1 alt_item
                40 => symbols![nt 18],                  // 40: alt_items_1 -> ε                       | ◄40                           | alt_items_1
                41 => symbols![],                       // 41: alt_item_1 -> repeat_item alt_item_2   | ►alt_item_2 ►repeat_item      |
                42 => symbols![],                       // 42: char_set_1 -> char_set_one char_set_2  | ►char_set_2 ►char_set_one     |
                43 => symbols![t 27, nt 8, nt 6],       // 43: rule_1 -> -> actions ;                 | ◄43 ; ►actions ->             | Id match actions
                44 => symbols![t 27, nt 8],             // 44: rule_1 -> ;                            | ◄44 ;                         | Id match
                45 => symbols![],                       // 45: repeat_item_1 -> + repeat_item_2       | ►repeat_item_2 +              |
                46 => symbols![nt 12],                  // 46: repeat_item_1 -> ?                     | ◄46 ?                         | item
                47 => symbols![],                       // 47: repeat_item_1 -> * repeat_item_3       | ►repeat_item_3 *              |
                48 => symbols![nt 12],                  // 48: repeat_item_1 -> ε                     | ◄48                           | item
                49 => symbols![t 28, t 28],             // 49: item_1 -> .. CharLit                   | ◄49 CharLit! ..               | CharLit CharLit
                50 => symbols![t 28],                   // 50: item_1 -> ε                            | ◄50                           | CharLit
                51 => symbols![t 33, t 33],             // 51: char_set_one_1 -> - SetChar            | ◄51 SetChar! -                | SetChar SetChar
                52 => symbols![t 33],                   // 52: char_set_one_1 -> ε                    | ◄52                           | SetChar
                53 => symbols![nt 19, nt 11],           // 53: alt_item_2 -> alt_item_1               | ●alt_item_1 ◄53               | alt_item_1 repeat_item
                54 => symbols![nt 19, nt 11],           // 54: alt_item_2 -> ε                        | ◄54                           | alt_item_1 repeat_item
                55 => symbols![nt 20, nt 14],           // 55: char_set_2 -> char_set_1               | ●char_set_1 ◄55               | char_set_1 char_set_one
                56 => symbols![nt 20, nt 14],           // 56: char_set_2 -> ε                        | ◄56                           | char_set_1 char_set_one
                57 => symbols![nt 12],                  // 57: repeat_item_2 -> ?                     | ◄57 ?                         | item
                58 => symbols![nt 12],                  // 58: repeat_item_2 -> ε                     | ◄58                           | item
                59 => symbols![nt 12],                  // 59: repeat_item_3 -> ?                     | ◄59 ?                         | item
                60 => symbols![nt 12],                  // 60: repeat_item_3 -> ε                     | ◄60                           | item
            ], Default, btreemap![0 => vec![0, 1], 1 => vec![2, 3, 4], 2 => vec![5], 3 => vec![6], 4 => vec![7], 5 => vec![8, 43, 44], 6 => vec![10],
                7 => vec![11, 12, 13, 14, 15, 16, 17], 8 => vec![18], 9 => vec![19], 10 => vec![20], 11 => vec![46, 48, 57, 58, 59, 60],
                12 => vec![22, 23, 24, 26, 27, 49, 50], 13 => vec![28, 29, 30], 14 => vec![31, 51, 52]]),

            /*
            (, false, false, 0, btreemap![], btreemap![], Default, btreemap![]),
            */
        ];

        // those parsers don't require type definition in wrapper_code.rs (avoids an unused_imports warning):
        let type_gen_exclusion = |x: u32| matches!(x, 603..=632 | 153);

        const WRAPPER_FILENAME: &str = "tests/out/wrapper_source.rs";

        // print sources
        const VERBOSE: bool = false;        // prints the `tests` values from the results (easier to set the other constants to false)
        const VERBOSE_TYPE: bool = false;   // prints the code module skeleton (easier to set the other constants to false)
        const PRINT_SOURCE: bool = false;   // prints the wrapper module (easier to set the other constants to false)

        // test options
        const TEST_SOURCE: bool = true;
        const TESTS_ALL: bool = true;       // do all tests before giving an error summary (can't compare sources)

        // CAUTION! Setting this to 'true' modifies the validation file with the current result
        const REPLACE_SOURCE: bool = false;

        // CAUTION! Empty the first btreemap if the NTs have changed

        let mut num_errors = 0;
        let mut num_src_errors = 0;
        let mut rule_id_iter = HashMap::<u32, u32>::new();
        for (test_id, (tr_id, test_source, test_source_parser, start_nt, nt_type, expected_items, has_value, expected_alts)) in tests.into_iter().enumerate() {
            // if !matches!(tr_id, 150..160) { continue }
            let rule_iter = rule_id_iter.entry(tr_id).and_modify(|x| *x += 1).or_insert(1);
            let ll1_maybe = TestRules(tr_id).to_prs_ll1();
            if ll1_maybe.is_none() { continue }
            let ll1 = ll1_maybe.unwrap();
            let symtab = ll1.get_symbol_table();
            if VERBOSE {
                println!("// {:=<80}\n// Test {test_id}: TestRule({tr_id}) #{rule_iter}, start {start_nt}:", "");
                println!("/*");
                symtab.unwrap().dump("symbol table:");
                println!("Terminals: {}", ll1.get_symbol_table().unwrap()
                    .get_terminals().enumerate()
                    .map(|(i, (s1, s2))| format!("{i}:{s1}{}", if let Some(s2t) = s2 { format!("=\"{s2t}\"") } else { String::new() })).join(", "));
                println!("LL1 <-> origin:\n{}", indent_source(vec![ll1.prs_alt_origins_str(true)], 4));
            }
            let original_str = get_original_str(&ll1, 12);
            let mut builder = ParserGen::build_from_rules(ll1, "Test".to_string());
            let ambig_warnings = builder.log.get_warnings().filter(|w| w.contains("calc_table: ambiguity")).join("\n");
            let result_is_ambiguous = !ambig_warnings.is_empty();
            set_has_value(&mut builder, has_value.clone());
            if VERBOSE {
                println!("before, NT with value: {}",
                         (0..builder.parsing_table.num_nt).into_iter().filter_map(|v|
                             if builder.nt_value[v] { Some(Symbol::NT(v as VarId).to_str(builder.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            builder.make_item_ops();
            if VERBOSE {
                println!("after,  NT with value: {}",
                         (0..builder.parsing_table.num_nt).into_iter().filter_map(|v|
                             if builder.nt_value[v] { Some(Symbol::NT(v as VarId).to_str(builder.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            let result_items = builder.item_ops.iter().map(|(f, v)| (f.clone(), v.clone())).collect::<BTreeMap<AltId, Vec<Symbol>>>();
            let result_alts = (0..builder.parsing_table.num_nt).filter_map(|v|
                if builder.parsing_table.parent[v].is_none() { Some((v as VarId, builder.gather_alts(v as VarId))) } else { None }
            ).collect::<BTreeMap<_, _>>();
            let test_name = format!("wrapper source for rule {tr_id} #{rule_iter}, start {}", Symbol::NT(start_nt).to_str(builder.get_symbol_table()));
            let rule_name = format!("{tr_id}_{rule_iter}");
            if !type_gen_exclusion(tr_id) {
                builder.add_lib(&format!("super::super::wrapper_code::code_{rule_name}::*"));
            }
            for (v, s) in nt_type.clone() {
                builder.add_nt_type(v, s);
            }
            let result_nt_type = builder.nt_type.iter().map(|(v, s)| (*v, s.clone())).collect::<BTreeMap<_, _>>();
            if VERBOSE {
                let gather_alts = (0..(builder.parsing_table.num_nt as VarId)).map(|v| (v, builder.gather_alts(v))).to_vec();
                println!("gather_alts:\n{}", gather_alts.iter().map(|(v, alts)| {
                    format!(
                        "- {} -> {}",
                        Symbol::NT(*v).to_str(builder.get_symbol_table()),
                        alts.iter().map(|a| {
                            let (va, _) = builder.parsing_table.alts[*a as usize];
                            format!("({}: {a})", Symbol::NT(va).to_str(builder.get_symbol_table()))
                        }).join(", "))
                }).join("\n"));
                println!("{original_str}");
                print_flags(&builder, 12);
                println!("            ({tr_id}, {test_source}, {test_source_parser}, {start_nt}, btreemap![", );
                if !result_nt_type.is_empty() {
                    println!("{}", result_nt_type.iter().map(|(v, s)| format!("                {v} => \"{s}\".to_string(),")).join("\n"));
                }
                println!("            ], btreemap![");
                print_items(&builder, 16, true);
                let has_value_str = match &has_value {
                    Set(s) => format!("Set(symbols![{}])", s.iter().map(|s| s.to_macro_item()).join(", ")),
                    All => "All".to_string(),
                    Default => "Default".to_string()
                };
                println!("            ], {has_value_str}, btreemap![{}]),",
                    if result_alts.is_empty() { "".to_string() } else { result_alts.iter().map(|(v, a)| format!("{v} => vec![{}]", a.iter().join(", "))).join(", ") }
                );
                let nbr_alts = builder.parsing_table.alts.len();
                let original = (0..nbr_alts)
                    .filter_map(|i| builder.get_original_alt_str(i as AltId, builder.get_symbol_table()).and_then(|s| Some(format!("- {i:3}: {s}"))))
                    .join("\n");
                if !original.is_empty() {
                    println!("Original alts:\n{original}");
                }
                println!("*/");
            }
            let mut src = vec![];
            if test_source_parser {
                src.push(builder.source_build_parser())
            }
            src.push(builder.source_wrapper());
            let builder_has_errors = builder.log.num_errors() > 0;
            src.insert(0, builder.source_use());
            if VERBOSE && builder_has_errors {
                println!("log:\n{}", builder.get_log().get_messages_str());
            }
            if VERBOSE_TYPE {
                if result_is_ambiguous {
                    println!("parsing table has ambiguities:\n{ambig_warnings}")
                }
                if builder_has_errors {
                    println!("builder couldn't generate the source");
                } else {
                    println!("pub(crate) mod code_{rule_name} {{");
                    println!("    // {0:-<60}\n    // {test_name}", "");
                    let st: &crate::SymbolTable = builder.get_symbol_table().unwrap();
                    for v in 0..(st.get_num_nt() as VarId) {
                        if let Some((_s, src)) = builder.get_nt_extra_info(v) {
                            println!();
                            println!("{}", src.into_iter().map(|line| format!("    {line}")).join("\n"));
                        }
                    }
                    println!("}}\n");
                }
            }
            let result_src = indent_source(src, 4);
            if PRINT_SOURCE && !builder_has_errors {
                println!("pub(crate) mod rules_{rule_name} {{");
                println!("    // {0:-<60}\n    // [{test_name}]\n\n{result_src}\n    // [{test_name}]\n    // {:-<60}", "");
                println!("}}\n");
            }
            if VERBOSE {
                println!("tag:     [{test_name}]");
                println!("code:     code_{rule_name}");
            }
            let expected_src = if test_source && !cfg!(miri) {
                let src = get_tagged_source(WRAPPER_FILENAME, &test_name);
                if (TEST_SOURCE || REPLACE_SOURCE) && src.is_none() {
                    println!("## couldn't find the source code");
                }
                src
            } else {
                None
            };
            let err_msg = format!("test {test_id} TestRules({tr_id}) #{rule_iter} failed");
            if TESTS_ALL {
                if result_items != expected_items || result_alts != expected_alts || result_nt_type != nt_type {
                    num_errors += 1;
                    println!("## ERROR: {err_msg}");
                }
                if (test_source && !cfg!(miri) && TEST_SOURCE && Some(&result_src) != expected_src.as_ref()) || builder_has_errors {
                    if builder_has_errors {
                        println!("## ERRORS WHILE GENERATING SOURCE: {err_msg}");
                    } else {
                        if REPLACE_SOURCE {
                            if replace_tagged_source(WRAPPER_FILENAME, &test_name, &result_src).is_err() {
                                num_errors += 1;
                                println!("## ERROR: {err_msg}, couldn't replace source");
                            }
                        }
                        println!("## SOURCE MISMATCH: {err_msg}");
                    }
                    num_errors += 1;
                    num_src_errors += 1;
                }
                if result_is_ambiguous {
                    println!("## ERROR: {err_msg}, parsing table had ambiguities:\n{ambig_warnings}");
                }
            } else {
                assert_eq!(result_items, expected_items, "{err_msg}, different items");
                assert_eq!(result_alts, expected_alts, "{err_msg}, different alts");
                assert_eq!(result_nt_type, nt_type, "{err_msg}, different NT types");
                if !cfg!(miri) && TEST_SOURCE {
                    assert!(!builder_has_errors, "{} errors reported by source builder", builder.log.num_errors());
                    if REPLACE_SOURCE && expected_src.is_some() && &result_src != expected_src.as_ref().unwrap() && !builder_has_errors {
                        replace_tagged_source(WRAPPER_FILENAME, &test_name, &result_src).expect("replacement failed");
                    }
                    assert_eq!(Some(result_src), expected_src, "{err_msg}");
                }
                assert!(!result_is_ambiguous, "{err_msg}, parsing table had ambiguities:\n{ambig_warnings}");
            }
        }
        if TESTS_ALL {
            assert_eq!(num_errors, 0, "{num_errors} test(s) have failed, including {num_src_errors} source error(s)");
        }
    }

    #[test]
    /// Tests [ParserGen::full_alt_str].
    fn expand_lfact() {
        let tests: Vec<(u32, Vec<Option<&str>>)> = vec![
            // a -> A | B
            (2, vec![
                Some(r#"a -> A"#),                // 0: a -> A
                Some(r#"a -> B"#),                // 1: a -> B
            ]),
            // a -> A B* C
            (102, vec![
                Some(r#"a -> A B* C"#),                       // 0: a -> A a_1 C
                Some(r#"`B` item in `a -> A  ►► B ◄◄ * C`"#), // 1: a_1 -> B a_1
                None,                                         // 2: a_1 -> ε
            ]),
            // a -> A B+ C
            (103, vec![
                Some(r#"a -> A B+ C"#),                       // 0: a -> A a_1 C
                Some(r#"`B` item in `a -> A  ►► B ◄◄ + C`"#), // 1: a_1 -> B a_2
                Some(r#"`B` item in `a -> A  ►► B ◄◄ + C`"#), // 2: a_2 -> a_1
                Some(r#"`B` item in `a -> A  ►► B ◄◄ + C`"#), // 3: a_2 -> ε
            ]),
            // a -> A (<L=i> B)+ C
            (201, vec![
                Some(r#"a -> A (<L> B)+ C"#),                                // 0: a -> A i C
                Some(r#"`<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`"#), // 1: i -> B a_1
                Some(r#"`<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`"#), // 2: a_1 -> i
                Some(r#"`<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`"#), // 3: a_1 -> ε
            ]),
            // a -> (<L=i> b A b B A)*
            // b -> C
            (202, vec![
                Some(r#"a -> (<L> b A b B A)*"#),                                        // 0: a -> i
                Some(r#"`<L> b A b B A` iteration in `a -> ( ►► <L> b A b B A ◄◄ )*`"#), // 1: i -> b A b B A i
                None,                                                                    // 2: i -> ε
                Some(r#"b -> C"#),                                                       // 3: b -> C
            ]),
            // a -> (<L=i> A (<L=j> b ",")* ";")* C
            // b -> B
            (208, vec![
                Some(r#"a -> (<L> A (<L> b ",")* ";")* C"#),                                                 // 0: a -> i C
                Some(r#"`<L> A (<L> b ",")* ";"` iteration in `a -> ( ►► <L> A (<L> b ",")* ";" ◄◄ )* C`"#), // 1: i -> A j ";" i
                None,                                                                                        // 2: i -> ε
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )* ";")* C`"#),              // 3: j -> b "," j
                None,                                                                                        // 4: j -> ε
                Some(r#"b -> B"#),                                                                           // 5: b -> B
            ]),
            // a -> (<L=i> A (<L=j> b ",")+ ";")+ C
            // b -> B
            (209, vec![
                Some(r#"a -> (<L> A (<L> b ",")+ ";")+ C"#),                                                 // 0: a -> i C
                Some(r#"`<L> A (<L> b ",")+ ";"` iteration in `a -> ( ►► <L> A (<L> b ",")+ ";" ◄◄ )+ C`"#), // 1: i -> A j ";" a_1
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )+ ";")+ C`"#),              // 2: j -> b "," a_2
                Some(r#"b -> B"#),                                                                           // 3: b -> B
                Some(r#"`<L> A (<L> b ",")+ ";"` iteration in `a -> ( ►► <L> A (<L> b ",")+ ";" ◄◄ )+ C`"#), // 4: a_1 -> i
                Some(r#"`<L> A (<L> b ",")+ ";"` iteration in `a -> ( ►► <L> A (<L> b ",")+ ";" ◄◄ )+ C`"#), // 5: a_1 -> ε
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )+ ";")+ C`"#),              // 6: a_2 -> j
                Some(r#"`<L> b ","` iteration in `a -> (<L> A ( ►► <L> b "," ◄◄ )+ ";")+ C`"#),              // 7: a_2 -> ε
            ]),
            // a -> (<L=i> A | B)*
            (250, vec![
                Some(r#"a -> (<L> A | B)*"#),                                // 0: a -> i
                Some(r#"`<L> A` iteration in `a -> ( ►► <L> A ◄◄  | B)*`"#), // 1: i -> A i
                Some(r#"`B` iteration in `a -> (<L> A |  ►► B ◄◄ )*`"#),     // 2: i -> B i
                None,                                                        // 3: i -> ε
            ]),
            // expr -> Id "." expr | "(" Num ")"
            (301, vec![
                Some(r#"expr -> Id "." expr"#),   // 0: expr -> Id "." expr
                Some(r#"expr -> "(" Num ")""#),   // 1: expr -> "(" Num ")"
            ]),
            // expr -> <L=expr> Id "." expr | "(" Num ")"
            (401, vec![
                Some(r#"expr -> <L> Id "." expr"#), // 0: expr -> Id "." expr
                Some(r#"expr -> "(" Num ")""#),     // 1: expr -> "(" Num ")"
            ]),
            // a -> a "!" | "?"
            (500, vec![
                Some(r#"a -> "?""#),              // 0: a -> "?" a_1
                Some(r#"a -> a "!""#),            // 1: a_1 -> "!" a_1
                None,                             // 2: a_1 -> ε
            ]),
            // a -> a "b" | a "c" | "a"
            (501, vec![
                Some(r#"a -> "a""#),              // 0: a -> "a" a_1
                Some(r#"a -> a "b""#),            // 1: a_1 -> "b" a_1
                Some(r#"a -> a "c""#),            // 2: a_1 -> "c" a_1
                None,                             // 3: a_1 -> ε
            ]),
            // e -> e "!" | "-" e | Num
            (580, vec![
                Some(r#"e -> "-" e"#),            // 0: e -> "-" e
                Some(r#"e -> Num"#),              // 1: e -> Num e_1
                Some(r#"e -> e "!""#),            // 2: e_1 -> "!" e_1
                None,                             // 3: e_1 -> ε
            ]),
            // e -> e "!" | <L=e> "-" e | Num
            (581, vec![
                Some(r#"e -> <L> "-" e"#),        // 0: e -> "-" e
                Some(r#"e -> Num"#),              // 1: e -> Num e_1
                Some(r#"e -> e "!""#),            // 2: e_1 -> "!" e_1
                None,                             // 3: e_1 -> ε
            ]),
            // e -> e "*" e | e "+" e | "!" e | Num
            (603, vec![
                None,                             // 0: e -> e_4 e_1
                Some(r#"e -> e "*" e"#),          // 1: e_1 -> "*" e_4 e_1
                Some(r#"e -> e "+" e"#),          // 2: e_1 -> "+" e_2 e_1
                None,                             // 3: e_1 -> ε
                None,                             // 4: e_2 -> e_4 e_3
                Some(r#"e -> e "*" e"#),          // 5: e_3 -> "*" e_4 e_3
                None,                             // 6: e_3 -> ε
                Some(r#"e -> "!" e"#),            // 7: e_4 -> "!" e
                Some(r#"e -> Num"#),              // 8: e_4 -> Num
            ]),
            // e -> e "*" e | e "+" | "!" e | Num
            (630, vec![
                None,                             // 0: e -> e_2 e_1
                Some(r#"e -> e "*" e"#),          // 1: e_1 -> "*" e_2 e_1
                Some(r#"e -> e "+""#),            // 2: e_1 -> "+" e_1
                None,                             // 3: e_1 -> ε
                Some(r#"e -> "!" e"#),            // 4: e_2 -> "!" e
                Some(r#"e -> Num"#),              // 5: e_2 -> Num
            ]),
            // a -> A | A B
            (700, vec![
                None,                             // 0: a -> A a_1
                Some(r#"a -> A B"#),              // 1: a_1 -> B
                Some(r#"a -> A"#),                // 2: a_1 -> ε
            ]),
            // a -> A B C | B B C | B C | B B A
            (704, vec![
                Some(r#"a -> A B C"#),            // 0: a -> A B C
                None,                             // 1: a -> B a_1
                None,                             // 2: a_1 -> B a_2
                Some(r#"a -> B C"#),              // 3: a_1 -> C
                Some(r#"a -> B B A"#),            // 4: a_2 -> A
                Some(r#"a -> B B C"#),            // 5: a_2 -> C
            ]),
            // a -> A* B a | C
            (810, vec![
                Some(r#"a -> A* B a"#),                           // 0: a -> a_1 B a
                Some(r#"a -> C"#),                                // 1: a -> C
                Some(r#"`A` item in `a ->  ►► A ◄◄ * B a | C`"#), // 2: a_1 -> A a_1
                None,                                             // 3: a_1 -> ε
            ]),
            // a -> A+ B a | C
            (811, vec![
                Some(r#"a -> A+ B a"#),                           // 0: a -> a_1 B a
                Some(r#"a -> C"#),                                // 1: a -> C
                Some(r#"`A` item in `a ->  ►► A ◄◄ + B a | C`"#), // 2: a_1 -> A a_2
                Some(r#"`A` item in `a ->  ►► A ◄◄ + B a | C`"#), // 3: a_2 -> a_1
                Some(r#"`A` item in `a ->  ►► A ◄◄ + B a | C`"#), // 4: a_2 -> ε
            ]),
            // a -> a A* C | B
            (820, vec![
                Some(r#"a -> B"#),                                // 0: a -> B a_2
                Some(r#"`A` item in `a -> a  ►► A ◄◄ * C | B`"#), // 1: a_1 -> A a_1
                None,                                             // 2: a_1 -> ε
                Some(r#"a -> a A* C"#),                           // 3: a_2 -> a_1 C a_2
                None,                                             // 4: a_2 -> ε
            ]),
            // a -> a A+ C | B
            (821, vec![
                Some(r#"a -> B"#),                                // 0: a -> B a_2
                Some(r#"`A` item in `a -> a  ►► A ◄◄ + C | B`"#), // 1: a_1 -> A a_3
                Some(r#"a -> a A+ C"#),                           // 2: a_2 -> a_1 C a_2
                None,                                             // 3: a_2 -> ε
                Some(r#"`A` item in `a -> a  ►► A ◄◄ + C | B`"#), // 4: a_3 -> a_1
                Some(r#"`A` item in `a -> a  ►► A ◄◄ + C | B`"#), // 5: a_3 -> ε
            ]),
            // a -> a A | B C | B D
            (870, vec![
                None,                             // 0: a -> B a_2
                Some(r#"a -> a A"#),              // 1: a_1 -> A a_1
                None,                             // 2: a_1 -> ε
                Some(r#"a -> B C"#),              // 3: a_2 -> C a_1
                Some(r#"a -> B D"#),              // 4: a_2 -> D a_1
            ]),
            // a -> a A B | a A C | D
            (871, vec![
                Some(r#"a -> D"#),                // 0: a -> D a_1
                None,                             // 1: a_1 -> A a_2
                None,                             // 2: a_1 -> ε
                Some(r#"a -> a A B"#),            // 3: a_2 -> B a_1
                Some(r#"a -> a A C"#),            // 4: a_2 -> C a_1
            ]),
            /*
            (999, vec![
            ]),
            */
        ];
        const VERBOSE: bool = false;
        const VERBOSE_SOLUTION: bool = false;
        let mut rule_id_iter = HashMap::<u32, u32>::new();
        let mut errors = 0;
        for (test_id, (tr_id, expected_full)) in tests.into_iter().enumerate() {
            let rule_iter = rule_id_iter.entry(tr_id).and_modify(|x| *x += 1).or_insert(1);
            if VERBOSE { println!("// {:=<80}\n// Test {test_id}: rules {tr_id} #{rule_iter}:", ""); }

            let expected_full = expected_full.into_iter()
                .map(|opt| if let Some(s) = opt { format!("Some(r#\"{s}\"#)") } else { "None".to_string() })
                .to_vec();
            let ll1 = TestRules(tr_id).to_prs_ll1().unwrap();
            let original_str = get_original_str(&ll1, 12);
            let builder = ParserGen::build_from_rules(ll1, "Test".to_string());
            let symtable = builder.get_symbol_table();
            let mut result_full = vec![];
            for (a_id, (_v, a)) in builder.parsing_table.alts.iter().index() {
                result_full.push(
                    format!("{}", if let Some(s) = a.get_origin().and_then(|_| Some(builder.full_alt_str(a_id, None, false))) {
                        format!("Some(r#\"{s}\"#)")
                    } else {
                        "None".to_string()
                    }));
            }
            if VERBOSE_SOLUTION || VERBOSE {
                println!("{original_str}");
                println!("            ({tr_id}, vec![", );
                let cols = result_full.iter().enumerate()
                    .map(|(i, s_full)| {
                        let (v, prod) = &builder.parsing_table.alts[i];
                        vec![
                            "".to_string(),
                            format!("{s_full},"),
                            format!("// {i}: {}", alt_to_rule_str(*v, prod, symtable)),
                        ]
                    })
                    .to_vec();
                let lines = columns_to_str(cols, Some(vec![16, 34, 0]));
                println!("{}", lines.join("\n"));
                println!("            ]),");
            }
            if result_full != expected_full {
                errors += 1;
                if VERBOSE {
                    println!("## ERROR: {result_full:?} doesn't match {expected_full:?}")
                }
            }
        }
        assert_eq!(errors, 0);
    }
}
