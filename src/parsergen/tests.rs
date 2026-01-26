// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use iter_index::IndexerIterator;
use lexigram_core::CollectJoin;
use crate::grammar::{grtree_to_str, ProdRuleSet};
use crate::parser::Symbol;
use crate::{VarId, LL1};

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
    use lexigram_core::CollectJoin;
    use crate::grammar::ProdRuleSet;
    use crate::LL1;
    use crate::grammar::tests::TestRules;
    use lexigram_core::log::{LogReader, LogStatus};
    use crate::build::BuildFrom;
    use crate::parsergen::ParserGen;
    use crate::file_utils::{get_tagged_source, replace_tagged_source};

    fn get_source(tr_id: u32, indent: usize, include_alts: bool, name: String) -> String {
        let rules = TestRules(tr_id).to_prs_general().expect(&format!("invalid test rule ID #{tr_id}"));
        assert_eq!(rules.get_log().num_errors(), 0, "building {tr_id} failed:\n- {}", rules.get_log().get_errors().join("\n- "));
        let ll1 = ProdRuleSet::<LL1>::build_from(rules);
        let mut builder = ParserGen::build_from_rules(ll1, name);
        builder.set_include_alts(include_alts);
        builder.use_full_lib(true);
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
                    let result = get_tagged_source(FILENAME, tag).map_err(|e| {
                        println!("source not found for {id} / {tr_id} / {tag} ({name}): {e}");
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
    use lexigram_core::{strip, CollectJoin};
    use crate::build::BuildFrom;
    use crate::parser::Symbol;
    use crate::{columns_to_str, VarId};
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
    use lexigram_core::log::{LogReader, LogStatus};
    use crate::build::BuildFrom;
    use crate::parsergen::{ParserGen, ParserTables};

    #[test]
    fn alternatives() {
        for include_alts in [false, true] {
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
    use lexigram_core::alt::alt_to_rule_str;
    use crate::grammar::tests::TestRules;
    use crate::parser::Symbol;
    use crate::{btreemap, columns_to_str, indent_source, symbols, AltId, VarId};
    use crate::parsergen::{NTValue, ParserGen};
    use lexigram_core::log::{LogReader, LogStatus};
    use crate::parsergen::SpanNbr;
    use crate::parsergen::tests::get_original_str;
    use crate::file_utils::{get_tagged_source, replace_tagged_source};
    use lexigram_core::CollectJoin;

    #[allow(unused_doc_comments)]
    #[allow(unused_variables)]
    #[allow(unused_mut)]
    /// Tests [ParserGen::source_build_parser], [ParserGen::source_wrapper], [ParserGen::source_use], and [ParserGen::make_item_ops].
    fn build_items(mut enable_test_source: bool, mut tests_all: bool, mut replace_source: bool) {
        let tests: Vec<(
            u32,                                    // TestRules #
            bool,                                   // test sources?
            bool,                                   // test sources include parser?
            u16,                                    // start NT
            BTreeMap<VarId, String>,                // NT types
            BTreeMap<u16, (SpanNbr, Vec<Symbol>)>,  // expected items
            // BTreeMap<u16, Vec<Symbol>>,  // expected items
            NTValue,                                // which symbols have a value
            BTreeMap<VarId, Vec<AltId>>,            // expected alt groups
        )> = vec![

            // CAUTION! Empty the first btreemap if the NTs have changed
            // ---------------------------------------------------------------------------
            // a -> A B
            (1, false, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![t 0, t 1]),           //  0: a -> A B | ◄0 B! A! | 2 | A B
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- NT/T simple mix
            // s -> Id "=" val | "exit" | "return" val
            // val -> Id | Num
            (13, true, false, 0, btreemap![
                0 => "SynS".to_string(),
                1 => "SynVal".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, nt 1]),          //  0: s -> Id "=" val   | ◄0 ►val "=" Id!  | 3 | Id val
                1 => (1, symbols![]),                   //  1: s -> "exit"       | ◄1 "exit"        | 1 |
                2 => (2, symbols![nt 1]),               //  2: s -> "return" val | ◄2 ►val "return" | 2 | val
                3 => (1, symbols![t 0]),                //  3: val -> Id         | ◄3 Id!           | 1 | Id
                4 => (1, symbols![t 4]),                //  4: val -> Num        | ◄4 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),

            // --------------------------------------------------------------------------- NT with/without value
            // a -> b c | c
            // b -> Op c
            // c -> Id
            (14, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![nt 1, nt 2]),         //  0: a -> b c  | ◄0 ►c ►b  | 2 | b c
                1 => (1, symbols![nt 2]),               //  1: a -> c    | ◄1 ►c     | 1 | c
                2 => (2, symbols![t 0, nt 2]),          //  2: b -> Op c | ◄2 ►c Op! | 2 | Op c
                3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
            ], NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
            (14, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![nt 1]),               //  0: a -> b c  | ◄0 ►c ►b  | 2 | b
                1 => (1, symbols![]),                   //  1: a -> c    | ◄1 ►c     | 1 |
                2 => (2, symbols![t 0]),                //  2: b -> Op c | ◄2 ►c Op! | 2 | Op
                3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
            ], NTValue::SetIds(vec![0, 1]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
            (14, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![nt 2]),               //  0: a -> b c  | ◄0 ►c ►b  | 2 | c
                1 => (1, symbols![nt 2]),               //  1: a -> c    | ◄1 ►c     | 1 | c
                2 => (2, symbols![t 0, nt 2]),          //  2: b -> Op c | ◄2 ►c Op! | 2 | Op c
                3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
            ], NTValue::SetIds(vec![0, 2]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
            (14, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![]),                   //  0: a -> b c  | ◄0 ►c ►b  | 2 |
                1 => (1, symbols![]),                   //  1: a -> c    | ◄1 ►c     | 1 |
                2 => (2, symbols![t 0]),                //  2: b -> Op c | ◄2 ►c Op! | 2 | Op
                3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
            ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),

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
                0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
                1 => (2, symbols![nt 1, t 1]),          //  1: a_1 -> B a_1 | ●a_1 ◄1 B!    | 2 | a_1 B
                2 => (1, symbols![nt 1]),               //  2: a_1 -> ε     | ◄2            | 1 | a_1
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
                1 => (0, symbols![]),                   //  1: a_1 -> B a_2 | ►a_2 B!       | 0 |
                2 => (2, symbols![nt 1, t 1]),          //  2: a_2 -> a_1   | ●a_1 ◄2       | 2 | a_1 B
                3 => (2, symbols![nt 1, t 1]),          //  3: a_2 -> ε     | ◄3            | 2 | a_1 B
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> (b A b B A)*
            // b -> C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (104, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 2]),                            //  0: a -> a_1             | ◄0 ►a_1                | 1 | a_1
                1 => (1, symbols![t 2]),                             //  1: b -> C               | ◄1 C!                  | 1 | C
                2 => (6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  2: a_1 -> b A b B A a_1 | ●a_1 ◄2 A! B! ►b A! ►b | 6 | a_1 b A b B A
                3 => (1, symbols![nt 2]),                            //  3: a_1 -> ε             | ◄3                     | 1 | a_1
            ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

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
                0 => (1, symbols![nt 2]),                            //  0: a -> a_1             | ◄0 ►a_1             | 1 | a_1
                1 => (1, symbols![t 2]),                             //  1: b -> C               | ◄1 C!               | 1 | C
                2 => (0, symbols![]),                                //  2: a_1 -> b A b B A a_2 | ►a_2 A! B! ►b A! ►b | 0 |
                3 => (6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  3: a_2 -> a_1           | ●a_1 ◄3             | 6 | a_1 b A b B A
                4 => (6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  4: a_2 -> ε             | ◄4                  | 6 | a_1 b A b B A
            ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

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
                0 => (2, symbols![nt 3, t 3]),          //  0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
                1 => (1, symbols![t 4]),                //  1: b -> B               | ◄1 B!               | 1 | B
                2 => (3, symbols![nt 2, nt 1]),         //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 | a_1 b
                3 => (1, symbols![nt 2]),               //  3: a_1 -> ε             | ◄3                  | 1 | a_1
                4 => (4, symbols![nt 3, t 0, nt 2]),    //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A a_1
                5 => (1, symbols![nt 3]),               //  5: a_2 -> ε             | ◄5                  | 1 | a_2
            ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),
            (106, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![nt 3, t 3]),          //  0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
                1 => (1, symbols![t 4]),                //  1: b -> B               | ◄1 B!               | 1 | B
                2 => (3, symbols![]),                   //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 |
                3 => (1, symbols![]),                   //  3: a_1 -> ε             | ◄3                  | 1 |
                4 => (4, symbols![nt 3, t 0]),          //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A
                5 => (1, symbols![nt 3]),               //  5: a_2 -> ε             | ◄5                  | 1 | a_2
            ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 1 => vec![1]]),

            // a -> A "B"* C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (108, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, t 2]),           //  0: a -> A a_1 C   | ◄0 C! ►a_1 A! | 3 | A C
                1 => (2, symbols![]),                   //  1: a_1 -> "B" a_1 | ●a_1 ◄1 "B"   | 2 |
                2 => (1, symbols![]),                   //  2: a_1 -> ε       | ◄2            | 1 |
            ], NTValue::Default, btreemap![0 => vec![0]]),

                0 => (4, symbols![t 0, nt 2]),          //  0: a -> Id "(" Id ":" type a_1 ")" | ◄0 ")" ►a_1 ►type ":" Id! "(" Id! | 4    | Id a_1
                1 => (1, symbols![t 0]),                //  1: type -> Id                      | ◄1 Id!                            | 1    | Id
                2 => (5, symbols![nt 2, t 0, nt 1]),    //  2: a_1 -> "," Id ":" type a_1      | ●a_1 ◄2 ►type ":" Id! ","         | 5, 3 | a_1 Id type
                3 => (1, symbols![nt 2]),               //  3: a_1 -> ε                        | ◄3                                | 1    | a_1

            // a -> Id "(" (Id ":" type ("," Id ":" type)*)? ")";
            //
            // a -> Id "(" Id ":" type ("," Id ":" type)* ")" | Id "(" ")"
            // type -> Id
            // NT flags:
            //  - a: parent_left_fact | parent_+_or_* (2080)
            //  - a_1: child_+_or_* | sep_list (32769)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            (110, true, false, 0, btreemap![
            ], btreemap![
                0 => (0, symbols![]),                   //  0: a -> Id "(" a_2            | ►a_2 "(" Id!              | 0    |
                1 => (1, symbols![t 0]),                //  1: type -> Id                 | ◄1 Id!                    | 1    | Id
                2 => (5, symbols![nt 2, t 0, nt 1]),    //  2: a_1 -> "," Id ":" type a_1 | ●a_1 ◄2 ►type ":" Id! "," | 5, 3 | a_1 Id type
                3 => (1, symbols![nt 2]),               //  3: a_1 -> ε                   | ◄3                        | 1    | a_1
                4 => (4, symbols![t 0, nt 2]),          //  4: a_2 -> Id ":" type a_1 ")" | ◄4 ")" ►a_1 ►type ":" Id! | 4    | Id a_1
                5 => (3, symbols![t 0]),                //  5: a_2 -> ")"                 | ◄5 ")"                    | 3    | Id
            ], NTValue::Default, btreemap![0 => vec![4, 5], 1 => vec![1]]),

            // a -> Id "(" Id ("," Id)* "/" Id ("," Id)* ")"
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* | sep_list (32769)
            //  - a_2: child_+_or_* | sep_list (32769)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            (111, false, false, 0, btreemap![
            ], btreemap![
                0 => (6, symbols![t 0, nt 1, nt 2]),    //  0: a -> Id "(" Id a_1 "/" Id a_2 ")" | ◄0 ")" ►a_2 Id! "/" ►a_1 Id! "(" Id! | 6    | Id a_1 a_2
                1 => (3, symbols![nt 1, t 0]),          //  1: a_1 -> "," Id a_1                 | ●a_1 ◄1 Id! ","                      | 3, 1 | a_1 Id
                2 => (1, symbols![nt 1]),               //  2: a_1 -> ε                          | ◄2                                   | 1    | a_1
                3 => (3, symbols![nt 2, t 0]),          //  3: a_2 -> "," Id a_2                 | ●a_2 ◄3 Id! ","                      | 3, 1 | a_2 Id
                4 => (1, symbols![nt 2]),               //  4: a_2 -> ε                          | ◄4                                   | 1    | a_2
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- norm+/* alternatives
            // a -> (A | B)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (150, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
                1 => (2, symbols![nt 1, t 0]),          //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | 2 | a_1 A
                2 => (2, symbols![nt 1, t 1]),          //  2: a_1 -> B a_1 | ●a_1 ◄2 B! | 2 | a_1 B
                3 => (1, symbols![nt 1]),               //  3: a_1 -> ε     | ◄3         | 1 | a_1
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
                1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
                2 => (0, symbols![]),                   //  2: a_1 -> B a_3 | ►a_3 B! | 0 |
                3 => (2, symbols![nt 1, t 0]),          //  3: a_2 -> a_1   | ●a_1 ◄3 | 2 | a_1 A
                4 => (2, symbols![nt 1, t 0]),          //  4: a_2 -> ε     | ◄4      | 2 | a_1 A
                5 => (2, symbols![nt 1, t 1]),          //  5: a_3 -> a_1   | ●a_1 ◄5 | 2 | a_1 B
                6 => (2, symbols![nt 1, t 1]),          //  6: a_3 -> ε     | ◄6      | 2 | a_1 B
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> A (B | b C b B C | E)* F
            // b -> D
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - a_1: child_+_or_* (1)
            // parents:
            //  - a_1 -> a
            (152, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0, nt 2, t 4]),                  //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!          | 3 | A a_1 F
                1 => (1, symbols![t 5]),                             //  1: b -> D               | ◄1 D!                  | 1 | D
                2 => (2, symbols![nt 2, t 1]),                       //  2: a_1 -> B a_1         | ●a_1 ◄2 B!             | 2 | a_1 B
                3 => (6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), //  3: a_1 -> b C b B C a_1 | ●a_1 ◄3 C! B! ►b C! ►b | 6 | a_1 b C b B C
                4 => (2, symbols![nt 2, t 3]),                       //  4: a_1 -> E a_1         | ●a_1 ◄4 E!             | 2 | a_1 E
                5 => (1, symbols![nt 2]),                            //  5: a_1 -> ε             | ◄5                     | 1 | a_1
            ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

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
                0 => (3, symbols![t 0, nt 2, t 4]),                   //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!       | 3 | A a_1 F
                1 => (1, symbols![t 5]),                              //  1: b -> D               | ◄1 D!               | 1 | D
                2 => (0, symbols![]),                                 //  2: a_1 -> B a_2         | ►a_2 B!             | 0 |
                3 => (0, symbols![]),                                 //  3: a_1 -> E a_3         | ►a_3 E!             | 0 |
                4 => (0, symbols![]),                                 //  4: a_1 -> b C b B C a_4 | ►a_4 C! B! ►b C! ►b | 0 |
                5 => (2, symbols![nt 2, t 1]),                        //  5: a_2 -> a_1           | ●a_1 ◄5             | 2 | a_1 B
                6 => (2, symbols![nt 2, t 1]),                        //  6: a_2 -> ε             | ◄6                  | 2 | a_1 B
                7 => (2, symbols![nt 2, t 3]),                        //  7: a_3 -> a_1           | ●a_1 ◄7             | 2 | a_1 E
                8 => (2, symbols![nt 2, t 3]),                        //  8: a_3 -> ε             | ◄8                  | 2 | a_1 E
                9 => (6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]),  //  9: a_4 -> a_1           | ●a_1 ◄9             | 6 | a_1 b C b B C
                10 => (6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), // 10: a_4 -> ε             | ◄10                 | 6 | a_1 b C b B C
            ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

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
                0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
                1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
                2 => (2, symbols![nt 1, t 2]),          //  2: a_1 -> C a_1 | ●a_1 ◄2 C! | 2 | a_1 C
                3 => (1, symbols![nt 1]),               //  3: a_1 -> ε     | ◄3         | 1 | a_1
                4 => (3, symbols![nt 1, t 0, t 1]),     //  4: a_2 -> B a_1 | ●a_1 ◄4 B! | 3 | a_1 A B
                5 => (2, symbols![nt 1, t 0]),          //  5: a_2 -> a_1   | ●a_1 ◄5    | 2 | a_1 A
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
                1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
                2 => (0, symbols![]),                   //  2: a_1 -> C a_3 | ►a_3 C! | 0 |
                3 => (0, symbols![]),                   //  3: a_2 -> B a_4 | ►a_4 B! | 0 |
                4 => (2, symbols![nt 1, t 0]),          //  4: a_2 -> a_1   | ●a_1 ◄4 | 2 | a_1 A
                5 => (2, symbols![nt 1, t 0]),          //  5: a_2 -> ε     | ◄5      | 2 | a_1 A
                6 => (2, symbols![nt 1, t 2]),          //  6: a_3 -> a_1   | ●a_1 ◄6 | 2 | a_1 C
                7 => (2, symbols![nt 1, t 2]),          //  7: a_3 -> ε     | ◄7      | 2 | a_1 C
                8 => (3, symbols![nt 1, t 0, t 1]),     //  8: a_4 -> a_1   | ●a_1 ◄8 | 3 | a_1 A B
                9 => (3, symbols![nt 1, t 0, t 1]),     //  9: a_4 -> ε     | ◄9      | 3 | a_1 A B
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (3, symbols![t 0, nt 2, t 6]),     //  0: a -> A a_2 G     | ◄0 G! ►a_2 A!   | 3 | A a_2 G
                1 => (3, symbols![nt 1, t 1, t 2]),     //  1: a_1 -> B C a_1   | ●a_1 ◄1 C! B!   | 3 | a_1 B C
                2 => (2, symbols![nt 1, t 3]),          //  2: a_1 -> D a_1     | ●a_1 ◄2 D!      | 2 | a_1 D
                3 => (1, symbols![nt 1]),               //  3: a_1 -> ε         | ◄3              | 1 | a_1
                4 => (3, symbols![nt 2, nt 1, t 4]),    //  4: a_2 -> a_1 E a_2 | ●a_2 ◄4 E! ►a_1 | 3 | a_2 a_1 E
                5 => (2, symbols![nt 2, t 5]),          //  5: a_2 -> F a_2     | ●a_2 ◄5 F!      | 2 | a_2 F
                6 => (1, symbols![nt 2]),               //  6: a_2 -> ε         | ◄6              | 1 | a_2
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (3, symbols![t 0, nt 2, t 6]),     //  0: a -> A a_2 G     | ◄0 G! ►a_2 A! | 3 | A a_2 G
                1 => (0, symbols![]),                   //  1: a_1 -> B C a_3   | ►a_3 C! B!    | 0 |
                2 => (0, symbols![]),                   //  2: a_1 -> D a_4     | ►a_4 D!       | 0 |
                3 => (0, symbols![]),                   //  3: a_2 -> F a_5     | ►a_5 F!       | 0 |
                4 => (0, symbols![]),                   //  4: a_2 -> a_1 E a_6 | ►a_6 E! ►a_1  | 0 |
                5 => (3, symbols![nt 1, t 1, t 2]),     //  5: a_3 -> a_1       | ●a_1 ◄5       | 3 | a_1 B C
                6 => (3, symbols![nt 1, t 1, t 2]),     //  6: a_3 -> ε         | ◄6            | 3 | a_1 B C
                7 => (2, symbols![nt 1, t 3]),          //  7: a_4 -> a_1       | ●a_1 ◄7       | 2 | a_1 D
                8 => (2, symbols![nt 1, t 3]),          //  8: a_4 -> ε         | ◄8            | 2 | a_1 D
                9 => (2, symbols![nt 2, t 5]),          //  9: a_5 -> a_2       | ●a_2 ◄9       | 2 | a_2 F
                10 => (2, symbols![nt 2, t 5]),         // 10: a_5 -> ε         | ◄10           | 2 | a_2 F
                11 => (3, symbols![nt 2, nt 1, t 4]),   // 11: a_6 -> a_2       | ●a_2 ◄11      | 3 | a_2 a_1 E
                12 => (3, symbols![nt 2, nt 1, t 4]),   // 12: a_6 -> ε         | ◄12           | 3 | a_2 a_1 E
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
                1 => (2, symbols![nt 1, t 1]),          //  1: i -> B i   | ●i ◄1 B!    | 2 | i B
                2 => (1, symbols![nt 1]),               //  2: i -> ε     | ◄2          | 1 | i
            ], NTValue::Default, btreemap![0 => vec![0]]),
            (200, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, t 2]),           //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
                1 => (2, symbols![t 1]),                //  1: i -> B i   | ●i ◄1 B!    | 2 | B
                2 => (1, symbols![]),                   //  2: i -> ε     | ◄2          | 1 |
            ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

            // a -> A (<L=i> B)+ C
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - i_1: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - i_1 -> i
            (201, true, false, 0, btreemap![
                0 => "SynMyA".to_string(),
                1 => "SynMyI".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
                1 => (0, symbols![]),                   //  1: i -> B i_1 | ►i_1 B!     | 0 |
                2 => (2, symbols![nt 1, t 1]),          //  2: i_1 -> i   | ●i ◄2       | 2 | i B
                3 => (2, symbols![nt 1, t 1]),          //  3: i_1 -> ε   | ◄3          | 2 | i B
            ], NTValue::Default, btreemap![0 => vec![0]]),
            (201, true, false, 0, btreemap![
                0 => "SynMyA".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, t 2]),           //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
                1 => (0, symbols![]),                   //  1: i -> B i_1 | ►i_1 B!     | 0 |
                2 => (2, symbols![t 1]),                //  2: i_1 -> i   | ●i ◄2       | 2 | B
                3 => (2, symbols![t 1]),                //  3: i_1 -> ε   | ◄3          | 2 | B
            ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),
            (201, true, false, 0, btreemap![
                0 => "SynMyA".to_string(),
                1 => "SynMyI".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
                1 => (0, symbols![]),                   //  1: i -> B i_1 | ►i_1 B!     | 0 |
                2 => (2, symbols![nt 1, t 1]),          //  2: i_1 -> i   | ●i ◄2       | 2 | i B
                3 => (2, symbols![nt 1, t 1]),          //  3: i_1 -> ε   | ◄3          | 2 | i B
            ], NTValue::SetIds(vec![1]), btreemap![0 => vec![0]]),

            // a -> (<L=i> b A b B A)*
            // b -> C
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            (202, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),                            //  0: a -> i           | ◄0 ►i                | 1 | i
                1 => (6, symbols![nt 1, nt 2, t 0, nt 2, t 1, t 0]), //  1: i -> b A b B A i | ●i ◄1 A! B! ►b A! ►b | 6 | i b A b B A
                2 => (1, symbols![nt 1]),                            //  2: i -> ε           | ◄2                   | 1 | i
                3 => (1, symbols![t 2]),                             //  3: b -> C           | ◄3 C!                | 1 | C
            ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

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
                0 => (2, symbols![nt 2, t 4]),          //  0: a -> a_1 C         | ◄0 C! ►a_1        | 2 | a_1 C
                1 => (3, symbols![nt 1, t 1]),          //  1: j -> B "," j       | ●j ◄1 "," B!      | 3 | j B
                2 => (1, symbols![nt 1]),               //  2: j -> ε             | ◄2                | 1 | j
                3 => (4, symbols![nt 2, t 0, nt 1]),    //  3: a_1 -> A j ";" a_1 | ●a_1 ◄3 ";" ►j A! | 4 | a_1 A j
                4 => (1, symbols![nt 2]),               //  4: a_1 -> ε           | ◄4                | 1 | a_1
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (2, symbols![nt 1, t 3]),          //  0: a -> i C       | ◄0 C! ►i        | 2 | i C
                1 => (4, symbols![nt 1, t 0, nt 2]),    //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | i A j
                2 => (1, symbols![nt 1]),               //  2: i -> ε         | ◄2              | 1 | i
                3 => (3, symbols![nt 2, nt 3]),         //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 | j b
                4 => (1, symbols![nt 2]),               //  4: j -> ε         | ◄4              | 1 | j
                5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
            ], NTValue::Default, btreemap![0 => vec![0], 3 => vec![5]]),
            //
            // 2) Here, 'i' needs to be in the list of valued nonterminals, or it'll generate the same
            // code as the 3rd example:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![nt 1, t 3]),          //  0: a -> i C       | ◄0 C! ►i        | 2 | i C
                1 => (4, symbols![nt 1, t 0]),          //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | i A
                2 => (1, symbols![nt 1]),               //  2: i -> ε         | ◄2              | 1 | i
                3 => (3, symbols![]),                   //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
                4 => (1, symbols![]),                   //  4: j -> ε         | ◄4              | 1 |
                5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
            ], NTValue::SetIds(vec![0, 1]), btreemap![0 => vec![0], 3 => vec![5]]),
            //
            // 3) Only 'a' has a value, the other exit (exit_i, exit_j, exit_b) don't return any value:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![t 3]),                //  0: a -> i C       | ◄0 C! ►i        | 2 | C
                1 => (4, symbols![t 0]),                //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | A
                2 => (1, symbols![]),                   //  2: i -> ε         | ◄2              | 1 |
                3 => (3, symbols![]),                   //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
                4 => (1, symbols![]),                   //  4: j -> ε         | ◄4              | 1 |
                5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
            ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 3 => vec![5]]),
            //
            // 4) Same items, but 'a' doesn't have any value, so there's no SynA nor any value for 'a'
            // on the stack:
            (208, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![t 3]),                //  0: a -> i C       | ◄0 C! ►i        | 2 | C
                1 => (4, symbols![t 0]),                //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | A
                2 => (1, symbols![]),                   //  2: i -> ε         | ◄2              | 1 |
                3 => (3, symbols![]),                   //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
                4 => (1, symbols![]),                   //  4: j -> ε         | ◄4              | 1 |
                5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
            ], NTValue::None, btreemap![0 => vec![0], 3 => vec![5]]),

            // a -> A (<L=i> "B")* C
            (210, true, false, 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => (3, symbols![t 0, t 2]),           //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
                1 => (2, symbols![]),                   //  1: i -> "B" i | ●i ◄1 "B"   | 2 |
                2 => (1, symbols![]),                   //  2: i -> ε     | ◄2          | 1 |
            ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

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
                0 => (0, symbols![]),                    //  0: a -> A a_1   | ►a_1 A!     | 0 |
                1 => (2, symbols![nt 1, t 2]),           //  1: i -> B i     | ●i ◄1 B!    | 2 | i B
                2 => (1, symbols![nt 1]),                //  2: i -> ε       | ◄2          | 1 | i
                3 => (4, symbols![t 0, t 0, nt 1, t 1]), //  3: a_1 -> A i C | ◄3 C! ►i A! | 4 | A A i C
                4 => (4, symbols![t 0, t 1, nt 1, t 1]), //  4: a_1 -> C i C | ◄4 C! ►i C! | 4 | A C i C
            ], NTValue::Default, btreemap![0 => vec![3, 4]]),

            // a -> Id "(" Id ":" type (<L=i> "<" ">" Id ":" type)* ")"
            // type -> Id
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form | sep_list (32897)
            // parents:
            //  - i -> a
            (212, true, false, 0, btreemap![
            ], btreemap![
                0 => (4, symbols![t 0, nt 1]),          //  0: a -> Id "(" Id ":" type i ")" | ◄0 ")" ►i ►type ":" Id! "(" Id! | 4    | Id i
                1 => (6, symbols![nt 1, t 0, nt 2]),    //  1: i -> "<" ">" Id ":" type i    | ●i ◄1 ►type ":" Id! ">" "<"     | 6, 3 | i Id type
                2 => (1, symbols![nt 1]),               //  2: i -> ε                        | ◄2                              | 1    | i
                3 => (1, symbols![t 0]),                //  3: type -> Id                    | ◄3 Id!                          | 1    | Id
            ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

            // --------------------------------------------------------------------------- norm+/* <L> alternatives
            // a -> (<L=i> A | B)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            (250, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: a -> i   | ◄0 ►i    | 1 | i
                1 => (2, symbols![nt 1, t 0]),          //  1: i -> A i | ●i ◄1 A! | 2 | i A
                2 => (2, symbols![nt 1, t 1]),          //  2: i -> B i | ●i ◄2 B! | 2 | i B
                3 => (1, symbols![nt 1]),               //  3: i -> ε   | ◄3       | 1 | i
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> (<L=i> A | B)+
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - i_1: child_left_fact (64)
            //  - i_2: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - i_1 -> i
            //  - i_2 -> i
            (251, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: a -> i     | ◄0 ►i   | 1 | i
                1 => (0, symbols![]),                   //  1: i -> A i_1 | ►i_1 A! | 0 |
                2 => (0, symbols![]),                   //  2: i -> B i_2 | ►i_2 B! | 0 |
                3 => (2, symbols![nt 1, t 0]),          //  3: i_1 -> i   | ●i ◄3   | 2 | i A
                4 => (2, symbols![nt 1, t 0]),          //  4: i_1 -> ε   | ◄4      | 2 | i A
                5 => (2, symbols![nt 1, t 1]),          //  5: i_2 -> i   | ●i ◄5   | 2 | i B
                6 => (2, symbols![nt 1, t 1]),          //  6: i_2 -> ε   | ◄6      | 2 | i B
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> A ((<L=j> b C b B C | D)+ E | F)+ G
            // b -> H
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - j: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - a_1: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - j_1: child_left_fact (64)
            //  - j_2: child_left_fact (64)
            //  - a_2: child_left_fact (64)
            //  - a_3: child_left_fact (64)
            // parents:
            //  - j -> a_1
            //  - a_1 -> a
            //  - j_1 -> j
            //  - j_2 -> j
            //  - a_2 -> a_1
            //  - a_3 -> a_1
            (252, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0, nt 3, t 6]),                  //  0: a -> A a_1 G       | ◄0 G! ►a_1 A!       | 3 | A a_1 G
                1 => (0, symbols![]),                                //  1: j -> D j_1         | ►j_1 D!             | 0 |
                2 => (0, symbols![]),                                //  2: j -> b C b B C j_2 | ►j_2 C! B! ►b C! ►b | 0 |
                3 => (1, symbols![t 7]),                             //  3: b -> H             | ◄3 H!               | 1 | H
                4 => (0, symbols![]),                                //  4: a_1 -> F a_2       | ►a_2 F!             | 0 |
                5 => (0, symbols![]),                                //  5: a_1 -> j E a_3     | ►a_3 E! ►j          | 0 |
                6 => (2, symbols![nt 1, t 3]),                       //  6: j_1 -> j           | ●j ◄6               | 2 | j D
                7 => (2, symbols![nt 1, t 3]),                       //  7: j_1 -> ε           | ◄7                  | 2 | j D
                8 => (6, symbols![nt 1, nt 2, t 1, nt 2, t 2, t 1]), //  8: j_2 -> j           | ●j ◄8               | 6 | j b C b B C
                9 => (6, symbols![nt 1, nt 2, t 1, nt 2, t 2, t 1]), //  9: j_2 -> ε           | ◄9                  | 6 | j b C b B C
                10 => (2, symbols![nt 3, t 5]),                      // 10: a_2 -> a_1         | ●a_1 ◄10            | 2 | a_1 F
                11 => (2, symbols![nt 3, t 5]),                      // 11: a_2 -> ε           | ◄11                 | 2 | a_1 F
                12 => (3, symbols![nt 3, nt 1, t 4]),                // 12: a_3 -> a_1         | ●a_1 ◄12            | 3 | a_1 j E
                13 => (3, symbols![nt 3, nt 1, t 4]),                // 13: a_3 -> ε           | ◄13                 | 3 | a_1 j E
            ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

            // a -> A (<L=i> (b C b B C | D)+ E | F)+ G
            // b -> H
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | parent_+_or_* | plus (6305)
            //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - i_1: child_left_fact (64)
            //  - i_2: child_left_fact (64)
            //  - a_2: child_left_fact (64)
            //  - a_3: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - a_1 -> i
            //  - i_1 -> i
            //  - i_2 -> i
            //  - a_2 -> a_1
            //  - a_3 -> a_1
            (253, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0, nt 1, t 6]),                   //  0: a -> A i G           | ◄0 G! ►i A!         | 3 | A i G
                1 => (0, symbols![]),                                 //  1: i -> F i_1           | ►i_1 F!             | 0 |
                2 => (0, symbols![]),                                 //  2: i -> a_1 E i_2       | ►i_2 E! ►a_1        | 0 |
                3 => (1, symbols![t 7]),                              //  3: b -> H               | ◄3 H!               | 1 | H
                4 => (0, symbols![]),                                 //  4: a_1 -> D a_2         | ►a_2 D!             | 0 |
                5 => (0, symbols![]),                                 //  5: a_1 -> b C b B C a_3 | ►a_3 C! B! ►b C! ►b | 0 |
                6 => (2, symbols![nt 1, t 5]),                        //  6: i_1 -> i             | ●i ◄6               | 2 | i F
                7 => (2, symbols![nt 1, t 5]),                        //  7: i_1 -> ε             | ◄7                  | 2 | i F
                8 => (3, symbols![nt 1, nt 3, t 4]),                  //  8: i_2 -> i             | ●i ◄8               | 3 | i a_1 E
                9 => (3, symbols![nt 1, nt 3, t 4]),                  //  9: i_2 -> ε             | ◄9                  | 3 | i a_1 E
                10 => (2, symbols![nt 3, t 3]),                       // 10: a_2 -> a_1           | ●a_1 ◄10            | 2 | a_1 D
                11 => (2, symbols![nt 3, t 3]),                       // 11: a_2 -> ε             | ◄11                 | 2 | a_1 D
                12 => (6, symbols![nt 3, nt 2, t 1, nt 2, t 2, t 1]), // 12: a_3 -> a_1           | ●a_1 ◄12            | 6 | a_1 b C b B C
                13 => (6, symbols![nt 3, nt 2, t 1, nt 2, t 2, t 1]), // 13: a_3 -> ε             | ◄13                 | 6 | a_1 b C b B C
            ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

            // a -> A (<L=i> (<L=j> b C b B C | D)* E | F)* G
            // b -> H
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | L-form | parent_+_or_* (2177)
            //  - j: child_+_or_* | L-form (129)
            // parents:
            //  - i -> a
            //  - j -> i
            (254, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0, nt 1, t 6]),                  //  0: a -> A i G       | ◄0 G! ►i A!          | 3 | A i G
                1 => (3, symbols![nt 1, nt 2, t 4]),                 //  1: i -> j E i       | ●i ◄1 E! ►j          | 3 | i j E
                2 => (2, symbols![nt 1, t 5]),                       //  2: i -> F i         | ●i ◄2 F!             | 2 | i F
                3 => (1, symbols![nt 1]),                            //  3: i -> ε           | ◄3                   | 1 | i
                4 => (6, symbols![nt 2, nt 3, t 1, nt 3, t 2, t 1]), //  4: j -> b C b B C j | ●j ◄4 C! B! ►b C! ►b | 6 | j b C b B C
                5 => (2, symbols![nt 2, t 3]),                       //  5: j -> D j         | ●j ◄5 D!             | 2 | j D
                6 => (1, symbols![nt 2]),                            //  6: j -> ε           | ◄6                   | 1 | j
                7 => (1, symbols![t 7]),                             //  7: b -> H           | ◄7 H!                | 1 | H
            ], NTValue::Default, btreemap![0 => vec![0], 3 => vec![7]]),

            // a -> A (<L=i> B A | B A C b | D)+ E
            // b -> F
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - i_1: parent_left_fact | child_left_fact (96)
            //  - i_2: child_left_fact (64)
            //  - a_1: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - i_1 -> i
            //  - i_2 -> i
            //  - a_1 -> i_1
            (256, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0, nt 1, t 4]),             //  0: a -> A i E     | ◄0 E! ►i A! | 3 | A i E
                1 => (0, symbols![]),                           //  1: i -> B A i_1   | ►i_1 A! B!  | 0 |
                2 => (0, symbols![]),                           //  2: i -> D i_2     | ►i_2 D!     | 0 |
                3 => (1, symbols![t 5]),                        //  3: b -> F         | ◄3 F!       | 1 | F
                4 => (0, symbols![]),                           //  4: i_1 -> C b a_1 | ►a_1 ►b C!  | 0 |
                5 => (3, symbols![nt 1, t 1, t 0]),             //  5: i_1 -> i       | ●i ◄5       | 3 | i B A
                6 => (3, symbols![nt 1, t 1, t 0]),             //  6: i_1 -> ε       | ◄6          | 3 | i B A
                7 => (2, symbols![nt 1, t 3]),                  //  7: i_2 -> i       | ●i ◄7       | 2 | i D
                8 => (2, symbols![nt 1, t 3]),                  //  8: i_2 -> ε       | ◄8          | 2 | i D
                9 => (5, symbols![nt 1, t 1, t 0, t 2, nt 2]),  //  9: a_1 -> i       | ●i ◄9       | 5 | i B A C b
                10 => (5, symbols![nt 1, t 1, t 0, t 2, nt 2]), // 10: a_1 -> ε       | ◄10         | 5 | i B A C b
            ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

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
                0 => (1, symbols![nt 1]),                //  0: a -> i         | ◄0 ►i      | 1 | i
                1 => (0, symbols![]),                    //  1: i -> A a_1     | ►a_1 A!    | 0 |
                2 => (0, symbols![]),                    //  2: i -> C a_2     | ►a_2 C!    | 0 |
                3 => (0, symbols![]),                    //  3: a_1 -> B A a_3 | ►a_3 A! B! | 0 |
                4 => (2, symbols![nt 1, t 0]),           //  4: a_1 -> i       | ●i ◄4      | 2 | i A
                5 => (2, symbols![nt 1, t 0]),           //  5: a_1 -> ε       | ◄5         | 2 | i A
                6 => (2, symbols![nt 1, t 2]),           //  6: a_2 -> i       | ●i ◄6      | 2 | i C
                7 => (2, symbols![nt 1, t 2]),           //  7: a_2 -> ε       | ◄7         | 2 | i C
                8 => (4, symbols![nt 1, t 0, t 1, t 0]), //  8: a_3 -> i       | ●i ◄8      | 4 | i A B A
                9 => (4, symbols![nt 1, t 0, t 1, t 0]), //  9: a_3 -> ε       | ◄9         | 4 | i A B A
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> (<L=i> A | A B | C | D (<L=j> E | E F | G)*)*
            // NT flags:
            //  - a: parent_+_or_* (2048)
            //  - i: child_+_or_* | parent_left_fact | L-form | parent_+_or_* (2209)
            //  - j: child_+_or_* | parent_left_fact | L-form (161)
            //  - i_1: child_left_fact (64)
            //  - j_1: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - j -> i
            //  - i_1 -> i
            //  - j_1 -> j
            (258, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: a -> i     | ◄0 ►i       | 1 | i
                1 => (0, symbols![]),                   //  1: i -> A i_1 | ►i_1 A!     | 0 |
                2 => (2, symbols![nt 1, t 2]),          //  2: i -> C i   | ●i ◄2 C!    | 2 | i C
                3 => (3, symbols![nt 1, t 3, nt 2]),    //  3: i -> D j i | ●i ◄3 ►j D! | 3 | i D j
                4 => (1, symbols![nt 1]),               //  4: i -> ε     | ◄4          | 1 | i
                5 => (0, symbols![]),                   //  5: j -> E j_1 | ►j_1 E!     | 0 |
                6 => (2, symbols![nt 2, t 6]),          //  6: j -> G j   | ●j ◄6 G!    | 2 | j G
                7 => (1, symbols![nt 2]),               //  7: j -> ε     | ◄7          | 1 | j
                8 => (3, symbols![nt 1, t 0, t 1]),     //  8: i_1 -> B i | ●i ◄8 B!    | 3 | i A B
                9 => (2, symbols![nt 1, t 0]),          //  9: i_1 -> i   | ●i ◄9       | 2 | i A
                10 => (3, symbols![nt 2, t 4, t 5]),    // 10: j_1 -> F j | ●j ◄10 F!   | 3 | j E F
                11 => (2, symbols![nt 2, t 4]),         // 11: j_1 -> j   | ●j ◄11      | 2 | j E
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> (<L=i> A | A B | C | D (<L=j> E | E F | G)+)+
            // NT flags:
            //  - a: parent_+_or_* | plus (6144)
            //  - i: child_+_or_* | parent_left_fact | L-form | parent_+_or_* | plus (6305)
            //  - j: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - i_1: parent_left_fact | child_left_fact (96)
            //  - i_2: child_left_fact (64)
            //  - i_3: child_left_fact (64)
            //  - j_1: parent_left_fact | child_left_fact (96)
            //  - j_2: child_left_fact (64)
            //  - a_1: child_left_fact (64)
            //  - a_2: child_left_fact (64)
            // parents:
            //  - i -> a
            //  - j -> i
            //  - i_1 -> i
            //  - i_2 -> i
            //  - i_3 -> i
            //  - j_1 -> j
            //  - j_2 -> j
            //  - a_1 -> i_1
            //  - a_2 -> j_1
            (259, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: a -> i       | ◄0 ►i      | 1 | i
                1 => (0, symbols![]),                   //  1: i -> A i_1   | ►i_1 A!    | 0 |
                2 => (0, symbols![]),                   //  2: i -> C i_2   | ►i_2 C!    | 0 |
                3 => (0, symbols![]),                   //  3: i -> D j i_3 | ►i_3 ►j D! | 0 |
                4 => (0, symbols![]),                   //  4: j -> E j_1   | ►j_1 E!    | 0 |
                5 => (0, symbols![]),                   //  5: j -> G j_2   | ►j_2 G!    | 0 |
                6 => (0, symbols![]),                   //  6: i_1 -> B a_1 | ►a_1 B!    | 0 |
                7 => (2, symbols![nt 1, t 0]),          //  7: i_1 -> i     | ●i ◄7      | 2 | i A
                8 => (2, symbols![nt 1, t 0]),          //  8: i_1 -> ε     | ◄8         | 2 | i A
                9 => (2, symbols![nt 1, t 2]),          //  9: i_2 -> i     | ●i ◄9      | 2 | i C
                10 => (2, symbols![nt 1, t 2]),         // 10: i_2 -> ε     | ◄10        | 2 | i C
                11 => (3, symbols![nt 1, t 3, nt 2]),   // 11: i_3 -> i     | ●i ◄11     | 3 | i D j
                12 => (3, symbols![nt 1, t 3, nt 2]),   // 12: i_3 -> ε     | ◄12        | 3 | i D j
                13 => (0, symbols![]),                  // 13: j_1 -> F a_2 | ►a_2 F!    | 0 |
                14 => (2, symbols![nt 2, t 4]),         // 14: j_1 -> j     | ●j ◄14     | 2 | j E
                15 => (2, symbols![nt 2, t 4]),         // 15: j_1 -> ε     | ◄15        | 2 | j E
                16 => (2, symbols![nt 2, t 6]),         // 16: j_2 -> j     | ●j ◄16     | 2 | j G
                17 => (2, symbols![nt 2, t 6]),         // 17: j_2 -> ε     | ◄17        | 2 | j G
                18 => (3, symbols![nt 1, t 0, t 1]),    // 18: a_1 -> i     | ●i ◄18     | 3 | i A B
                19 => (3, symbols![nt 1, t 0, t 1]),    // 19: a_1 -> ε     | ◄19        | 3 | i A B
                20 => (3, symbols![nt 2, t 4, t 5]),    // 20: a_2 -> j     | ●j ◄20     | 3 | j E F
                21 => (3, symbols![nt 2, t 4, t 5]),    // 21: a_2 -> ε     | ◄21        | 3 | j E F
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- right_rec
            // expr -> Id "." expr | "(" Num ")"
            // NT flags:
            //  - expr: right_rec (2)
            (301, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0, nt 0]),          //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id expr
                1 => (3, symbols![t 3]),                //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
            ], NTValue::Default, btreemap![0 => vec![0, 1]]),
            (301, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0]),                //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id
                1 => (3, symbols![t 3]),                //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
            ], NTValue::None, btreemap![0 => vec![0, 1]]),

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
                0 => (3, symbols![nt 0, t 0]),          //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | expr Id
                1 => (4, symbols![nt 0, t 3]),          //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | expr Num
            ], NTValue::Default, btreemap![0 => vec![0, 1]]),
            (401, true, false, 0, btreemap![
            ], btreemap![
                0 => (3, symbols![t 0]),                //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | Id
                1 => (4, symbols![t 3]),                //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | Num
            ], NTValue::None, btreemap![0 => vec![0, 1]]),

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
                0 => (1, symbols![nt 1]),               //  0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
                1 => (1, symbols![t 1]),                //  1: f -> Id           | ◄1 Id!          | 1 | Id
                2 => (3, symbols![nt 0, t 1]),          //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | e Id
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε          | ◄3              | 1 | e
            ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),
            (502, true, false, 0, btreemap![
                1 => "SynF".to_string(),
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
                1 => (1, symbols![t 1]),                //  1: f -> Id           | ◄1 Id!          | 1 | Id
                2 => (3, symbols![t 1]),                //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | Id
                3 => (1, symbols![]),                   //  3: e_1 -> ε          | ◄3              | 1 |
            ], NTValue::SetIds(vec![1]), btreemap![0 => vec![0], 1 => vec![1]]),


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
                0 => (2, symbols![nt 0]),               //  0: e -> "-" e     | ◄0 ►e "-"    | 2 | e
                1 => (1, symbols![t 2]),                //  1: e -> Num e_1   | ►e_1 ◄1 Num! | 1 | Num
                2 => (2, symbols![nt 0]),               //  2: e_1 -> "!" e_1 | ●e_1 ◄2 "!"  | 2 | e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε       | ◄3           | 1 | e
            ], NTValue::Default, btreemap![0 => vec![0, 1]]),

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
                0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "+" e_2 e_1 | ●e_1 ◄1 ►e_2 "+" | 3 | e e
                2 => (1, symbols![nt 0]),               //  2: e_1 -> ε           | ◄2               | 1 | e
                3 => (1, symbols![t 1]),                //  3: e_2 -> Num         | ◄3 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // e -> e "*" e | e "+" e | "!" e | Num
            (603, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
                4 => (1, symbols![nt 0]),               //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
                5 => (3, symbols![nt 0, nt 0]),         //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
                6 => (1, symbols![nt 0]),               //  6: e_3 -> ε           | ◄6               | 1 | e
                7 => (2, symbols![nt 0]),               //  7: e_4 -> "!" e       | ◄7 ►e "!"        | 2 | e
                8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | "!" e | e "+" e | Num
            (604, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
                4 => (1, symbols![nt 0]),               //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
                5 => (3, symbols![nt 0, nt 0]),         //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
                6 => (1, symbols![nt 0]),               //  6: e_3 -> ε           | ◄6               | 1 | e
                7 => (2, symbols![nt 0]),               //  7: e_4 -> "!" e_2     | ◄7 ►e_2 "!"      | 2 | e
                8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> "!" e | e "*" e | e "+" e | Num
            (605, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
                4 => (1, symbols![nt 0]),               //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
                5 => (3, symbols![nt 0, nt 0]),         //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
                6 => (1, symbols![nt 0]),               //  6: e_3 -> ε           | ◄6               | 1 | e
                7 => (2, symbols![nt 0]),               //  7: e_4 -> "!" e_4     | ◄7 ►e_4 "!"      | 2 | e
                8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" e | <R> e "!" e | Num
            (606, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "!" e e_1   | ●e_1 ◄3 ►e "!"   | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
                7 => (1, symbols![nt 0]),               //  7: e_3 -> ε           | ◄7               | 1 | e
                8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | <R> e "!" e | e "+" e | Num
            (607, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "!" e_2 e_1 | ●e_1 ◄2 ►e_2 "!" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "!" e_2 e_3 | ●e_3 ◄7 ►e_2 "!" | 3 | e e
                8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
                9 => (1, symbols![t 3]),                //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> <R> e "!" e | e "*" e | e "+" e | Num
            (608, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "!" e_4 e_1 | ●e_1 ◄1 ►e_4 "!"  | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4                | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "!" e_4 e_3 | ●e_3 ◄6 ►e_4 "!"  | 3 | e e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | 3 | e e
                8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8                | 1 | e
                9 => (1, symbols![nt 0]),               //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | 1 | e
                10 => (3, symbols![nt 0, nt 0]),        // 10: e_5 -> "!" e_4 e_5 | ●e_5 ◄10 ►e_4 "!" | 3 | e e
                11 => (1, symbols![nt 0]),              // 11: e_5 -> ε           | ◄11               | 1 | e
                12 => (1, symbols![t 3]),               // 12: e_6 -> Num         | ◄12 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" e | e "!" | Num
            (609, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
                3 => (2, symbols![nt 0]),               //  3: e_1 -> "!" e_1     | ●e_1 ◄3 "!"      | 2 | e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
                7 => (1, symbols![nt 0]),               //  7: e_3 -> ε           | ◄7               | 1 | e
                8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "!" | e "+" e | Num
            (610, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (2, symbols![nt 0]),               //  2: e_1 -> "!" e_1     | ●e_1 ◄2 "!"      | 2 | e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
                7 => (2, symbols![nt 0]),               //  7: e_3 -> "!" e_3     | ●e_3 ◄7 "!"      | 2 | e
                8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
                9 => (1, symbols![t 3]),                //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "!" | e "*" e | e "+" e | Num
            (611, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6     | 1 | e
                1 => (2, symbols![nt 0]),               //  1: e_1 -> "!" e_1     | ●e_1 ◄1 "!"      | 2 | e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6     | 1 | e
                6 => (2, symbols![nt 0]),               //  6: e_3 -> "!" e_3     | ●e_3 ◄6 "!"      | 2 | e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | 3 | e e
                8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
                9 => (1, symbols![nt 0]),               //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6     | 1 | e
                10 => (2, symbols![nt 0]),              // 10: e_5 -> "!" e_5     | ●e_5 ◄10 "!"     | 2 | e
                11 => (1, symbols![nt 0]),              // 11: e_5 -> ε           | ◄11              | 1 | e
                12 => (1, symbols![t 3]),               // 12: e_6 -> Num         | ◄12 Num!         | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "!" e | e "*" e | e "+" e | Num
            (612, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "!" e_6 e_1 | ●e_1 ◄1 ►e_6 "!"  | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4                | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "!" e_6 e_3 | ●e_3 ◄6 ►e_6 "!"  | 3 | e e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | 3 | e e
                8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8                | 1 | e
                9 => (1, symbols![nt 0]),               //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | 1 | e
                10 => (3, symbols![nt 0, nt 0]),        // 10: e_5 -> "!" e_6 e_5 | ●e_5 ◄10 ►e_6 "!" | 3 | e e
                11 => (1, symbols![nt 0]),              // 11: e_5 -> ε           | ◄11               | 1 | e
                12 => (1, symbols![t 3]),               // 12: e_6 -> Num         | ◄12 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" e | <P> e "!" e | Num
            (613, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "!" e_2 e_1 | ●e_1 ◄3 ►e_2 "!" | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
                7 => (1, symbols![nt 0]),               //  7: e_3 -> ε           | ◄7               | 1 | e
                8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | <P> e "!" e | e "+" e | Num
            (614, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "!" e_4 e_1 | ●e_1 ◄2 ►e_4 "!" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
                4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
                5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
                6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "!" e_4 e_3 | ●e_3 ◄7 ►e_4 "!" | 3 | e e
                8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
                9 => (1, symbols![t 3]),                //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" | "!" e | Num
            (630, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
                2 => (2, symbols![nt 0]),               //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
                4 => (2, symbols![nt 0]),               //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
                5 => (1, symbols![t 3]),                //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | e "+" | <R> "!" e | Num
            (631, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
                2 => (2, symbols![nt 0]),               //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
                4 => (2, symbols![nt 0]),               //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
                5 => (1, symbols![t 3]),                //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),
            // e -> e "*" e | <R> e "+" | "!" e | Num
            (632, true, true, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
                2 => (2, symbols![nt 0]),               //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
                3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
                4 => (2, symbols![nt 0]),               //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
                5 => (1, symbols![t 3]),                //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "/" e_4 e_1 | ●e_1 ◄2 ►e_4 "/" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
                4 => (3, symbols![nt 0, nt 0]),         //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | 3 | e e
                5 => (1, symbols![nt 0]),               //  5: e_1 -> ε           | ◄5               | 1 | e
                6 => (1, symbols![nt 0]),               //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | 3 | e e
                8 => (3, symbols![nt 0, nt 0]),         //  8: e_3 -> "/" e_4 e_3 | ●e_3 ◄8 ►e_4 "/" | 3 | e e
                9 => (1, symbols![nt 0]),               //  9: e_3 -> ε           | ◄9               | 1 | e
                10 => (2, symbols![nt 0]),              // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
                11 => (1, symbols![t 4]),               // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e e_1   | ●e_1 ◄3 ►e "+"   | 3 | e e
                4 => (3, symbols![nt 0, nt 0]),         //  4: e_1 -> "-" e e_1   | ●e_1 ◄4 ►e "-"   | 3 | e e
                5 => (1, symbols![nt 0]),               //  5: e_1 -> ε           | ◄5               | 1 | e
                6 => (1, symbols![nt 0]),               //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | 3 | e e
                8 => (3, symbols![nt 0, nt 0]),         //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | 3 | e e
                9 => (1, symbols![nt 0]),               //  9: e_3 -> ε           | ◄9               | 1 | e
                10 => (2, symbols![nt 0]),              // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
                11 => (1, symbols![t 4]),               // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
                1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
                2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | 3 | e e
                3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
                4 => (3, symbols![nt 0, nt 0]),         //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | 3 | e e
                5 => (1, symbols![nt 0]),               //  5: e_1 -> ε           | ◄5               | 1 | e
                6 => (1, symbols![nt 0]),               //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
                7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | 3 | e e
                8 => (3, symbols![nt 0, nt 0]),         //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | 3 | e e
                9 => (1, symbols![nt 0]),               //  9: e_3 -> ε           | ◄9               | 1 | e
                10 => (2, symbols![nt 0]),              // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
                11 => (1, symbols![t 4]),               // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // a -> a A a a | B
            // NT flags:
            //  - a: parent_left_rec | parent_amb (1536)
            //  - a_1: child_left_rec (4)
            // parents:
            //  - a_1 -> a
            //  - a_2 -> a
            (650, true, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 0]),                  //  0: a -> a_2 a_1       | ►a_1 ◄0 ►a_2       | 1 | a
                1 => (4, symbols![nt 0, t 0, nt 0, nt 0]), //  1: a_1 -> A a a_2 a_1 | ●a_1 ◄1 ►a_2 ►a A! | 4 | a A a a
                2 => (1, symbols![nt 0]),                  //  2: a_1 -> ε           | ◄2                 | 1 | a
                3 => (1, symbols![t 1]),                   //  3: a_2 -> B           | ◄3 B!              | 1 | B
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (0, symbols![]),                   //  0: a -> A a_1   | ►a_1 A! | 0 |
                1 => (1, symbols![t 4]),                //  1: a -> E       | ◄1 E!   | 1 | E
                2 => (0, symbols![]),                   //  2: a_1 -> B a_2 | ►a_2 B! | 0 |
                3 => (1, symbols![t 0]),                //  3: a_1 -> ε     | ◄3      | 1 | A
                4 => (3, symbols![t 0, t 1, t 2]),      //  4: a_2 -> C     | ◄4 C!   | 3 | A B C
                5 => (3, symbols![t 0, t 1, t 3]),      //  5: a_2 -> D     | ◄5 D!   | 3 | A B D
                6 => (2, symbols![t 0, t 1]),           //  6: a_2 -> ε     | ◄6      | 2 | A B
            ], NTValue::Default, btreemap![0 => vec![1, 3, 4, 5, 6]]),

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
                0 => (3, symbols![nt 1, t 1, nt 0]),    //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | 3 | a_1 B a
                1 => (1, symbols![t 2]),                //  1: a -> C       | ◄1 C!         | 1 | C
                2 => (2, symbols![nt 1, t 0]),          //  2: a_1 -> A a_1 | ●a_1 ◄2 A!    | 2 | a_1 A
                3 => (1, symbols![nt 1]),               //  3: a_1 -> ε     | ◄3            | 1 | a_1
            ], NTValue::Default, btreemap![0 => vec![0, 1]]),

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
                0 => (3, symbols![nt 1, t 1, nt 0]),    //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | 3 | a_1 B a
                1 => (1, symbols![t 2]),                //  1: a -> C       | ◄1 C!         | 1 | C
                2 => (0, symbols![]),                   //  2: a_1 -> A a_2 | ►a_2 A!       | 0 |
                3 => (2, symbols![nt 1, t 0]),          //  3: a_2 -> a_1   | ●a_1 ◄3       | 2 | a_1 A
                4 => (2, symbols![nt 1, t 0]),          //  4: a_2 -> ε     | ◄4            | 2 | a_1 A
            ], NTValue::Default, btreemap![0 => vec![0, 1]]),

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
                0 => (1, symbols![t 2]),                //  0: a -> B a_2       | ►a_2 ◄0 B!      | 1 | B
                1 => (2, symbols![nt 1, t 0]),          //  1: a_1 -> A a_1     | ●a_1 ◄1 A!      | 2 | a_1 A
                2 => (1, symbols![nt 1]),               //  2: a_1 -> ε         | ◄2              | 1 | a_1
                3 => (3, symbols![nt 0, nt 1, t 1]),    //  3: a_2 -> a_1 C a_2 | ●a_2 ◄3 C! ►a_1 | 3 | a a_1 C
                4 => (1, symbols![nt 0]),               //  4: a_2 -> ε         | ◄4              | 1 | a
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![t 2]),                //  0: a -> B a_2       | ►a_2 ◄0 B!      | 1 | B
                1 => (0, symbols![]),                   //  1: a_1 -> A a_3     | ►a_3 A!         | 0 |
                2 => (3, symbols![nt 0, nt 1, t 1]),    //  2: a_2 -> a_1 C a_2 | ●a_2 ◄2 C! ►a_1 | 3 | a a_1 C
                3 => (1, symbols![nt 0]),               //  3: a_2 -> ε         | ◄3              | 1 | a
                4 => (2, symbols![nt 1, t 0]),          //  4: a_3 -> a_1       | ●a_1 ◄4         | 2 | a_1 A
                5 => (2, symbols![nt 1, t 0]),          //  5: a_3 -> ε         | ◄5              | 2 | a_1 A
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 0]),               //  0: a -> a_3 a_2               | ►a_2 ◄0 ►a_3             | 1 | a
                1 => (0, symbols![]),                   //  1: a_1 -> Num a_4             | ►a_4 Num!                | 0 |
                2 => (3, symbols![nt 0, nt 0]),         //  2: a_2 -> "x" a_3 a_2         | ●a_2 ◄2 ►a_3 "x"         | 3 | a a
                3 => (5, symbols![nt 0, nt 1]),         //  3: a_2 -> "*" "[" a_1 "]" a_2 | ●a_2 ◄3 "]" ►a_1 "[" "*" | 5 | a a_1
                4 => (1, symbols![nt 0]),               //  4: a_2 -> ε                   | ◄4                       | 1 | a
                5 => (2, symbols![nt 0]),               //  5: a_3 -> "-" a               | ◄5 ►a "-"                | 2 | a
                6 => (1, symbols![t 6]),                //  6: a_3 -> Id                  | ◄6 Id!                   | 1 | Id
                7 => (2, symbols![nt 1, t 3]),          //  7: a_4 -> a_1                 | ●a_1 ◄7                  | 2 | a_1 Num
                8 => (2, symbols![nt 1, t 3]),          //  8: a_4 -> ε                   | ◄8                       | 2 | a_1 Num
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
                0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
                1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
                2 => (1, symbols![nt 1]),               //  2: a_1 -> ε     | ◄2         | 1 | a_1
                3 => (3, symbols![nt 1, t 0, t 1]),     //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | 3 | a_1 A B
                4 => (3, symbols![nt 1, t 0, t 2]),     //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | 3 | a_1 A C
            ], NTValue::Default, btreemap![0 => vec![0]]),

            // --------------------------------------------------------------------------- right_rec + left_fact
            // expr -> <L> Num "^" expr | Num
            // NT flags:
            //  - expr: right_rec | parent_left_fact | L-form (162)
            //  - expr_1: child_left_fact (64)
            // parents:
            //  - expr_1 -> expr
            (862, true, false, 0, btreemap![
            ], btreemap![
                0 => (0, symbols![]),                   //  0: expr -> Num expr_1 | ►expr_1 Num! | 0 |
                1 => (3, symbols![nt 0, t 0]),          //  1: expr_1 -> "^" expr | ●expr ◄1 "^" | 3 | expr Num
                2 => (2, symbols![nt 0, t 0]),          //  2: expr_1 -> ε        | ◄2           | 2 | expr Num
            ], NTValue::Default, btreemap![0 => vec![1, 2]]),

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
                0 => (0, symbols![]),                   //  0: a -> B a_2   | ►a_2 B!    | 0 |
                1 => (2, symbols![nt 0, t 0]),          //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | 2 | a A
                2 => (1, symbols![nt 0]),               //  2: a_1 -> ε     | ◄2         | 1 | a
                3 => (2, symbols![t 1, t 2]),           //  3: a_2 -> C a_1 | ►a_1 ◄3 C! | 2 | B C
                4 => (2, symbols![t 1, t 3]),           //  4: a_2 -> D a_1 | ►a_1 ◄4 D! | 2 | B D
            ], NTValue::Default, btreemap![0 => vec![3, 4]]),

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
                0 => (1, symbols![t 3]),                //  0: a -> D a_1   | ►a_1 ◄0 D! | 1 | D
                1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
                2 => (1, symbols![nt 0]),               //  2: a_1 -> ε     | ◄2         | 1 | a
                3 => (3, symbols![nt 0, t 0, t 1]),     //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | 3 | a A B
                4 => (3, symbols![nt 0, t 0, t 2]),     //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | 3 | a A C
            ], NTValue::Default, btreemap![0 => vec![0]]),

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
            //  - option_1: child_+_or_* | sep_list (32769)
            //  - actions_1: child_+_or_* | sep_list (32769)
            //  - alt_items_1: child_+_or_* | sep_list (32769)
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
                0 => (2, symbols![nt 2, nt 15]),        //  0: file -> header file_1                    | ◄0 ►file_1 ►header                  | 2    | header file_1
                1 => (1, symbols![nt 15]),              //  1: file -> file_1                           | ◄1 ►file_1                          | 1    | file_1
                2 => (1, symbols![nt 4]),               //  2: file_item -> option                      | ◄2 ►option                          | 1    | option
                3 => (1, symbols![nt 3]),               //  3: file_item -> declaration                 | ◄3 ►declaration                     | 1    | declaration
                4 => (1, symbols![nt 5]),               //  4: file_item -> rule                        | ◄4 ►rule                            | 1    | rule
                5 => (3, symbols![t 27]),               //  5: header -> "lexicon" Id ";"               | ◄5 ";" Id! "lexicon"                | 3    | Id
                6 => (3, symbols![t 27]),               //  6: declaration -> "mode" Id ";"             | ◄6 ";" Id! "mode"                   | 3    | Id
                7 => (4, symbols![nt 16]),              //  7: option -> "channels" "{" Id option_1 "}" | ◄7 "}" ►option_1 Id! "{" "channels" | 4    | option_1
                8 => (5, symbols![t 27, nt 8]),         //  8: rule -> "fragment" Id ":" match ";"      | ◄8 ";" ►match ":" Id! "fragment"    | 5    | Id match
                9 => (0, symbols![]),                   //  9: rule -> Id ":" match rule_1              | ►rule_1 ►match ":" Id!              | 0    |
                10 => (1, symbols![nt 17]),             // 10: actions -> action actions_1              | ◄10 ►actions_1 ►action              | 1    | actions_1
                11 => (4, symbols![t 27]),              // 11: action -> "mode" "(" Id ")"              | ◄11 ")" Id! "(" "mode"              | 4    | Id
                12 => (4, symbols![t 27]),              // 12: action -> "push" "(" Id ")"              | ◄12 ")" Id! "(" "push"              | 4    | Id
                13 => (1, symbols![]),                  // 13: action -> "pop"                          | ◄13 "pop"                           | 1    |
                14 => (1, symbols![]),                  // 14: action -> "skip"                         | ◄14 "skip"                          | 1    |
                15 => (1, symbols![]),                  // 15: action -> "more"                         | ◄15 "more"                          | 1    |
                16 => (4, symbols![t 27]),              // 16: action -> "type" "(" Id ")"              | ◄16 ")" Id! "(" "type"              | 4    | Id
                17 => (4, symbols![t 27]),              // 17: action -> "channel" "(" Id ")"           | ◄17 ")" Id! "(" "channel"           | 4    | Id
                18 => (1, symbols![nt 9]),              // 18: match -> alt_items                       | ◄18 ►alt_items                      | 1    | alt_items
                19 => (1, symbols![nt 18]),             // 19: alt_items -> alt_item alt_items_1        | ◄19 ►alt_items_1 ►alt_item          | 1    | alt_items_1
                20 => (1, symbols![nt 19]),             // 20: alt_item -> alt_item_1                   | ◄20 ►alt_item_1                     | 1    | alt_item_1
                21 => (0, symbols![]),                  // 21: repeat_item -> item repeat_item_1        | ►repeat_item_1 ►item                | 0    |
                22 => (3, symbols![nt 9]),              // 22: item -> "(" alt_items ")"                | ◄22 ")" ►alt_items "("              | 3    | alt_items
                23 => (2, symbols![nt 12]),             // 23: item -> "~" item                         | ◄23 ►item "~"                       | 2    | item
                24 => (1, symbols![t 27]),              // 24: item -> Id                               | ◄24 Id!                             | 1    | Id
                25 => (0, symbols![]),                  // 25: item -> CharLit item_1                   | ►item_1 CharLit!                    | 0    |
                26 => (1, symbols![t 29]),              // 26: item -> StrLit                           | ◄26 StrLit!                         | 1    | StrLit
                27 => (1, symbols![nt 13]),             // 27: item -> char_set                         | ◄27 ►char_set                       | 1    | char_set
                28 => (3, symbols![nt 20]),             // 28: char_set -> "[" char_set_1 "]"           | ◄28 "]" ►char_set_1 "["             | 3    | char_set_1
                29 => (1, symbols![]),                  // 29: char_set -> "."                          | ◄29 "."                             | 1    |
                30 => (1, symbols![t 30]),              // 30: char_set -> FixedSet                     | ◄30 FixedSet!                       | 1    | FixedSet
                31 => (1, symbols![t 30]),              // 31: char_set_one -> FixedSet                 | ◄31 FixedSet!                       | 1    | FixedSet
                32 => (0, symbols![]),                  // 32: char_set_one -> SetChar char_set_one_1   | ►char_set_one_1 SetChar!            | 0    |
                33 => (2, symbols![nt 15, nt 1]),       // 33: file_1 -> file_item file_1               | ●file_1 ◄33 ►file_item              | 2    | file_1 file_item
                34 => (1, symbols![nt 15]),             // 34: file_1 -> ε                              | ◄34                                 | 1    | file_1
                35 => (3, symbols![nt 16, t 27]),       // 35: option_1 -> "," Id option_1              | ●option_1 ◄35 Id! ","               | 3, 1 | option_1 Id
                36 => (1, symbols![nt 16]),             // 36: option_1 -> ε                            | ◄36                                 | 1    | option_1
                37 => (3, symbols![nt 17, nt 7]),       // 37: actions_1 -> "," action actions_1        | ●actions_1 ◄37 ►action ","          | 3, 1 | actions_1 action
                38 => (1, symbols![nt 17]),             // 38: actions_1 -> ε                           | ◄38                                 | 1    | actions_1
                39 => (3, symbols![nt 18, nt 10]),      // 39: alt_items_1 -> "|" alt_item alt_items_1  | ●alt_items_1 ◄39 ►alt_item "|"      | 3, 1 | alt_items_1 alt_item
                40 => (1, symbols![nt 18]),             // 40: alt_items_1 -> ε                         | ◄40                                 | 1    | alt_items_1
                41 => (0, symbols![]),                  // 41: alt_item_1 -> repeat_item alt_item_2     | ►alt_item_2 ►repeat_item            | 0    |
                42 => (0, symbols![]),                  // 42: char_set_1 -> char_set_one char_set_2    | ►char_set_2 ►char_set_one           | 0    |
                43 => (6, symbols![t 27, nt 8, nt 6]),  // 43: rule_1 -> "->" actions ";"               | ◄43 ";" ►actions "->"               | 6    | Id match actions
                44 => (4, symbols![t 27, nt 8]),        // 44: rule_1 -> ";"                            | ◄44 ";"                             | 4    | Id match
                45 => (0, symbols![]),                  // 45: repeat_item_1 -> "+" repeat_item_2       | ►repeat_item_2 "+"                  | 0    |
                46 => (2, symbols![nt 12]),             // 46: repeat_item_1 -> "?"                     | ◄46 "?"                             | 2    | item
                47 => (0, symbols![]),                  // 47: repeat_item_1 -> "*" repeat_item_3       | ►repeat_item_3 "*"                  | 0    |
                48 => (1, symbols![nt 12]),             // 48: repeat_item_1 -> ε                       | ◄48                                 | 1    | item
                49 => (3, symbols![t 28, t 28]),        // 49: item_1 -> ".." CharLit                   | ◄49 CharLit! ".."                   | 3    | CharLit CharLit
                50 => (1, symbols![t 28]),              // 50: item_1 -> ε                              | ◄50                                 | 1    | CharLit
                51 => (3, symbols![t 33, t 33]),        // 51: char_set_one_1 -> "-" SetChar            | ◄51 SetChar! "-"                    | 3    | SetChar SetChar
                52 => (1, symbols![t 33]),              // 52: char_set_one_1 -> ε                      | ◄52                                 | 1    | SetChar
                53 => (2, symbols![nt 19, nt 11]),      // 53: alt_item_2 -> alt_item_1                 | ●alt_item_1 ◄53                     | 2    | alt_item_1 repeat_item
                54 => (2, symbols![nt 19, nt 11]),      // 54: alt_item_2 -> ε                          | ◄54                                 | 2    | alt_item_1 repeat_item
                55 => (2, symbols![nt 20, nt 14]),      // 55: char_set_2 -> char_set_1                 | ●char_set_1 ◄55                     | 2    | char_set_1 char_set_one
                56 => (2, symbols![nt 20, nt 14]),      // 56: char_set_2 -> ε                          | ◄56                                 | 2    | char_set_1 char_set_one
                57 => (3, symbols![nt 12]),             // 57: repeat_item_2 -> "?"                     | ◄57 "?"                             | 3    | item
                58 => (2, symbols![nt 12]),             // 58: repeat_item_2 -> ε                       | ◄58                                 | 2    | item
                59 => (3, symbols![nt 12]),             // 59: repeat_item_3 -> "?"                     | ◄59 "?"                             | 3    | item
                60 => (2, symbols![nt 12]),             // 60: repeat_item_3 -> ε                       | ◄60                                 | 2    | item
            ], NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2, 3, 4], 2 => vec![5], 3 => vec![6], 4 => vec![7], 5 => vec![8, 43, 44], 6 => vec![10], 7 => vec![11, 12, 13, 14, 15, 16, 17], 8 => vec![18], 9 => vec![19], 10 => vec![20], 11 => vec![46, 48, 57, 58, 59, 60], 12 => vec![22, 23, 24, 26, 27, 49, 50], 13 => vec![28, 29, 30], 14 => vec![31, 51, 52]]),

            // NT flags:
            //  - program: parent_+_or_* | plus (6144)
            //  - decl_i: child_+_or_* | L-form (129)
            //  - inst_i: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - decl: parent_+_or_* (2048)
            //  - id_i: child_+_or_* | L-form | sep_list (32897)
            //  - expr: parent_left_rec | parent_amb (1536)
            //  - expr_1: child_left_rec (4)
            //  - expr_2: right_rec (2)
            //  - inst_i_1: child_left_fact (64)
            // parents:
            //  - decl_i -> program
            //  - inst_i -> program
            //  - id_i -> decl
            //  - expr_1 -> expr
            //  - expr_2 -> expr
            //  - inst_i_1 -> inst_i
            (902, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![nt 1, nt 2]),         //  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i         | 2    | decl_i inst_i
                1 => (2, symbols![nt 1, nt 3]),         //  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl           | 2    | decl_i decl
                2 => (1, symbols![nt 1]),               //  2: decl_i -> ε                   | ◄2                         | 1    | decl_i
                3 => (0, symbols![]),                   //  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst            | 0    |
                4 => (3, symbols![t 2, nt 4]),          //  4: decl -> Type Id id_i ";"      | ◄4 ";" ►id_i Id! Type!     | 3    | Type id_i
                5 => (4, symbols![t 2, t 1]),           //  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef" | 4    | Type Id
                6 => (3, symbols![nt 4, t 1]),          //  6: id_i -> "," Id id_i           | ●id_i ◄6 Id! ","           | 3, 1 | id_i Id
                7 => (1, symbols![nt 4]),               //  7: id_i -> ε                     | ◄7                         | 1    | id_i
                8 => (5, symbols![t 1, nt 6]),          //  8: inst -> "let" Id "=" expr ";" | ◄8 ";" ►expr "=" Id! "let" | 5    | Id expr
                9 => (3, symbols![nt 6]),               //  9: inst -> "print" expr ";"      | ◄9 ";" ►expr "print"       | 3    | expr
                10 => (1, symbols![nt 6]),              // 10: expr -> expr_2 expr_1         | ►expr_1 ◄10 ►expr_2        | 1    | expr
                11 => (3, symbols![nt 6, nt 6]),        // 11: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄11 ►expr_2 "+"    | 3    | expr expr
                12 => (3, symbols![nt 6, nt 6]),        // 12: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "-"    | 3    | expr expr
                13 => (1, symbols![nt 6]),              // 13: expr_1 -> ε                   | ◄13                        | 1    | expr
                14 => (2, symbols![nt 6]),              // 14: expr_2 -> "-" expr_2          | ◄14 ►expr_2 "-"            | 2    | expr
                15 => (1, symbols![t 1]),               // 15: expr_2 -> Id                  | ◄15 Id!                    | 1    | Id
                16 => (1, symbols![t 0]),               // 16: expr_2 -> Num                 | ◄16 Num!                   | 1    | Num
                17 => (2, symbols![nt 2, nt 5]),        // 17: inst_i_1 -> inst_i            | ●inst_i ◄17                | 2    | inst_i inst
                18 => (2, symbols![nt 2, nt 5]),        // 18: inst_i_1 -> ε                 | ◄18                        | 2    | inst_i inst
            ], NTValue::Default, btreemap![0 => vec![0], 3 => vec![4, 5], 5 => vec![8, 9], 6 => vec![10]]),
            (902, true, false, 0, btreemap![
            ], btreemap![
                0 => (2, symbols![]),                   //  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i         | 2 |
                1 => (2, symbols![]),                   //  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl           | 2 |
                2 => (1, symbols![]),                   //  2: decl_i -> ε                   | ◄2                         | 1 |
                3 => (0, symbols![]),                   //  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst            | 0 |
                4 => (4, symbols![t 2, t 1]),           //  4: decl -> Type Id id_i ";"      | ◄4 ";" ►id_i Id! Type!     | 4 | Type Id
                5 => (4, symbols![t 2, t 1]),           //  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef" | 4 | Type Id
                6 => (3, symbols![t 1]),                //  6: id_i -> "," Id id_i           | ●id_i ◄6 Id! ","           | 3 | Id
                7 => (1, symbols![]),                   //  7: id_i -> ε                     | ◄7                         | 1 |
                8 => (5, symbols![t 1]),                //  8: inst -> "let" Id "=" expr ";" | ◄8 ";" ►expr "=" Id! "let" | 5 | Id
                9 => (3, symbols![]),                   //  9: inst -> "print" expr ";"      | ◄9 ";" ►expr "print"       | 3 |
                10 => (1, symbols![]),                  // 10: expr -> expr_2 expr_1         | ►expr_1 ◄10 ►expr_2        | 1 |
                11 => (3, symbols![]),                  // 11: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄11 ►expr_2 "+"    | 3 |
                12 => (3, symbols![]),                  // 12: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "-"    | 3 |
                13 => (1, symbols![]),                  // 13: expr_1 -> ε                   | ◄13                        | 1 |
                14 => (2, symbols![]),                  // 14: expr_2 -> "-" expr_2          | ◄14 ►expr_2 "-"            | 2 |
                15 => (1, symbols![t 1]),               // 15: expr_2 -> Id                  | ◄15 Id!                    | 1 | Id
                16 => (1, symbols![t 0]),               // 16: expr_2 -> Num                 | ◄16 Num!                   | 1 | Num
                17 => (2, symbols![]),                  // 17: inst_i_1 -> inst_i            | ●inst_i ◄17                | 2 |
                18 => (2, symbols![]),                  // 18: inst_i_1 -> ε                 | ◄18                        | 2 |
            ], NTValue::None, btreemap![0 => vec![0], 3 => vec![4, 5], 5 => vec![8, 9], 6 => vec![10]]),

            // NT flags:
            //  - program: parent_+_or_* (2048)
            //  - stmt_i: child_+_or_* | L-form (129)
            //  - decl: parent_+_or_* (2048)
            //  - expr: parent_left_rec | parent_amb (1536)
            //  - decl_1: child_+_or_* | sep_list (32769)
            //  - expr_1: child_left_rec (4)
            //  - expr_2: right_rec (2)
            // parents:
            //  - stmt_i -> program
            //  - decl_1 -> decl
            //  - expr_1 -> expr
            //  - expr_2 -> expr
            (903, false, false, 0, btreemap![
            ], btreemap![
                0 => (1, symbols![nt 1]),               //  0: program -> stmt_i             | ◄0 ►stmt_i                 | 1    | stmt_i
                1 => (2, symbols![nt 1, nt 2]),         //  1: stmt_i -> stmt stmt_i         | ●stmt_i ◄1 ►stmt           | 2    | stmt_i stmt
                2 => (1, symbols![nt 1]),               //  2: stmt_i -> ε                   | ◄2                         | 1    | stmt_i
                3 => (1, symbols![nt 3]),               //  3: stmt -> decl                  | ◄3 ►decl                   | 1    | decl
                4 => (1, symbols![nt 4]),               //  4: stmt -> inst                  | ◄4 ►inst                   | 1    | inst
                5 => (3, symbols![t 2, nt 6]),          //  5: decl -> Type Id decl_1 ";"    | ◄5 ";" ►decl_1 Id! Type!   | 3    | Type decl_1
                6 => (4, symbols![t 2, t 1]),           //  6: decl -> "typedef" Type Id ";" | ◄6 ";" Id! Type! "typedef" | 4    | Type Id
                7 => (4, symbols![t 1, nt 5]),          //  7: inst -> Id "=" expr ";"       | ◄7 ";" ►expr "=" Id!       | 4    | Id expr
                8 => (3, symbols![nt 5]),               //  8: inst -> "print" expr ";"      | ◄8 ";" ►expr "print"       | 3    | expr
                9 => (1, symbols![nt 5]),               //  9: expr -> expr_2 expr_1         | ►expr_1 ◄9 ►expr_2         | 1    | expr
                10 => (3, symbols![nt 6, t 1]),         // 10: decl_1 -> "," Id decl_1       | ●decl_1 ◄10 Id! ","        | 3, 1 | decl_1 Id
                11 => (1, symbols![nt 6]),              // 11: decl_1 -> ε                   | ◄11                        | 1    | decl_1
                12 => (3, symbols![nt 5, nt 5]),        // 12: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "+"    | 3    | expr expr
                13 => (3, symbols![nt 5, nt 5]),        // 13: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄13 ►expr_2 "-"    | 3    | expr expr
                14 => (1, symbols![nt 5]),              // 14: expr_1 -> ε                   | ◄14                        | 1    | expr
                15 => (2, symbols![nt 5]),              // 15: expr_2 -> "-" expr_2          | ◄15 ►expr_2 "-"            | 2    | expr
                16 => (1, symbols![t 1]),               // 16: expr_2 -> Id                  | ◄16 Id!                    | 1    | Id
                17 => (1, symbols![t 0]),               // 17: expr_2 -> Num                 | ◄17 Num!                   | 1    | Num
            ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3, 4], 3 => vec![5, 6], 4 => vec![7, 8], 5 => vec![9]]),
            /*
            (, false, false, 0, btreemap![], btreemap![], Default, btreemap![]),
            */
        ];

        // those parsers don't require type definition in wrapper_code.rs (avoids an unused_imports warning):
        let type_gen_exclusion = |x: u32| matches!(x, 603..=632 | 153);

        const WRAPPER_FILENAME: &str = "tests/out/wrapper_source.rs";

        // print sources
        const VERBOSE: bool = true;        // prints the `tests` values from the results (easier to set the other constants to false)
        const VERBOSE_TYPE: bool = false;   // prints the code module skeleton (easier to set the other constants to false)
        const PRINT_SOURCE: bool = false;   // prints the wrapper module (easier to set the other constants to false)

        // override options
        // enable_test_source = true;
        // tests_all = true;

        // CAUTION! Setting this to 'true' modifies the validation file with the current result
        // replace_source = false;

        let mut num_errors = 0;
        let mut num_src_errors = 0;
        let mut rule_id_iter = HashMap::<u32, u32>::new();
        for (test_id, (tr_id, test_source, test_source_parser, start_nt, nt_type, expected_items, has_value, expected_alts)) in tests.into_iter().enumerate() {
            // if !matches!(tr_id, 109..150 | 212..250) { continue }
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
            if !ll1.has_no_errors() {
                if VERBOSE {
                    println!("## LL(1) build errors:\n{}", ll1.get_log());
                    num_errors += 1;
                    continue;
                }
            }
            let original_str = get_original_str(&ll1, 12);
            let mut builder = ParserGen::build_from_rules(ll1, "Test".to_string());
            builder.set_gen_span_params(true);
            builder.set_include_alts(true);
            builder.use_full_lib(true);
            let ambig_warnings = builder.log.get_warnings().filter(|w| w.contains("calc_table: ambiguity")).join("\n");
            let result_is_ambiguous = !ambig_warnings.is_empty();
            builder.set_nt_value(&has_value);
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
            let result_items = builder.item_ops.iter().enumerate()
                .map(|(a_id, v)| (a_id as AltId, (builder.span_nbrs[a_id], v.clone())))
                .collect::<BTreeMap<AltId, (SpanNbr, Vec<Symbol>)>>();
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
                builder.print_flags(12);
                println!("            ({tr_id}, {test_source}, {test_source_parser}, {start_nt}, btreemap![", );
                if !result_nt_type.is_empty() {
                    println!("{}", result_nt_type.iter().map(|(v, s)| format!("                {v} => \"{s}\".to_string(),")).join("\n"));
                }
                println!("            ], btreemap![");
                builder.print_items(16, true, true);
                let has_value_str = match &has_value {
                    NTValue::SetIds(s) => format!("NTValue::SetIds(vec![{}])", s.iter().map(|s| s.to_string()).join(", ")),
                    NTValue::SetNames(s) => format!("NTValue::SetNames(vec![{}])", s.iter().map(|s| format!("{s:?}")).join(", ")),
                    NTValue::Parents | NTValue::Default | NTValue::None => format!("NTValue::{has_value:?}"),
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
                if (enable_test_source || replace_source) && src.is_err() {
                    println!("## couldn't find the source code: {}", src.as_ref().err().unwrap());
                }
                src.ok()
            } else {
                None
            };
            let err_msg = format!("test {test_id} TestRules({tr_id}) #{rule_iter} failed");
            if tests_all {
                if result_items != expected_items || result_alts != expected_alts || result_nt_type != nt_type {
                    num_errors += 1;
                    println!("## ERROR: {err_msg}{}{}{}",
                             if result_items != expected_items { ", items mismatch" } else { "" },
                             if result_alts != expected_alts { ", alts mismatch" } else { "" },
                             if result_nt_type != nt_type { ", result type mismatch" } else { "" });
                }
                if (test_source && !cfg!(miri) && enable_test_source && Some(&result_src) != expected_src.as_ref()) || builder_has_errors {
                    if builder_has_errors {
                        println!("## ERRORS WHILE GENERATING SOURCE: {err_msg}");
                    } else {
                        if replace_source {
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
                if !cfg!(miri) && enable_test_source {
                    assert!(!builder_has_errors, "{} errors reported by source builder", builder.log.num_errors());
                    if replace_source && expected_src.is_some() && &result_src != expected_src.as_ref().unwrap() && !builder_has_errors {
                        replace_tagged_source(WRAPPER_FILENAME, &test_name, &result_src).expect("replacement failed");
                    }
                    assert_eq!(Some(result_src), expected_src, "{err_msg}");
                }
                assert!(!result_is_ambiguous, "{err_msg}, parsing table had ambiguities:\n{ambig_warnings}");
            }
        }
        if tests_all {
            assert_eq!(num_errors, 0, "{num_errors} test(s) have failed, including {num_src_errors} source error(s)");
        }
    }

    #[test]
    fn check_build_items() {
        // tests the generated sources
        const TEST_SOURCE: bool = true;

        // does all tests before giving an error summary
        const TESTS_ALL: bool = true;

        // CAUTION! Setting this to 'true' modifies the validation file with the current result
        const REPLACE_SOURCE: bool = true;

        build_items(TEST_SOURCE, TESTS_ALL, REPLACE_SOURCE);
    }

    #[ignore]
    #[test]
    fn write_build_items() {
        const TEST_SOURCE: bool = true;
        const TESTS_ALL: bool = true;

        build_items(TEST_SOURCE, TESTS_ALL, true);
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
