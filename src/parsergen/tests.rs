#![cfg(test)]

mod gen_integration {
    use std::fs::File;
    use std::io::Read;
    use crate::grammar::ProdRuleSet;
    use crate::grammar::tests::{build_prs, build_rts, complete_symbol_table};
    use crate::{CollectJoin, LL1};
    use crate::parsergen::ParserBuilder;
    use crate::parsergen::tests::gen_integration::T::{PRS, RTS};
    use crate::symbol_table::SymbolTable;

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub(crate) enum T { RTS(u32), PRS(u32) }

    fn get_source(rules_id: T, indent: usize) -> String {
        let rules = match rules_id {
            RTS(rts_id) => {
                let rts = build_rts(rts_id);
                let mut rules = ProdRuleSet::from(rts);
                rules.set_start(0);
                if rules.get_symbol_table().is_none() {
                    let mut symbol_table = SymbolTable::new();
                    complete_symbol_table(&mut symbol_table, rules.get_num_t(), rules.get_num_nt(), false);
                    rules.set_symbol_table(symbol_table);
                }
                rules
            }
            PRS(prs_id) => {
                build_prs(prs_id, false)
            }
        };
        assert_eq!(rules.get_log().num_errors(), 0, "building {rules_id:?} failed:\n- {}", rules.get_log().get_errors().join("\n- "));
        let ll1 = ProdRuleSet::<LL1>::from(rules);
        let mut builder = ParserBuilder::from_rules(ll1);
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

    fn get_test_data<'a>(id: u32) -> Option<(T, usize, &'a str)> {
        match id {
            0 => Some((PRS(13), 8, "write_source_code_from_ll1")),
            1 => Some((PRS( 4), 4, "write_source_code_for_integration_listener")),
            2 => Some((PRS(13), 4, "write_source_code_for_integration_listener2")),
            3 => Some((PRS(20), 4, "write_source_code_for_integration_listener3")),
            4 => Some((PRS(30), 4, "write_source_code_for_integration_listener4")),
            5 => Some((PRS(31), 4, "write_source_code_for_integration_listener5")),
            6 => Some((PRS(32), 4, "write_source_code_for_integration_listener6")),
            7 => Some((RTS(21), 4, "write_source_code_for_integration_listener7")),
            8 => Some((RTS(22), 4, "write_source_code_for_integration_listener8")),
            _ => None
        }
    }

    fn do_test(id: u32, verbose: bool) -> bool {
        if let Some((rule_id, indent, tag)) = get_test_data(id) {
            let expected = get_source(rule_id, indent);
            if verbose {
                let s = String::from_utf8(vec![32; indent]).unwrap();
                println!("{s}// [{tag}]\n{expected}{s}// [{tag}]");
            }
            let result = get_integration_source(tag);
            assert_eq!(result, expected, "test failed for {id} / {rule_id:?} / {tag}");
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

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener7() {
        do_test(7, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener8() {
        do_test(8, true);
    }
}

mod wrapper_source {
    use std::collections::BTreeMap;
    use crate::grammar::{ruleflag, Symbol, VarId};
    use crate::grammar::tests::{symbol_to_macro, T};
    use crate::{btreemap, CharLen, CollectJoin, symbols};
    use crate::grammar::tests::T::{PRS, RTS};
    use crate::parsergen::ParserBuilder;
    use crate::dfa::TokenId;

    fn print_flags(builder: &ParserBuilder, indent: usize) {
        let tbl = builder.get_symbol_table();
        let prefix = format!("{:width$}//", "", width=indent);
        let nt_flags = builder.parsing_table.flags.iter().enumerate().filter_map(|(nt, &f)|
            if f != 0 { Some(format!("{prefix}  - {}: {} ({})", Symbol::NT(nt as VarId).to_str(tbl), ruleflag::to_string(f).join(" | "), f)) } else { None }
        ).join("\n");
        let parents = builder.parsing_table.parent.iter().enumerate().filter_map(|(c, &par)|
            if let Some(p) = par { Some(format!("{prefix}  - {} -> {}", Symbol::NT(c as VarId).to_str(tbl), Symbol::NT(p).to_str(tbl))) } else { None }
        ).join("\n");
        println!("{prefix} NT flags:\n{}", if nt_flags.is_empty() { format!("{prefix}  - (nothing)") } else { nt_flags });
        println!("{prefix} parents:\n{}", if parents.is_empty() { format!("{prefix}  - (nothing)") } else { parents });
    }

    fn print_items(builder: &ParserBuilder, result_items: &BTreeMap<VarId, Vec<Symbol>>, indent: usize) {
        let tbl = builder.get_symbol_table();
        let fields = (0..builder.parsing_table.factors.len())
            .filter_map(|f| {
                let f_id = f as VarId;
                let (v, factor) = &builder.parsing_table.factors[f];
                let ops = &builder.opcodes[f];
                if let Some(it) = result_items.get(&f_id) {
                    Some((
                        format!("{f_id} => symbols![{}],", it.iter().map(|s| symbol_to_macro(s)).join(", ")),
                        format!("{f_id:2}: {} -> {}", Symbol::NT(*v).to_str(tbl), factor.iter().map(|s| s.to_str(tbl)).join(" ")),
                        ops.into_iter().map(|s| s.to_str(tbl)).join(" "),
                        format!("{}", it.iter().map(|s| s.to_str(tbl)).join(" ")),
                    ))
                } else {
                    None
                }
            }).to_vec();
        let width = fields.iter().fold((40, 0, 0), |acc, s| {
            (acc.0.max(s.0.charlen()), acc.1.max(s.1.charlen()), acc.2.max(s.2.charlen()))
        });
        for (symbols, c_factor, c_ops, c_items) in fields {
            println!("{:indent$}{symbols:width_a$}// {c_factor:width_b$} | {c_ops:width_c$} | {c_items}",
                     "", width_a = width.0, width_b = width.1, width_c = width.2, indent = indent);
        }
    }

    #[test]
    #[allow(unused_doc_comments)]
    fn build_items() {
        let tests: Vec<(T, VarId, BTreeMap<VarId, Vec<Symbol>>)> = vec![
            // --------------------------------------------------------------------------- NT/T simple mix
            // NT flags:
            //  - (nothing)
            // parents:
            //  - (nothing)
            (PRS(34), 0, btreemap![
                0 => symbols![t 0, nt 1],               //  0: S -> id = VAL   | ◄0 ►VAL = id!  | id VAL
                1 => symbols![],                        //  1: S -> exit       | ◄1 exit        |
                2 => symbols![nt 1],                    //  2: S -> return VAL | ◄2 ►VAL return | VAL
                3 => symbols![t 0],                     //  3: VAL -> id       | ◄3 id!         | id
                4 => symbols![t 1],                     //  4: VAL -> num      | ◄4 num!        | num
            ]),
            // --------------------------------------------------------------------------- norm* R/L
            // A -> a (b)* c
            // NT flags:
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(21), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c   // !
                1 => symbols![t 1, nt 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | b A_1     // !
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ]),
            // A -> a (b <L>)* c
            // NT flags:
            //  - A_1: child_+_or_* | L-form (129)
            // parents:
            //  - A_1 -> A
            (RTS(22), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![t 1, nt 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | b A_1
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ]),
            // --------------------------------------------------------------------------- norm+ R/L
            // A -> a (b)+ c
            // NT flags:
            //  - A_1: child_+_or_* | parent_left_fact (33)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(23), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c   // !
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![t 1, nt 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | b A_1     // !
                3 => symbols![t 1, nt 1],               //  3: A_2 -> ε     | ◄3            | b A_1     // !!!
            ]),
            // A -> a (b <L>)+ c
            // NT flags:
            //  - A_1: child_+_or_* | parent_left_fact | L-form (161)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(24), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![t 1, nt 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | b A_1
                3 => symbols![t 1, nt 1],               //  3: A_2 -> ε     | ◄3            | b A_1
            ]),
            // --------------------------------------------------------------------------- left_fact
            // NT flags:
            //  - A: parent_left_fact (32)
            //  - A_1: parent_left_fact | child_left_fact (96)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (PRS(28), 0, btreemap![                     /// A -> a | a b | a b c | a b d | e
                0 => symbols![],                        //  0: A -> a A_1   | ►A_1 a! |
                1 => symbols![t 4],                     //  1: A -> e       | ◄1 e!   | e
                2 => symbols![],                        //  2: A_1 -> b A_2 | ►A_2 b! |
                3 => symbols![t 0],                     //  3: A_1 -> ε     | ◄3      | a
                4 => symbols![t 0, t 1, t 2],           //  4: A_2 -> c     | ◄4 c!   | a b c
                5 => symbols![t 0, t 1, t 3],           //  5: A_2 -> d     | ◄5 d!   | a b d
                6 => symbols![t 0, t 1],                //  6: A_2 -> ε     | ◄6      | a b
            ]),
            // --------------------------------------------------------------------------- left_rec [left_fact]
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(33), 0, btreemap![                     /// A -> A a | b c | b d
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![t 0, nt 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | a A
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2         |
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ◄3 ►A_1 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ◄4 ►A_1 d! | b d
            ]),
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec | parent_left_fact (36)
            //  - E_2: child_left_fact (64)
            // parents:
            //  - E_1 -> E
            //  - E_2 -> E_1
            (PRS(32), 0, btreemap![                     /// E -> F | E . id | E . id ( )
                                                        /// F -> id
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ◄0 ►E_1 ►F  | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!      | id
                2 => symbols![],                        //  2: E_1 -> . id E_2 | ►E_2 id! .  |
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3          |
                4 => symbols![t 1, nt 0],               //  4: E_2 -> ( ) E_1  | ●E_1 ◄4 ) ( | id E
                5 => symbols![t 1, nt 0],               //  5: E_2 -> E_1      | ●E_1 ◄5     | id E
            ]),
            // --------------------------------------------------------------------------- right_rec L/R
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | '}'
            // NT flags:
            //  - LIST: right_rec (2)
            // parents:
            //  - (nothing)
            (PRS(20), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![t 5, t 5, nt 1],          //  1: LIST -> id : id ; LIST     | ◄1 ►LIST ; id! : id!  | id id LIST
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ]),
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> <L> id ':' id ';' LIST | '}'
            // NT flags:
            //  - LIST: right_rec | L-form (130)
            // parents:
            //  - (nothing)
            (PRS(30), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![t 5, t 5],                //  1: LIST -> id : id ; LIST     | ●LIST ◄1 ; id! : id!  | id id
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ]),
            // --------------------------------------------------------------------------- left_rec + amb
            // (PRS(9), 0, btreemap![]),
            // (PRS(10), 0, btreemap![]),
            // (PRS(13), 0, btreemap![]),
            // (PRS(15), 0, btreemap![]),
            // ---------------------------------------------------------------------------
            /*
            (PRS(), 0, btreemap![]),
            */
        ];
        const TESTS_ALL: bool = true;
        const VERBOSE: bool = true;
        let mut num_errors = 0;
        for (test_id, (rule_id, start_nt, expected_items)) in tests.into_iter().enumerate() {
            // if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
            let ll1 = rule_id.get_prs(test_id, start_nt, true);
            let mut builder = ParserBuilder::from_rules(ll1);
            builder.nt_value = (0..builder.parsing_table.num_nt)
                .map(|nt| builder.parsing_table.parent[nt].is_none()).to_vec();
            let items = builder.build_item_ops();
            let result_items = items.into_iter().collect::<BTreeMap<VarId, Vec<Symbol>>>();
            if VERBOSE {
                print_flags(&builder, 12);
                println!("            ({rule_id:?}, {start_nt}, btreemap![", );
                print_items(&builder, &result_items, 16);
                println!("            ]),");
            }
            let err_msg = format!("test {test_id} {rule_id:?}/{start_nt} failed ");
            if TESTS_ALL {
                if result_items != expected_items {
                    num_errors += 1;
                    println!("## ERROR: {err_msg}");
                }
            } else {
                assert_eq!(result_items, expected_items, );
            }
        }
        if TESTS_ALL {
            assert_eq!(num_errors, 0, "{num_errors} tests have failed");
        }
    }
}