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

    fn get_source(rules_id: T, indent: usize, is_t_data: bool, name: String) -> String {
        let rules = match rules_id {
            RTS(rts_id) => {
                let rts = build_rts(rts_id);
                let mut rules = ProdRuleSet::from(rts);
                rules.set_start(0);
                if rules.get_symbol_table().is_none() {
                    let mut symbol_table = SymbolTable::new();
                    complete_symbol_table(&mut symbol_table, rules.get_num_t(), rules.get_num_nt(), is_t_data);
                    rules.set_symbol_table(symbol_table);
                }
                rules
            }
            PRS(prs_id) => {
                build_prs(prs_id, is_t_data)
            }
        };
        assert_eq!(rules.get_log().num_errors(), 0, "building {rules_id:?} failed:\n- {}", rules.get_log().get_errors().join("\n- "));
        let ll1 = ProdRuleSet::<LL1>::from(rules);
        let mut builder = ParserBuilder::from_rules(ll1, name);
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

    fn get_test_data<'a>(id: u32) -> Option<(T, usize, bool, &'a str, &'a str)> {
        match id {
            //         rules   indent  t_data   tag name                                      listener name
             0 => Some((PRS(13), 8,    false, "write_source_code_from_ll1",                   "None")),
             1 => Some((PRS( 4), 4,    false, "write_source_code_for_integration_listener",   "Expr")),
             2 => Some((PRS(13), 4,    false, "write_source_code_for_integration_listener2",  "Expr")),
             3 => Some((PRS(20), 4,    false, "write_source_code_for_integration_listener3",  "Struct")),
             4 => Some((PRS(30), 4,    false, "write_source_code_for_integration_listener4",  "Struct")),
             5 => Some((PRS(31), 4,    false, "write_source_code_for_integration_listener5",  "Expr")),
             6 => Some((PRS(32), 4,    false, "write_source_code_for_integration_listener6",  "Expr")),
             7 => Some((RTS(21), 4,    false, "write_source_code_for_integration_listener7",  "Star")),
             8 => Some((RTS(22), 4,    false, "write_source_code_for_integration_listener8",  "Star")),
             9 => Some((RTS(16), 4,    true,  "write_source_code_for_integration_listener9",  "Plus")),
            10 => Some((RTS(23), 4,    false, "write_source_code_for_integration_listener10", "Plus")),
            11 => Some((RTS(27), 4,    false, "write_source_code_for_integration_listener11", "Plus")),
            12 => Some((PRS(33), 4,    true,  "write_source_code_for_integration_listener12", "LeftRec")),
            _ => None
        }
    }

    fn do_test(id: u32, verbose: bool) -> bool {
        if let Some((rule_id, indent, is_t_data, tag, name)) = get_test_data(id) {
            let expected = get_source(rule_id, indent, is_t_data, name.to_string());
            if verbose {
                let s = String::from_utf8(vec![32; indent]).unwrap();
                println!("{s}// [{tag}]\n{expected}{s}// [{tag}]");
            }
            let result = get_integration_source(tag);
            assert_eq!(result, expected, "test failed for {id} / {rule_id:?} / {tag} ({name})");
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

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener9() {
        do_test(9, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener10() {
        do_test(10, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener11() {
        do_test(11, true);
    }

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener12() {
        do_test(12, true);
    }
}

mod opcodes {
    use crate::grammar::{Symbol, VarId};
    use crate::grammar::tests::{print_prs_summary, T};
    use crate::{CharLen, CollectJoin, strip};
    use crate::parser::{OpCode, Parser};
    use crate::parsergen::ParserBuilder;
    use crate::dfa::TokenId;

    fn get_factors_str(parser: &Parser) -> Vec<String> {
        parser.get_factors().iter().enumerate().map(|(id, (v, f))|
            format!("{id:2}: {} -> {}", Symbol::NT(*v).to_str(parser.get_symbol_table()), f.iter().map(|s| s.to_str(parser.get_symbol_table())).join(" "))
        ).collect()
    }

    pub(crate) fn opcode_to_macro(s: &OpCode) -> String {
        match s {
            OpCode::Empty => "e".to_string(),
            OpCode::T(t) => format!("t {t}"),
            OpCode::NT(v) => format!("nt {v}"),
            OpCode::Loop(v) => format!("loop {v}"),
            OpCode::Exit(v) => format!("exit {v}"),
            OpCode::End => "end".to_string(),
        }
    }

    fn print_opcodes(parser: &Parser) {
        let factors = get_factors_str(&parser);
        if !factors.is_empty() {
            let indent = 16;
            let opcodes = factors.into_iter().zip(parser.get_opcodes()).map(|(s, ops)|
                (
                    format!("strip![{}],", ops.iter().map(|o| opcode_to_macro(o)).join(", ")),
                    s,
                    ops.into_iter().map(|s| s.to_str(parser.get_symbol_table())).join(" ")
                )
            ).to_vec();
            let width = opcodes.iter().fold((39, 0), |acc, s| (acc.0.max(s.0.charlen()), acc.1.max(s.1.charlen())));
            let code = opcodes.into_iter()
                .map(|(a, b, c)| format!("{:indent$}{a:width_a$} // {b:width_b$} - {c}", "", indent=indent, width_a=width.0, width_b=width.1))
                .join("\n");
            println!("{}", code);
        }
    }

    #[test]
    fn parser_opcodes() {
        // terminal:     t (static) or t! (contains a string)
        // non-terminal: ►A
        // exit:         ◄2 (factor #2)
        // loop:         ●1 (factor #1)
        let tests: Vec<(T, VarId, Vec<Vec<OpCode>>)> = vec![
            // [A] + and * normalization ---------------------------------------------------
            // (note that + normalization implies a [D] left factorization)
            (T::RTS(9), 0, vec![                        // A -> var (id ,)+
                strip![exit 0, nt 1, t 1],              //  0: A -> var A_1    - ◄0 ►A_1 var
                strip![nt 2, t 3, t 2],                 //  1: A_1 -> id , A_2 - ►A_2 , id!
                strip![loop 1, exit 2],                 //  2: A_2 -> A_1      - ●A_1 ◄2
                strip![exit 3],                         //  3: A_2 -> ε        - ◄3
            ]),
            (T::RTS(12), 0, vec![                       // A -> b (c d)*
                strip![exit 0, nt 1, t 1],              //  0: A -> b A_1     - ◄0 ►A_1 b
                strip![loop 1, exit 1, t 3, t 2],       //  1: A_1 -> c d A_1 - ●A_1 ◄1 d c
                strip![exit 2],                         //  2: A_1 -> ε       - ◄2
            ]),
            // [A + A] cascaded + normalizations -------------------------------------------
            (T::RTS(17), 0, vec![                       // A -> a ( (b)+ c)+ d
                strip![exit 0, t 3, nt 2, t 0],         //  0: A -> a A_2 d     - ◄0 d ►A_2 a
                strip![nt 3, t 1],                      //  1: A_1 -> b A_3     - ►A_3 b
                strip![nt 4, t 2, nt 1],                //  2: A_2 -> A_1 c A_4 - ►A_4 c ►A_1
                strip![loop 1, exit 3],                 //  3: A_3 -> A_1       - ●A_1 ◄3
                strip![exit 4],                         //  4: A_3 -> ε         - ◄4
                strip![loop 2, exit 5],                 //  5: A_4 -> A_2       - ●A_2 ◄5
                strip![exit 6],                         //  6: A_4 -> ε         - ◄6
            ]),
            // [B] right recursion ---------------------------------------------------------
            (T::PRS(16), 0, vec![                       // A -> B A | b     B -> a
                strip![exit 0, nt 0, nt 1],             //  0: A -> B A - ◄0 ►A ►B
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b
                strip![exit 2, t 0],                    //  2: B -> a   - ◄2 a
            ]),
            (T::PRS(29), 0, vec![                       // A -> <L> B A | b     B -> a
                strip![loop 0, exit 0, nt 1],           //  0: A -> B A - ●A ◄0 ►B
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b
                strip![exit 2, t 0],                    //  2: B -> a   - ◄2 a
            ]),
            (T::PRS(20), 0, vec![
                strip![exit 0, nt 1, t 1, t 5, t 0],        //  0: STRUCT -> struct id { LIST - ◄0 ►LIST { id! struct
                strip![exit 1, nt 1, t 4, t 5, t 3, t 5],   //  1: LIST -> id : id ; LIST     - ◄1 ►LIST ; id! : id!
                strip![exit 2, t 2],                        //  2: LIST -> }                  - ◄2 }
            ]),
            (T::PRS(30), 0, vec![
                strip![exit 0, nt 1, t 1, t 5, t 0],        //  0: STRUCT -> struct id { LIST - ◄0 ►LIST { id! struct
                strip![loop 1, exit 1, t 4, t 5, t 3, t 5], //  1: LIST -> id : id ; LIST     - ●LIST ◄1 ; id! : id!
                strip![exit 2, t 2],                        //  2: LIST -> }                  - ◄2 }
            ]),
            // [C] left recursion ----------------------------------------------------------
            (T::PRS(26), 0, vec![                       // A -> A a | b
                strip![exit 0, nt 1, t 1],              //  0: A -> b A_1   - ◄0 ►A_1 b
                strip![loop 1, exit 1, t 0],            //  1: A_1 -> a A_1 - ●A_1 ◄1 a
                strip![exit 2],                         //  2: A_1 -> ε     - ◄2
            ]),
            (T::PRS(31), 0, vec![
                strip![exit 0, nt 2, nt 1],             //  0: E -> F E_1      - ◄0 ►E_1 ►F
                strip![exit 1, t 1],                    //  1: F -> id         - ◄1 id!
                strip![loop 2, exit 2, t 1, t 0],       //  2: E_1 -> . id E_1 - ●E_1 ◄2 id! .
                strip![exit 3],                         //  3: E_1 -> ε        - ◄3
            ]),
            (T::PRS(32), 0, vec![                       // E -> F | E . id | E . id ( );  F -> id
                strip![exit 0, nt 2, nt 1],             //  0: E -> F E_1      - ◄0 ►E_1 ►F
                strip![exit 1, t 1],                    //  1: F -> id         - ◄1 id!
                strip![nt 3, t 1, t 0],                 //  2: E_1 -> . id E_2 - ►E_2 id! .
                strip![exit 3],                         //  3: E_1 -> ε        - ◄3
                strip![loop 2, exit 4, t 3, t 2],       //  4: E_2 -> ( ) E_1  - ●E_1 ◄4 ) (
                strip![loop 2, exit 5],                 //  5: E_2 -> E_1      - ●E_1 ◄5
            ]),
            (T::PRS(4), 0, vec![                        // E -> E + T | E - T | T
                                                        // T -> T * F | T / F | F
                                                        // F -> ( E ) | NUM | ID
                strip![exit 0, nt 3, nt 1],             //  0: E -> T E_1     - ◄0 ►E_1 ►T
                strip![exit 1, nt 4, nt 2],             //  1: T -> F T_1     - ◄1 ►T_1 ►F
                strip![exit 2, t 5, nt 0, t 4],         //  2: F -> ( E )     - ◄2 ) ►E (
                strip![exit 3, t 6],                    //  3: F -> N         - ◄3 N!
                strip![exit 4, t 7],                    //  4: F -> I         - ◄4 I!
                strip![loop 3, exit 5, nt 1, t 0],      //  5: E_1 -> - T E_1 - ●E_1 ◄5 ►T -
                strip![loop 3, exit 6, nt 1, t 1],      //  6: E_1 -> + T E_1 - ●E_1 ◄6 ►T +
                strip![exit 7],                         //  7: E_1 -> ε       - ◄7
                strip![loop 4, exit 8, nt 2, t 2],      //  8: T_1 -> / F T_1 - ●T_1 ◄8 ►F /
                strip![loop 4, exit 9, nt 2, t 3],      //  9: T_1 -> * F T_1 - ●T_1 ◄9 ►F *
                strip![exit 10],                        // 10: T_1 -> ε       - ◄10
            ]),
            // [C/amb] left recursion and ambiguity ----------------------------------------
            (T::PRS(22), 0, vec![                       // E -> E * E | E & * E | E + E | E & + E | id
                strip![exit 0, nt 1, t 3],              //  0: E -> id E_1     - ◄0 ►E_1 id!
                strip![loop 1, exit 1, t 3, t 0],       //  1: E_1 -> * id E_1 - ●E_1 ◄1 id! *
                strip![loop 1, exit 2, t 3, t 1],       //  2: E_1 -> + id E_1 - ●E_1 ◄2 id! +
                strip![nt 2, t 2],                      //  3: E_1 -> & E_2    - ►E_2 &
                strip![exit 4],                         //  4: E_1 -> ε        - ◄4
                strip![loop 1, exit 5, t 3, t 0],       //  5: E_2 -> * id E_1 - ●E_1 ◄5 id! *
                strip![loop 1, exit 6, t 3, t 1],       //  6: E_2 -> + id E_1 - ●E_1 ◄6 id! +
            ]),
            (T::PRS(26), 1, vec![                       // B -> B a B | b
                strip![exit 0, nt 1, t 1],              //  0: B -> b B_1     - ◄0 ►B_1 b
                strip![loop 1, exit 1, t 1, t 0],       //  1: B_1 -> a b B_1 - ●B_1 ◄1 b a
                strip![exit 2],                         //  2: B_1 -> ε       - ◄2
            ]),
            (T::PRS(13), 0, vec![
                strip![exit 0, nt 2, nt 1],             //  0: E -> F E_1     - ◄0 ►E_1 ►F
                strip![exit 1, t 5, nt 0, t 4],         //  1: F -> ( E )     - ◄1 ) ►E (
                strip![exit 2, t 6],                    //  2: F -> N         - ◄2 N!
                strip![exit 3, t 7],                    //  3: F -> I         - ◄3 I!
                strip![loop 2, exit 4, nt 1, t 9],      //  4: E_1 -> : F E_1 - ●E_1 ◄4 ►F :
                strip![loop 2, exit 5, nt 1, t 8],      //  5: E_1 -> ^ F E_1 - ●E_1 ◄5 ►F ^
                strip![loop 2, exit 6, nt 1, t 2],      //  6: E_1 -> / F E_1 - ●E_1 ◄6 ►F /
                strip![loop 2, exit 7, nt 1, t 3],      //  7: E_1 -> * F E_1 - ●E_1 ◄7 ►F *
                strip![loop 2, exit 8, nt 1, t 0],      //  8: E_1 -> - F E_1 - ●E_1 ◄8 ►F -
                strip![loop 2, exit 9, nt 1, t 1],      //  9: E_1 -> + F E_1 - ●E_1 ◄9 ►F +
                strip![exit 10],                        // 10: E_1 -> ε       - ◄10
            ]),
            // [A + C] normalization + left recursion --------------------------------------
            (T::RTS(26), 0, vec![                       // A -> A a* b | c
                strip![exit 0, nt 2, t 0],              //  0: A -> a A_2       - ◄0 ►A_2 a
                strip![loop 1, exit 1, t 2],            //  1: A_1 -> c A_1     - ●A_1 ◄1 c
                strip![exit 2],                         //  2: A_1 -> ε         - ◄2
                strip![loop 2, exit 3, t 1, nt 1],      //  3: A_2 -> A_1 b A_2 - ●A_2 ◄3 b ►A_1
                strip![exit 4],                         //  4: A_2 -> ε         - ◄4
            ]),
            // [A + C + D] normalization + left factorization, left recursion --------------
            (T::RTS(16), 0, vec![                       // A -> A a+ b | c
                strip![exit 0, nt 2, t 0],              //  0: A -> a A_2       - ◄0 ►A_2 a
                strip![nt 3, t 2],                      //  1: A_1 -> c A_3     - ►A_3 c
                strip![loop 2, exit 2, t 1, nt 1],      //  2: A_2 -> A_1 b A_2 - ●A_2 ◄2 b ►A_1
                strip![exit 3],                         //  3: A_2 -> ε         - ◄3
                strip![loop 1, exit 4],                 //  4: A_3 -> A_1       - ●A_1 ◄4
                strip![exit 5],                         //  5: A_3 -> ε         - ◄5
            ]),
            // [D] left factorization -----------------------------------------------------
            (T::PRS(28), 0, vec![                       // A -> a | a b | a b c | a b d | e
                strip![nt 1, t 0],                      //  0: A -> a A_1   - ►A_1 a
                strip![exit 1, t 4],                    //  1: A -> e       - ◄1 e
                strip![nt 2, t 1],                      //  2: A_1 -> b A_2 - ►A_2 b
                strip![exit 3],                         //  3: A_1 -> ε     - ◄3
                strip![exit 4, t 2],                    //  4: A_2 -> c     - ◄4 c
                strip![exit 5, t 3],                    //  5: A_2 -> d     - ◄5 d
                strip![exit 6],                         //  6: A_2 -> ε     - ◄6
            ]),
            (T::PRS(33), 0, vec![                       // A -> A a | b c | b d
                strip![exit 0, nt 2, t 1],              //  0: A -> b A_2   - ◄0 ►A_2 b
                strip![loop 1, exit 1, t 0],            //  1: A_1 -> a A_1 - ●A_1 ◄1 a
                strip![exit 2],                         //  2: A_1 -> ε     - ◄2
                strip![nt 1, exit 3, t 2],              //  3: A_2 -> c A_1 - ►A_1 ◄3 c
                strip![nt 1, exit 4, t 3],              //  4: A_2 -> d A_1 - ►A_1 ◄4 d
            ]),
            /*
            (T::PRS(), 0, vec![
            ]),
            */
        ];
        const VERBOSE: bool = false;
        const TESTS_ALL: bool = false;
        let mut num_errors = 0;
        for (test_id, (rule_id, start_nt, expected_opcodes)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
            let ll1 = rule_id.get_prs(test_id, start_nt, false);
            if VERBOSE {
                print!("- ");
                print_prs_summary(&ll1);
            }
            let parser = ParserBuilder::from_rules(ll1, "Test".to_string()).make_parser();
            if VERBOSE {
                println!("Final factors and opcodes:");
                print_opcodes(&parser);
            }
            let err_msg = format!("test {test_id} {rule_id:?}/{start_nt} failed");
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

mod wrapper_source {
    use std::collections::{BTreeMap, HashSet};
    use std::fs::File;
    use std::io::Read;
    use crate::grammar::{ruleflag, FactorId, Symbol, VarId};
    use crate::grammar::tests::{symbol_to_macro, T};
    use crate::{btreemap, CharLen, CollectJoin, symbols};
    use crate::grammar::tests::T::{PRS, RTS};
    use crate::parsergen::ParserBuilder;
    use crate::dfa::TokenId;
    use crate::parsergen::tests::wrapper_source::HasValue::{Set, All, Default};

    #[derive(Clone)]
    enum HasValue { Set(Vec<Symbol>), All, Default }

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

    fn print_items(builder: &ParserBuilder, result_items: &BTreeMap<FactorId, Vec<Symbol>>, indent: usize) {
        let tbl = builder.get_symbol_table();
        let fields = (0..builder.parsing_table.factors.len())
            .filter_map(|f| {
                let f_id = f as FactorId;
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

    fn set_has_value(builder: &mut ParserBuilder, has_value: HasValue) {
        let mut valuables = HashSet::<TokenId>::new();
        let num_t = builder.parsing_table.num_t as TokenId - 1;    // excluding the end symbol
        match has_value {
            Set(symbols) => {
                for s in symbols {
                    match s {
                        Symbol::T(t) => { valuables.insert(t); }
                        Symbol::NT(v) => { builder.nt_value[v as usize] = true; }
                        _ => {}
                    }
                }
            },
            All | Default => {
                for v in 0..builder.parsing_table.num_nt {
                    if builder.parsing_table.parent[v].is_none() { builder.nt_value[v] = true }
                }
                if let All = has_value {
                    valuables.extend(0..num_t);
                } else /* Default */ {
                    valuables.extend((0..num_t).filter(|t| builder.symbol_table.is_t_data(*t)));
                }
            }
        }
        for t in 0..num_t {
            let is_valuable = valuables.contains(&t);
            if builder.symbol_table.is_t_data(t) != is_valuable {
                if is_valuable {
                    builder.symbol_table.set_t_name(t, None);
                } else {
                    let name = builder.symbol_table.get_t_name(t);
                    builder.symbol_table.set_t_name(t, Some(name));
                }
            }
        }
    }

    fn get_wrapper_source(tag: &str) -> Option<String> {
        const FILENAME: &str = "data/test/wrapper_source.out.rs";
        let file_tag = format!("[{tag}]");
        let mut file = File::open(FILENAME).ok()?;
        let mut buffer = String::new();
        file.read_to_string(&mut buffer).expect("Couldn't read source file");
        let mut result = buffer.lines()
            .skip_while(|l| !l.contains(&file_tag))
            .skip(2)
            .take_while(|l| !l.contains(&file_tag))
            .join("\n");
        result.push('\n');
        Some(result)
    }

    #[test]
    #[allow(unused_doc_comments)]
    fn build_items() {
        let tests: Vec<(T, u16, BTreeMap<u16, Vec<Symbol>>, HasValue)> = vec![
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
            ], Set(symbols![nt 0, nt 1, t 0, t 1])),
            // --------------------------------------------------------------------------- norm* R/L
            // A -> a (b)* c
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(21), 0, btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All),

            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* | L-form (129)
            // parents:
            //  - A_1 -> A
            (RTS(22), 0, btreemap![                     /// A -> a (b <L>)* c
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All),
            // NT flags:
            //  - A: parent_left_fact | parent_+_or_* (2080)
            //  - A_1: child_+_or_* | L-form (129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(32), 0, btreemap![                     /// A -> a (a | c) (b <L>)* c
                0 => symbols![],                        //  0: A -> a A_2     | ►A_2 a!       |
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1   | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε       | ◄2            |
                3 => symbols![t 0, t 0, nt 1, t 2],     //  3: A_2 -> a A_1 c | ◄3 c! ►A_1 a! | a a A_1 c
                4 => symbols![t 0, t 2, nt 1, t 2],     //  4: A_2 -> c A_1 c | ◄4 c! ►A_1 c! | a c A_1 c
            ], All),

            // When the repeated item has no data:

            // A -> a (#)* c
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(25), 0, btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> # A_1 | ●A_1 ◄1 #     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Default),
            // --------------------------------------------------------------------------- norm+ R/L
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(23), 0, btreemap![                     /// A -> a (b)+ c
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![nt 1, t 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | A_1 b
                3 => symbols![nt 1, t 1],               //  3: A_2 -> ε     | ◄3            | A_1 b
            ], All),
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(27), 0, btreemap![                     /// A -> a (B)+ c; B -> b
                0 => symbols![t 0, nt 2, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![t 1],                     //  1: B -> b       | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B A_2 | ►A_2 ►B       |
                3 => symbols![nt 2, nt 1],              //  3: A_2 -> A_1   | ●A_1 ◄3       | A_1 B
                4 => symbols![nt 2, nt 1],              //  4: A_2 -> ε     | ◄4            | A_1 B
            ], All),
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(28), 0, btreemap![                     /// A -> (a B)+ c; B -> b
                0 => symbols![nt 2, t 2],               //  0: A -> A_1 c     | ◄0 c! ►A_1 | A_1 c
                1 => symbols![t 1],                     //  1: B -> b         | ◄1 b!      | b
                2 => symbols![],                        //  2: A_1 -> a B A_2 | ►A_2 ►B a! |
                3 => symbols![nt 2, t 0, nt 1],         //  3: A_2 -> A_1     | ●A_1 ◄3    | A_1 a B
                4 => symbols![nt 2, t 0, nt 1],         //  4: A_2 -> ε       | ◄4         | A_1 a B
            ], All),
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            (RTS(29), 0, btreemap![                     /// A -> a ( (B b)* c)* d
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!           | b
                2 => symbols![nt 2, nt 1, t 1],         //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 B b
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, nt 2, t 2],         //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], All),
            (RTS(29), 0, btreemap![                     /// A -> a ( (B b)* c)* d
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![],                        //  1: B -> b           | ◄1 b            |
                2 => symbols![],                        //  2: A_1 -> B b A_1   | ●A_1 ◄2 b ►B    |
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, t 2],               //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], Set(symbols![nt 0, t 0, t 2, t 3])),
            (RTS(29), 0, btreemap![                     /// A -> a ( (B b)* c)* d
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!           | b
                2 => symbols![nt 2, t 1],               //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 b
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, nt 2, t 2],         //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], Set(symbols![t 0, t 1, t 2, t 3])),
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - A_3: child_left_fact (64)
            //  - A_4: child_left_fact (64)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            //  - A_3 -> A_1
            //  - A_4 -> A_2
            (RTS(30), 0, btreemap![                     /// A -> a ( (B b)+ c)+ d
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
                3 => symbols![],                        //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
                4 => symbols![nt 2, nt 1, t 1],         //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 B b
                5 => symbols![nt 2, nt 1, t 1],         //  5: A_3 -> ε         | ◄5            | A_1 B b
                6 => symbols![nt 3, nt 2, t 2],         //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
                7 => symbols![nt 3, nt 2, t 2],         //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
            ], All),
            (RTS(30), 0, btreemap![                     /// A -> a ( (B b)+ c)+ d
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
                3 => symbols![],                        //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
                4 => symbols![nt 2, t 1],               //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 b
                5 => symbols![nt 2, t 1],               //  5: A_3 -> ε         | ◄5            | A_1 b
                6 => symbols![nt 3, nt 2, t 2],         //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
                7 => symbols![nt 3, nt 2, t 2],         //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
            ], Set(symbols![t 0, t 1, t 2, t 3])),
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(24), 0, btreemap![                     /// A -> a (b <L>)+ c
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![nt 1, t 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | A_1 b
                3 => symbols![nt 1, t 1],               //  3: A_2 -> ε     | ◄3            | A_1 b
            ], Default),
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
            ], Default),
            // --------------------------------------------------------------------------- left_rec [left_fact]
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec (4)
            // parents:
            //  - E_1 -> E
            (PRS(31), 0, btreemap![                     /// E -> F | E . id ; F -> id
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ◄0 ►E_1 ►F    | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!        | id
                2 => symbols![nt 0, t 1],               //  2: E_1 -> . id E_1 | ●E_1 ◄2 id! . | E id
                3 => symbols![],                        //  3: E_1 -> ε        | ◄3            |
            ], Default),
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(33), 0, btreemap![                     /// A -> A a | b c | b d
                0 => symbols![],                        //  0: A -> b A_2   | ◄0 ►A_2 b! |
                1 => symbols![nt 0, t 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2         |
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d
            ], Default),
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
                4 => symbols![nt 0, t 1],               //  4: E_2 -> ( ) E_1  | ●E_1 ◄4 ) ( | E id
                5 => symbols![nt 0, t 1],               //  5: E_2 -> E_1      | ●E_1 ◄5     | E id
            ], Default),
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
            ], Default),
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> <L> id ':' id ';' LIST | '}'
            // NT flags:
            //  - LIST: right_rec | L-form (130)
            // parents:
            //  - (nothing)
            (PRS(30), 0, btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![nt 1, t 5, t 5],          //  1: LIST -> id : id ; LIST     | ●LIST ◄1 ; id! : id!  | LIST id id
                2 => symbols![nt 1],                    //  2: LIST -> }                  | ◄2 }                  | LIST
            ], Default),
            // ---------------------------------------------------------------------------
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* (2560)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_left_rec (4)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(26), 0, btreemap![                     /// A -> A (c)* b | a
                0 => symbols![t 0],                     //  0: A -> a A_2       | ◄0 ►A_2 a!      | a
                1 => symbols![nt 1, t 2],               //  1: A_1 -> c A_1     | ●A_1 ◄1 c!      | A_1 c
                2 => symbols![],                        //  2: A_1 -> ε         | ◄2              |
                3 => symbols![nt 0, nt 1, t 1],         //  3: A_2 -> A_1 b A_2 | ●A_2 ◄3 b! ►A_1 | A A_1 b
                4 => symbols![],                        //  4: A_2 -> ε         | ◄4              |
            ], Default),
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* | plus (6656)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_rec (4)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (RTS(16), 0, btreemap![                     /// A -> A (c)+ b | a
                0 => symbols![t 0],                     //  0: A -> a A_2       | ◄0 ►A_2 a!      | a
                1 => symbols![],                        //  1: A_1 -> c A_3     | ►A_3 c!         |
                2 => symbols![nt 0, nt 1, t 1],         //  2: A_2 -> A_1 b A_2 | ●A_2 ◄2 b! ►A_1 | A A_1 b
                3 => symbols![],                        //  3: A_2 -> ε         | ◄3              |
                4 => symbols![nt 1, t 2],               //  4: A_3 -> A_1       | ●A_1 ◄4         | A_1 c
                5 => symbols![nt 1, t 2],               //  5: A_3 -> ε         | ◄5              | A_1 c
            ], Default),

            // --------------------------------------------------------------------------- left_rec + amb
            // NT flags:
            //  - E: parent_left_rec | parent_amb (1536)
            //  - E_1: child_left_rec | child_amb (12)
            // parents:
            //  - E_1 -> E
            (PRS(13), 0, btreemap![                     /// // E -> E : E | E ^ E | E / E | E * E | E - E | E + E | F;  F -> ( E ) | NUM | ID
                0 => symbols![nt 1],                    //  0: E -> F E_1     | ◄0 ►E_1 ►F   | F
                1 => symbols![nt 0],                    //  1: F -> ( E )     | ◄1 ) ►E (    | E
                2 => symbols![t 6],                     //  2: F -> N         | ◄2 N!        | N
                3 => symbols![t 7],                     //  3: F -> I         | ◄3 I!        | I
                4 => symbols![nt 0, nt 1],              //  4: E_1 -> : F E_1 | ●E_1 ◄4 ►F : | E F  // ?? asm, not sure yet
                5 => symbols![nt 0, nt 1],              //  5: E_1 -> ^ F E_1 | ●E_1 ◄5 ►F ^ | E F
                6 => symbols![nt 0, nt 1],              //  6: E_1 -> / F E_1 | ●E_1 ◄6 ►F / | E F
                7 => symbols![nt 0, nt 1],              //  7: E_1 -> * F E_1 | ●E_1 ◄7 ►F * | E F
                8 => symbols![nt 0, nt 1],              //  8: E_1 -> - F E_1 | ●E_1 ◄8 ►F - | E F
                9 => symbols![nt 0, nt 1],              //  9: E_1 -> + F E_1 | ●E_1 ◄9 ►F + | E F
                10 => symbols![],                       // 10: E_1 -> ε       | ◄10          |
            ], Default),
            // (PRS(9), 0, btreemap![]),
            // (PRS(10), 0, btreemap![]),
            // (PRS(15), 0, btreemap![]),
            // (RTS(31), 0, btreemap![], Default),  // TODO: reports error, not supported, user must create NT for OR under + or *
            // --------------------------------------------------------------------------- misc
            // NT flags:
            //  - A: parent_left_fact (32)
            //  - A_1: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            (PRS(35), 0, btreemap![                     /// A -> a | a b b | a c c
                0 => symbols![],                        //  0: A -> a A_1 | ►A_1 a!  |
                1 => symbols![t 0, t 1, t 1],           //  1: A_1 -> b b | ◄1 b! b! | a b b
                2 => symbols![t 0, t 2, t 2],           //  2: A_1 -> c c | ◄2 c! c! | a c c
                3 => symbols![t 0],                     //  3: A_1 -> ε   | ◄3       | a
            ], Default),
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(33), 0, btreemap![
                0 => symbols![nt 2, t 1],               //  0: A -> A_1 b     | ◄0 b! ►A_1    | A_1 b
                1 => symbols![t 0],                     //  1: A -> a         | ◄1 a!         | a
                2 => symbols![t 1],                     //  2: B -> b         | ◄2 b!         | b
                3 => symbols![nt 2, nt 1, t 2],         //  3: A_1 -> B c A_1 | ●A_1 ◄3 c! ►B | A_1 B c
                4 => symbols![],                        //  4: A_1 -> ε       | ◄4            |
            ], All),
            // ---------------------------------------------------------------------------
            /*
            (PRS(), 0, btreemap![], Default),
            (RTS(), 0, btreemap![], Default),
            */
        ];
        const VERBOSE: bool = true;
        const PRINT_SOURCE: bool = true;
        const TEST_SOURCE: bool = true;
        const TESTS_ALL: bool = false;
        let mut num_errors = 0;
        for (test_id, (rule_id, start_nt, expected_items, has_value)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
            let ll1 = rule_id.get_prs(test_id, start_nt, true);
            let mut builder = ParserBuilder::from_rules(ll1, "Test".to_string());
            set_has_value(&mut builder, has_value.clone());
            if VERBOSE {
                println!("before, NT with value: {}",
                         (0..builder.parsing_table.num_nt).into_iter().filter_map(|v|
                             if builder.nt_value[v] { Some(Symbol::NT(v as VarId).to_str(builder.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            builder.build_item_ops();
            if VERBOSE {
                println!("after,  NT with value: {}",
                         (0..builder.parsing_table.num_nt).into_iter().filter_map(|v|
                             if builder.nt_value[v] { Some(Symbol::NT(v as VarId).to_str(builder.get_symbol_table())) } else { None }
                         ).join(", "));
            }
            let result_items = builder.item_ops.iter().map(|(f, v)| (f.clone(), v.clone())).collect::<BTreeMap<FactorId, Vec<Symbol>>>();
            let src = builder.source_wrapper();
            let test_name = format!("wrapper source for test {test_id}: {rule_id:?}, start {}", Symbol::NT(start_nt).to_str(builder.get_symbol_table()));
            if VERBOSE {
                print_flags(&builder, 12);
                println!("            ({rule_id:?}, {start_nt}, btreemap![", );
                print_items(&builder, &result_items, 16);
                let has_value_str = match &has_value {
                    Set(s) => format!("Set(symbols![{}])", s.iter().map(|s| symbol_to_macro(s)).join(", ")),
                    All => "All".to_string(),
                    Default => "Default".to_string()
                };
                println!("            ], {has_value_str}),");
            }
            let mut result_src = src.into_iter().map(|s| if !s.is_empty() { format!("    {s}") } else { s }).join("\n");
            result_src.push_str("\n\n");
            if PRINT_SOURCE {
                println!("// {0:-<60}\n// [{test_name}]\n\n{result_src}// [{test_name}]\n// {:-<60}\n", "");
            }
            let expected_src = get_wrapper_source(&test_name);
            let err_msg = format!("test {test_id} {rule_id:?}/{start_nt} failed ");
            if TESTS_ALL {
                if result_items != expected_items {
                    num_errors += 1;
                    println!("## ERROR: {err_msg}");
                }
                if TEST_SOURCE && Some(result_src) != expected_src {
                    num_errors += 1;
                    println!("## SOURCE MISMATCH: {err_msg}");
                }
            } else {
                assert_eq!(result_items, expected_items, "{err_msg}");
                if TEST_SOURCE {
                    assert_eq!(Some(result_src), expected_src, "{err_msg}");
                }
            }
        }
        if TESTS_ALL {
            assert_eq!(num_errors, 0, "{num_errors} tests have failed");
        }
    }
}
