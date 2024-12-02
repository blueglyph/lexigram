#![cfg(test)]

use crate::{columns_to_str, CollectJoin};
use crate::grammar::{ruleflag, FactorId, Symbol, VarId};
use crate::grammar::tests::symbol_to_macro;
use crate::parsergen::ParserGen;

pub(crate) fn print_items(builder: &ParserGen, indent: usize, show_symbols: bool) {
    let tbl = builder.get_symbol_table();
    let fields = (0..builder.parsing_table.factors.len())
        .filter_map(|f| {
            let f_id = f as FactorId;
            let (v, factor) = &builder.parsing_table.factors[f];
            let ops = &builder.opcodes[f];
            if let Some(it) = builder.item_ops.get(&f_id) {
                let mut cols = vec![];
                if show_symbols {
                    cols.push(format!("{f_id} => symbols![{}],", it.iter().map(|s| symbol_to_macro(s)).join(", ")));
                }
                cols.extend([
                    format!("// {f_id:2}: {} -> {}", Symbol::NT(*v).to_str(tbl), factor.iter().map(|s| s.to_str(tbl)).join(" ")),
                    format!("| {}", ops.into_iter().map(|s| s.to_str(tbl)).join(" ")),
                    format!("| {}", it.iter().map(|s| s.to_str(tbl)).join(" ")),
                ]);
                Some(cols)
            } else {
                None
            }
        }).to_vec();
    let widths = if show_symbols { vec![40, 0, 0, 0] } else { vec![16, 0, 0] };
    for l in columns_to_str(fields, Some(widths)) {
        println!("{:indent$}{l}", "", indent = indent)
    }
}

pub(crate) fn print_flags(builder: &ParserGen, indent: usize) {
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

mod gen_integration {
    use crate::grammar::ProdRuleSet;
    use crate::grammar::tests::{build_prs, build_rts, complete_symbol_table};
    use crate::{CollectJoin, LL1};
    use crate::parsergen::ParserGen;
    use crate::parsergen::tests::gen_integration::T::{PRS, RTS};
    use crate::symbol_table::SymbolTable;
    use crate::test_tools::get_tagged_source;

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
        let mut builder = ParserGen::from_rules(ll1, name);
        builder.build_source_code(indent, false)
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
            13 => Some((PRS(36), 4,    false, "write_source_code_for_integration_listener13",  "Expr")),
            _ => None
        }
    }

    fn do_test(id: u32, verbose: bool) -> bool {
        const FILENAME: &str = "tests/integration/parser_examples.rs";
        if let Some((rule_id, indent, is_t_data, tag, name)) = get_test_data(id) {
            let expected = get_source(rule_id, indent, is_t_data, name.to_string());
            if verbose {
                let s = String::from_utf8(vec![32; indent]).unwrap();
                println!("{s}// [{tag}]\n{expected}{s}// [{tag}]");
            }
            let result = get_tagged_source(FILENAME, tag).expect("");
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

    #[ignore]
    #[test]
    fn write_source_code_for_integration_listener13() {
        do_test(13, true);
    }
}

mod opcodes {
    use crate::grammar::{Symbol, VarId};
    use crate::grammar::tests::{print_prs_summary, T};
    use crate::{CollectJoin, strip, columns_to_str};
    use crate::parser::{OpCode, Parser};
    use crate::parsergen::ParserGen;
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
                vec![
                    format!("strip![{}],", ops.iter().map(|o| opcode_to_macro(o)).join(", ")),
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
            // // A -> a A | b
            // - NT flags:
            //   - A: right_rec (2)
            (T::RTS(35), 0, vec![
                strip![exit 0, nt 0, t 0],              //  0: A -> a A - ◄0 ►A a!
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b!
            ]),
            // A -> a A <L> | b
            // - NT flags:
            //   - A: right_rec | L-form (130)
            (T::RTS(36), 0, vec![
                strip![loop 0, exit 0, t 0],            //  0: A -> a A - ●A ◄0 a!
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b!
            ]),
            // // A -> a A | b
            (T::PRS(40), 0, vec![
                strip![exit 0, nt 0, t 0],              //  0: A -> a A - ◄0 ►A a!
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b!
            ]),
            // A -> a A <L> | b
            (T::PRS(41), 0, vec![
                strip![loop 0, exit 0, t 0],            //  0: A -> a A - ●A ◄0 a!
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b!
            ]),
            // A -> a A <L> | b
            (T::PRS(42), 0, vec![
                strip![loop 0, exit 0, t 0],            //  0: A -> a A - ●A ◄0 a!
                strip![exit 1, t 1],                    //  1: A -> b   - ◄1 b!
            ]),
            // [C] left recursion ----------------------------------------------------------
            (T::PRS(26), 0, vec![                       // A -> A a | b
                strip![nt 1, exit 0, t 1],              //  0: A -> b A_1   - ►A_1 ◄0 b
                strip![loop 1, exit 1, t 0],            //  1: A_1 -> a A_1 - ●A_1 ◄1 a
                strip![exit 2],                         //  2: A_1 -> ε     - ◄2
            ]),
            (T::PRS(31), 0, vec![                       // E -> F | E . id;  F -> id
                strip![nt 2, exit 0, nt 1],             //  0: E -> F E_1      - ►E_1 ◄0 ►F
                strip![exit 1, t 1],                    //  1: F -> id         - ◄1 id!
                strip![loop 2, exit 2, t 1, t 0],       //  2: E_1 -> . id E_1 - ●E_1 ◄2 id! .
                strip![exit 3],                         //  3: E_1 -> ε        - ◄3
            ]),
            (T::PRS(36), 0, vec![                       // E -> F | num | E . id;  F -> id
                strip![nt 2, exit 0, nt 1],             //  0: E -> F E_1      - ►E_1 ◄0 ►F
                strip![nt 2, exit 1, t 2],              //  1: E -> num E_1    - ►E_1 ◄1 num!
                strip![exit 2, t 1],                    //  2: F -> id         - ◄2 id!
                strip![loop 2, exit 3, t 1, t 0],       //  3: E_1 -> . id E_1 - ●E_1 ◄3 id! .
                strip![exit 4],                         //  4: E_1 -> ε        - ◄4
            ]),
            (T::PRS(32), 0, vec![                       // E -> F | E . id | E . id ( );  F -> id
                strip![nt 2, exit 0, nt 1],             //  0: E -> F E_1      - ►E_1 ◄0 ►F
                strip![exit 1, t 1],                    //  1: F -> id         - ◄1 id!
                strip![nt 3, t 1, t 0],                 //  2: E_1 -> . id E_2 - ►E_2 id! .
                strip![exit 3],                         //  3: E_1 -> ε        - ◄3
                strip![loop 2, exit 4, t 3, t 2],       //  4: E_2 -> ( ) E_1  - ●E_1 ◄4 ) (
                strip![loop 2, exit 5],                 //  5: E_2 -> E_1      - ●E_1 ◄5
            ]),
            (T::PRS(4), 0, vec![                        // E -> E + T | E - T | T
                                                        // T -> T * F | T / F | F
                                                        // F -> ( E ) | NUM | ID
                strip![nt 3, exit 0, nt 1],             //  0: E -> T E_1     - ►E_1 ◄0 ►T
                strip![nt 4, exit 1, nt 2],             //  1: T -> F T_1     - ►T_1 ◄1 ►F
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
            // A -> A a c? | A b c? | d
            // - NT flags:
            //   - A: parent_left_rec (512)
            //   - A_1: child_left_rec | parent_left_fact (36)
            //   - A_2: child_left_fact (64)
            //   - A_3: child_left_fact (64)
            // - parents:
            //   - A_1 -> A
            //   - A_2 -> A_1
            //   - A_3 -> A_1
            (T::RTS(38), 0, vec![
                strip![nt 1, exit 0, t 3],              //  0: A -> d A_1   - ►A_1 ◄0 d!
                strip![nt 2, t 0],                      //  1: A_1 -> a A_2 - ►A_2 a!
                strip![nt 3, t 1],                      //  2: A_1 -> b A_3 - ►A_3 b!
                strip![exit 3],                         //  3: A_1 -> ε     - ◄3
                strip![loop 1, exit 4, t 2],            //  4: A_2 -> c A_1 - ●A_1 ◄4 c!
                strip![loop 1, exit 5],                 //  5: A_2 -> A_1   - ●A_1 ◄5
                strip![loop 1, exit 6, t 2],            //  6: A_3 -> c A_1 - ●A_1 ◄6 c!
                strip![loop 1, exit 7],                 //  7: A_3 -> A_1   - ●A_1 ◄7
            ]),
            // [C/amb] left recursion and ambiguity ----------------------------------------
            (T::PRS(22), 0, vec![                       // E -> E * E | E & * E | E + E | E & + E | id
                strip![nt 1, exit 0, t 3],              //  0: E -> id E_1     - ►E_1 ◄0 id!
                strip![loop 1, exit 1, t 3, t 0],       //  1: E_1 -> * id E_1 - ●E_1 ◄1 id! *
                strip![loop 1, exit 2, t 3, t 1],       //  2: E_1 -> + id E_1 - ●E_1 ◄2 id! +
                strip![nt 2, t 2],                      //  3: E_1 -> & E_2    - ►E_2 &
                strip![exit 4],                         //  4: E_1 -> ε        - ◄4
                strip![loop 1, exit 5, t 3, t 0],       //  5: E_2 -> * id E_1 - ●E_1 ◄5 id! *
                strip![loop 1, exit 6, t 3, t 1],       //  6: E_2 -> + id E_1 - ●E_1 ◄6 id! +
            ]),
            (T::PRS(26), 1, vec![                       // B -> B a B | b
                strip![nt 1, exit 0, t 1],              //  0: B -> b B_1     - ►B_1 ◄0 b
                strip![loop 1, exit 1, t 1, t 0],       //  1: B_1 -> a b B_1 - ●B_1 ◄1 b a
                strip![exit 2],                         //  2: B_1 -> ε       - ◄2
            ]),
            (T::PRS(13), 0, vec![
                strip![nt 2, exit 0, nt 1],             //  0: E -> F E_1     - ►E_1 ◄0 ►F
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
                strip![nt 2, exit 0, t 0],              //  0: A -> a A_2       - ►A_2 ◄0 a
                strip![loop 1, exit 1, t 2],            //  1: A_1 -> c A_1     - ●A_1 ◄1 c
                strip![exit 2],                         //  2: A_1 -> ε         - ◄2
                strip![loop 2, exit 3, t 1, nt 1],      //  3: A_2 -> A_1 b A_2 - ●A_2 ◄3 b ►A_1
                strip![exit 4],                         //  4: A_2 -> ε         - ◄4
            ]),
            // [A + C + D] normalization + left factorization, left recursion --------------
            (T::RTS(16), 0, vec![                       // A -> A a+ b | c
                strip![nt 2, exit 0, t 0],              //  0: A -> a A_2       - ►A_2 ◄0 a
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
                strip![nt 2, t 1],                      //  0: A -> b A_2   - ►A_2 b
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
            let parser = ParserGen::from_rules(ll1, "Test".to_string()).make_parser();
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
    use std::collections::{BTreeMap, HashMap, HashSet};
    use crate::grammar::{FactorId, Symbol, VarId};
    use crate::grammar::tests::{symbol_to_macro, T};
    use crate::{btreemap, CollectJoin, symbols, columns_to_str, hashset, indent_source};
    use crate::grammar::tests::T::{PRS, RTS};
    use crate::parsergen::ParserGen;
    use crate::dfa::TokenId;
    use crate::parsergen::tests::{print_flags, print_items};
    use crate::parsergen::tests::wrapper_source::HasValue::{Set, All, Default};
    use crate::test_tools::{get_tagged_source, replace_tagged_source};

    #[derive(Clone)]
    enum HasValue { Set(Vec<Symbol>), All, Default }

    fn set_has_value(builder: &mut ParserGen, has_value: HasValue) {
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

    #[test]
    #[allow(unused_doc_comments)]
    fn build_items() {
        let tests: Vec<(
            T,                              // rule (PRS or RTS)
            u16,                            // start NT
            BTreeMap<VarId, String>,        // NT types
            BTreeMap<u16, Vec<Symbol>>,     // expected items
            HasValue,                       // which symbols have a value
            BTreeMap<VarId, Vec<FactorId>>, // expected factor groups
        )> = vec![
            // --------------------------------------------------------------------------- NT/T simple mix
            // NT flags:
            //  - (nothing)
            // parents:
            //  - (nothing)
            (PRS(34), 0, btreemap![
                0 => "SynS".to_string(),
                1 => "SynVal".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1],               //  0: S -> id = VAL   | ◄0 ►VAL = id!  | id VAL
                1 => symbols![],                        //  1: S -> exit       | ◄1 exit        |
                2 => symbols![nt 1],                    //  2: S -> return VAL | ◄2 ►VAL return | VAL
                3 => symbols![t 0],                     //  3: VAL -> id       | ◄3 id!         | id
                4 => symbols![t 1],                     //  4: VAL -> num      | ◄4 num!        | num
            ], Set(symbols![nt 0, nt 1, t 0, t 1]), btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),
            // --------------------------------------------------------------------------- norm* R/L
            // A -> a (b)* c
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(21), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![nt 1, t 1],               //  1: A_1 -> b A_1 | ●A_1 ◄1 b!    | A_1 b
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], All, btreemap![0 => vec![0]]),
            (RTS(21), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> b A_1 | ●A_1 ◄1 b     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Set(symbols![nt 0, t 0, t 2]), btreemap![0 => vec![0]]),

            // TODO: check - with only nt 0, - with nothing

            // A -> a (b <L=AIter1>)* c
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - AIter1: child_+_or_* | L-form (129)
            // parents:
            //  - AIter1 -> A
            (RTS(22), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynAIter".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a AIter1 c    | ◄0 c! ►AIter1 a! | a AIter1 c
                1 => symbols![nt 1, t 1],               //  1: AIter1 -> b AIter1 | ●AIter1 ◄1 b!    | AIter1 b
                2 => symbols![],                        //  2: AIter1 -> ε        | ◄2               |
            ], All, btreemap![0 => vec![0]]),
            (RTS(22), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a AIter1 c    | ◄0 c! ►AIter1 a! | a c
                1 => symbols![],                        //  1: AIter1 -> b AIter1 | ●AIter1 ◄1 b     |
                2 => symbols![],                        //  2: AIter1 -> ε        | ◄2               |
            ], Set(symbols![nt 0, t 0, t 2]), btreemap![0 => vec![0]]),

            // A -> a (a | c) (b <L=AIter1>)* c
            // NT flags:
            //  - A: parent_left_fact | parent_+_or_* (2080)
            //  - AIter1: child_+_or_* | L-form (129)
            //  - A_1: child_left_fact (64)
            // parents:
            //  - AIter1 -> A
            //  - A_1 -> A
            (RTS(32), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynAIter".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: A -> a A_1         | ►A_1 a!          |
                1 => symbols![nt 1, t 1],               //  1: AIter1 -> b AIter1 | ●AIter1 ◄1 b!    | AIter1 b
                2 => symbols![],                        //  2: AIter1 -> ε        | ◄2               |
                3 => symbols![t 0, t 0, nt 1, t 2],     //  3: A_1 -> a AIter1 c  | ◄3 c! ►AIter1 a! | a a AIter1 c
                4 => symbols![t 0, t 2, nt 1, t 2],     //  4: A_1 -> c AIter1 c  | ◄4 c! ►AIter1 c! | a c AIter1 c
            ], All, btreemap![0 => vec![3, 4]]),

            // When the repeated item has no data:

            // A -> a (#)* c
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(25), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 0, t 2],                //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a c
                1 => symbols![],                        //  1: A_1 -> # A_1 | ●A_1 ◄1 #     |
                2 => symbols![],                        //  2: A_1 -> ε     | ◄2            |
            ], Default, btreemap![0 => vec![0]]),
            // --------------------------------------------------------------------------- norm+ R/L
            // A -> a (b)+ c
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(23), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![],                        //  1: A_1 -> b A_2 | ►A_2 b!       |
                2 => symbols![nt 1, t 1],               //  2: A_2 -> A_1   | ●A_1 ◄2       | A_1 b
                3 => symbols![nt 1, t 1],               //  3: A_2 -> ε     | ◄3            | A_1 b
            ], All, btreemap![0 => vec![0]]),

            // A -> a (B)+ c
            // B -> b
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(27), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynB".to_string(),
                2 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 2, t 2],          //  0: A -> a A_1 c | ◄0 c! ►A_1 a! | a A_1 c
                1 => symbols![t 1],                     //  1: B -> b       | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B A_2 | ►A_2 ►B       |
                3 => symbols![nt 2, nt 1],              //  3: A_2 -> A_1   | ●A_1 ◄3       | A_1 B
                4 => symbols![nt 2, nt 1],              //  4: A_2 -> ε     | ◄4            | A_1 B
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),

            // A -> (a B)+ c
            // B -> b
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (RTS(28), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynB".to_string(),
                2 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![nt 2, t 2],               //  0: A -> A_1 c     | ◄0 c! ►A_1 | A_1 c
                1 => symbols![t 1],                     //  1: B -> b         | ◄1 b!      | b
                2 => symbols![],                        //  2: A_1 -> a B A_2 | ►A_2 ►B a! |
                3 => symbols![nt 2, t 0, nt 1],         //  3: A_2 -> A_1     | ●A_1 ◄3    | A_1 a B
                4 => symbols![nt 2, t 0, nt 1],         //  4: A_2 -> ε       | ◄4         | A_1 a B
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),

            // A -> a (b <L=AIter1>)+ c
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - AIter1: child_+_or_* | parent_left_fact | L-form | plus (4257)
            //  - A_1: child_left_fact (64)
            // parents:
            //  - AIter1 -> A
            //  - A_1 -> AIter1
            (RTS(24), 0, btreemap![
                0 => "SynMyA".to_string(),
                1 => "SynMyAIter".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 1, t 2],          //  0: A -> a AIter1 c | ◄0 c! ►AIter1 a! | a AIter1 c
                1 => symbols![],                        //  1: AIter1 -> b A_1 | ►A_1 b!          |
                2 => symbols![nt 1, t 1],               //  2: A_1 -> AIter1   | ●AIter1 ◄2       | AIter1 b
                3 => symbols![nt 1, t 1],               //  3: A_1 -> ε        | ◄3               | AIter1 b
            ], Default, btreemap![0 => vec![0]]),
            // --------------------------------------------------------------------------- norm+/* 2 levels
            // A -> a ( (B b)* c)* d
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - A_1 -> A_2
            //  - A_2 -> A
            (RTS(29), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynB".to_string(),
                2 => "SynA1".to_string(),
                3 => "SynA2".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!           | b
                2 => symbols![nt 2, nt 1, t 1],         //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 B b
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, nt 2, t 2],         //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),
            (RTS(29), 0, btreemap![
                0 => "SynA".to_string(),
                3 => "SynA2".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![],                        //  1: B -> b           | ◄1 b            |
                2 => symbols![],                        //  2: A_1 -> B b A_1   | ●A_1 ◄2 b ►B    |
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, t 2],               //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], Set(symbols![nt 0, t 0, t 2, t 3]), btreemap![0 => vec![0], 1 => vec![1]]),
            (RTS(29), 0, btreemap![
                2 => "SynA1".to_string(),
                3 => "SynA2".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a!   | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!           | b
                2 => symbols![nt 2, t 1],               //  2: A_1 -> B b A_1   | ●A_1 ◄2 b! ►B   | A_1 b
                3 => symbols![],                        //  3: A_1 -> ε         | ◄3              |
                4 => symbols![nt 3, nt 2, t 2],         //  4: A_2 -> A_1 c A_2 | ●A_2 ◄4 c! ►A_1 | A_2 A_1 c
                5 => symbols![],                        //  5: A_2 -> ε         | ◄5              |
            ], Set(symbols![t 0, t 1, t 2, t 3]), btreemap![0 => vec![0], 1 => vec![1]]),

            // A -> a (<L=AIter1> (<L=AIter2> b)* c)* d
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - AIter2: child_+_or_* | L-form (129)
            //  - AIter1: child_+_or_* | L-form | parent_+_or_* (2177)
            // parents:
            //  - AIter2 -> AIter1
            //  - AIter1 -> A
            (RTS(39), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynAiter2".to_string(),
                2 => "SynAiter1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 2, t 3],          //  0: A -> a AIter1 d           | ◄0 d! ►AIter1 a!      | a AIter1 d
                1 => symbols![nt 1, t 1],               //  1: AIter2 -> b AIter2        | ●AIter2 ◄1 b!         | AIter2 b
                2 => symbols![],                        //  2: AIter2 -> ε               | ◄2                    |
                3 => symbols![nt 2, nt 1, t 2],         //  3: AIter1 -> AIter2 c AIter1 | ●AIter1 ◄3 c! ►AIter2 | AIter1 AIter2 c
                4 => symbols![],                        //  4: AIter1 -> ε               | ◄4                    |
            ], Default, btreemap![0 => vec![0]]),

            // a ( (<L=AIter1> b)* c)* d
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - AIter1: child_+_or_* | L-form (129)
            //  - A_1: child_+_or_* | parent_+_or_* (2049)
            // parents:
            //  - AIter1 -> A_1
            //  - A_1 -> A
            (RTS(40), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynAiter1".to_string(),
                2 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 2, t 3],          //  0: A -> a A_1 d        | ◄0 d! ►A_1 a!      | a A_1 d
                1 => symbols![nt 1, t 1],               //  1: AIter1 -> b AIter1  | ●AIter1 ◄1 b!      | AIter1 b
                2 => symbols![],                        //  2: AIter1 -> ε         | ◄2                 |
                3 => symbols![nt 2, nt 1, t 2],         //  3: A_1 -> AIter1 c A_1 | ●A_1 ◄3 c! ►AIter1 | A_1 AIter1 c
                4 => symbols![],                        //  4: A_1 -> ε            | ◄4                 |
            ], Default, btreemap![0 => vec![0]]),

            // A -> a ( (B b)+ c)+ d
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
            (RTS(30), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynB".to_string(),
                2 => "SynA1".to_string(),
                3 => "SynA2".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
                3 => symbols![],                        //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
                4 => symbols![nt 2, nt 1, t 1],         //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 B b
                5 => symbols![nt 2, nt 1, t 1],         //  5: A_3 -> ε         | ◄5            | A_1 B b
                6 => symbols![nt 3, nt 2, t 2],         //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
                7 => symbols![nt 3, nt 2, t 2],         //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
            ], All, btreemap![0 => vec![0], 1 => vec![1]]),
            (RTS(30), 0,btreemap![
                2 => "SynA1".to_string(),
                3 => "SynA2".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 3, t 3],          //  0: A -> a A_2 d     | ◄0 d! ►A_2 a! | a A_2 d
                1 => symbols![t 1],                     //  1: B -> b           | ◄1 b!         | b
                2 => symbols![],                        //  2: A_1 -> B b A_3   | ►A_3 b! ►B    |
                3 => symbols![],                        //  3: A_2 -> A_1 c A_4 | ►A_4 c! ►A_1  |
                4 => symbols![nt 2, t 1],               //  4: A_3 -> A_1       | ●A_1 ◄4       | A_1 b
                5 => symbols![nt 2, t 1],               //  5: A_3 -> ε         | ◄5            | A_1 b
                6 => symbols![nt 3, nt 2, t 2],         //  6: A_4 -> A_2       | ●A_2 ◄6       | A_2 A_1 c
                7 => symbols![nt 3, nt 2, t 2],         //  7: A_4 -> ε         | ◄7            | A_2 A_1 c
            ], Set(symbols![t 0, t 1, t 2, t 3]), btreemap![0 => vec![0], 1 => vec![1]]),

            // A -> a ( (b)+ (b)+ )+ c ( (b)+ (b)+ )+ d
            // NT flags:
            //  - A: parent_+_or_* | plus (6144)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_3: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - A_4: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_5: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_6: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
            //  - A_7: child_left_fact (64)
            //  - A_8: child_left_fact (64)
            //  - A_9: child_left_fact (64)
            //  - A_10: child_left_fact (64)
            //  - A_11: child_left_fact (64)
            //  - A_12: child_left_fact (64)
            // parents:
            //  - A_1 -> A_3
            //  - A_2 -> A
            //  - A_3 -> A
            //  - A_4 -> A_6
            //  - A_5 -> A
            //  - A_6 -> A
            //  - A_7 -> A_1
            //  - A_8 -> A_2
            //  - A_9 -> A_3
            //  - A_10 -> A_4
            //  - A_11 -> A_5
            //  - A_12 -> A_6
            (RTS(34), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
                2 => "SynA2".to_string(),
                3 => "SynA3".to_string(),
                4 => "SynA4".to_string(),
                5 => "SynA5".to_string(),
                6 => "SynA6".to_string(),
            ], btreemap![
                0 => symbols![t 0, nt 3, t 2, nt 6, t 3], //  0: A -> a A_3 c A_6 d  | ◄0 d! ►A_6 c! ►A_3 a! | a A_3 c A_6 d
                1 => symbols![],                          //  1: A_1 -> b A_7        | ►A_7 b!               |
                2 => symbols![],                          //  2: A_2 -> b A_8        | ►A_8 b!               |
                3 => symbols![],                          //  3: A_3 -> A_1 A_2 A_9  | ►A_9 ►A_2 ►A_1        |
                4 => symbols![],                          //  4: A_4 -> b A_10       | ►A_10 b!              |
                5 => symbols![],                          //  5: A_5 -> b A_11       | ►A_11 b!              |
                6 => symbols![],                          //  6: A_6 -> A_4 A_5 A_12 | ►A_12 ►A_5 ►A_4       |
                7 => symbols![nt 1, t 1],                 //  7: A_7 -> A_1          | ●A_1 ◄7               | A_1 b
                8 => symbols![nt 1, t 1],                 //  8: A_7 -> ε            | ◄8                    | A_1 b
                9 => symbols![nt 2, t 1],                 //  9: A_8 -> A_2          | ●A_2 ◄9               | A_2 b
                10 => symbols![nt 2, t 1],                // 10: A_8 -> ε            | ◄10                   | A_2 b
                11 => symbols![nt 3, nt 1, nt 2],         // 11: A_9 -> A_3          | ●A_3 ◄11              | A_3 A_1 A_2
                12 => symbols![nt 3, nt 1, nt 2],         // 12: A_9 -> ε            | ◄12                   | A_3 A_1 A_2
                13 => symbols![nt 4, t 1],                // 13: A_10 -> A_4         | ●A_4 ◄13              | A_4 b
                14 => symbols![nt 4, t 1],                // 14: A_10 -> ε           | ◄14                   | A_4 b
                15 => symbols![nt 5, t 1],                // 15: A_11 -> A_5         | ●A_5 ◄15              | A_5 b
                16 => symbols![nt 5, t 1],                // 16: A_11 -> ε           | ◄16                   | A_5 b
                17 => symbols![nt 6, nt 4, nt 5],         // 17: A_12 -> A_6         | ●A_6 ◄17              | A_6 A_4 A_5
                18 => symbols![nt 6, nt 4, nt 5],         // 18: A_12 -> ε           | ◄18                   | A_6 A_4 A_5
            ], Default, btreemap![0 => vec![0]]),
            // --------------------------------------------------------------------------- left_fact
            // A -> a | a b | a b c | a b d | e
            // NT flags:
            //  - A: parent_left_fact (32)
            //  - A_1: parent_left_fact | child_left_fact (96)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            (PRS(28), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: A -> a A_1   | ►A_1 a! |
                1 => symbols![t 4],                     //  1: A -> e       | ◄1 e!   | e
                2 => symbols![],                        //  2: A_1 -> b A_2 | ►A_2 b! |
                3 => symbols![t 0],                     //  3: A_1 -> ε     | ◄3      | a
                4 => symbols![t 0, t 1, t 2],           //  4: A_2 -> c     | ◄4 c!   | a b c
                5 => symbols![t 0, t 1, t 3],           //  5: A_2 -> d     | ◄5 d!   | a b d
                6 => symbols![t 0, t 1],                //  6: A_2 -> ε     | ◄6      | a b
            ], Default, btreemap![0 => vec![1, 3, 4, 5, 6]]),
            // --------------------------------------------------------------------------- left_rec [left_fact]
            // E -> F | E . id
            // F -> id
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec (4)
            // parents:
            //  - E_1 -> E
            (PRS(31), 0, btreemap![
                0 => "SynE".to_string(),
                1 => "SynF".to_string(),
            ], btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F    | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!        | id
                2 => symbols![nt 0, t 1],               //  2: E_1 -> . id E_1 | ●E_1 ◄2 id! . | E id
                3 => symbols![nt 0],                    //  3: E_1 -> ε        | ◄3            | E
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),

            // E -> F | num | E . id
            // F -> id
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec (4)
            // parents:
            //  - E_1 -> E
            (PRS(36), 0, btreemap![
                0 => "SynE".to_string(),
                1 => "SynF".to_string(),
            ], btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F    | F
                1 => symbols![t 2],                     //  1: E -> num E_1    | ◄1 ►E_1 num!  | num
                2 => symbols![t 1],                     //  2: F -> id         | ◄2 id!        | id
                3 => symbols![nt 0, t 1],               //  3: E_1 -> . id E_1 | ●E_1 ◄3 id! . | E id
                4 => symbols![nt 0],                    //  4: E_1 -> ε        | ◄4            | E
            ], Default, btreemap![0 => vec![0, 1], 1 => vec![2]]),

            // A -> A a | b c | b d
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(33), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![nt 0, t 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a
                2 => symbols![nt 0],                    //  2: A_1 -> ε     | ◄2         | A
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d
            ], Default, btreemap![0 => vec![3, 4]]),

            // A -> A a | A b | b c | b d
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec (4)
            //  - A_2: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (PRS(38), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![nt 0, t 0],               //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a
                2 => symbols![nt 0, t 1],               //  2: A_1 -> b A_1 | ●A_1 ◄2 b! | A b
                3 => symbols![nt 0],                    //  3: A_1 -> ε     | ◄3         | A
                4 => symbols![t 1, t 2],                //  4: A_2 -> c A_1 | ►A_1 ◄4 c! | b c
                5 => symbols![t 1, t 3],                //  5: A_2 -> d A_1 | ►A_1 ◄5 d! | b d
            ], Default, btreemap![0 => vec![4, 5]]),

            // A -> A a b | A a c | b c | b d
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec | parent_left_fact (36)
            //  - A_2: child_left_fact (64)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (PRS(39), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: A -> b A_2   | ►A_2 b!    |
                1 => symbols![],                        //  1: A_1 -> a A_3 | ►A_3 a!    |
                2 => symbols![nt 0],                    //  2: A_1 -> ε     | ◄2         | A
                3 => symbols![t 1, t 2],                //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c
                4 => symbols![t 1, t 3],                //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d
                5 => symbols![nt 0, t 0, t 1],          //  5: A_3 -> b A_1 | ●A_1 ◄5 b! | A a b
                6 => symbols![nt 0, t 0, t 2],          //  6: A_3 -> c A_1 | ●A_1 ◄6 c! | A a c
            ], Default, btreemap![0 => vec![3, 4]]),

            // E -> F | E . id | E . id ( )
            // F -> id
            // NT flags:
            //  - E: parent_left_rec (512)
            //  - E_1: child_left_rec | parent_left_fact (36)
            //  - E_2: child_left_fact (64)
            // parents:
            //  - E_1 -> E
            //  - E_2 -> E_1
            (PRS(32), 0, btreemap![
                0 => "SynE".to_string(),
                1 => "SynF".to_string(),
            ], btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1      | ►E_1 ◄0 ►F  | F
                1 => symbols![t 1],                     //  1: F -> id         | ◄1 id!      | id
                2 => symbols![],                        //  2: E_1 -> . id E_2 | ►E_2 id! .  |
                3 => symbols![nt 0],                    //  3: E_1 -> ε        | ◄3          | E
                4 => symbols![nt 0, t 1],               //  4: E_2 -> ( ) E_1  | ●E_1 ◄4 ) ( | E id
                5 => symbols![nt 0, t 1],               //  5: E_2 -> E_1      | ●E_1 ◄5     | E id
            ], Default, btreemap![0 => vec![0], 1 => vec![1]]),

            // A -> A a c? | A b c? | d
            // NT flags:
            //  - A: parent_left_rec (512)
            //  - A_1: child_left_rec | parent_left_fact (36)
            //  - A_2: child_left_fact (64)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A_1
            //  - A_3 -> A_1
            (RTS(38), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 3],                     //  0: A -> d A_1   | ►A_1 ◄0 d! | d
                1 => symbols![],                        //  1: A_1 -> a A_2 | ►A_2 a!    |
                2 => symbols![],                        //  2: A_1 -> b A_3 | ►A_3 b!    |
                3 => symbols![nt 0],                    //  3: A_1 -> ε     | ◄3         | A
                4 => symbols![nt 0, t 0, t 2],          //  4: A_2 -> c A_1 | ●A_1 ◄4 c! | A a c
                5 => symbols![nt 0, t 0],               //  5: A_2 -> A_1   | ●A_1 ◄5    | A a
                6 => symbols![nt 0, t 1, t 2],          //  6: A_3 -> c A_1 | ●A_1 ◄6 c! | A b c
                7 => symbols![nt 0, t 1],               //  7: A_3 -> A_1   | ●A_1 ◄7    | A b
            ], Default, btreemap![0 => vec![0]]),
            (RTS(38), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![t 3],                     //  0: A -> d A_1   | ►A_1 ◄0 d! | d
                1 => symbols![],                        //  1: A_1 -> a A_2 | ►A_2 a     |
                2 => symbols![],                        //  2: A_1 -> b A_3 | ►A_3 b     |
                3 => symbols![nt 0],                    //  3: A_1 -> ε     | ◄3         | A
                4 => symbols![nt 0],                    //  4: A_2 -> c A_1 | ●A_1 ◄4 c  | A
                5 => symbols![nt 0],                    //  5: A_2 -> A_1   | ●A_1 ◄5    | A
                6 => symbols![nt 0],                    //  6: A_3 -> c A_1 | ●A_1 ◄6 c  | A
                7 => symbols![nt 0],                    //  7: A_3 -> A_1   | ●A_1 ◄7    | A
            ], Set(symbols!(nt 0, t 3)), btreemap![0 => vec![0]]),
            (RTS(38), 0, btreemap![
            ], btreemap![
                0 => symbols![],                        //  0: A -> d A_1   | ►A_1 ◄0 d  |
                1 => symbols![],                        //  1: A_1 -> a A_2 | ►A_2 a     |
                2 => symbols![],                        //  2: A_1 -> b A_3 | ►A_3 b     |
                3 => symbols![],                        //  3: A_1 -> ε     | ◄3         |
                4 => symbols![],                        //  4: A_2 -> c A_1 | ●A_1 ◄4 c  |
                5 => symbols![],                        //  5: A_2 -> A_1   | ●A_1 ◄5    |
                6 => symbols![],                        //  6: A_3 -> c A_1 | ●A_1 ◄6 c  |
                7 => symbols![],                        //  7: A_3 -> A_1   | ●A_1 ◄7    |
            ], Set(symbols!()), btreemap![0 => vec![0]]),
            // --------------------------------------------------------------------------- right_rec L/R
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | '}'
            // NT flags:
            //  - LIST: right_rec (2)
            // parents:
            //  - (nothing)
            (PRS(20), 0, btreemap![
                0 => "SynStruct".to_string(),
                1 => "SynList".to_string(),
            ], btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![t 5, t 5, nt 1],          //  1: LIST -> id : id ; LIST     | ◄1 ►LIST ; id! : id!  | id id LIST
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ], Default, btreemap![0 => vec![0], 1 => vec![1, 2]]),
            (PRS(20), 0, btreemap![
                0 => "SynStruct".to_string(),
            ], btreemap![
                0 => symbols![t 5],                     //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id
                1 => symbols![t 5, t 5],                //  1: LIST -> id : id ; LIST     | ◄1 ►LIST ; id! : id!  | id id
                2 => symbols![],                        //  2: LIST -> }                  | ◄2 }                  |
            ], Set(symbols![nt 0, t 5]), btreemap![0 => vec![0], 1 => vec![1, 2]]),

            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | id ';' LIST | '}'
            // NT flags:
            //  - LIST: right_rec | parent_left_fact (34)
            //  - LIST_1: child_left_fact (64)
            // parents:
            //  - LIST_1 -> LIST
            (PRS(37), 0, btreemap![
                0 => "SynStruct".to_string(),
                1 => "SynList".to_string(),
            ], btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![],                        //  1: LIST -> }                  | ◄1 }                  |
                2 => symbols![],                        //  2: LIST -> id LIST_1          | ►LIST_1 id!           |
                3 => symbols![t 5, t 5],                //  3: LIST_1 -> : id ; LIST      | ●LIST ◄3 ; id! :      | id id
                4 => symbols![t 5],                     //  4: LIST_1 -> ; LIST           | ●LIST ◄4 ;            | id
            ], Default, btreemap![0 => vec![0], 1 => vec![1, 3, 4]]),

            // STRUCT -> 'struct' id '{' LIST
            // LIST -> <L> id ':' id ';' LIST | '}'
            // NT flags:
            //  - LIST: right_rec | L-form (130)
            // parents:
            //  - (nothing)
            (PRS(30), 0, btreemap![
                0 => "SynStruct".to_string(),
                1 => "SynList".to_string(),
            ], btreemap![
                0 => symbols![t 5, nt 1],               //  0: STRUCT -> struct id { LIST | ◄0 ►LIST { id! struct | id LIST
                1 => symbols![nt 1, t 5, t 5],          //  1: LIST -> id : id ; LIST     | ●LIST ◄1 ; id! : id!  | LIST id id
                2 => symbols![nt 1],                    //  2: LIST -> }                  | ◄2 }                  | LIST
            ], Default, btreemap![0 => vec![0], 1 => vec![1, 2]]),
            // ---------------------------------------------------------------------------
            // A -> A (c)* b | a
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* (2560)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_left_rec (4)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(26), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0],                     //  0: A -> a A_2       | ►A_2 ◄0 a!      | a
                1 => symbols![nt 1, t 2],               //  1: A_1 -> c A_1     | ●A_1 ◄1 c!      | A_1 c
                2 => symbols![],                        //  2: A_1 -> ε         | ◄2              |
                3 => symbols![nt 0, nt 1, t 1],         //  3: A_2 -> A_1 b A_2 | ●A_2 ◄3 b! ►A_1 | A A_1 b
                4 => symbols![nt 0],                    //  4: A_2 -> ε         | ◄4              | A
            ], Default, btreemap![0 => vec![0]]),

            // A -> A (c)+ b | a
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* | plus (6656)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_rec (4)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (RTS(16), 0, btreemap![
                0 => "SynA".to_string(),
                1 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![t 0],                     //  0: A -> a A_2       | ►A_2 ◄0 a!      | a
                1 => symbols![],                        //  1: A_1 -> c A_3     | ►A_3 c!         |
                2 => symbols![nt 0, nt 1, t 1],         //  2: A_2 -> A_1 b A_2 | ●A_2 ◄2 b! ►A_1 | A A_1 b
                3 => symbols![nt 0],                    //  3: A_2 -> ε         | ◄3              | A
                4 => symbols![nt 1, t 2],               //  4: A_3 -> A_1       | ●A_1 ◄4         | A_1 c
                5 => symbols![nt 1, t 2],               //  5: A_3 -> ε         | ◄5              | A_1 c
            ], Default, btreemap![0 => vec![0]]),
            // --------------------------------------------------------------------------- misc
            // A -> a | a b b | a c c
            // NT flags:
            //  - A: parent_left_fact (32)
            //  - A_1: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            (PRS(35), 0, btreemap![
                0 => "SynA".to_string(),
            ], btreemap![
                0 => symbols![],                        //  0: A -> a A_1 | ►A_1 a!  |
                1 => symbols![t 0, t 1, t 1],           //  1: A_1 -> b b | ◄1 b! b! | a b b
                2 => symbols![t 0, t 2, t 2],           //  2: A_1 -> c c | ◄2 c! c! | a c c
                3 => symbols![t 0],                     //  3: A_1 -> ε   | ◄3       | a
            ], Default, btreemap![0 => vec![1, 2, 3]]),

            // A -> (B c)* b | a
            // B -> b
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - A_1: child_+_or_* (1)
            // parents:
            //  - A_1 -> A
            (RTS(33), 0, btreemap![
                0 => "SynMyA".to_string(),
                1 => "SynB".to_string(),
                2 => "SynA1".to_string(),
            ], btreemap![
                0 => symbols![nt 2, t 1],               //  0: A -> A_1 b     | ◄0 b! ►A_1    | A_1 b
                1 => symbols![t 0],                     //  1: A -> a         | ◄1 a!         | a
                2 => symbols![t 1],                     //  2: B -> b         | ◄2 b!         | b
                3 => symbols![nt 2, nt 1, t 2],         //  3: A_1 -> B c A_1 | ●A_1 ◄3 c! ►B | A_1 B c
                4 => symbols![],                        //  4: A_1 -> ε       | ◄4            |
            ], All, btreemap![0 => vec![0, 1], 1 => vec![2]]),
            // --------------------------------------------------------------------------- left_rec + amb
            // E -> E : E | E ^ E | E / E | E * E | E - E | E + E | F
            // F -> ( E ) | NUM | ID
            // NT flags:
            //  - E: parent_left_rec | parent_amb (1536)
            //  - E_1: child_left_rec | child_amb (12)
            // parents:
            //  - E_1 -> E
            /*
            (PRS(13), 0, btreemap![
                0 => symbols![nt 1],                    //  0: E -> F E_1     | ►E_1 ◄0 ►F   | F
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
            ], Default, btreemap![
            ]),
            */
            // (PRS(9), 0, btreemap![]),
            // (PRS(10), 0, btreemap![]),
            // (PRS(15), 0, btreemap![]),
            // (RTS(31), 0, btreemap![], Default),  // TODO: reports error, not supported, user must create NT for OR under + or *
            // ---------------------------------------------------------------------------
            // (RTS(100), 0, btreemap![], btreemap![], Default, btreemap![]),
            /*
            (PRS(), 0, btreemap![], btreemap![], Default, btreemap![]),
            (RTS(), 0, btreemap![], btreemap![], Default, btreemap![]),
            */
        ];

        const WRAPPER_FILENAME: &str = "tests/gen/wrapper_source.rs";

        // print sources
        const VERBOSE: bool = true;        // prints the `tests` values from the results (easier to set the other constants to false)
        const VERBOSE_TYPE: bool = false;   // prints the code module skeleton (easier to set the other constants to false)
        const PRINT_SOURCE: bool = true;   // prints the wrapper module (easier to set the other constants to false)

        // test options
        const TEST_SOURCE: bool = true;
        const TESTS_ALL: bool = true;

        // CAUTION! Setting this to 'true' modifies the validation file with the current result
        const REPLACE_SOURCE: bool = false;

        let mut num_errors = 0;
        let mut rule_id_iter = HashMap::<T, u32>::new();
        for (test_id, (rule_id, start_nt, nt_type, expected_items, has_value, expected_factors)) in tests.into_iter().enumerate() {
            let rule_iter = rule_id_iter.entry(rule_id).and_modify(|x| *x += 1).or_insert(1);

if !hashset![RTS(22), RTS(24)].contains(&rule_id) || *rule_iter != 1 { continue }

            if VERBOSE { println!("// {:=<80}\n// Test {test_id}: rules {rule_id:?} #{rule_iter}, start {start_nt}:", ""); }
            let ll1 = rule_id.get_prs(test_id, start_nt, true);
            let mut builder = ParserGen::from_rules(ll1, "Test".to_string());
            set_has_value(&mut builder, has_value.clone());
            if VERBOSE {
                println!("/*");
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
            let result_factors = (0..builder.parsing_table.num_nt).filter_map(|v|
                if builder.parsing_table.parent[v].is_none() { Some((v as VarId, builder.gather_factors(v as VarId))) } else { None }
            ).collect::<BTreeMap<_, _>>();
            let test_name = format!("wrapper source for rule {rule_id:?} #{rule_iter}, start {}", Symbol::NT(start_nt).to_str(builder.get_symbol_table()));
            let rule_name = match rule_id {
                RTS(n) => format!("rts_{n}_{rule_iter}"),
                PRS(n) => format!("prs_{n}_{rule_iter}"),
            };
            if !hashset!["rts_29_3", "rts_30_2"].contains(rule_name.as_str()) {
                builder.add_lib(&format!("super::super::wrapper_code::code_{rule_name}::*"));
            }
            for (v, s) in nt_type.clone() {
                builder.add_nt_type(v, s);
            }
            let wrapper_src = builder.source_wrapper();
            let mut src = vec![builder.source_use()];
            src.push(wrapper_src);
            if VERBOSE_TYPE {
                println!("pub(crate) mod code_{rule_name} {{");
                println!("    // {0:-<60}\n    // {test_name}", "");
                let st: &crate::symbol_table::SymbolTable = builder.get_symbol_table().unwrap();
                for v in 0..(st.get_num_nt() as VarId) {
                    if let Some((_s, src)) = builder.get_nt_extra_info(v) {
                        println!();
                        println!("{}", src.into_iter().map(|line| format!("    {line}")).join("\n"));
                    }
                }
                println!("}}\n");
            }
            let result_nt_type = builder.nt_type.iter().map(|(v, s)| (*v, s.clone())).collect::<BTreeMap<_, _>>();
            if VERBOSE {
                print_flags(&builder, 12);
                println!("            ({rule_id:?}, {start_nt}, btreemap![", );
                if !result_nt_type.is_empty() {
                    println!("{}", result_nt_type.iter().map(|(v, s)| format!("                {v} => \"{s}\".to_string(),")).join("\n"));
                }
                println!("            ], btreemap![");
                print_items(&builder, 16, true);
                let has_value_str = match &has_value {
                    Set(s) => format!("Set(symbols![{}])", s.iter().map(|s| symbol_to_macro(s)).join(", ")),
                    All => "All".to_string(),
                    Default => "Default".to_string()
                };
                println!("            ], {has_value_str}, btreemap![{}]),",
                    if result_factors.is_empty() { "".to_string() } else { result_factors.iter().map(|(v, factors)| format!("{v} => vec![{}]", factors.iter().join(", "))).join(", ") }
                );
                println!("*/");
            }
            let result_src = indent_source(src, 4);
            if PRINT_SOURCE {
                println!("pub(crate) mod rules_{rule_name} {{");
                println!("    // {0:-<60}\n    // [{test_name}]\n\n{result_src}\n    // [{test_name}]\n    // {:-<60}", "");
                println!("}}\n");
            }
            let expected_src = get_tagged_source(WRAPPER_FILENAME, &test_name);
            let err_msg = format!("test {test_id} {rule_id:?} #{rule_iter} failed ");
            if TESTS_ALL {
                if result_items != expected_items || result_factors != expected_factors || result_nt_type != nt_type {
                    num_errors += 1;
                    println!("## ERROR: {err_msg}");
                }
                if TEST_SOURCE && Some(&result_src) != expected_src.as_ref() {
                    if REPLACE_SOURCE {
                        replace_tagged_source(WRAPPER_FILENAME, &test_name, &result_src).expect("replacement failed");
                    }
                    num_errors += 1;
                    println!("## SOURCE MISMATCH: {err_msg}");
                }
            } else {
                assert_eq!(result_items, expected_items, "{err_msg}");
                assert_eq!(result_factors, expected_factors, "{err_msg}");
                assert_eq!(result_nt_type, nt_type, "{err_msg}");
                if TEST_SOURCE {
                    if REPLACE_SOURCE && expected_src.is_some() && &result_src != expected_src.as_ref().unwrap() {
                        replace_tagged_source(WRAPPER_FILENAME, &test_name, &result_src).expect("replacement failed");
                    }
                    assert_eq!(Some(result_src), expected_src, "{err_msg}");
                }
            }
        }
        if TESTS_ALL {
            assert_eq!(num_errors, 0, "{num_errors} tests have failed");
        }
    }

    #[test]
    fn expand_lfact() {
        let tests: Vec<(T, Vec<(&str, &str)>, BTreeMap<VarId, Vec<(VarId, FactorId)>>)> = vec![
            // A -> a (<L=AIter1> (<L=AIter2> b)* c)* d
            // NT flags:
            //  - A: parent_+_or_* (2048)
            //  - AIter2: child_+_or_* | L-form (129)
            //  - AIter1: child_+_or_* | L-form | parent_+_or_* (2177)
            // parents:
            //  - AIter2 -> AIter1
            //  - AIter1 -> A
            (RTS(39), vec![
                // now:
                // ("A -> a AIter1 d",           "A -> a (AIter2 c <L>)* d"),                                                 // 0: A -> a AIter1 d
                // ("AIter2 -> b AIter2",        "(b <L>)* iteration in AIter1 ->  ► (b <L>)* ◄  c (AIter2 c <L>)*"),         // 1: AIter2 -> b AIter2
                // ("AIter2 -> ε",               "end of (b <L>)* iterations in AIter1 ->  ► (b <L>)* ◄  c (AIter2 c <L>)*"), // 2: AIter2 -> ε
                // ("AIter1 -> AIter2 c AIter1", "AIter1 -> (b <L>)* c (AIter2 c <L>)*"),                                     // 3: AIter1 -> AIter2 c AIter1
                // ("AIter1 -> ε",               "AIter1 -> ε"),                                                              // 4: AIter1 -> ε

                // should be:
                ("A -> a AIter1 d",                 "A -> a (AIter2 c <L>)* d"),                                 // 0: A -> a AIter1 d
                ("AIter2 -> b AIter2",              "(b <L>)* iteration in ( (b <L>)* c <L>)*"),                 // 1: AIter2 -> b AIter2
                ("AIter2 -> ε",                     "end of (b <L>)* iterations in ( (b <L>)* c <L>)*"),         // 2: AIter2 -> ε
                ("AIter1 -> AIter2 c AIter1",       "(AIter2 c <L>)* iteration in a (AIter2 c <L>)* d"),         // 3: AIter1 -> AIter2 c AIter1
                ("AIter1 -> ε",                     "end of (AIter2 c <L>)* iterations in a (AIter2 c <L>)* d"), // 4: AIter1 -> ε
            ], btreemap![
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
            ]),
            // A -> A (c)* b | c
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* (2560)
            //  - A_1: child_+_or_* (1)
            //  - A_2: child_left_rec (4)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            (RTS(26), vec![
                // ParserBuilder::expand_lfact()    // ParserBuilder::full_factor_str()
                ("A -> a A_2",                      "A -> a"),                                   // 0: A -> a A_2
                ("A_1 -> c A_1",                    "[c]* item in A -> A  ► [c]* ◄  b"),         // 1: A_1 -> c A_1
                ("A_1 -> ε",                        "end of [c]* items in A -> A  ► [c]* ◄  b"), // 2: A_1 -> ε
                ("A_2 -> A_1 b A_2",                "A -> A [c]* b"),                            // 3: A_2 -> A_1 b A_2
                ("A_2 -> ε",                        "A -> ε (end of loop)"),                     // 4: A_2 -> ε
            ], btreemap![
                // ParserBuilder::get_top_factors()
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
            ]),
            // A -> A (c)+ b | c
            // NT flags:
            //  - A: parent_left_rec | parent_+_or_* | plus (6656)
            //  - A_1: child_+_or_* | parent_left_fact | plus (4129)
            //  - A_2: child_left_rec (4)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (RTS(16), vec![
                ("A -> a A_2",                      "A -> a"),                                   // 0: A -> a A_2
                ("A_1 -> c | c A_1",                "[c]+ item in A -> A  ► [c]+ ◄  b"),         // 1: A_1 -> c A_3
                ("A_2 -> A_1 b A_2",                "A -> A [c]+ b"),                            // 2: A_2 -> A_1 b A_2
                ("A_2 -> ε",                        "A -> ε (end of loop)"),                     // 3: A_2 -> ε
                ("A_3 -> A_1",                      "[c]+ item in A -> A  ► [c]+ ◄  b"),         // 4: A_3 -> A_1
                ("A_3 -> ε",                        "end of [c]+ items in A -> A  ► [c]+ ◄  b"), // 5: A_3 -> ε
            ], btreemap![
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
                3 => vec![(2, 0)],
            ]),
            // A -> (B c)* b | a
            // B -> b
            (RTS(33), vec![
                ("A -> A_1 b",                      "A -> [B c]* b"),                              // 0: A -> A_1 b
                ("A -> a",                          "A -> a"),                                     // 1: A -> a
                ("B -> b",                          "B -> b"),                                     // 2: B -> b
                ("A_1 -> B c A_1",                  "[B c]* item in A ->  ► [B c]* ◄  b"),         // 3: A_1 -> B c A_1
                ("A_1 -> ε",                        "end of [B c]* items in A ->  ► [B c]* ◄  b"), // 4: A_1 -> ε
            ], btreemap![
                0 => vec![],
                1 => vec![],
                2 => vec![(2, 0)],
            ]),
            // A -> a (a | c) (b <L>)* c
            (RTS(32), vec![
                ("A -> a a AIter1 c | a c AIter1 c",   "A -> a a (b <L>)* c | a c (b <L>)* c"),                          // 0: A -> a A_1
                ("AIter1 -> b AIter1",                 "(b <L>)* iteration in A -> a a  ► (b <L>)* ◄  c | ..."),         // 1: AIter1 -> b AIter1
                ("AIter1 -> ε",                        "end of (b <L>)* iterations in A -> a a  ► (b <L>)* ◄  c | ..."), // 2: AIter1 -> ε
                ("A_1 -> a AIter1 c",                  "A -> a a (b <L>)* c"),                                           // 3: A_1 -> a AIter1 c
                ("A_1 -> c AIter1 c",                  "A -> a c (b <L>)* c"),                                           // 4: A_1 -> c AIter1 c
            ], btreemap![
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
            ]),
            // A -> a | a b | a b c | a b d | e
            (PRS(28), vec![
                ("A -> a | a b | a b c | a b d",    "A -> a | a b | a b c | a b d"), // A -> a A_1
                ("A -> e",                          "A -> e"),                       // A -> e
                ("A_1 -> b | b c | b d",            "A -> a b | a b c | a b d"),     // A_1 -> b A_2
                ("A_1 -> ε",                        "A -> a"),                       // A_1 -> ε
                ("A_2 -> c",                        "A -> a b c"),                   // A_2 -> c
                ("A_2 -> d",                        "A -> a b d"),                   // A_2 -> d
                ("A_2 -> ε",                        "A -> a b"),                     // A_2 -> ε
            ], btreemap![
                0 => vec![],
                1 => vec![(1, 0)],
                2 => vec![(1, 0)],
            ]),
            // E -> F | E . id ; F -> id
            (PRS(31), vec![
                ("E -> F E_1",                      "E -> F"),                  // E -> F E_1
                ("F -> id",                         "F -> id"),                 // F -> id
                ("E_1 -> . id E_1",                 "E -> E . id"),             // E_1 -> . id E_1
                ("E_1 -> ε",                        "E -> ε (end of loop)"),    // E_1 -> ε
            ], btreemap![
                0 => vec![],
                1 => vec![],
                2 => vec![(2, 0)],
            ]),
            // A -> A a | b c | b d
            (PRS(33), vec![
                ("A -> b c A_1 | b d A_1",          "A -> b c | b d"),          // A -> b A_2
                ("A_1 -> a A_1",                    "A -> A a"),                // A_1 -> a A_1
                ("A_1 -> ε",                        "A -> ε (end of loop)"),    // A_1 -> ε
                ("A_2 -> c A_1",                    "A -> b c"),                // A_2 -> c A_1
                ("A_2 -> d A_1",                    "A -> b d"),                // A_2 -> d A_1
            ], btreemap![
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
            ]),
            // A -> A a | A b | b c | b d
            (PRS(38), vec![
                ("A -> b c A_1 | b d A_1",          "A -> b c | b d"),          // A -> b A_2
                ("A_1 -> a A_1",                    "A -> A a"),                // A_1 -> a A_1
                ("A_1 -> b A_1",                    "A -> A b"),                // A_1 -> b A_1
                ("A_1 -> ε",                        "A -> ε (end of loop)"),    // A_1 -> ε
                ("A_2 -> c A_1",                    "A -> b c"),                // A_2 -> c A_1
                ("A_2 -> d A_1",                    "A -> b d"),                // A_2 -> d A_1
            ], btreemap![
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
            ]),
            // A -> A a b | A a c | b c | b d
            // NT flags:
            //  - A: parent_left_fact | parent_left_rec (544)
            //  - A_1: child_left_rec | parent_left_fact (36)
            //  - A_2: child_left_fact (64)
            //  - A_3: child_left_fact (64)
            // parents:
            //  - A_1 -> A
            //  - A_2 -> A
            //  - A_3 -> A_1
            (PRS(39), vec![
                ("A -> b c A_1 | b d A_1",          "A -> b c | b d"),          // A -> b A_2
                ("A_1 -> a b A_1 | a c A_1",        "A -> A a b | A a c"),      // A_1 -> a A_3
                ("A_1 -> ε",                        "A -> ε (end of loop)"),    // A_1 -> ε
                ("A_2 -> c A_1",                    "A -> b c"),                // A_2 -> c A_1
                ("A_2 -> d A_1",                    "A -> b d"),                // A_2 -> d A_1
                ("A_3 -> b A_1",                    "A -> A a b"),              // A_3 -> b A_1
                ("A_3 -> c A_1",                    "A -> A a c"),              // A_3 -> c A_1
            ], btreemap![
                0 => vec![],
                1 => vec![(2, 0)],
                2 => vec![(2, 0)],
                3 => vec![(2, 0)],
            ]),
            /*
            (PRS(), vec![
            ], btreemap![
            ]),
            */
        ];
        const VERBOSE: bool = true;
        for (test_id, (rule_id, expected_expanded_full, expected_top_factors)) in tests.into_iter().enumerate() {
            let expected_expanded = expected_expanded_full.iter().map(|(a, _)| a.to_string()).to_vec();
            let expected_full = expected_expanded_full.iter().map(|(_, b)| b.to_string()).to_vec();
            let ll1 = rule_id.get_prs(test_id, 0, true);
            let builder = ParserGen::from_rules(ll1, "Test".to_string());
            let mut result_expanded = vec![];
            let mut result_full = vec![];
            for (f_id, (v, prod)) in builder.parsing_table.factors.iter().enumerate() {
                let mut expanded = vec![prod.symbols().clone()];
                builder.expand_lfact(&mut expanded);
                result_expanded.push(format!("{} -> {}",
                                             Symbol::NT(*v).to_str(builder.get_symbol_table()),
                                             expanded.iter().map(|fact| builder.factor_to_str(fact)).join(" | ")));
                result_full.push(format!("{}", builder.full_factor_str(f_id as FactorId, None, false)));
            }
            let mut result_top_factors = BTreeMap::<VarId, Vec<(VarId, FactorId)>>::new();
            for group in builder.nt_parent.iter().filter(|v| !v.is_empty()) {
                for v in group {
                    let mut top_factors = builder.get_top_factors(*v);
                    top_factors.sort();
                    result_top_factors.insert(*v, top_factors);
                }
            }
            if VERBOSE {
                println!("            ({rule_id:?}, vec![", );
                let cols = result_expanded.iter().zip(&result_full).enumerate()
                    .map(|(_i, (s, s_full))| {
                        // let (v, prod) = &builder.parsing_table.factors[i];
                        vec![
                            "".to_string(),
                            format!("(\"{s}\","),
                            format!("  \"{s_full}\"),"),
                            // format!("// {}", builder.ntfactor_to_str(*v, prod)),
                        ]
                    })
                    .to_vec();
                let lines = columns_to_str(cols, Some(vec![16, 34, 0]));
                let cols = lines.into_iter().enumerate().map(|(i, s)| {
                    let (v, prod) = &builder.parsing_table.factors[i];
                    vec![
                        s,
                        format!("// {i}: {}", builder.ntfactor_to_str(*v, prod)),
                    ]
                }).to_vec();
                let lines = columns_to_str(cols, Some(vec![80, 0]));
                println!("{}", lines.join("\n"));
                println!("            ], btreemap![");
                for (v, s) in &result_top_factors {
                    println!("                {v} => vec![{}],", s.iter().map(|&x| format!("{x:?}")).join(", "));
                }
                println!("            ]),");
            }
            let expected_expanded = expected_expanded.into_iter().map(|s| s.to_string()).to_vec();
            assert_eq!(result_expanded, expected_expanded, "Test {test_id}: {rule_id:?} failed");
            assert_eq!(result_full, expected_full, "Test {test_id}: {rule_id:?} failed");
            assert_eq!(result_top_factors, expected_top_factors, "Test {test_id}: {rule_id:?} failed");
        }
    }
}
