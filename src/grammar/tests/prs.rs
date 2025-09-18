// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use super::*;

#[test]
fn prs_remove_recursion() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            // A -> A b | A c | d | d e     A -> d A_1 | d e A_1
            // B -> A f | g | h             B -> A f | g | h
            //                              A_1 -> b A_1 | c A_1 | ε
            0 => prule!(t 3, nt 2; t 3, t 4, nt 2),
            1 => prule!(nt 0, t 5; t 6; t 7),
            2 => prule!(t 1, nt 2; t 2, nt 2; e),
        ]),
        (2, btreemap![]),
        (4, btreemap![
            // E -> E - T | E + T | T     E -> T E_1
            // T -> T / F | T * F | F     T -> F T_1
            // F -> ( E ) | N | I         F -> ( E ) | NUM | ID
            //                            E_1 -> + T E_1 | - T E_1 | ε
            //                            T_1 -> * F T_1 | / F T_1 | ε
            0 => prule!(nt 1, nt 3),
            1 => prule!(nt 2, nt 4),
            2 => prule!(t 4, nt 0, t 5; t 6; t 7),
            3 => prule!(t 0, nt 1, nt 3; t 1, nt 1, nt 3; e),
            4 => prule!(t 2, nt 2, nt 4; t 3, nt 2, nt 4; e),
        ]),
        (8, btreemap![
            // (0) A -> A_2 A_1
            // (1) A_1 -> a A_2 A_1 | ε
            // (2) A_2 -> b
            0 => prule!(nt 2, nt 1),
            1 => prule!(t 0, nt 2, nt 1; e),
            2 => prule!(t 1),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_prs(test_id, false);
        if VERBOSE {
            println!("{:=<80}\ntest {test_id}:", "");
            rules.print_rules(false, false);
        }
        rules.remove_recursion();
        let result = <BTreeMap<_, _>>::from(&rules);
        if VERBOSE {
            println!("=>");
            rules.print_rules(true, false);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(rules.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(rules.log.get_warnings().join("\n"), "", "test {test_id} failed");
        rules.remove_recursion();
        let result = <BTreeMap<_, _>>::from(&rules);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
    }
}

#[test]
fn prs_left_factorize() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            // A -> d A_1 | A A_2
            // B -> A f | g | h
            // A_1 -> e | ε
            // A_2 -> b | c
            0 => prule!(t 3, nt 2; nt 0, nt 3),
            1 => prule!(nt 0, t 5; t 6; t 7),
            2 => prule!(t 4; e),
            3 => prule!(t 1; t 2),
        ]),
        (1, btreemap![
            // A -> b A_1 | c b | d c A_2
            // A_1 -> b c A_3 | c g | d e A_4
            // A_2 -> e | ε
            // A_3 -> d | ε
            // A_4 -> f | g
            0 => prule!(t 1, nt 1; t 2, t 1; t 3, t 2, nt 2),
            1 => prule!(t 1, t 2, nt 3; t 2, t 6; t 3, t 4, nt 4),
            2 => prule!(t 4; e),
            3 => prule!(t 3; #R, e),
            4 => prule!(t 5; t 6),
        ]),
        (2, btreemap![]),
        (3, btreemap![
            // A -> a
            // B -> c B_1
            // C -> c | b
            // D -> d
            // B_1 -> a | ε
            0 => prule!(t 0),
            1 => prule!(t 2, nt 4),
            2 => prule!(t 2; t 1),
            3 => prule!(t 3),
            4 => prule!(t 0; e),
        ]),
        (4, btreemap![
            // E -> E E_1 | T
            // T -> T T_1 | F
            // F -> ( E ) | NUM | ID
            // E_1 -> + T | - T
            // T_1 -> * F | / F
            0 => prule!(nt 0, nt 3; nt 1),
            1 => prule!(nt 1, nt 4; nt 2),
            2 => prule!(t 4, nt 0, t 5; t 6; t 7),
            3 => prule!(t 0, nt 1; t 1, nt 1),
            4 => prule!(t 2, nt 2; t 3, nt 2),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
            rules.print_rules(false, false);
        }
        rules.left_factorize();
        let result = BTreeMap::<_, _>::from(&rules);
        if VERBOSE {
            println!("=>");
            rules.print_rules(true, false);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(rules.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(rules.log.get_warnings().join("\n"), "", "test {test_id} failed");
        rules.left_factorize();
        let result = BTreeMap::<_, _>::from(&rules);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
    }
}

#[test]
fn prs_ll1_from() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            // A -> d A_2
            // B -> A f | g | h
            // A_1 -> b A_1 | c A_1 | ε
            // A_2 -> e A_1 | A_1
            0 => prule!(t 3, nt 3),
            1 => prule!(nt 0, t 5; t 6; t 7),
            2 => prule!(t 1, nt 2; t 2, nt 2; e),
            3 => prule!(t 4, nt 2; nt 2),
        ]),
        (1, btreemap![
            // A -> b A_1 | c b | d c A_2
            // A_1 -> b c A_3 | c g | d e A_4
            // A_2 -> e | ε
            // A_3 -> d | ε
            // A_4 -> f | g
            0 => prule!(t 1, nt 1; t 2, t 1; t 3, t 2, nt 2),
            1 => prule!(t 1, t 2, nt 3; t 2, t 6; t 3, t 4, nt 4),
            2 => prule!(t 4; e),
            3 => prule!(t 3; #256, e),
            4 => prule!(t 5; t 6),
        ]),
        (4, btreemap![
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            0 => prule!(nt 1, nt 3),
            1 => prule!(nt 2, nt 4),
            2 => prule!(t 4, nt 0, t 5; t 6; t 7),
            3 => prule!(t 0, nt 1, nt 3; t 1, nt 1, nt 3; e),
            4 => prule!(t 2, nt 2, nt 4; t 3, nt 2, nt 4; e),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let rules_lr = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
            rules_lr.print_rules(false, false);
        }
        let ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&ll1);
        if VERBOSE {
            println!("=>");
            ll1.print_rules(true, false);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
        let rules_ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&rules_ll1);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
   }
}

#[test]
#[should_panic]
/// We test that the code compiles, but it must also panic because the `remove_ambiguity()`
/// method isn't implemented yet for LR grammars.
fn prs_lr_from() {
    let mut test_id = 0;
    loop {
        let rules = build_prs(test_id, false);
        if rules.prules.is_empty() {
            break;
        }
        let _lr = ProdRuleSet::<LR>::build_from(rules);
        test_id += 1;
    }
}

#[test]
fn prs_calc_first() {
    let tests: Vec<(u32, VarId, HashMap<Symbol, HashSet<Symbol>>)> = vec![
        (4, 0, hashmap![
            // E -> E + T | E - T | T
            // T -> T * F | T / F | F
            // F -> ( E ) | NUM | ID
            // ->
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            //
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F, 3:E_1, 4:T_1
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(nt 0) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 1) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 2) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 3) => hashset![sym!(t 0), sym!(t 1), sym!(e)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(e)],
        ]),
        (5, 0, hashmap![
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 1), sym!(t 2)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(e)],
            sym!(nt 2) => hashset![sym!(t 1), sym!(e)]
        ]),
        (43, 0, hashmap![
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 2), sym!(e)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(t 2)],
            sym!(nt 2) => hashset![sym!(t 2), sym!(t 5), sym!(t 6)],
            sym!(nt 3) => hashset![sym!(t 2), sym!(t 5), sym!(t 6)],
            sym!(e) => hashset![sym!(e)],
        ]),
        (51, 0, hashmap![
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(t 8) => hashset![sym!(t 8)],
            sym!(t 9) => hashset![sym!(t 9)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 1) => hashset![sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 2) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 9), sym!(e)],
            sym!(nt 3) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(t 9), sym!(e)],
            sym!(nt 5) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(nt 6) => hashset![sym!(t 2), sym!(t 9), sym!(e)],
            sym!(nt 7) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(t 7), sym!(t 8)],
            sym!(e) => hashset![sym!(e)],
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, start, expected) in tests {
        let rules_lr = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
        }
        let mut ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        if VERBOSE {
            ll1.print_rules(false, false);
            let b = map_and_print_first(&first, ll1.get_symbol_table());
            for (sym, set) in &b {
                println!("            sym!({}) => hashset![{}],", sym.to_macro_item(),
                         set.iter().map(|s| format!("sym!({})", s.to_macro_item())).join(", "));
            }
        }
        assert_eq!(first, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
   }
}

#[test]
fn prs_calc_follow() {
    let tests: Vec<(u32, VarId, HashMap<Symbol, HashSet<Symbol>>)> = vec![
        (4, 0, hashmap![
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            //
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F, 3:E_1, 4:T_1
            sym!(nt 0) => hashset![sym!(t 5), sym!(end)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(end)],
            sym!(nt 2) => hashset![sym!(t 0), sym!(t 1), sym!(t 2), sym!(t 3), sym!(t 5), sym!(end)],
            sym!(nt 3) => hashset![sym!(t 5), sym!(end)],
            sym!(nt 4) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(end)],
        ]),
        (5, 0, hashmap![
            sym!(nt 0) => hashset![sym!(end)],
            sym!(nt 1) => hashset![sym!(t 1), sym!(t 2)],
            sym!(nt 2) => hashset![sym!(t 2)],
        ]),
        (43, 0, hashmap![
            sym!(nt 0) => hashset![sym!(end)],
            sym!(nt 1) => hashset![sym!(t 7)],
            sym!(nt 2) => hashset![sym!(t 1), sym!(t 3)],
            sym!(nt 3) => hashset![sym!(t 1), sym!(t 3), sym!(t 4)],
        ]),
        (51, 0, hashmap![
            sym!(nt 0) => hashset![sym!(t 6), sym!(end)],
            sym!(nt 1) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 2) => hashset![sym!(t 6), sym!(end)],
            sym!(nt 3) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 5) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 6) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
            sym!(nt 7) => hashset![sym!(t 2), sym!(t 3), sym!(t 4), sym!(t 6), sym!(t 9), sym!(end)],
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, start, expected) in tests {
        let rules_lr = build_prs(test_id, false);
        if VERBOSE {
            println!("test {test_id}:");
        }
        let mut ll1 = ProdRuleSet::<LL1>::build_from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        if VERBOSE {
            ll1.print_rules(false, false);
            let b = map_and_print_follow(&follow, ll1.get_symbol_table());
            for (sym, set) in &b {
                println!("            sym!({}) => hashset![{}],", sym.to_macro_item(),
                         set.iter().map(|s| format!("sym!({})", s.to_macro_item())).join(", "));
            }
        }
        assert_eq!(follow, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
   }
}

#[test]
fn prs_grammar_notes() {
    let tests: Vec<(T, VarId, Vec<&str>, Vec<&str>)> = vec![
        //        warnings                                  errors
        //        -------------------------------------     -------------------------------------
        (T::PRS(1000), 0, vec![],                           vec!["recursive rules must have at least one independent alternative"]),
        (T::PRS(1002), 0, vec!["ambiguity for NT"],         vec![]),
        (T::PRS(1003), 0, vec![],                           vec!["no terminal in grammar"]),
        (T::PRS(1004), 0, vec![],                           vec!["no terminal used in the table"]),
        (T::PRS(1005), 0, vec!["unused nonterminals",
                               "unused terminals"],         vec![]),
        (T::PRS(1006), 0, vec![],                           vec!["there are 2 rules but the symbol table has 1 nonterminal symbols: dropping the table"]),
        (T::RTS(101), 0,  vec![],                           vec!["there are 2 rules but the symbol table has 1 nonterminal symbols: dropping the table"]),
        (T::RTS(500), 0, vec![],                            vec!["in A, (<L=AIter1> a <L=AIter2>)*: conflicting <L=AIter1> and <L=AIter2>",
                                                                 "normalize_var(AIter2): error while normalizing the rules, 0 remaining nodes instead of 1"]),
        (T::RTS(501), 0, vec![],                            vec!["in A, (<L=AIter1> a | <L=AIter2> b)*: conflicting <L=AIter1> and <L=AIter2>",
                                                                 "normalize_var(AIter2): error while normalizing the rules, 0 remaining nodes instead of 1"]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, expected_warnings, expected_errors)) in tests.into_iter().enumerate() {
        if VERBOSE {
            println!("{:=<80}\ntest {test_id} with {ll_id:?}/{start}:", "");
        }
        let mut ll1 = ll_id.try_build_prs(start, false);
        if VERBOSE {
            ll1.print_rules(false, false);
            ll1.print_logs();
        }
        let mut parsing_table = None;
        if ll1.log.num_errors() == 0 {
            let first = ll1.calc_first();
            if ll1.log.num_errors() == 0 {
                let follow = ll1.calc_follow(&first);
                if ll1.log.num_errors() == 0 {
                    parsing_table = Some(ll1.calc_table(&first, &follow, false));
                }
            }
            if VERBOSE {
                println!("=>");
                ll1.print_rules(false, false);
                if let Some(table) = &parsing_table {
                    print_alts(&table.alts, ll1.get_symbol_table());
                    println!("table:");
                    table.print(ll1.get_symbol_table(), 12);
                }
                ll1.print_logs();
            }
        }
        assert_eq!(ll1.log.num_errors(), expected_errors.len(), "test {test_id}/{ll_id:?}/{start} failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings.len(), "test {test_id}/{ll_id:?}/{start} failed on # warnings");
        let err_discr = ll1.log.get_errors().zip(expected_errors).filter_map(|(e, ee)|
            if !e.contains(ee) { Some(format!("- \"{e}\" doesn't contain \"{ee}\"")) } else { None }
        ).to_vec();
        assert!(err_discr.is_empty(), "test {test_id}/{ll_id:?}/{start} has discrepancies in the expected error messages:\n{}", err_discr.join("\n"));
        let warn_discr = ll1.log.get_warnings().zip(expected_warnings).filter_map(|(w, ew)|
            if !w.contains(ew) { Some(format!("- \"{w}\" doesn't contain \"{ew}\"")) } else { None }
        ).to_vec();
        assert!(warn_discr.is_empty(), "test {test_id}/{ll_id:?}/{start} has discrepancies in the expected warning messages:\n{}", warn_discr.join("\n"));
   }
}

#[test]
fn prs_calc_table() {
    let tests: Vec<(T, VarId, usize, Vec<(VarId, Alternative)>, Vec<AltId>)> = vec![
        (T::PRS(4), 0, 0, vec![
            // E -> E + T | E - T | T
            // T -> T * F | T / F | F
            // F -> ( E ) | NUM | ID
            // - 0: E -> T E_1
            // - 1: T -> F T_1
            // - 2: F -> ( E )
            // - 3: F -> NUM
            // - 4: F -> ID
            // - 5: E_1 -> + T E_1
            // - 6: E_1 -> - T E_1
            // - 7: E_1 -> ε
            // - 8: T_1 -> * F T_1
            // - 9: T_1 -> / F T_1
            // - 10: T_1 -> ε
            (0, alt!(nt 1, nt 3)),
            (1, alt!(nt 2, nt 4)),
            (2, alt!(t 4, nt 0, t 5)),
            (2, alt!(t 6)),
            (2, alt!(t 7)),
            (3, alt!(t 0, nt 1, nt 3)),
            (3, alt!(t 1, nt 1, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 2, nt 2, nt 4)),
            (4, alt!(t 3, nt 2, nt 4)),
            (4, alt!(e)),
        ], vec![
            //     |  -   +   /   *   (   )   N   I   $
            // ----+-------------------------------------
            // E   |  .   .   .   .   0   p   0   0   p
            // T   |  p   p   .   .   1   p   1   1   p
            // F   |  p   p   p   p   2   p   3   4   p
            // E_1 |  5   6   .   .   .   7   .   .   7
            // T_1 | 10  10   8   9   .  10   .   .  10
             11,  11,  11,  11,   0,  12,   0,   0,  12,
             12,  12,  11,  11,   1,  12,   1,   1,  12,
             12,  12,  12,  12,   2,  12,   3,   4,  12,
              5,   6,  11,  11,  11,   7,  11,  11,   7,
             10,  10,   8,   9,  11,  10,  11,  11,  10,
        ]),
        (T::PRS(5), 0, 0, vec![
            // - 0: A -> A1 A2 ; ;
            // - 1: A1 -> - A1
            // - 2: A1 -> ε
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, alt!(nt 1, nt 2, t 2, t 2)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(e)),
        ], vec![
            //    |  -   +   ;   $
            // ---+-----------------
            // A  |  0   0   0   p
            // A1 |  1   2   2   .
            // A2 |  .   3   4   .
              0,   0,   0,   6,
              1,   2,   2,   5,
              5,   3,   4,   5,
        ]),
        (T::PRS(6), 1, 0, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: A -> A1 A2 ;
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, alt!(t 0, nt 0)),
            (0, alt!(e)),
            (1, alt!(nt 0, nt 2, t 2)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(e)),
        ], vec![
            //    |  -   +   ;   $
            // ---+-----------------
            // A1 |  0   1   1   .
            // A  |  2   2   2   p
            // A2 |  .   3   4   .
              0,   1,   1,   5,
              2,   2,   2,   6,
              5,   3,   4,   5,
        ]),
        (T::PRS(7), 1, 0, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: X -> A
            // - 3: A -> A1 A2 ;
            // - 4: A2 -> + A2
            // - 5: A2 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(e)),
            (1, alt!(t 0, nt 2)),
            (2, alt!(nt 0, nt 3, t 3)),
            (3, alt!(t 2, nt 3)),
            (3, alt!(e)),
        ], vec![
            //    |  >   -   +   ;   $
            // ---+---------------------
            // A1 |  .   0   1   1   .
            // X  |  2   .   .   .   p
            // A  |  .   3   3   3   p
            // A2 |  .   .   4   5   .
              6,   0,   1,   1,   6,
              2,   6,   6,   6,   7,
              6,   3,   3,   3,   7,
              6,   6,   4,   5,   6,
        ]),
        (T::PRS(7), 2, 2, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: A -> A1 A2 ;
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(e)),
            (1, alt!(nt 0, nt 2, t 3)),
            (2, alt!(t 2, nt 2)),
            (2, alt!(e)),
        ], vec![
            //    |  >   -   +   ;   $
            // ---+---------------------
            // A1 |  .   0   1   1   .
            // A  |  .   2   2   2   p
            // A2 |  .   .   3   4   .
              5,   0,   1,   1,   5,
              5,   2,   2,   2,   6,
              5,   5,   3,   4,   5,
        ]),
        (T::PRS(8), 0, 0, vec![
            // A -> A a A | b
            // - 0: A -> A_2 A_1
            // - 1: A_1 -> a A_2 A_1
            // - 2: A_1 -> ε
            // - 3: A_2 -> b
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1)),
        ], vec![
            //     |  a   b   $
            // ----+-------------
            // A   |  .   0   p
            // A_1 |  1   .   2
            // A_2 |  p   3   p
              4,   0,   5,
              1,   4,   2,
              5,   3,   5,
        ]),
        (T::PRS(14), 0, 0, vec![
            // - 0: A -> A_2 A_1
            // - 1: A_1 -> A_2 A_1
            // - 2: A_1 -> ε
            // - 3: A_2 -> a
            (0, alt!(nt 2, nt 1)),
            (1, alt!(nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 0)),
        ], vec![
            //     |  a   $
            // ----+---------
            // A   |  0   p
            // A_1 |  1   2
            // A_2 |  3   p
              0,   5,
              1,   2,
              3,   5,
        ]),
        (T::PRS(17), 0, 0, vec![
            // - 0: A -> B
            // - 1: A -> a
            // - 2: B -> C )
            // - 3: C -> ( A
            (0, alt!(nt 1)),
            (0, alt!(t 0)),
            (1, alt!(nt 2, t 2)),
            (2, alt!(t 1, nt 0)),
        ], vec![
            //   |  a   (   )   $
            // --+-----------------
            // A |  1   0   p   p
            // B |  .   2   p   p
            // C |  .   3   p   .
              1,   0,   5,   5,
              4,   2,   5,   5,
              4,   3,   5,   4,
        ]),
        (T::PRS(18), 0, 0, vec![
            // - 0: A -> a
            (0, alt!(t 0)),
        ], vec![
            //   |  a   $
            // --+---------
            // A |  0   p
              0,   2,
        ]),
        (T::PRS(19), 0, 0, vec![
            // - 0: A -> a
            (0, alt!(t 0)),
            (0, alt!(e)),
        ], vec![
            //   |   a   $
            // --+---------
            // A |   0   1
              0,   1,
        ]),
        (T::PRS(20), 0, 0, vec![
            // - 0: STRUCT -> struct id { LIST
            // - 1: LIST -> id : id ; LIST
            // - 2: LIST -> }
            (0, alt!(t 0, t 5, t 1, nt 1)),
            (1, alt!(t 5, t 3, t 5, t 4, nt 1)),
            (1, alt!(t 2)),
        ], vec![
            //        | struct  {   }   :   ;  id   $
            // -------+--------------------------------
            // STRUCT |   0     .   .   .   .   .   p
            // LIST   |   .     .   2   .   .   1   p
              0,   3,   3,   3,   3,   3,   4,
              3,   3,   2,   3,   3,   1,   4,
        ]),
/*
        (22, 0, 0, vec![
        ], vec![
        ]),
        (T::PRS(23), 0, 0, vec![
        ], vec![
        ]),
*/
        (T::PRS(24), 0, 0, vec![
            // - 0: A -> a B d
            // - 1: A -> e
            // - 2: B -> b c B_1
            // - 3: B_1 -> B
            // - 4: B_1 -> ε
            (0, alt!(t 0, nt 1, t 3)),
            (0, alt!(t 4)),
            (1, alt!(t 1, t 2, nt 2)),
            (2, alt!(nt 1)),
            (2, alt!(e)),
        ], vec![
            //     |  a   b   c   d   e   $
            // ----+-------------------------
            // A   |  0   .   .   .   1   p
            // B   |  .   2   .   p   .   .
            // B_1 |  .   3   .   4   .   .
              0,   5,   5,   5,   1,   6,
              5,   2,   5,   6,   5,   5,
              5,   3,   5,   4,   5,   5,
        ]),
        (T::PRS(25), 0, 0, vec![
            // A -> A a b c | A a b d | A a e | f
            // - 0: A -> f A_1
            // - 1: A_1 -> a A_2
            // - 2: A_1 -> ε
            // - 3: A_2 -> b A_3
            // - 4: A_2 -> e A_1
            // - 5: A_3 -> c A_1
            // - 6: A_3 -> d A_1
            (0, alt!(t 5, nt 1)),
            (1, alt!(t 0, nt 2)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 3)),
            (2, alt!(t 4, nt 1)),
            (3, alt!(t 2, nt 1)),
            (3, alt!(t 3, nt 1)),
        ], vec![
            //     |  a   b   c   d   e   f   $
            // ----+-----------------------------
            // A   |  .   .   .   .   .   0   p
            // A_1 |  1   .   .   .   .   .   2
            // A_2 |  .   3   .   .   4   .   p
            // A_3 |  .   .   5   6   .   .   p
              7,   7,   7,   7,   7,   0,   8,
              1,   7,   7,   7,   7,   7,   2,
              7,   3,   7,   7,   4,   7,   8,
              7,   7,   5,   6,   7,   7,   8,
        ]),
        (T::PRS(27), 0, 0, vec![
            // A -> A a | A b | c | d
            // - 0: A -> c A_1
            // - 1: A -> d A_1
            // - 2: A_1 -> a A_1
            // - 3: A_1 -> b A_1
            // - 4: A_1 -> ε
            (0, alt!(t 2, nt 1)),
            (0, alt!(t 3, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(t 1, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  a   b   c   d   $
            // ----+---------------------
            // A   |  .   .   0   1   p
            // A_1 |  2   3   .   .   4
              5,   5,   0,   1,   6,
              2,   3,   5,   5,   4,
        ]),
        (T::PRS(28), 0, 0, vec![
            // - 0: A -> a A_1
            // - 1: A -> e
            // - 2: A_1 -> b A_2
            // - 3: A_1 -> ε
            // - 4: A_2 -> c
            // - 5: A_2 -> d
            // - 6: A_2 -> ε
            (0, alt!(t 0, nt 1)),
            (0, alt!(t 4)),
            (1, alt!(t 1, nt 2)),
            (1, alt!(e)),
            (2, alt!(t 2)),
            (2, alt!(t 3)),
            (2, alt!(e)),
        ], vec![
            //     |  a   b   c   d   e   $
            // ----+-------------------------
            // A   |  0   .   .   .   1   p
            // A_1 |  .   2   .   .   .   3
            // A_2 |  .   .   4   5   .   6
              0,   7,   7,   7,   1,   8,
              7,   2,   7,   7,   7,   3,
              7,   7,   4,   5,   7,   6,
        ]),
        (T::PRS(43), 0, 0, vec![
            // BATCH -> GROUP ';' BATCH <L> | ε
            // GROUP -> '[' EXPR ']' | '(' EXPR ')'
            // EXPR -> FACTOR '*' FACTOR;
            // FACTOR -> id | int | '(' EXPR ')';
            //
            // first:                   follow:
            // -----------------------------------------
            // [ => [                   BATCH => $
            // ] => ]                   GROUP => $
            // ( => (                   EXPR => ] )
            // ) => )                   FACTOR => ] ) *
            // * => *
            // id => id
            // int => int
            // ; => ;
            // BATCH => [ ( ε
            // GROUP => [ (
            // EXPR => ( id int
            // FACTOR => ( id int
            // ε => ε
            //
            (0, alt!(nt 1, t 7, nt 0)),   // - 0: BATCH -> GROUP ; BATCH
            (0, alt!(e)),                 // - 1: BATCH -> ε
            (1, alt!(t 0, nt 2, t 1)),    // - 2: GROUP -> [ EXPR ]
            (1, alt!(t 2, nt 2, t 3)),    // - 3: GROUP -> ( EXPR )
            (2, alt!(nt 3, t 4, nt 3)),   // - 4: EXPR -> FACTOR * FACTOR
            (3, alt!(t 5)),               // - 5: FACTOR -> id
            (3, alt!(t 6)),               // - 6: FACTOR -> int
            (3, alt!(t 2, nt 2, t 3)),    // - 7: FACTOR -> ( EXPR )
        ], vec![
            //        |  [   ]   (   )   *  id  int  ;   $
            // -------+-------------------------------------
            // BATCH  |  0   .   0   .   .   .   .   .   1
            // GROUP  |  2   .   3   .   .   .   .   p   .
            // EXPR   |  .   p   4   p   .   4   4   .   .
            // FACTOR |  .   p   7   p   p   5   6   .   .
              0,   8,   0,   8,   8,   8,   8,   8,   1,
              2,   8,   3,   8,   8,   8,   8,   9,   8,
              8,   9,   4,   9,   8,   4,   4,   8,   8,
              8,   9,   7,   9,   9,   5,   6,   8,   8,
        ]),
        (T::PRS(51), 0, 0, vec![
            // E -> abs E | E ^ E | E ' | E * E | - E | E + E | F
            // F -> ( E ) | NUM | ID
            // - 0: E -> E_3 E_b
            // - 1: F -> ( E )
            // - 2: F -> NUM
            // - 3: F -> ID
            // - 4: E_b -> ^ E_3 E_b
            // - 5: E_b -> ' E_b
            // - 6: E_b -> * E_2 E_b
            // - 7: E_b -> + E_1 E_b
            // - 8: E_b -> ε
            // - 9: E_1 -> E_3 E_1b
            // - 10: E_1b -> ^ E_3 E_1b
            // - 11: E_1b -> ' E_1b
            // - 12: E_1b -> * E_2 E_1b
            // - 13: E_1b -> ε
            // - 14: E_2 -> E_3 E_2b
            // - 15: E_2b -> ^ E_3 E_2b
            // - 16: E_2b -> ' E_2b
            // - 17: E_2b -> ε
            // - 18: E_3 -> - E_1
            // - 19: E_3 -> abs E_3
            // - 20: E_3 -> F
            (0, alt!(nt 7, nt 2)),
            (1, alt!(t 5, nt 0, t 6)),
            (1, alt!(t 7)),
            (1, alt!(t 8)),
            (2, alt!(t 2, nt 7, nt 2)),
            (2, alt!(t 9, nt 2)),
            (2, alt!(t 3, nt 5, nt 2)),
            (2, alt!(t 4, nt 3, nt 2)),
            (2, alt!(e)),
            (3, alt!(nt 7, nt 4)),
            (4, alt!(t 2, nt 7, nt 4)),
            (4, alt!(t 9, nt 4)),
            (4, alt!(t 3, nt 5, nt 4)),
            (4, alt!(e)),
            (5, alt!(nt 7, nt 6)),
            (6, alt!(t 2, nt 7, nt 6)),
            (6, alt!(t 9, nt 6)),
            (6, alt!(e)),
            (7, alt!(t 1, nt 3)),
            (7, alt!(t 0, nt 7)),
            (7, alt!(nt 1)),
        ], vec![
            //      | abs  -   ^   *   +   (   )  NUM ID   '   $
            // -----+---------------------------------------------
            // E    |  0   0   .   .   .   0   p   0   0   .   p
            // F    |  .   .   p   p   p   1   p   2   3   p   p
            // E_b  |  .   .   4   6   7   .   8   .   .   5   8
            // E_1  |  9   9   p   p   p   9   p   9   9   p   p
            // E_1b |  .   .  10  12  13   .  13   .   .  11  13
            // E_2  | 14  14   p   p   p  14   p  14  14   p   p
            // E_2b |  .   .  15  17  17   .  17   .   .  16  17
            // E_3  | 19  18   p   p   p  20   p  20  20   p   p
              0,   0,  21,  21,  21,   0,  22,   0,   0,  21,  22,
             21,  21,  22,  22,  22,   1,  22,   2,   3,  22,  22,
             21,  21,   4,   6,   7,  21,   8,  21,  21,   5,   8,
              9,   9,  22,  22,  22,   9,  22,   9,   9,  22,  22,
             21,  21,  10,  12,  13,  21,  13,  21,  21,  11,  13,
             14,  14,  22,  22,  22,  14,  22,  14,  14,  22,  22,
             21,  21,  15,  17,  17,  21,  17,  21,  21,  16,  17,
             19,  18,  22,  22,  22,  20,  22,  20,  20,  22,  22,
        ]),
        (T::PRS(52), 0, 0, vec![
            // - 0: E -> E_4 E_1
            // - 1: F -> NUM
            // - 2: F -> ID
            // - 3: E_1 -> * E_4 E_1
            // - 4: E_1 -> ! E_1
            // - 5: E_1 -> ' E_1
            // - 6: E_1 -> + E_2 E_1
            // - 7: E_1 -> ε
            // - 8: E_2 -> E_4 E_3
            // - 9: E_3 -> <G> * E_4 E_3
            // - 10: E_3 -> <G> ! E_3
            // - 11: E_3 -> <G> ' E_3
            // - 12: E_3 -> ε
            // - 13: E_4 -> F
            (0, alt!(nt 5, nt 2)),
            (1, alt!(t 4)),
            (1, alt!(t 5)),
            (2, alt!(t 0, nt 5, nt 2)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(t 2, nt 2)),
            (2, alt!(t 3, nt 3, nt 2)),
            (2, alt!(e)),
            (3, alt!(nt 5, nt 4)),
            (4, alt!(t 0, nt 5, nt 4)),
            (4, alt!(t 1, nt 4)),
            (4, alt!(t 2, nt 4)),
            (4, alt!(e)),
            (5, alt!(nt 1)),
        ], vec![
            //     |  *   !   '   +  NUM ID   $
            // ----+-----------------------------
            // E   |  .   .   .   .   0   0   p
            // F   |  p   p   p   p   1   2   p
            // E_1 |  3   4   5   6   .   .   7
            // E_2 |  p   p   p   p   8   8   p
            // E_3 |  9  10  11  12   .   .  12
            // E_4 |  p   p   p   p  13  13   p
             14,  14,  14,  14,   0,   0,  15,
             15,  15,  15,  15,   1,   2,  15,
              3,   4,   5,   6,  14,  14,   7,
             15,  15,  15,  15,   8,   8,  15,
              9,  10,  11,  12,  14,  14,  12,
             15,  15,  15,  15,  13,  13,  15,
        ]),
        (T::PRS(53), 0, 0, vec![
            // E -> <R>E ^ E | <R>E * E | - E | E + E | F
            // F -> ID | NUM | ( E )
            // - 0: E -> E_3 E_b
            // - 1: F -> ID
            // - 2: F -> NUM
            // - 3: F -> ( E )
            // - 4: E_b -> ^ E_2 E_b     R-assoc (256)
            // - 5: E_b -> * E_1 E_b     R-assoc (256)
            // - 6: E_b -> + E_1 E_b
            // - 7: E_b -> ε
            // - 8: E_1 -> E_3 E_1b
            // - 9: E_1b -> ^ E_2 E_1b     R-assoc
            // - 10: E_1b -> * E_1 E_1b     R-assoc
            // - 11: E_1b -> ε
            // - 12: E_2 -> E_3 E_2b
            // - 13: E_2b -> ^ E_2 E_2b     R-assoc
            // - 14: E_2b -> ε
            // - 15: E_3 -> - E_1
            // - 16: E_3 -> F
            (0, alt!(nt 7, nt 2)),
            (1, alt!(t 4)),
            (1, alt!(t 5)),
            (1, alt!(t 6, nt 0, t 7)),
            (2, alt!(#R, t 0, nt 5, nt 2)),
            (2, alt!(#R, t 1, nt 3, nt 2)),
            (2, alt!(t 3, nt 3, nt 2)),
            (2, alt!(e)),
            (3, alt!(nt 7, nt 4)),
            (4, alt!(#R, t 0, nt 5, nt 4)),
            (4, alt!(#R, t 1, nt 3, nt 4)),
            (4, alt!(e)),
            (5, alt!(nt 7, nt 6)),
            (6, alt!(#R, t 0, nt 5, nt 6)),
            (6, alt!(e)),
            (7, alt!(t 2, nt 3)),
            (7, alt!(nt 1)),
        ], vec![
            //      |  ^   *   -   +  ID  NUM  (   )   $
            // -----+-------------------------------------
            // E    |  .   .   0   .   0   0   0   p   p
            // F    |  p   p   .   p   1   2   3   p   p
            // E_b  |  4   5   .   6   .   .   .   7   7
            // E_1  |  p   p   8   p   8   8   8   p   p
            // E_1b |  9  10   .  11   .   .   .  11  11
            // E_2  |  p   p  12   p  12  12  12   p   p
            // E_2b | 13  14   .  14   .   .   .  14  14
            // E_3  |  p   p  15   p  16  16  16   p   p
             17,  17,   0,  17,   0,   0,   0,  18,  18,
             18,  18,  17,  18,   1,   2,   3,  18,  18,
              4,   5,  17,   6,  17,  17,  17,   7,   7,
             18,  18,   8,  18,   8,   8,   8,  18,  18,
              9,  10,  17,  11,  17,  17,  17,  11,  11,
             18,  18,  12,  18,  12,  12,  12,  18,  18,
             13,  14,  17,  14,  17,  17,  17,  14,  14,
             18,  18,  15,  18,  16,  16,  16,  18,  18,
        ]),
        (T::PRS(54), 0, 0, vec![
            // E -> <R>E * E | E ! | E -- | <R>E + E | ID | NUM
            // - 0: E -> E_2 E_b
            // - 1: E_b -> * E_1 E_b     R-assoc
            // - 2: E_b -> ! E_b
            // - 3: E_b -> -- E_b
            // - 4: E_b -> + E E_b     R-assoc
            // - 5: E_b -> ε
            // - 6: E_1 -> E_2 E_1b
            // - 7: E_1b -> * E_1 E_1b     R-assoc
            // - 8: E_1b -> ε
            // - 9: E_2 -> ID
            // - 10: E_2 -> NUM
            (0, alt!(nt 4, nt 1)),
            (1, alt!(#R, t 1, nt 2, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(#R, t 3, nt 0, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(#R, t 1, nt 2, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 4)),
            (4, alt!(t 5)),
        ], vec![
            //      |  !   *  --   +  ID  NUM  $
            // -----+-----------------------------
            // E    |  p   p   p   p   0   0   p
            // E_b  |  2   1   3   4   .   .   5
            // E_1  |  p   p   p   p   6   6   p
            // E_1b |  8   7   8   8   .   .   8
            // E_2  |  p   p   p   p   9  10   p
             12,  12,  12,  12,   0,   0,  12,
              2,   1,   3,   4,  11,  11,   5,
             12,  12,  12,  12,   6,   6,  12,
              8,   7,   8,   8,  11,  11,   8,
             12,  12,  12,  12,   9,  10,  12,
        ]),
        (T::PRS(55), 0, 0, vec![
            // E -> E * E | E -- | ! E | E + E | ID | NUM
            // - 0: E -> E_2 E_b
            // - 1: E_b -> * E_2 E_b
            // - 2: E_b -> -- E_b
            // - 3: E_b -> + E_1 E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_2 E_1b
            // - 6: E_1b -> * E_2 E_1b
            // - 7: E_1b -> -- E_1b
            // - 8: E_1b -> ε
            // - 9: E_2 -> ! E_1
            // - 10: E_2 -> ID
            // - 11: E_2 -> NUM
            (0, alt!(nt 4, nt 1)),
            (1, alt!(t 1, nt 4, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(t 3, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(t 1, nt 4, nt 3)),
            (3, alt!(t 2, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 0, nt 2)),
            (4, alt!(t 4)),
            (4, alt!(t 5)),
        ], vec![
            //      |  !   *  --   +  ID  NUM  $
            // -----+-----------------------------
            // E    |  0   .   .   .   0   0   p
            // E_b  |  .   1   2   3   .   .   4
            // E_1  |  5   p   p   p   5   5   p
            // E_1b |  .   6   7   8   .   .   8
            // E_2  |  9   p   p   p  10  11   p
              0,  12,  12,  12,   0,   0,  13,
             12,   1,   2,   3,  12,  12,   4,
              5,  13,  13,  13,   5,   5,  13,
             12,   6,   7,   8,  12,  12,   8,
              9,  13,  13,  13,  10,  11,  13,
        ]),
        (T::PRS(56), 0, 0, vec![
            // E -> E * E | ! E | E -- | E + E | ID | NUM
            // - 0: E -> E_3 E_b
            // - 1: E_b -> * E_3 E_b
            // - 2: E_b -> -- E_b
            // - 3: E_b -> + E_1 E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_3 E_1b
            // - 6: E_1b -> * E_3 E_1b
            // - 7: E_1b -> -- E_1b
            // - 8: E_1b -> ε
            // - 9: E_2 -> E_3 E_2b
            // - 10: E_2b -> * E_3 E_2b
            // - 11: E_2b -> ε
            // - 12: E_3 -> ! E_2
            // - 13: E_3 -> ID
            // - 14: E_3 -> NUM
            (0, alt!(nt 6, nt 1)),
            (1, alt!(t 1, nt 6, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(t 3, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 6, nt 3)),
            (3, alt!(t 1, nt 6, nt 3)),
            (3, alt!(t 2, nt 3)),
            (3, alt!(e)),
            (4, alt!(nt 6, nt 5)),
            (5, alt!(t 1, nt 6, nt 5)),
            (5, alt!(e)),
            (6, alt!(t 0, nt 4)),
            (6, alt!(t 4)),
            (6, alt!(t 5)),
        ], vec![
            //      |  !   *  --   +  ID  NUM  $
            // -----+-----------------------------
            // E    |  0   .   .   .   0   0   p
            // E_b  |  .   1   2   3   .   .   4
            // E_1  |  5   p   p   p   5   5   p
            // E_1b |  .   6   7   8   .   .   8
            // E_2  |  9   p   p   p   9   9   p
            // E_2b |  .  10  11  11   .   .  11
            // E_3  | 12   p   p   p  13  14   p
              0,  15,  15,  15,   0,   0,  16,
             15,   1,   2,   3,  15,  15,   4,
              5,  16,  16,  16,   5,   5,  16,
             15,   6,   7,   8,  15,  15,   8,
              9,  16,  16,  16,   9,   9,  16,
             15,  10,  11,  11,  15,  15,  11,
             12,  16,  16,  16,  13,  14,  16,
        ]),
        (T::PRS(57), 0, 0, vec![
            // E -> E ^ E | E * E | E + E | ID | NUM
            // - 0: E -> E_3 E_b
            // - 1: E_b -> ^ E_3 E_b
            // - 2: E_b -> * E_2 E_b
            // - 3: E_b -> + E_1 E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_3 E_1b
            // - 6: E_1b -> ^ E_3 E_1b
            // - 7: E_1b -> * E_2 E_1b
            // - 8: E_1b -> ε
            // - 9: E_2 -> E_3 E_2b
            // - 10: E_2b -> ^ E_3 E_2b
            // - 11: E_2b -> ε
            // - 12: E_3 -> ID
            // - 13: E_3 -> NUM
            (0, alt!(nt 6, nt 1)),
            (1, alt!(t 0, nt 6, nt 1)),
            (1, alt!(t 1, nt 4, nt 1)),
            (1, alt!(t 2, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 6, nt 3)),
            (3, alt!(t 0, nt 6, nt 3)),
            (3, alt!(t 1, nt 4, nt 3)),
            (3, alt!(e)),
            (4, alt!(nt 6, nt 5)),
            (5, alt!(t 0, nt 6, nt 5)),
            (5, alt!(e)),
            (6, alt!(t 3)),
            (6, alt!(t 4)),
        ], vec![
            //      |  ^   *   +  ID  NUM  $
            // -----+-------------------------
            // E    |  .   .   .   0   0   p
            // E_b  |  1   2   3   .   .   4
            // E_1  |  p   p   p   5   5   p
            // E_1b |  6   7   8   .   .   8
            // E_2  |  p   p   p   9   9   p
            // E_2b | 10  11  11   .   .  11
            // E_3  |  p   p   p  12  13   p
             14,  14,  14,   0,   0,  15,
              1,   2,   3,  14,  14,   4,
             15,  15,  15,   5,   5,  15,
              6,   7,   8,  14,  14,   8,
             15,  15,  15,   9,   9,  15,
             10,  11,  11,  14,  14,  11,
             15,  15,  15,  12,  13,  15,
        ]),
        (T::PRS(66), 0, 0, vec![
            // E -> E . * E | E -- | E . + E | ! E | ID
            // - 0: E -> E_4 E_1
            // - 1: E_1 -> <G> -- E_1
            // - 2: E_1 -> <G> . E_5
            // - 3: E_1 -> ε
            // - 4: E_2 -> E_4 E_3
            // - 5: E_3 -> <G> . * E_4 E_3
            // - 6: E_3 -> <G> -- E_3
            // - 7: E_3 -> ε
            // - 8: E_4 -> ! E
            // - 9: E_4 -> ID
            // - 10: E_5 -> <G> * E_4 E_1
            // - 11: E_5 -> <G> + E_2 E_1
            (0, alt!(nt 4, nt 1)),
            (1, alt!(t 1, nt 1)),
            (1, alt!(t 4, nt 5)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(t 4, t 0, nt 4, nt 3)),
            (3, alt!(t 1, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 3, nt 0)),
            (4, alt!(t 5)),
            (5, alt!(t 0, nt 4, nt 1)),
            (5, alt!(t 2, nt 2, nt 1)),
        ], vec![
            //     |  *  --   +   !   .  ID   $
            // ----+-----------------------------
            // E   |  .   p   .   0   p   0   p
            // E_1 |  .   1   .   .   2   .   3
            // E_2 |  .   p   .   4   p   4   p
            // E_3 |  .   6   .   .   5   .   7
            // E_4 |  .   p   .   8   p   9   p
            // E_5 | 10   p  11   .   p   .   p
             12,  13,  12,   0,  13,   0,  13,
             12,   1,  12,  12,   2,  12,   3,
             12,  13,  12,   4,  13,   4,  13,
             12,   6,  12,  12,   5,  12,   7,
             12,  13,  12,   8,  13,   9,  13,
             10,  13,  11,  12,  13,  12,  13,
        ]),
        (T::PRS(58), 0, 0, vec![
            // E -> E + | - E | 0
            // - 0: E -> - E
            // - 1: E -> 0 E_1
            // - 2: E_1 -> + E_1
            // - 3: E_1 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(t 2, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  .   0   1   p
            // E_1 |  2   .   .   3
              4,   0,   1,   5,
              2,   4,   4,   3,
        ]),
        (T::PRS(61), 0, 0, vec![
            // E -> E + | - E | 0 | 1
            // - 0: E -> - E
            // - 1: E -> 0 E_1
            // - 2: E -> 1 E_1
            // - 3: E_1 -> + E_1
            // - 4: E_1 -> ε
            (0, alt!(t 1, nt 0)),
            (0, alt!(t 2, nt 1)),
            (0, alt!(t 3, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  +   -   0   1   $
            // ----+---------------------
            // E   |  .   0   1   2   p
            // E_1 |  3   .   .   .   4
              5,   0,   1,   2,   6,
              3,   5,   5,   5,   4,
        ]),
        (T::PRS(70), 0, 0, vec![
            // E -> - E | E + | 0
            // - 0: E -> E_1 E_b
            // - 1: E_b -> + E_b
            // - 2: E_b -> ε
            // - 3: E_1 -> - E_1
            // - 4: E_1 -> 0
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(t 2)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  .   0   0   p
            // E_b |  1   .   .   2
            // E_1 |  p   3   4   p
              5,   0,   0,   6,
              1,   5,   5,   2,
              6,   3,   4,   6,
        ]),
        (T::PRS(59), 0, 0, vec![
            // E -> E + E | - E | 0
            // - 0: E -> E_1 E_b
            // - 1: E_b -> + E_1 E_b
            // - 2: E_b -> ε
            // - 3: E_1 -> - E
            // - 4: E_1 -> 0
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 0)),
            (2, alt!(t 2)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  p   0   0   p
            // E_b |  1   .   .   2
            // E_1 |  p   3   4   p
              6,   0,   0,   6,
              1,   5,   5,   2,
              6,   3,   4,   6,
        ]),
        (T::PRS(64), 0, 0, vec![
            // E -> - E | E + E | 0
            // - 0: E -> E_1 E_b
            // - 1: E_b -> + E_1 E_b
            // - 2: E_b -> ε
            // - 3: E_1 -> - E_1
            // - 4: E_1 -> 0
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 1, nt 2)),
            (2, alt!(t 2)),
        ], vec![
            //     |  +   -   0   $
            // ----+-----------------
            // E   |  .   0   0   p
            // E_b |  1   .   .   2
            // E_1 |  p   3   4   p
              5,   0,   0,   6,
              1,   5,   5,   2,
              6,   3,   4,   6,
        ]),
        (T::PRS(63), 0, 0, vec![
            // E -> <R>E ^ E | E * E | - E | E + E | ID
            // - 0: E -> E_1b E3
            // - 1: E3 -> ^ E_b E3     R-assoc (256)
            // - 2: E3 -> * E_b E3
            // - 3: E3 -> + E5 E3
            // - 4: E3 -> ε
            // - 5: E5 -> E_1b E6
            // - 6: E6 -> ^ E_b E6     R-assoc
            // - 7: E6 -> * E_b E6
            // - 8: E6 -> ε
            // - 9: E_b -> E_1b E_1
            // - 10: E_1 -> ^ E_b E_1     R-assoc
            // - 11: E_1 -> ε
            // - 12: E_1b -> - E5
            // - 13: E_1b -> ID
            (0, alt!(nt 6, nt 1)),
            (1, alt!(#R, t 0, nt 4, nt 1)),
            (1, alt!(t 1, nt 4, nt 1)),
            (1, alt!(t 3, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 6, nt 3)),
            (3, alt!(#R, t 0, nt 4, nt 3)),
            (3, alt!(t 1, nt 4, nt 3)),
            (3, alt!(e)),
            (4, alt!(nt 6, nt 5)),
            (5, alt!(#R, t 0, nt 4, nt 5)),
            (5, alt!(e)),
            (6, alt!(t 2, nt 2)),
            (6, alt!(t 4)),
        ], vec![
            //      |  ^   *   -   +  ID   $
            // -----+-------------------------
            // E    |  .   .   0   .   0   p
            // E3   |  1   2   .   3   .   4
            // E5   |  p   p   5   p   5   p
            // E6   |  6   7   .   8   .   8
            // E_b  |  p   p   9   p   9   p
            // E_1  | 10  11   .  11   .  11
            // E_1b |  p   p  12   p  13   p
             14,  14,   0,  14,   0,  15,
              1,   2,  14,   3,  14,   4,
             15,  15,   5,  15,   5,  15,
              6,   7,  14,   8,  14,   8,
             15,  15,   9,  15,   9,  15,
             10,  11,  14,  11,  14,  11,
             15,  15,  12,  15,  13,  15,
        ]),
        (T::PRS(65), 0, 0, vec![
            // E -> E ! | E * E | E + | - E | ID
            // - 0: E -> E_2 E_b
            // - 1: E_b -> ! E_b
            // - 2: E_b -> * E_1 E_b
            // - 3: E_b -> + E_b
            // - 4: E_b -> ε
            // - 5: E_1 -> E_2 E_1b
            // - 6: E_1b -> ! E_1b
            // - 7: E_1b -> ε
            // - 8: E_2 -> - E
            // - 9: E_2 -> ID
            (0, alt!(nt 4, nt 1)),
            (1, alt!(t 1, nt 1)),
            (1, alt!(t 0, nt 2, nt 1)),
            (1, alt!(t 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(nt 4, nt 3)),
            (3, alt!(t 1, nt 3)),
            (3, alt!(e)),
            (4, alt!(t 3, nt 0)),
            (4, alt!(t 4)),
        ], vec![
            //      |  *   !   +   -  ID   $
            // -----+-------------------------
            // E    |  p   p   p   0   0   p
            // E_b  |  2   1   3   .   .   4
            // E_1  |  p   p   p   5   5   p
            // E_1b |  7   6   7   .   .   7
            // E_2  |  p   p   p   8   9   p
             11,  11,  11,   0,   0,  11,
              2,   1,   3,  10,  10,   4,
             11,  11,  11,   5,   5,  11,
              7,   6,   7,  10,  10,   7,
             11,  11,  11,   8,   9,  11,
        ]),

        (T::PRS(100), 0, 0, vec![
            // - 0: A -> c A_1
            // - 1: A_1 -> a A b A_1
            // - 2: A_1 -> ε
            (0, alt!(t 2, nt 1)),
            (1, alt!(t 0, nt 0, t 1, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  a   b   c   $
            // ----+-----------------
            // A   |  .   p   0   p
            // A_1 |  1   2   .   2
              3,   4,   0,   4,
              1,   2,   3,   2,
        ]),
        (T::PRS(101), 0, 0, vec![
            // - 0: A -> a A A
            // - 1: A -> b
            (0, alt!(t 0, nt 0, nt 0)),
            (0, alt!(t 1)),
        ], vec![
            //   |  a   b   $
            // --+-------------
            // A |  0   1   p
              0,   1,   3,
        ]),
        (T::PRS(102), 0, 0, vec![
            // - 0: A -> A_2 A_1
            // - 1: A_1 -> a A b A_2 A_1
            // - 2: A_1 -> ε
            // - 3: A_2 -> c
            (0, alt!(nt 2, nt 1)),
            (1, alt!(t 0, nt 0, t 1, nt 2, nt 1)),
            (1, alt!(e)),
            (2, alt!(t 2)),
        ], vec![
            //     |  a   b   c   $
            // ----+-----------------
            // A   |  .   p   0   p
            // A_1 |  1   2   .   2
            // A_2 |  p   p   3   p
              4,   5,   0,   5,
              1,   2,   4,   2,
              5,   5,   3,   5,
        ]),
        (T::PRS(103), 0, 0, vec![
            // - 0: A -> a B c
            // - 1: A -> d
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(t 0, nt 1, t 2)),
            (0, alt!(t 3)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   |  a   b   c   d   $
            // --+---------------------
            // A |  0   p   p   1   p
            // B |  2   .   3   2   .
              0,   5,   5,   1,   5,
              2,   4,   3,   2,   4,
        ]),
        (T::PRS(104), 0, 2, vec![
            // - 0: A -> B c
            // - 1: A -> a
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(nt 1, t 2)),
            (0, alt!(t 0)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   |  a   b   c   $
            // --+-----------------
            // A |  1   p   0   p
            // B |  2   .   3   .
              1,   5,   0,   5,
              2,   4,   3,   4,
            // calc_table: ambiguity for NT 'A', T 'a': <B c> or <a> => <a> has been chosen
            // calc_table: ambiguity for NT 'B', T 'c': <A b A B> or <ε> => <ε> has been chosen
        ]),
        (T::PRS(105), 0, 2, vec![
            // - 0: A -> a B
            // - 1: A -> c
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(t 0, nt 1)),
            (0, alt!(t 2)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   |  a   b   c   $
            // --+-----------------
            // A |  0   p   1   p
            // B |  2   3   2   3
              0,   5,   1,   5,
              2,   3,   2,   3,
            // calc_table: ambiguity for NT 'B', T 'a': <A b A B> or <ε> => <A b A B> has been chosen
            // calc_table: ambiguity for NT 'B', T 'c': <A b A B> or <ε> => <A b A B> has been chosen
        ]),
        (T::PRS(106), 0, 4, vec![
            // - 0: A -> B
            // - 1: A -> a
            // - 2: B -> A b A B
            // - 3: B -> ε
            (0, alt!(nt 1)),
            (0, alt!(t 0)),
            (1, alt!(nt 0, t 1, nt 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //   | a  b  $
            // --+----------
            // A | 1  0  0
            // B | 2  2  3
              1,   0,   0,
              2,   2,   3,
            // calc_table: ambiguity for NT 'A', T 'a': <B> or <B> or <a> => <a> has been chosen
            // calc_table: ambiguity for NT 'A', T 'b': <B> or <B> => <B> has been chosen
            // calc_table: ambiguity for NT 'B', T 'a': <A b A B> or <ε> => <A b A B> has been chosen
            // calc_table: ambiguity for NT 'B', T 'b': <A b A B> or <ε> => <A b A B> has been chosen
        ]),
        (T::RTS(56), 0, 1, vec![
            // A -> (a b)* a
            // - 0: A -> A_1 a
            // - 1: A_1 -> a b A_1
            // - 2: A_1 -> ε
            (0, alt!(nt 1, t 0)),
            (1, alt!(t 0, t 1, nt 1)),
            (1, alt!(e)),

        ], vec![
            //     |  a   b   $
            // ----+-------------
            // A   |  0   .   p
            // A_1 |  1   .   .
              0,   3,   4,
              1,   3,   3,
            // calc_table: ambiguity for NT 'A_1', T 'a': <a b A_1> or <ε> => <a b A_1> has been chosen
        ]),
        (T::RTS(57), 0, 0, vec![
            // - 0: A -> a A_1
            // - 1: A_1 -> b a A_1
            // - 2: A_1 -> ε
            (0, alt!(t 0, nt 1)),
            (1, alt!(t 1, t 0, nt 1)),
            (1, alt!(e)),
        ], vec![
            //     |  a   b   $
            // ----+-------------
            // A   |  0   .   p
            // A_1 |  .   1   2
              0,   3,   4,
              3,   1,   2,
        ]),
        // (T::RTS(57), 0, 0, vec![
        //     // - 0: A -> a A_1
        //     // - 1: A_1 -> b a A_1
        //     // - 2: A_1 -> ε
        //     (0, alt!(t 0, nt 1)),
        //     (1, alt!(t 1, t 0, nt 1)),
        //     (1, alt!(e)),
        // ], vec![
        //     //     |  a   b   $
        //     // ----+-------------
        //     // A   |  0   .   p
        //     // A_1 |  .   1   2
        //       0,   3,   4,
        //       3,   1,   2,
        // ]),
        // (T::RTS(57), 0, 0, vec![
        // ], vec![
        // ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (rule_id, start, expected_warnings, expected_alts, expected_table)) in tests.into_iter().enumerate() {
        let mut ll1 = rule_id.build_prs(test_id, start, false);
        if VERBOSE {
            println!("{:=<80}\ntest {test_id} with {rule_id:?}/{start}:", "");
            ll1.print_rules(false, false);
        }
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        if VERBOSE {
            map_and_print_first(&first, ll1.get_symbol_table());
            map_and_print_follow(&follow, ll1.get_symbol_table());
        }
        let parsing_table = ll1.calc_table(&first, &follow, true);
        let LLParsingTable { num_nt, num_t, alts, table, .. } = &parsing_table;
        assert_eq!(num_nt * num_t, table.len(), "incorrect table size in test {test_id}/{rule_id:?}/{start}");
        if VERBOSE {
            println!("num_nt = {num_nt}, num_t = {num_t}");
            ll1.print_rules(false, false);
            print_alts(&alts, ll1.get_symbol_table());
            println!("{}",
                     alts.iter().enumerate().map(|(_id, (v, f))| {
                         let flags = if f.get_flags() != 0 {
                             let mut vf = ruleflag::alt_info_to_string(f.get_flags());
                             format!("#{}, ", if vf.len() == 1 { vf.pop().unwrap() } else { f.get_flags().to_string() })
                         } else {
                             String::new()
                         };
                         format!("            ({v}, alt!({flags}{})),", f.iter().map(|s| s.to_macro_item()).join(", "))
                     }
            ).join("\n"));
            println!("table:");
            parsing_table.print(ll1.get_symbol_table(), 12);
            for i in 0..*num_nt {
                println!("            {},", (0..*num_t).map(|j| format!("{:3}", table[i * num_t + j])).join(", "));
            }
            if table.len() < 30 {
                println!("vec![{}]", table.iter().map(|x| x.to_string()).join(", "));
            }
            println!("flags:");
            for v in 0..ll1.num_nt as VarId {
                let parent = ll1.get_parent(v).map(|p| ll1.get_symbol_table().unwrap().get_nt_name(p));
                println!("- {}: {}{}",
                         ll1.get_symbol_table().unwrap().get_nt_name(v),
                         ruleflag::to_string(ll1.get_flags(v)).join(" "),
                         if let Some(p) = parent { format!(", parent: {p}") } else { String::new() } );
            }
            ll1.print_logs();
        }
        assert_eq!(*alts, expected_alts, "test {test_id}/{rule_id:?}/{start} failed");
        assert_eq!(*table, expected_table, "test {test_id}/{rule_id:?}/{start} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id}/{rule_id:?}/{start} failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings, "test {test_id}/{rule_id:?}/{start} failed, warnings: {}", ll1.log.get_warnings().join("\n"));
   }
}

#[test]
fn build_prs_error() {
    let rts = build_rts(101);
    let text = format!("rts errors: {}", rts.get_log().num_errors());
    assert_eq!(rts.get_log().num_errors(), 0, "{text}");
    let rts_normalized = RuleTreeSet::<Normalized>::build_from(rts.clone());
    let rts_normalized_err = RuleTreeSet::<Normalized>::try_build_from(rts);
    let text = format!("rts_normalized errors: {}, err: {}", rts_normalized.get_log().num_errors(), rts_normalized_err.is_err());
    assert!(rts_normalized.get_log().num_errors() > 0, "{text}");
    assert!(rts_normalized_err.is_err(), "{text}");
    let prs = ProdRuleSet::build_from(rts_normalized.clone());
    let prs_e = ProdRuleSet::try_build_from(rts_normalized);
    let text = format!("prs errors: {}, err: {}", prs.get_log().num_errors(), prs_e.is_err());
    assert!(prs.get_log().num_errors() > 0, "{text}");
    assert!(prs_e.is_err(), "{text}");
}
