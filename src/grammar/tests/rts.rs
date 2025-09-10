// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use super::*;

#[test]
fn ruletreeset_to_str() {
    let tests = vec![
        // RTS, var, node, emphasis, expected
        (18, 0, None, None, "a | b | c"),
        (56, 0, Some(2), None, "a b"),
        (23, 0, None, None, "a b+ c"),
        (56, 0, None, None, "(a b)* a"),
        (38, 0, None, None, "A a c? | A b c? | d"),
        (55, 0, None, None, "a ((b c | d)+ e)+ f"),
        (55, 0, None, Some(3), "a ( ►► (b c | d)+ e ◄◄ )+ f"),
        (53, 0, None, None, "(<L=AIter1> a d | B)+ c"),
        (44, 0, None, None, r#""-" E | <R> E ("*" | "/" <P>) E | E ("+" | "-" <P>) E | ID"#),
    ];
    const VERBOSE: bool = false;
    let mut errors = 0;
    for (test_id, (rts_id, var, node_maybe, emphasis_maybe, expected_str)) in tests.into_iter().enumerate() {
        let rts = build_rts(rts_id);
        let result_str = rts.to_str(var, node_maybe, emphasis_maybe);
        if VERBOSE {
            // println!("{test_id} ({rule_id})");
            let tfmt = GrTreeFmt {
                tree: &rts.get_tree(var).unwrap(),
                show_ids: true,
                show_depth: false,
                symbol_table: rts.get_symbol_table(),
                start_node: None,
            };
            println!("{test_id} ({rts_id}) => {tfmt}");
            println!("        ({rts_id}, {var}, {node_maybe:?}, {emphasis_maybe:?}, \"{result_str}\"),");
        }
        if result_str != expected_str {
            errors += 1;
            println!("{test_id} is wrong: \"{result_str}\" instead of \"{expected_str}\"");
        }
    }
    assert_eq!(errors, 0);
}

#[test]
fn cleanup_tree() {
    let tests: Vec<(u32, Option<usize>, (Option<(bool, bool)>, &str), (Option<(bool, bool)>, &str))> = vec![
        // a b ε c ε
        (65, None, (Some((false, false)), "a b c"), (Some((false, false)), "a b c")),
        // a b | c ε | ε | d | ε | <P> ε | <R> <P>
        (66, None, (Some((false, true)), "a b | c | d | ε"), (Some((false, true)), "a b | c | d")),
        // <P> ε | <R> <P>
        (67, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (68, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (69, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (70, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (71, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // a | ε
        (72, None, (Some((false, true)), "a | ε"), (Some((false, true)), "a")),
        // (a b)*
        (56, Some(1), (None, "(a b)*"), (None, "(a b)*")),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_SOLUTION: bool = false;
    let mut errors = 0;
    for (test_id, root, expected_false, expected_true) in tests {
        if VERBOSE { println!("{:=<80}\ntest {test_id}:", ""); }
        let rules = build_rts(test_id);
        let sym_tab = rules.symbol_table.clone();
        let st = sym_tab.as_ref();
        let mut result = vec![];
        let s1 = rules.to_str(0, root, None);
        for del_empty_term in [false, true] {
            if VERBOSE { println!("del_empty_term = {del_empty_term}"); }
            let mut t = rules.trees[0].clone();
            let si1 = t.to_str_index(root, st);
            let output = grtree_cleanup(&mut t, root, del_empty_term);
            let s2 = grtree_to_str(&t, root, None, st, false);
            let si2 = t.to_str_index(root, st);
            if VERBOSE {
                println!("  {s1}  =>  {s2}   {output:?}");
                println!("  {si1}  =>  {si2}");
            }
            result.push((output, s2));
        }
        let expected = [expected_false, expected_true].into_iter().map(|(a, b)| (a, b.to_string())).to_vec();
        if VERBOSE_SOLUTION {
            println!("        // {s1}");
            println!("        ({test_id}, {root:?}, {:?}, {:?}),", result[0], result[1]);
        }
        if result != expected {
            errors += 1;
            if VERBOSE { println!("## error:\n- result   = {result:?}\n- expected = {expected:?}"); }
        }
    }
    assert_eq!(errors, 0);
}

// cargo +nightly miri test --package lexigram --lib grammar::tests::ruletree_normalize -- --exact
#[test]
fn rts_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>)> = vec![
        //   A -> b | c | D
        (0, btreemap![0 => r#"b | c | D"#]),
        //   A -> B C | d | e | F G | h | i | J K
        (1, btreemap![0 => r#"B C | d | e | F G | h | i | J K"#]),
        //   A -> B C (D | E) F G (H | I)
        (2, btreemap![0 => r#"B C D F G H | B C D F G I | B C E F G H | B C E F G I"#]),
        //   A -> B C (D | E) F (G H | I)
        (3, btreemap![0 => r#"B C D F G H | B C D F I | B C E F G H | B C E F I"#]),
        //   A -> A B C (D | E) F (G H | I)
        (4, btreemap![0 => r#"A B C D F G H | A B C D F I | A B C E F G H | A B C E F I"#]),
        //   A -> B?
        (5, btreemap![0 => r#"B | ε"#]),
        //   A -> (B C)?
        (6, btreemap![0 => r#"B C | ε"#]),
        //   A -> (B C | D)?
        (7, btreemap![0 => r#"B C | D | ε"#]),
        //   A -> b c+
        (8, btreemap![0 => r#"b A_1"#, 1 => r#"c A_1 | c"#]),
        //   A -> "var" (id ",")+
        (9, btreemap![0 => r#""var" A_1"#, 1 => r#"id "," A_1 | id ",""#]),
        //   A -> b (c d | e)+
        (10, btreemap![0 => r#"b A_1"#, 1 => r#"c d A_1 | c d | e A_1 | e"#]),
        //   A -> b c*
        (11, btreemap![0 => r#"b A_1"#, 1 => r#"c A_1 | ε"#]),
        //   A -> b (c d)*
        (12, btreemap![0 => r#"b A_1"#, 1 => r#"c d A_1 | ε"#]),
        //   A -> a (b+ c)+ d
        (17, btreemap![0 => r#"a A_2 d"#, 1 => r#"b A_1 | b"#, 2 => r#"A_1 c A_2 | A_1 c"#]),
        //   A -> b (c d | e)*
        (13, btreemap![0 => r#"b A_1"#, 1 => r#"c d A_1 | e A_1 | ε"#]),
        //   A -> A (b <L=AIter1>)* c | d
        (19, btreemap![0 => r#"A AIter1 c | d"#, 2 => r#"b <L=AIter1> AIter1 | ε"#]),
        //   A -> A (b | c <R> | d) A | e
        (15, btreemap![0 => r#"A b A | A c <R> A | A d A | e"#]),
        //   E -> "-" E | E ("*" | "/" <P>) E | E ("+" | "-" <P>) E | ID
        (42, btreemap![0 => r#""-" E | E "*" E | E "/" <P> E | E "+" E | E "-" <P> E | ID"#]),
        //   A -> a b | c | d | e
        (45, btreemap![0 => r#"a b | c | d | e"#]),
        //   A -> (a | b) (c | d)
        (46, btreemap![0 => r#"a c | a d | b c | b d"#]),
        //   A -> (a | b) (c d | e)*
        (47, btreemap![0 => r#"a A_1 | b A_1"#, 1 => r#"c d A_1 | e A_1 | ε"#]),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const SHOW_RESULTS_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected) in tests {
        let mut rules = build_rts(test_id);
        let sym_tab = rules.get_symbol_table();
        let originals = rules.get_non_empty_nts()
            .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false)))
            .to_vec();
        if SHOW_RESULTS_ONLY {
            println!("{}", originals.iter().map(|s| format!("        // {s}")).join(""));
        }
        if VERBOSE {
            println!("{:=<80}\ntest {test_id}:", "");
            println!("- original:\n{}", originals.join("\n"));
            println!("- original (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }
        rules.normalize();
        assert_eq!(rules.log.num_errors(), 0, "test {test_id} failed to normalize:\n{}", rules.log.get_messages_str());
        if let Some(err) = check_rts_sanity(&rules, VERBOSE_DETAILS) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let result = BTreeMap::from_iter(rules.get_non_empty_nts()
            .map(|(id, _t)| (id, format!("{}", rules.to_str(id, None, None)))));
        if VERBOSE {
            let sym_tab = rules.get_symbol_table();
            println!("- normalized:\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("- normalized (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("flags:  {:?}", rules.flags);
            println!("parent: {:?}", rules.parent);
            println!("{}", rules.get_non_empty_nts()
                .map(|(id, t)| format!("- {id} => {} (depth {})",
                                       rules.to_str(id, None, None),
                                       t.depth().unwrap_or(0))).join("\n"));
        }
        if SHOW_RESULTS_ONLY {
            println!("        ({test_id}, btreemap![{}]),", result.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        if result != expected {
            errors += 1;
            if !SHOW_RESULTS_ONLY {
                println!("## ERROR ## test {test_id} failed");
            }
        };
    }
    assert_eq!(errors, 0);
}

#[test]
fn orig_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>)> = vec![
        //   A -> b | c | D
        (0, btreemap![0 => r#"b | c | D"#]),
        //   A -> A B C (D | E) F (G H | I)
        (4, btreemap![0 => r#"A B C D F G H | A B C D F I | A B C E F G H | A B C E F I"#]),
        //   A -> B?
        (5, btreemap![0 => r#"B | ε"#]),
        //   A -> (B C)?
        (6, btreemap![0 => r#"B C | ε"#]),
        //   A -> (B C | D)?
        (7, btreemap![0 => r#"B C | D | ε"#]),
        //   A -> a b?
        (73, btreemap![0 => r#"a b | a"#]),
        //   A -> b c+
        (8, btreemap![0 => r#"b c+"#]),
        //   A -> "var" (id ",")+
        (9, btreemap![0 => r#""var" (id ",")+"#]),
        //   A -> b (c d | e)+
        (10, btreemap![0 => r#"b (c d | e)+"#]),
        //   A -> b c*
        (11, btreemap![0 => r#"b c*"#]),
        //   A -> b (c d)*
        (12, btreemap![0 => r#"b (c d)*"#]),
        //   A -> a (b+ c)+ d
        (13, btreemap![0 => r#"b (c d | e)*"#]),
        //   A -> b? (c | d)?
        (14, btreemap![0 => r#"b c | b d | b | c | d | ε"#]),
        //   A -> A a c? | A b c? | d
        (38, btreemap![0 => r#"A a c | A a | A b c | A b | d"#]),
        //   A -> a? b* c?
        (59, btreemap![0 => r#"a b* c | a b* | b* c | b*"#]),
        //   A -> a? (b? | c?)*
        (60, btreemap![0 => r#"a (b | c)* | (b | c)*"#]),
        //   A -> a ε?
        (61, btreemap![0 => r#"a"#]),
        //   A -> (a | ε)?
        (62, btreemap![0 => r#"a | ε"#]),
        //   A -> a ε*
        (63, btreemap![0 => r#"a"#]),
        //   A -> a ε+
        (64, btreemap![0 => r#"a"#]),
        (17, btreemap![0 => r#"a (b+ c)+ d"#]),
        //   A -> a A | b
        (35, btreemap![0 => r#"a A | b"#]),
        //   A -> A (b <L=AIter1>)* c | d
        (19, btreemap![0 => r#"A (b <L=AIter1>)* c | d"#]),
        //   A -> A (b | c <R> | d) A | e
        (15, btreemap![0 => r#"A b A | A c <R> A | A d A | e"#]),
        //   E -> "-" E | E ("*" | "/" <P>) E | E ("+" | "-" <P>) E | ID
        (42, btreemap![0 => r#""-" E | E "*" E | E "/" <P> E | E "+" E | E "-" <P> E | ID"#]),
        //   A -> a b | c | d | e
        (45, btreemap![0 => r#"a b | c | d | e"#]),
        //   A -> (a | b) (c | d)
        (46, btreemap![0 => r#"a c | a d | b c | b d"#]),
        //   A -> (a | b) (c d | e)*
        (47, btreemap![0 => r#"a (c d | e)* | b (c d | e)*"#]),
        //   A -> a (b c)+ d
        (54, btreemap![0 => r#"a (b c)+ d"#]),
        //   A -> (a d | B)* c        //   B -> b
        (50, btreemap![0 => r#"(a d | B)* c"#, 1 => r#"b"#]),
        //   A -> (a d | B)+ c        //   B -> b
        (52, btreemap![0 => r#"(a d | B)+ c"#, 1 => r#"b"#]),
        //   A -> a (b <L=AIter1>)* c
        (22, btreemap![0 => r#"a (b <L=AIter1>)* c"#]),
        //   A -> (a B)+ c        //   B -> b
        (28, btreemap![0 => r#"(a B)+ c"#, 1 => r#"b"#]),
    ];
    const VERBOSE: bool = true;
    const SHOW_RESULTS_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected) in tests {
        let rules = build_rts(test_id);
        let sym_tab = rules.get_symbol_table();
        let originals = rules.get_non_empty_nts()
            .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false)))
            .to_vec();
        if VERBOSE && !SHOW_RESULTS_ONLY {
            println!("{:=<80}\ntest {test_id}:", "");
            println!("- original:\n{}", originals.join("\n"));
            println!("- original (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }
        let rules = RuleTreeSet::<Normalized>::build_from(rules);
        assert_eq!(rules.log.num_errors(), 0, "test {test_id} failed to normalize:\n{}", rules.log.get_messages_str());
        if let Some(err) = check_rts_sanity(&rules, false) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let sym_tab = rules.get_symbol_table();
        let result = rules.origin.trees.iter().index::<VarId>()
                .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                .map(|(v, t)| (v, grtree_to_str(t, None, None, sym_tab, false)))
            .collect::<BTreeMap<_, _>>();
        if VERBOSE && !SHOW_RESULTS_ONLY {
            println!("- normalized:\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("- normalized (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("- original tree partially normalized:\n{}",
                     rules.origin.trees.iter().index::<VarId>()
                         .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("  other format:\n{}",
                     rules.origin.trees.iter().index::<VarId>()
                         .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("- mapping normalized -> original:");
            let mut map = rules.origin.map.iter()
                .filter(|((va, _), (_, _))| !is_grtree_empty_symbol(&rules.get_tree(*va).unwrap()))
                .map(|((va, ida), (vo, ido))| ((*va, *ida), (*vo, *ido)))
                .collect::<Vec<_>>();
            map.sort();
            let cols = map.chunks(5)
                .map(|chunk| {
                    let mut v = chunk.into_iter()
                        .map(|&((va, ida), (vo, ido))| format!("| {} #{ida}  =>  {} #{ido}", Symbol::NT(va).to_str(sym_tab), Symbol::NT(vo).to_str(sym_tab)))
                        .to_vec();
                    v.resize(5, "|".to_string());
                    v
                })
                .to_vec();
            println!("{}", indent_source(vec![columns_to_str(cols, Some(vec![20; 5]))], 2));
        }
        let prules = ProdRuleSet::<General>::build_from(rules);
        if VERBOSE {
            println!("PRS origin:\n{}", indent_source(vec![prules.prs_factor_origins_str(true)], 4));
        }
        let ll1 = ProdRuleSet::<LL1>::build_from(prules);
        // println!("{}", ll1.symbol_table.as_ref().unwrap().get_nonterminals().enumerate().map(|(idx, nt)| format!("{idx}:{nt}")).join(", "));
        if VERBOSE {
            println!("LL1 origin:\n{}", indent_source(vec![ll1.prs_factor_origins_str(true)], 4));
            // let sym_tab = ll1.get_symbol_table();
            // println!("  other format:\n{}",
            //          ll1.origin.trees.iter().index::<VarId>()
            //              .filter(|(_, t)| !is_grtree_empty_symbol(&t))
            //              .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }

        if VERBOSE || SHOW_RESULTS_ONLY {
            println!("{}", originals.iter().map(|s| format!("        // {s}")).join(""));
            println!("        ({test_id}, btreemap![{}]),", result.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        if result != expected {
            errors += 1;
            if !SHOW_RESULTS_ONLY {
                println!("## ERROR ## test {test_id} failed");
            }
        };
    }
    assert_eq!(errors, 0);
}

#[test]
fn rts_prodrule_from() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>, Vec<u32>, Vec<Option<VarId>>)> = vec![
        (0, btreemap![0 => prule!(t 1; t 2; nt 3)], vec![0], vec![None]),
        (1, btreemap![0 => prule!(nt 1, nt 2; t 3; t 4; nt 5, nt 6;t 7; t 8; nt 9, nt 10)], vec![0], vec![None]),
        (2, btreemap![0 => prule!(
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 8;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 8;
        )], vec![0], vec![None]),
        (3, btreemap![0 => prule!(
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 3, nt 5, nt 8;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 4, nt 5, nt 8;
        )], vec![0], vec![None]),
        (4, btreemap![0 => prule!(
            nt 0, nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 0, nt 1, nt 2, nt 3, nt 5, nt 8;
            nt 0, nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 0, nt 1, nt 2, nt 4, nt 5, nt 8;
        )], vec![0], vec![None]),
        (5, btreemap![0 => prule!(nt 1; e)], vec![0], vec![None]),
        (6, btreemap![0 => prule!(nt 1, nt 2; e)], vec![0], vec![None]),
        (7, btreemap![0 => prule!(nt 1, nt 2; nt 3; e)], vec![0], vec![None]),
        (8, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, nt 1; t 2)
        ], vec![6144, 4097], vec![None, Some(0)]),
        (9, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; t 2, t 3)
        ], vec![6144, 4097], vec![None, Some(0)]),
        (10, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; t 2, t 3; t 4, nt 1; t 4)
        ], vec![6144, 4097], vec![None, Some(0)]),
        (11, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, nt 1; e)
        ], vec![2048, 1], vec![None, Some(0)]),
        (12, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; e)
        ], vec![2048, 1], vec![None, Some(0)]),
        (13, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; t 4, nt 1; e)
        ], vec![2048, 1], vec![None, Some(0)]),
        (14, btreemap![
            0 => prule!(t 1, t 2; t 1, t 3; t 1; t 2; t 3; e)
        ], vec![0], vec![None]),
        (15, btreemap![
            0 => prule!(nt 0, t 1, nt 0; #R, nt 0, t 2, nt 0; nt 0, t 3, nt 0; t 4)
        ], vec![0], vec![None]),
        (17, btreemap![
            0 => prule!(t 0, nt 2, t 3),
            1 => prule!(t 1, nt 1; t 1),
            2 => prule!(nt 1, t 2, nt 2; nt 1, t 2),
        ], vec![6144, 4097, 6145], vec![None, Some(2), Some(0)]),
        (35, btreemap![
            0 => prule!(t 0, nt 0; t 1),
        ], vec![0], vec![None]), // R_RECURSION not set yet (set by ProdRuleSet<T>::remove_left_recursion())
        (36, btreemap![
            0 => prule!(t 0, nt 0; t 1),
        ], vec![128], vec![None]), // R_RECURSION not set yet (set by ProdRuleSet<T>::remove_left_recursion())
    ];
    const VERBOSE: bool = false;
    for (test_id, expected, expected_flags, expected_parent) in tests {
        let trees = build_rts(test_id);
        if VERBOSE {
            println!("\ntest {test_id}:");
        }
        let mut rules = ProdRuleSet::build_from(trees);
        assert!(rules.log.has_no_errors(), "test {test_id} failed to create production rules:\n{}", rules.log.get_messages_str());
        rules.simplify();
        let result = rules.get_non_empty_nts().map(|(id, p)| (id, p.clone())).collect::<BTreeMap<_, _>>();
        let num_vars = result.len();
        if VERBOSE {
            println!("=>");
            rules.print_rules(true, true);
            print_expected_code(&result);
            println!("  Flags: {}", rules.flags.iter().take(num_vars).index::<VarId>().map(|(nt, flag)| format!("\n  - {}: {}",
                Symbol::NT(nt).to_str(rules.get_symbol_table()), ruleflag::to_string(*flag).join(" "))).join(""));
            if !rules.log.is_empty() {
                println!("  Messages:{}", rules.log.get_messages().map(|m| format!("\n  - {m:?}")).join(""));
            }
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(rules.flags[..num_vars], expected_flags, "test {test_id} failed (flags)");
        assert_eq!(rules.parent[..num_vars], expected_parent, "test {test_id} failed (parent)");
    }
}
