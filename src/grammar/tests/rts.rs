// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use super::*;

// ---------------------------------------------------------------------------------------------
// RuleTreeSet

fn check_rts_sanity<T>(rules: &RuleTreeSet<T>, verbose: bool) -> Option<String> {
    let mut msg = String::new();
    for (var, tree) in rules.get_trees_iter() {
        let mut indices = HashSet::<usize>::new();
        let mut n = 0;
        for node in tree.iter_post_depth_simple() {
            n += 1;
            if indices.contains(&node.index) {
                msg.push_str(&format!("duplicate index {} for var {var} in tree {:#}\n", node.index, tree.to_str(None, None)));
            }
            indices.insert(node.index);
        }
        if verbose { println!("  {var} uses {}/{} nodes", n, tree.len()); }
    }
    if msg.is_empty() {
        None
    } else {
        Some(msg)
    }
}

impl<T> RuleTreeSet<T> {
    pub fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &GrTree)> {
        self.get_trees_iter().filter(|(_, rule)| !is_grtree_empty_symbol(&rule))
    }
}

// ---------------------------------------------------------------------------------------------

#[test]
fn ruletreeset_to_str() {
    let tests = vec![
        // RTS, var, node, emphasis, expected
        (4, 0, None, None, "A B | C D"),
        (6, 0, None, None, "(A | B) (C | D) E"),
        (10, 0, None, None, "(A | B)?"),
        (31, 0, None, None, r#"A B | C ε | ε | D | ε | <P> ε | <R> <P>"#),
        (32, 0, None, None, r#"<P> ε | <R> <P>"#),
        (35, 0, None, None, r#"ε"#),
        (104, 0, None, None, "(b A b B A)*"),
        (200, 0, None, None, "A (<L=i> B)* C"),
        (200, 0, None, Some(3), "A ( ►► <L=i> B ◄◄ )* C"),
        (200, 0, Some(3), None, "<L=i> B"),
        (207, 0, None, None, r#"(A (<L=j> B ",")+ ";")+ C"#),
        (600, 0, None, None, r#"e "+" e | Num"#),
    ];
    const VERBOSE: bool = false;
    let mut errors = 0;
    for (test_id, (rts_id, var, node_maybe, emphasis_maybe, expected_str)) in tests.into_iter().enumerate() {
        let rts = TestRules(rts_id).to_rts_general().unwrap();
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
        // A B ε C ε
        (30, None, (Some((false, false)), "A B C"), (Some((false, false)), "A B C")),
        // A B | C ε | ε | D | ε | <P> ε | <R> <P>
        (31, None, (Some((false, true)), "A B | C | D | ε"), (Some((false, true)), "A B | C | D")),
        // <P> ε | <R> <P>
        (32, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (33, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (34, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (35, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (36, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // A | ε
        (37, None, (Some((false, true)), "A | ε"), (Some((false, true)), "A")),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_SOLUTION: bool = false;
    let mut errors = 0;
    for (test_id, root, expected_false, expected_true) in tests {
        if VERBOSE { println!("{:=<80}\ntest {test_id}:", ""); }
        let rules = TestRules(test_id).to_rts_general().unwrap();
        let sym_tab = rules.symbol_table.clone();
        let st = sym_tab.as_ref();
        let mut result = vec![];
        let s1 = rules.to_str(0, root, None);
        for del_empty_term in [false, true] {
            if VERBOSE { println!("del_empty_term = {del_empty_term}"); }
            let mut t = rules.trees[0].clone();
            let si1 = t.to_str_index(root, st);
            let output = grtree_cleanup(&mut t, root, del_empty_term);
            let s2 = grtree_to_str(&t, root, None, None, st, false);
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
    assert!(errors == 0, "{errors} error(s)");
}

// cargo +nightly miri test --package lexigram --lib grammar::tests::ruletree_normalize -- --exact
#[test]
fn rts_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>, BTreeMap<VarId, &str>)> = vec![
        (0, //   a -> A
         btreemap![0 => r#"a -> A"#],
         btreemap![0 => r#"a -> A"#]),
        (1, //   a -> A B
         btreemap![0 => r#"a -> A B"#],
         btreemap![0 => r#"a -> A B"#]),
        (2, //   a -> A | B
         btreemap![0 => r#"a -> A | B"#],
         btreemap![0 => r#"a -> A | B"#]),
        (3, //   a -> A B | C
         btreemap![0 => r#"a -> A B | C"#],
         btreemap![0 => r#"a -> A B | C"#]),
        (4, //   a -> A B | C D
         btreemap![0 => r#"a -> A B | C D"#],
         btreemap![0 => r#"a -> A B | C D"#]),
        (5, //   a -> (A | B) (C | D)
         btreemap![0 => r#"a -> A C | A D | B C | B D"#],
         btreemap![0 => r#"a -> A C | A D | B C | B D"#]),
        (6, //   a -> (A | B) (C | D) E
         btreemap![0 => r#"a -> A C E | A D E | B C E | B D E"#],
         btreemap![0 => r#"a -> A C E | A D E | B C E | B D E"#]),
        (7, //   a -> A?
         btreemap![0 => r#"a -> A | ε"#],
         btreemap![0 => r#"a -> A | ε"#]),
        (8, //   a -> A? B
         btreemap![0 => r#"a -> A B | B"#],
         btreemap![0 => r#"a -> A B | B"#]),
        (9, //   a -> (A B)?
         btreemap![0 => r#"a -> A B | ε"#],
         btreemap![0 => r#"a -> A B | ε"#]),
        (10, //   a -> (A | B)?
         btreemap![0 => r#"a -> A | B | ε"#],
         btreemap![0 => r#"a -> A | B | ε"#]),
        (11, //   a -> A b;   b -> B
         btreemap![0 => r#"a -> A b"#, 1 => r#"b -> B"#],
         btreemap![0 => r#"a -> A b"#, 1 => r#"b -> B"#]),
        (30, //   a -> A B ε C ε
         btreemap![0 => r#"a -> A B C"#],
         btreemap![0 => r#"a -> A B C"#]),
        (31, //   a -> A B | C ε | ε | D | ε | <P> ε | <R> <P>
         btreemap![0 => r#"a -> A B | C | D | ε"#],
         btreemap![0 => r#"a -> A B | C | D | ε"#]),
        (32, //   a -> <P> ε | <R> <P>
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (33, //   a -> <P> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (34, //   a -> <P> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (35, //   a -> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (36, //   a -> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (37, //   a -> A | ε
         btreemap![0 => r#"a -> A | ε"#],
         btreemap![0 => r#"a -> A | ε"#]),
        (100, //   a -> A*
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | ε"#],
         btreemap![0 => r#"a -> A*"#]),
        (101, //   a -> A+
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | A"#],
         btreemap![0 => r#"a -> A+"#]),
        (102, //   a -> A B* C
         btreemap![0 => r#"a -> A a_1 C"#, 1 => r#"a_1 -> B a_1 | ε"#],
         btreemap![0 => r#"a -> A B* C"#]),
        (103, //   a -> A B+ C
         btreemap![0 => r#"a -> A a_1 C"#, 1 => r#"a_1 -> B a_1 | B"#],
         btreemap![0 => r#"a -> A B+ C"#]),
        (104, //   a -> (b A b B A)*;   b -> C
         btreemap![0 => r#"a -> a_1"#, 1 => r#"b -> C"#, 2 => r#"a_1 -> b A b B A a_1 | ε"#],
         btreemap![0 => r#"a -> (b A b B A)*"#, 1 => r#"b -> C"#]),
        (105, //   a -> (b A b B A)+;   b -> C
         btreemap![0 => r#"a -> a_1"#, 1 => r#"b -> C"#, 2 => r#"a_1 -> b A b B A a_1 | b A b B A"#],
         btreemap![0 => r#"a -> (b A b B A)+"#, 1 => r#"b -> C"#]),
        (106, //   a -> (A (b ",")* ";")* C;   b -> B
         btreemap![0 => r#"a -> a_2 C"#, 1 => r#"b -> B"#, 2 => r#"a_1 -> b "," a_1 | ε"#, 3 => r#"a_2 -> A a_1 ";" a_2 | ε"#],
         btreemap![0 => r#"a -> (A (b ",")* ";")* C"#, 1 => r#"b -> B"#]),
        (107, //   a -> (A (b ",")+ ";")+ C;   b -> B
         btreemap![0 => r#"a -> a_2 C"#, 1 => r#"b -> B"#, 2 => r#"a_1 -> b "," a_1 | b ",""#, 3 => r#"a_2 -> A a_1 ";" a_2 | A a_1 ";""#],
         btreemap![0 => r#"a -> (A (b ",")+ ";")+ C"#, 1 => r#"b -> B"#]),
        (150, //   a -> (A | B)*
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | B a_1 | ε"#],
         btreemap![0 => r#"a -> (A | B)*"#]),
        (151, //   a -> (A | B)+
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | A | B a_1 | B"#],
         btreemap![0 => r#"a -> (A | B)+"#]),
        (200, //   a -> A (<L=i> B)* C
         btreemap![0 => r#"a -> A i C"#, 1 => r#"i -> <L> B i | ε"#],
         btreemap![0 => r#"a -> A (<L=i> B)* C"#]),
        (201, //   a -> A (<L=i> B)+ C
         btreemap![0 => r#"a -> A i C"#, 1 => r#"i -> <L> B i | <L> B"#],
         btreemap![0 => r#"a -> A (<L=i> B)+ C"#]),
        (202, //   a -> (<L=i> b A b B A)*;   b -> C
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L> b A b B A i | ε"#, 2 => r#"b -> C"#],
         btreemap![0 => r#"a -> (<L=i> b A b B A)*"#, 1 => r#"i -> <empty>"#, 2 => r#"b -> C"#]),
        (203, //   a -> (<L=i> b A b B A)+;   b -> C
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L> b A b B A i | <L> b A b B A"#, 2 => r#"b -> C"#],
         btreemap![0 => r#"a -> (<L=i> b A b B A)+"#, 1 => r#"i -> <empty>"#, 2 => r#"b -> C"#]),
        (204, //   a -> (<L=i> A (B ",")* ";")*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L> A a_1 ";" i | ε"#, 2 => r#"a_1 -> B "," a_1 | ε"#],
         btreemap![0 => r#"a -> (<L=i> A (B ",")* ";")*"#]),
        (205, //   a -> (<L=i> A (B ",")+ ";")+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L> A a_1 ";" i | <L> A a_1 ";""#, 2 => r#"a_1 -> B "," a_1 | B ",""#],
         btreemap![0 => r#"a -> (<L=i> A (B ",")+ ";")+"#]),
        (206, //   a -> (A (<L=j> B ",")* ";")* C
         btreemap![0 => r#"a -> a_1 C"#, 1 => r#"j -> <L> B "," j | ε"#, 2 => r#"a_1 -> A j ";" a_1 | ε"#],
         btreemap![0 => r#"a -> (A (<L=j> B ",")* ";")* C"#]),
        (207, //   a -> (A (<L=j> B ",")+ ";")+ C
         btreemap![0 => r#"a -> a_1 C"#, 1 => r#"j -> <L> B "," j | <L> B ",""#, 2 => r#"a_1 -> A j ";" a_1 | A j ";""#],
         btreemap![0 => r#"a -> (A (<L=j> B ",")+ ";")+ C"#]),
        (208, //   a -> (<L=i> A (<L=j> b ",")* ";")* C;   b -> B
         btreemap![0 => r#"a -> i C"#, 1 => r#"i -> <L> A j ";" i | ε"#, 2 => r#"j -> <L> b "," j | ε"#, 3 => r#"b -> B"#],
         btreemap![0 => r#"a -> (<L=i> A (<L=j> b ",")* ";")* C"#, 1 => r#"i -> <empty>"#, 2 => r#"j -> <empty>"#, 3 => r#"b -> B"#]),
        (209, //   a -> (<L=i> A (<L=j> b ",")+ ";")+ C;   b -> B
         btreemap![0 => r#"a -> i C"#, 1 => r#"i -> <L> A j ";" i | <L> A j ";""#, 2 => r#"j -> <L> b "," j | <L> b ",""#, 3 => r#"b -> B"#],
         btreemap![0 => r#"a -> (<L=i> A (<L=j> b ",")+ ";")+ C"#, 1 => r#"i -> <empty>"#, 2 => r#"j -> <empty>"#, 3 => r#"b -> B"#]),
        (250, //   a -> (<L=i> A | B)*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L> A i | B i | ε"#],
         btreemap![0 => r#"a -> (<L=i> A | B)*"#]),
        (251, //   a -> (<L=i> A | B)+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L> A i | <L> A | B i | B"#],
         btreemap![0 => r#"a -> (<L=i> A | B)+"#]),
     ];
    const VERBOSE: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const SHOW_RESULTS_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected, expected_orig) in tests {
        let mut rules = TestRules(test_id).to_rts_general().unwrap();
        let sym_tab = rules.get_symbol_table();
        let originals = rules.get_trees_iter()
            .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, Some(v), sym_tab, false)))
            .to_vec();
        if SHOW_RESULTS_ONLY {
            println!("        ({test_id}, // {}", originals.iter().join("; "));
        }
        if VERBOSE {
            println!("{:=<80}\ntest {test_id}:", "");
            println!("- original:\n{}", originals.join("\n"));
            println!("- original (detailed):\n{}",
                     rules.get_trees_iter()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }
        rules.normalize();
        assert_eq!(rules.log.num_errors(), 0, "test {test_id} failed to normalize:\n{}", rules.log.get_messages_str());
        if let Some(err) = check_rts_sanity(&rules, false) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let sym_tab = rules.get_symbol_table();
        let result = BTreeMap::from_iter(rules.get_trees_iter()
            .map(|(id, _t)| (id, format!("{} -> {}", Symbol::NT(id).to_str(sym_tab), rules.to_str(id, None, None)))));
        let result_orig = rules.origin.trees.iter().index::<VarId>()
                .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                .map(|(v, t)| (v, format!("{} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, Some(v), sym_tab, false))))
            .collect::<BTreeMap<_, _>>();
        if VERBOSE {
            println!("- normalized:\n{}",
                     rules.get_trees_iter()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, Some(v), sym_tab, false))).join("\n"));
            println!("- normalized (detailed):\n{}",
                     rules.get_trees_iter()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("- flags:  {:?}", rules.flags);
            println!("- parent: {:?}", rules.parent);
            // println!("{}", rules.get_trees_iter()
            //     .map(|(id, t)| format!("- {id} => {} (depth {})",
            //                            rules.to_str(id, None, None),
            //                            t.depth().unwrap_or(0))).join("\n"));
            if VERBOSE_DETAILS {
                println!("- original tree partially normalized:\n{}",
                         rules.origin.trees.iter().index::<VarId>()
                             .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                             .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, Some(v), sym_tab, false))).join("\n"));
                println!("  other format:\n{}",
                         rules.origin.trees.iter().index::<VarId>()
                             .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                             .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab))).join("\n"));
                println!("- mapping normalized -> original:");
                let mut map = rules.origin.map.iter()
                    .filter(|((va, _), (_, _))| !is_grtree_empty_symbol(&rules.get_tree(*va).expect(&format!("can't find tree {va}"))))
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
        }
        if SHOW_RESULTS_ONLY {
            println!("         btreemap![{}],", result.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
            println!("         btreemap![{}]),", result_orig.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        let expected_orig = expected_orig.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        if result != expected || result_orig != expected_orig {
            errors += 1;
            if !SHOW_RESULTS_ONLY {
                println!("## ERROR ## test {test_id} failed");
            }
        };
    }
    assert!(errors == 0, "{errors} error(s)");
}
