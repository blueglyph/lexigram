#![cfg(test)]

use std::collections::{BTreeMap, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{btreemap, gnode, sym};

#[test]
fn gnode() {
    let symbols = [
        ("Arrow".to_string(), Some("->".to_string())),
        ("Colon".to_string(), Some(":".to_string())),
        ("Id".to_string(), None),
    ];
    let t: GrNode = gnode!(t crate::regexgen::Id::Arrow);
    assert_eq!(t.to_string(), "[0]");
    assert_eq!(t.to_str(&symbols), "'->'");
    if let GrNode::Symbol(s) = t {
        assert_eq!(s.to_string(), "[0]");
        assert_eq!(s.to_str(&symbols), "'->'");
    } else {
        panic!();
    };
    assert_eq!(gnode!([1]), GrNode::Symbol(Symbol::T(1 as TokenId)));
    assert_eq!(gnode!(t 2), GrNode::Symbol(Symbol::T(2 as TokenId)));
    assert_eq!(gnode!(nt 3), GrNode::Symbol(Symbol::NT(3 as VarId)));
    assert_eq!(gnode!(e), GrNode::Symbol(Symbol::Empty));
    assert_eq!(gnode!(&), GrNode::Concat);
    assert_eq!(gnode!(|), GrNode::Or);
    assert_eq!(gnode!(?), GrNode::Maybe);
    assert_eq!(gnode!(+), GrNode::Plus);
    assert_eq!(gnode!(*), GrNode::Star);
}

#[test]
fn dup() {
    let mut rules = RuleTreeSet::<General>::new();
    let tree = rules.get_tree_mut(0);
    let mut a = Dup::new(tree.add(None, gnode!(nt 1)));
    let mut b = Dup::new(tree.add(None, gnode!(nt 2)));
    let mut result = Vec::new();
    for _ in 1..=3 {
        result.push(tree.get_dup(&mut a));
        result.push(tree.get_dup(&mut b));
    }
    assert_eq!(result, [0, 1, 2, 3, 4, 5]);
    assert_eq!(tree.len(), 6);
    let result2 = (0..6).map(|i| tree.get(i).clone()).to_vec();
    assert_eq!(result2, [gnode!(nt 1), gnode!(nt 2), gnode!(nt 1), gnode!(nt 2), gnode!(nt 1), gnode!(nt 2)]);
}

fn build_rules(id: u32) -> RuleTreeSet<General> {
    let mut rules = RuleTreeSet::new();
    // reserve a few variables just so the NT indices are not confusing:
    // we want new variables to begin at 10.
    rules.set_next_var(Some(10));
    let tree = rules.get_tree_mut(0);

    match id {
        0 => {
            let top = tree.addc_iter(None, gnode!(|), [gnode!(t 1), gnode!(t 2), gnode!(nt 3)]);
            tree.set_root(top);
        }
        1 => {
            let top = tree.add_root(gnode!(|));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(t 3), gnode!(t 4)]);
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            let or = tree.addc_iter(Some(top), gnode!(|), [gnode!(t 7), gnode!(t 8)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 9), gnode!(nt 10)]);
        }
        2 => {
            let top = tree.add_root(gnode!(&));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 7), gnode!(nt 8)]);
        }
        3 => {
            let top = tree.add_root(gnode!(&));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.add(Some(top), gnode!(nt 5));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 6), gnode!(nt 7)]);
            tree.add(Some(or), gnode!(nt 8));
        }
        4 => {
            let top = tree.add_root(gnode!(&));
            let cc = tree.add(Some(top), gnode!(&));
            tree.addc_iter(Some(cc), gnode!(&), [gnode!(nt 0), gnode!(nt 1)]);
            tree.add(Some(cc), gnode!(nt 2));
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.add(Some(top), gnode!(nt 5));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 6), gnode!(nt 7)]);
            tree.add(Some(or), gnode!(nt 8));
        }
        5 => {
            let top = tree.add_root(gnode!(?));
            tree.add(Some(top), gnode!(nt 1));
        }
        6 => {
            let top = tree.add_root(gnode!(?));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
        }
        7 => {
            let top = tree.add_root(gnode!(?));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.add(Some(or), gnode!(nt 3));
        }
        8 => { // 12+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            tree.addc(Some(cc), gnode!(+), gnode!(nt 2));
        }
        9 => { // 1(23)+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(p), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
        }
        10 => { // 1(23|4)+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(+));
            let or = tree.add(Some(p), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
            tree.add(Some(or), gnode!(nt 4));
        }
        11 => { // 12*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            tree.addc(Some(cc), gnode!(*), gnode!(nt 2));
        }
        12 => { // 1(23)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(p), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
        }
        13 => { // 1(23|4)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(*));
            let or = tree.add(Some(p), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
            tree.add(Some(or), gnode!(nt 4));
        }

        _ => {}
    }
    rules
}

fn check_sanity<T>(rules: &RuleTreeSet<T>, verbose: bool) -> Option<String> {
    let mut msg = String::new();
    for (var, tree) in rules.get_trees_iter() {
        let mut indices = HashSet::<usize>::new();
        let mut n = 0;
        for node in tree.iter_depth_simple() {
            n += 1;
            if indices.contains(&node.index) {
                msg.push_str(&format!("duplicate index {} for var {var} in tree {tree:#}\n", node.index));
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

// cargo +nightly miri test --package rlexer --lib grammar::tests::ruletree_normalize -- --exact
#[test]
fn ruletree_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>)> = vec![
        // |([1], [2], 3) (depth 1)
        (0, btreemap![0 => "|([1], [2], 3)"]),
        // |(&(1, 2), |([3], [4]), &(5, 6), |([7], [8], &(9, 10))) (depth 3)
        (1, btreemap![0 => "|(&(1, 2), [3], [4], &(5, 6), [7], [8], &(9, 10))"]),
        // &(&(1, 2), |(3, 4), &(5, 6), |(7, 8)) (depth 2)
        (2, btreemap![0 => "|(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 6, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 6, 8))"]),
        // &(&(1, 2), |(3, 4), 5, |(&(6, 7), 8)) (depth 3)
        (3, btreemap![0 => "|(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 8))"]),
        // &(&(&(0, 1), 2), |(3, 4), 5, |(&(6, 7), 8)) (depth 3)
        (4, btreemap![0 => "|(&(0, 1, 2, 3, 5, 6, 7), &(0, 1, 2, 3, 5, 8), &(0, 1, 2, 4, 5, 6, 7), &(0, 1, 2, 4, 5, 8))"]),
        // ?(1)
        (5, btreemap![0 => "|(1, ε)"]),
        // ?(&(1, 2))
        (6, btreemap![0 => "|(&(1, 2), ε)"]),
        // ?(|(&(1, 2), 3))
        (7, btreemap![0 => "|(&(1, 2), 3, ε)"]),
        // &(1, +(2))
        (8, btreemap![0 => "&(1, 10)", 10 => "|(&(2, 10), 2)"]),
        // &(1, +(&(2, 3))) (depth 3)
        (9, btreemap![0 => "&(1, 10)", 10 => "|(&(2, 3, 10), &(2, 3))"]),
        // &(1, +(|(&(2, 3), 4))) (depth 4)
        (10, btreemap![0 => "&(1, 10)", 10 => "|(&(2, 3, 10), &(2, 3), &(4, 10), 4)"]),
        // &(1, *(2))
        (11, btreemap![0 => "&(1, 10)", 10 => "|(&(2, 10), ε)"]),
        // &(1, *(&(2, 3))) (depth 3)
        (12, btreemap![0 => "&(1, 10)", 10 => "|(&(2, 3, 10), ε)"]),
        // &(1, *(|(&(2, 3), 4))) (depth 4)
        (13, btreemap![0 => "&(1, 10)", 10 => "|(&(2, 3, 10), &(4, 10), ε)"]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_rules(test_id);
        let vars = rules.get_vars().to_vec();
        rules.normalize();
        if let Some(err) = check_sanity(&rules, VERBOSE) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let result = BTreeMap::from_iter(rules.get_trees_iter().map(|(id, t)| (id, format!("{t}"))));
        if VERBOSE {
            println!("{}", rules.get_trees_iter().map(|(id, t)| format!("- {id} => {t:#} (depth {})", t.depth().unwrap_or(0))).join("\n"));
            println!("({test_id}, btreemap![{}]),\n", result.iter().map(|(ref id, t)| format!("{id} => \"{t}\"")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

#[test]
fn prodrule_from() {
    let tests: Vec<(u32, BTreeMap<VarId, Vec<Vec<Symbol>>>)> = vec![
        (0, btreemap![0 => vec![vec![sym!(t 1)], vec![sym!(t 2)], vec![sym!(nt 3)]]]),
        // |(&(1, 2), [3], [4], &(5, 6), [7], [8], &(9, 10))
        (1, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 2)], vec![sym!(t 3)], vec![sym!(t 4)], vec![sym!(nt 5), sym!(nt 6)],
            vec![sym!(t 7)], vec![sym!(t 8)], vec![sym!(nt 9), sym!(nt 10)]
        ]]),
        // |(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 6, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 6, 8))
        (2, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 3), sym!(nt 5), sym!(nt 6), sym!(nt 7)],
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 3), sym!(nt 5), sym!(nt 6), sym!(nt 8)],
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 4), sym!(nt 5), sym!(nt 6), sym!(nt 7)],
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 4), sym!(nt 5), sym!(nt 6), sym!(nt 8)]
        ]]),
        // |(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 8))
        (3, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 3), sym!(nt 5), sym!(nt 6), sym!(nt 7)],
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 3), sym!(nt 5), sym!(nt 8)],
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 4), sym!(nt 5), sym!(nt 6), sym!(nt 7)],
            vec![sym!(nt 1), sym!(nt 2), sym!(nt 4), sym!(nt 5), sym!(nt 8)],
        ]]),
        // |(&(0, 1, 2, 3, 5, 6, 7), &(0, 1, 2, 3, 5, 8), &(0, 1, 2, 4, 5, 6, 7), &(0, 1, 2, 4, 5, 8))
        (4, btreemap![0 => vec![
            vec![sym!(nt 0), sym!(nt 1), sym!(nt 2), sym!(nt 3), sym!(nt 5), sym!(nt 6), sym!(nt 7)],
            vec![sym!(nt 0), sym!(nt 1), sym!(nt 2), sym!(nt 3), sym!(nt 5), sym!(nt 8)],
            vec![sym!(nt 0), sym!(nt 1), sym!(nt 2), sym!(nt 4), sym!(nt 5), sym!(nt 6), sym!(nt 7)],
            vec![sym!(nt 0), sym!(nt 1), sym!(nt 2), sym!(nt 4), sym!(nt 5), sym!(nt 8)],
        ]]),
        // |(1, ε)
        (5, btreemap![0 => vec![
            vec![sym!(nt 1)], vec![sym!(e)]
        ]]),
        // |(&(1, 2), ε)
        (6, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 2)], vec![sym!(e)]
        ]]),
        // |(&(1, 2), 3, ε)
        (7, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 2)], vec![sym!(nt 3)], vec![sym!(e)]
        ]]),
        // 0 => &(1, 10), 10 => |(&(2, 10), 2)
        (8, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 10)]
        ], 10 => vec![
            vec![sym!(nt 2), sym!(nt 10)], vec![sym!(nt 2)]
        ]]),
        // 0 => &(1, 10), 10 => |(&(2, 3, 10), &(2, 3))
        (9, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 10)]
        ], 10 => vec![
            vec![sym!(nt 2), sym!(nt 3), sym!(nt 10)], vec![sym!(nt 2), sym!(nt 3)]
        ]]),
        // 0 => &(1, 10), 10 => |(&(2, 3, 10), &(2, 3), &(4, 10), 4)
        (10, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 10)]
        ], 10 => vec![
            vec![sym!(nt 2), sym!(nt 3), sym!(nt 10)], vec![sym!(nt 2), sym!(nt 3)], vec![sym!(nt 4), sym!(nt 10)], vec![sym!(nt 4)]
        ]]),
        // 0 => "&(1, 10)", 10 => "|(&(2, 10), ε)"
        (11, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 10)]
        ], 10 => vec![
            vec![sym!(nt 2), sym!(nt 10)], vec![sym!(e)]
        ]]),
        // [0 => "&(1, 10)", 10 => "|(&(2, 3, 10), ε)"]
        (12, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 10)]
        ], 10 => vec![
            vec![sym!(nt 2), sym!(nt 3), sym!(nt 10)], vec![sym!(e)]
        ]]),
        // 0 => "&(1, 10)", 10 => "|(&(2, 3, 10), &(4, 10), ε)"
        (13, btreemap![0 => vec![
            vec![sym!(nt 1), sym!(nt 10)]
        ], 10 => vec![
            vec![sym!(nt 2), sym!(nt 3), sym!(nt 10)], vec![sym!(nt 4), sym!(nt 10)], vec![sym!(e)]
        ]]),
    ];
    for (test_id, expected) in tests {
        let trees = build_rules(test_id);
        let rules = ProdRuleSet::from(trees);
        let result = rules.get_prods_iter().map(|(id, p)| (id, p.clone())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

// ---------------------------------------------------------------------------------------------

fn build_prodrules(id: u32) -> ProdRuleSet<LR> {
    let mut rules = ProdRuleSet::new();
    let prods = &mut rules.prods;
    match id {
        0 => {
            prods.extend([
                /* 0 */ vec![vec![sym!(nt 0), sym!(t 1)], vec![sym!(nt 0), sym!(t 2)], vec![sym!(t 3)], vec![sym!(t 4)]],
                /* 1 */ vec![vec![sym!(nt 0), sym!(t 5)], vec![sym!(t 6)], vec![sym!(t 7)]],
            ]);
        }
        _ => {}
    };
    rules
}

#[test]
fn test_remove_left_recursion() {
    const VERBOSE: bool = true;
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
            0 => vec![vec![sym!(t 3), sym!(nt 2)], vec![sym!(t 4), sym!(nt 2)]],
            1 => vec![vec![sym!(nt 0), sym!(t 5)], vec![sym!(t 6)], vec![sym!(t 7)]],
            2 => vec![vec![sym!(t 1), sym!(nt 2)], vec![sym!(t 2), sym!(nt 2)], vec![sym!(e)]],
        ])
    ];
    for (test_id, expected) in tests {
        let mut rules = build_prodrules(test_id);
        if VERBOSE {
            println!("test {test_id}:");
            println!("   {}", rules.get_prods_iter().map(|(var, p)| format!("{var} -> {}", prod_to_string(p))).join("\n   "));
        }
        rules.remove_left_recursion();
        if VERBOSE {
            println!("=> {}", rules.get_prods_iter().map(|(var, p)| format!("{var} -> {}", prod_to_string(p))).join("\n   "));
        }
        let result = rules.get_prods_iter().map(|(var, p)| (var, p.clone())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}