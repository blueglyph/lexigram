#![cfg(test)]

use std::collections::{BTreeMap, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{btreemap, gnode};

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
    let mut tree = RuleTree::new();
    let mut a = Dup::new(tree.0.add(None, gnode!(nt 1)));
    let mut b = Dup::new(tree.0.add(None, gnode!(nt 2)));
    let mut result = Vec::new();
    for i in 1..=3 {
        result.push(tree.get_dup(&mut a));
        result.push(tree.get_dup(&mut b));
    }
    assert_eq!(result, [0, 1, 2, 3, 4, 5]);
    assert_eq!(tree.0.len(), 6);
    let result2 = (0..6).map(|i| tree.0.get(i).clone()).to_vec();
    assert_eq!(result2, [gnode!(nt 1), gnode!(nt 2), gnode!(nt 1), gnode!(nt 2), gnode!(nt 1), gnode!(nt 2)]);
}

fn build_tree(id: u32) -> RuleTree {
    let mut tree = RuleTree::new();
    match id {
        0 => {
            let top = tree.0.addc_iter(None, gnode!(|), [gnode!(t 1), gnode!(t 2), gnode!(nt 3)]);
            tree.0.set_root(top);
        }
        1 => {
            let top = tree.0.add_root(gnode!(|));
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.0.addc_iter(Some(top), gnode!(|), [gnode!(t 3), gnode!(t 4)]);
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            let or = tree.0.addc_iter(Some(top), gnode!(|), [gnode!(t 7), gnode!(t 8)]);
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 9), gnode!(nt 10)]);
        }
        2 => {
            let top = tree.0.add_root(gnode!(&));
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.0.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            tree.0.addc_iter(Some(top), gnode!(|), [gnode!(nt 7), gnode!(nt 8)]);
        }
        3 => {
            let top = tree.0.add_root(gnode!(&));
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.0.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.0.add(Some(top), gnode!(nt 5));
            let or = tree.0.add(Some(top), gnode!(|));
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 6), gnode!(nt 7)]);
            tree.0.add(Some(or), gnode!(nt 8));
        }
        4 => {
            let top = tree.0.add_root(gnode!(&));
            let cc = tree.0.add(Some(top), gnode!(&));
            tree.0.addc_iter(Some(cc), gnode!(&), [gnode!(nt 0), gnode!(nt 1)]);
            tree.0.add(Some(cc), gnode!(nt 2));
            tree.0.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.0.add(Some(top), gnode!(nt 5));
            let or = tree.0.add(Some(top), gnode!(|));
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 6), gnode!(nt 7)]);
            tree.0.add(Some(or), gnode!(nt 8));
        }
        5 => {
            let top = tree.0.add_root(gnode!(?));
            tree.0.add(Some(top), gnode!(nt 1));
        }
        6 => {
            let top = tree.0.add_root(gnode!(?));
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
        }
        7 => {
            let top = tree.0.add_root(gnode!(?));
            let or = tree.0.add(Some(top), gnode!(|));
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.0.add(Some(or), gnode!(nt 3));
        }
        8 => { // 12+
            let cc = tree.0.add_root(gnode!(&));
            tree.0.add(Some(cc), gnode!(nt 1));
            tree.0.addc(Some(cc), gnode!(+), gnode!(nt 2));
        }
        9 => { // 1(23)+
            let cc = tree.0.add_root(gnode!(&));
            tree.0.add(Some(cc), gnode!(nt 1));
            let p = tree.0.add(Some(cc), gnode!(+));
            tree.0.addc_iter(Some(p), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
        }
        10 => { // 1(23|4)+
            let cc = tree.0.add_root(gnode!(&));
            tree.0.add(Some(cc), gnode!(nt 1));
            let p = tree.0.add(Some(cc), gnode!(+));
            let or = tree.0.add(Some(p), gnode!(|));
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
            tree.0.add(Some(or), gnode!(nt 4));
        }
        11 => { // 12*
            let cc = tree.0.add_root(gnode!(&));
            tree.0.add(Some(cc), gnode!(nt 1));
            tree.0.addc(Some(cc), gnode!(*), gnode!(nt 2));
        }
        12 => { // 1(23)*
            let cc = tree.0.add_root(gnode!(&));
            tree.0.add(Some(cc), gnode!(nt 1));
            let p = tree.0.add(Some(cc), gnode!(*));
            tree.0.addc_iter(Some(p), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
        }
        13 => { // 1(23|4)*
            let cc = tree.0.add_root(gnode!(&));
            tree.0.add(Some(cc), gnode!(nt 1));
            let p = tree.0.add(Some(cc), gnode!(*));
            let or = tree.0.add(Some(p), gnode!(|));
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
            tree.0.add(Some(or), gnode!(nt 4));
        }

        _ => {}
    }
    tree
}

fn check_sanity(id: VarId, tree: &RuleTree, verbose: bool) -> Option<String> {
    let mut indices = HashSet::<usize>::new();
    let mut n = 0;
    for node in tree.0.iter_depth_simple() {
        n += 1;
        if indices.contains(&node.index) {
            return Some(format!("duplicate index {} in tree {:#}", node.index, tree));
        }
        indices.insert(node.index);
    }
    if verbose { println!("  {id} uses {}/{} nodes", n, tree.0.len()); }
    None
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
        let mut tree = build_tree(test_id);
        if VERBOSE { println!("test {test_id}:\n- 0 -> {tree} (depth {})", tree.0.depth().unwrap()); }
        let new = tree.normalize(0, 10);
        for (id, t) in &new {
            if let Some(err) = check_sanity(*id, t, VERBOSE) {
                panic!("test {test_id} failed on NT {id}: {}", err);
            }
        }
        let result = BTreeMap::from_iter(new.iter().map(|(id, t)| (*id, format!("{t}"))));
        if VERBOSE {
            println!("{}", new.iter().map(|(ref id, t)| format!("- {id} => {t:#} (depth {})", t.0.depth().unwrap_or(0))).join("\n"));
            println!("({test_id}, btreemap![{}]),\n", result.iter().map(|(ref id, t)| format!("{id} => \"{t}\"")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}