#![cfg(test)]

use std::collections::HashMap;
use super::*;
use crate::dfa::TokenId;
use crate::{gnode, hashmap};

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
            tree.0.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            let or = tree.0.add(Some(top), gnode!(|));
            tree.0.addc_iter(Some(or), gnode!(&), [gnode!(nt 7), gnode!(nt 8)]);
            tree.0.add(Some(or), gnode!(nt 9));
        }
        _ => {}
    }
    tree
}

#[test]
fn ruletree_normalize() {
    let tests: Vec<(u32, HashMap<VarId, &str>)> = vec![
        // |([1], [2], 3) (depth 1)
        (0, hashmap![0 => "|([1], [2], 3)"]),
        // |(&(1, 2), |([3], [4]), &(5, 6), |([7], [8], &(9, 10))) (depth 3)
        (1, hashmap![0 => "|(&(1, 2), [3], [4], &(5, 6), [7], [8], &(9, 10))"]),
        // &(&(1, 2), |(3, 4), &(5, 6), |(7, 8)) (depth 2)
        (2, hashmap![0 => "|(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 6, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 6, 8))"]),
        // &(&(1, 2), |(3, 4), &(5, 6), |(&(7, 8), 9)) (depth 3)
        (3, hashmap![0 => "|(&(1, 2, 3, 5, 6, 7, 8), &(1, 2, 3, 5, 6, 9), &(1, 2, 4, 5, 6, 7, 8), &(1, 2, 4, 5, 6, 9))"]),
    ];
    const VERBOSE: bool = true;
    for (test_id, expected) in tests {
        let mut tree = build_tree(test_id);
        if VERBOSE { println!("test {test_id}:\n- 0 -> {tree} (depth {})", tree.0.depth().unwrap()); }
        let new = tree.normalize(0, 1);
        let result = HashMap::from_iter(new.iter().map(|(id, t)| (*id, format!("{t}"))));
        if VERBOSE {
            println!("{}", new.iter().map(|(ref id, t)| format!("- {id} => {t:#} (depth {})", t.0.depth().unwrap())).join("\n"));
            println!("{}", new.iter().map(|(ref id, t)| format!("    => \"{t}\"")).join("\n"));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<HashMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}