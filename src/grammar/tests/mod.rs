// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

pub mod prs;
pub mod rts;

use std::collections::{BTreeMap, BTreeSet, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{alt, btreemap, gnode, hashmap, hashset, prule, sym, LL1};
use crate::grammar::NTConversion::Removed;
use crate::log::TryBuildFrom;

// ---------------------------------------------------------------------------------------------

fn is_grtree_empty_symbol(rule: &GrTree) -> bool {
    rule.get_root()
        .and_then(|root| Some(*rule.get(root) == GrNode::Symbol(Symbol::Empty)))
        .unwrap_or(false)
}

// ---------------------------------------------------------------------------------------------

#[test]
fn gnode_macro() {
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
fn prod_macros() {
    assert_eq!(alt!(nt 1, t 2, e), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    assert_eq!(alt!(#128, nt 1, t 2, e), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    assert_eq!(alt!(#L, nt 1, t 2, e), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    // with extra comma:
    assert_eq!(alt!(nt 1, t 2, e,), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    assert_eq!(alt!(#128, nt 1, t 2, e,), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    assert_eq!(alt!(#L, nt 1, t 2, e,), Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));

    assert_eq!(prule!(nt 1, t 2, nt 1, t 3; nt 2; e),
               vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    Alternative::new(vec![sym!(nt  2)]),
                    Alternative::new(vec![sym!(e)])]);
    assert_eq!(prule!(nt 1, t 2, nt 1, t 3; #128, nt 2; e),
               vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    Alternative::new(vec![sym!(nt  2)]).with_flags(128),
                    Alternative::new(vec![sym!(e)])]);
    assert_eq!(prule!(nt 1, t 2, nt 1, t 3; #L, nt 2; e),
               vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    Alternative::new(vec![sym!(nt  2)]).with_flags(128),
                    Alternative::new(vec![sym!(e)])]);
    // with extra semicolon:
    assert_eq!(prule!(nt 1, t 2, nt 1, t 3; nt 2; e;),
               vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    Alternative::new(vec![sym!(nt  2)]),
                    Alternative::new(vec![sym!(e)])]);
    assert_eq!(prule!(nt 1, t 2, nt 1, t 3; #R, nt 2; e;),
               vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    Alternative::new(vec![sym!(nt  2)]).with_flags(256),
                    Alternative::new(vec![sym!(e)])]);
    assert_eq!(prule!(nt 1, t 2, nt 1, t 3; #256, nt 2; e;),
               vec![Alternative::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    Alternative::new(vec![sym!(nt  2)]).with_flags(256),
                    Alternative::new(vec![sym!(e)])]);
    assert_eq!(prule!(%(1, 2), nt 0),
               vec![Alternative::new(vec![Symbol::NT(0)]).with_origin(1, 2)]);
    assert_eq!(prule!(nt 0; %(2, 3), t 0),
               vec![Alternative::new(vec![Symbol::NT(0)]),
                    Alternative::new(vec![Symbol::T(0)]).with_origin(2, 3)]);
    assert_eq!(prule!(#L, %(3, 4), t 1),
               vec![Alternative::new(vec![Symbol::T(1)]).with_flags(ruleflag::L_FORM).with_origin(3, 4)]);
    assert_eq!(prule!(#(1, 2), %(3, 4), t 2),
               vec![Alternative::new(vec![Symbol::T(2)]).with_flags(1).with_ambig_alt_id(2).with_origin(3, 4)]);
    assert_eq!(prule!(nt 0; #L, %(3, 4), t 1),
               vec![Alternative::new(vec![Symbol::NT(0)]),
                    Alternative::new(vec![Symbol::T(1)]).with_flags(ruleflag::L_FORM).with_origin(3, 4)]);
    assert_eq!(prule!(nt 0; #(1, 2), %(3, 4), t 2),
               vec![Alternative::new(vec![Symbol::NT(0)]),
                    Alternative::new(vec![Symbol::T(2)]).with_flags(1).with_ambig_alt_id(2).with_origin(3, 4)]);
}

#[test]
fn symbol_to_str() {
    let mut symtable = SymbolTable::new();
    symtable.extend_terminals([
        ("Arrow".to_string(), Some("->".to_string())),
        ("Colon".to_string(), Some(":".to_string())),
        ("Id".to_string(), None),
    ]);
    symtable.extend_nonterminals((0_u8..26).map(|i| format!("{}", char::from(i + 65))));
    let tests = vec![
        (sym!(t 0), vec!["->", ":0"]),
        (sym!(t 1), vec![":", ":1"]),
        (sym!(t 2), vec!["Id", ":2"]),
        (sym!(nt 0), vec!["A", "0"]),
        (sym!(e), vec!["ε", "ε"]),
    ];
    for (symbol, expected) in tests {
        assert_eq!(symbol.to_str(Some(&symtable)), expected[0], "test on {symbol} has failed");
        assert_eq!(symbol.to_str::<SymbolTable>(None), expected[1], "test on {symbol} has failed");
        let node = GrNode::Symbol(symbol);
        assert_eq!(node.to_str(Some(&symtable)), expected[0], "test on {symbol} has failed");
        assert_eq!(node.to_str(None), expected[1], "test on {symbol} has failed");
    }
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
