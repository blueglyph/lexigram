// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

mod prs;
mod rts;

use std::collections::{BTreeMap, BTreeSet, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{btreemap, gnode, hashmap, hashset, LL1, prod, prodf, sym};
use crate::grammar::NTConversion::Removed;
use crate::log::TryBuildFrom;

// ---------------------------------------------------------------------------------------------

#[allow(dead_code)]
fn rts_to_str<T>(rules: &RuleTreeSet<T>) -> String {
    rules.get_trees_iter()
        .map(|(id, t)| format!("- {id} => {:#} (depth {})",
                               t.to_str(None, rules.get_symbol_table()), t.depth().unwrap_or(0)))
        .join("\n")
}

fn is_grtree_empty_symbol(rule: &GrTree) -> bool {
    rule.get_root()
        .and_then(|root| Some(*rule.get(root) == GrNode::Symbol(Symbol::Empty)))
        .unwrap_or(false)
}

impl<T> RuleTreeSet<T> {
    fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &GrTree)> {
        self.get_trees_iter().filter(|(_, rule)| !is_grtree_empty_symbol(&rule))
    }
}

impl<T> ProdRuleSet<T> {
    fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &ProdRule)> {
        self.get_prods_iter().filter(|(_, rule)| **rule != prod!(e))
    }
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
    assert_eq!(prodf!(nt 1, t 2, e), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    assert_eq!(prodf!(#128, nt 1, t 2, e), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    assert_eq!(prodf!(#L, nt 1, t 2, e), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    // with extra comma:
    assert_eq!(prodf!(nt 1, t 2, e,), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]));
    assert_eq!(prodf!(#128, nt 1, t 2, e,), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));
    assert_eq!(prodf!(#L, nt 1, t 2, e,), ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(e)]).with_flags(128));

    assert_eq!(prod!(nt 1, t 2, nt 1, t 3; nt 2; e),
               vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    ProdFactor::new(vec![sym!(nt  2)]),
                     ProdFactor::new(vec![sym!(e)])]);
    assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #128, nt 2; e),
               vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    ProdFactor::new(vec![sym!(nt  2)]).with_flags(128),
                    ProdFactor::new(vec![sym!(e)])]);
    assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #L, nt 2; e),
               vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    ProdFactor::new(vec![sym!(nt  2)]).with_flags(128),
                    ProdFactor::new(vec![sym!(e)])]);
    // with extra semicolon:
    assert_eq!(prod!(nt 1, t 2, nt 1, t 3; nt 2; e;),
               vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    ProdFactor::new(vec![sym!(nt  2)]),
                     ProdFactor::new(vec![sym!(e)])]);
    assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #R, nt 2; e;),
               vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    ProdFactor::new(vec![sym!(nt  2)]).with_flags(256),
                    ProdFactor::new(vec![sym!(e)])]);
    assert_eq!(prod!(nt 1, t 2, nt 1, t 3; #256, nt 2; e;),
               vec![ProdFactor::new(vec![sym!(nt 1), sym!(t 2), sym!(nt 1), sym!(t 3)]),
                    ProdFactor::new(vec![sym!(nt  2)]).with_flags(256),
                    ProdFactor::new(vec![sym!(e)])]);
    assert_eq!(prod!(%(1, 2), nt 0),
               vec![ProdFactor::new(vec![Symbol::NT(0)]).with_origin(1, 2)]);
    assert_eq!(prod!(nt 0; %(2, 3), t 0),
               vec![ProdFactor::new(vec![Symbol::NT(0)]),
                    ProdFactor::new(vec![Symbol::T(0)]).with_origin(2, 3)]);
    assert_eq!(prod!(#L, %(3, 4), t 1),
               vec![ProdFactor::new(vec![Symbol::T(1)]).with_flags(ruleflag::L_FORM).with_origin(3, 4)]);
    assert_eq!(prod!(#(1, 2), %(3, 4), t 2),
               vec![ProdFactor::new(vec![Symbol::T(2)]).with_flags(1).with_ambig_factor_id(2).with_origin(3, 4)]);
    assert_eq!(prod!(nt 0; #L, %(3, 4), t 1),
               vec![ProdFactor::new(vec![Symbol::NT(0)]),
                    ProdFactor::new(vec![Symbol::T(1)]).with_flags(ruleflag::L_FORM).with_origin(3, 4)]);
    assert_eq!(prod!(nt 0; #(1, 2), %(3, 4), t 2),
               vec![ProdFactor::new(vec![Symbol::NT(0)]),
                    ProdFactor::new(vec![Symbol::T(2)]).with_flags(1).with_ambig_factor_id(2).with_origin(3, 4)]);
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

// ---------------------------------------------------------------------------------------------
// RuleTreeSet

fn check_rts_sanity<T>(rules: &RuleTreeSet<T>, verbose: bool) -> Option<String> {
    let mut msg = String::new();
    for (var, tree) in rules.get_trees_iter() {
        let mut indices = HashSet::<usize>::new();
        let mut n = 0;
        for node in tree.iter_depth_simple() {
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

pub(crate) fn build_rts(id: u32) -> RuleTreeSet<General> {
    let mut rules = RuleTreeSet::new();
    let tree = rules.get_tree_mut(0);
    let mut extend_nt = true;

    match id {
        0 => { // A -> |(b, c, D)
            let top = tree.addc_iter(None, gnode!(|), [gnode!(t 1), gnode!(t 2), gnode!(nt 3)]);
            tree.set_root(top);
        }
        1 => { // A -> |(&(B, C), d, e, &(F, G), h, i, &(J, K))
            let top = tree.add_root(gnode!(|));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(t 3), gnode!(t 4)]);
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            let or = tree.addc_iter(Some(top), gnode!(|), [gnode!(t 7), gnode!(t 8)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 9), gnode!(nt 10)]);
        }
        2 => { // A -> |(&(B, C, D, F, G, H), &(B, C, D, F, G, I), &(B, C, E, F, G, H), &(B, C, E, F, G, I))
            let top = tree.add_root(gnode!(&));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 7), gnode!(nt 8)]);
        }
        3 => { // A -> |(&(B, C, D, F, G, H), &(B, C, D, F, I), &(B, C, E, F, G, H), &(B, C, E, F, I))
            let top = tree.add_root(gnode!(&));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.add(Some(top), gnode!(nt 5));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 6), gnode!(nt 7)]);
            tree.add(Some(or), gnode!(nt 8));
        }
        4 => { // A -> |(&(A, B, C, D, F, G, H), &(A, B, C, D, F, I), &(A, B, C, E, F, G, H), &(A, B, C, E, F, I))
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
        5 => { // A -> B?
            let top = tree.add_root(gnode!(?));
            tree.add(Some(top), gnode!(nt 1));
        }
        6 => { // A -> (B C)?
            let top = tree.add_root(gnode!(?));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
        }
        7 => { // |(&(B, C), D, ε)
            let top = tree.add_root(gnode!(?));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.add(Some(or), gnode!(nt 3));
        }
        8 => { // A -> c b+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 1));
            tree.addc(Some(cc), gnode!(+), gnode!(t 2));
        }
        9 => { // A -> var (id ,)+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 1));
            let p = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(p), gnode!(&), [gnode!(t 2), gnode!(t 3)]);
            let mut table = SymbolTable::new();
            table.extend_nonterminals(["A".to_string()]);
            table.extend_terminals([
                ("-".to_string(), None), // not used
                ("var".to_string(), Some("var".to_string())),
                ("id".to_string(), None),
                (",".to_string(), Some(",".to_string())),
            ]);
            rules.symbol_table = Some(table);
        }
        10 => { // A -> b (c d|e)+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 1));
            let p = tree.add(Some(cc), gnode!(+));
            let or = tree.add(Some(p), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 2), gnode!(t 3)]);
            tree.add(Some(or), gnode!(t 4));
        }
        11 => { // A -> b c*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 1));
            tree.addc(Some(cc), gnode!(*), gnode!(t 2));
        }
        12 => { // A -> b (c d)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 1));
            let p = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(p), gnode!(&), [gnode!(t 2), gnode!(t 3)]);
        }
        13 => { // A -> b (c d|e)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 1));
            let p = tree.add(Some(cc), gnode!(*));
            let or = tree.add(Some(p), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 2), gnode!(t 3)]);
            tree.add(Some(or), gnode!(t 4));
        }
        14 => { // A -> b? (c|d)?
            let cc = tree.add_root(gnode!(&));
            tree.addc(Some(cc), gnode!(?), gnode!(t 1));
            let m = tree.add(Some(cc), gnode!(?));
            tree.addc_iter(Some(m), gnode!(|), [gnode!(t 2), gnode!(t 3)]);
        }
        15 => { // A -> A (b|c <R>|d) A|e
            let or = tree.add_root(gnode!(|));
            let cc = tree.add(Some(or), gnode!(&));
            tree.add(Some(cc), gnode!(nt 0));
            let or2 = tree.add(Some(cc), gnode!(|));
            tree.add(Some(or2), gnode!(t 1));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 2), gnode!(R)]);
            tree.addc(Some(or2), gnode!(&), gnode!(t 3));
            tree.add(Some(cc), gnode!(nt 0));
            tree.add(Some(or), gnode!(t 4));
        }
        16 => { // A -> A (c)+ b | a
            let or = tree.add_root(gnode!(|));
            let cc1 = tree.addc(Some(or), gnode!(&), gnode!(nt 0));
            tree.addc(Some(cc1), gnode!(+), gnode!(t 2));
            tree.add(Some(cc1), gnode!(t 1));
            tree.add(Some(or), gnode!(t 0));
        }
        17 => { // A -> a ( (b)+ c)+ d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(+));
            let cc2 = tree.add(Some(p1), gnode!(&));
            tree.addc(Some(cc2), gnode!(+), gnode!(t 1));
            tree.add(Some(cc2), gnode!(t 2));
            tree.add(Some(cc), gnode!(t 3));
        }
        18 => { // A -> a | b | c
            let or = tree.add_root(gnode!(|));
            tree.add(Some(or), gnode!(t 0));
            tree.addc(Some(or), gnode!(&), gnode!(t 1));
            tree.add(Some(or), gnode!(t 2));
        }
        19 => { // A -> A (b <L=B>)* c | d
            let or = tree.add_root(gnode!(|));
            let cc = tree.add(Some(or), gnode!(&));
            tree.add(Some(cc), gnode!(nt 0));
            let s1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(s1), gnode!(&), [gnode!(t 1), gnode!(L 1)]);
            tree.add(Some(cc), gnode!(t 2));
            tree.add(Some(or), gnode!(t 3));
        }
        20 => { // A -> A (b)* c | d
            let or = tree.add_root(gnode!(|));
            let cc = tree.add(Some(or), gnode!(&));
            tree.add(Some(cc), gnode!(nt 0));
            tree.addc(Some(cc), gnode!(*), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 2));
            tree.add(Some(or), gnode!(t 3));
        }
        21 => { // A -> a (b)* c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(*), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 2));
            // symbol table defined below
        }
        22 => { // A -> a (b <L=B>)* c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(p1), gnode!(&), [gnode!(t 1), gnode!(L 1)]);
            tree.add(Some(cc), gnode!(t 2));
            let _b_tree = rules.get_tree_mut(1);
            // symbol table defined below
        }
        23 => { // A -> a (b)+ c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(+), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 2));
            // symbol table defined below
        }
        24 => { // A -> a (b <L=B>)+ c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(p1), gnode!(&), [gnode!(t 1), gnode!(L 1)]);
            tree.add(Some(cc), gnode!(t 2));
            // symbol table defined below
        }
        25 => { // A -> a (#)* c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(*), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 2));
            // symbol table defined below
        }
        26 => { // A (c)* b | a (see also RTS(16))
            let or = tree.add_root(gnode!(|));
            let cc1 = tree.addc(Some(or), gnode!(&), gnode!(nt 0));
            tree.addc(Some(cc1), gnode!(*), gnode!(t 2));
            tree.add(Some(cc1), gnode!(t 1));
            tree.add(Some(or), gnode!(t 0));
        }
        27 => { // A -> a (B)+ c ; B -> b
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(+), gnode!(nt 1));
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        28 => { // A -> (a B)+ c ; B -> b
            let cc = tree.add_root(gnode!(&));
            let p1 = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(p1), gnode!(&), [gnode!(t 0), gnode!(nt 1)]);
            // let cc2 = tree.add(Some(p1), gnode!(&));
            // tree.add_iter(Some(cc2), [gnode!(t 0), gnode!(nt 1)]);
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        29 => { // A -> a ( (B b)* c)* d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(*));
            let cc2 = tree.add(Some(p1), gnode!(&));
            let p2 = tree.add(Some(cc2), gnode!(*));
            tree.addc_iter(Some(p2), gnode!(&), [gnode!(nt 1), gnode!(t 1)]);
            tree.add(Some(cc2), gnode!(t 2));
            tree.add(Some(cc), gnode!(t 3));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        30 => { // A -> a ( (B b)+ c)+ d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(+));
            let cc2 = tree.add(Some(p1), gnode!(&));
            let p2 = tree.add(Some(cc2), gnode!(+));
            tree.addc_iter(Some(p2), gnode!(&), [gnode!(nt 1), gnode!(t 1)]);
            tree.add(Some(cc2), gnode!(t 2));
            tree.add(Some(cc), gnode!(t 3));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        32 => { // A -> a (a | c) (b <L=B>)* c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc_iter(Some(cc), gnode!(|), [gnode!(t 0), gnode!(t 2)]);
            let p1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(p1), gnode!(&), [gnode!(t 1), gnode!(L 1)]);
            tree.add(Some(cc), gnode!(t 2));
            // symbol table defined below
        }
        33 => { // A -> (B c)* b | a; B -> b
            let or = tree.add_root(gnode!(|));
            let cc1 = tree.add(Some(or), gnode!(&));
            let p2 = tree.add(Some(cc1), gnode!(*));
            tree.addc_iter(Some(p2), gnode!(&), [gnode!(nt 1), gnode!(t 2)]);
            tree.add(Some(cc1), gnode!(t 1));
            tree.add(Some(or), gnode!(t 0));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        34 => { // A -> a ( (b)+ (b)+ )+ c ( (b)+ (b)+ )+ d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(+));
            let cc2 = tree.add(Some(p1), gnode!(&));
            tree.addc(Some(cc2), gnode!(+), gnode!(t 1));
            tree.addc(Some(cc2), gnode!(+), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 2));
            let p1 = tree.add(Some(cc), gnode!(+));
            let cc2 = tree.add(Some(p1), gnode!(&));
            tree.addc(Some(cc2), gnode!(+), gnode!(t 1));
            tree.addc(Some(cc2), gnode!(+), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 3));
        }
        35 => { // A -> a A | b
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 0), gnode!(nt 0)]);
            tree.add(Some(or), gnode!(t 1));
        }
        36 => { // A -> a A <L=A> | b
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 0), gnode!(nt 0), gnode!(L 0)]);
            tree.add(Some(or), gnode!(t 1));
        }
        37 => { // A -> a (b <L=B>)* C; C -> c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let s1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(s1), gnode!(&), [gnode!(t 1), gnode!(L 1)]);
            tree.add(Some(cc), gnode!(nt 2));
            let _b_tree = rules.get_tree_mut(1);
            let c_tree = rules.get_tree_mut(2);
            c_tree.add_root(gnode!(t 2));
        }
        38 => {
            // A -> A a c? | A b c? | d
            let or = tree.add_root(gnode!(|));
            let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 0), gnode!(t 0)]);
            tree.addc(Some(cc1), gnode!(?), gnode!(t 2));
            let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 0), gnode!(t 1)]);
            tree.addc(Some(cc1), gnode!(?), gnode!(t 2));
            tree.add(Some(or), gnode!(t 3));
        }
        39 => { // A -> a (<L=AIter1> (<L=AIter2> b)* c)* d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(*));
            let cc2 = tree.add(Some(p1), gnode!(&));
            tree.add(Some(cc2), gnode!(L 2));
            let p2 = tree.add(Some(cc2), gnode!(*));
            tree.addc_iter(Some(p2), gnode!(&), [gnode!(L 1), gnode!(t 1)]);
            tree.add(Some(cc2), gnode!(t 2));
            tree.add(Some(cc), gnode!(t 3));
            let _b_tree = rules.get_tree_mut(1);
            let _c_tree = rules.get_tree_mut(2);
        }
        40 => { // A -> a ( (<L=AIter1> b)* c)* d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(*));
            let cc2 = tree.add(Some(p1), gnode!(&));
            let p2 = tree.add(Some(cc2), gnode!(*));
            tree.addc_iter(Some(p2), gnode!(&), [gnode!(L 1), gnode!(t 1)]);
            tree.add(Some(cc2), gnode!(t 2));
            tree.add(Some(cc), gnode!(t 3));
            let _b_tree = rules.get_tree_mut(1);
        }
        41 => { // A -> A x A | A * [(NUM)+] | - A | ID
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 0), gnode!(t 0), gnode!(nt 0)]);
            let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 0), gnode!(t 1), gnode!(t 3)]);
            tree.addc(Some(cc1), gnode!(+), gnode!(t 5));
            tree.add(Some(cc1), gnode!(t 4));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 2), gnode!(nt 0)]);
            tree.add(Some(or), gnode!(t 6));

            let mut table = SymbolTable::new();
            table.extend_nonterminals(["A".to_string()]);
            table.extend_terminals([
                ("CROSS".to_string(), Some("x".to_string())),   // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("NEG".to_string(), Some("-".to_string())),     // 2
                ("LB".to_string(), Some("[".to_string())),      // 3
                ("RB".to_string(), Some("]".to_string())),      // 4
                ("NUM".to_string(), None),                      // 5
                ("ID".to_string(), None),                       // 6
            ]);
            rules.symbol_table = Some(table);
        }
        42 => { // E -> - E | E (* | / <P>) E | E (+ | - <P>) E | ID
            let or = tree.add_root(gnode!(|));
            // - E
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 3), gnode!(nt 0)]);
            // E (* | / <P>) E
            let cc1 = tree.addc(Some(or), gnode!(&), gnode!(nt 0));
            let or2 = tree.addc(Some(cc1), gnode!(|), gnode!(t 0));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 1), gnode!(P)]);
            tree.add(Some(cc1), gnode!(nt 0));
            // E (+ | - <P>) E
            let cc1 = tree.addc(Some(or), gnode!(&), gnode!(nt 0));
            let or2 = tree.addc(Some(cc1), gnode!(|), gnode!(t 2));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 3), gnode!(P)]);
            tree.add(Some(cc1), gnode!(nt 0));
            // ID
            tree.add(Some(or), gnode!(t 4));

            let mut table = SymbolTable::new();
            table.extend_nonterminals(["E".to_string()]);
            table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),   // 0
                ("DIV".to_string(), Some("/".to_string())),   // 1
                ("ADD".to_string(), Some("+".to_string())),   // 2
                ("SUB".to_string(), Some("-".to_string())),   // 3
                ("ID".to_string(), None),                     // 4
            ]);
            rules.symbol_table = Some(table);
        }
        43 => { // E -> - E | <R> E (* | / <P>) E | <R> E (+ | - <P>) E | ID
            let or = tree.add_root(gnode!(|));
            // - E
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 3), gnode!(nt 0)]);
            // <R> E (* | / <P>) E
            let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(R), gnode!(nt 0)]);
            let or2 = tree.addc(Some(cc1), gnode!(|), gnode!(t 0));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 1), gnode!(P)]);
            tree.add(Some(cc1), gnode!(nt 0));
            // <R> E (+ | - <P>) E
            let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(R), gnode!(nt 0)]);
            let or2 = tree.addc(Some(cc1), gnode!(|), gnode!(t 2));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 3), gnode!(P)]);
            tree.add(Some(cc1), gnode!(nt 0));
            // ID
            tree.add(Some(or), gnode!(t 4));

            let mut table = SymbolTable::new();
            table.extend_nonterminals(["E".to_string()]);
            table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),   // 0
                ("DIV".to_string(), Some("/".to_string())),   // 1
                ("ADD".to_string(), Some("+".to_string())),   // 2
                ("SUB".to_string(), Some("-".to_string())),   // 3
                ("ID".to_string(), None),                     // 4
            ]);
            rules.symbol_table = Some(table);
        }
        44 => { // E -> - E | <R> E (* | / <P>) E | E (+ | - <P>) E | ID
            let or = tree.add_root(gnode!(|));
            // - E
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 3), gnode!(nt 0)]);
            // <R> E (* | / <P>) E
            let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(R), gnode!(nt 0)]);
            let or2 = tree.addc(Some(cc1), gnode!(|), gnode!(t 0));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 1), gnode!(P)]);
            tree.add(Some(cc1), gnode!(nt 0));
            // E (+ | - <P>) E
            let cc1 = tree.addc(Some(or), gnode!(&), gnode!(nt 0));
            let or2 = tree.addc(Some(cc1), gnode!(|), gnode!(t 2));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 3), gnode!(P)]);
            tree.add(Some(cc1), gnode!(nt 0));
            // ID
            tree.add(Some(or), gnode!(t 4));

            let mut table = SymbolTable::new();
            table.extend_nonterminals(["E".to_string()]);
            table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),   // 0
                ("DIV".to_string(), Some("/".to_string())),   // 1
                ("ADD".to_string(), Some("+".to_string())),   // 2
                ("SUB".to_string(), Some("-".to_string())),   // 3
                ("ID".to_string(), None),                     // 4
            ]);
            rules.symbol_table = Some(table);
        }
        45 => { // A -> a b | (c | d) | e
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 0), gnode!(t 1)]);
            tree.addc_iter(Some(or), gnode!(|), [gnode!(t 2), gnode!(t 3)]);
            tree.add(Some(or), gnode!(t 4));
        }
        46 => { // A -> (a | b) (c | d)
            let cc = tree.add_root(gnode!(&));
            tree.addc_iter(Some(cc), gnode!(|), [gnode!(t 0), gnode!(t 1)]);
            tree.addc_iter(Some(cc), gnode!(|), [gnode!(t 2), gnode!(t 3)]);
        }
        47 => { // A -> (a | b) (c d | e)*
            let cc = tree.add_root(gnode!(&));
            tree.addc_iter(Some(cc), gnode!(|), [gnode!(t 0), gnode!(t 1)]);
            let s1 = tree.add(Some(cc), gnode!(*));
            let o2 = tree.add(Some(s1), gnode!(|));
            tree.addc_iter(Some(o2), gnode!(&), [gnode!(t 2), gnode!(t 3)]);
            tree.add(Some(o2), gnode!(t 4));
        }
        48 => { // A -> a (b <L=C>)* B; B -> c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(p1), gnode!(&), [gnode!(t 1), gnode!(L 2)]);
            tree.add(Some(cc), gnode!(nt 1));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 2));
            // symbol table defined below
            let _c_tree = rules.get_tree_mut(2);
        }
        50 => { // A -> (a d | B)* c ; B -> b  (NOT SUPPORTED! Users have to split that manually if they need a value for a|B)
            let cc = tree.add_root(gnode!(&));
            let p1 = tree.add(Some(cc), gnode!(*));
            let or2 = tree.add(Some(p1), gnode!(|));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 0), gnode!(t 3)]);
            tree.add(Some(or2), gnode!(nt 1));
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        51 => { // A -> (<L=2> a d | B)* c ; B -> b  (NOT SUPPORTED! Users have to split that manually if they need a value for a|B)
            let cc = tree.add_root(gnode!(&));
            let p1 = tree.add(Some(cc), gnode!(*));
            let o2 = tree.add(Some(p1), gnode!(|));
            tree.addc_iter(Some(o2), gnode!(&), [gnode!(L 2), gnode!(t 0), gnode!(t 3)]);
            tree.add(Some(o2), gnode!(nt 1));
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
            let _b_tree = rules.get_tree_mut(2);
        }
        52 => { // A -> (a d | B)+ c ; B -> b  (NOT SUPPORTED! Users have to split that manually if they need a value for a|B)
            let cc = tree.add_root(gnode!(&));
            let p1 = tree.add(Some(cc), gnode!(+));
            let or2 = tree.add(Some(p1), gnode!(|));
            tree.addc_iter(Some(or2), gnode!(&), [gnode!(t 0), gnode!(t 3)]);
            tree.add(Some(or2), gnode!(nt 1));
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        53 => { // A -> (<L=2> a d | B)+ c ; B -> b  (NOT SUPPORTED! Users have to split that manually if they need a value for a|B)
            let cc = tree.add_root(gnode!(&));
            let p1 = tree.add(Some(cc), gnode!(+));
            let o2 = tree.add(Some(p1), gnode!(|));
            tree.addc_iter(Some(o2), gnode!(&), [gnode!(L 2), gnode!(t 0), gnode!(t 3)]);
            tree.add(Some(o2), gnode!(nt 1));
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
            let _b_tree = rules.get_tree_mut(2);
        }

        54 => { // A -> a (b c)+ d
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(p1), gnode!(&), [gnode!(t 1), gnode!(t 2)]);
            tree.add(Some(cc), gnode!(t 3));
        }
        55 => { // A -> a ( (b c | d)+ e)+ f
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let p1 = tree.add(Some(cc), gnode!(+));
            let cc2 = tree.add(Some(p1), gnode!(&));
            let p2 = tree.add(Some(cc2), gnode!(+));
            let or3 = tree.add(Some(p2), gnode!(|));
            tree.addc_iter(Some(or3), gnode!(&), [gnode!(t 1), gnode!(t 2)]);
            tree.add(Some(or3), gnode!(t 3));
            tree.add(Some(cc2), gnode!(t 4));
            tree.add(Some(cc), gnode!(t 5));
        }
        56 => { // A -> (a b)* a
            let cc = tree.add_root(gnode!(&));
            let star1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(star1), gnode!(&), [gnode!(t 0), gnode!(t 1)]);
            tree.add(Some(cc), gnode!(t 0));
        }
        57 => { // A -> a (b a)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let star1 = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(star1), gnode!(&), [gnode!(t 1), gnode!(t 0)]);
        }
        58 => { // A -> A a | b
            let cc = tree.add_root(gnode!(&));
            let or = tree.add(Some(cc), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 0), gnode!(t 0)]);
            tree.add(Some(or), gnode!(t 1));
        }
        59 => { // A -> a? b* c?
            let cc = tree.add_root(gnode!(&));
            tree.addc(Some(cc), gnode!(?), gnode!(t 0));
            tree.addc(Some(cc), gnode!(*), gnode!(t 1));
            tree.addc(Some(cc), gnode!(?), gnode!(t 2));
        }
        60 => { // A -> a? (b? | c?)*
            let cc = tree.add_root(gnode!(&));
            tree.addc(Some(cc), gnode!(?), gnode!(t 0));
            let star = tree.add(Some(cc), gnode!(*));
            let or1 = tree.add(Some(star), gnode!(|));
            tree.addc(Some(or1), gnode!(?), gnode!(t 1));
            let cc2 = tree.add(Some(or1), gnode!(&));
            tree.addc(Some(cc2), gnode!(?), gnode!(t 2));
        }
        61 => { // A -> a ε?
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(?), gnode!(e));
        }
        62 => { // A -> (a | ε)?
            let cc = tree.add_root(gnode!(&));
            let maybe1 = tree.add(Some(cc), gnode!(?));
            tree.addc_iter(Some(maybe1), gnode!(|), [gnode!(t 0), gnode!(e)]);
        }
        63 => { // A -> a ε*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(*), gnode!(e));
        }
        64 => { // A -> a ε+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(+), gnode!(e));
        }
        65 => { // A -> a b ε c ε
            let cc = tree.add_root(gnode!(&));
            tree.add_iter(Some(cc), [gnode!(t 0), gnode!(t 1), gnode!(e), gnode!(t 2), gnode!(e)]);
        }
        66 => { // A -> a b | c ε | ε | d | <P> ε | <R> <P>
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 0), gnode!(t 1)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 2), gnode!(e)]);
            tree.add_iter(Some(or), [gnode!(e), gnode!(t 3), gnode!(e)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(P), gnode!(e)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(R), gnode!(P)]);
        }
        67 => { // A -> <P> ε | <R> <P>
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(P), gnode!(e)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(R), gnode!(P)]);
        }
        68 => { // A -> <P> ε
            let cc = tree.add_root(gnode!(&));
            tree.add_iter(Some(cc), [gnode!(P), gnode!(e)]);
        }
        69 => { // A -> |(<P> ε)
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(P), gnode!(e)]);
        }
        70 => { // A -> &(ε)
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(e));
        }
        71 => { // A -> |(&(ε))
            let or = tree.add_root(gnode!(|));
            tree.addc(Some(or), gnode!(&), gnode!(e));
        }
        72 => { // A -> a | ε
            let or = tree.add_root(gnode!(|));
            tree.add_iter(Some(or), [gnode!(t 0), gnode!(e)]);
        }
        73 => { // A -> a b?
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(?), gnode!(t 1));
        }
        100 => {
            // lexiparser
            rules = crate::lexi::tests::build_rts();
        }
        101 => {
            extend_nt = false;
            let cc = tree.add_root(gnode!(&));
            tree.add_iter(Some(cc), [gnode!(t 0), gnode!(nt 1)]);
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
            let mut table = SymbolTable::new();
            table.extend_nonterminals(["A".to_string()]);
            rules.symbol_table = Some(table);
        }
        _ => {}
    }
    if rules.symbol_table.is_none() {
        const VERBOSE: bool = false;
        let mut table = SymbolTable::new();
        let mut lforms = btreemap![];
        let mut num_nt: VarId = 0;
        let mut num_t = 0;
        let mut num_iter = 0;
        for (v, t) in rules.get_trees_iter() {
            assert!(v < 26);
            if v >= num_nt { num_nt = v + 1 }
            let mut iter = 1;
            for n in t.iter_depth_simple() {
                match n.deref() {
                    GrNode::Symbol(Symbol::NT(nt)) => {
                        if *nt >= num_nt { num_nt = *nt + 1 }
                    }
                    GrNode::Symbol(Symbol::T(t)) => {
                        if *t >= num_t { num_t = *t + 1 }
                    }
                    GrNode::LForm(nt) if *nt != v => {
                        lforms.insert(*nt, format!("{}Iter{iter}", char::from(v as u8 + 65)));
                        num_iter += 1;
                        iter += 1;
                    }
                    _ => {}
                }
            }
        }
        assert!(extend_nt || lforms.is_empty(), "cannot disable extend_nt when there are lforms");
        if extend_nt {
            let table_num_nt = table.get_num_nt() as VarId;
            if VERBOSE && table_num_nt < num_nt {
                println!("adding {table_num_nt}..{num_nt} NTs to the symbol table");
            }
            table.extend_nonterminals((table_num_nt..num_nt).map(|v| char::from(v as u8 + 65).to_string()));
            if VERBOSE && rules.get_num_nt() < num_nt {
                println!("adding {num_nt}..{} rules to the RuleTreeSet", rules.get_num_nt());
            }
            for v in rules.get_num_nt()..num_nt {   // adds missing NT to avoid error messages in RTS or PRS methods
                let tree = rules.get_tree_mut(v);
                tree.add_root(gnode!(e));
            }
            if VERBOSE {
                println!("rules have become:\n{}", rts_to_str(&rules));
            }
        }
        for (nt, name) in lforms {
            if nt >= num_nt {
                table.extend_nonterminals((num_nt..=nt).map(|v| if v < nt { "???".to_string() } else { name.clone() }));
                num_nt = nt + 1;
                rules.set_tree(nt, GrTree::new());
            } else {
                table.set_nt_name(nt, name);
            }
        }
        if 21 <= id && id <= 25 || id == 27 || id == 32 {
            table.extend_terminals([
                ("a".to_string(), None),
                if id != 25 { ("b".to_string(), None) } else { ("#".to_string(), Some("#".to_string())) },
                ("c".to_string(), None),
            ]);
        } else {
            table.extend_terminals((0..num_t).map(|v| (char::from(v as u8 + 97).to_string(), None)));
        }
        if VERBOSE { println!("RTS({id}): num_nt = {num_nt}, num_t = {num_t}, num_iter = {num_iter}, table = {:?}", table); }
        rules.symbol_table = Some(table);
    }
    rules
}

// ---------------------------------------------------------------------------------------------
// ProdRuleSet

impl<T> ProdRuleSet<T> {
    fn new() -> Self {
        Self {
            prods: Vec::new(),
            origin: Origin::new(),
            num_nt: 0,
            num_t: 0,
            symbol_table: None,
            flags: Vec::new(),
            parent: Vec::new(),
            start: None,
            name: None,
            nt_conversion: HashMap::new(),
            log: BufLog::new(),
            _phantom: PhantomData
        }
    }

    pub(crate) fn print_prs_summary(&self) {
        let factors = self.get_factors().map(|(v, f)| (v, f.clone())).collect::<Vec<_>>();
        print_factors(&factors, self.get_symbol_table());
        let nt_flags = self.flags.iter().index().filter_map(|(nt, &f)|
            if f != 0 { Some(format!("  - {}: {} ({})", Symbol::NT(nt).to_str(self.get_symbol_table()), ruleflag::to_string(f).join(" | "), f)) } else { None }
        ).join("\n");
        let parents = self.parent.iter().index().filter_map(|(c, &par)|
            par.map(|p| format!("  - {} -> {}", Symbol::NT(c).to_str(self.get_symbol_table()), Symbol::NT(p).to_str(self.get_symbol_table())))
        ).join("\n");
        println!("- NT flags:\n{}", if nt_flags.is_empty() { "  - (nothing)".to_string() } else { nt_flags });
        println!("- parents:\n{}", if parents.is_empty() { "  - (nothing)".to_string() } else { parents });
    }

    pub(crate) fn print_logs(&self) {
        println!("{}\n", self.log.get_messages_str());
    }
}

pub fn print_factors<T: SymInfoTable>(factors: &Vec<(VarId, ProdFactor)>, symbol_table: Option<&T>) {
    println!("factors:\n{}",
             factors.iter().enumerate().map(|(id, (v, f))|
                 format!("            // - {id}: {} -> {}{}",
                         Symbol::NT(*v).to_str(symbol_table),
                         f.to_str(symbol_table),
                         if f.flags != 0 { format!("     {} ({})", ruleflag::to_string(f.flags).join(" | "), f.flags) } else { "".to_string() }
                 )
    ).join("\n"));
}

impl<T> From<&ProdRuleSet<T>> for BTreeMap<VarId, ProdRule> {
    fn from(rules: &ProdRuleSet<T>) -> Self {
        rules.get_prods_iter().map(|(var, p)| (var, p.clone())).collect::<BTreeMap<_, _>>()

    }
}

fn print_expected_code(result: &BTreeMap<VarId, ProdRule>) {
    println!("            {}", result.iter().map(|(i, p)|
        format!("{i} => prod!({}),", p.iter()
            .map(|f| format!("{}{}", if f.flags != 0 { format!("#{}, ", f.flags) } else { "".to_string() }, f.iter().map(|s| s.to_macro_item()).join(", ")))
            .join("; "))).join("\n            "))
}

fn map_and_print_first<'a>(first: &'a HashMap<Symbol, HashSet<Symbol>>, symbol_table: Option<&'a SymbolTable>) -> BTreeMap<&'a Symbol, BTreeSet<&'a Symbol>> {
    println!("first: ");
    let b = first.iter().map(|(s, hs)| (s, hs.iter().collect::<BTreeSet<_>>())).collect::<BTreeMap<_, _>>();
    for (sym, set) in &b {
        println!("// {} => {}", sym.to_str(symbol_table), set.iter().map(|s| s.to_str(symbol_table)).join(" "));
    }
    b
}

fn map_and_print_follow<'a>(follow: &'a HashMap<Symbol, HashSet<Symbol>>, symbol_table: Option<&'a SymbolTable>) -> BTreeMap<&'a Symbol, BTreeSet<&'a Symbol>> {
    println!("follow:");
    let b = follow.iter().map(|(s, hs)| (s, hs.iter().collect::<BTreeSet<_>>())).collect::<BTreeMap<_, _>>();
    for (sym, set) in &b {
        println!("// {} => {}", sym.to_str(symbol_table), set.iter().map(|s| s.to_str(symbol_table)).join(" "));
    }
    b
}

fn def_arith_symbols(symbol_table: &mut SymbolTable, has_term: bool) {
    symbol_table.extend_terminals([
        ("SUB".to_string(), Some("-".to_string())),
        ("ADD".to_string(), Some("+".to_string())),
        ("DIV".to_string(), Some("/".to_string())),
        ("MUL".to_string(), Some("*".to_string())),
        ("LPAREN".to_string(), Some("(".to_string())),
        ("RPAREN".to_string(), Some(")".to_string())),
        ("N".to_string(), None),
        ("I".to_string(), None)
    ]);
    symbol_table.extend_nonterminals(["E".to_string()]);
    if has_term {
        symbol_table.extend_nonterminals(["T".to_string()]);
    }
    symbol_table.extend_nonterminals(["F".to_string()]);
}

pub(crate) fn complete_symbol_table(symbol_table: &mut SymbolTable, num_t: usize, num_nt: usize, is_t_data: bool) {
    if symbol_table.get_num_t() == 0 {
        assert!(num_t <= 26);
        symbol_table.extend_terminals((0..num_t).map(|i| (format!("{}", char::from(i as u8 + 97)),
                                                          if is_t_data { None } else { Some(format!("{}", char::from(i as u8 + 97))) })));
    }
    if symbol_table.get_num_nt() == 0 {
        assert!(num_nt <= 26);
        symbol_table.extend_nonterminals((0..num_nt as u8).map(|i| format!("{}", char::from(i + 65))));
    }

}

pub(crate) fn build_prs(id: u32, is_t_data: bool) -> ProdRuleSet<General> {
    let mut rules = ProdRuleSet::new();
    let mut symbol_table = SymbolTable::new();
    let prods = &mut rules.prods;
    let mut start = Some(0);
    let flags = HashMap::<VarId, u32>::new();
    let parents = HashMap::<VarId, VarId>::new();   // (child, parent)
    let mut extend_nt = true;
    match id {
        // misc tests ----------------------------------------------------------
        0 => {
            prods.extend([
                prod!(nt 0, t 1; nt 0, t 2; t 3; t 3, t 4), // A -> A b | A c | d | d e
                prod!(nt 0, t 5; t 6; t 7),                 // B -> A f | g | h
            ]);
        }
        1 => {
            prods.extend([
                // A -> d c | b b c d | d c e | b d e g | b b c | c b | b c g | b d e f
                prod!(
                    t 3, t 2;
                    t 1, t 1, t 2, t 3;
                    t 3, t 2, t 4;
                    t 1, t 3, t 4, t 6;
                    #R, t 1, t 1, t 2;
                    t 2, t 1;
                    t 1, t 2, t 6;
                    t 1, t 3, t 4, t 5;
                )
            ]);
        }
        2 => { // tests empty prod
            prods.extend([]);
        }
        3 => { // tests continue / break consistency in left_factorize
            prods.extend([
                prod!(t 0),
                prod!(t 2, t 0; t 2),
                prod!(t 2; t 1),
                prod!(t 3)]);
        }
        4 => {
            // classical arithmetic grammar
            // T:  0:-, 1:+, 2:/, 3:*, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F
            def_arith_symbols(&mut symbol_table, true);
            prods.extend([
                prod!(nt 0, t 0, nt 1; nt 0, t 1, nt 1; nt 1),  // E -> E + T | E - T | T
                prod!(nt 1, t 2, nt 2; nt 1, t 3, nt 2; nt 2),  // T -> T * F | T / F | F
                prod!(t 4, nt 0, t 5; t 6; t 7),                // F -> ( E ) | NUM | ID
            ]);
        }
        5 => {
            // ε in first propagation
            symbol_table.extend_terminals([
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_nonterminals(["A".to_string(), "A1".to_string(), "A2".to_string()]);
            prods.extend([
                prod!(nt 1, nt 2, t 2, t 2), // A -> A1 A2 ; ;
                prod!(t 0, nt 1; e),         // A1 -> - A1 | ε
                prod!(t 1, nt 2; e),         // A2 -> + A2 | ε
            ]);
        }
        6 => {
            // another starting NT
            symbol_table.extend_terminals([
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_nonterminals(["A1".to_string(), "A".to_string(), "A2".to_string()]);
            prods.extend([
                prod!(t 0, nt 0; e),    // A1 -> - A1 | ε
                prod!(nt 0, nt 2, t 2), // A -> A1 A2 ;     <-- start
                prod!(t 1, nt 2; e),    // A2 -> + A2 | ε
            ]);
            start = Some(1);
        }
        7 => {
            // ε in first propagation
            symbol_table.extend_terminals([
                ("START".to_string(), Some(">".to_string())),
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_nonterminals(["A1".to_string(), "X".to_string(), "A".to_string(), "A2".to_string()]);
            prods.extend([
                prod!(t 1, nt 0; e),    // A1 -> - A1 | ε
                prod!(t 0, nt 2),       // X -> > A         <-- start I
                prod!(nt 0, nt 3, t 3), // A -> A1 A2 ;     <-- start II
                prod!(t 2, nt 3; e),    // A2 -> + A2 | ε
            ]);
            start = Some(1);
        }
        8 => {
            // ambiguous
            prods.extend([
                prod!(nt 0, t 0, nt 0; t 1),    // A -> A a A | b
            ]);
        }
        9 => {
            // simple rules
            prods.extend([
                prod!(t 0, nt 1, t 2),          // A -> a B c
                prod!(t 1; t 3),                // B -> b | d
            ])
        }
        14 => {
            // A -> A A | a
            prods.extend([
                prod!(nt 0, nt 0; t 0)
            ]);
        }
        16 => {
            // A -> B A | b
            // B -> a
            prods.extend([
                prod!(nt 1, nt 0; t 1),
                prod!(t 0)
            ]);
        }
        17 => { // circular dependency (works as long as there's a non-terminal in the loop and an accepting alternative)
            // A -> B | a
            // B -> C ')'
            // C -> '(' A
            symbol_table.extend_terminals([
                ("a".to_string(), Some("a".to_string())),
                ("(".to_string(), Some("(".to_string())),
                (")".to_string(), Some(")".to_string())),
            ]);
            prods.extend([
                prod!(nt 1; t 0),
                prod!(nt 2, t 2),
                prod!(t 1, nt 0),
            ]);
        }
        18 => {
            // A -> a
            prods.extend([
                prod!(t 0),
            ]);
        }
        19 => {
            // A -> a | ε
            prods.extend([
                prod!(t 0; e),
            ]);
        }
        20 => {
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | '}'
            symbol_table.extend_terminals([
                /* 0 */ ("struct".to_string(), Some("struct".to_string())),
                /* 1 */ ("{".to_string(), Some("{".to_string())),
                /* 2 */ ("}".to_string(), Some("}".to_string())),
                /* 3 */ (":".to_string(), Some(":".to_string())),
                /* 4 */ (";".to_string(), Some(";".to_string())),
                /* 5 */ ("id".to_string(), None),
            ]);
            symbol_table.extend_nonterminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prods.extend([
                prod!(t 0, t 5, t 1, nt 1),
                prod!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ]);
        }
        24 => {
            // A -> a (b c)+ d | e
            // =>
            // A -> a B d | e
            // B -> b c B | b c
            prods.extend([
                prod!(t 0, nt 1, t 3; t 4),
                prod!(t 1, t 2, nt 1; t 1, t 2),
            ]);
        }
        25 => {
            // A -> A a b c | A a b d | A a e | f
            prods.extend([
                prod!(nt 0, t 0, t 1, t 2; nt 0, t 0, t 1, t 3; nt 0, t 0, t 4; t 5)
            ]);
        }
        27 => {
            // A -> A a | A b | c | d
            prods.extend([
                prod!(nt 0, t 0; nt 0, t 1; t 2; t 3),
            ]);
        }
        28 => {
            // A -> a | a b | a b c | a b d | e
            prods.extend([
                prod!(t 0; t 0, t 1; t 0, t 1, t 2; t 0, t 1, t 3; t 4),
            ]);
        }
        29 => { // L-form counterpart of #16
            // A -> <L> B A | b
            // B -> a
            prods.extend([
                prod!(nt 1, nt 0; t 1),
                prod!(t 0)
            ]);
            rules.set_flags(0, ruleflag::L_FORM);
        }
        30 => { // L-form counterpart of #20
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> <L> id ':' id ';' LIST | '}'
            symbol_table.extend_terminals([
                /* 0 */ ("struct".to_string(), Some("struct".to_string())),
                /* 1 */ ("{".to_string(), Some("{".to_string())),
                /* 2 */ ("}".to_string(), Some("}".to_string())),
                /* 3 */ (":".to_string(), Some(":".to_string())),
                /* 4 */ (";".to_string(), Some(";".to_string())),
                /* 5 */ ("id".to_string(), None),
            ]);
            symbol_table.extend_nonterminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prods.extend([
                prod!(t 0, t 5, t 1, nt 1),
                prod!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ]);
            rules.set_flags(1, ruleflag::L_FORM);
        }
        31 => {
            // E -> F | E . id
            // F -> id
            prods.extend([
                prod!(nt 1; nt 0, t 0, t 1),
                prod!(t 1),
            ]);
            symbol_table.extend_nonterminals(["E".to_string(), "F".to_string()]);
            symbol_table.extend_terminals([
                (".".to_string(), Some(".".to_string())),
                ("id".to_string(), None),
            ]);
        }
        32 => {
            // E -> F | E . id | E . id ( )
            // F -> id
            prods.extend([
                prod!(nt 1; nt 0, t 0, t 1; nt 0, t 0, t 1, t 2, t 3),
                prod!(t 1),
            ]);
            symbol_table.extend_nonterminals(["E".to_string(), "F".to_string()]);
            symbol_table.extend_terminals([
                (".".to_string(), Some(".".to_string())),
                ("id".to_string(), None),
                ("(".to_string(), Some("(".to_string())),
                (")".to_string(), Some(")".to_string())),
            ]);
        }
        33 => {
            // A -> A a | b c | b d
            prods.extend([
                prod!(nt 0, t 0; t 1, t 2; t 1, t 3),
            ]);
        }
        34 => {
            // S -> id = VAL | exit | return VAL
            // VAL -> id | num
            prods.extend([
                prod!(t 0, t 2, nt 1; t 3; t 4, nt 1),
                prod!(t 0; t 1),
            ]);
            symbol_table.extend_nonterminals(["S".to_string(), "VAL".to_string()]);
            symbol_table.extend_terminals([
                ("id".to_string(), None),
                ("num".to_string(), None),
                ("=".to_string(), Some("=".to_string())),
                ("exit".to_string(), Some("exit".to_string())),
                ("return".to_string(), Some("return".to_string())),
            ]);
        }
        35 => {
            // A -> a | a b b | a c c
            prods.extend([
                prod!(t 0; t 0, t 1, t 1; t 0, t 2, t 2),
            ]);
        }
        36 => {
            // E -> F | num | E . id
            // F -> id
            prods.extend([
                prod!(nt 1; t 2; nt 0, t 0, t 1),
                prod!(t 1),
            ]);
            symbol_table.extend_nonterminals(["E".to_string(), "F".to_string()]);
            symbol_table.extend_terminals([
                (".".to_string(), Some(".".to_string())),
                ("id".to_string(), None),
                ("num".to_string(), None),
            ]);
        }
        37 => {
            // STRUCT -> 'struct' id '{' LIST
            // LIST -> id ':' id ';' LIST | id ';' LIST | '}'
            symbol_table.extend_terminals([
                /* 0 */ ("struct".to_string(), Some("struct".to_string())),
                /* 1 */ ("{".to_string(), Some("{".to_string())),
                /* 2 */ ("}".to_string(), Some("}".to_string())),
                /* 3 */ (":".to_string(), Some(":".to_string())),
                /* 4 */ (";".to_string(), Some(";".to_string())),
                /* 5 */ ("id".to_string(), None),
            ]);
            symbol_table.extend_nonterminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prods.extend([
                prod!(t 0, t 5, t 1, nt 1),
                prod!(t 5, t 3, t 5, t 4, nt 1; t 5, t 4, nt 1 ; t 2),
            ]);
        }
        38 => {
            // A -> A a | A b | b c | b d
            prods.extend([
                prod!(nt 0, t 0; nt 0, t 1; t 1, t 2; t 1, t 3),
            ]);
        }
        39 => {
            // A -> A a b | A a c | b c | b d
            prods.extend([
                prod!(nt 0, t 0, t 1; nt 0, t 0, t 2; t 1, t 2; t 1, t 3),
            ]);
        }
        40 => {
            // A -> a A | b
            prods.extend([
                prod!(t 0, nt 0; t 1),
            ]);
        }
        41 => {
            // A -> a A <L> | b (by explicitly setting the l-form flag)
            prods.extend([
                prod!(t 0, nt 0; t 1),
            ]);
            rules.set_flags(0, ruleflag::L_FORM);
        }
        42 => {
            // A -> a A <L> | b (l-form set through the ProdFactor flags)
            prods.extend([
                prod!(#L, t 0, nt 0; t 1),
            ]);
        }
        43 => {
            // BATCH -> GROUP ';' BATCH <L> | ε
            // GROUP -> '[' EXPR ']' | '(' EXPR ')'
            // EXPR -> FACTOR '*' FACTOR;
            // FACTOR -> id | int | '(' EXPR ')';
            symbol_table.extend_terminals([
                ("[".to_string(), Some("[".to_string())),   // 0
                ("]".to_string(), Some("]".to_string())),   // 1
                ("(".to_string(), Some("(".to_string())),   // 2
                (")".to_string(), Some(")".to_string())),   // 3
                ("*".to_string(), Some("*".to_string())),   // 4
                ("id".to_string(), None),                   // 5
                ("int".to_string(), None),                  // 6
                (";".to_string(), Some(";".to_string())),   // 7
            ]);
            symbol_table.extend_nonterminals([
                "BATCH".to_string(),                        // 0
                "GROUP".to_string(),                        // 1
                "EXPR".to_string(),                         // 2
                "FACTOR".to_string(),                       // 3
            ]);
            prods.extend([
                prod!(#L, nt 1, t 7, nt 0; e),
                prod!(t 0, nt 2, t 1; t 2, nt 2, t 3),
                prod!(nt 3, t 4, nt 3),
                prod!(t 5; t 6; t 2, nt 2, t 3),
            ]);
        }

        // fixes ---------------------------------------------------------------
        44 => {
            // PRS(63) generated strange code for the rrec E5 -> E6 ^ E5 | E6:
            // we compare here rrec and rrec + lfact, with another NT that has value

            // A -> B a A | B
            // B -> b
            prods.extend([
                prod!(nt 1, t 0, nt 0; nt 1),
                prod!(t 1),
            ])
        }
        45 => {
            // A -> a A | B
            // B -> b
            prods.extend([
                prod!(t 0, nt 0; nt 1),
                prod!(t 1),
            ])
        }
        46 => {
            // A -> A B a | B
            // B -> b
            prods.extend([
                prod!(nt 0, nt 1, t 0; nt 1),
                prod!(t 1),
            ])
        }
        47 => {
            // same as 44 with <L>
            // A -> B a A <L> | B
            // B -> b
            prods.extend([
                prod!(#L, nt 1, t 0, nt 0; nt 1),
                prod!(t 1),
            ])
        }
        48 => {
            // A -> a A <L> | B
            // B -> b
            prods.extend([
                prod!(#L, t 0, nt 0; nt 1),
                prod!(t 1),
            ])
        }

        // ambiguous grammar reconstruction tests ------------------------------
        50 => {
            // classical ambiguous arithmetic grammar
            // E -> E '^' E | E '*' E | E '+' E | F;
            // F -> ( E ) | NUM | ID
            symbol_table.extend_terminals([
                ("ABS".to_string(), Some("abs".to_string())),   // 0
                ("NEG".to_string(), Some("-".to_string())),     // 1
                ("EXP".to_string(), Some("^".to_string())),     // 2
                ("MUL".to_string(), Some("*".to_string())),     // 3
                ("ADD".to_string(), Some("+".to_string())),     // 4
                ("LPAREN".to_string(), Some("(".to_string())),  // 5
                ("RPAREN".to_string(), Some(")".to_string())),  // 6
                ("NUM".to_string(), None),                      // 7
                ("ID".to_string(), None)                        // 8
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            symbol_table.extend_nonterminals(["F".to_string()]);   // 1
            prods.extend([
                prod!(nt 0, t 2, nt 0; nt 0, t 3, nt 0; nt 0, t 4, nt 0; nt 1),
                prod!(t 5, nt 0, t 6; t 7; t 8),
            ]);
        }
        51 => {
            // classical ambiguous arithmetic grammar
            // E -> 'abs' E | E '^' E | E '\'' | E '*' E | '-' E | E '+' E | F;
            // F -> ( E ) | NUM | ID
            symbol_table.extend_terminals([
                ("ABS".to_string(), Some("abs".to_string())),   // 0
                ("NEG".to_string(), Some("-".to_string())),     // 1
                ("EXP".to_string(), Some("^".to_string())),     // 2
                ("MUL".to_string(), Some("*".to_string())),     // 3
                ("ADD".to_string(), Some("+".to_string())),     // 4
                ("LPAREN".to_string(), Some("(".to_string())),  // 5
                ("RPAREN".to_string(), Some(")".to_string())),  // 6
                ("NUM".to_string(), None),                      // 7
                ("ID".to_string(), None),                       // 8
                ("PRIME".to_string(), Some("'".to_string())),   // 9
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            symbol_table.extend_nonterminals(["F".to_string()]);   // 1
            prods.extend([
                prod!(t 0, nt 0;
                    nt 0, t 2, nt 0;
                    nt 0, t 9;
                    nt 0, t 3, nt 0;
                    t 1, nt 0;
                    nt 0, t 4, nt 0;
                    nt 1),
                prod!(t 5, nt 0, t 6; t 7; t 8),
            ]);
        }
        52 => {
            // lrec chain:
            // E -> E * E | E ! | E ' | E + E | F;
            // F -> NUM | ID
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("FAC".to_string(), Some("!".to_string())),     // 1
                ("PRIME".to_string(), Some("'".to_string())),   // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("NUM".to_string(), None),                      // 4
                ("ID".to_string(), None),                       // 5
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            symbol_table.extend_nonterminals(["F".to_string()]);   // 1
            prods.extend([
                prod!(nt 0, t 0, nt 0; nt 0, t 1; nt 0, t 2; nt 0, t 3, nt 0; nt 1),
                prod!(t 4; t 5),
            ]);
        }
        53 => {
            // E -> E ^ E <R> | E * E <R> | - E | E + E | F
            // F ->  ID | NUM | ( E )
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),     // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("NEG".to_string(), Some("-".to_string())),     // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
                ("LPAREN".to_string(), Some("(".to_string())),  // 6
                ("RPAREN".to_string(), Some(")".to_string())),  // 7
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
                "F".to_string(),        // 1
            ]);
            prods.extend([
                prod!(#R, nt 0, t 0, nt 0; #R, nt 0, t 1, nt 0; t 2, nt 0; nt 0, t 3, nt 0; nt 1),
                prod!(t 4; t 5; t 6, nt 0, t 7),
            ]);
        }
        54 => {
            // E -> E * E <R> | E ! | E -- | E + E <R> | ID | NUM
            symbol_table.extend_terminals([
                ("FACT".to_string(), Some("!".to_string())),    // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("DEC".to_string(), Some("--".to_string())),    // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(#R, nt 0, t 1, nt 0; nt 0, t 0; nt 0, t 2; #R, nt 0, t 3, nt 0; t 4; t 5)
            ]);
        }
        55 => {
            // E -> E * E | E -- | ! E | E + E | ID | NUM
            symbol_table.extend_terminals([
                ("FACT".to_string(), Some("!".to_string())),    // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("DEC".to_string(), Some("--".to_string())),    // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 1, nt 0; nt 0, t 2; t 0, nt 0; nt 0, t 3, nt 0; t 4; t 5)
            ]);
        }
        56 => {
            // E -> E * E | ! E | E -- | E + E | ID | NUM
            symbol_table.extend_terminals([
                ("FACT".to_string(), Some("!".to_string())),    // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("DEC".to_string(), Some("--".to_string())),    // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
                ("NUM".to_string(), None),                      // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 1, nt 0; t 0, nt 0; nt 0, t 2; nt 0, t 3, nt 0; t 4; t 5)
            ]);
        }
        57 => {
            // E -> E ^ E | E * E | E + E | ID | NUM
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),     // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("ID".to_string(), None),                       // 3
                ("NUM".to_string(), None),                      // 4
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 0, nt 0; nt 0, t 1, nt 0; nt 0, t 2, nt 0; t 3; t 4)
            ]);
        }
        58 => {
            // E -> E + | - E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 0; t 1, nt 0; t 2)
            ])
        }
        59 => {
            // E -> E + E | - E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 0, nt 0; t 1, nt 0; t 2)
            ])
        }
        60 => {
            // E -> E + | <L> - E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 0; #L, t 1, nt 0; t 2)
            ])
        }
        61 => {
            // compare to 58
            // E -> E + | - E | 0 | 1
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
                ("ONE".to_string(), Some("1".to_string())),     // 3
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 0; t 1, nt 0; t 2; t 3)
            ])
        }
        62 => {
            // E -> E * E | - E | E + E | ID;
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("NEG".to_string(), Some("-".to_string())),     // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("ID".to_string(), None),                       // 3
            ]);
            symbol_table.extend_nonterminals(["E".to_string()]);   // 0
            prods.extend([
                prod!(nt 0, t 0, nt 0; t 1, nt 0; nt 0, t 2, nt 0; t 3),
            ]);
        }
        63 => {
            // E -> <R> E ^ E | E * E | - E | E + E | ID;
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),     // 0
                ("MUL".to_string(), Some("*".to_string())),     // 1
                ("NEG".to_string(), Some("-".to_string())),     // 2
                ("ADD".to_string(), Some("+".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
                // "E3".to_string(),       // 1
                // "E5".to_string(),       // 2
                // "E6".to_string(),       // 3
            ]);
            prods.extend([
                prod!(#R, nt 0, t 0, nt 0; nt 0, t 1, nt 0; t 2, nt 0; nt 0, t 3, nt 0; t 4),
            ]);
        }
        64 => {
            // E -> - E | E + E | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(t 1, nt 0; nt 0, t 0, nt 0; t 2)
            ])
        }
        65 => {
            // E -> E ! | E * E | E + | - E | ID
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("FACT".to_string(), Some("!".to_string())),    // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("SUB".to_string(), Some("-".to_string())),     // 3
                ("ID".to_string(), None),                       // 4
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 1; nt 0, t 0, nt 0; nt 0, t 2; t 3, nt 0; t 4),
            ])
        }
        66 => {
            // left factorization issues:
            // E -> E . * E | E -- | E . + E | ! E | ID
            symbol_table.extend_terminals([
                ("MUL".to_string(), Some("*".to_string())),     // 0
                ("DEC".to_string(), Some("--".to_string())),    // 1
                ("ADD".to_string(), Some("+".to_string())),     // 2
                ("NOT".to_string(), Some("!".to_string())),     // 3
                ("DOT".to_string(), Some(".".to_string())),      // 4
                ("ID".to_string(), None),                       // 5
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(nt 0, t 4, t 0, nt 0; nt 0, t 1; nt 0, t 4, t 2, nt 0; t 3, nt 0; t 5)
            ]);
        }

        // test of mixed recursions --------------------------------------------
        70 => {
            // E -> - E | E + | 0
            symbol_table.extend_terminals([
                ("ADD".to_string(), Some("+".to_string())),     // 0
                ("SUB".to_string(), Some("-".to_string())),     // 1
                ("ZERO".to_string(), Some("0".to_string())),    // 2
            ]);
            symbol_table.extend_nonterminals([
                "E".to_string(),        // 0
            ]);
            prods.extend([
                prod!(t 1, nt 0; nt 0, t 0; t 2)
            ])
        }

        // ambiguity? ----------------------------------------------------------
        100 => {
            // A -> A a A b | c (amb removed)
            prods.extend([
                prod!(nt 0, t 0, nt 0, t 1; t 2),
            ]);
        }
        101 => {
            // A -> a A A | b (no amb)
            prods.extend([
                prod!(t 0, nt 0, nt 0; t 1),
            ]);
        }
        102 => {
            // A -> A a A b A | c (amb removed)
            prods.extend([
                prod!(nt 0, t 0, nt 0, t 1, nt 0; t 2)
            ]);
        }
        103 => {
            // A -> a (A b A)* c | d (no amb)
            // =>
            // A -> a B c | d
            // B -> A b A B | ε
            prods.extend([
                prod!(t 0, nt 1, t 2; t 3),
                prod!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        104 => {
            // A -> (A b A)* c | a (amb A:a, B:c)
            // =>
            // A -> B c | a
            // B -> A b A B | ε
            prods.extend([
                prod!(nt 1, t 2; t 0),
                prod!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        105 => {
            // A -> a (A b A)* | c (amb)
            // =>
            // A -> a B | c
            // B -> A b A B | ε
            prods.extend([
                prod!(t 0, nt 1; t 2),
                prod!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        106 => {
            // A -> (A b A)* | a (very amb)
            // =>
            // A -> A1 | a
            // A1 -> A b A A1 | ε
            prods.extend([
                prod!(nt 1; t 0),
                prod!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }

        // warnings and errors -------------------------------------------------
        1000 => { // A -> A a  (error: missing non-recursive factor)
            prods.extend([
                prod!(nt 0, t 0)
            ]);
        },
        1001 => { // A -> A a A A | b (error: cannot remove recursion)
            prods.extend([
                prod!(nt 0, t 0, nt 0, nt 0; t 1)
            ]);
        },
        1002 => { // A -> A a A a A | b (warning: ambiguous)
            prods.extend([
                prod!(nt 0, t 0, nt 0, t 0, nt 0; t 1)
            ]);
        },
        1003 => { // (error: no terminal in grammar)
            prods.extend([
                prod!(nt 1),
                prod!(nt 2),
                prod!(nt 0),
            ]);
        },
        1004 => { // (error: no terminal used in table)
            prods.extend([
                prod!(nt 1),
                prod!(nt 2, t 0),
                prod!(nt 0),
            ]);
        },
        1005 => { // (warnings: unused terminals, unused non-terminals)
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_nonterminals(["A".to_string(), "B".to_string()]);
            prods.extend([
                prod!(t 1),
                prod!(t 1),
            ]);
        },
        1006 => { // symbol_table.num_nt != rules.num_nt
            extend_nt = false;
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_nonterminals(["A".to_string()]);
            prods.extend([
                prod!(t 0, nt 1),
                prod!(t 1),
            ])
        }
        _ => {}
    };
    for (v, f) in flags {
        rules.set_flags(v, f);
    }
    for (child, parent) in parents {
        rules.set_parent(child, parent);
    }
    rules.calc_num_symbols();
    let calc_num_nt = if extend_nt { rules.num_nt } else { symbol_table.get_num_nt() };
    complete_symbol_table(&mut symbol_table, rules.num_t, calc_num_nt, is_t_data);
    rules.set_symbol_table(symbol_table);
    if let Some(start) = start {
        rules.set_start(start);
    }
    rules
}

// ---------------------------------------------------------------------------------------------
// Tests the chain from RTS to LL1 grammar

fn build_ll1_from_rts(id: u32) -> ProdRuleSet<LL1> {
    let mut rts = RuleTreeSet::new();
    let mut symbol_table = SymbolTable::new();
    let start = Some(0);
    let mut tree = vec![GrTree::new()];
    match id {
        100 => {
            // A -> a b | c
            let or = tree[0].add_root(gnode!(|));
            let cc = tree[0].add(Some(or), gnode!(&));
            tree[0].add_iter(Some(cc), [gnode!(t 0), gnode!(t 1)]);
            tree[0].add(Some(or), gnode!(t 2));
        }
        102 => {
            // A -> a (b B | c)+
            // B -> a
            tree.push(GrTree::new());
            let cc = tree[0].add_root(gnode!(&));
            tree[0].add(Some(cc), gnode!(t 0));
            let p = tree[0].add(Some(cc), gnode!(+));
            let or = tree[0].add(Some(p), gnode!(|));
            tree[0].addc_iter(Some(or), gnode!(&), [gnode!(t 1), gnode!(nt 1)]);
            tree[0].add(Some(or), gnode!(t 2));
            tree[1].add_root(gnode!(t 0));
        }
        _ => {}
    }
    let num_nt = tree.len();
    for (i, t) in tree.into_iter().index() {
        rts.set_tree(i, t);
    }
    let num_t = rts.get_terminals().iter().max().map(|n| *n + 1).unwrap_or(0);
    if symbol_table.get_num_t() == 0 {
        symbol_table.extend_terminals((0..num_t).map(|i| (format!("{}", char::from(i as u8 + 97)), None)));
    }
    if symbol_table.get_num_nt() == 0 {
        assert!(num_nt <= 26);
        symbol_table.extend_nonterminals((0..num_nt as u8).map(|i| format!("{}", char::from(i + 65))));
    }
    rts.set_symbol_table(symbol_table);
    if let Some(start) = start {
        rts.set_start(start);
    }

    let rules = ProdRuleSet::<General>::build_from(rts);
    ProdRuleSet::<LL1>::build_from(rules)
}

#[test]
fn rts_prs() {
    let tests = vec![
        (100, 0, vec![
            // - 0: A -> a b
            // - 1: A -> c
            (0, prodf!(t 0, t 1)),
            (0, prodf!(t 2)),
        ]),
        (102, 0, vec![
            // A -> A a A b A | c
            //
            // - 0: A -> a A_1
            // - 1: B -> a
            // - 2: A_1 -> b B A_2
            // - 3: A_1 -> c A_3
            // - 4: A_2 -> A_1
            // - 5: A_2 -> ε
            // - 6: A_3 -> A_1
            // - 7: A_3 -> ε
            (0, prodf!(t 0, nt 2)),
            (1, prodf!(t 0)),
            (2, prodf!(t 1, nt 1, nt 3)),
            (2, prodf!(t 2, nt 4)),
            (3, prodf!(nt 2)),
            (3, prodf!(e)),
            (4, prodf!(nt 2)),
            (4, prodf!(e)),
        ])
    ];
    const VERBOSE: bool = false;
    for (ll_id, start, expected_factors) in tests {
        let mut ll1 = build_ll1_from_rts(ll_id);
        ll1.set_start(start);
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        let parsing_table = ll1.calc_table(&first, &follow, false);
        let LLParsingTable { factors, .. } = &parsing_table;
        if VERBOSE {
            print_factors(&factors, ll1.get_symbol_table());
            println!("{}",
                     factors.iter().enumerate().map(|(_id, (v, f))|
                         format!("            ({v}, prodf!({})),", f.iter().map(|s| s.to_macro_item()).join(", "))
                     ).join("\n"));
        }
        assert_eq!(*factors, expected_factors, "test {ll_id}/{start} failed");
    }
}

// ---------------------------------------------------------------------------------------------
// Pre-parser

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub(crate) enum T { RTS(u32), PRS(u32) }

impl T {
    /// Build a PRS from RTS or PRS rules, does not verify if there are errors in the log
    pub(crate) fn try_build_prs(&self, start_nt: VarId, is_t_data: bool) -> ProdRuleSet<LL1> {
        const VERBOSE: bool = false;
        let mut ll1 = match self {
            T::RTS(id) => {
                let mut rts = build_rts(*id);
                if rts.get_symbol_table().is_none() {
                    let num_nt = rts.trees.len();
                    let num_t = rts.get_terminals().iter().map(|token| *token as usize).max().unwrap_or(0) + 1;
                    let mut symbol_table = SymbolTable::new();
                    complete_symbol_table(&mut symbol_table, num_t, num_nt, is_t_data);
                    rts.set_symbol_table(symbol_table);
                }
                let rules = ProdRuleSet::build_from(rts);
                if VERBOSE {
                    print!("General rules\n- ");
                    rules.print_prs_summary();
                }
                ProdRuleSet::<LL1>::build_from(rules)
            }
            T::PRS(id) => {
                let general = build_prs(*id, is_t_data);
                if VERBOSE {
                    print!("General rules\n- ");
                    general.print_prs_summary();
                }
                ProdRuleSet::<LL1>::build_from(general)
            }
        };
        ll1.set_start(start_nt);
        ll1
    }

    /// Build a PRS from RTS or PRS rules and verifies there are no errors in the log
    pub(crate) fn build_prs(&self, test_id: usize, start_nt: VarId, is_t_data: bool) -> ProdRuleSet<LL1> {
        let ll1 = self.try_build_prs(start_nt, is_t_data);
        assert_eq!(ll1.get_log().num_errors(), 0, "test {test_id}/{self:?}/{start_nt} failed:\n- {}", ll1.get_log().get_errors().join("\n- "));
        ll1
    }
}

#[test]
fn rts_prs_flags() {
    let tests: Vec<(T, VarId, BTreeMap<VarId, u32>, BTreeMap<usize, u32>, BTreeMap<VarId, VarId>, BTreeMap<VarId, NTConversion>)> = vec![
        (T::RTS(9), 0,btreemap![0 => 6144, 1 => 4129, 2 => 64],     // NT flags
         btreemap![],                                               // factor flags
         btreemap![1 => 0, 2 => 1],                                 // parents
         btreemap![]),
        (T::RTS(11), 0, btreemap![0 => 2048, 1 => 1],
         btreemap![],
         btreemap![1 => 0],
         btreemap![]),
        (T::RTS(12), 0, btreemap![0 => 2048, 1 => 1],
         btreemap![],
         btreemap![1 => 0],
         btreemap![]),
        (T::RTS(15), 0, btreemap![0 => 1536, 1 => 4, 2 => 512, 3 => 4],
         btreemap![2 => 256, 6 => 8192, 7 => 8448],
         btreemap![1 => 0, 2 => 0, 3 => 2, 4 => 0],
         btreemap![]),
        (T::RTS(16), 0, btreemap![0 => 6656, 1 => 4129, 2 => 4, 3 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 0, 3 => 1],
         btreemap![]),
        (T::RTS(17), 0, btreemap![0 => 6144, 1 => 4129, 2 => 6177, 3 => 64, 4 => 64],
         btreemap![],
         btreemap![1 => 2, 2 => 0, 3 => 1, 4 => 2],
         btreemap![]),
        (T::RTS(18), 0, btreemap![], btreemap![], btreemap![],
         btreemap![]),
        // 0: A (b <L=B>)* c | d       0: A -> d A_1
        // 1: (B)                  ->  1: AIter1 -> b AIter1 | ε
        // 2:                          2: A_1 -> AIter1 c A_1 | ε
        // - NT flags:
        //   - A: parent_left_rec | parent_+_or_* (2560)
        //   - AIter1: child_+_or_* | L-form (129)
        //   - A_1: child_left_rec (4)
        // - parents:
        //   - AIter1 -> A
        //   - A_1 -> A
        (T::RTS(19), 0, btreemap![0 => 2560, 1 => 129, 2 => 4],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![]),
        (T::RTS(20), 0, btreemap![0 => 2560, 1 => 1, 2 => 4],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![]),
        (T::RTS(29), 0, btreemap![0 => 2048, 2 => 1, 3 => 2049],
         btreemap![],
         btreemap![2 => 3, 3 => 0],
         btreemap![]),
        (T::RTS(35), 0, btreemap![0 => 2],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::RTS(36), 0, btreemap![0 => 130],
         btreemap![],
         btreemap![],
         btreemap![]),
        // 0: A -> a (b <L=AIter1>)* C      0: A -> a AIter1 C
        // 1: (AIter1)                  ->  1: C -> c
        // 2: C -> c                        2: AIter1 -> b AIter1 | ε
        // - NT flags:
        //   - A: parent_+_or_* (2048)
        //   - AIter1: child_+_or_* | L-form (129)
        // - parents:
        //   - AIter1 -> A
        (T::RTS(37), 0, btreemap![0 => 2048, 2 => 129],
         btreemap![],
         btreemap![2 => 0],
         btreemap![1 => MovedTo(2), 2 => MovedTo(1)]),
        // - 0: A -> a AIter1 d
        // - 1: AIter2 -> b AIter2
        // - 2: AIter2 -> ε
        // - 3: AIter1 -> AIter2 c AIter1
        // - 4: AIter1 -> ε
        // - NT flags:
        //   - A: parent_+_or_* (2048)
        //   - AIter2: child_+_or_* | L-form (129)
        //   - AIter1: child_+_or_* | L-form | parent_+_or_* (2177)
        // - parents:
        //   - AIter2 -> AIter1
        //   - AIter1 -> A
        (T::RTS(39), 0, btreemap![0 => 2048, 1 => 129, 2 => 2177],
         btreemap![],
         btreemap![1 => 2, 2 => 0],
         btreemap![]),
        (T::PRS(0), 0, btreemap![0 => 544, 1 => 4, 2 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![1 => Removed]),
        (T::PRS(1), 0, btreemap![0 => 32, 1 => 96, 2 => 64, 3 => 64, 4 => 64],
         btreemap![9 => 256],
         btreemap![1 => 0, 2 => 0, 3 => 1, 4 => 1],
         btreemap![]),
        (T::PRS(25), 0, btreemap![0 => 512, 1 => 36, 2 => 96, 3 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 1, 3 => 2],
         btreemap![]),
        (T::PRS(28), 0, btreemap![0 => 32, 1 => 96, 2 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 1],
         btreemap![]),
        (T::PRS(31), 0, btreemap![0 => 512, 2 => 4],
         btreemap![],
         btreemap![2 => 0],
         btreemap![]),
        (T::PRS(32), 0, btreemap![0 => 512, 2 => 36, 3 => 64],
         btreemap![],
         btreemap![2 => 0, 3 => 2],
         btreemap![]),
        (T::PRS(33), 0, btreemap![0 => 544, 1 => 4, 2 => 64],
         btreemap![],
         btreemap![1 => 0, 2 => 0],
         btreemap![]),
        (T::PRS(40), 0, btreemap![0 => 2],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::PRS(41), 0, btreemap![0 => 130],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::PRS(42), 0, btreemap![0 => 130],
         btreemap![],
         btreemap![],
         btreemap![]),
        (T::PRS(44), 0, btreemap![0 => 34, 2 => 64],
         btreemap![],
         btreemap![2 => 0],
         btreemap![]),
        /*
        (T::PRS(), 0, btreemap![], btreemap![], btreemap![], btreemap![]),
        */
    ];
    const VERBOSE: bool = false;
    const VERBOSE_DETAILS: bool = false;
    for (test_id, (rule_id, start_nt, expected_flags, expected_fflags, expected_parent, expected_nt_conversion)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\nTest {test_id}: rules {rule_id:?}, start {start_nt}:", ""); }
        let mut ll1 = rule_id.build_prs(test_id, start_nt, true);
        if VERBOSE && VERBOSE_DETAILS {
            print!("Before table creation:\n- ");
            ll1.print_prs_summary();
        }
        let _parsing_table = ll1.make_parsing_table(false);
        if VERBOSE && ll1.log.num_warnings() + ll1.log.num_notes() > 0 {
            ll1.print_logs();
        }
        let result_flags = ll1.flags.iter().index().filter_map(|(v, &f)| if f != 0 { Some((v, f)) } else { None }).collect::<BTreeMap<_, _>>();
        let result_fflags = ll1.prods.iter().flat_map(|p| p.iter().map(|f| f.flags)).enumerate().filter_map(|(i, f)| if f != 0 { Some((i, f)) } else { None }).collect::<BTreeMap<_, _>>();
        let result_parent = ll1.parent.iter().index().filter_map(|(v, &par)| if let Some(p) = par { Some((v, p)) } else { None }).collect::<BTreeMap<_, _>>();
        let result_nt_conversion = ll1.nt_conversion.iter().map(|(v1, v2)| (*v1, *v2)).collect::<BTreeMap<_, _>>();
        if VERBOSE {
            print!("- ");
            ll1.print_prs_summary();
            println!("=>");
            println!("        (T::{rule_id:?}, {start_nt}, btreemap![{}],", result_flags.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            println!("         btreemap![{}],", result_fflags.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            println!("         btreemap![{}],", result_parent.iter().map(|(v, f)| format!("{v} => {f}")).join(", "));
            println!("         btreemap![{}]),", result_nt_conversion.iter().map(|(v1, v2)| format!("{v1} => {v2:?}")).join(", "));
        }
        assert_eq!(result_flags, expected_flags, "test {test_id}/{rule_id:?}/{start_nt} failed");
        assert_eq!(result_fflags, expected_fflags, "test {test_id}/{rule_id:?}/{start_nt} failed");
        assert_eq!(result_parent, expected_parent, "test {test_id}/{rule_id:?}/{start_nt} failed");
        assert_eq!(result_nt_conversion, expected_nt_conversion, "test {test_id}/{rule_id:?}/{start_nt} failed");
    }
}
