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
use crate::rtsgen::RtsGen;
use crate::log::BuildInto;

// ---------------------------------------------------------------------------------------------

fn is_grtree_empty_symbol(rule: &GrTree) -> bool {
    rule.get_root()
        .and_then(|root| Some(*rule.get(root) == GrNode::Symbol(Symbol::Empty)))
        .unwrap_or(false)
}

// ---------------------------------------------------------------------------------------------

pub struct RtsGeneral {}
pub struct RtsNormalized {}
pub struct PrsGeneral {}
pub struct PrsLL1 {}

trait BuildTest {
    type Item;
    fn build_test_rules(id: u32) -> Option<Self::Item>;
}

impl BuildTest for RtsGeneral {
    type Item = RuleTreeSet<General>;

    fn build_test_rules(id: u32) -> Option<Self::Item> {
        let specs = match id {
            // 0xx = basic, &, |, distribution, ?
            // -----------------------------------------------------------------------------
            0 => vec![r#"a -> A;"#],
            1 => vec![r#"a -> A B;"#],
            2 => vec![r#"a -> A | B;"#],
            3 => vec![r#"a -> A B | C;"#],
            4 => vec![r#"a -> (A B) | (C D);"#],
            5 => vec![r#"a -> (A | B) (C | D);"#],
            6 => vec![r#"a -> (A | B) ((C | D) E);"#],
            7 => vec![r#"a -> A?;"#],
            8 => vec![r#"a -> A? B;"#],
            9 => vec![r#"a -> (A B)?;"#],
            10 => vec![r#"a -> (A | B)?;"#],
            11 => vec![r#"a -> A b;"#,
                       r#"b -> B;"#],
            12 => vec![r#"a -> A;"#,
                       r#"b -> B;"#],   // b should be dropped

            // 1xx = +* repetitions: multilevel, and multilevel +* with |
            // -----------------------------------------------------------------------------
            100 => vec![r#"a -> A*;"#],
            101 => vec![r#"a -> A+;"#],
            102 => vec![r#"a -> A B* C;"#],
            103 => vec![r#"a -> A B+ C;"#],
            104 => vec![r#"a -> (A B)*;"#],
            105 => vec![r#"a -> (A B)+;"#],
            106 => vec![r#"a -> (A (B ",")* ";")*;"#],
            107 => vec![r#"a -> (A (B ",")+ ";")+;"#],
            // TODO (not yet fully supported in parsergen)
            150 => vec![r#"a -> (A | B)*;"#],
            151 => vec![r#"a -> (A | B)+;"#],

            // 2xx = +* repetitions: with <L>
            // -----------------------------------------------------------------------------
            200 => vec![r#"a -> (<L=i> A)*;"#],
            201 => vec![r#"a -> (<L=i> A)+;"#],
            202 => vec![r#"a -> (<L=i> A B)*;"#],
            203 => vec![r#"a -> (<L=i> A B)+;"#],
            204 => vec![r#"a -> (<L=i> A (B ",")* ";")*;"#],
            205 => vec![r#"a -> (<L=i> A (B ",")+ ";")+;"#],
            206 => vec![r#"a -> (A (<L=j> B ",")* ";")*;"#],
            207 => vec![r#"a -> (A (<L=j> B ",")+ ";")+;"#],
            208 => vec![r#"a -> (<L=i> A (<L=j> B ",")* ";")*;"#],
            209 => vec![r#"a -> (<L=i> A (<L=j> B ",")+ ";")+;"#],
            // TODO (not yet fully supported in parsergen)
            250 => vec![r#"a -> (<L=i> A | B)*;"#],
            251 => vec![r#"a -> (<L=i> A | B)+;"#],

            // 3xx = rrec: simple
            // -----------------------------------------------------------------------------
            300 => vec![r#"a -> "?" a | "!";"#],

            // 4xx = lrec: <L>
            // -----------------------------------------------------------------------------
            400 => vec![r#"a -> <L> "?" a | "!";"#],

            // 5xx = lrec: simple
            // -----------------------------------------------------------------------------
            500 => vec![r#"a -> a "!" | "?";"#],

            // 6xx = lrec: amb
            // -----------------------------------------------------------------------------
            600 => vec![r#"e -> e "+" e | Num;"#],

            // 7xx = lfact
            // -----------------------------------------------------------------------------
            700 => vec![r#"a -> A B | A C;"#],
            701 => vec![r#"a -> A B C | B B C | B C | B B A;"#],

            // 8xx = combinations
            // -----------------------------------------------------------------------------
            // 0xx and 1xx (or 2xx) normalization
            800 => vec![r#"a -> (A?)*"#],
            801 => vec![r#"a -> (A?)+"#],
            802 => vec![r#"a -> (A*)?"#],
            803 => vec![r#"a -> (A+)?"#],

            // 1xx/2xx and 3xx/4xx rrec
            810 => vec![r#"a -> A* a"#],
            811 => vec![r#"a -> A+ a"#],

            812 => vec![r#"a -> (A a)*"#],
            813 => vec![r#"a -> (A a)+"#],

            // 1xx/2xx and 5xx lrec
            820 => vec![r#"a -> a A* | B"#],
            821 => vec![r#"a -> a A+ | B"#],

            822 => vec![r#"a -> (a A)* | B"#],
            823 => vec![r#"a -> (a A)+ | B"#],

            // 1xx/2xx and 6xx amb
            830 => vec![r#"a -> (a A)* a | B"#],
            831 => vec![r#"a -> (a A)+ a | B"#],

            832 => vec![r#"a -> a (A a)* | B"#],
            833 => vec![r#"a -> a (A a)+ | B"#],

            834 => vec![r#"a -> (a A a)* | B;  // "#],
            // 1xx/2xx and 7xx lfact
            840 => vec![r#"a -> (A B | A C)*"#],
            841 => vec![r#"a -> (A B | A C)+"#],

            842 => vec![r#"a -> A* B* | A* C*"#],
            843 => vec![r#"a -> A+ B+ | A+ C+"#],

            // 3xx/4xx and 7xx
            850 => vec![r#"a -> A B a | A C a | D"#],

            // 5xx and 7xx
            860 => vec![r#"a -> a A B | a A C | D"#],

            // 1yxx = errors
            // -----------------------------------------------------------------------------

            // -----------------------------------------------------------------------------
            _ => return None
        };
        let text = specs.join("\n");
        match RtsGen::new().parse(text.clone()) {
            Ok(rts) => Some(rts),
            Err(log) => panic!("Error while parsing those rules:\n{:-<80}\n{text}\n{:-<80}\nLog:\n{log}\n{:-<80}", "", "", ""),
        }
    }
}

impl BuildTest for RtsNormalized {
    type Item = RuleTreeSet<Normalized>;

    fn build_test_rules(id: u32) -> Option<Self::Item> {
        RtsGeneral::build_test_rules(id).and_then(|rts| Some(rts.build_into()))
    }
}

impl BuildTest for PrsGeneral {
    type Item = ProdRuleSet<General>;

    fn build_test_rules(id: u32) -> Option<Self::Item> {
        RtsNormalized::build_test_rules(id).and_then(|rts| Some(rts.build_into()))
    }
}

impl BuildTest for PrsLL1 {
    type Item = ProdRuleSet<LL1>;

    fn build_test_rules(id: u32) -> Option<Self::Item> {
        PrsGeneral::build_test_rules(id).and_then(|rts| Some(rts.build_into()))
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
