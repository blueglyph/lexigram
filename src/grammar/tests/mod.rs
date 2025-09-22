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

#[derive(Clone, Copy, Debug)]
pub struct TestRules(u32);

#[allow(unused)]
impl TestRules {
    pub fn to_rts_general(self) -> Option<RuleTreeSet<General>> {
        let specs = match self.0 {
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
            // empty
            30 => vec![r#"a -> A B ε C ε;"#],
            31 => vec![r#"a -> A B | C ε | ε | D | ε | <P> ε | <R> <P>;"#],
            32 => vec![r#"a -> <P> ε | <R> <P>;"#],
            33 => vec![r#"a -> <P> ε;"#],
            34 => vec![r#"a -> <P> ε;"#],
            35 => vec![r#"a -> ε;"#],
            36 => vec![r#"a -> ε;"#],
            37 => vec![r#"a -> A | ε;"#],

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

            // 4xx = rrec: <L>
            // -----------------------------------------------------------------------------
            400 => vec![r#"a -> <L> "?" a | "!";"#],

            // 5xx = lrec: simple
            // -----------------------------------------------------------------------------
            500 => vec![r#"a -> a "!" | "?";"#],

            // 6xx = lrec: amb
            // -----------------------------------------------------------------------------
            600 => vec![r#"e -> e "+" e | Num;"#],
            // ----- swapping independent terms shouldn't have an impact:
            601 => vec![r#"e -> e "*" e | e "+" e | Num | Id;"#],
            602 => vec![r#"e -> Num | e "*" e | Id | e "+" e;"#],
            // ----- prefix op:
            603 => vec![r#"e -> e "*" e | e "+" e |   "!" e | Num;"#],
            604 => vec![r#"e -> e "*" e |   "!" e | e "+" e | Num;"#],
            605 => vec![r#"e ->   "!" e | e "*" e | e "+" e | Num;"#],
            // ----- right-associative op:
            606 => vec![r#"e ->     e "*" e |     e "+" e | <R> e "!" e | Num;"#],
            607 => vec![r#"e ->     e "*" e | <R> e "!" e |     e "+" e | Num;"#],
            608 => vec![r#"e -> <R> e "!" e |     e "*" e |     e "+" e | Num;"#],
            // ----- postfix op:
            609 => vec![r#"e -> e "*" e | e "+" e | e "!"   | Num;"#],
            610 => vec![r#"e -> e "*" e | e "!"   | e "+" e | Num;"#],
            611 => vec![r#"e -> e "!"   | e "*" e | e "+" e | Num;"#],
            // ----- same priority:
            612 => vec![r#"e -> e "!" e |     e "*" e |     e "+" e | Num;"#],
            613 => vec![r#"e -> e "*" e |     e "+" e | <P> e "!" e | Num;"#],
            614 => vec![r#"e -> e "*" e | <P> e "!" e |     e "+" e | Num;"#],
            // ----- postfix & prefix ops:
            630 => vec![r#"e ->     e "+" |     "-" e | Num;"#],
            631 => vec![r#"e ->     e "+" | <R> "-" e | Num;"#],
            632 => vec![r#"e -> <R> e "+" |     "-" e | Num;"#],
            // ----- <L> rrec
            633 => vec![r#"e -> e "*" e | <L> "!" e | e "+" e | Num;"#],
            634 => vec![r#"e -> e "+" | <L> "-" e | Num;"#],

            // Existing tests in wrapper_source.rs:
            //
            // PRS(58)  // E -> E + | - E | 0
            // PRS(60)  // E -> E + | <L> - E | 0
            // RTS(42)  // E -> - E | E (* | / <P>) E | E (+ | - <P>) E | ID
            // RTS(43)  // E -> - E | <R> E (* | / <P>) E | <R> E (+ | - <P>) E | ID
            // RTS(44)  // E -> - E | <R> E (* | / <P>) E | E (+ | - <P>) E | ID
            // PRS(63)  // E -> <R> E ^ E | E * E | - E | E + E | ID;

            // 7xx = lfact
            // -----------------------------------------------------------------------------
            700 => vec![r#"a -> A | A B;"#],
            701 => vec![r#"a -> A | A B | C;"#],
            702 => vec![r#"a -> A B | A C;"#],
            703 => vec![r#"a -> B | A B | A C;"#],
            704 => vec![r#"a -> A B C | B B C | B C | B B A;"#],

            // 8xx = combinations
            // -----------------------------------------------------------------------------
            // 0xx and 1xx (or 2xx) normalization
            800 => vec![r#"a -> (A?)*;"#],
            801 => vec![r#"a -> (A?)+;"#],
            802 => vec![r#"a -> (A*)?;"#],  // TODO: check if ambiguous in parsing table
            803 => vec![r#"a -> (A+)?;"#],

            // 1xx/2xx and 3xx/4xx rrec
            810 => vec![r#"a -> A* a;"#],
            811 => vec![r#"a -> A+ a;"#],

            812 => vec![r#"a -> (A a)*;"#],  // TODO: check if ambiguous in parsing table
            813 => vec![r#"a -> (A a)+;"#],

            // 1xx/2xx and 5xx lrec
            820 => vec![r#"a -> a A* | B;"#],   // TODO: check if ambiguous in parsing table
            821 => vec![r#"a -> a A+ | B;"#],

            822 => vec![r#"a -> (a A)* | B;"#], // TODO: check if ambiguous in parsing table
            823 => vec![r#"a -> (a A)+ | B;"#],

            // 1xx/2xx and 6xx amb              // TODO: check if ambiguous in parsing table (remove those tests?/detect?)
            830 => vec![r#"a -> (a A)* a | B;"#],
            831 => vec![r#"a -> (a A)+ a | B;"#],

            832 => vec![r#"a -> a (A a)* | B;"#],
            833 => vec![r#"a -> a (A a)+ | B;"#],

            834 => vec![r#"a -> (a A a)* | B;"#],
            // 1xx/2xx and 7xx lfact
            840 => vec![r#"a -> (A B | A C)*;"#],
            841 => vec![r#"a -> (A B | A C)+;"#],

            842 => vec![r#"a -> A* B* | A* C*;"#],  // TODO: is it possible to factorize this?
            843 => vec![r#"a -> A+ B+ | A+ C+;"#],

            // 3xx/4xx and 7xx
            850 => vec![r#"a -> A B a | A C a | D;"#],
            851 => vec![r#"a -> A (B | C) a <L> | D;"#],

            // 5xx and 7xx
            860 => vec![r#"a -> a A B | a A C | D;"#],

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

    pub fn to_rts_normalized(self) -> Option<RuleTreeSet<Normalized>> {
        self.to_rts_general().map(|rts| rts.build_into())
    }

    pub fn to_prs_general(self) -> Option<ProdRuleSet<General>> {
        self.to_rts_normalized().map(|rts| rts.build_into())
    }

    pub fn to_prs_ll1(self) -> Option<ProdRuleSet<LL1>> {
        self.to_prs_general().map(|prs| prs.build_into())
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
