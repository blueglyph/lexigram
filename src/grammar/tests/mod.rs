// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

pub mod prs;
pub mod rts;
pub mod old_build_rts_prs;

use std::collections::{BTreeMap, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{alt, btreemap, gnode, hashmap, prule, sym, LL1};
use crate::log::TryBuildFrom;
use crate::log::BuildInto;
use crate::rtsgen::RtsGen;
use crate::columns_to_str;

// ---------------------------------------------------------------------------------------------

fn is_grtree_empty_symbol(rule: &GrTree) -> bool {
    rule.get_root()
        .and_then(|root| Some(*rule.get(root) == GrNode::Symbol(Symbol::Empty)))
        .unwrap_or(false)
}

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct TestRules(pub u32);

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
            13 => vec![r#"s -> Id "=" val | "exit" | "return" val;"#,
                       r#"val -> Id | Num;"#],
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
            106 => vec![r#"a -> (A (b ",")* ";")* C; b -> B;"#],
            107 => vec![r#"a -> (A (b ",")+ ";")+ C; b -> B;"#],
            108 => vec![r#"a -> A "B"* C;"#],

            // TODO (not yet fully supported in parsergen)
            150 => vec![r#"a -> (A | B)*;"#],
            151 => vec![r#"a -> (A | B)+;"#],
            152 => vec![r#"a -> A ( (B C | D)+ E | F)+ G;"#],

            // 2xx = +* repetitions: with <L>
            // -----------------------------------------------------------------------------
            200 => vec![r#"a -> A (<L=i> B)* C;"#],
            201 => vec![r#"a -> A (<L=i> B)+ C;"#],
            202 => vec![r#"a -> (<L=i> A B)*;"#],
            203 => vec![r#"a -> (<L=i> A B)+;"#],
            204 => vec![r#"a -> (<L=i> A (      B ",")* ";")*;"#],
            205 => vec![r#"a -> (<L=i> A (      B ",")+ ";")+;"#],
            206 => vec![r#"a -> (      A (<L=j> B ",")* ";")* C;"#],
            207 => vec![r#"a -> (      A (<L=j> B ",")+ ";")+ C;"#],
            208 => vec![r#"a -> (<L=i> A (<L=j> b ",")* ";")* C; b -> B;"#],
            209 => vec![r#"a -> (<L=i> A (<L=j> b ",")+ ";")+ C; b -> B;"#],
            210 => vec![r#"a -> A (<L=i> "B")* C;"#],
            211 => vec![r#"a -> A (A | C) (B <L=i>)* C;"#],

            // TODO (not yet fully supported in parsergen)
            250 => vec![r#"a -> (<L=i> A | B)*;"#],
            251 => vec![r#"a -> (<L=i> A | B)+;"#],
            252 => vec![r#"a -> A (      (<L=j> B C | D)+ E | F)+ G;"#],
            253 => vec![r#"a -> A (<L=i> (      B C | D)+ E | F)+ G;"#],
            253 => vec![r#"a -> A (<L=i> (<L=j> B C | D)+ E | F)+ G;"#],

            // 3xx = rrec: simple
            // -----------------------------------------------------------------------------
            300 => vec![r#"a -> "?" a | "!";"#],
            301 => vec![r#"expr -> Id "." expr | "(" Num ")";"#],
            302 => vec![r#"expr -> Num "^" expr | Num;"#],

            // 4xx = rrec: <L>
            // -----------------------------------------------------------------------------
            400 => vec![r#"a -> <L> "?" a | "!";"#],
            401 => vec![r#"expr -> <L> Id "." expr | "(" Num ")";"#],

            // 5xx = lrec: simple
            // -----------------------------------------------------------------------------
            500 => vec![r#"a -> a "!" | "?";"#],
            501 => vec![r#"a -> a "b" | a "c" | "a";"#],
            502 => vec![r#"e -> f | e "." Id;"#,
                        r#"f -> Id;"#],

            // ----- rrec + lrec
            580 => vec![r#"e -> e "!" |     "-" e | Num;"#],
            581 => vec![r#"e -> e "!" | <L> "-" e | Num;"#],

            // 6xx = lrec: amb
            // -----------------------------------------------------------------------------
            // note: [TOKENS0] inserts predfined tokens: token Mul = "*", Add = "+", Op = "!", Num, Id;
            600 => vec![r#"e -> e "+" e | Num;"#],
            // ----- swapping independent terms shouldn't have an impact:
            601 => vec![r#"e -> e "*" e | e "+" e | Num | Id;"#],
            602 => vec![r#"e -> Num | e "*" e | Id | e "+" e;"#],
            // ----- prefix op:
            603 => vec![r#"[TOKENS0] e -> e "*" e | e "+" e |   "!" e | Num;"#],
            604 => vec![r#"[TOKENS0] e -> e "*" e |   "!" e | e "+" e | Num;"#],
            605 => vec![r#"[TOKENS0] e ->   "!" e | e "*" e | e "+" e | Num;"#],
            // ----- right-associative op:
            606 => vec![r#"[TOKENS0] e ->     e "*" e |     e "+" e | <R> e "!" e | Num;"#],
            607 => vec![r#"[TOKENS0] e ->     e "*" e | <R> e "!" e |     e "+" e | Num;"#],
            608 => vec![r#"[TOKENS0] e -> <R> e "!" e |     e "*" e |     e "+" e | Num;"#],
            // ----- postfix op:
            609 => vec![r#"[TOKENS0] e -> e "*" e | e "+" e | e "!"   | Num;"#],
            610 => vec![r#"[TOKENS0] e -> e "*" e | e "!"   | e "+" e | Num;"#],
            611 => vec![r#"[TOKENS0] e -> e "!"   | e "*" e | e "+" e | Num;"#],
            // ----- same priority:
            612 => vec![r#"[TOKENS0] e -> e "!" e |     e "*" e |     e "+" e | Num;"#],
            613 => vec![r#"[TOKENS0] e -> e "*" e |     e "+" e | <P> e "!" e | Num;"#],
            614 => vec![r#"[TOKENS0] e -> e "*" e | <P> e "!" e |     e "+" e | Num;"#],
            // ----- postfix & prefix ops:
            630 => vec![r#"[TOKENS0] e -> e "*" e |     e "+" |     "!" e | Num;"#],
            631 => vec![r#"[TOKENS0] e -> e "*" e |     e "+" | <R> "!" e | Num;"#],
            632 => vec![r#"[TOKENS0] e -> e "*" e | <R> e "+" |     "!" e | Num;"#],
            // ----- <L> rrec (only allowed when no ambiguity)
            634 => vec![r#"e -> e "+" | <L> "!" e | Num;"#],

            640 => vec![r#"e -> "-" e |     e ("*" | "/" <P>) e |     e ("+" | "-" <P>) e | Id;"#],
            641 => vec![r#"e -> "-" e | <R> e ("*" | "/" <P>) e | <R> e ("+" | "-" <P>) e | Id;"#],
            642 => vec![r#"e -> "-" e | <R> e ("*" | "/" <P>) e |     e ("+" | "-" <P>) e | Id;"#],

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
            705 => vec![r#"a -> A | A B | A B C | A B D | E;"#],

            // 8xx = combinations
            // -----------------------------------------------------------------------------
            // 0xx and 1xx (or 2xx) normalization
            800 => vec![r#"a -> (A?)*;"#],
            801 => vec![r#"a -> (A?)+;"#],
            802 => vec![r#"a -> (A*)?;"#],  // ambiguous!
            803 => vec![r#"a -> (A+)?;"#],

            // 1xx/2xx and 3xx/4xx rrec
            810 => vec![r#"a -> A* a;"#],
            811 => vec![r#"a -> A+ a;"#],

            812 => vec![r#"a -> (A a)*;"#],  // ambiguous!
            813 => vec![r#"a -> (A a)+;"#],

            // 1xx/2xx and 5xx lrec
            820 => vec![r#"a -> a A* | B;"#],   // ambiguous!
            821 => vec![r#"a -> a A+ | B;"#],   // ambiguous!

            822 => vec![r#"a -> (a A)* | B;"#], // ambiguous!
            823 => vec![r#"a -> (a A)+ | B;"#],

            // 1xx/2xx and 6xx amb
            830 => vec![r#"a -> (a A)* a | B;"#],   // ambiguous!
            831 => vec![r#"a -> (a A)+ a | B;"#],   // ambiguous!

            832 => vec![r#"a -> a (A a)* | B;"#],   // ambiguous!
            833 => vec![r#"a -> a (A a)+ | B;"#],   // ambiguous!

            834 => vec![r#"a -> (a A a)* | B;"#],   // ambiguous!

            // 1xx/2xx and 7xx lfact
            840 => vec![r#"a -> (A B | A C)*;"#],
            841 => vec![r#"a -> (A B | A C)+;"#],

            842 => vec![r#"a -> A* B* | A* C*;"#],  // TODO: is it possible to factorize this?
            843 => vec![r#"a -> A+ B+ | A+ C+;"#],  // TODO: is it possible to factorize this?

            // 3xx and 6xx
            850 => vec![r#"a -> a A a a | B;"#],

            // 3xx/4xx and 7xx
            860 => vec![r#"a -> A B a | A C a | D;"#],
            861 => vec![r#"a -> A (B | C) a <L> | D;"#],
            862 => vec![r#"expr -> <L> Num "^" expr | Num;"#],

            // 5xx and 7xx
            870 => vec![r#"a -> a A B | a A C | D;"#],

            // 9xx = general examples
            // -----------------------------------------------------------------------------
            // rtsgen (somewhat simplified)
            900 => vec![
                r#"ruleset -> (<L=rule_iter> rule)+;"#,
                r#"rule -> rule_nt "=>" rts_expr ";" | rule_nt "->" prs_expr ";";"#,
                r#"rule_nt -> NT;"#,
                r#"rts_expr -> "&" rts_children | "|" rts_children | "+" rts_children | "*" rts_children | "?" rts_children | item;"#,
                r#"rts_children -> "(" rts_expr* ")";"#,
                r#"prs_expr -> prs_expr "+" | prs_expr "*" | prs_expr "?" | prs_expr prs_expr | prs_expr "|" prs_expr | "(" prs_expr ")" | item;"#,
                r#"item -> NT | T | "ε" | "<L>" | "<P>" | "<R>";"#,
            ],

            // 1yxx = errors
            // -----------------------------------------------------------------------------
            1000 => vec![r#"a -> a A;"#],
            1001 => vec![r#"a -> b; b -> c; c -> a;"#],
            1002 => vec![r#"a -> b; b -> c A; c -> a;"#],
            1003 => vec![r#"token B; a -> A; b -> A;"#],
            1004 => vec![r#"a -> A; b -> B;"#],             // b is removed manually from symbol table
            1005 => vec![r#"a -> (<L=x> A <L=y>)*;"#],
            1006 => vec![r#"a -> (<L=x> A | <L=y> B)*;"#],
            1007 => vec![r#"a -> <L> b; b -> b <L> | A;"#],
            1008 => vec![r#""#],                            // start is set to None
            1009 => vec![r#"e -> e "*" e | <L> "!" e | e "+" e | Num;"#],

            // -----------------------------------------------------------------------------
            _ => return None
        };
        let mut text = specs.join("\n");
        if text.starts_with("[TOKENS0] ") {
            text = format!("{}\n{}", r#"token Mul = "*", Add = "+", Op = "!", Num, Id;"#, &text[10..]);
        }
        match RtsGen::new().parse(text.clone()) {
            Ok(mut rts) => {
                self.manual_transform(&mut rts);
                Some(rts)
            },
            Err(log) => panic!("Error while parsing those rules:\n{:-<80}\n{text}\n{:-<80}\nLog:\n{log}\n{:-<80}", "", "", ""),
        }
    }

    fn manual_transform(&self, rts: &mut RuleTreeSet<General>) {
        match self.0 {
            // manual transformations:
            1004 => {
                // removing one NT from the symbol table to create a mismatch:
                rts.symbol_table.as_mut().unwrap().remove_nonterminal(1);
            }
            1008 => {
                rts.start = None;
            }
            _ => {}
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
