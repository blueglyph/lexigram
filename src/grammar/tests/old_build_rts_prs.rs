// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashMap;
use std::ops::Deref;
use crate::{btreemap, prule, CollectJoin, General, SymbolTable, LL1};
use crate::grammar::{gnode, ruleflag, GrNode, GrTree, ProdRuleSet, RuleTreeSet, Symbol, VarId};
use crate::grammar::tests::prs;
use crate::log::{BuildFrom, LogReader, LogStatus};

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

        // error detection in tree
        500 => { // A -> (<L=B> a <L=C>)*
            let star = tree.add_root(gnode!(*));
            tree.addc_iter(Some(star), gnode!(&), [gnode!(L 1), gnode!(t 0), gnode!(L 2)]);
        }
        501 => { // A -> (<L=B> a | <L=C> b)*
            let star = tree.add_root(gnode!(*));
            let or = tree.add(Some(star), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(L 1), gnode!(t 0)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(L 2), gnode!(t 1)]);
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

pub(crate) fn build_prs(id: u32, is_t_data: bool) -> ProdRuleSet<General> {
    let mut rules = ProdRuleSet::new();
    let mut symbol_table = SymbolTable::new();
    let prules = &mut rules.prules;
    let mut start = Some(0);
    let flags = HashMap::<VarId, u32>::new();
    let parents = HashMap::<VarId, VarId>::new();   // (child, parent)
    let mut extend_nt = true;
    match id {
        // misc tests ----------------------------------------------------------
        0 => {
            prules.extend([
                prule!(nt 0, t 1; nt 0, t 2; t 3; t 3, t 4), // A -> A b | A c | d | d e
                prule!(nt 0, t 5; t 6; t 7),                 // B -> A f | g | h
            ]);
        }
        1 => {
            prules.extend([
                // A -> d c | b b c d | d c e | b d e g | b b c | c b | b c g | b d e f
                prule!(
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
        2 => { // tests empty prule
            prules.extend([]);
        }
        3 => { // tests continue / break consistency in left_factorize
            prules.extend([
                prule!(t 0),
                prule!(t 2, t 0; t 2),
                prule!(t 2; t 1),
                prule!(t 3)]);
        }
        4 => {
            // classical arithmetic grammar
            // T:  0:-, 1:+, 2:/, 3:*, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F
            def_arith_symbols(&mut symbol_table, true);
            prules.extend([
                prule!(nt 0, t 0, nt 1; nt 0, t 1, nt 1; nt 1),  // E -> E + T | E - T | T
                prule!(nt 1, t 2, nt 2; nt 1, t 3, nt 2; nt 2),  // T -> T * F | T / F | F
                prule!(t 4, nt 0, t 5; t 6; t 7),                // F -> ( E ) | NUM | ID
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
            prules.extend([
                prule!(nt 1, nt 2, t 2, t 2), // A -> A1 A2 ; ;
                prule!(t 0, nt 1; e),         // A1 -> - A1 | ε
                prule!(t 1, nt 2; e),         // A2 -> + A2 | ε
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
            prules.extend([
                prule!(t 0, nt 0; e),    // A1 -> - A1 | ε
                prule!(nt 0, nt 2, t 2), // A -> A1 A2 ;     <-- start
                prule!(t 1, nt 2; e),    // A2 -> + A2 | ε
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
            prules.extend([
                prule!(t 1, nt 0; e),    // A1 -> - A1 | ε
                prule!(t 0, nt 2),       // X -> > A         <-- start I
                prule!(nt 0, nt 3, t 3), // A -> A1 A2 ;     <-- start II
                prule!(t 2, nt 3; e),    // A2 -> + A2 | ε
            ]);
            start = Some(1);
        }
        8 => {
            // ambiguous
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1),    // A -> A a A | b
            ]);
        }
        9 => {
            // simple rules
            prules.extend([
                prule!(t 0, nt 1, t 2),          // A -> a B c
                prule!(t 1; t 3),                // B -> b | d
            ])
        }
        14 => {
            // A -> A A | a
            prules.extend([
                prule!(nt 0, nt 0; t 0)
            ]);
        }
        16 => {
            // A -> B A | b
            // B -> a
            prules.extend([
                prule!(nt 1, nt 0; t 1),
                prule!(t 0)
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
            prules.extend([
                prule!(nt 1; t 0),
                prule!(nt 2, t 2),
                prule!(t 1, nt 0),
            ]);
        }
        18 => {
            // A -> a
            prules.extend([
                prule!(t 0),
            ]);
        }
        19 => {
            // A -> a | ε
            prules.extend([
                prule!(t 0; e),
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
            prules.extend([
                prule!(t 0, t 5, t 1, nt 1),
                prule!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ]);
        }
        24 => {
            // A -> a (b c)+ d | e
            // =>
            // A -> a B d | e
            // B -> b c B | b c
            prules.extend([
                prule!(t 0, nt 1, t 3; t 4),
                prule!(t 1, t 2, nt 1; t 1, t 2),
            ]);
        }
        25 => {
            // A -> A a b c | A a b d | A a e | f
            prules.extend([
                prule!(nt 0, t 0, t 1, t 2; nt 0, t 0, t 1, t 3; nt 0, t 0, t 4; t 5)
            ]);
        }
        27 => {
            // A -> A a | A b | c | d
            prules.extend([
                prule!(nt 0, t 0; nt 0, t 1; t 2; t 3),
            ]);
        }
        28 => {
            // A -> a | a b | a b c | a b d | e
            prules.extend([
                prule!(t 0; t 0, t 1; t 0, t 1, t 2; t 0, t 1, t 3; t 4),
            ]);
        }
        29 => { // L-form counterpart of #16
            // A -> <L> B A | b
            // B -> a
            prules.extend([
                prule!(nt 1, nt 0; t 1),
                prule!(t 0)
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
            prules.extend([
                prule!(t 0, t 5, t 1, nt 1),
                prule!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ]);
            rules.set_flags(1, ruleflag::L_FORM);
        }
        31 => {
            // E -> F | E . id
            // F -> id
            prules.extend([
                prule!(nt 1; nt 0, t 0, t 1),
                prule!(t 1),
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
            prules.extend([
                prule!(nt 1; nt 0, t 0, t 1; nt 0, t 0, t 1, t 2, t 3),
                prule!(t 1),
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
            prules.extend([
                prule!(nt 0, t 0; t 1, t 2; t 1, t 3),
            ]);
        }
        34 => {
            // S -> id = VAL | exit | return VAL
            // VAL -> id | num
            prules.extend([
                prule!(t 0, t 2, nt 1; t 3; t 4, nt 1),
                prule!(t 0; t 1),
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
            prules.extend([
                prule!(t 0; t 0, t 1, t 1; t 0, t 2, t 2),
            ]);
        }
        36 => {
            // E -> F | num | E . id
            // F -> id
            prules.extend([
                prule!(nt 1; t 2; nt 0, t 0, t 1),
                prule!(t 1),
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
            prules.extend([
                prule!(t 0, t 5, t 1, nt 1),
                prule!(t 5, t 3, t 5, t 4, nt 1; t 5, t 4, nt 1 ; t 2),
            ]);
        }
        38 => {
            // A -> A a | A b | b c | b d
            prules.extend([
                prule!(nt 0, t 0; nt 0, t 1; t 1, t 2; t 1, t 3),
            ]);
        }
        39 => {
            // A -> A a b | A a c | b c | b d
            prules.extend([
                prule!(nt 0, t 0, t 1; nt 0, t 0, t 2; t 1, t 2; t 1, t 3),
            ]);
        }
        40 => {
            // A -> a A | b
            prules.extend([
                prule!(t 0, nt 0; t 1),
            ]);
        }
        41 => {
            // A -> a A <L> | b (by explicitly setting the l-form flag)
            prules.extend([
                prule!(t 0, nt 0; t 1),
            ]);
            rules.set_flags(0, ruleflag::L_FORM);
        }
        42 => {
            // A -> a A <L> | b (l-form set through the Alternative flags)
            prules.extend([
                prule!(#L, t 0, nt 0; t 1),
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
            prules.extend([
                prule!(#L, nt 1, t 7, nt 0; e),
                prule!(t 0, nt 2, t 1; t 2, nt 2, t 3),
                prule!(nt 3, t 4, nt 3),
                prule!(t 5; t 6; t 2, nt 2, t 3),
            ]);
        }

        // fixes ---------------------------------------------------------------
        44 => {
            // PRS(63) generated strange code for the rrec E5 -> E6 ^ E5 | E6:
            // we compare here rrec and rrec + lfact, with another NT that has value

            // A -> B a A | B
            // B -> b
            prules.extend([
                prule!(nt 1, t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }
        45 => {
            // A -> a A | B
            // B -> b
            prules.extend([
                prule!(t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }
        46 => {
            // A -> A B a | B
            // B -> b
            prules.extend([
                prule!(nt 0, nt 1, t 0; nt 1),
                prule!(t 1),
            ])
        }
        47 => {
            // same as 44 with <L>
            // A -> B a A <L> | B
            // B -> b
            prules.extend([
                prule!(#L, nt 1, t 0, nt 0; nt 1),
                prule!(t 1),
            ])
        }
        48 => {
            // A -> a A <L> | B
            // B -> b
            prules.extend([
                prule!(#L, t 0, nt 0; nt 1),
                prule!(t 1),
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
            prules.extend([
                prule!(nt 0, t 2, nt 0; nt 0, t 3, nt 0; nt 0, t 4, nt 0; nt 1),
                prule!(t 5, nt 0, t 6; t 7; t 8),
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
            prules.extend([
                prule!(t 0, nt 0;
                    nt 0, t 2, nt 0;
                    nt 0, t 9;
                    nt 0, t 3, nt 0;
                    t 1, nt 0;
                    nt 0, t 4, nt 0;
                    nt 1),
                prule!(t 5, nt 0, t 6; t 7; t 8),
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
            prules.extend([
                prule!(nt 0, t 0, nt 0; nt 0, t 1; nt 0, t 2; nt 0, t 3, nt 0; nt 1),
                prule!(t 4; t 5),
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
            prules.extend([
                prule!(#R, nt 0, t 0, nt 0; #R, nt 0, t 1, nt 0; t 2, nt 0; nt 0, t 3, nt 0; nt 1),
                prule!(t 4; t 5; t 6, nt 0, t 7),
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
            prules.extend([
                prule!(#R, nt 0, t 1, nt 0; nt 0, t 0; nt 0, t 2; #R, nt 0, t 3, nt 0; t 4; t 5)
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
            prules.extend([
                prule!(nt 0, t 1, nt 0; nt 0, t 2; t 0, nt 0; nt 0, t 3, nt 0; t 4; t 5)
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
            prules.extend([
                prule!(nt 0, t 1, nt 0; t 0, nt 0; nt 0, t 2; nt 0, t 3, nt 0; t 4; t 5)
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
            prules.extend([
                prule!(nt 0, t 0, nt 0; nt 0, t 1, nt 0; nt 0, t 2, nt 0; t 3; t 4)
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
            prules.extend([
                prule!(nt 0, t 0; t 1, nt 0; t 2)
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
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1, nt 0; t 2)
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
            prules.extend([
                prule!(nt 0, t 0; #L, t 1, nt 0; t 2)
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
            prules.extend([
                prule!(nt 0, t 0; t 1, nt 0; t 2; t 3)
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
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1, nt 0; nt 0, t 2, nt 0; t 3),
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
            prules.extend([
                prule!(#R, nt 0, t 0, nt 0; nt 0, t 1, nt 0; t 2, nt 0; nt 0, t 3, nt 0; t 4),
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
            prules.extend([
                prule!(t 1, nt 0; nt 0, t 0, nt 0; t 2)
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
            prules.extend([
                prule!(nt 0, t 1; nt 0, t 0, nt 0; nt 0, t 2; t 3, nt 0; t 4),
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
            prules.extend([
                prule!(nt 0, t 4, t 0, nt 0; nt 0, t 1; nt 0, t 4, t 2, nt 0; t 3, nt 0; t 5)
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
            prules.extend([
                prule!(t 1, nt 0; nt 0, t 0; t 2)
            ])
        }

        // ambiguity? ----------------------------------------------------------
        100 => {
            // A -> A a A b | c (amb removed)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 1; t 2),
            ]);
        }
        101 => {
            // A -> a A A | b (no amb)
            prules.extend([
                prule!(t 0, nt 0, nt 0; t 1),
            ]);
        }
        102 => {
            // A -> A a A b A | c (amb removed)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 1, nt 0; t 2)
            ]);
        }
        103 => {
            // A -> a (A b A)* c | d (no amb)
            // =>
            // A -> a B c | d
            // B -> A b A B | ε
            prules.extend([
                prule!(t 0, nt 1, t 2; t 3),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        104 => {
            // A -> (A b A)* c | a (amb A:a, B:c)
            // =>
            // A -> B c | a
            // B -> A b A B | ε
            prules.extend([
                prule!(nt 1, t 2; t 0),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        105 => {
            // A -> a (A b A)* | c (amb)
            // =>
            // A -> a B | c
            // B -> A b A B | ε
            prules.extend([
                prule!(t 0, nt 1; t 2),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }
        106 => {
            // A -> (A b A)* | a (very amb)
            // =>
            // A -> A1 | a
            // A1 -> A b A A1 | ε
            prules.extend([
                prule!(nt 1; t 0),
                prule!(nt 0, t 1, nt 0, nt 1; e),
            ]);
        }

        // warnings and errors -------------------------------------------------
        1000 => { // A -> A a  (error: missing non-recursive alternative)
            prules.extend([
                prule!(nt 0, t 0)
            ]);
        },
        1001 => { // A -> A a A A | b (error: cannot remove recursion)
            prules.extend([
                prule!(nt 0, t 0, nt 0, nt 0; t 1)
            ]);
        },
        1002 => { // A -> A a A a A | b (warning: ambiguous)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 0, nt 0; t 1)
            ]);
        },
        1003 => { // (error: no terminal in grammar)
            prules.extend([
                prule!(nt 1),
                prule!(nt 2),
                prule!(nt 0),
            ]);
        },
        1004 => { // (error: no terminal used in table)
            prules.extend([
                prule!(nt 1),
                prule!(nt 2, t 0),
                prule!(nt 0),
            ]);
        },
        1005 => { // (warnings: unused terminals, unused nonterminals)
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_nonterminals(["A".to_string(), "B".to_string()]);
            prules.extend([
                prule!(t 1),
                prule!(t 1),
            ]);
        },
        1006 => { // symbol_table.num_nt != rules.num_nt
            extend_nt = false;
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_nonterminals(["A".to_string()]);
            prules.extend([
                prule!(t 0, nt 1),
                prule!(t 1),
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
    prs::complete_symbol_table(&mut symbol_table, rules.num_t, calc_num_nt, is_t_data);
    rules.set_symbol_table(symbol_table);
    if let Some(start) = start {
        rules.set_start(start);
    }
    rules
}

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
                    prs::complete_symbol_table(&mut symbol_table, num_t, num_nt, is_t_data);
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