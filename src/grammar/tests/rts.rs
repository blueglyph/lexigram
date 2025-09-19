// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use crate::grammar::tests::prs::print_expected_code;
use super::*;

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

#[test]
fn ruletreeset_to_str() {
    let tests = vec![
        // RTS, var, node, emphasis, expected
        (4, 0, None, None, "A B | C D"),
        (6, 0, None, None, "(A | B) (C | D) E"),
        (10, 0, None, None, "(A | B)?"),
        (200, 0, None, None, "(<L=i> A)*"),
        (200, 0, None, Some(2), "( ►► <L=i> A ◄◄ )*"),
        (200, 0, Some(2), None, "<L=i> A"),
        (207, 0, None, None, r#"(A (<L=j> B ",")+ ";")+"#),
        (600, 0, None, None, r#"e "+" e | Num"#),
    ];
    const VERBOSE: bool = true;
    let mut errors = 0;
    for (test_id, (rts_id, var, node_maybe, emphasis_maybe, expected_str)) in tests.into_iter().enumerate() {
        let rts = RtsGeneral::build_test_rules(rts_id).unwrap();
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
        // a b ε c ε
        (65, None, (Some((false, false)), "a b c"), (Some((false, false)), "a b c")),
        // a b | c ε | ε | d | ε | <P> ε | <R> <P>
        (66, None, (Some((false, true)), "a b | c | d | ε"), (Some((false, true)), "a b | c | d")),
        // <P> ε | <R> <P>
        (67, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (68, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (69, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (70, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (71, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // a | ε
        (72, None, (Some((false, true)), "a | ε"), (Some((false, true)), "a")),
        // (a b)*
        (56, Some(1), (None, "(a b)*"), (None, "(a b)*")),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_SOLUTION: bool = false;
    let mut errors = 0;
    for (test_id, root, expected_false, expected_true) in tests {
        if VERBOSE { println!("{:=<80}\ntest {test_id}:", ""); }
        let rules = build_rts(test_id);
        let sym_tab = rules.symbol_table.clone();
        let st = sym_tab.as_ref();
        let mut result = vec![];
        let s1 = rules.to_str(0, root, None);
        for del_empty_term in [false, true] {
            if VERBOSE { println!("del_empty_term = {del_empty_term}"); }
            let mut t = rules.trees[0].clone();
            let si1 = t.to_str_index(root, st);
            let output = grtree_cleanup(&mut t, root, del_empty_term);
            let s2 = grtree_to_str(&t, root, None, st, false);
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
    assert_eq!(errors, 0);
}

// cargo +nightly miri test --package lexigram --lib grammar::tests::ruletree_normalize -- --exact
#[test]
fn rts_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>)> = vec![
        //   A -> b | c | D
        (0, btreemap![0 => r#"b | c | D"#]),
        //   A -> B C | d | e | F G | h | i | J K
        (1, btreemap![0 => r#"B C | d | e | F G | h | i | J K"#]),
        //   A -> B C (D | E) F G (H | I)
        (2, btreemap![0 => r#"B C D F G H | B C D F G I | B C E F G H | B C E F G I"#]),
        //   A -> B C (D | E) F (G H | I)
        (3, btreemap![0 => r#"B C D F G H | B C D F I | B C E F G H | B C E F I"#]),
        //   A -> A B C (D | E) F (G H | I)
        (4, btreemap![0 => r#"A B C D F G H | A B C D F I | A B C E F G H | A B C E F I"#]),
        //   A -> B?
        (5, btreemap![0 => r#"B | ε"#]),
        //   A -> (B C)?
        (6, btreemap![0 => r#"B C | ε"#]),
        //   A -> (B C | D)?
        (7, btreemap![0 => r#"B C | D | ε"#]),
        //   A -> b c+
        (8, btreemap![0 => r#"b A_1"#, 1 => r#"c A_1 | c"#]),
        //   A -> "var" (id ",")+
        (9, btreemap![0 => r#""var" A_1"#, 1 => r#"id "," A_1 | id ",""#]),
        //   A -> b (c d | e)+
        (10, btreemap![0 => r#"b A_1"#, 1 => r#"c d A_1 | c d | e A_1 | e"#]),
        //   A -> b c*
        (11, btreemap![0 => r#"b A_1"#, 1 => r#"c A_1 | ε"#]),
        //   A -> b (c d)*
        (12, btreemap![0 => r#"b A_1"#, 1 => r#"c d A_1 | ε"#]),
        //   A -> a (b+ c)+ d
        (17, btreemap![0 => r#"a A_2 d"#, 1 => r#"b A_1 | b"#, 2 => r#"A_1 c A_2 | A_1 c"#]),
        //   A -> b (c d | e)*
        (13, btreemap![0 => r#"b A_1"#, 1 => r#"c d A_1 | e A_1 | ε"#]),
        //   A -> A (b <L=AIter1>)* c | d
        (19, btreemap![0 => r#"A AIter1 c | d"#, 1 => r#"b <L=AIter1> AIter1 | ε"#]),
        //   A -> A (b | c <R> | d) A | e
        (15, btreemap![0 => r#"A b A | A c <R> A | A d A | e"#]),
        //   E -> "-" E | E ("*" | "/" <P>) E | E ("+" | "-" <P>) E | ID
        (42, btreemap![0 => r#""-" E | E "*" E | E "/" <P> E | E "+" E | E "-" <P> E | ID"#]),
        //   A -> a b | c | d | e
        (45, btreemap![0 => r#"a b | c | d | e"#]),
        //   A -> (a | b) (c | d)
        (46, btreemap![0 => r#"a c | a d | b c | b d"#]),
        //   A -> (a | b) (c d | e)*
        (47, btreemap![0 => r#"a A_1 | b A_1"#, 1 => r#"c d A_1 | e A_1 | ε"#]),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const SHOW_RESULTS_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected) in tests {
        let mut rules = build_rts(test_id);
        let sym_tab = rules.get_symbol_table();
        let originals = rules.get_non_empty_nts()
            .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false)))
            .to_vec();
        if SHOW_RESULTS_ONLY {
            println!("{}", originals.iter().map(|s| format!("        // {s}")).join(""));
        }
        if VERBOSE {
            println!("{:=<80}\ntest {test_id}:", "");
            println!("- original:\n{}", originals.join("\n"));
            println!("- original (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }
        rules.normalize();
        assert_eq!(rules.log.num_errors(), 0, "test {test_id} failed to normalize:\n{}", rules.log.get_messages_str());
        if let Some(err) = check_rts_sanity(&rules, VERBOSE_DETAILS) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let result = BTreeMap::from_iter(rules.get_non_empty_nts()
            .map(|(id, _t)| (id, format!("{}", rules.to_str(id, None, None)))));
        if VERBOSE {
            let sym_tab = rules.get_symbol_table();
            println!("- normalized:\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("- normalized (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("flags:  {:?}", rules.flags);
            println!("parent: {:?}", rules.parent);
            println!("{}", rules.get_non_empty_nts()
                .map(|(id, t)| format!("- {id} => {} (depth {})",
                                       rules.to_str(id, None, None),
                                       t.depth().unwrap_or(0))).join("\n"));
        }
        if SHOW_RESULTS_ONLY {
            println!("        ({test_id}, btreemap![{}]),", result.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        if result != expected {
            errors += 1;
            if !SHOW_RESULTS_ONLY {
                println!("## ERROR ## test {test_id} failed");
            }
        };
    }
    assert_eq!(errors, 0);
}

#[test]
fn orig_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>)> = vec![
        //   A -> b | c | D
        (0, btreemap![0 => r#"b | c | D"#]),
        //   A -> A B C (D | E) F (G H | I)
        (4, btreemap![0 => r#"A B C D F G H | A B C D F I | A B C E F G H | A B C E F I"#]),
        //   A -> B?
        (5, btreemap![0 => r#"B | ε"#]),
        //   A -> (B C)?
        (6, btreemap![0 => r#"B C | ε"#]),
        //   A -> (B C | D)?
        (7, btreemap![0 => r#"B C | D | ε"#]),
        //   A -> a b?
        (73, btreemap![0 => r#"a b | a"#]),
        //   A -> b c+
        (8, btreemap![0 => r#"b c+"#]),
        //   A -> "var" (id ",")+
        (9, btreemap![0 => r#""var" (id ",")+"#]),
        //   A -> b (c d | e)+
        (10, btreemap![0 => r#"b (c d | e)+"#]),
        //   A -> b c*
        (11, btreemap![0 => r#"b c*"#]),
        //   A -> b (c d)*
        (12, btreemap![0 => r#"b (c d)*"#]),
        //   A -> a (b+ c)+ d
        (13, btreemap![0 => r#"b (c d | e)*"#]),
        //   A -> b? (c | d)?
        (14, btreemap![0 => r#"b c | b d | b | c | d | ε"#]),
        //   A -> A a c? | A b c? | d
        (38, btreemap![0 => r#"A a c | A a | A b c | A b | d"#]),
        //   A -> a? b* c?
        (59, btreemap![0 => r#"a b* c | a b* | b* c | b*"#]),
        //   A -> a? (b? | c?)*
        (60, btreemap![0 => r#"a (b | c)* | (b | c)*"#]),
        //   A -> a ε?
        (61, btreemap![0 => r#"a"#]),
        //   A -> (a | ε)?
        (62, btreemap![0 => r#"a | ε"#]),
        //   A -> a ε*
        (63, btreemap![0 => r#"a"#]),
        //   A -> a ε+
        (64, btreemap![0 => r#"a"#]),
        (17, btreemap![0 => r#"a (b+ c)+ d"#]),
        //   A -> a A | b
        (35, btreemap![0 => r#"a A | b"#]),
        //   A -> A (b <L=AIter1>)* c | d
        (19, btreemap![0 => r#"A (b <L=AIter1>)* c | d"#]),
        //   A -> A (b | c <R> | d) A | e
        (15, btreemap![0 => r#"A b A | A c <R> A | A d A | e"#]),
        //   E -> "-" E | E ("*" | "/" <P>) E | E ("+" | "-" <P>) E | ID
        (42, btreemap![0 => r#""-" E | E "*" E | E "/" <P> E | E "+" E | E "-" <P> E | ID"#]),
        //   A -> a b | c | d | e
        (45, btreemap![0 => r#"a b | c | d | e"#]),
        //   A -> (a | b) (c | d)
        (46, btreemap![0 => r#"a c | a d | b c | b d"#]),
        //   A -> (a | b) (c d | e)*
        (47, btreemap![0 => r#"a (c d | e)* | b (c d | e)*"#]),
        //   A -> a (b c)+ d
        (54, btreemap![0 => r#"a (b c)+ d"#]),
        //   A -> (a d | B)* c        //   B -> b
        (50, btreemap![0 => r#"(a d | B)* c"#, 1 => r#"b"#]),
        //   A -> (a d | B)+ c        //   B -> b
        (52, btreemap![0 => r#"(a d | B)+ c"#, 1 => r#"b"#]),
        //   A -> a (b <L=AIter1>)* c
        (22, btreemap![0 => r#"a (b <L=AIter1>)* c"#]),
        //   A -> (a B)+ c        //   B -> b
        (28, btreemap![0 => r#"(a B)+ c"#, 1 => r#"b"#]),
    ];
    const VERBOSE: bool = false;
    const SHOW_RESULTS_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected) in tests {
        let rules = build_rts(test_id);
        let sym_tab = rules.get_symbol_table();
        let originals = rules.get_non_empty_nts()
            .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false)))
            .to_vec();
        if VERBOSE && !SHOW_RESULTS_ONLY {
            println!("{:=<80}\ntest {test_id}:", "");
            println!("- original:\n{}", originals.join("\n"));
            println!("- original (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }
        let rules = RuleTreeSet::<Normalized>::build_from(rules);
        assert_eq!(rules.log.num_errors(), 0, "test {test_id} failed to normalize:\n{}", rules.log.get_messages_str());
        if let Some(err) = check_rts_sanity(&rules, false) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let sym_tab = rules.get_symbol_table();
        let result = rules.origin.trees.iter().index::<VarId>()
                .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                .map(|(v, t)| (v, grtree_to_str(t, None, None, sym_tab, false)))
            .collect::<BTreeMap<_, _>>();
        if VERBOSE && !SHOW_RESULTS_ONLY {
            println!("- normalized:\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("- normalized (detailed):\n{}",
                     rules.get_non_empty_nts()
                         .map(|(v, t)| format!("  NT[{v}] {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("- original tree partially normalized:\n{}",
                     rules.origin.trees.iter().index::<VarId>()
                         .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("  other format:\n{}",
                     rules.origin.trees.iter().index::<VarId>()
                         .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
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
        let prules = ProdRuleSet::<General>::build_from(rules);
        if VERBOSE {
            println!("PRS origin:\n{}", indent_source(vec![prules.prs_alt_origins_str(true)], 4));
        }
        let ll1 = ProdRuleSet::<LL1>::build_from(prules);
        // println!("{}", ll1.symbol_table.as_ref().unwrap().get_nonterminals().enumerate().map(|(idx, nt)| format!("{idx}:{nt}")).join(", "));
        if VERBOSE {
            println!("LL1 origin:\n{}", indent_source(vec![ll1.prs_alt_origins_str(true)], 4));
            // let sym_tab = ll1.get_symbol_table();
            // println!("  other format:\n{}",
            //          ll1.origin.trees.iter().index::<VarId>()
            //              .filter(|(_, t)| !is_grtree_empty_symbol(&t))
            //              .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }

        if VERBOSE || SHOW_RESULTS_ONLY {
            println!("{}", originals.iter().map(|s| format!("        // {s}")).join(""));
            println!("        ({test_id}, btreemap![{}]),", result.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        if result != expected {
            errors += 1;
            if !SHOW_RESULTS_ONLY {
                println!("## ERROR ## test {test_id} failed");
            }
        };
    }
    assert_eq!(errors, 0);
}

#[test]
fn rts_prodrule_from() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>, Vec<u32>, Vec<Option<VarId>>)> = vec![
        (0, btreemap![0 => prule!(t 1; t 2; nt 3)], vec![0], vec![None]),
        (1, btreemap![0 => prule!(nt 1, nt 2; t 3; t 4; nt 5, nt 6;t 7; t 8; nt 9, nt 10)], vec![0], vec![None]),
        (2, btreemap![0 => prule!(
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 8;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 8;
        )], vec![0], vec![None]),
        (3, btreemap![0 => prule!(
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 3, nt 5, nt 8;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 4, nt 5, nt 8;
        )], vec![0], vec![None]),
        (4, btreemap![0 => prule!(
            nt 0, nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 0, nt 1, nt 2, nt 3, nt 5, nt 8;
            nt 0, nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 0, nt 1, nt 2, nt 4, nt 5, nt 8;
        )], vec![0], vec![None]),
        (5, btreemap![0 => prule!(nt 1; e)], vec![0], vec![None]),
        (6, btreemap![0 => prule!(nt 1, nt 2; e)], vec![0], vec![None]),
        (7, btreemap![0 => prule!(nt 1, nt 2; nt 3; e)], vec![0], vec![None]),
        (8, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, nt 1; t 2)
        ], vec![6144, 4097], vec![None, Some(0)]),
        (9, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; t 2, t 3)
        ], vec![6144, 4097], vec![None, Some(0)]),
        (10, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; t 2, t 3; t 4, nt 1; t 4)
        ], vec![6144, 4097], vec![None, Some(0)]),
        (11, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, nt 1; e)
        ], vec![2048, 1], vec![None, Some(0)]),
        (12, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; e)
        ], vec![2048, 1], vec![None, Some(0)]),
        (13, btreemap![
            0 => prule!(t 1, nt 1),
            1 =>  prule!(t 2, t 3, nt 1; t 4, nt 1; e)
        ], vec![2048, 1], vec![None, Some(0)]),
        (14, btreemap![
            0 => prule!(t 1, t 2; t 1, t 3; t 1; t 2; t 3; e)
        ], vec![0], vec![None]),
        (15, btreemap![
            0 => prule!(nt 0, t 1, nt 0; #R, nt 0, t 2, nt 0; nt 0, t 3, nt 0; t 4)
        ], vec![0], vec![None]),
        (17, btreemap![
            0 => prule!(t 0, nt 2, t 3),
            1 => prule!(t 1, nt 1; t 1),
            2 => prule!(nt 1, t 2, nt 2; nt 1, t 2),
        ], vec![6144, 4097, 6145], vec![None, Some(2), Some(0)]),
        (35, btreemap![
            0 => prule!(t 0, nt 0; t 1),
        ], vec![0], vec![None]), // R_RECURSION not set yet (set by ProdRuleSet<T>::remove_left_recursion())
        (36, btreemap![
            0 => prule!(t 0, nt 0; t 1),
        ], vec![128], vec![None]), // R_RECURSION not set yet (set by ProdRuleSet<T>::remove_left_recursion())
    ];
    const VERBOSE: bool = false;
    for (test_id, expected, expected_flags, expected_parent) in tests {
        let trees = build_rts(test_id);
        if VERBOSE {
            println!("\ntest {test_id}:");
        }
        let mut rules = ProdRuleSet::build_from(trees);
        assert!(rules.log.has_no_errors(), "test {test_id} failed to create production rules:\n{}", rules.log.get_messages_str());
        rules.simplify();
        let result = rules.get_non_empty_nts().map(|(id, p)| (id, p.clone())).collect::<BTreeMap<_, _>>();
        let num_vars = result.len();
        if VERBOSE {
            println!("=>");
            rules.print_rules(true, true);
            print_expected_code(&result);
            println!("  Flags: {}", rules.flags.iter().take(num_vars).index::<VarId>().map(|(nt, flag)| format!("\n  - {}: {}",
                Symbol::NT(nt).to_str(rules.get_symbol_table()), ruleflag::to_string(*flag).join(" "))).join(""));
            if !rules.log.is_empty() {
                println!("  Messages:{}", rules.log.get_messages().map(|m| format!("\n  - {m:?}")).join(""));
            }
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(rules.flags[..num_vars], expected_flags, "test {test_id} failed (flags)");
        assert_eq!(rules.parent[..num_vars], expected_parent, "test {test_id} failed (parent)");
    }
}

#[allow(dead_code)]
fn rts_to_str<T>(rules: &RuleTreeSet<T>) -> String {
    rules.get_trees_iter()
        .map(|(id, t)| format!("- {id} => {:#} (depth {})",
                               t.to_str(None, rules.get_symbol_table()), t.depth().unwrap_or(0)))
        .join("\n")
}

impl<T> RuleTreeSet<T> {
    fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &GrTree)> {
        self.get_trees_iter().filter(|(_, rule)| !is_grtree_empty_symbol(&rule))
    }
}