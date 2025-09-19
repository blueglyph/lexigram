// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

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
        (31, 0, None, None, r#"A B | C ε | ε | D | ε | <P> ε | <R> <P>"#),
        (32, 0, None, None, r#"<P> ε | <R> <P>"#),
        (35, 0, None, None, r#"ε"#),
        (200, 0, None, None, "(<L=i> A)*"),
        (200, 0, None, Some(2), "( ►► <L=i> A ◄◄ )*"),
        (200, 0, Some(2), None, "<L=i> A"),
        (207, 0, None, None, r#"(A (<L=j> B ",")+ ";")+"#),
        (600, 0, None, None, r#"e "+" e | Num"#),
    ];
    const VERBOSE: bool = false;
    let mut errors = 0;
    for (test_id, (rts_id, var, node_maybe, emphasis_maybe, expected_str)) in tests.into_iter().enumerate() {
        let rts = TestRules(rts_id).to_rts_general().unwrap();
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
        // A B ε C ε
        (30, None, (Some((false, false)), "A B C"), (Some((false, false)), "A B C")),
        // A B | C ε | ε | D | ε | <P> ε | <R> <P>
        (31, None, (Some((false, true)), "A B | C | D | ε"), (Some((false, true)), "A B | C | D")),
        // <P> ε | <R> <P>
        (32, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (33, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // <P> ε
        (34, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (35, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // ε
        (36, None, (Some((true, true)), "ε"), (Some((true, true)), "ε")),
        // A | ε
        (37, None, (Some((false, true)), "A | ε"), (Some((false, true)), "A")),
        // (A B)*
        (104, Some(3), (None, "(A B)*"), (None, "(A B)*")),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_SOLUTION: bool = false;
    let mut errors = 0;
    for (test_id, root, expected_false, expected_true) in tests {
        if VERBOSE { println!("{:=<80}\ntest {test_id}:", ""); }
        let rules = TestRules(test_id).to_rts_general().unwrap();
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
    assert!(errors == 0, "{errors} error(s)");
}

// cargo +nightly miri test --package lexigram --lib grammar::tests::ruletree_normalize -- --exact
#[test]
fn rts_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>, BTreeMap<VarId, &str>)> = vec![
        (0, //   a -> A
         btreemap![0 => r#"a -> A"#],
         btreemap![0 => r#"a -> A"#]),
        (1, //   a -> A B
         btreemap![0 => r#"a -> A B"#],
         btreemap![0 => r#"a -> A B"#]),
        (2, //   a -> A | B
         btreemap![0 => r#"a -> A | B"#],
         btreemap![0 => r#"a -> A | B"#]),
        (3, //   a -> A B | C
         btreemap![0 => r#"a -> A B | C"#],
         btreemap![0 => r#"a -> A B | C"#]),
        (4, //   a -> A B | C D
         btreemap![0 => r#"a -> A B | C D"#],
         btreemap![0 => r#"a -> A B | C D"#]),
        (5, //   a -> (A | B) (C | D)
         btreemap![0 => r#"a -> A C | A D | B C | B D"#],
         btreemap![0 => r#"a -> A C | A D | B C | B D"#]),
        (6, //   a -> (A | B) (C | D) E
         btreemap![0 => r#"a -> A C E | A D E | B C E | B D E"#],
         btreemap![0 => r#"a -> A C E | A D E | B C E | B D E"#]),
        (7, //   a -> A?
         btreemap![0 => r#"a -> A | ε"#],
         btreemap![0 => r#"a -> A | ε"#]),
        (8, //   a -> A? B
         btreemap![0 => r#"a -> A B | B"#],
         btreemap![0 => r#"a -> A B | B"#]),
        (9, //   a -> (A B)?
         btreemap![0 => r#"a -> A B | ε"#],
         btreemap![0 => r#"a -> A B | ε"#]),
        (10, //   a -> (A | B)?
         btreemap![0 => r#"a -> A | B | ε"#],
         btreemap![0 => r#"a -> A | B | ε"#]),
        (11, //   a -> A b        (11, //   b -> B
         btreemap![0 => r#"a -> A b"#, 1 => r#"b -> B"#],
         btreemap![0 => r#"a -> A b"#, 1 => r#"b -> B"#]),
        (30, //   a -> A B ε C ε
         btreemap![0 => r#"a -> A B C"#],
         btreemap![0 => r#"a -> A B C"#]),
        (31, //   a -> A B | C ε | ε | D | ε | <P> ε | <R> <P>
         btreemap![0 => r#"a -> A B | C | D | ε"#],
         btreemap![0 => r#"a -> A B | C | D | ε"#]),
        (32, //   a -> <P> ε | <R> <P>
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (33, //   a -> <P> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (34, //   a -> <P> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (35, //   a -> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (36, //   a -> ε
         btreemap![0 => r#"a -> ε"#],
         btreemap![]),
        (37, //   a -> A | ε
         btreemap![0 => r#"a -> A | ε"#],
         btreemap![0 => r#"a -> A | ε"#]),
        (100, //   a -> A*
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | ε"#],
         btreemap![0 => r#"a -> A*"#]),
        (101, //   a -> A+
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | A"#],
         btreemap![0 => r#"a -> A+"#]),
        (102, //   a -> A B* C
         btreemap![0 => r#"a -> A a_1 C"#, 1 => r#"a_1 -> B a_1 | ε"#],
         btreemap![0 => r#"a -> A B* C"#]),
        (103, //   a -> A B+ C
         btreemap![0 => r#"a -> A a_1 C"#, 1 => r#"a_1 -> B a_1 | B"#],
         btreemap![0 => r#"a -> A B+ C"#]),
        (104, //   a -> (A B)*
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A B a_1 | ε"#],
         btreemap![0 => r#"a -> (A B)*"#]),
        (105, //   a -> (A B)+
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A B a_1 | A B"#],
         btreemap![0 => r#"a -> (A B)+"#]),
        (106, //   a -> (A (B ",")* ";")*
         btreemap![0 => r#"a -> a_2"#, 1 => r#"a_1 -> B "," a_1 | ε"#, 2 => r#"a_2 -> A a_1 ";" a_2 | ε"#],
         btreemap![0 => r#"a -> (A (B ",")* ";")*"#]),
        (107, //   a -> (A (B ",")+ ";")+
         btreemap![0 => r#"a -> a_2"#, 1 => r#"a_1 -> B "," a_1 | B ",""#, 2 => r#"a_2 -> A a_1 ";" a_2 | A a_1 ";""#],
         btreemap![0 => r#"a -> (A (B ",")+ ";")+"#]),
        (150, //   a -> (A | B)*
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | B a_1 | ε"#],
         btreemap![0 => r#"a -> (A | B)*"#]),
        (151, //   a -> (A | B)+
         btreemap![0 => r#"a -> a_1"#, 1 => r#"a_1 -> A a_1 | A | B a_1 | B"#],
         btreemap![0 => r#"a -> (A | B)+"#]),
        (200, //   a -> (<L=i> A)*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A i | ε"#],
         btreemap![0 => r#"a -> (<L=i> A)*"#]),
        (201, //   a -> (<L=i> A)+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A i | <L=i> A"#],
         btreemap![0 => r#"a -> (<L=i> A)+"#]),
        (202, //   a -> (<L=i> A B)*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A B i | ε"#],
         btreemap![0 => r#"a -> (<L=i> A B)*"#]),
        (203, //   a -> (<L=i> A B)+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A B i | <L=i> A B"#],
         btreemap![0 => r#"a -> (<L=i> A B)+"#]),
        (204, //   a -> (<L=i> A (B ",")* ";")*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A a_1 ";" i | ε"#, 2 => r#"a_1 -> B "," a_1 | ε"#],
         btreemap![0 => r#"a -> (<L=i> A (B ",")* ";")*"#]),
        (205, //   a -> (<L=i> A (B ",")+ ";")+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A a_1 ";" i | <L=i> A a_1 ";""#, 2 => r#"a_1 -> B "," a_1 | B ",""#],
         btreemap![0 => r#"a -> (<L=i> A (B ",")+ ";")+"#]),
        (206, //   a -> (A (<L=j> B ",")* ";")*
         btreemap![0 => r#"a -> a_1"#, 1 => r#"j -> <L=j> B "," j | ε"#, 2 => r#"a_1 -> A j ";" a_1 | ε"#],
         btreemap![0 => r#"a -> (A (<L=j> B ",")* ";")*"#]),
        (207, //   a -> (A (<L=j> B ",")+ ";")+
         btreemap![0 => r#"a -> a_1"#, 1 => r#"j -> <L=j> B "," j | <L=j> B ",""#, 2 => r#"a_1 -> A j ";" a_1 | A j ";""#],
         btreemap![0 => r#"a -> (A (<L=j> B ",")+ ";")+"#]),
        (208, //   a -> (<L=i> A (<L=j> B ",")* ";")*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A j ";" i | ε"#, 2 => r#"j -> <L=j> B "," j | ε"#],
         btreemap![0 => r#"a -> (<L=i> A (<L=j> B ",")* ";")*"#]),
        (209, //   a -> (<L=i> A (<L=j> B ",")+ ";")+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A j ";" i | <L=i> A j ";""#, 2 => r#"j -> <L=j> B "," j | <L=j> B ",""#],
         btreemap![0 => r#"a -> (<L=i> A (<L=j> B ",")+ ";")+"#]),
        (250, //   a -> (<L=i> A | B)*
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A i | B i | ε"#],
         btreemap![0 => r#"a -> (<L=i> A | B)*"#]),
        (251, //   a -> (<L=i> A | B)+
         btreemap![0 => r#"a -> i"#, 1 => r#"i -> <L=i> A i | <L=i> A | B i | B"#],
         btreemap![0 => r#"a -> (<L=i> A | B)+"#]),
    ];
    const VERBOSE: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const SHOW_RESULTS_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected, expected_orig) in tests {
        let mut rules = TestRules(test_id).to_rts_general().unwrap();
        let sym_tab = rules.get_symbol_table();
        let originals = rules.get_trees_iter()
            .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false)))
            .to_vec();
        if SHOW_RESULTS_ONLY {
            println!("{}", originals.iter().map(|s| format!("        ({test_id}, // {s}")).join(""));
        }
        if VERBOSE {
            println!("{:=<80}\ntest {test_id}:", "");
            println!("- original:\n{}", originals.join("\n"));
            println!("- original (detailed):\n{}",
                     rules.get_trees_iter()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
        }
        rules.normalize();
        assert_eq!(rules.log.num_errors(), 0, "test {test_id} failed to normalize:\n{}", rules.log.get_messages_str());
        if let Some(err) = check_rts_sanity(&rules, false) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let sym_tab = rules.get_symbol_table();
        let result = BTreeMap::from_iter(rules.get_trees_iter()
            .map(|(id, _t)| (id, format!("{} -> {}", Symbol::NT(id).to_str(sym_tab), rules.to_str(id, None, None)))));
        let result_orig = rules.origin.trees.iter().index::<VarId>()
                .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                .map(|(v, t)| (v, format!("{} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))))
            .collect::<BTreeMap<_, _>>();
        if VERBOSE {
            println!("- normalized:\n{}",
                     rules.get_trees_iter()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
            println!("- normalized (detailed):\n{}",
                     rules.get_trees_iter()
                         .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab) )).join("\n"));
            println!("- flags:  {:?}", rules.flags);
            println!("- parent: {:?}", rules.parent);
            // println!("{}", rules.get_trees_iter()
            //     .map(|(id, t)| format!("- {id} => {} (depth {})",
            //                            rules.to_str(id, None, None),
            //                            t.depth().unwrap_or(0))).join("\n"));
            if VERBOSE_DETAILS {
                println!("- original tree partially normalized:\n{}",
                         rules.origin.trees.iter().index::<VarId>()
                             .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                             .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), grtree_to_str(t, None, None, sym_tab, false))).join("\n"));
                println!("  other format:\n{}",
                         rules.origin.trees.iter().index::<VarId>()
                             .filter(|(_, t)| !is_grtree_empty_symbol(&t))
                             .map(|(v, t)| format!("  {} -> {}", Symbol::NT(v).to_str(sym_tab), t.to_str_index(None, sym_tab))).join("\n"));
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
        }
        if SHOW_RESULTS_ONLY {
            println!("         btreemap![{}],", result.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
            println!("         btreemap![{}]),", result_orig.iter().map(|(ref id, t)| format!("{id} => r#\"{t}\"#")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        let expected_orig = expected_orig.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        if result != expected || result_orig != expected_orig {
            errors += 1;
            if !SHOW_RESULTS_ONLY {
                println!("## ERROR ## test {test_id} failed");
            }
        };
    }
    assert!(errors == 0, "{errors} error(s)");
}

#[test]
fn rts_prodrule_from() {
    let tests: Vec<(u32, Vec<&str>, Vec<u32>, Vec<Option<VarId>>)> = vec![
        //  rules                                               flags
        // --------------------------------------------------------------------------------
        (1, vec![
            r#"a -> A B"#,                                      //
        ], vec![0], vec![None]),
        (4, vec![
            r#"a -> A B | C D"#,                                //
        ], vec![0], vec![None]),
        (8, vec![
            r#"a -> A B | B"#,                                  //
        ], vec![0], vec![None]),
        (11, vec![
            r#"a -> A b"#,                                      //
            r#"b -> B"#,                                        //
        ], vec![0, 0], vec![None, None]),
        (100, vec![
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | ε"#,                              // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (101, vec![
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A a_1 | A"#,                              // child_+_or_* | plus
        ], vec![6144, 4097], vec![None, Some(0)]),
        (105, vec![
            r#"a -> a_1"#,                                      // parent_+_or_* | plus
            r#"a_1 -> A B a_1 | A B"#,                          // child_+_or_* | plus
        ], vec![6144, 4097], vec![None, Some(0)]),
        (106, vec![
            r#"a -> a_2"#,                                      // parent_+_or_*
            r#"a_1 -> B "," a_1 | ε"#,                          // child_+_or_*
            r#"a_2 -> A a_1 ";" a_2 | ε"#,                      // child_+_or_* | parent_+_or_*
        ], vec![2048, 1, 2049], vec![None, Some(2), Some(0)]),
        (150, vec![
            r#"a -> a_1"#,                                      // parent_+_or_*
            r#"a_1 -> A a_1 | B a_1 | ε"#,                      // child_+_or_*
        ], vec![2048, 1], vec![None, Some(0)]),
        (208, vec![
            r#"a -> i"#,                                        // parent_+_or_*
            r#"i -> A j ";" i | ε"#,                            // child_+_or_* | L-form | parent_+_or_*
            r#"j -> B "," j | ε"#,                              // child_+_or_* | L-form
        ], vec![2048, 2177, 129], vec![None, Some(0), Some(1)]),
        /* template:
        (1, vec![
        ], vec![], vec![]),
        */
    ];
    const VERBOSE: bool = true;
    const VERBOSE_ANSWER_ONLY: bool = false;
    let mut errors = 0;
    for (test_id, expected, expected_flags, expected_parent) in tests {
        let rts = TestRules(test_id).to_rts_general().unwrap();
        if VERBOSE && !VERBOSE_ANSWER_ONLY {
            let symtab = rts.get_symbol_table();
            println!("{:=<80}\ntest {test_id}:", "");
            println!(
                "Original rules:\n{}",
                rts.get_non_empty_nts()
                    .map(|(v, t)| format!("- [{v:3}] {} -> {}", Symbol::NT(v).to_str(symtab), grtree_to_str(t, None, None, symtab, false)))
                    .join("\n"));
        }
        let mut prs = ProdRuleSet::build_from(rts);
        assert!(prs.log.has_no_errors(), "test {test_id} failed to create production rules:\n{}", prs.log.get_messages_str());
        prs.simplify();
        let symtab = prs.get_symbol_table();
        let result = prs.get_prules_iter().map(|(id, p)| prule_to_rule_str(id, &p, symtab)).to_vec();
        let num_vars = result.len();
        if VERBOSE || VERBOSE_ANSWER_ONLY {
            let flags = (0..num_vars).into_iter().map(|nt| prs.flags[nt]).join(", ");
            let parents = (0..num_vars).into_iter().map(|nt| format!("{:?}", prs.parent[nt])).join(", ");
            let comment_flags = (0..num_vars).into_iter().map(|nt| format!(" // {}", ruleflag::to_string(prs.flags[nt]).join(" | "))).to_vec();
            let lines = result.iter().zip(comment_flags).map(|(s1, s2)| vec![format!("r#\"{s1}\"#,"), s2]).to_vec();
            let cols = columns_to_str(lines, Some(vec![51, 0]));
            if !VERBOSE_ANSWER_ONLY { println!("Code:"); }
            println!("        ({test_id}, vec![");
            println!("{}", cols.into_iter().map(|s| format!("            {s}")).join("\n"));
            println!("        ], vec![{flags}], vec![{parents}]),");
            if !VERBOSE_ANSWER_ONLY && !prs.log.is_empty() {
                println!("Messages:\n{}", prs.log);
            }
        }
        // let fail1 = result != expected;
        let fail1 = result != expected.into_iter().map(|s| s.to_string()).to_vec();
        let fail2 = prs.flags[..num_vars] != expected_flags;
        let fail3 = prs.parent[..num_vars] != expected_parent;
        if  fail1 || fail2 || fail3 {
            errors += 1;
            if !VERBOSE_ANSWER_ONLY {
                let msg = format!("## ERROR ## test {test_id}: ");
                if fail1 { println!("{msg}rules don't match"); }
                if fail2 { println!("{msg}flags don't match"); }
                if fail3 { println!("{msg}parents don't match"); }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

#[allow(unused)]
fn rts_to_str<T>(rules: &RuleTreeSet<T>) -> String {
    rules.get_trees_iter()
        .map(|(id, t)| format!("- {id} => {:#} (depth {})",
                               t.to_str(None, rules.get_symbol_table()), t.depth().unwrap_or(0)))
        .join("\n")
}

#[allow(unused)]
impl<T> RuleTreeSet<T> {
    fn get_non_empty_nts(&self) -> impl Iterator<Item=(VarId, &GrTree)> {
        self.get_trees_iter().filter(|(_, rule)| !is_grtree_empty_symbol(&rule))
    }
}