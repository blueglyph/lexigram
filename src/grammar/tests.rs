#![cfg(test)]

use std::collections::{BTreeMap, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{btreemap, gnode, prod, prodf, sym};

// ---------------------------------------------------------------------------------------------
// Supporting functions

fn print_production_rules<T>(prods: &ProdRuleSet<T>) {
    println!("   {}", prods.get_prods_iter().map(|(var, p)|
        format!("{} -> {}",
                Symbol::NT(var).to_str(prods.get_symbol_table()),
                prod_to_string(p, prods.get_symbol_table()))
    ).join("\n   "));
}

// ---------------------------------------------------------------------------------------------

#[test]
fn gnode() {
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
fn symbol_to_str() {
    let mut symtable = SymbolTable::new();
    symtable.extend_terminals([
        ("Arrow".to_string(), Some("->".to_string())),
        ("Colon".to_string(), Some(":".to_string())),
        ("Id".to_string(), None),
    ]);
    symtable.extend_non_terminals((0_u8..26).map(|i| format!("{}", char::from(i + 65))));
    let tests = vec![
        (sym!(t 0), vec!["->", ":0"]),
        (sym!(t 1), vec![":", ":1"]),
        (sym!(t 2), vec!["Id", ":2"]),
        (sym!(nt 0), vec!["A", "0"]),
        (sym!(e), vec!["ε", "ε"]),
    ];
    for (symbol, expected) in tests {
        assert_eq!(symbol.to_str(Some(&symtable)), expected[0], "test on {symbol} has failed");
        assert_eq!(symbol.to_str(None), expected[1], "test on {symbol} has failed");
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
                msg.push_str(&format!("duplicate index {} for var {var} in tree {tree:#}\n", node.index));
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

fn build_rts(id: u32) -> RuleTreeSet<General> {
    let mut rules = RuleTreeSet::new();
    // reserve a few variables just so the NT indices are not confusing:
    // we want new variables to begin at 10.
    rules.set_next_var(Some(10));
    let tree = rules.get_tree_mut(0);

    match id {
        0 => {
            let top = tree.addc_iter(None, gnode!(|), [gnode!(t 1), gnode!(t 2), gnode!(nt 3)]);
            tree.set_root(top);
        }
        1 => {
            let top = tree.add_root(gnode!(|));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(t 3), gnode!(t 4)]);
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            let or = tree.addc_iter(Some(top), gnode!(|), [gnode!(t 7), gnode!(t 8)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 9), gnode!(nt 10)]);
        }
        2 => {
            let top = tree.add_root(gnode!(&));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 5), gnode!(nt 6)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 7), gnode!(nt 8)]);
        }
        3 => {
            let top = tree.add_root(gnode!(&));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.addc_iter(Some(top), gnode!(|), [gnode!(nt 3), gnode!(nt 4)]);
            tree.add(Some(top), gnode!(nt 5));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 6), gnode!(nt 7)]);
            tree.add(Some(or), gnode!(nt 8));
        }
        4 => {
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
        5 => {
            let top = tree.add_root(gnode!(?));
            tree.add(Some(top), gnode!(nt 1));
        }
        6 => {
            let top = tree.add_root(gnode!(?));
            tree.addc_iter(Some(top), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
        }
        7 => {
            let top = tree.add_root(gnode!(?));
            let or = tree.add(Some(top), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 1), gnode!(nt 2)]);
            tree.add(Some(or), gnode!(nt 3));
        }
        8 => { // 12+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            tree.addc(Some(cc), gnode!(+), gnode!(nt 2));
        }
        9 => { // 1(23)+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(p), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
        }
        10 => { // 1(23|4)+
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(+));
            let or = tree.add(Some(p), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
            tree.add(Some(or), gnode!(nt 4));
        }
        11 => { // 12*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            tree.addc(Some(cc), gnode!(*), gnode!(nt 2));
        }
        12 => { // 1(23)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(p), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
        }
        13 => { // 1(23|4)*
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(nt 1));
            let p = tree.add(Some(cc), gnode!(*));
            let or = tree.add(Some(p), gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(nt 2), gnode!(nt 3)]);
            tree.add(Some(or), gnode!(nt 4));
        }

        _ => {}
    }
    rules
}

// cargo +nightly miri test --package rlexer --lib grammar::tests::ruletree_normalize -- --exact
#[test]
fn rts_normalize() {
    let tests: Vec<(u32, BTreeMap<VarId, &str>)> = vec![
        // |([1], [2], 3) (depth 1)
        (0, btreemap![0 => "|(:1, :2, 3)"]),
        // |(&(1, 2), |(:3, :4), &(5, 6), |(:7, :8, &(9, 10))) (depth 3)
        (1, btreemap![0 => "|(&(1, 2), :3, :4, &(5, 6), :7, :8, &(9, 10))"]),
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
        let mut rules = build_rts(test_id);
        let vars = rules.get_vars().to_vec();
        rules.normalize();
        if let Some(err) = check_rts_sanity(&rules, VERBOSE) {
            panic!("test {test_id} failed:\n{}", err);
        }
        let result = BTreeMap::from_iter(rules.get_trees_iter().map(|(id, t)| (id, format!("{t}"))));
        if VERBOSE {
            println!("{}", rules.get_trees_iter().map(|(id, t)| format!("- {id} => {t:#} (depth {})", t.depth().unwrap_or(0))).join("\n"));
            println!("({test_id}, btreemap![{}]),\n", result.iter().map(|(ref id, t)| format!("{id} => \"{t}\"")).join(", "));
        }
        let expected = expected.into_iter().map(|(id, s)| (id, s.to_string())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

#[test]
fn rts_prodrule_from() {
    let tests: Vec<(u32, BTreeMap<VarId, Vec<Vec<Symbol>>>)> = vec![
        (0, btreemap![0 => prod!(t 1; t 2; nt 3)]),
        // |(&(1, 2), [3], [4], &(5, 6), [7], [8], &(9, 10))
        (1, btreemap![0 => prod!(nt 1, nt 2; t 3; t 4; nt 5, nt 6;t 7; t 8; nt 9, nt 10)]),
        // |(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 6, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 6, 8))
        (2, btreemap![0 => prod!(
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 8;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 8;
        )]),
        // |(&(1, 2, 3, 5, 6, 7), &(1, 2, 3, 5, 8), &(1, 2, 4, 5, 6, 7), &(1, 2, 4, 5, 8))
        (3, btreemap![0 => prod!(
            nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 3, nt 5, nt 8;
            nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 1, nt 2, nt 4, nt 5, nt 8;
        )]),
        // |(&(0, 1, 2, 3, 5, 6, 7), &(0, 1, 2, 3, 5, 8), &(0, 1, 2, 4, 5, 6, 7), &(0, 1, 2, 4, 5, 8))
        (4, btreemap![0 => prod!(
            nt 0, nt 1, nt 2, nt 3, nt 5, nt 6, nt 7;
            nt 0, nt 1, nt 2, nt 3, nt 5, nt 8;
            nt 0, nt 1, nt 2, nt 4, nt 5, nt 6, nt 7;
            nt 0, nt 1, nt 2, nt 4, nt 5, nt 8;
        )]),
        // |(1, ε)
        (5, btreemap![0 => prod!(nt 1; e)]),
        // |(&(1, 2), ε)
        (6, btreemap![0 => prod!(nt 1, nt 2; e)]),
        // |(&(1, 2), 3, ε)
        (7, btreemap![0 => prod!(nt 1, nt 2; nt 3; e)]),
        // 0 => &(1, 10), 10 => |(&(2, 10), 2)
        (8, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 10; nt 2)]),
        // 0 => &(1, 10), 10 => |(&(2, 3, 10), &(2, 3))
        (9, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 3, nt 10; nt 2, nt 3)]),
        // 0 => &(1, 10), 10 => |(&(2, 3, 10), &(2, 3), &(4, 10), 4)
        (10, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 3, nt 10; nt 2, nt 3; nt 4, nt 10; nt 4)]),
        // 0 => "&(1, 10)", 10 => "|(&(2, 10), ε)"
        (11, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 10; e)]),
        // [0 => "&(1, 10)", 10 => "|(&(2, 3, 10), ε)"],
        (12, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 3, nt 10; e)]),
        // 0 => "&(1, 10)", 10 => "|(&(2, 3, 10), &(4, 10), ε)"
        (13, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 3, nt 10; nt 4, nt 10; e)]),
    ];
    for (test_id, expected) in tests {
        let trees = build_rts(test_id);
        let rules = ProdRuleSet::from(trees);
        let result = rules.get_prods_iter().map(|(id, p)| (id, p.clone())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

// ---------------------------------------------------------------------------------------------
// ProdRuleSet

fn print_expected_code(result: &BTreeMap<VarId, ProdRule>) {
    println!("\n            {}", result.iter().map(|(i, p)|
        format!("{i} => prod!({}),", p.iter().map(|f| f.iter().map(|s|
            match s {
                Symbol::Empty => "e".to_string(),
                Symbol::T(x) => format!("t {x}"),
                Symbol::NT(x) => format!("nt {x}"),
                Symbol::End => "end".to_string(),
            }).join(", ")).join("; "))).join("\n            "))
}

fn def_arith_symbols(symbol_table: &mut SymbolTable) {
    symbol_table.extend_terminals([
        ("PLUS".to_string(), Some("+".to_string())),
        ("MINUS".to_string(), Some("-".to_string())),
        ("MUL".to_string(), Some("*".to_string())),
        ("DIV".to_string(), Some("/".to_string())),
        ("LPAREN".to_string(), Some("(".to_string())),
        ("RPAREN".to_string(), Some(")".to_string())),
        ("NUM".to_string(), None),
        ("ID".to_string(), None)
    ]);
    symbol_table.extend_non_terminals([
        "E".to_string(), "T".to_string(), "F".to_string()
    ]);
}
impl<T> From<&ProdRuleSet<T>> for BTreeMap<VarId, ProdRule> {
    fn from(rules: &ProdRuleSet<T>) -> Self {
        rules.get_prods_iter().map(|(var, p)| (var, p.clone())).collect::<BTreeMap<_, _>>()

    }
}

fn build_prs(id: u32) -> ProdRuleSet<LR> {
    let mut rules = ProdRuleSet::new();
    let mut symbol_table = SymbolTable::new();
    let prods = &mut rules.prods;
    match id {
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
                    t 1, t 1, t 2;
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
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F
            def_arith_symbols(&mut symbol_table);
            prods.extend([
                prod!(nt 0, t 0, nt 1; nt 0, t 1, nt 1; nt 1),  // E -> E + T | E - T | T
                prod!(nt 1, t 2, nt 2; nt 1, t 3, nt 2; nt 2),  // T -> T * F | T / F | F
                prod!(t 4, nt 0, t 5; t 6; t 7),                // F -> ( E ) | NUM | ID
            ]);
        }
        _ => {}
    };
    if symbol_table.get_terminals().is_empty() {
        symbol_table.extend_terminals((0..26).map(|i| (format!("{}", char::from(i as u8 + 97)), None)));
    }
    if symbol_table.get_non_terminals().is_empty() {
        // finds the highest NT and populates the symbol table:
        let num_nt = prods.len().max(prods.iter().map(|p|
            p.iter().map(|f|
                f.iter().filter_map(|s|
                    if let Symbol::NT(v) = s { Some(*v) } else { None }).max().unwrap_or(0)).max().unwrap_or(0)
        ).max().unwrap_or(0) as usize + 1);
        assert!(num_nt <= 26);
        symbol_table.extend_non_terminals((0..num_nt as u8).map(|i| format!("{}", char::from(i + 65))));
    }
    rules.set_symbol_table(symbol_table);
    rules
}

#[test]
fn prs_remove_left_recursion() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
           // A -> d A_1 | d e A_1
           // B -> A f | g | h
           // A_1 -> b A_1 | c A_1 | ε
            0 => prod!(t 3, nt 2; t 3, t 4, nt 2),
            1 => prod!(nt 0, t 5; t 6; t 7),
            2 => prod!(t 1, nt 2; t 2, nt 2; e),
        ]),
        (2, btreemap![]),
        (4, btreemap![
           // E -> T E_1
           // T -> F T_1
           // F -> ( E ) | NUM | ID
           // E_1 -> + T E_1 | - T E_1 | ε
           // T_1 -> * F T_1 | / F T_1 | ε
            0 => prod!(nt 1, nt 3),
            1 => prod!(nt 2, nt 4),
            2 => prod!(t 4, nt 0, t 5; t 6; t 7),
            3 => prod!(t 0, nt 1, nt 3; t 1, nt 1, nt 3; e),
            4 => prod!(t 2, nt 2, nt 4; t 3, nt 2, nt 4; e),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_prs(test_id);
        if VERBOSE {
            println!("test {test_id}:");
            print_production_rules(&rules);
        }
        rules.remove_left_recursion();
        let result = <BTreeMap<_, _>>::from(&rules);
        if VERBOSE {
            println!("=>");
            print_production_rules(&rules);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        rules.remove_left_recursion();
        let result = <BTreeMap<_, _>>::from(&rules);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
    }
}

#[test]
fn prs_left_factorize() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
           // A -> d A_1 | A A_2
           // B -> A f | g | h
           // A_1 -> ε | e
           // A_2 -> b | c
            0 => prod!(t 3, nt 2; nt 0, nt 3),
            1 => prod!(nt 0, t 5; t 6; t 7),
            2 => prod!(e; t 4),
            3 => prod!(t 1; t 2),
        ]),
        (1, btreemap![
            // A -> b A_4 | c b | d c A_3
            // A_1 -> ε | d
            // A_2 -> f | g
            // A_3 -> ε | e
            // A_4 -> b c A_1 | c g | d e A_2
            0 => prod!(t 1, nt 4; t 2, t 1; t 3, t 2, nt 3),
            1 => prod!(e; t 3),
            2 => prod!(t 5; t 6),
            3 => prod!(e; t 4),
            4 => prod!(t 1, t 2, nt 1; t 2, t 6; t 3, t 4, nt 2),
        ]),
        (2, btreemap![]),
        (3, btreemap![
            0 => prod!(t 0),
            1 => prod!(t 2, nt 4),
            2 => prod!(t 2; t 1),
            3 => prod!(t 3),
            4 => prod!(e; t 0),
        ]),
        (4, btreemap![
           // E -> E E_1 | T
           // T -> T T_1 | F
           // F -> ( E ) | NUM | ID
           // E_1 -> + T | - T
           // T_1 -> * F | / F
            0 => prod!(nt 0, nt 3; nt 1),
            1 => prod!(nt 1, nt 4; nt 2),
            2 => prod!(t 4, nt 0, t 5; t 6; t 7),
            3 => prod!(t 0, nt 1; t 1, nt 1),
            4 => prod!(t 2, nt 2; t 3, nt 2),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let mut rules = build_prs(test_id);
        if VERBOSE {
            println!("test {test_id}:");
            print_production_rules(&rules);
        }
        rules.left_factorize();
        let result = BTreeMap::<_, _>::from(&rules);
        if VERBOSE {
            println!("=>");
            print_production_rules(&rules);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        rules.left_factorize();
        let result = BTreeMap::<_, _>::from(&rules);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
    }
}

#[test]
fn prs_ll1_from() {
    let tests: Vec<(u32, BTreeMap<VarId, ProdRule>)> = vec![
        (0, btreemap![
           // A -> d A_2
           // B -> A f | g | h
           // A_1 -> b A_1 | c A_1 | ε
           // A_2 -> e A_1 | A_1
            0 => prod!(t 3, nt 3),
            1 => prod!(nt 0, t 5; t 6; t 7),
            2 => prod!(t 1, nt 2; t 2, nt 2; e),
            3 => prod!(t 4, nt 2; nt 2),
        ]),
        (1, btreemap![
           // A -> b A_4 | c b | d c A_3
           // A_1 -> ε | d
           // A_2 -> f | g
           // A_3 -> ε | e
           // A_4 -> b c A_1 | c g | d e A_2
            0 => prod!(t 1, nt 4; t 2, t 1; t 3, t 2, nt 3),
            1 => prod!(e; t 3),
            2 => prod!(t 5; t 6),
            3 => prod!(e; t 4),
            4 => prod!(t 1, t 2, nt 1; t 2, t 6; t 3, t 4, nt 2),
        ]),
        (4, btreemap![
           // E -> T E_1
           // T -> F T_1
           // F -> ( E ) | NUM | ID
           // E_1 -> + T E_1 | - T E_1 | ε
           // T_1 -> * F T_1 | / F T_1 | ε
            0 => prod!(nt 1, nt 3),
            1 => prod!(nt 2, nt 4),
            2 => prod!(t 4, nt 0, t 5; t 6; t 7),
            3 => prod!(t 0, nt 1, nt 3; t 1, nt 1, nt 3; e),
            4 => prod!(t 2, nt 2, nt 4; t 3, nt 2, nt 4; e),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let rules_lr = build_prs(test_id);
        if VERBOSE {
            println!("test {test_id}:");
            print_production_rules(&rules_lr);
        }
        let rules_ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&rules_ll1);
        if VERBOSE {
            println!("=>");
            print_production_rules(&rules_ll1);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        let rules_ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&rules_ll1);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
   }
}
