#![cfg(test)]

use std::collections::{BTreeMap, BTreeSet, HashSet};
use super::*;
use crate::dfa::TokenId;
use crate::{btreemap, gnode, hashmap, hashset, LL1, LR, prod, prodf, sym};

// ---------------------------------------------------------------------------------------------
// Supporting functions

pub(super) fn print_production_rules<T>(prods: &ProdRuleSet<T>) {
    println!("    {}", prods.get_prods_iter().map(|(var, p)|
        format!("{} -> {}",
                Symbol::NT(var).to_str(prods.get_symbol_table()),
                prod_to_string(p, prods.get_symbol_table()))
    ).join("\n    "));
}

pub(super) fn print_factors<T>(ll1: &ProdRuleSet<T>, factors: &Vec<(VarId, ProdFactor)>) {
    println!("factors:\n{}",
             factors.iter().enumerate().map(|(id, (v, f))|
                 format!("            // - {id}: {} -> {}", Symbol::NT(*v).to_str(ll1.get_symbol_table()),
                         f.iter().map(|s| s.to_str(ll1.get_symbol_table())).join(" "))
    ).join("\n"));
}

pub(crate) fn symbol_to_macro(s: &Symbol) -> String {
    match s {
        Symbol::Empty => "e".to_string(),
        Symbol::T(x) => format!("t {x}"),
        Symbol::NT(x) => format!("nt {x}"),
        Symbol::Rec(x) => format!("rec {x}"),
        Symbol::Exit(x) => format!("exit {x}"),
        Symbol::End => "end".to_string(),
    }
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
        14 => { // 1?(2|3)?
            let cc = tree.add_root(gnode!(&));
            tree.addc(Some(cc), gnode!(?), gnode!(nt 1));
            let m = tree.add(Some(cc), gnode!(?));
            tree.addc_iter(Some(m), gnode!(|), [gnode!(nt 2), gnode!(nt 3)]);
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
        // 0 => "&(1, 10)", 10 => "|(&(2, 3, 10), ε)"
        (12, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 3, nt 10; e)]),
        // 0 => "&(1, 10)", 10 => "|(&(2, 3, 10), &(4, 10), ε)"
        (13, btreemap![
            0 => prod!(nt 1, nt 10),
            10 => prod!(nt 2, nt 3, nt 10; nt 4, nt 10; e)]),
        (14, btreemap![ // 1?(2|3)?
            0 => prod!(nt 1, nt 2; nt 1, nt 3; nt 1; nt 2; nt 3; e)
        ])
    ];
    for (test_id, expected) in tests {
        let trees = build_rts(test_id);
        let mut rules = ProdRuleSet::from(trees);
        rules.simplify();
        let result = rules.get_prods_iter().map(|(id, p)| (id, p.clone())).collect::<BTreeMap<_, _>>();
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

// ---------------------------------------------------------------------------------------------
// ProdRuleSet

fn print_expected_code(result: &BTreeMap<VarId, ProdRule>) {
    println!("\n            {}", result.iter().map(|(i, p)|
        format!("{i} => prod!({}),", p.iter().map(|f| f.iter().map(|s| symbol_to_macro(s))
            .join(", ")).join("; "))).join("\n            "))
}

fn print_ll1_table(symbol_table: Option<&SymbolTable>, parsing_table: &LLParsingTable, indent: usize) {
    let LLParsingTable { num_nt, num_t, factors, table } = parsing_table;
    let error = factors.len() as VarId;
    let str_nt = (0..*num_nt).map(|i| Symbol::NT(i as VarId).to_str(symbol_table)).to_vec();
    let max_nt_len = str_nt.iter().map(|s| s.len()).max().unwrap();
    let str_t = (0..*num_t).map(|j| if j + 1 < *num_t { Symbol::T(j as VarId).to_str(symbol_table) } else { "$".to_string() }).to_vec();
    let max_t_len = str_t.iter().map(|s| s.len()).max().unwrap().max(2);
    let t_len = str_t.iter().map(|s| s.len().max(2)).to_vec();
    println!("{:<i$}// {:<w$} | {}", "", "", (0..*num_t).map(|j| format!("{:^w$}", str_t[j], w = t_len[j])).join(" "), w = max_nt_len, i = indent);
    println!("{:<i$}// {:-<w$}-+-{:-<t$}", "", "", "", w = max_nt_len, t = *num_t + t_len.iter().sum::<usize>(), i = indent);
    for i in 0..*num_nt {
        print!("{:<i$}// {:<w$} |", "", str_nt[i], w = max_nt_len, i = indent);
        for j in 0..*num_t {
            let value = table[i * num_t + j];
            if value < error {
                print!(" {:^w$}", value, w = t_len[j]);
            } else {
                print!(" {:^w$}", ".", w = t_len[j]);
            }
        }
        println!();
    }
}

fn print_logs<T>(rules: &ProdRuleSet<T>) {
    let mut msg = Vec::<String>::new();
    msg.push(format!("Errors: {}", rules.log.num_errors()));
    msg.extend(rules.log.get_errors().cloned());
    msg.push("".to_string());
    msg.push(format!("Warnings: {}", rules.log.num_warnings()));
    msg.extend(rules.log.get_warnings().cloned());
    msg.push("".to_string());
    msg.push(format!("Notes: {}", rules.log.num_notes()));
    msg.extend(rules.log.get_notes().cloned());
    println!("{}\n", msg.join("\n"));
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
    symbol_table.extend_non_terminals(["E".to_string()]);
    if has_term {
        symbol_table.extend_non_terminals(["T".to_string()]);
    }
    symbol_table.extend_non_terminals(["F".to_string()]);
}

impl<T> From<&ProdRuleSet<T>> for BTreeMap<VarId, ProdRule> {
    fn from(rules: &ProdRuleSet<T>) -> Self {
        rules.get_prods_iter().map(|(var, p)| (var, p.clone())).collect::<BTreeMap<_, _>>()

    }
}

pub(crate) fn build_prs(id: u32) -> ProdRuleSet<General> {
    let mut rules = ProdRuleSet::new();
    let mut symbol_table = SymbolTable::new();
    let prods = &mut rules.prods;
    let mut start = Some(0);
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
            symbol_table.extend_non_terminals(["A".to_string(), "A1".to_string(), "A2".to_string()]);
            prods.extend([
                prod!(nt 1, nt 2, t 2, t 2), // A -> A1 A2 ; ;
                prod!(t 0, nt 1; e),    // A1 -> - A1 | ε
                prod!(t 1, nt 2; e),    // A2 -> + A2 | ε
            ]);
        }
        6 => {
            // another starting NT
            symbol_table.extend_terminals([
                ("SUB".to_string(), Some("-".to_string())),
                ("ADD".to_string(), Some("+".to_string())),
                ("SEMI".to_string(), Some(";".to_string()))
            ]);
            symbol_table.extend_non_terminals(["A1".to_string(), "A".to_string(), "A2".to_string()]);
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
            symbol_table.extend_non_terminals(["A1".to_string(), "X".to_string(), "A".to_string(), "A2".to_string()]);
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
            // A -> A a A | A b A | c
            prods.extend([
                prod!(nt 0, t 0, nt 0; nt 0, t 1, nt 0; t 2),
            ])
        }
        10 => {
            // A -> A a A | A b A | c | d
            prods.extend([
                prod!(nt 0, t 0, nt 0; nt 0, t 1, nt 0; t 2; t 3),
            ])
        }
        11 => {
            // A -> A a | A b | A c d A | A e f A | g | h i
            prods.extend([
                prod!(nt 0, t 0; nt 0, t 1; nt 0, t 2, t 3, nt 0; nt 0, t 4, t 5, nt 0; t 6; t 7, t 8),
            ])
        }
        12 => {
            // A -> A a | A b | A c d A | A e f A | g
            prods.extend([
                prod!(nt 0, t 0; nt 0, t 1; nt 0, t 2, t 3, nt 0; nt 0, t 4, t 5, nt 0; t 6),
            ])
        }
        13 => {
            // classical ambiguous arithmetic grammar
            // E -> E : E | E ^ E | E / E | E * E | E - E | E + E | F
            // F -> ( E ) | NUM | ID
            // T:  0:-, 1:+, 2:/, 3:*, 4:(, 5:), 6:NUM, 7:ID, 8:^, 9::
            // NT: 0:E, 1:F
            def_arith_symbols(&mut symbol_table, false);
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),  // exponent, right-associative
                ("DUM".to_string(), Some(":".to_string())),  // dummy high-priority left-associative; a:b = max(a,b)
            ]);
            prods.extend([
                prod!(nt 0, t 9, nt 0; nt 0, t 8, nt 0; nt 0, t 2, nt 0; nt 0, t 3, nt 0; nt 0, t 0, nt 0; nt 0, t 1, nt 0; nt 1),
                prod!(t 4, nt 0, t 5; t 6; t 7),
            ]);
        }
        14 => {
            // A -> A A | a
            prods.extend([
                prod!(nt 0, nt 0; t 0)
            ]);
        }
        15 => {
            // classical ambiguous arithmetic grammar
            // E -> E / E | E * E | E - E | E + E | - T | T
            // T -> T : T | T ^ T | F
            // F -> N | I | ( E )
            // T:  0:-, 1:+, 2:/, 3:*, 4:(, 5:), 6:NUM, 7:ID, 8:^, 9::
            // NT: 0:E, 1:T, 2:F
            def_arith_symbols(&mut symbol_table, true);
            symbol_table.extend_terminals([
                ("EXP".to_string(), Some("^".to_string())),  // exponent, right-associative
                ("DUM".to_string(), Some(":".to_string())),  // dummy high-priority left-associative; a:b = max(a,b)
            ]);
            prods.extend([
                prod!(nt 0, t 2, nt 0; nt 0, t 3, nt 0; nt 0, t 0, nt 0; nt 0, t 1, nt 0; t 0, nt 1; nt 1),
                prod!(nt 1, t 9, nt 1; nt 1, t 8, nt 1; nt 2),
                prod!(t 6; t 7; t 4, nt 0, t 5),
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
            symbol_table.extend_non_terminals([
                /* 0 */ "STRUCT".to_string(),
                /* 1 */ "LIST".to_string(),
            ]);
            prods.extend([
                prod!(t 0, t 5, t 1, nt 1),
                prod!(t 5, t 3, t 5, t 4, nt 1; t 2),
            ])
        }
        21 => {
            //  A -> A a1 | A a2 | A b1 A | A b2 A | c1 | c2 [ | d1 A | d2 A ]
            //       ^^^^^^^^^^^   ^^^^^^^^^^^^^^^               ^^^^^^^^^^^
            symbol_table.extend_terminals([
                ("a1".to_string(), Some("a1".to_string())), // 0
                ("a2".to_string(), Some("a2".to_string())), // 1
                ("b1".to_string(), Some("b1".to_string())), // 2
                ("b2".to_string(), Some("b2".to_string())), // 3
                ("c1".to_string(), Some("c1".to_string())), // 4
                ("c2".to_string(), Some("c2".to_string())), // 5
                ("d1".to_string(), Some("d1".to_string())), // 6
                ("d2".to_string(), Some("d2".to_string())), // 7
            ]);
            if false {
                // TODO: needs a new grammar change (polymorphic recursive rule)
                prods.extend([
                    prod!(nt 0, t 0; nt 0, t 1; nt 0, t 2, nt 0; nt 0, t 3, nt 0; t 4; t 5; t 6, nt 0; t 7, nt 0),
                ]);
            } else {
                prods.extend([
                    prod!(nt 0, t 0; nt 0, t 1; nt 0, t 2, nt 0; nt 0, t 3, nt 0; t 4; t 5),
                ]);
                //      A -> A a1 | A a2 | A b1 A | A b2 A | c1 | c2
                // =>
                //      A -> c1 A_2 | c2 A_2
                //      A_1 -> c1 | c2
                //      A_2 -> a1 A_2 | a2 A_2 | b1 A_1 A_2 | b2 A_1 A_2 | ε
            }
        }
        22 => {
            // E -> E * E | E '&' '*' E | E + E | E '&' '+' E | id
            symbol_table.extend_terminals([
                ("*".to_string(), Some("*".to_string())),
                ("+".to_string(), Some("+".to_string())),
                ("&".to_string(), Some("&".to_string())),
                ("id".to_string(), None),
            ]);
            symbol_table.extend_non_terminals(["E".to_string()]);
            prods.extend([
                prod!(nt 0, t 0, nt 0; nt 0, t 2, t 0, nt 0; nt 0, t 1, nt 0; nt 0, t 2, t 1, nt 0; t 3),
            ]);
        }
        23 => {
            // E -> E * E | E '&' '*' E | E + E | E '&' '+' E | F
            // F -> id | num
            symbol_table.extend_terminals([
                ("*".to_string(), Some("*".to_string())),
                ("+".to_string(), Some("+".to_string())),
                ("&".to_string(), Some("&".to_string())),
                ("id".to_string(), None),
                ("num".to_string(), None),
            ]);
            symbol_table.extend_non_terminals(["E".to_string(), "F".to_string()]);
            prods.extend([
                prod!(nt 0, t 0, nt 0; nt 0, t 2, t 0, nt 0; nt 0, t 1, nt 0; nt 0, t 2, t 1, nt 0; nt 1),
                prod!(t 3; t 4),
            ]);
        }

        // ambiguity?
        100 => {
            // A -> A a A b | c (amb removed)
            prods.extend([
                prod!(nt 0, t 0, nt 0, t 1; t 2),
            ])
        }
        101 => {
            // A -> a A A | b (no amb)
            prods.extend([
                prod!(t 0, nt 0, nt 0; t 1),
            ])
        }
        102 => {
            // A -> A a A b A | c (amb removed)
            prods.extend([
                prod!(nt 0, t 0, nt 0, t 1, nt 0; t 2)
            ])
        }

        // warnings and errors
        1000 => { // A -> A a  (missing non-recursive factor)
            prods.extend([
                prod!(nt 0, t 0)
            ]);
        },
        1001 => { // A -> A a A A | b (cannot remove recursion)
            prods.extend([
                prod!(nt 0, t 0, nt 0, nt 0; t 1)
            ]);
        },
        1002 => { // A -> A a A a A | b (ambiguous)
            prods.extend([
                prod!(nt 0, t 0, nt 0, t 0, nt 0; t 1)
            ]);
        },
        1003 => { // no terminal in grammar
            prods.extend([
                prod!(nt 1),
                prod!(nt 2),
                prod!(nt 0),
            ]);
        },
        1004 => { // no terminal used in table
            prods.extend([
                prod!(nt 1),
                prod!(nt 2, t 0),
                prod!(nt 0),
            ]);
        },
        1005 => {
            symbol_table.extend_terminals([("a".to_string(), None), ("b".to_string(), None)]);
            symbol_table.extend_non_terminals(["A".to_string(), "B".to_string()]);
            prods.extend([
                prod!(t 1),
                prod!(t 1),
            ]);
        }
        _ => {}
    };
    rules.calc_num_symbols();
    if symbol_table.get_terminals().is_empty() {
        symbol_table.extend_terminals((0..rules.num_t).map(|i| (format!("{}", char::from(i as u8 + 97)), None)));
    }
    if symbol_table.get_non_terminals().is_empty() {
        assert!(rules.num_nt <= 26);
        symbol_table.extend_non_terminals((0..rules.num_nt as u8).map(|i| format!("{}", char::from(i + 65))));
    }
    rules.set_symbol_table(symbol_table);
    if let Some(start) = start {
        rules.set_start(start);
    }
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
        (8, btreemap![
            // A -> b A_1
            // A_1 -> a b A_1 | ε
            0 => prod!(t 1, nt 1),
            1 => prod!(t 0, t 1, nt 1; e),
        ]),
        (9, btreemap![ // A -> A a A | A b A | c
            // A -> c A_1
            // A_1 -> a c A_1 | b c A_1 | ε
            0 => prod!(t 2, nt 1),
            1 => prod!(t 0, t 2, nt 1; t 1, t 2, nt 1; e)
        ]),
        (10, btreemap![ // A -> A a A | A b A | c | d
            // A -> c A_2 | d A_2
            // A_1 -> c | d
            // A_2 -> a A_1 A_2 | b A_1 A_2 | ε
            0 => prod!(t 2, nt 2; t 3, nt 2),
            1 => prod!(t 2; t 3),
            2 => prod!(t 0, nt 1, nt 2; t 1, nt 1, nt 2; e)
        ]),
        (11, btreemap![ // A -> A a | A b | A c d A | A e f A | g | h i
            // A -> g A_2 | h i A_2
            // A_1 -> g | h i
            // A_2 -> a A_2 | b A_2 | c d A_1 A_2 | e f A_1 A_2 | ε
            0 => prod!(t 6, nt 2; t 7, t 8, nt 2),
            1 => prod!(t 6; t 7, t 8),
            2 => prod!(t 0, nt 2; t 1, nt 2; t 2, t 3, nt 1, nt 2; t 4, t 5, nt 1, nt 2; e)
        ]),
        (12, btreemap![ // A -> A a | A b | A c d A | A e f A | g
            // A -> g A_1
            // A_1 -> a A_1 | b A_1 | c d g A_1 | e f g A_1 | ε
            0 => prod!(t 6, nt 1),
            1 => prod!(t 0, nt 1; t 1, nt 1; t 2, t 3, t 6, nt 1; t 4, t 5, t 6, nt 1; e)
        ]),
        (13, btreemap![
            // E -> F E_1
            // F -> ( E ) | N | I
            // E_1 -> : F E_1 | ^ F E_1 | / F E_1 | * F E_1 | - F E_1 | + F E_1 | ε
            0 => prod!(nt 1, nt 2),
            1 => prod!(t 4, nt 0, t 5; t 6; t 7),
            2 => prod!(t 9, nt 1, nt 2; t 8, nt 1, nt 2; t 2, nt 1, nt 2; t 3, nt 1, nt 2; t 0, nt 1, nt 2; t 1, nt 1, nt 2; e),
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
        assert_eq!(rules.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(rules.log.get_warnings().join("\n"), "", "test {test_id} failed");
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
        assert_eq!(rules.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(rules.log.get_warnings().join("\n"), "", "test {test_id} failed");
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
        (8, btreemap![
            // A -> b A_1
            // A_1 -> a b A_1 | ε
            0 => prod!(t 1, nt 1),
            1 => prod!(t 0, t 1, nt 1; e),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected) in tests {
        let rules_lr = build_prs(test_id);
        if VERBOSE {
            println!("test {test_id}:");
            print_production_rules(&rules_lr);
        }
        let ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&ll1);
        if VERBOSE {
            println!("=>");
            print_production_rules(&ll1);
            print_expected_code(&result);
        }
        assert_eq!(result, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
        let rules_ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        let result = BTreeMap::<_, _>::from(&rules_ll1);
        assert_eq!(result, expected, "test {test_id} failed on 2nd operation");
   }
}

#[test]
#[should_panic]
/// We test that the code compiles, but it must also panic because the `remove_ambiguity()`
/// method isn't implemented yet for LR grammars.
fn prs_lr_from() {
    let mut test_id = 0;
    loop {
        let rules = build_prs(test_id);
        if rules.prods.is_empty() {
            break;
        }
        let lr = ProdRuleSet::<LR>::from(rules);
        test_id += 1;
    }
}

#[test]
fn prs_calc_first() {
    let tests: Vec<(u32, VarId, HashMap<Symbol, HashSet<Symbol>>)> = vec![
        (4, 0, hashmap![
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            //
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F, 3:E_1, 4:T_1
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(t 3) => hashset![sym!(t 3)],
            sym!(t 4) => hashset![sym!(t 4)],
            sym!(t 5) => hashset![sym!(t 5)],
            sym!(t 6) => hashset![sym!(t 6)],
            sym!(t 7) => hashset![sym!(t 7)],
            sym!(nt 0) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 1) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 2) => hashset![sym!(t 4), sym!(t 6), sym!(t 7)],
            sym!(nt 3) => hashset![sym!(t 0), sym!(t 1), sym!(e)],
            sym!(nt 4) => hashset![sym!(t 2), sym!(t 3), sym!(e)],
        ]),
        (5, 0, hashmap![
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(t 2) => hashset![sym!(t 2)],
            sym!(nt 0) => hashset![sym!(t 0), sym!(t 1), sym!(t 2)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(e)],
            sym!(nt 2) => hashset![sym!(t 1), sym!(e)]
        ]),
        (8, 0, hashmap![
            sym!(e) => hashset![sym!(e)],
            sym!(t 0) => hashset![sym!(t 0)],
            sym!(t 1) => hashset![sym!(t 1)],
            sym!(nt 0) => hashset![sym!(t 1)],
            sym!(nt 1) => hashset![sym!(e), sym!(t 0)],
        ])
    ];
    const VERBOSE: bool = false;
    for (test_id, start, expected) in tests {
        let rules_lr = build_prs(test_id);
        if VERBOSE {
            println!("test {test_id}:");
        }
        let mut ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        if VERBOSE {
            print_production_rules(&ll1);
            let b = map_and_print_first(&first, ll1.get_symbol_table());
            for (sym, set) in &b {
                println!("            sym!({}) => hashset![{}],", symbol_to_macro(sym),
                         set.iter().map(|s| format!("sym!({})", symbol_to_macro(s))).join(", "));
            }
        }
        assert_eq!(first, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
   }
}

#[test]
fn prs_calc_follow() {
    let tests: Vec<(u32, VarId, HashMap<Symbol, HashSet<Symbol>>)> = vec![
        (4, 0, hashmap![
            // E -> T E_1
            // T -> F T_1
            // F -> ( E ) | NUM | ID
            // E_1 -> + T E_1 | - T E_1 | ε
            // T_1 -> * F T_1 | / F T_1 | ε
            //
            // T:  0:+, 1:-, 2:*, 3:/, 4:(, 5:), 6:NUM, 7:ID,
            // NT: 0:E, 1:T, 2:F, 3:E_1, 4:T_1
            sym!(nt 0) => hashset![sym!(t 5), sym!(end)],
            sym!(nt 1) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(end)],
            sym!(nt 2) => hashset![sym!(t 0), sym!(t 1), sym!(t 2), sym!(t 3), sym!(t 5), sym!(end)],
            sym!(nt 3) => hashset![sym!(t 5), sym!(end)],
            sym!(nt 4) => hashset![sym!(t 0), sym!(t 1), sym!(t 5), sym!(end)],
        ]),
        (5, 0, hashmap![
            sym!(nt 0) => hashset![sym!(end)],
            sym!(nt 1) => hashset![sym!(t 1), sym!(t 2)],
            sym!(nt 2) => hashset![sym!(t 2)],
        ]),
        (8, 0, hashmap![
            sym!(nt 0) => hashset![sym!(end)],
            sym!(nt 1) => hashset![sym!(end)],
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, start, expected) in tests {
        let rules_lr = build_prs(test_id);
        if VERBOSE {
            println!("test {test_id}:");
        }
        let mut ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        if VERBOSE {
            print_production_rules(&ll1);
            let b = map_and_print_follow(&follow, ll1.get_symbol_table());
            for (sym, set) in &b {
                println!("            sym!({}) => hashset![{}],", symbol_to_macro(sym),
                         set.iter().map(|s| format!("sym!({})", symbol_to_macro(s))).join(", "));
            }
        }
        assert_eq!(follow, expected, "test {test_id} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id} failed");
        assert_eq!(ll1.log.get_warnings().join("\n"), "", "test {test_id} failed");
   }
}

#[test]
fn prs_calc_table() {
    let tests: Vec<(u32, VarId, usize, Vec<(VarId, ProdFactor)>, Vec<VarId>)> = vec![
        (4, 0, 0, vec![
            // - 0: E -> T E_1
            // - 1: T -> F T_1
            // - 2: F -> ( E )
            // - 3: F -> NUM
            // - 4: F -> ID
            // - 5: E_1 -> + T E_1
            // - 6: E_1 -> - T E_1
            // - 7: E_1 -> ε
            // - 8: T_1 -> * F T_1
            // - 9: T_1 -> / F T_1
            // - 10: T_1 -> ε
            (0, prodf!(nt 1, nt 3)),
            (1, prodf!(nt 2, nt 4)),
            (2, prodf!(t 4, nt 0, t 5)),
            (2, prodf!(t 6)),
            (2, prodf!(t 7)),
            (3, prodf!(t 0, nt 1, nt 3)),
            (3, prodf!(t 1, nt 1, nt 3)),
            (3, prodf!(e)),
            (4, prodf!(t 2, nt 2, nt 4)),
            (4, prodf!(t 3, nt 2, nt 4)),
            (4, prodf!(e)),
        ], vec![
            //     |   -   +   /   *   (   )   N   I   $
            // ----+-------------------------------------
            //   E |   .   .   .   .   0   .   0   0   .
            //   T |   .   .   .   .   1   .   1   1   .
            //   F |   .   .   .   .   2   .   3   4   .
            // E_1 |   5   6   .   .   .   7   .   .   7
            // T_1 |  10  10   8   9   .  10   .   .  10
             11,  11,  11,  11,   0,  11,   0,   0,  11,
             11,  11,  11,  11,   1,  11,   1,   1,  11,
             11,  11,  11,  11,   2,  11,   3,   4,  11,
              5,   6,  11,  11,  11,   7,  11,  11,   7,
             10,  10,   8,   9,  11,  10,  11,  11,  10,
        ]),
        (5, 0, 0, vec![
            // - 0: A -> A1 A2 ; ;
            // - 1: A1 -> - A1
            // - 2: A1 -> ε
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, prodf!(nt 1, nt 2, t 2, t 2)),
            (1, prodf!(t 0, nt 1)),
            (1, prodf!(e)),
            (2, prodf!(t 1, nt 2)),
            (2, prodf!(e)),
        ], vec![
            //    |   -   +   ;   $
            // ---+-----------------
            //  A |   0   0   0   .
            // A1 |   1   2   2   .
            // A2 |   .   3   4   .
            0, 0, 0, 5,
            1, 2, 2, 5,
            5, 3, 4, 5,
        ]),
        (6, 1, 0, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: A -> A1 A2 ;
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, prodf!(t 0, nt 0)),
            (0, prodf!(e)),
            (1, prodf!(nt 0, nt 2, t 2)),
            (2, prodf!(t 1, nt 2)),
            (2, prodf!(e)),
        ], vec![
            //    |   -   +   ;   $
            // ---+-----------------
            // A1 |   0   1   1   .
            //  A |   2   2   2   .
            // A2 |   .   3   4   .
              0,   1,   1,   5,
              2,   2,   2,   5,
              5,   3,   4,   5,
        ]),
        (7, 1, 0, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: X -> A
            // - 3: A -> A1 A2 ;
            // - 4: A2 -> + A2
            // - 5: A2 -> ε
            (0, prodf!(t 1, nt 0)),
            (0, prodf!(e)),
            (1, prodf!(t 0, nt 2)),
            (2, prodf!(nt 0, nt 3, t 3)),
            (3, prodf!(t 2, nt 3)),
            (3, prodf!(e)),
        ], vec![
            //    |   >   -   +   ;   $
            // ---+---------------------
            // A1 |   .   0   1   1   .
            //  X |   2   .   .   .   .
            //  A |   .   3   3   3   .
            // A2 |   .   .   4   5   .
              6,   0,   1,   1,   6,
              2,   6,   6,   6,   6,
              6,   3,   3,   3,   6,
              6,   6,   4,   5,   6,
        ]),
        (7, 2, 2, vec![
            // - 0: A1 -> - A1
            // - 1: A1 -> ε
            // - 2: A -> A1 A2 ;
            // - 3: A2 -> + A2
            // - 4: A2 -> ε
            (0, prodf!(t 1, nt 0)),
            (0, prodf!(e)),
            (1, prodf!(nt 0, nt 2, t 3)),
            (2, prodf!(t 2, nt 2)),
            (2, prodf!(e)),
        ], vec![
            //    |   >   -   +   ;   $
            // ---+---------------------
            // A1 |   .   0   1   1   .
            //  A |   .   2   2   2   .
            // A2 |   .   .   3   4   .
              5,   0,   1,   1,   5,
              5,   2,   2,   2,   5,
              5,   5,   3,   4,   5,
        ]),
        (8, 0, 0, vec![
            // A -> A a A | b
            // - 0: A -> b A_1
            // - 1: A_1 -> a b A_1
            // - 2: A_1 -> ε
            (0, prodf!(t 1, nt 1)),
            (1, prodf!(t 0, t 1, nt 1)),
            (1, prodf!(e)),
        ], vec![
            //     |   a   b   $
            // ----+-------------
            //   A |   .   0   .
            // A_1 |   1   .   2
              3,   0,   3,
              1,   3,   2,
        ]),
        (13, 0, 0, vec![
            // - 0: E -> F E_1
            // - 1: F -> ( E )
            // - 2: F -> N
            // - 3: F -> I
            // - 4: E_1 -> : F E_1
            // - 5: E_1 -> ^ F E_1
            // - 6: E_1 -> / F E_1
            // - 7: E_1 -> * F E_1
            // - 8: E_1 -> - F E_1
            // - 9: E_1 -> + F E_1
            // - 10: E_1 -> ε
            (0, prodf!(nt 1, nt 2)),
            (1, prodf!(t 4, nt 0, t 5)),
            (1, prodf!(t 6)),
            (1, prodf!(t 7)),
            (2, prodf!(t 9, nt 1, nt 2)),
            (2, prodf!(t 8, nt 1, nt 2)),
            (2, prodf!(t 2, nt 1, nt 2)),
            (2, prodf!(t 3, nt 1, nt 2)),
            (2, prodf!(t 0, nt 1, nt 2)),
            (2, prodf!(t 1, nt 1, nt 2)),
            (2, prodf!(e)),
        ], vec![
            //     |   -   +   /   *   (   )   N   I   ^   :   $
            // ----+---------------------------------------------
            //   E |   .   .   .   .   0   .   0   0   .   .   .
            //   F |   .   .   .   .   1   .   2   3   .   .   .
            // E_1 |   8   9   6   7   .  10   .   .   5   4  10
             11,  11,  11,  11,   0,  11,   0,   0,  11,  11,  11,
             11,  11,  11,  11,   1,  11,   2,   3,  11,  11,  11,
              8,   9,   6,   7,  11,  10,  11,  11,   5,   4,  10,
        ]),
        (14, 0, 0, vec![
            // - 0: A -> a A_1
            // - 1: A_1 -> a A_1
            // - 2: A_1 -> ε
            (0, prodf!(t 0, nt 1)),
            (1, prodf!(t 0, nt 1)),
            (1, prodf!(e)),
        ], vec![
            //     |   a   $
            // ----+---------
            //   A |   0   .
            // A_1 |   1   2
              0,   3,
              1,   2,
        ]),
        (15, 0, 0, vec![
            // - 0: E -> - T E_2
            // - 1: E -> T E_2
            // - 2: T -> F T_1
            // - 3: F -> N
            // - 4: F -> I
            // - 5: F -> ( E )
            // - 6: E_1 -> - T
            // - 7: E_1 -> T
            // - 8: E_2 -> / E_1 E_2
            // - 9: E_2 -> * E_1 E_2
            // - 10: E_2 -> - E_1 E_2
            // - 11: E_2 -> + E_1 E_2
            // - 12: E_2 -> ε
            // - 13: T_1 -> : F T_1
            // - 14: T_1 -> ^ F T_1
            // - 15: T_1 -> ε
            (0, prodf!(t 0, nt 1, nt 4)),
            (0, prodf!(nt 1, nt 4)),
            (1, prodf!(nt 2, nt 5)),
            (2, prodf!(t 6)),
            (2, prodf!(t 7)),
            (2, prodf!(t 4, nt 0, t 5)),
            (3, prodf!(t 0, nt 1)),
            (3, prodf!(nt 1)),
            (4, prodf!(t 2, nt 3, nt 4)),
            (4, prodf!(t 3, nt 3, nt 4)),
            (4, prodf!(t 0, nt 3, nt 4)),
            (4, prodf!(t 1, nt 3, nt 4)),
            (4, prodf!(e)),
            (5, prodf!(t 9, nt 2, nt 5)),
            (5, prodf!(t 8, nt 2, nt 5)),
            (5, prodf!(e)),
        ], vec![
            //     |   -   +   /   *   (   )   N   I   ^   :   $
            // ----+---------------------------------------------
            //   E |   0   .   .   .   1   .   1   1   .   .   .
            //   T |   .   .   .   .   2   .   2   2   .   .   .
            //   F |   .   .   .   .   5   .   3   4   .   .   .
            // E_1 |   6   .   .   .   7   .   7   7   .   .   .
            // E_2 |  10  11   8   9   .  12   .   .   .   .  12
            // T_1 |  15  15  15  15   .  15   .   .  14  13  15
              0,  16,  16,  16,   1,  16,   1,   1,  16,  16,  16,
             16,  16,  16,  16,   2,  16,   2,   2,  16,  16,  16,
             16,  16,  16,  16,   5,  16,   3,   4,  16,  16,  16,
              6,  16,  16,  16,   7,  16,   7,   7,  16,  16,  16,
             10,  11,   8,   9,  16,  12,  16,  16,  16,  16,  12,
             15,  15,  15,  15,  16,  15,  16,  16,  14,  13,  15,
        ]),
        (17, 0, 0, vec![
            // - 0: A -> B
            // - 1: A -> a
            // - 2: B -> C )
            // - 3: C -> ( A
            (0, prodf!(nt 1)),
            (0, prodf!(t 0)),
            (1, prodf!(nt 2, t 2)),
            (2, prodf!(t 1, nt 0)),
        ], vec![
            //   |   a   (   )   $
            // --+-----------------
            // A |   1   0   .   .
            // B |   .   2   .   .
            // C |   .   3   .   .
              1,   0,   4,   4,
              4,   2,   4,   4,
              4,   3,   4,   4,
        ]),
        (18, 0, 0, vec![
            // - 0: A -> a
            (0, prodf!(t 0)),
        ], vec![
            //   |   a   $
            // --+---------
            // A |   0   .
              0,   1,
        ]),
        (19, 0, 0, vec![
            // - 0: A -> a
            (0, prodf!(t 0)),
            (0, prodf!(e)),
        ], vec![
            //   |   a   $
            // --+---------
            // A |   0   1
              0,   1,
        ]),
        (20, 0, 0, vec![
            // - 0: STRUCT -> struct id { LIST
            // - 1: LIST -> id : id ; LIST
            // - 2: LIST -> }
            (0, prodf!(t 0, t 5, t 1, nt 1)),
            (1, prodf!(t 5, t 3, t 5, t 4, nt 1)),
            (1, prodf!(t 2)),
        ], vec![
            //        | struct {  }  :  ;  id $
            // -------+--------------------------
            // STRUCT |   0    .  .  .  .  .  .
            // LIST   |   .    .  2  .  .  1  .
              0,   3,   3,   3,   3,   3,   3,
              3,   3,   2,   3,   3,   1,   3,
        ]),
        (22, 0, 0, vec![
            // - 0: E -> id E_1
            // - 1: E_1 -> ε
            // - 2: E_1 -> * id E_1
            // - 3: E_1 -> + id E_1
            // - 4: E_1 -> & E_2
            // - 5: E_2 -> * id E_1
            // - 6: E_2 -> + id E_1
            (0, prodf!(t 3, nt 1)),
            (1, prodf!(e)),
            (1, prodf!(t 0, t 3, nt 1)),
            (1, prodf!(t 1, t 3, nt 1)),
            (1, prodf!(t 2, nt 2)),
            (2, prodf!(t 0, t 3, nt 1)),
            (2, prodf!(t 1, t 3, nt 1)),
        ], vec![
            //     | *  +  &  id $
            // ----+----------------
            // E   | .  .  .  0  .
            // E_1 | 2  3  4  .  1
            // E_2 | 5  6  .  .  .
              7,   7,   7,   0,   7,
              2,   3,   4,   7,   1,
              5,   6,   7,   7,   7,
        ]),
        (23, 0, 0, vec![
            // - 0: E -> F E_1
            // - 1: F -> id
            // - 2: F -> num
            // - 3: E_1 -> ε
            // - 4: E_1 -> * F E_1
            // - 5: E_1 -> + F E_1
            // - 6: E_1 -> & E_2
            // - 7: E_2 -> * F E_1
            // - 8: E_2 -> + F E_1
            (0, prodf!(nt 1, nt 2)),
            (1, prodf!(t 3)),
            (1, prodf!(t 4)),
            (2, prodf!(e)),
            (2, prodf!(t 0, nt 1, nt 2)),
            (2, prodf!(t 1, nt 1, nt 2)),
            (2, prodf!(t 2, nt 3)),
            (3, prodf!(t 0, nt 1, nt 2)),
            (3, prodf!(t 1, nt 1, nt 2)),
        ], vec![
            //     | *  +  &  id num $
            // ----+--------------------
            // E   | .  .  .  0   0  .
            // F   | .  .  .  1   2  .
            // E_1 | 4  5  6  .   .  3
            // E_2 | 7  8  .  .   .  .
              9,   9,   9,   0,   0,   9,
              9,   9,   9,   1,   2,   9,
              4,   5,   6,   9,   9,   3,
              7,   8,   9,   9,   9,   9,
        ]),

        (100, 0, 0, vec![
            // - 0: A -> c A_1
            // - 1: A_1 -> a A b A_1
            // - 2: A_1 -> ε
            (0, prodf!(t 2, nt 1)),
            (1, prodf!(t 0, nt 0, t 1, nt 1)),
            (1, prodf!(e)),
        ], vec![
            //     | a  b  c  $
            // ----+-------------
            // A   | .  .  0  .
            // A_1 | 1  2  .  2
              3,   3,   0,   3,
              1,   2,   3,   2,
        ]),
        (101, 0, 0, vec![
            // - 0: A -> a A A
            // - 1: A -> b
            (0, prodf!(t 0, nt 0, nt 0)),
            (0, prodf!(t 1)),
        ], vec![
            //   | a  b  $
            // --+----------
            // A | 0  1  .
              0,   1,   2,
        ]),
        (102, 0, 0, vec![
            // - 0: A -> c A_1
            // - 1: A_1 -> a A b c A_1
            // - 2: A_1 -> ε
            (0, prodf!(t 2, nt 1)),
            (1, prodf!(t 0, nt 0, t 1, t 2, nt 1)),
            (1, prodf!(e)),
        ], vec![
            //     | a  b  c  $
            // ----+-------------
            // A   | .  .  0  .
            // A_1 | 1  2  .  2
              3,   3,   0,   3,
              1,   2,   3,   2,
        ]),
        // (103, 0, 0, vec![
        // ], vec![
        // ]),
    ];
    const VERBOSE: bool = true;
    for (test_id, (ll_id, start, expected_warnings, expected_factors, expected_table)) in tests.into_iter().enumerate() {
if ll_id < 100 { continue; }
        let rules_lr = build_prs(ll_id);
        if VERBOSE {
            println!("test {test_id} with {ll_id}/{start}:");
            print_production_rules(&rules_lr);
        }
        let mut ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        ll1.set_start(start);
        let first = ll1.calc_first();
        let follow = ll1.calc_follow(&first);
        if VERBOSE {
            map_and_print_first(&first, ll1.get_symbol_table());
            map_and_print_follow(&follow, ll1.get_symbol_table());
        }
        let parsing_table = ll1.calc_table(&first, &follow);
        let LLParsingTable { num_nt, num_t, factors, table } = &parsing_table;
        assert_eq!(num_nt * num_t, table.len(), "incorrect table size in test {test_id}/{ll_id}/{start}");
        if VERBOSE {
            println!("num_nt = {num_nt}, num_t = {num_t}");
            let error = factors.len() as VarId;
            print_production_rules(&ll1);
            print_factors(&ll1, &factors);
            println!("{}",
                     factors.iter().enumerate().map(|(id, (v, f))|
                         format!("            ({v}, prodf!({})),", f.iter().map(|s| symbol_to_macro(s)).join(", "))
            ).join("\n"));
            println!("table:");
            print_ll1_table(ll1.get_symbol_table(), &parsing_table, 12);
            for i in 0..*num_nt {
                println!("            {},", (0..*num_t).map(|j| format!("{:3}", table[i * num_t + j])).join(", "));
            }
            if table.len() < 30 {
                println!("vec![{}]", table.iter().map(|x| x.to_string()).join(", "));
            }
            print_logs(&ll1);
        }
        assert_eq!(*factors, expected_factors, "test {test_id}/{ll_id}/{start} failed");
        assert_eq!(*table, expected_table, "test {test_id}/{ll_id}/{start} failed");
        assert_eq!(ll1.log.get_errors().join("\n"), "", "test {test_id}/{ll_id}/{start} failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings, "test {test_id}/{ll_id}/{start} failed, warnings: {}", ll1.log.get_warnings().join("\n"));
   }
}

#[test]
fn prs_grammar_notes() {
    let tests: Vec<(u32, VarId, &[&str], &[&str])> = vec![
        //        warnings                                  errors
        //        -------------------------------------     -------------------------------------
        (1000, 0, &[],                                      &["requires factors not starting with"]),
        (1001, 0, &[],                                      &["cannot remove recursion from"]),
        (1002, 0, &["ambiguity for NT"],                    &[]),
        (1003, 0, &[],                                      &["no terminal in grammar"]),
        (1004, 0, &[],                                      &["no terminal used in the table"]),
        (1005, 0, &["unused non-terminals",
                    "unused terminals"],                    &[]),
    ];
    const VERBOSE: bool = true;
    for (test_id, (ll_id, start, expected_warnings, expected_errors)) in tests.into_iter().enumerate() {
        let rules_lr = build_prs(ll_id);
        if VERBOSE {
            println!("test {test_id} with {ll_id}/{start}:");
        }
        if VERBOSE {
            print_production_rules(&rules_lr);
        }
        let mut ll1 = ProdRuleSet::<LL1>::from(rules_lr.clone());
        ll1.set_start(start);
        let mut parsing_table = None;
        let first = ll1.calc_first();
        if ll1.log.num_errors() == 0 {
            let follow = ll1.calc_follow(&first);
            if ll1.log.num_errors() == 0 {
                parsing_table = Some(ll1.calc_table(&first, &follow));
            }
        }
        if VERBOSE {
            println!("=>");
            print_production_rules(&ll1);
            if let Some(table) = &parsing_table {
                print_factors(&ll1, &table.factors);
                println!("table:");
                print_ll1_table(ll1.get_symbol_table(), &table, 12);
            }
            print_logs(&ll1);
        }
        assert_eq!(ll1.log.num_errors(), expected_errors.len(), "test {test_id}/{ll_id}/{start} failed on # errors");
        assert_eq!(ll1.log.num_warnings(), expected_warnings.len(), "test {test_id}/{ll_id}/{start} failed on # warnings");
        let err_discr = ll1.log.get_errors().zip(expected_errors).filter_map(|(e, ee)|
            if !e.contains(ee) { Some(format!("- \"{e}\" doesn't contain \"{ee}\"")) } else { None }
        ).to_vec();
        assert!(err_discr.is_empty(), "test {test_id}/{ll_id}/{start} has discrepancies in the expected error messages:\n{}", err_discr.join("\n"));
        let warn_discr = ll1.log.get_warnings().zip(expected_warnings).filter_map(|(w, ew)|
            if !w.contains(ew) { Some(format!("- \"{w}\" doesn't contain \"{ew}\"")) } else { None }
        ).to_vec();
        assert!(warn_discr.is_empty(), "test {test_id}/{ll_id}/{start} has discrepancies in the expected warning messages:\n{}", warn_discr.join("\n"));
   }
}
