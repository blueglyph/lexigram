// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

//! Rulesets manually built from hand-made [GrTree]s or [ProdRule](crate::grammar::ProdRule)s.
//!
//! This serves as basic tests in case recent tests, built [from plain text grammars](crate::grammar::tests::TestRules::to_rts_general)
//! and parsed by [RtsGen](crate::rtsgen::RtsGen), don't work any more, since they require a lot of backend code to work correctly.
//!
//! There aren't any original references in rules built by [build_prs], so those cannot be used everywhere;
//! for example, it would be a problem for code in the [parsergen](crate::parsergen) module.

use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use iter_index::IndexerIterator;
use lexigram_core::{CollectJoin, TokenId};
use lexigram_core::lexer::{CaretCol, Pos, PosSpan};
use crate::{btreemap, make_stream, prule, General, SymbolTable, LL1};
use crate::grammar::{gnode, GrNode, GrTree, ProdRuleSet, RuleTreeSet, SepInfo, Symbol, VarId};
use crate::grammar::tests::prs;
use lexigram_core::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_core::parser::ListenerWrapper;
use crate::build::BuildFrom;
use crate::parsergen::{ParserGen, LLParserTables};

pub(crate) fn build_rts(id: u32) -> RuleTreeSet<General> {
    let mut rules = RuleTreeSet::new();
    let tree = rules.get_tree_mut(0);
    let extend_nt = true;

    match id {
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
        23 => { // A -> a (b)+ c
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(+), gnode!(t 1));
            tree.add(Some(cc), gnode!(t 2));
            // symbol table defined below
        }
        27 => { // A -> a (B)+ c ; B -> b
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            tree.addc(Some(cc), gnode!(+), gnode!(nt 1));
            tree.add(Some(cc), gnode!(t 2));
            let b_tree = rules.get_tree_mut(1);
            b_tree.add_root(gnode!(t 1));
        }
        28 => { // A -> a (b c)* d;
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let plus = tree.add(Some(cc), gnode!(*));
            tree.addc_iter(Some(plus), gnode!(&), [gnode!(t 1), gnode!(t 2)]);
            tree.add(Some(cc), gnode!(t 3));

        }
        30 => { // A -> a (b e / c)+ d;
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let plus = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(plus), gnode!(&), [gnode!(t 1), gnode!(t 4), gnode!(/), gnode!(t 2)]);
            tree.add(Some(cc), gnode!(t 3));

        }
        31 => { // A -> (a (b e / c)+)+ d;
            let cc = tree.add_root(gnode!(&));
            let plus = tree.add(Some(cc), gnode!(+));
            let cc1 = tree.add(Some(plus), gnode!(&));
            tree.add(Some(cc1), gnode!(t 0));
            let plus1 = tree.add(Some(cc1), gnode!(+));
            tree.addc_iter(Some(plus1), gnode!(&), [gnode!(t 1), gnode!(t 4), gnode!(/), gnode!(t 2)]);
            tree.add(Some(cc), gnode!(t 3));

        }
        100 => { // A -> a ()*
            let cc = tree.add_root(gnode!(&));
            // tree.add_iter(Some(cc), [gnode!(t 0), gnode!(*)]);
            tree.add(Some(cc), gnode!(t 0));
            tree.addc_iter(Some(cc), gnode!(*), []);
        }
        130 => { // A -> a (b / c / e)+ d;
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let plus = tree.add(Some(cc), gnode!(+));
            tree.addc_iter(Some(plus), gnode!(&), [gnode!(t 1), gnode!(/), gnode!(t 2), gnode!(/), gnode!(t 4)]);
            tree.add(Some(cc), gnode!(t 3));

        }
        131 => { // A -> a (b / c | e)+ d;
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let plus = tree.add(Some(cc), gnode!(+));
            let or1 = tree.add(Some(plus), gnode!(|));
            tree.addc_iter(Some(or1), gnode!(&), [gnode!(t 1), gnode!(/), gnode!(t 2)]);
            tree.add(Some(or1), gnode!(t 4));
            tree.add(Some(cc), gnode!(t 3));
        }
        132 => { // A -> a / b;
            let cc = tree.add_root(gnode!(&));
            tree.addc_iter(Some(cc), gnode!(&), [gnode!(t 0), gnode!(/), gnode!(t 1)]);
        }
        133 => { // A -> (a / b)*;
            let star = tree.add_root(gnode!(*));
            tree.addc_iter(Some(star), gnode!(&), [gnode!(t 0), gnode!(/), gnode!(t 1)]);
        }
        134 => { // A -> a / b | c;
            let or = tree.add_root(gnode!(|));
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 0), gnode!(/), gnode!(t 1)]);
            tree.addc_iter(Some(or), gnode!(&), [gnode!(t 2)]);
        }
        135 => { // A -> a (b / c | e / f)+ d;
            let cc = tree.add_root(gnode!(&));
            tree.add(Some(cc), gnode!(t 0));
            let plus = tree.add(Some(cc), gnode!(+));
            let or1 = tree.add(Some(plus), gnode!(|));
            tree.addc_iter(Some(or1), gnode!(&), [gnode!(t 1), gnode!(/), gnode!(t 2)]);
            tree.addc_iter(Some(or1), gnode!(&), [gnode!(t 4), gnode!(/), gnode!(t 5)]);
            tree.add(Some(cc), gnode!(t 3));
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
            for n in t.iter_post_depth_simple() {
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
        if matches!(id, 23 | 27) {
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
    rules.sep_info = SepInfo::NtAlt(vec![]);
    let start = Some(0);
    let flags = HashMap::<VarId, u32>::new();
    let parents = HashMap::<VarId, VarId>::new();   // (child, parent)
    let extend_nt = true;
    match id {
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
        8 => {
            // ambiguous
            prules.extend([
                prule!(nt 0, t 0, nt 0; t 1),    // A -> A a A | b
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
        33 => {
            // A -> A a | b c | b d
            prules.extend([
                prule!(nt 0, t 0; t 1, t 2; t 1, t 3),
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
        100 => {
            // A -> A a A b | c (amb removed)
            prules.extend([
                prule!(nt 0, t 0, nt 0, t 1; t 2),
            ]);
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

// ---------------------------------------------------------------------------------------------

#[test]
fn parser_parse_stream() {

    struct Stub(BufLog);
    impl ListenerWrapper for Stub {
        fn get_log_mut(&mut self) -> &mut impl Logger {
            &mut self.0
        }
    }

    let tests = vec![
        (4, 0, vec![
            ("I*I", true),
            ("(N)", true),
            ("((N))", true),
        ]),
        (5, 0, vec![
            ("++;;", true),
            ("--+;;", true),
            ("+-;;", false),
            ("++;;-", false),
            ("++;-", false),
            ("-", false),
        ]),
        (8, 0, vec![ // ambiguous grammar but that should work
            ("b a b a b", true),
            ("b", true),
        ]),
        (16, 0, vec![ // A -> B A | b ; B -> a
            ("aaab", true),
        ]),
        (17, 0, vec![
            ("(((a)))", true),
            ("((a)", false),
            ("((a)))", false),
        ]),
        (18, 0, vec![
            ("a", true),
            ("", false),
            ("aa", false),
        ]),
        (19, 0, vec![
            ("a", true),
            ("", true),
            ("aa", false),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, sequences)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id}/{start}", ""); }
        let mut ll1 = ProdRuleSet::<LL1>::build_from(build_prs(ll_id, false));
        ll1.set_start(start);
        let symbols = (0..ll1.get_num_t() as TokenId)
            .map(|t| (Symbol::T(t).to_str(ll1.get_symbol_table()), t))
            .collect::<HashMap<_, _>>();
        ll1.set_name("Test".to_string());
        let parser_tables = LLParserTables::build_from(ParserGen::build_from_rules_ll1(ll1));
        let mut parser = parser_tables.make_parser();
        for (input, expected_success) in sequences {
            if VERBOSE { println!("{:-<60}\ninput '{input}'", ""); }
            let stream = input.chars().into_iter().index_start::<CaretCol>(1).filter_map(|(i, c)| {
                if c.is_ascii_whitespace() {
                    None
                } else {
                    let c_str = c.to_string();
                    if let Some(s) = symbols.get(&c_str) {
                        // println!("stream: '{}' -> sym!({})", c, symbol_to_macro(s));
                        let pos = Pos(1, i);
                        Some((*s, c_str, PosSpan::new(pos, pos)))
                    } else {
                        panic!("unrecognized test input '{c}' in test {test_id}/{ll_id}/{start}, input {input}");
                    }
                }
            });
            let mut listener = Stub(BufLog::new());
            let success = match parser.parse_stream(&mut listener, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully"); }
                    true
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    false
                }
            };
            if VERBOSE {
                let msg = listener.0.get_messages().map(|s| format!("- {s:?}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            assert_eq!(success, expected_success, "test {test_id}/{ll_id}/{start} failed for input {input}");
        }
    }
}

#[test]
fn parser_parse_stream_id() {

    struct Stub(BufLog);
    impl ListenerWrapper for Stub {
        fn get_log_mut(&mut self) -> &mut impl Logger {
            &mut self.0
        }
    }

    let tests = vec![
        (T::RTS(9), 0, 2, 999, vec![
            ("var a , b ,", None),
        ]),
        (T::RTS(23), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", None),
        ]),
        (T::RTS(27), 0, 999, 999, vec![
            // A -> a (b)+ c
            ("a b b c", None),
        ]),
        (T::PRS(20), 0, 5, 999, vec![
            ("struct test1 { a : int ; b : string ; c : bool ; }", None),
            ("struct test2 { a : int ; b : string ; c : bool }", Some(vec![
                "syntax error: found input '}' instead of ';', line 1, col 15"
            ])),
        ]),
        (T::PRS(33), 0, 999, 999, vec![
            // A -> A a | b c | b d
            ("b c a a", None),
            ("b d a a", None),
            ("b c", None),
            ("b d", None),
        ]),
        (T::PRS(43), 0, 7, 6, vec![
            // BATCH -> GROUP ';' BATCH <L> | ε
            // GROUP -> '[' EXPR ']' | '(' EXPR ')'
            // EXPR -> FACTOR '*' FACTOR;
            // FACTOR -> id | int | '(' EXPR ')';
            ("[ 1 * 2 ] ;", None),
            ("[ ( 1 * 2 * 3 ] ;", Some(vec![
                "syntax error: found input '*' instead of ')', line 1, col 6"
            ])),
            ("[ 1 * 2 ; [ ( 3 * 4 ) * ] ; [ 5 * 6 ] ;", Some(vec![
                "syntax error: found input ';' instead of ']', line 1, col 5",
                "syntax error: found input ']' instead of '(', 'id', 'int' while parsing '►FACTOR', line 1, col 13"
            ])),
        ]),
        (T::PRS(51), 0, 8, 7, vec![
            // E -> 'abs' E | E '^' E | E '*' E | '-' E | E '+' E | F;
            // F -> ( E ) | NUM | ID
            ("1 ^ 2", None),
            ("1 + 2 * 3 + 4 ^ 5 * 6 + 7 ^ 8", None),
            ("2 ' ^ 3", None),
            ("- 4 * 3", None),
            ("3 * - 4", None),
            ("( 1 + 2 ) * ( 3 + - abs i * - 5 + 6 ) ^ 2", None)
        ]),
        (T::PRS(61), 0, 99, 99, vec![
            ("- 0 +", None),
            ("- - 1 + +", None),
            ("- 0", None),
            ("1 +", None),
            ("0", None),
            ("1", None),
        ]),
        (T::PRS(63), 0, 4, 99, vec![
            ("a * b", None),
            ("a + b", None),
            ("- a", None),
            ("a * b + c", None),
            ("a + b * c", None),
            ("- a * b", None),
            ("a * - b", None),
        ]),
        (T::PRS(100), 0, 999, 999, vec![
            ("c a c a c b b", None),
            ("c a c b a c b", None),
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, (ll_id, start, id_id, num_id, sequences)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {ll_id:?}/{start}", ""); }
        let mut ll1 = ll_id.build_prs(test_id, start, false);
        ll1.set_name("Test".to_string());
        let parser_tables = LLParserTables::build_from(ParserGen::build_from_rules_ll1(ll1));
        let mut parser = parser_tables.make_parser();
        for (input, expected_errors) in sequences {
            if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
            let stream = make_stream(input, parser_tables.get_symbol_table().get_terminals(), true, id_id, num_id, VERBOSE);
            let mut listener = Stub(BufLog::new());
            let errors = match parser.parse_stream(&mut listener, stream) {
                Ok(_) => {
                    if VERBOSE { println!("parsing completed successfully"); }
                    None
                }
                Err(e) => {
                    if VERBOSE { println!("parsing failed: {e}"); }
                    Some(listener.0.get_errors().map(|s| s.get_inner_str()).to_vec())
                }
            };
            if VERBOSE {
                let msg = listener.0.get_messages().map(|s| format!("- {s}")).join("\n");
                if !msg.is_empty() {
                    println!("Messages:\n{msg}");
                }
            }
            assert_eq!(errors, expected_errors, "test {test_id}/{ll_id:?}/{start} failed for input {input}");
        }
    }
}

#[test]
fn test_empty_repeat() {
    let prs = T::RTS(100).try_build_prs(0, true);
    let error = prs.log.get_errors().find(|s| s.to_string().contains("* must have one child; found none"));
    assert!(error.is_some(), "didn't find the expected error:\n{}", prs.log);
}

#[test]
fn test_sep() {
    const VERBOSE: bool = false;
    const VERBOSE_SOLUTION: bool = false;
    static TESTS: &[(u32, &[&str], &[&str])] = &[
        (28, &[
            "A -> a A_1 d",
            "A_1 -> b c A_1",
            "A_1 -> ε",
        ], &[
        ]),
        // A -> a (b e / c)+ d;
        (30, &[
            "A -> a b e A_1 d",
            "A_1 -> c b e A_1",
            "A_1 -> ε",
        ], &[
        ]),
        // A -> (a (b e / c)+)+ d
        //
        // A -> A_2 d
        // A_1 -> b e / c A_1 | b e / c
        // A_2 -> a A_1 A_2 | a A_1
        (31, &[
            "A -> A_2 d",
            "A_1 -> c b e A_1",
            "A_1 -> ε",
            "A_2 -> a b e A_1 A_3",
            "A_3 -> A_2",
            "A_3 -> ε",
        ], &[
        ]),
        (130, &[
        ], &[
            "only one separator / is allowed inside + repetitions: A -> a ( ►► b / c / e ◄◄ )+ d",
        ]),
        (131, &[
        ], &[
            "separators / can't be mixed with OR productions inside + repetitions: A -> a (b / c |  ►► e ◄◄ )+ d",
        ]),
        (132, &[
        ], &[
            "separators / are supported only inside + repetitions: A -> a / b",
        ]),
        (133, &[
        ], &[
            "separators / are allowed only inside + repetitions: A -> ( ►► a / b ◄◄ )*",
        ]),
        (134, &[
        ], &[
            "separators / are allowed only inside + repetitions: A ->  ►► a / b ◄◄  | c",
        ]),
        (135, &[
        ], &[
            "separators / can't be mixed with OR productions inside + repetitions: A -> a (b / c |  ►► e / f ◄◄ )+ d",
        ]),
    ];
    for &(rts_id, expected_alts, expected_errors) in TESTS {
        if VERBOSE && !VERBOSE_SOLUTION { println!("{:=<80}\ntest {rts_id}", ""); }
        let prs = T::RTS(rts_id).try_build_prs(0, true);
        let alts = if prs.has_no_errors() {
            prs.get_alts().map(|(v, a)| format!("{}", a.to_rule_str(v, prs.get_symbol_table(), 0))).to_vec()
        } else {
            vec![]
        };
        let expected_errors = expected_errors.into_iter().cloned().collect::<HashSet<&str>>();
        let errors = prs.log.get_errors().map(|e| e.get_inner_str()).collect::<HashSet<&str>>();
        if VERBOSE_SOLUTION {
            print!("        ({rts_id}, &[{}\n        ], &[", alts.iter().map(|s| format!("\n            {s:?},")).join(""));
            println!("{}\n        ]),", errors.iter().map(|s| format!("\n            {s:?},")).join(""));
        }
        if VERBOSE && !VERBOSE_SOLUTION {
            println!("{}", prs.log);
        }
        if errors.is_empty() {
            assert_eq!(alts, expected_alts, "test {rts_id} failed: alt mismatch");
        }
        assert_eq!(errors, expected_errors, "test {rts_id} failed: error mismatch");
    }
}