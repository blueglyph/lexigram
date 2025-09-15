// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use lexigram_lib::grammar::{grtree_to_str, GrTreeExt, Symbol};
use lexigram_lib::log::LogReader;
use lexigram_lib::CollectJoin;
use crate::RtsGen;

#[test]
fn simple() {
    let mut parser = RtsGen::new();
    let tests = vec![
        (
            vec![
                r#"a => |(&(A B b) C D &(E F) G H &(I J)); // RTS form"#,
                r#"b => |(+(&("-" "=")) ?(|(C D)) E);"#],
            vec![
                r#"a => |(&(A, B, b), C, D, &(E, F), G, H, &(I, J))"#,
                r#"b => |(+(&(-, =)), ?(|(C, D)), E)"#]
        ),
        (
            vec![
                r#"a -> A B b | C | D | E F | G | H | I J; // PRS form"#,
                r#"b -> (A B)+ | (C | D)? | E;"#],
            vec![
                r#"a => |(&(A, B, b), C, D, &(E, F), G, H, &(I, J))"#,
                r#"b => |(+(&(A, B)), ?(|(C, D)), E)"#]
        ),
        (
            vec![
                r#"a => |(&(A B b) C D &(E F) G H &(I J)); // RTS form"#,
                r#"b -> (A B)+ | (C | D)? | E;             // PRS form"#],
            vec![
                r#"a => |(&(A, B, b), C, D, &(E, F), G, H, &(I, J))"#,
                r#"b => |(+(&(A, B)), ?(|(C, D)), E)"#]
        ),
    ];
    for (text_vec, expected) in tests {
        let text = text_vec.join("\n");
        println!("{:=<80}\n{text}\n{0:=<80}", "", );
        match parser.parse(text) {
            Ok(rts) => {
                println!("Rules:");
                for (v, tree) in rts.get_trees_iter() {
                    println!("- NT[{v:2}] {} -> {}", Symbol::NT(v).to_str(rts.get_symbol_table()), grtree_to_str(tree, None, None, rts.get_symbol_table(), false));
                }
                println!("Symbol table:");
                let symtab = rts.get_symbol_table().unwrap();
                println!("- nonterminals:\n{}", symtab.get_nonterminals().enumerate().map(|(v, s)| format!("  - NT[{v}]: {s}")).join("\n"));
                println!("- terminals:\n{}",
                         symtab.get_terminals().enumerate()
                             .map(|(t, (n, v_maybe))| format!("  - T[{t}]: {n}{}", if let Some(v) = v_maybe { format!(" = {v}") } else { String::new() })).join("\n"));
                println!("Log:\n{}", rts.get_log());
                let result_rts = rts.get_trees_iter()
                    .map(|(v, tree)| format!("{} => {}", Symbol::NT(v).to_str(rts.get_symbol_table()), tree.to_str(None, rts.get_symbol_table())))
                    .to_vec();
                let expected_rts = expected.into_iter().map(|s| s.to_string()).to_vec();
                println!("result code:");
                for s in &result_rts {
                    println!("                r#\"{s}\"#,");
                }
                assert_eq!(result_rts, expected_rts, "test failed");
            }
            Err(log) => println!("errors during parsing:\n{log}"),
        }
    }
}

