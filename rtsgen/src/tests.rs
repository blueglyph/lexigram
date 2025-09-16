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
        (   // 0: simple RTS with & + ? * |
            vec![
                r#"a => |(&(A B b) C D &(E F) G H &(I J)); // RTS form"#,
                r#"b => |(+(&("-" "=")) *(".") ?(|(C D)) E);"#],
            vec![
                r#"a => |(&(A, B, b), C, D, &(E, F), G, H, &(I, J));"#,
                r#"b => |(+(&("-", "=")), *("."), ?(|(C, D)), E);"#]
        ),
        (   // 1: simple PRS with & + ? * |
            vec![
                r#"a -> A B b | C | D | E F | G | H | I J; // PRS form"#,
                r#"b -> (A B)+ | (C | D)? | (E F)* | G;"#],
            vec![
                r#"a => |(&(A, B, b), C, D, &(E, F), G, H, &(I, J));"#,
                r#"b => |(+(&(A, B)), ?(|(C, D)), *(&(E, F)), G);"#]
        ),
        (   // 2: mix RTS/PRS
            vec![
                r#"a => |(&(A B b) C D &(E F) G H &(I J)); // RTS form"#,
                r#"b -> (A B)* | (C | D)? | E;             // PRS form"#],
            vec![
                r#"a => |(&(A, B, b), C, D, &(E, F), G, H, &(I, J));"#,
                r#"b => |(*(&(A, B)), ?(|(C, D)), E);"#]
        ),
        (   // 3: empty
            vec![r#"a -> "." | ε | € | {};"#],
            vec![r#"a => |(".", ε, ε, ε);"#]
        ),
        (   // 4: <R> <P>
            vec![r#"a -> <R> a "^" a | a "*" a | <P> a "/" a | Id;"#],
            vec![r#"a => |(&(<R>, a, "^", a), &(a, "*", a), &(<P>, a, "/", a), Id);"#]
        ),
        (   // 5: <L> in rrec
            vec![r#"a -> <L> "-" a | "+";"#],
            vec![r#"a => |(&(<L=a>, "-", a), "+");"#]
        ),
        (   // 6: <L=nt> in +
            vec![
                r#"line -> (<L=line_iter> "-")+;"#],
            vec![
                r#"line => +(&(<L=line_iter>, "-"));"#,
                r#"line_iter => ε;"#]
        ),
        (   // 7: string literals
            vec![
                r#"char -> esc | unicode;"#,
                r#"esc -> "\n\r\t" | "\"\\";"#,
                r#"unicode -> "\u{03b5}";"#],
            vec![
                r#"char => |(esc, unicode);"#,
                r#"esc => |("\n\r\t", "\"\\");"#,
                r#"unicode => "ε";"#]
        ),
        (   // 8: reordering
            vec![
                r#"a -> "a" d (<L=a_iter> c)+;"#,
                r#"b -> "b";"#,
                r#"c -> "c" b;"#,
                r#"d -> "d" b;"#],
            vec![
                r#"a => &("a", d, +(&(<L=a_iter>, c)));"#,
                r#"a_iter => ε;"#,
                r#"b => "b";"#,
                r#"c => &("c", b);"#,
                r#"d => &("d", b);"#]
        ),
        (   // 9: embedded + * with <L=nt>
            vec![
                r#"x -> (<L=x1> y | (<L=x2> "+" z)+ )*;"#,
                r#"y -> "y";"#,
                r#"z -> "z";"#],
            vec![
                r#"x => *(|(&(<L=x1>, y), +(&(<L=x2>, "+", z))));"#,
                r#"x1 => ε;"#,
                r#"x2 => ε;"#,
                r#"y => "y";"#,
                r#"z => "z";"#]
        ),
        /*
        (
            vec![r#""#],
            vec![r#""#]
        ),
        */
    ];
    const VERBOSE: bool = false;
    const VERBOSE_ANSWER: bool = false;
    let mut errors = 0;
    for (test_id, (text_vec, expected)) in tests.into_iter().enumerate() {
        let text = text_vec.join("\n");
        if VERBOSE { println!("\n{:=<80}\n{text}\n{0:=<80}\ntest {test_id}:", "", ); }
        let msg = format!("## ERROR ## test {test_id} failed");
        match parser.parse(text) {
            Ok(rts) => {
                let symtab = rts.get_symbol_table();
                if VERBOSE {
                    println!("Rules:");
                    for (v, tree) in rts.get_trees_iter() {
                        println!("- NT[{v:2}] {} -> {}", Symbol::NT(v).to_str(symtab), grtree_to_str(tree, None, None, symtab, false));
                    }
                    println!("Symbol table:");
                    println!("- nonterminals:\n{}", symtab.unwrap().get_nonterminals().enumerate().map(|(v, s)| format!("  - NT[{v}]: {s}")).join("\n"));
                    println!("- terminals:\n{}",
                             symtab.unwrap().get_terminals().enumerate()
                                 .map(|(t, (n, v_maybe))| format!("  - T[{t}]: {n}{}", if let Some(v) = v_maybe { format!(" = {v:?}") } else { String::new() })).join("\n"));
                    println!("Log:\n{}", rts.get_log());
                }
                let result_rts = rts.get_trees_iter()
                    .map(|(v, tree)| format!("{} => {};", Symbol::NT(v).to_str(rts.get_symbol_table()), tree.to_str(None, rts.get_symbol_table())))
                    .to_vec();
                let expected_rts = expected.into_iter().map(|s| s.to_string()).to_vec();
                if VERBOSE | VERBOSE_ANSWER {
                    println!("result code:");
                    for s in &result_rts {
                        println!("                r#\"{s}\"#,");
                    }
                }
                if result_rts != expected_rts {
                    errors += 1;
                    println!("{msg}: result mismatch");
                }
            }
            Err(log) => {
                println!("errors during parsing:\n{log}");
                if !expected.is_empty() {
                    errors += 1;
                    println!("{msg}: was expecting results");
                }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

