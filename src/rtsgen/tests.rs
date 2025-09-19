// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(all(test, feature = "rtsgen"))]

// =============================================================================================
// Simple parser based on microcalc lexicon and grammar

use crate::grammar::{grtree_to_str, GrTreeExt, RuleTreeSet, Symbol};
use crate::log::{LogReader, LogStatus};
use crate::{CollectJoin, General};
use crate::rtsgen::RtsGen;

#[test]
fn simple() {
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
                r#"line_iter => <empty>;"#]
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
                r#"a_iter => <empty>;"#,
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
                r#"x1 => <empty>;"#,
                r#"x2 => <empty>;"#,
                r#"y => "y";"#,
                r#"z => "z";"#]
        ),
        /* template:
        (
            vec![r#""#],
            vec![r#""#]
        ),
        */
    ];
    const VERBOSE: bool = false;
    const VERBOSE_ANSWER: bool = false;
    let mut parser = RtsGen::new();
    let mut errors = 0;
    for (test_id, (text_vec, expected)) in tests.into_iter().enumerate() {
        let text = text_vec.join("\n");
        if VERBOSE { println!("\n{:=<80}\n{text}\n{0:-<80}\ntest {test_id}:", "", ); }
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
                let result_rts = (0..rts.get_num_nt())
                    .map(|v| (v, rts.get_tree(v).unwrap()))
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

#[test]
fn catch_errors() {
    let tests: Vec<(Vec<&str>, Vec<&str>)> = vec![
        (   // 0: undefined nonterminals
            vec![r#"x -> (<L=x1> y | (<L=x2> "+" z)+ )*;"#],
            vec!["undefined nonterminals: 'y', 'z'"],
        ),
        (   // 1: string literals
            vec![r#"esc -> "\w";"#],
            vec!["lexical error: invalid character 'w'"],
        ),
        (   // 2: string literals
            vec![r#"esc -> "\u{123456789abcdef}";"#],
            vec!["'123456789abcdef' isn't a valid hexadecimal value"],
        ),
        (   // 4: nonterminal already defined
            vec![r#"a -> "a"; a -> "b";"#],
            vec!["nonterminal 'a' is defined multiple times"],
        ),
        /* template:
        (   //
            vec![],
            vec![],
        ),
        */
    ];
    const VERBOSE: bool = false;
    const VERBOSE_ANSWER: bool = false;
    const TEST_UNEXPECTED: bool = false; // errors tend to generate others in cascade
    let mut parser = RtsGen::new();
    let mut errors = 0;
    for (test_id, (text_vec, mut expected_errors)) in tests.into_iter().enumerate() {
        let text = text_vec.join("\n");
        if VERBOSE { println!("\n{:=<80}\n{text}\n{0:-<80}\ntest {test_id}:", "", ); }
        let msg = format!("## ERROR ## test {test_id} failed");
        let log = match parser.parse(text) {
            Ok(rts) => rts.give_log(),
            Err(log) => log,
        };
        if VERBOSE { println!("Log:\n{log}"); }
        let mut result_errors = log.get_errors().map(|s| s.as_str()).to_vec();
        if VERBOSE_ANSWER {
            println!("Result:");
            println!("            vec![\n{}", result_errors.iter().map(|s| format!("                {s:?},")).join("\n"));
            println!("            ],");
        }
        if !TEST_UNEXPECTED && expected_errors.is_empty() {
            println!("{msg}: there is no expected errors in this test");
            errors += 1;
        }
        for i_res in 0..result_errors.len() {
            for i_exp in 0..expected_errors.len() {
                if result_errors[i_res].contains(&expected_errors[i_exp]) {
                    result_errors.remove(i_res);
                    expected_errors.remove(i_exp);
                    break;
                }
            }
        }
        if !expected_errors.is_empty() || TEST_UNEXPECTED && !result_errors.is_empty() {
            errors += 1;
            println!("{msg}:");
            if !expected_errors.is_empty() {
                println!("- missing errors:\n{}", expected_errors.into_iter().map(|s| format!("  - {s}")).join("\n"));
            }
            if TEST_UNEXPECTED && !result_errors.is_empty() {
                println!("- unexpected errors:\n{}", result_errors.into_iter().map(|s| format!("  - {s}")).join("\n"));
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

#[test]
fn t_names() {
    let tests = vec![
        (
            vec![r#"a -> "a" | "b" "+" "a" | "all_your_base" | "+++" | "ε";"#],
            vec![
                ("A", Some("a")),
                ("B", Some("b")),
                ("Add", Some("+")),
                ("AllYourBase", Some("all_your_base")),
                ("Token4", Some("+++")),
                ("Token5", Some("ε")),
            ],
        ),
        (
            vec![r#"a -> "1" | "123x" | "_hidden" | X | "in words" | " " | "\n";"#],
            vec![
                ("Tok1", Some("1")),
                ("Tok123x", Some("123x")),
                ("TokHidden", Some("_hidden")),
                ("X", None),
                ("InWords", Some("in words")),
                ("Space", Some(" ")),
                ("EOL", Some("\n")),
            ],
        )
    ];
    const VERBOSE: bool = false;
    const VERBOSE_ANSWER: bool = false;
    let mut parser = RtsGen::new();
    let mut errors = 0;
    for (test_id, (text_vec, expected_t)) in tests.into_iter().enumerate() {
        let text = text_vec.join("\n");
        if VERBOSE { println!("\n{:=<80}\n{text}\n{0:-<80}\ntest {test_id}:", "", ); }
        let msg = format!("## ERROR ## test {test_id} failed");
        let rts = parser.parse(text)
            .unwrap_or_else(|log| RuleTreeSet::<General>::with_log(log));
        if rts.get_log().has_no_errors() {
            if VERBOSE { println!("Log:\n{}", rts.get_log()); }
            // let expected_t = expected_t.into_iter().map(|(n, v_maybe)| (n.to_string(), v_maybe.map(|v| v.to_string()))).to_vec();
            let result_t = rts.get_symbol_table().unwrap()
                .get_terminals()
                .map(|(n, v_maybe)| (n.as_str(), v_maybe.as_ref().map(|s| s.as_str())))
                .to_vec();
            if result_t != expected_t {
                errors += 1;
                println!("{msg}: mismatch\n- result  : {result_t:?}\n- expected: {expected_t:?}");
            }
            if VERBOSE_ANSWER {
                println!("Result:");
                println!("            vec![\n{}", result_t.iter().map(|s| format!("                {s:?},")).join("\n"));
                println!("            ],");
            }
        } else {
            errors += 1;
            println!("{msg}: parsing errors\n{}", rts.get_log());
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}