// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexi_gram::lexigram_lib::CollectJoin;
use lexi_gram::options::Options;
use crate::arg_opt::{parse_args, ArgOptions};

#[test]
fn option_order() {
    static TESTS: &[(&str, usize, usize, usize, usize)] = &[
        ("--indent 1 -x a -l b",                                                1, 1, 1, 1),
        ("--indent 1 -x a -l b -g c -p d",                                      1, 1, 1, 1),
        ("--indent 1 --indent 2 -x a -l b -g c -p d --types e --listener f",    2, 2, 2, 2),
        ("--indent 1 -x a --indent 2 -l b -g c -p d --types e --listener f",    2, 1, 1, 1),
        ("--indent 1 -x a -l b --indent 2 -g c -p d --types e --listener f",    2, 1, 1, 1),
        ("--indent 1 -x a -l b -g c --indent 2 -p d --types e --listener f",    1, 2, 1, 1),
        ("--indent 1 -x a -l b -g c -p d --indent 2 --types e --listener f",    1, 2, 1, 1),
        ("--indent 1 -x a -l b -g c -p d --types e --indent 2 --listener f",    1, 1, 2, 1),
        ("--indent 1 -x a -l b -g c -p d --types e --listener f --indent 2",    1, 1, 1, 2),
        ("--indent 1 -c ac -l b -p d --types e --listener f",                   1, 1, 1, 1),
        ("--indent 1 --indent 2 -c ac -l b -p d --types e --listener f",        2, 2, 2, 2),
        ("--indent 1 -c ac --indent 2 -l b -p d --types e --listener f",        2, 1, 1, 1),
        ("--indent 1 -c ac -l b --indent 2 -p d --types e --listener f",        2, 1, 1, 1),
        ("--indent 1 -c ac -l b -p d --indent 2 --types e --listener f",        1, 2, 1, 1),
    ];
    const VERBOSE: bool = false;

    for (test_id, &(line, exp_lex, exp_par, exp_ty, exp_li)) in TESTS.iter().enumerate() {
        let args = line.split(" ").map(|s| s.to_string()).to_vec();
        let result = parse_args(args);
        let indents = match result {
            Ok((_, ArgOptions { gen_options: Options { lexer_indent, parser_indent, types_indent, listener_indent, .. }, ..})) =>
                Some((lexer_indent, parser_indent, types_indent, listener_indent)),
            Err(_) => None,
        };
        let text = if let Some((lex, par, ty, li)) = indents {
            format!("lexer: {lex}, parser: {par}, types: {ty}, listener: {li}")
        } else {
            format!("ERROR")
        };
        if VERBOSE {
            println!("{test_id}: {line}\n  {text}", )
        }
        assert_eq!(indents, Some((exp_lex, exp_par, exp_ty, exp_li)), "test {test_id}, {line:?} => {text}");
    }
}

#[test]
fn header_order() {
    static TESTS: &[(&str, &str, &str)] = &[
        ("--header 1 -x a -l b",                                                "1",  "1" ),
        ("--header 1 -x a -l b -g c -p d",                                      "1",  "1" ),
        ("--header 1 --header 2 -x a -l b -g c -p d --types e --listener f",    "12", "12"),
        ("--header 1 -x a --header 2 -l b -g c -p d --types e --listener f",    "12", "1" ),
        ("--header 1 -x a -l b --header 2 -g c -p d --types e --listener f",    "12", "1" ),
        ("--header 1 -x a -l b -g c --header 2 -p d --types e --listener f",    "1",  "12"),
        ("--header 1 -x a -l b -g c -p d --header 2 --types e --listener f",    "1",  "12"),
        ("--header 1 -x a -l b -g c -p d --types e --header 2 --listener f",    "1",  "1" ),
        ("--header 1 -x a -l b -g c -p d --types e --listener f --header 2",    "1",  "1" ),
        ("--header 1 -c ac -l b -p d --types e --listener f",                   "1",  "1" ),
        ("--header 1 --header 2 -c ac -l b -p d --types e --listener f",        "12", "12"),
        ("--header 1 -c ac --header 2 -l b -p d --types e --listener f",        "12", "1" ),
        ("--header 1 -c ac -l b --header 2 -p d --types e --listener f",        "12", "1" ),
        ("--header 1 -c ac -l b -p d --header 2 --types e --listener f",        "1",  "12"),
    ];
    const VERBOSE: bool = false;

    for (test_id, &(line, exp_lex, exp_par)) in TESTS.iter().enumerate() {
        let args = line.split(" ").map(|s| s.to_string()).to_vec();
        let result = parse_args(args);
        let indents = match result {
            Ok((_, ArgOptions { gen_options: Options { lexer_headers, parser_headers, .. }, ..})) =>
                Some((lexer_headers.join(""), parser_headers.join(""))),
            Err(_) => None,
        };
        let text = if let Some((lex, par)) = &indents {
            format!("lexer: {lex:?}, parser: {par:?}")
        } else {
            format!("ERROR")
        };
        if VERBOSE {
            println!("- {test_id:2}: {line:<70}  {text}", )
        }
        let (exp_lex, exp_par) = (exp_lex.to_string(), exp_par.to_string());
        assert_eq!(indents, Some((exp_lex, exp_par)), "test {test_id}, {line:?} => {text}");
    }
}
