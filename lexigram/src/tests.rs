// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use crate::gen_parser::{try_gen_parser, Action, CodeLocation, Options, Specification};

#[test]
fn from_file_to_file() {
    const VERBOSE: bool = true;
    let lexer = Specification::File { filename: "data/test1.l".to_string() };
    let parser = Specification::File { filename: "data/test1.g".to_string() };
    let options = Options {
        lexer_code: CodeLocation::File { filename: "tests/out/test1_lexer.rs".to_string() },
        lexer_indent: 0,
        parser_code: CodeLocation::File { filename: "tests/out/test1_parser.rs".to_string() },
        parser_indent: 0,
        extra_libs: vec!["super::listener_types::test1::*".to_string()],
        gen_parser_alts: true,
        gen_wrapper: true,
        gen_span_params: true,
    };
    match try_gen_parser(lexer, parser, Action::Generate, options) {
        Ok(log) => {
            if VERBOSE { println!("Parser generated successfully\n{log}"); }
        }
        Err(err) => panic!("Error: {err}"),
    }
}