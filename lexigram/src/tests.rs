// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexigram_lib::file_utils::{get_tagged_source, replace_tagged_source};
use lexigram_lib::log::BufLog;
use crate::gen_parser::{try_gen_parser, Action, CodeLocation, GenParserError, Options, Specification};

const TEST1_LEXICON_FILENAME: &str = "data/test1.l";
const TEST1_GRAMMAR_FILENAME: &str = "data/test1.g";
const TEST1_LEXER_FILENAME: &str = "tests/out/test1_lexer.rs";
const TEST1_PARSER_FILENAME: &str = "tests/out/test1_parser.rs";
const TEST1_TAGS_FILENAME: &str = "tests/out/test1_tags.rs";

const TEST1_LEXICON_TAG: &str = "test1_lexicon_tag";
const TEST1_GRAMMAR_TAG: &str = "test1_grammar_tag";
const TEST1_LEXER_TAG: &str = "test1_lexer_tag";
const TEST1_PARSER_TAG: &str = "test1_parser_tag";

/// (Re)generates the initial content
#[ignore]
#[test]
fn gen_init_content() {
    gen_init_file_to_file();
    gen_init_string_to_tag();
    gen_init_tag_to_tag();
}

/// Launches the try_gen_parser tests.
/// We launch them in serie and not in parallel because some of them modify the same files.
#[test]
fn test_all() {
    test_file_to_file();
    test_string_to_tag();
    test_tag_to_tag();
}

fn action_file_to_file(action: Action) -> Result<BufLog, GenParserError> {
    let lexer_spec = Specification::File { filename: TEST1_LEXICON_FILENAME.to_string() };
    let parser_spec = Specification::File { filename: TEST1_GRAMMAR_FILENAME.to_string() };
    let options = Options {
        lexer_code: CodeLocation::File { filename: TEST1_LEXER_FILENAME.to_string() },
        lexer_indent: 0,
        parser_code: CodeLocation::File { filename: TEST1_PARSER_FILENAME.to_string() },
        parser_indent: 0,
        lexer_headers: vec!["#![allow(unused)]".to_string()],
        parser_headers: vec!["#![allow(unused)]".to_string()],
        extra_libs: vec!["super::listener_types::test1::*".to_string()],
        gen_parser_alts: true,
        gen_wrapper: true,
        gen_span_params: true,
    };
    try_gen_parser(lexer_spec, parser_spec, action, options)
}

/// Generates the lexer/parser files in [TEST1_LEXER_FILENAME] and [TEST1_PARSER_FILENAME] the first time when
/// those files don't exist yet (visual check required).
fn gen_init_file_to_file() {
    match action_file_to_file(Action::Generate) {
        Ok(log) => println!("{log}"),
        Err(e) => {
            panic!("first code generation: {e}")
        }
    }
}

/// Verifies that the destination files are created again and identical to their previous version
/// (which was the reference).
#[ignore]
#[test]
fn test_file_to_file() {
    const VERBOSE: bool = false;

    // checks that the reference files exist
    let expected_lexer = std::fs::read_to_string(TEST1_LEXER_FILENAME)
        .expect(&format!("lexer code should already exist in {TEST1_LEXER_FILENAME}"));
    let expected_parser = std::fs::read_to_string(TEST1_PARSER_FILENAME)
        .expect(&format!("parser code should already exist in {TEST1_PARSER_FILENAME}"));

    // deletes the reference files
    std::fs::remove_file(TEST1_LEXER_FILENAME).expect(&format!("couldn't delete {TEST1_LEXER_FILENAME}"));
    std::fs::remove_file(TEST1_PARSER_FILENAME).expect(&format!("couldn't delete {TEST1_PARSER_FILENAME}"));

    // generates the lexer and parser files
    match action_file_to_file(Action::Generate) {
        Ok(log) => {
            if VERBOSE { println!("Parser generated successfully\n{log}"); }
        }
        Err(err) => panic!("Error: {err}"),
    }

    // reads the new files
    let result_lexer = std::fs::read_to_string(TEST1_LEXER_FILENAME)
        .expect(&format!("lexer code should have been written in {TEST1_LEXER_FILENAME}"));
    let result_parser = std::fs::read_to_string(TEST1_PARSER_FILENAME)
        .expect(&format!("parser code should have been written in {TEST1_PARSER_FILENAME}"));

    // compares the new files to their reference
    assert_eq!(result_lexer, expected_lexer);
    assert_eq!(result_parser, expected_parser);
}

// ---------------------------------------------------------------------------------------------

fn read_lexicon_grammar() -> (String, String) {
    (
        std::fs::read_to_string(TEST1_LEXICON_FILENAME).expect(&format!("couldn't find lexicon in {TEST1_LEXICON_FILENAME}")),
        std::fs::read_to_string(TEST1_GRAMMAR_FILENAME).expect(&format!("couldn't find grammar in {TEST1_GRAMMAR_FILENAME}")),
    )
}

fn action_string_to_tag(action: Action) -> Result<BufLog, GenParserError> {
    let (lexicon, grammar) = read_lexicon_grammar();
    let lexer_spec = Specification::String(lexicon);
    let parser_spec = Specification::String(grammar);
    let options = Options {
        lexer_code: CodeLocation::FileTag { filename: TEST1_TAGS_FILENAME.to_string(), tag: TEST1_LEXER_TAG.to_string() },
        lexer_indent: 0,
        parser_code: CodeLocation::FileTag { filename: TEST1_TAGS_FILENAME.to_string(), tag: TEST1_PARSER_TAG.to_string() },
        parser_indent: 0,
        lexer_headers: Vec::new(),
        parser_headers: Vec::new(),
        extra_libs: vec!["super::listener_types::test1::*".to_string()],
        gen_parser_alts: true,
        gen_wrapper: true,
        gen_span_params: true,
    };
    try_gen_parser(lexer_spec, parser_spec, action, options)
}

/// Generates the lexer/parser files in [TEST1_LEXER_FILENAME] and [TEST1_PARSER_FILENAME] the first time when
/// those files don't exist yet (visual check required).
fn gen_init_string_to_tag() {
    match action_string_to_tag(Action::Generate) {
        Ok(log) => println!("{log}"),
        Err(e) => {
            panic!("first code generation: {e}")
        }
    }
}

/// Verifies that the destination tag contents are created again and identical to their previous version
/// (which was the reference).
#[ignore]
#[test]
fn test_string_to_tag() {
    const VERBOSE: bool = false;

    // checks that the reference texts exist
    let expected_lexer = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXER_TAG)
        .expect(&format!("lexer code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_LEXER_TAG}]"));
    let expected_parser = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_PARSER_TAG)
        .expect(&format!("parser code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_PARSER_TAG}]"));

    // deletes the reference texts
    replace_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXER_TAG, "removed\n")
        .expect(&format!("couldn't remove {TEST1_TAGS_FILENAME} [{TEST1_LEXER_TAG}]"));
    replace_tagged_source(TEST1_TAGS_FILENAME, TEST1_PARSER_TAG, "removed\n")
        .expect(&format!("couldn't remove {TEST1_TAGS_FILENAME} [{TEST1_PARSER_TAG}]"));

    // generates the lexer and parser files
    match action_string_to_tag(Action::Generate) {
        Ok(log) => {
            if VERBOSE { println!("Parser generated successfully\n{log}"); }
        }
        Err(err) => panic!("Error: {err}"),
    }

    // reads the new files
    let result_lexer = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXER_TAG)
        .expect(&format!("lexer code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_LEXER_TAG}]"));
    let result_parser = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_PARSER_TAG)
        .expect(&format!("parser code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_PARSER_TAG}]"));

    // compares the new files to their reference
    assert_eq!(result_lexer, expected_lexer);
    assert_eq!(result_parser, expected_parser);
}

// ---------------------------------------------------------------------------------------------

fn action_tag_to_tag(action: Action) -> Result<BufLog, GenParserError> {
    let lexer_spec = Specification::FileTag { filename: TEST1_TAGS_FILENAME.to_string(), tag: TEST1_LEXICON_TAG.to_string() };
    let parser_spec = Specification::FileTag { filename: TEST1_TAGS_FILENAME.to_string(), tag: TEST1_GRAMMAR_TAG.to_string() };
    let options = Options {
        lexer_code: CodeLocation::FileTag { filename: TEST1_TAGS_FILENAME.to_string(), tag: TEST1_LEXER_TAG.to_string() },
        lexer_indent: 0,
        parser_code: CodeLocation::FileTag { filename: TEST1_TAGS_FILENAME.to_string(), tag: TEST1_PARSER_TAG.to_string() },
        parser_indent: 0,
        lexer_headers: Vec::new(),
        parser_headers: Vec::new(),
        extra_libs: vec!["super::listener_types::test1::*".to_string()],
        gen_parser_alts: true,
        gen_wrapper: true,
        gen_span_params: true,
    };
    try_gen_parser(lexer_spec, parser_spec, action, options)
}

/// Generates the lexicon/grammar in [TEST1_TAGS_FILENAME], tags [TEST1_LEXICON_TAG] and [TEST1_GRAMMAR_TAG]
/// the first time when those contents don't exist yet.
/// The reference lexer and parser in the same file can be (re)generated with [gen_init_string_to_tag].
fn gen_init_tag_to_tag() {
    let (lexicon, grammar) = read_lexicon_grammar();
    replace_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXICON_TAG, &lexicon)
        .expect(&format!("couldn't copy lexicon into {TEST1_TAGS_FILENAME} [{TEST1_LEXICON_TAG}]"));
    replace_tagged_source(TEST1_TAGS_FILENAME, TEST1_GRAMMAR_TAG, &grammar)
        .expect(&format!("couldn't copy grammar into {TEST1_TAGS_FILENAME} [{TEST1_GRAMMAR_TAG}]"));
}

/// Verifies that the destination tag contents are created again and identical to their previous version
/// (which was the reference).
#[ignore]
#[test]
fn test_tag_to_tag() {
    const VERBOSE: bool = false;

    // checks that the reference texts exist
    let expected_lexer = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXER_TAG)
        .expect(&format!("lexer code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_LEXER_TAG}]"));
    let expected_parser = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_PARSER_TAG)
        .expect(&format!("parser code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_PARSER_TAG}]"));

    // deletes the reference texts
    replace_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXER_TAG, "removed\n")
        .expect(&format!("couldn't remove {TEST1_TAGS_FILENAME} [{TEST1_LEXER_TAG}]"));
    replace_tagged_source(TEST1_TAGS_FILENAME, TEST1_PARSER_TAG, "removed\n")
        .expect(&format!("couldn't remove {TEST1_TAGS_FILENAME} [{TEST1_PARSER_TAG}]"));

    // generates the lexer and parser files
    match action_tag_to_tag(Action::Generate) {
        Ok(log) => {
            if VERBOSE { println!("Parser generated successfully\n{log}"); }
        }
        Err(err) => panic!("Error: {err}"),
    }

    // reads the new files
    let result_lexer = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_LEXER_TAG)
        .expect(&format!("lexer code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_LEXER_TAG}]"));
    let result_parser = get_tagged_source(TEST1_TAGS_FILENAME, TEST1_PARSER_TAG)
        .expect(&format!("parser code should already exist in {TEST1_TAGS_FILENAME} [{TEST1_PARSER_TAG}]"));

    // compares the new files to their reference
    assert_eq!(result_lexer, expected_lexer);
    assert_eq!(result_parser, expected_parser);
}
