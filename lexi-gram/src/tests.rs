// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexigram_lib::file_utils::{get_tagged_source, replace_tagged_source};
use lexigram_lib::log::BufLog;
use lexigram_lib::lexergen::LexigramCrate;
use crate::gen_parser::{try_gen_parser, GenParserError};
use crate::{gencode, genspec};
use crate::options::{Action, Options, OptionsBuilder, ERR_LEXER_AFTER_PARSER, ERR_LEXER_CODE_ALREADY_SET, ERR_PARSER_SET_BEFORE_LEXER_NOT_SET, ERR_LEXER_SPEC_ALREADY_SET, ERR_LEXER_SPEC_OR_CODE_ALREADY_SET, ERR_PARSER_CODE_ALREADY_SET, ERR_PARSER_SPEC_ALREADY_SET, ERR_PARSER_SPEC_OR_CODE_ALREADY_SET, ERR_MISSING_LEXER_OPTION, ERR_MISSING_PARSER_OPTION};

const TEST_LEXICON_FILENAME: &str = "data/test1.l";
const TEST_GRAMMAR_FILENAME: &str = "data/test1.g";

fn read_lexicon_grammar() -> (String, String) {
    (
        std::fs::read_to_string(TEST_LEXICON_FILENAME).expect(&format!("couldn't find lexicon in {TEST_LEXICON_FILENAME}")),
        std::fs::read_to_string(TEST_GRAMMAR_FILENAME).expect(&format!("couldn't find grammar in {TEST_GRAMMAR_FILENAME}")),
    )
}

/// (Re)generates the initial content
#[ignore]
#[test]
fn gen_init_content() {
    test1::gen_init_file_to_file();
    test2::gen_init_string_to_tag();
    test3::gen_init_tag_to_tag();
}

// ---------------------------------------------------------------------------------------------

mod test1 {
    use super::*;

    const TEST1_LEXER_FILENAME: &str = "tests/out/test1_lexer.rs";
    const TEST1_PARSER_FILENAME: &str = "tests/out/test1_parser.rs";

    fn action_file_to_file(action: Action) -> Result<BufLog, GenParserError> {
        let options = OptionsBuilder::new()
            .headers(["#![allow(unused)]"])
            .lexer(genspec!(filename: TEST_LEXICON_FILENAME), gencode!(filename: TEST1_LEXER_FILENAME))
            .parser(genspec!(filename: TEST_GRAMMAR_FILENAME), gencode!(filename: TEST1_PARSER_FILENAME))
            .extra_libs(["super::listener_types::test1::*"])
            .span_params(true)
            .use_full_lib(true)
            .build()
            .expect("should have no error");
        try_gen_parser(action, options)
    }

    /// Generates the lexer/parser files in [TEST1_LEXER_FILENAME] and [TEST1_PARSER_FILENAME] the first time when
    /// those files don't exist yet (visual check required).
    pub(super) fn gen_init_file_to_file() {
        match action_file_to_file(Action::Generate) {
            Ok(log) => println!("{log}"),
            Err(e) => {
                panic!("first code generation: {e}")
            }
        }
    }

    /// Verifies that the destination files are created again and identical to their previous version
    /// (which was the reference).
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
}
// ---------------------------------------------------------------------------------------------

mod test2 {
    use super::*;

    const TEST2_TAGS_FILENAME: &str = "tests/out/test2_tags.rs";
    const TEST2_LEXER_TAG: &str = "test2_lexer_tag";
    const TEST2_PARSER_TAG: &str = "test2_parser_tag";

    fn action_string_to_tag(action: Action) -> Result<BufLog, GenParserError> {
        let (lexicon, grammar) = read_lexicon_grammar();
        let options = OptionsBuilder::new()
            .lexer(genspec!(string: lexicon), gencode!(filename: TEST2_TAGS_FILENAME, tag: TEST2_LEXER_TAG))
            .parser(genspec!(string: grammar), gencode!(filename: TEST2_TAGS_FILENAME, tag: TEST2_PARSER_TAG))
            .extra_libs(["super::listener_types::test1::*"])
            .span_params(true)
            .set_crate(LexigramCrate::Custom("core".to_string()))
            .build()
            .expect("should have no error");
        try_gen_parser(action, options)
    }

    /// Generates the lexer/parser files in [TEST1_LEXER_FILENAME] and [TEST1_PARSER_FILENAME] the first time when
    /// those files don't exist yet (visual check required).
    pub(super) fn gen_init_string_to_tag() {
        match action_string_to_tag(Action::Generate) {
            Ok(log) => println!("{log}"),
            Err(e) => {
                panic!("first code generation: {e}")
            }
        }
    }

    /// Verifies that the destination tag contents are created again and identical to their previous version
    /// (which was the reference).
    #[test]
    fn test_string_to_tag() {
        const VERBOSE: bool = false;

        // checks that the reference texts exist
        let expected_lexer = get_tagged_source(TEST2_TAGS_FILENAME, TEST2_LEXER_TAG)
            .expect(&format!("lexer code should already exist in {TEST2_TAGS_FILENAME} [{TEST2_LEXER_TAG}]"));
        let expected_parser = get_tagged_source(TEST2_TAGS_FILENAME, TEST2_PARSER_TAG)
            .expect(&format!("parser code should already exist in {TEST2_TAGS_FILENAME} [{TEST2_PARSER_TAG}]"));

        // deletes the reference texts
        replace_tagged_source(TEST2_TAGS_FILENAME, TEST2_LEXER_TAG, "removed\n")
            .expect(&format!("couldn't remove {TEST2_TAGS_FILENAME} [{TEST2_LEXER_TAG}]"));
        replace_tagged_source(TEST2_TAGS_FILENAME, TEST2_PARSER_TAG, "removed\n")
            .expect(&format!("couldn't remove {TEST2_TAGS_FILENAME} [{TEST2_PARSER_TAG}]"));

        // generates the lexer and parser files
        match action_string_to_tag(Action::Generate) {
            Ok(log) => {
                if VERBOSE { println!("Parser generated successfully\n{log}"); }
            }
            Err(err) => panic!("Error: {err}"),
        }

        // reads the new files
        let result_lexer = get_tagged_source(TEST2_TAGS_FILENAME, TEST2_LEXER_TAG)
            .expect(&format!("lexer code should already exist in {TEST2_TAGS_FILENAME} [{TEST2_LEXER_TAG}]"));
        let result_parser = get_tagged_source(TEST2_TAGS_FILENAME, TEST2_PARSER_TAG)
            .expect(&format!("parser code should already exist in {TEST2_TAGS_FILENAME} [{TEST2_PARSER_TAG}]"));

        // compares the new files to their reference
        assert_eq!(result_lexer, expected_lexer);
        assert_eq!(result_parser, expected_parser);
    }
}

// ---------------------------------------------------------------------------------------------

mod test3 {
    use super::*;

    const TEST3_TAGS_FILENAME: &str = "tests/out/test3_tags.rs";
    const TEST3_LEXICON_TAG: &str = "test3_lexicon_tag";
    const TEST3_GRAMMAR_TAG: &str = "test3_grammar_tag";
    const TEST3_LEXER_TAG: &str = "test3_lexer_tag";
    const TEST3_PARSER_TAG: &str = "test3_parser_tag";

    fn action_tag_to_tag(action: Action) -> Result<BufLog, GenParserError> {
        let options = OptionsBuilder::new()
            .lexer(
                genspec!(filename: TEST3_TAGS_FILENAME, tag: TEST3_LEXICON_TAG),
                gencode!(filename: TEST3_TAGS_FILENAME, tag: TEST3_LEXER_TAG))
            .parser(
                genspec!(filename: TEST3_TAGS_FILENAME, tag: TEST3_GRAMMAR_TAG),
                gencode!(filename: TEST3_TAGS_FILENAME, tag: TEST3_PARSER_TAG))
            .extra_libs(["super::listener_types::test1::*"])
            .span_params(true)
            .set_crate(LexigramCrate::Full)
            .build()
            .expect("should have no error");
        try_gen_parser(action, options)
    }

    /// Generates the lexicon/grammar in [TEST3_TAGS_FILENAME], tags [TEST3_LEXICON_TAG] and [TEST3_GRAMMAR_TAG]
    /// the first time when those contents don't exist yet.
    /// The reference lexer and parser in the same file can be (re)generated with [gen_init_string_to_tag].
    pub(super) fn gen_init_tag_to_tag() {
        let (lexicon, grammar) = read_lexicon_grammar();
        replace_tagged_source(TEST3_TAGS_FILENAME, TEST3_LEXICON_TAG, &lexicon)
            .expect(&format!("couldn't copy lexicon into {TEST3_TAGS_FILENAME} [{TEST3_LEXICON_TAG}]"));
        replace_tagged_source(TEST3_TAGS_FILENAME, TEST3_GRAMMAR_TAG, &grammar)
            .expect(&format!("couldn't copy grammar into {TEST3_TAGS_FILENAME} [{TEST3_GRAMMAR_TAG}]"));
    }

    /// Verifies that the destination tag contents are created again and identical to their previous version
    /// (which was the reference).
    #[test]
    fn test_tag_to_tag() {
        const VERBOSE: bool = false;

        // checks that the reference texts exist
        let expected_lexer = get_tagged_source(TEST3_TAGS_FILENAME, TEST3_LEXER_TAG)
            .expect(&format!("lexer code should already exist in {TEST3_TAGS_FILENAME} [{TEST3_LEXER_TAG}]"));
        let expected_parser = get_tagged_source(TEST3_TAGS_FILENAME, TEST3_PARSER_TAG)
            .expect(&format!("parser code should already exist in {TEST3_TAGS_FILENAME} [{TEST3_PARSER_TAG}]"));

        // deletes the reference texts
        replace_tagged_source(TEST3_TAGS_FILENAME, TEST3_LEXER_TAG, "removed\n")
            .expect(&format!("couldn't remove {TEST3_TAGS_FILENAME} [{TEST3_LEXER_TAG}]"));
        replace_tagged_source(TEST3_TAGS_FILENAME, TEST3_PARSER_TAG, "removed\n")
            .expect(&format!("couldn't remove {TEST3_TAGS_FILENAME} [{TEST3_PARSER_TAG}]"));

        // generates the lexer and parser files
        match action_tag_to_tag(Action::Generate) {
            Ok(log) => {
                if VERBOSE { println!("Parser generated successfully\n{log}"); }
            }
            Err(err) => panic!("Error: {err}"),
        }

        // reads the new files
        let result_lexer = get_tagged_source(TEST3_TAGS_FILENAME, TEST3_LEXER_TAG)
            .expect(&format!("lexer code should already exist in {TEST3_TAGS_FILENAME} [{TEST3_LEXER_TAG}]"));
        let result_parser = get_tagged_source(TEST3_TAGS_FILENAME, TEST3_PARSER_TAG)
            .expect(&format!("parser code should already exist in {TEST3_TAGS_FILENAME} [{TEST3_PARSER_TAG}]"));

        // compares the new files to their reference
        assert_eq!(result_lexer, expected_lexer);
        assert_eq!(result_parser, expected_parser);
    }
}

// ---------------------------------------------------------------------------------------------

mod test4 {
    use super::*;

    // those files and tags needn't exist
    const TEST4_LEXER_FILENAME: &str = "tests/out/test4_lexer.rs";
    const TEST4_PARSER_FILENAME: &str = "tests/out/test4_parser.rs";
    const TEST4_LEXICON_FILENAME: &str = "tests/out/test4_lexicon.l";
    const TEST4_GRAMMAR_FILENAME: &str = "tests/out/test4_grammar.g";
    const TEST4_LEXICON_TAG: &str = "test4_lexicon_tag";
    const TEST4_GRAMMAR_TAG: &str = "test4_grammar_tag";
    const TEST4_LEXER_TAG: &str = "test4_lexer_tag";
    const TEST4_PARSER_TAG: &str = "test4_parser_tag";

    #[test]
    fn options_builder() {
        let options1_expected = Options {
            lexer_spec: genspec!(filename: TEST_LEXICON_FILENAME),
            lexer_code: gencode!(filename: TEST4_LEXER_FILENAME),
            lexer_indent: 0,
            parser_spec: genspec!(filename: TEST_GRAMMAR_FILENAME),
            parser_code: gencode!(filename: TEST4_PARSER_FILENAME),
            parser_indent: 0,
            lexer_headers: vec!["#![allow(unused)]".to_string()],
            parser_headers: vec!["#![allow(unused)]".to_string()],
            extra_libs: vec!["super::listener_types::test1::*".to_string()],
            gen_parser_alts: true,
            gen_wrapper: true,
            gen_span_params: false,
            lib_crate: LexigramCrate::Core,
        };
        let options1b = OptionsBuilder::new()
            .indent(0)
            .headers(["#![allow(unused)]"])
            .lexer(genspec!(filename: TEST_LEXICON_FILENAME), gencode!(filename: TEST4_LEXER_FILENAME))
            .parser(genspec!(filename: TEST_GRAMMAR_FILENAME), gencode!(filename: TEST4_PARSER_FILENAME))
            .extra_libs(["super::listener_types::test1::*"])
            .parser_alts(true)
            .wrapper(true)
            .span_params(false)
            .build()
            .expect("should have no error");
        let options1c = OptionsBuilder::new()
            .headers(["#![allow(unused)]"])
            .lexer(genspec!(filename: TEST_LEXICON_FILENAME), gencode!(filename: TEST4_LEXER_FILENAME))
            .parser(genspec!(filename: TEST_GRAMMAR_FILENAME), gencode!(filename: TEST4_PARSER_FILENAME))
            .extra_libs(["super::listener_types::test1::*"])
            .parser_alts(true)
            .build()
            .expect("should have no error");
        assert_eq!(options1b, options1_expected);
        assert_eq!(options1c, options1_expected);

        let lexicon = "lexicon Test; A: 'a';".to_string();
        let grammar = "grammar Test; a: A;".to_string();
        let options2_expected = Options {
            lexer_spec: genspec!(string: lexicon),
            lexer_code: gencode!(filename: TEST4_LEXER_FILENAME, tag: TEST4_LEXER_TAG),
            lexer_indent: 0,
            parser_spec: genspec!(string: grammar),
            parser_code: gencode!(filename: TEST4_PARSER_FILENAME, tag: TEST4_PARSER_TAG),
            parser_indent: 0,
            lexer_headers: Vec::new(),
            parser_headers: Vec::new(),
            extra_libs: vec!["super::listener_types::test1::*".to_string()],
            gen_parser_alts: false,
            gen_wrapper: true,
            gen_span_params: false,
            lib_crate: LexigramCrate::Full,
        };
        let options2 = OptionsBuilder::new()
            .lexer(genspec!(string: lexicon), gencode!(filename: TEST4_LEXER_FILENAME, tag: TEST4_LEXER_TAG))
            .parser(genspec!(string: grammar), gencode!(filename: TEST4_PARSER_FILENAME, tag: TEST4_PARSER_TAG))
            .extra_libs(["super::listener_types::test1::*"])
            .set_crate(LexigramCrate::Full)
            .build()
            .expect("should have no error");
        assert_eq!(options2, options2_expected);

        let options3_expected = Options {
            lexer_spec: genspec!(filename: TEST4_LEXICON_FILENAME, tag: TEST4_LEXICON_TAG),
            lexer_code: gencode!(filename: TEST4_LEXER_FILENAME, tag: TEST4_LEXER_TAG),
            lexer_indent: 0,
            parser_spec: genspec!(filename: TEST4_GRAMMAR_FILENAME, tag: TEST4_GRAMMAR_TAG),
            parser_code: gencode!(filename: TEST4_PARSER_FILENAME, tag: TEST4_PARSER_TAG),
            parser_indent: 0,
            lexer_headers: Vec::new(),
            parser_headers: Vec::new(),
            extra_libs: vec!["super::listener_types::test1::*".to_string()],
            gen_parser_alts: false,
            gen_wrapper: true,
            gen_span_params: false,
            lib_crate: LexigramCrate::Core,
        };
        let options3 = OptionsBuilder::new()
            .lexer(genspec!(filename: TEST4_LEXICON_FILENAME, tag: TEST4_LEXICON_TAG), gencode!(filename: TEST4_LEXER_FILENAME, tag: TEST4_LEXER_TAG))
            .parser(genspec!(filename: TEST4_GRAMMAR_FILENAME, tag: TEST4_GRAMMAR_TAG), gencode!(filename: TEST4_PARSER_FILENAME, tag: TEST4_PARSER_TAG))
            .extra_libs(["super::listener_types::test1::*"])
            .build()
            .expect("should have no error");
        assert_eq!(options3, options3_expected);
    }
}

// ---------------------------------------------------------------------------------------------

mod failing_tests {
    use super::*;

    const TEST5_LEXICON_FILENAME: &str = "../build-rtsgen/src/rtsgen.l";
    const TEST5_TAGS_FILENAME: &str = "../src/rtsgen/mod.rs";
    const TEST5_LEXER_TAG: &str = "rtsgen_lexer";
    const TEST5_PARSER_TAG: &str = "rtsgen_parser";

    #[test]
    fn bad_params() {
        let opt_empty = Options {
            lexer_spec: genspec!(none),
            lexer_code: gencode!(filename: ""),
            lexer_indent: 0,
            parser_spec: genspec!(none),
            parser_code: gencode!(filename: ""),
            parser_indent: 0,
            lexer_headers: vec![],
            parser_headers: vec![],
            extra_libs: vec![],
            gen_parser_alts: false,
            gen_wrapper: false,
            gen_span_params: false,
            lib_crate: LexigramCrate::Core,
        };
        let opt_fake = Options {
            lexer_spec: genspec!(none),
            lexer_code: gencode!(filename: TEST5_TAGS_FILENAME, tag: TEST5_LEXER_TAG),
            lexer_indent: 4,
            parser_spec: genspec!(none),
            parser_code: gencode!(filename: TEST5_TAGS_FILENAME, tag: TEST5_PARSER_TAG),
            parser_indent: 4,
            lexer_headers: vec![],
            parser_headers: vec![],
            extra_libs: vec![],
            gen_parser_alts: false,
            gen_wrapper: false,
            gen_span_params: false,
            lib_crate: LexigramCrate::Full,
        };

        let tests = vec![
            (   // 0
                genspec!(none),
                genspec!(string: ""),
                &opt_empty,
                Action::Generate,
                "cannot verify sources without any lexicon"
            ),
            (   // 1
                genspec!(filename: "fake filename"),
                genspec!(string: ""),
                &opt_empty,
                Action::Generate,
                "error while reading the lexicon (file 'fake filename')"
            ),
            (   // 2
                genspec!(string: "bad lexicon"),
                genspec!(string: ""),
                &opt_empty,
                Action::Generate,
                "error while building the parser: Errors have occurred in Lexi:"
            ),
            (   // 3
                genspec!(string: "lexicon Fake; A: 'a';"),
                genspec!(string: "bad grammar"),
                &opt_empty,
                Action::Generate,
                "error while building the parser: Errors have occurred in Gram:"
            ),
            (   // 4
                genspec!(string: "lexicon Fake; A: 'a';"),
                genspec!(string: "grammar Fake; a: A;"),
                &opt_empty,
                Action::Generate,
                "error while writing the lexer code (file '')"
            ),
            (   // 5
                genspec!(none),
                genspec!(string: "grammar Fake; a: A;"),
                &opt_empty,
                Action::Generate,
                "invalid parameters: cannot verify sources without any lexicon"
            ),
            (   // 6
                genspec!(filename: TEST5_LEXICON_FILENAME),
                genspec!(none),
                &opt_fake,
                Action::Verify,
                "parser source code verification required but no grammar was provided"
            ),
        ];
        const VERBOSE: bool = false;
        for (id, (lexer_spec, parser_spec, option, action, message)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("Test {id} {:-<80}", "") }
            let mut options = option.clone();
            options.lexer_spec = lexer_spec;
            options.parser_spec = parser_spec;
            match try_gen_parser(action, options) {
                Ok(_) => panic!("test {id} doesn't show the error '{message}'"),
                Err(e) => {
                    if VERBOSE { println!("# {e}"); }
                    assert!(e.to_string().contains(message), "test {id} doesn't show the error '{message}': {e}")
                }
            }
        }
    }

    #[test]
    fn options_builder_errors() {
        let tests = vec![
            (
                OptionsBuilder::new()
                    .lexer_spec(genspec!(string: "lexicon"))
                    .lexer_code(gencode!(stdout))
                    .lexer_spec(genspec!(string: "lexicon"))
                    .build(),
                ERR_LEXER_SPEC_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .lexer_spec(genspec!(string: "lexicon"))
                    .build(),
                ERR_LEXER_SPEC_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer_code(gencode!(filename: "lexer.rs"))
                    .lexer_spec(genspec!(string: "lexicon"))
                    .lexer_code(gencode!(stdout))
                    .build(),
                ERR_LEXER_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .lexer_code(gencode!(stdout))
                    .build(),
                ERR_LEXER_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer_code(gencode!(stdout))
                    .parser_spec(genspec!(string: "grammar"))
                    .lexer_spec(genspec!(string: "lexicon"))
                    .build(),
                ERR_LEXER_AFTER_PARSER
            ),
            (
                OptionsBuilder::new()
                    .lexer_spec(genspec!(string: "lexicon"))
                    .parser_code(gencode!(filename: "grammar.g"))
                    .lexer_code(gencode!(filename: "lexer.rs"))
                    .build(),
                ERR_LEXER_AFTER_PARSER
            ),
            (
                OptionsBuilder::new()
                    .lexer_spec(genspec!(string: "lexicon"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .build(),
                ERR_LEXER_AFTER_PARSER
            ),
            (
                OptionsBuilder::new()
                    .lexer_spec(genspec!(string: "lexicon"))
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .build(),
                ERR_LEXER_SPEC_OR_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer_code(gencode!(filename: "lexer.rs"))
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .build(),
                ERR_LEXER_SPEC_OR_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(stdout))
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .build(),
                ERR_LEXER_SPEC_OR_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer_spec(genspec!(string: "lexicon"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .build(),
                ERR_MISSING_LEXER_OPTION
            ),
            (
                OptionsBuilder::new()
                    .lexer_code(gencode!(filename: "lexer.rs"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .build(),
                ERR_MISSING_LEXER_OPTION
            ),
            (
                OptionsBuilder::new()
                    .parser_spec(genspec!(string: "grammar"))
                    .build(),
                ERR_PARSER_SET_BEFORE_LEXER_NOT_SET
            ),
            (
                OptionsBuilder::new()
                    .parser_code(gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_SET_BEFORE_LEXER_NOT_SET
            ),
            (
                OptionsBuilder::new()
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_SET_BEFORE_LEXER_NOT_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser_spec(genspec!(string: "grammar"))
                    .parser_spec(genspec!(string: "grammar"))
                    .build(),
                ERR_PARSER_SPEC_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .parser_spec(genspec!(string: "grammar"))
                    .build(),
                ERR_PARSER_SPEC_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser_code(gencode!(filename: "parser.rs"))
                    .parser_code(gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .parser_code(gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser_spec(genspec!(string: "grammar"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_SPEC_OR_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser_code(gencode!(filename: "parser.rs"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_SPEC_OR_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .parser(genspec!(string: "grammar"), gencode!(filename: "parser.rs"))
                    .build(),
                ERR_PARSER_SPEC_OR_CODE_ALREADY_SET
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser_spec(genspec!(string: "grammar"))
                    .build(),
                ERR_MISSING_PARSER_OPTION
            ),
            (
                OptionsBuilder::new()
                    .lexer(genspec!(string: "lexicon"), gencode!(filename: "lexer.rs"))
                    .parser_code(gencode!(filename: "parser.rs"))
                    .build(),
                ERR_MISSING_PARSER_OPTION
            ),
        ];
        for (id, (result, expected_message)) in tests.into_iter().enumerate() {
            let msg = format!("## ERROR: test {id}: expected Err({expected_message:?}) instead of {result:?}");
            match result {
                Ok(_) => panic!("{msg}"),
                Err(s) => if s != expected_message {
                    panic!("{msg}")
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Generates the test lexer in lexigram-core/src/lexer/tests.rs

mod gen_test_lexer {
    use super::*;
    use crate::LEXIGRAM_PKG_VERSION;

    const LEXER_TEST_FILENAME: &str = "../lexigram-core/src/lexer/tests.rs";
    const TAG_LEXICON: &str = "lexer1_lexicon";
    const TAG_CODE: &str = "lexer1_source";

    fn action(action: Action) -> Result<BufLog, GenParserError> {
        let options = OptionsBuilder::new()
            .lexer(
                genspec!(filename: LEXER_TEST_FILENAME, tag: TAG_LEXICON),
                gencode!(filename: LEXER_TEST_FILENAME, tag: TAG_CODE))
            .indent(4)
            .headers([
                format!("// This code is generated with lexigram version {LEXIGRAM_PKG_VERSION} from {}", file!()),
                format!("// and corresponds to the lexicon above between tags [{}]", TAG_LEXICON)])
            .set_crate(LexigramCrate::Custom("crate".to_string()))
            .build()
            .expect("option build error");
        try_gen_parser(action, options)
    }

    #[ignore]
    #[test]
    fn gen_lexer1() {
        match action(Action::Generate) {
            Ok(log) => {
                println!("Successful:\n{log}");
            }
            Err(e) => {
                let msg = e.to_string();
                if let Some(log) = e.get_log() {
                    println!("{log}");
                }
                panic!("Failed to build lexer:\n{msg}");
            }
        }
    }

    #[test]
    fn check_lexer1() {
        match action(Action::Verify) {
            Ok(_) => { }
            Err(e) => {
                println!("{e:?}");
                let msg = e.to_string();
                if let Some(log) = e.get_log() {
                    println!("{log}");
                }
                panic!("Verification failed:\n{msg}");
            }
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Generates the test lexer/parser in lexigram-core/src/parser/tests.rs

mod gen_test_parser {
    use super::*;
    use crate::LEXIGRAM_PKG_VERSION;

    const SOURCE_FILENAME: &str = "../lexigram-core/src/parser/tests.rs";
    const TAG_LEXICON: &str = "lexer_lexicon";
    const TAG_GRAMMAR: &str = "parser_grammar";
    const TAG_LEXER_CODE: &str = "lexer_source";
    const TAG_PARSER_CODE: &str = "parser_source";

    fn action(action: Action) -> Result<BufLog, GenParserError> {
        let options = OptionsBuilder::new()
            .indent(4)
            .lexer(
                genspec!(filename: SOURCE_FILENAME, tag: TAG_LEXICON),
                gencode!(filename: SOURCE_FILENAME, tag: TAG_LEXER_CODE))
            .headers([
                format!("// This code is generated from {}", file!()),
                format!("// and corresponds to the lexicon above between tags [{}]", TAG_LEXICON)])
            .parser(
                genspec!(filename: SOURCE_FILENAME, tag: TAG_GRAMMAR),
                gencode!(filename: SOURCE_FILENAME, tag: TAG_PARSER_CODE))
            .headers([
                format!("// This code is generated with lexigram version {LEXIGRAM_PKG_VERSION} from {}", file!()),
                format!("// and corresponds to the grammar above between tags [{}]", TAG_GRAMMAR)])
            .wrapper(false)
            .set_crate(LexigramCrate::Custom("crate".to_string()))
            .build()
            .expect("option build error");
        try_gen_parser(action, options)
    }

    #[ignore]
    #[test]
    fn gen_parser() {
        match action(Action::Generate) {
            Ok(log) => {
                println!("Successful:\n{log}");
            }
            Err(e) => {
                let msg = e.to_string();
                if let Some(log) = e.get_log() {
                    println!("{log}");
                }
                panic!("Failed to build lexer:\n{msg}");
            }
        }
    }

    #[test]
    fn check_parser() {
        match action(Action::Verify) {
            Ok(_) => { }
            Err(e) => {
                let msg = e.to_string();
                if let Some(log) = e.get_log() {
                    println!("{log}");
                }
                panic!("Verification failed:\n{msg}");
            }
        }
    }
}
