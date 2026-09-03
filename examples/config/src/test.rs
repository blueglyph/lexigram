// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexi_gram::lexigram_lib::log::BufLog;
use lexi_gram::lexigram_lib::parsergen::ParserType;
use crate::ConfigResult;
use crate::parser_ll1::ConfigLL1Parser;
use crate::parser_lalr::ConfigLALRParser;

// abstract type for several parser types
enum TestParser<'l, 'p, 'ls> {
    LL1(ConfigLL1Parser<'l, 'p, 'ls>),
    LALR(ConfigLALRParser<'l, 'p, 'ls>)
}

impl<'l, 'p, 'ls: 'l> TestParser<'l, 'p, 'ls> {
    fn new(parser_type: ParserType) -> Self {
        match parser_type {
            ParserType::LL1 => TestParser::LL1(ConfigLL1Parser::new()),
            ParserType::LALR => TestParser::LALR(ConfigLALRParser::new()),
        }
    }

    pub fn parse(&mut self, text: &'ls str) -> Result<ConfigResult, BufLog> {
        match self {
            TestParser::LL1(ll1) => ll1.parse(text),
            TestParser::LALR(lalr) => lalr.parse(text),
        }
    }
}

#[test]
fn test_run() {
    const VERBOSE: bool = false;
    for parser_type in [ParserType::LL1, ParserType::LALR] {
        if VERBOSE { println!("{:=<80}\nparser {parser_type:?}", "") }
        let tests = vec![(SRC1, options_1::options()), (SRC2, options_2::options())];
        let mut p = TestParser::new(parser_type);
        for (i, (src, expected_options)) in tests.into_iter().enumerate() {
            if VERBOSE { println!("source #{i}"); }
            match p.parse(src) {
                Ok(ConfigResult { options, log }) => {
                    if VERBOSE { println!("{options:#?}\n\nlog:{log}"); }
                    assert_eq!(options, expected_options);
                }
                Err(log) => panic!("error\n{log}"),
            }
        }
    }
}


// ---------------------------------------------------------
static SRC1: &str = r#"
def SOURCE_FILENAME = "../watcher/src/lib.rs";

lexer {
    combined: "src/watcher.lg",
    output: SOURCE_FILENAME ["watcher_lexer"],
    indent: 4
}
parser {
    output: SOURCE_FILENAME ["watcher_parser"],
    indent: 4
}
options {
    nt-value: set { "<default>", "-lexer", "-parser" },
    nt-value: set { "options" },
    spans: true
}
"#;

pub mod options_1 {
    use lexi_gram::lexigram_lib::parsergen::NTValue;
    use lexi_gram::options::{Options, OptionsBuilder};
    use lexi_gram::{gencode, genspec};

    static LEXICON_FILENAME: &str = "src/watcher.lg";
    static SOURCE_FILENAME: &str = "../watcher/src/lib.rs";
    static LEXER_TAG: &str = "watcher_lexer";
    static PARSER_TAG: &str = "watcher_parser";
    const LEXER_INDENT: usize = 4;
    const PARSER_INDENT: usize = 4;

    pub fn options() -> Options {
        OptionsBuilder::new()
            .combined_spec(genspec!(filename: LEXICON_FILENAME))
            .lexer_code(gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
            .indent(LEXER_INDENT)
            // grammar is combined with lexicon, no need to define parser_spec
            .parser_code(gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
            .indent(PARSER_INDENT)
            .set_nt_value(NTValue::SetNames(vec![
                "<default>".to_string(),
                "-lexer".to_string(),
                "-parser".to_string(),
                "options".to_string()]))
            .span_params(true)
            .build()
            .expect("should have no error")
    }
}

// ---------------------------------------------------------
static SRC2: &str = r#"
def LEXICON_GRAMMAR_FILENAME = "src/microcalc.lg";
def SOURCE_FILENAME = "../microcalc/src/main.rs";

lexer {
    input: LEXICON_GRAMMAR_FILENAME,
    output: SOURCE_FILENAME ["microcalc_lexer"],
    indent: 4
}
parser {
    input: LEXICON_GRAMMAR_FILENAME,
    output: SOURCE_FILENAME ["microcalc_parser"],
    indent: 4
}
options {
    libs: { "super::listener_types::*" },
    nt-value: default
}
"#;

mod options_2 {
    use lexi_gram::options::{Options, OptionsBuilder};
    use lexi_gram::{gencode, genspec};

    // static LEXICON_FILENAME: &str = "src/microcalc.l";
    // static GRAMMAR_FILENAME: &str = "src/microcalc.g";
    static LEXICON_GRAMMAR_FILENAME: &str = "src/microcalc.lg";
    static SOURCE_FILENAME: &str = "../microcalc/src/main.rs";
    static LEXER_TAG: &str = "microcalc_lexer";
    static PARSER_TAG: &str = "microcalc_parser";
    const LEXER_INDENT: usize = 4;
    const PARSER_INDENT: usize = 4;

    pub fn options() -> Options {
        OptionsBuilder::new()
            .combined_spec(genspec!(filename: LEXICON_GRAMMAR_FILENAME))
            .lexer_code(gencode!(filename: SOURCE_FILENAME, tag: LEXER_TAG))
            .indent(LEXER_INDENT)
            .parser_code(gencode!(filename: SOURCE_FILENAME, tag: PARSER_TAG))
            .indent(PARSER_INDENT)
            .libs(["super::listener_types::*"])
            .build()
            .expect("should have no error")
    }
}
// ---------------------------------------------------------
