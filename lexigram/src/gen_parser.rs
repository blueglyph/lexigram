// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Cursor;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::{BuildError, LL1};
use lexigram_lib::log::{BufLog, TryBuildFrom, TryBuildInto};
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::file_utils::SrcTagError;
use crate::{Gram, Lexi};
use crate::lexi::SymbolicDfa;
use crate::options::{Action, Options, Specification};

// ---------------------------------------------------------------------------------------------

pub fn try_gen_source_code(lexicon: String, grammar_opt: Option<String>, options: &Options) -> Result<(String, Option<String>, BufLog), BuildError> {
    // 1. Lexer

    let lexicon_stream = CharReader::new(Cursor::new(lexicon));
    let lexi = Lexi::new(lexicon_stream);

    // - reads the lexicon and builds the DFA
    let SymbolicDfa { dfa, symbol_table } = lexi.try_build_into()?;

    // - builds the lexer
    let mut lexgen = LexerGen::try_build_from(dfa)?;
    lexgen.symbol_table = Some(symbol_table.clone());
    lexgen.extend_headers(&options.lexer_headers);
    let (mut log, lexer_source) = lexgen.try_gen_source_code(options.lexer_indent)?;

    // // - writes the source code between existing tags:
    // replace_tagged_source(SOURCE_FILENAME, LEXER_TAG, &lexer_source)
    //     .expect(&format!("lexer source replacement failed; check that file {SOURCE_FILENAME} exists and has the tags {LEXER_TAG}"));

    // 2. Parser

    let parser_source = if let Some(grammar) = grammar_opt {
        // let file = File::open(GRAMMAR_FILENAME)
        //     .expect(&format!("couldn't open lexicon file {GRAMMAR_FILENAME}"));
        // let grammar_stream = CharReader::new(BufReader::new(file));
        let grammar_stream = CharReader::new(Cursor::new(grammar));

        // - parses the grammar
        let gram = Gram::new(symbol_table, grammar_stream);
        let ll1 = ProdRuleSet::<LL1>::try_build_from(gram)?;

        // - generates Lexi's parser source code (parser + listener):
        let mut builder = ParserGen::try_build_from(ll1)?;
        builder.set_parents_have_value();
        builder.set_include_alts(options.gen_parser_alts);
        builder.extend_headers(&options.parser_headers);
        builder.extend_libs(options.extra_libs.clone());
        builder.set_gen_span_params(options.gen_span_params);
        let (parser_log, parser_src) = builder.try_gen_source_code(options.parser_indent, options.gen_wrapper)?;
        log.extend(parser_log);
        Some(parser_src)
    } else {
        None
    };
    Ok((lexer_source, parser_source, log))
}

// ---------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum GenParserError {
    Source(SrcTagError, String),
    Build(BuildError, String),
    Mismatch(String),
    InvalidParameter(String),
}

impl Display for GenParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GenParserError::Source(s, context) => write!(f, "{context}: {s}"),
            GenParserError::Build(b, context) => write!(f, "{context}: {b}"),
            GenParserError::Mismatch(s) => write!(f, "mismatch when verifying source code: {s}"),
            GenParserError::InvalidParameter(s) => write!(f, "invalid parameters: {s}"),
        }
    }
}

impl Error for GenParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            GenParserError::Source(e, _) => Some(e),
            GenParserError::Build(b, _) => Some(b),
            GenParserError::Mismatch(_)
            | GenParserError::InvalidParameter(_) => None,
        }
    }
}

pub fn try_gen_parser(lexer_spec: Specification, parser_spec: Specification, action: Action, options: Options) -> Result<BufLog, GenParserError> {
    let lexer_spec_type = lexer_spec.get_type();
    let lexicon_opt = lexer_spec.get()
        .map_err(|e| GenParserError::Source(e, format!("error while reading the lexicon ({lexer_spec_type})")))?;
    let Some(lexicon) = lexicon_opt else {
        return Err(GenParserError::InvalidParameter("cannot verify sources without any lexicon".to_string()))
    };
    let parser_spec_type = parser_spec.get_type();
    let grammar_opt = parser_spec.get()
        .map_err(|e| GenParserError::Source(e, format!("error while reading the grammar ({parser_spec_type})")))?;
    let (lexer_source, parser_source_opt, log) = try_gen_source_code(lexicon, grammar_opt, &options)
        .map_err(|e| GenParserError::Build(e, "error while building the parser".to_string()))?;
    match action {
        Action::Verify => {
            if let Some(expected_lexer) = options.lexer_code.read()
                .map_err(|e| GenParserError::Source(e, format!("error while reading the expected lexer code ({})", options.lexer_code.get_type())))?
            {
                if lexer_source != expected_lexer {
                    return Err(GenParserError::Mismatch("lexer sources differ".to_string()))
                }
            }
            if let Some(expected_parser) = options.parser_code.read()
                .map_err(|e| GenParserError::Source(e, format!("error while reading the expected parser code ({})", options.parser_code.get_type())))?
            {
                if let Some(parser_source) = parser_source_opt {
                    if parser_source != expected_parser {
                        return Err(GenParserError::Mismatch("parser sources differ".to_string()));
                    }
                } else {
                    return Err(GenParserError::InvalidParameter("parser source code verification required but no grammar was provided".to_string()));
                }
            }
            Ok(log)
        }
        Action::Generate => {
            options.lexer_code.write(&lexer_source)
                .map_err(|e| GenParserError::Source(e, format!("error while writing the lexer code ({})", options.lexer_code.get_type())))?;
            if let Some(parser_source) = parser_source_opt {
                options.parser_code.write(&parser_source)
                    .map_err(|e| GenParserError::Source(e, format!("error while writing the parser code ({})", options.parser_code.get_type())))?;
            }
            Ok(log)
        }
    }
}

