// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Cursor;
use lexigram_lib::build::{BuildError, TryBuildFrom, TryBuildInto};
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::LL1;
use lexigram_lib::log::BufLog;
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::file_utils::SrcTagError;
use crate::{Gram, Lexi};
use crate::lexi::SymbolicDfa;
use crate::options::{Action, Options};

// ---------------------------------------------------------------------------------------------

/// Generates the source code for the lexer, the parser, and the wrapper / listener.
///
/// Notes:
/// * `options.lexer_spec` and `options.parser_spec`, which specify how to get the lexicon and the grammar, aren't used
/// by this function since they're given explicitly as string arguments.
/// * `options.lexer_code` and `options.parser_code`, which specify where to store the generated code, aren't used either,
/// since it's returned by the function as a string for the lexer and an optional string for the parser (if it must be generated).
pub fn try_gen_source_code(lexicon: String, grammar_opt: Option<String>, options: &Options) -> Result<(String, Option<String>, BufLog), BuildError> {
    // 1. Lexer

    let lexicon_stream = CharReader::new(Cursor::new(lexicon));
    let lexi = Lexi::new(lexicon_stream);

    // - reads the lexicon and builds the DFA
    let SymbolicDfa { dfa, symbol_table, .. } = lexi.try_build_into()?;

    // - builds the lexer
    let mut lexgen = LexerGen::try_build_from(dfa)?;
    lexgen.symbol_table = Some(symbol_table.clone());
    lexgen.extend_headers(&options.lexer_headers);
    lexgen.set_crate(options.lib_crate.clone());
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
        builder.set_crate(options.lib_crate.clone());
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

impl GenParserError {
    pub fn get_log(self) -> Option<BufLog> {
        if let GenParserError::Build(build_error, _) = self {
            Some(build_error.get_log())
        } else {
            None
        }
    }
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

/// Generates (or just verifies) the source code for the lexer, the parser, and the wrapper / listener.
///
/// The options given in argument include the location of the lexicon and grammar, and where the resulting
/// code should be written. See [Options](crate::options::Options) and
/// [OptionsBuilder](crate::options::OptionsBuilder) for further details.
pub fn try_gen_parser(action: Action, options: Options) -> Result<BufLog, GenParserError> {
    let lexer_spec_type = options.lexer_spec.get_type();
    let lexicon_opt = options.lexer_spec.clone().get()
        .map_err(|e| GenParserError::Source(e, format!("error while reading the lexicon ({lexer_spec_type})")))?;
    let Some(lexicon) = lexicon_opt else {
        return Err(GenParserError::InvalidParameter("cannot verify sources without any lexicon".to_string()))
    };
    let parser_spec_type = options.parser_spec.get_type();
    let grammar_opt = options.parser_spec.clone().get()
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

