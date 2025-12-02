// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::{Cursor, Read, Write};
use crate::{Gram, Lexi};
use crate::lexi::SymbolicDfa;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::lexergen::LexerGen;
use lexigram_lib::{BuildError, LL1};
use lexigram_lib::log::{BufLog, TryBuildFrom, TryBuildInto};
use lexigram_lib::parsergen::ParserGen;
use lexigram_lib::file_utils::{get_tagged_source, replace_tagged_source, SrcTagError};

/// Action performed by the source generator: generates the source or verifies it
/// (verification is only possible with some [CodeLocation] options).
#[derive(Clone, Debug)]
pub enum Action { Generate, Verify }

/// Location of the code to be generated or verified
#[derive(Clone, PartialEq, Debug)]
pub enum CodeLocation {
    /// * Generate mode: doesn't write anything.
    /// * Verify mode: returns `None`.
    None,
    /// * Generate mode: creates a new file or overwrites an existing one.
    /// * Verify mode: reads the expected code from an existing file.
    File { filename: String },
    /// * Generate mode: inserts the code between tags into an existing file.
    /// * Verify mode: reads the expected code between tags of an existing file.
    FileTag { filename: String, tag: String },
    /// * Generate mode: writes the code to stdout.
    /// * Verification mode: reads the expected code from stdin.
    StdIO,
}

impl CodeLocation {
    pub fn read(&self) -> Result<Option<String>, SrcTagError> {
        match self {
            CodeLocation::None => Ok(None),
            CodeLocation::File { filename } => Ok(std::fs::read_to_string(filename)?).map(|s| Some(s)),
            CodeLocation::FileTag { filename, tag } => get_tagged_source(&filename, &tag).map(|s| Some(s)),
            CodeLocation::StdIO => {
                let mut source = String::new();
                let size = std::io::stdin().read_to_string(&mut source)?;
                Ok(if size > 0 { Some(source) } else { None })
            }
        }
    }

    pub fn write(&self, source: &str) -> Result<(), SrcTagError> {
        match self {
            CodeLocation::None => Ok(()),
            CodeLocation::File { filename } => {
                Ok(std::fs::write(filename, source)?)
            }
            CodeLocation::FileTag { filename, tag } => replace_tagged_source(filename, tag, &source),
            CodeLocation::StdIO => {
                Ok(std::io::stdout().write_all(source.as_bytes())?)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Options {
    /// Action performed
    pub action: Action,
    /// Location of the generated/verified lexer code
    pub lexer_code: CodeLocation,
    /// Indentation of lexer source code
    pub lexer_indent: usize,
    /// Location of the generated/verified parser code
    pub parser_code: CodeLocation,
    /// Indentation of parser source code
    pub parser_indent: usize,
    /// Extra `use` libraries to include in the parser code (only if `parser_code` isn't `None`)
    pub extra_libs: Vec<String>,
    /// Includes the definitions of the alternatives in the parser, for debugging purposes
    pub gen_parser_alts: bool,
    /// Generates the wrapper, which is necessary to interface a listener (only if `parser_code` isn't `None`)
    pub gen_wrapper: bool,
    /// Generates the span parameters in the listener methods, to get the position of the terminals/nonterminals (only if `gen_wrapper` is `true`)
    pub gen_span_params: bool,
}

pub fn try_gen_source_code(lexicon: String, grammar_opt: Option<String>, options: &Options) -> Result<(String, Option<String>, BufLog), BuildError> {
    // 1. Lexer

    let lexicon_stream = CharReader::new(Cursor::new(lexicon));
    let lexi = Lexi::new(lexicon_stream);

    // - reads the lexicon and builds the DFA
    let SymbolicDfa { dfa, symbol_table } = lexi.try_build_into()?;

    // - builds the lexer
    let mut lexgen = LexerGen::try_build_from(dfa)?;
    lexgen.symbol_table = Some(symbol_table.clone());
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

/// Specification of the lexer or parser (lexicon or grammar content or location)
#[derive(Clone, Debug)]
pub enum Specification {
    /// No source
    None,
    /// Source is in a string
    String(String),
    /// Source is an existing file
    File { filename: String },
    /// Source is between tags in an existing file
    FileTag { filename: String, tag: String },
}

impl Specification {
    pub fn get(self) -> Result<Option<String>, SrcTagError> {
        match self {
            Specification::None => Ok(None),
            Specification::String(s) => Ok(Some(s)),
            Specification::File { filename } => Ok(std::fs::read_to_string(filename)?).map(|s| Some(s)),
            Specification::FileTag { filename, tag } => get_tagged_source(&filename, &tag).map(|s| Some(s)),
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum GenParserError {
    Source(SrcTagError),
    Build(BuildError),
    Mismatch(String),
    InvalidParameter(String),
}

impl From<SrcTagError> for GenParserError {
    fn from(e: SrcTagError) -> Self {
        GenParserError::Source(e)
    }
}

impl From<BuildError> for GenParserError {
    fn from(e: BuildError) -> Self {
        GenParserError::Build(e)
    }
}

impl Display for GenParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GenParserError::Source(s) => s.fmt(f),
            GenParserError::Build(b) => b.fmt(f),
            GenParserError::Mismatch(s) => write!(f, "mismatch when verifying source code: {s}"),
            GenParserError::InvalidParameter(s) => write!(f, "invalid parameters: {s}"),
        }
    }
}

impl Error for GenParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            GenParserError::Source(e) => Some(e),
            GenParserError::Build(b) => Some(b),
            GenParserError::Mismatch(_)
            | GenParserError::InvalidParameter(_) => None,
        }
    }
}

pub fn try_gen_parser(lexer_spec: Specification, parser_spec: Specification, action: Action, options: Options) -> Result<BufLog, GenParserError> {
    let lexicon_opt = lexer_spec.get()?;
    let Some(lexicon) = lexicon_opt else {
        return Err(GenParserError::InvalidParameter("cannot verify sources without any lexicon".to_string()))
    };
    let grammar_opt = parser_spec.get()?;
    let (lexer_source, parser_source_opt, log) = try_gen_source_code(lexicon, grammar_opt, &options)?;
    match action {
        Action::Verify => {
            if let Some(expected_lexer) = options.lexer_code.read()? {
                if lexer_source != expected_lexer {
                    return Err(GenParserError::Mismatch("lexer sources differ".to_string()))
                }
            }
            if let Some(expected_parser) = options.parser_code.read()? {
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
            options.lexer_code.write(&lexer_source)?;
            if let Some(parser_source) = parser_source_opt {
                options.parser_code.write(&parser_source)?;
            }
            Ok(log)
        }
    }
}
