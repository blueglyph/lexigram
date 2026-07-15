// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// =============================================================================================
// Generates the source of the pandemonium parser

use std::error::Error;
use std::fmt::{Display, Formatter};
use lexigram_lib::build::{BuildError, BuildErrorSource, TryBuildFrom, TryBuildInto};
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::lexergen::{LexerGen, LexerGenOptions};
use lexigram_lib::{file_utils, General, LL1, LR};
use lexigram_lib::log::{BufLog, LogReader, Logger};
use lexigram_lib::parsergen::{ParserGen, ParserType};
use lexigram_lib::file_utils::{DiffResult, SrcTagError};
use lexigram_lib::lexer::Pos;
use crate::{Gram, Lexi};
use crate::lexi::SymbolicDfa;
use crate::options::{Action, Options};

// ---------------------------------------------------------------------------------------------

/// Generates the source code for the lexer, the parser, and the wrapper / listener. The latter is
/// an [Option] and includes three parts (3 formatted strings):
/// * the indented source of the wrapper
/// * the indented source of the template for the user types
/// * the indented source of the template for the listener implementation
///
/// Notes:
/// * `options.lexer_spec` and `options.parser_spec`, which specify how to get the lexicon and the grammar, aren't used
///   by this function since they're given explicitly as string arguments.
/// * `options.lexer_code` and `options.parser_code`, which specify where to store the generated code, aren't used either,
///   since it's returned by the function as a string for the lexer and an optional string for the parser (if it must be generated).
/// * if the lexicon and the grammar are combined in `lexicon`, the `grammar_opt` parameter must be `None`.
pub fn try_gen_source_code(lexicon: String, grammar_opt: Option<String>, options: &Options)
    -> Result<(String, Option<(String, String, String)>, BufLog), BuildError>
{
    // 1. Lexer

    let mut lexi = Lexi::new(lexicon.as_str());
    lexi.set_options(options.into());

    // - reads the lexicon and builds the DFA
    let SymbolicDfa { dfa, symbol_table, terminal_hooks, pos_grammar_opt } = lexi.try_build_into()?;

    // - builds the lexer
    let mut lexgen = LexerGen::try_build_from(dfa)?;
    lexgen.set_options(LexerGenOptions::from(options));
    lexgen.symbol_table = Some(symbol_table.clone());

    let is_combined = pos_grammar_opt.is_some();
    if pos_grammar_opt.is_some() && grammar_opt.is_some() {
        // conflict of two grammar locations
        let mut log = lexgen.give_log();
        log.add_error("conflict: grammar detected in lexicon and another grammar given explicitly".to_string());
        return Err(BuildError::new(log, BuildErrorSource::Lexigram));
    }

    // - writes the source code between existing tags:
    let (mut log, lexer_source) = lexgen.try_gen_source_code(options.lexer_indent)?;

    // 2. Parser

    let parser_sources = if grammar_opt.is_some() || is_combined {
        let (grammar, start_pos) = if let Some(g) = grammar_opt.as_deref() {
            // grammar not combined, starting at position (1, 1)
            (g, Pos(1, 1))
        } else {
            if let Some(pos_grammar) = pos_grammar_opt {
                // grammar combined with lexicon: will have to skip to the grammar position
                (lexicon.as_str(), pos_grammar)
            } else {
                log.add_error("no starting position with combined lexicon/grammar");
                return Err(BuildError::new(log, BuildErrorSource::Gram));
            }
        };

        // - parses the grammar
        let mut gram = Gram::new(symbol_table, grammar);
        gram.set_options(options.into());
        gram.set_start_nt(options.start_nt.clone());
        if let Err(s) = gram.gramlexer.skip_to_pos(start_pos) {
            log.add_error(s);
            return Err(BuildError::new(log, BuildErrorSource::Gram));
        }
        let general = ProdRuleSet::<General>::try_build_from(gram)?;
        let mut builder = match options.parser_type {
            ParserType::LL1 => {
                let ll1 = ProdRuleSet::<LL1>::try_build_from(general)?;
                let mut builder = ParserGen::build_from_rules_ll1(ll1);
                builder.set_terminal_hooks(terminal_hooks);
                builder
            }
            ParserType::LALR => {
                let mut lr = ProdRuleSet::<LR>::try_build_from(general)?;
                lr.set_terminal_hooks(terminal_hooks);
                ParserGen::build_lalr_from_rules_lr(lr)
            }
        };

        // - generates Lexi's parser source code (parser + listener):
        builder.set_options(options.into());
        let (parser_log, parser_src, types_src, listener_src) = builder.try_gen_source_code()?;
        log.extend(parser_log);
        Some((parser_src, types_src, listener_src))
    } else {
        None
    };
    Ok((lexer_source, parser_sources, log))
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
/// code should be written. See [Options] and
/// [OptionsBuilder](crate::options::OptionsBuilder) for further details.
pub fn try_gen_parser(action: Action, options: Options) -> Result<BufLog, GenParserError> {
    let is_combined = options.lexer_spec == options.parser_spec;
    let lexer_spec_type = options.lexer_spec.get_type();
    let lexicon_opt = options.lexer_spec.clone().get()
        .map_err(|e| GenParserError::Source(e, format!("error while reading the lexicon ({lexer_spec_type})")))?;
    let Some(lexicon) = lexicon_opt else {
        return Err(GenParserError::InvalidParameter("cannot verify sources without any lexicon".to_string()))
    };
    let parser_spec_type = options.parser_spec.get_type();
    let grammar_opt = if !is_combined {
        options.parser_spec.clone().get()
            .map_err(|e| GenParserError::Source(e, format!("error while reading the grammar ({parser_spec_type})")))?
    } else {
        None
    };
    let (lexer_source, parser_source_opt, log) = try_gen_source_code(lexicon, grammar_opt, &options)
        .map_err(|e| GenParserError::Build(e, "error while building the parser".to_string()))?;
    match action {
        Action::Verify => {
            if let Some(expected_lexer) = options.lexer_code.read()
                .map_err(|e| GenParserError::Source(e, format!("error while reading the expected lexer code ({})", options.lexer_code.get_type())))?
            {
                match file_utils::simple_diff(&lexer_source, &expected_lexer) {
                    DiffResult::Equal => {}
                    DiffResult::Mismatch { line_num, line1, line2 } => {
                        return Err(GenParserError::Mismatch(
                            format!("lexer sources differ, line {line_num}:\ngenerated: '{line1}'\nreference: '{line2}'")))
                    }
                }
            }
            if let Some(expected_parser) = options.parser_code.read()
                .map_err(|e| GenParserError::Source(e, format!("error while reading the expected parser code ({})", options.parser_code.get_type())))?
            {
                if let Some((parser_source, ..)) = &parser_source_opt {
                    match file_utils::simple_diff(parser_source, &expected_parser) {
                        DiffResult::Equal => {}
                        DiffResult::Mismatch { line_num, line1, line2 } => {
                            return Err(GenParserError::Mismatch(
                                format!("parser sources differ, line {line_num}:\ngenerated: '{line1}'\nreference: '{line2}'")))
                        }
                    }
                } else {
                    return Err(GenParserError::InvalidParameter("parser source code verification required but no grammar was provided".to_string()));
                }
            }
            if let Some(expected_parser) = options.types_code.read()
                .map_err(|e| GenParserError::Source(
                    e,
                    format!("error while reading the expected template code for user types ({})", options.types_code.get_type())))?
            {
                if let Some((_parser_source, types_source, _listener_source)) = &parser_source_opt {
                    match file_utils::simple_diff(types_source, &expected_parser) {
                        DiffResult::Equal => {}
                        DiffResult::Mismatch { line_num, line1, line2 } => {
                            return Err(GenParserError::Mismatch(
                                format!("template for user types differ, line {line_num}:\ngenerated: '{line1}'\nreference: '{line2}'")))
                        }
                    }
                } else {
                    return Err(GenParserError::InvalidParameter(
                        "verification required for the template of the user types but no grammar was provided".to_string()));
                }
            }
            if let Some(expected_parser) = options.listener_code.read()
                .map_err(|e| GenParserError::Source(
                    e,
                    format!("error while reading the expected template code of the listener implementation ({})", options.listener_code.get_type())))?
            {
                if let Some((_parser_source, _types_source, listener_source)) = &parser_source_opt {
                    match file_utils::simple_diff(listener_source, &expected_parser) {
                        DiffResult::Equal => {}
                        DiffResult::Mismatch { line_num, line1, line2 } => {
                            return Err(GenParserError::Mismatch(
                                format!("template for user types differ, line {line_num}:\ngenerated: '{line1}'\nreference: '{line2}'")))
                        }
                    }
                } else {
                    return Err(GenParserError::InvalidParameter(
                        "verification required for the template code of the listener implementation but no grammar was provided".to_string()));
                }
            }
            Ok(log)
        }
        Action::Generate => {
            options.lexer_code.write(&lexer_source)
                .map_err(|e| GenParserError::Source(e, format!("error while writing the lexer code ({})", options.lexer_code.get_type())))?;
            if let Some((parser_source, types_source, listener_source)) = parser_source_opt {
                options.parser_code.write(&parser_source)
                    .map_err(|e| GenParserError::Source(
                        e,
                        format!("error while writing the parser code ({})", options.parser_code.get_type())))?;
                options.types_code.write(&types_source)
                    .map_err(|e| GenParserError::Source(
                        e,
                        format!("error while writing the template for the user types ({})", options.types_code.get_type())))?;
                options.listener_code.write(&listener_source)
                    .map_err(|e| GenParserError::Source(
                        e,
                        format!("error while writing the template for the listener implementation ({})", options.listener_code.get_type())))?;
            }
            Ok(log)
        }
    }
}

