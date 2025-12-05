// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::{Read, Write};
use lexigram_lib::file_utils::{get_tagged_source, replace_tagged_source, SrcTagError};

/// Action performed by the source generator: generates the source or verifies it
/// (verification is only possible with some [CodeLocation] options).
#[derive(Clone, PartialEq, Debug)]
pub enum Action { Generate, Verify }

/// Specification of the lexer or parser (lexicon or grammar content or location)
#[derive(Clone, PartialEq, Debug)]
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
    pub fn get_type(&self) -> String {
        match self {
            Specification::None => "no content".to_string(),
            Specification::String(_) => "String text".to_string(),
            Specification::File { filename } => format!("file '{filename}'"),
            Specification::FileTag { filename, tag } => format!("file '{filename}' / tag '{tag}'"),
        }
    }

    pub fn get(self) -> Result<Option<String>, SrcTagError> {
        match self {
            Specification::None => Ok(None),
            Specification::String(s) => Ok(Some(s)),
            Specification::File { filename } => Ok(std::fs::read_to_string(filename)?).map(|s| Some(s)),
            Specification::FileTag { filename, tag } => get_tagged_source(&filename, &tag).map(|s| Some(s)),
        }
    }
}

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
    pub fn get_type(&self) -> String {
        match self {
            CodeLocation::None => "no content".to_string(),
            CodeLocation::File { filename } => format!("file '{filename}'"),
            CodeLocation::FileTag { filename, tag } => format!("file '{filename}' / tag '{tag}'"),
            CodeLocation::StdIO => "stdin/stdout".to_string(),
        }
    }

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

// ---------------------------------------------------------------------------------------------

/// Options used to generate the source code of the lexer, parser, wrapper, and listener from a lexicon and a grammar
/// (the grammar is optional if only the lexer must be generated).
///
/// See [OptionsBuilder] for the accompanying builder.
#[derive(Clone, PartialEq, Debug)]
pub struct Options {
    /// Specification of the lexer (lexicon)
    pub lexer_spec: Specification,
    /// Location of the generated/verified lexer code
    pub lexer_code: CodeLocation,
    /// Indentation of lexer source code
    pub lexer_indent: usize,
    /// Specification of the parser (grammar)
    pub parser_spec: Specification,
    /// Location of the generated/verified parser code
    pub parser_code: CodeLocation,
    /// Indentation of parser source code
    pub parser_indent: usize,
    /// Extra headers before the lexer code
    pub lexer_headers: Vec<String>,
    /// Extra headers before the parser code
    pub parser_headers: Vec<String>,
    /// Extra `use` libraries to include in the parser code (only if `parser_code` isn't `None`)
    pub extra_libs: Vec<String>,
    /// Includes the definitions of the alternatives in the parser, for debugging purposes
    pub gen_parser_alts: bool,
    /// Generates the wrapper, which is necessary to interface a listener (only if `parser_code` isn't `None`)
    pub gen_wrapper: bool,
    /// Generates the span parameters in the listener methods, to get the position of the terminals/nonterminals (only if `gen_wrapper` is `true`)
    pub gen_span_params: bool,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            lexer_spec: Specification::None,
            lexer_code: CodeLocation::None,
            lexer_indent: 0,
            parser_spec: Specification::None,
            parser_code: CodeLocation::None,
            parser_indent: 0,
            lexer_headers: vec![],
            parser_headers: vec![],
            extra_libs: vec![],
            gen_parser_alts: true,
            gen_wrapper: true,
            gen_span_params: true,
        }
    }
}

enum BuilderState { Start, Lexer, Parser }

/// Builder of the [Options] object.
///
/// There are 3 types of options:
/// * options related to the lexer: [lexer](OptionsBuilder::lexer), [indent](OptionsBuilder::indent), [headers](OptionsBuilder::headers)
/// * options related to the parser: [parser](OptionsBuilder::parser), [indent](OptionsBuilder::indent), [headers](OptionsBuilder::headers)
/// * general options: [extra_libs](OptionsBuilder::extra_libs), [parser_alts](OptionsBuilder::parser_alts),
/// [wrapper](OptionsBuilder::wrapper), [span_params](OptionsBuilder::span_params)
///
/// Initially, the default option settings corresponds to [Object]'s defaults. The builder offers a convenient way to chain method
/// in order to set custom options.
///
/// The builder offers a way to use some of those options differently, depending on their position in the method chain.
/// The options [indent](OptionsBuilder::indent) and [headers](OptionsBuilder::headers) are:
/// * applied to both the lexer and parser if placed before [lexer](OptionsBuilder::lexer) and [parser](OptionsBuilder::parser)
/// * applied to the lexer if placed after [lexer](OptionsBuilder::lexer) and before [parser](OptionsBuilder::parser)
/// * applied to the parser if placed after [parser](OptionsBuilder::parser).
///
/// The option [parser](OptionsBuilder::parser) mustn't be used before [lexer](OptionsBuilder::lexer).
///
/// The [build](OptionsBuilder::build) method creates the resulting [Options] object. It doesn't check the object is properly set, however,
/// since the user may want to modify it later. For instance, it doesn't verify that the lexer is defined as something else than 'none'.
/// The current configuration is cleared after that point, and a new object can be created again with the same builder.
///
/// The [options](OptionsBuilder::options) method moves the builder to create the resulting [Options] object, so it can't be reused
/// (unless cloned before callind the method).
pub struct OptionsBuilder {
    options: Options,
    state: BuilderState,
}

impl OptionsBuilder {
    /// Creates a builder for [Options]
    pub fn new() -> Self {
        OptionsBuilder { state: BuilderState::Start, options: Options::default() }
    }

    /// Sets the location of the lexer's lexicon specification and generated code (default is none for both)
    pub fn lexer(&mut self, lexer_spec: Specification, lexer_code: CodeLocation) -> &mut Self {
        match self.state {
            BuilderState::Start => {
                self.state = BuilderState::Lexer;
                self.options.lexer_spec = lexer_spec;
                self.options.lexer_code = lexer_code;
            }
            BuilderState::Lexer => panic!("lexer() called twice"),
            BuilderState::Parser => panic!("lexer() called after parser()"),
        }
        self
    }

    /// Sets the location the parser's grammar specification and generated code (default is none for both)
    pub fn parser(&mut self, parser_spec: Specification, parser_code: CodeLocation) -> &mut Self {
        match self.state {
            BuilderState::Start | BuilderState::Lexer => {
                self.state = BuilderState::Parser;
                self.options.parser_spec = parser_spec;
                self.options.parser_code = parser_code;
            }
            BuilderState::Parser => panic!("parser() called twice"),
        }
        self
    }

    /// Sets the indentation of the generated code, in number of space characters (default is 0)
    pub fn indent(&mut self, indent: usize) -> &mut Self {
        match self.state {
            BuilderState::Start => {
                self.options.lexer_indent = indent;
                self.options.parser_indent = indent;
            }
            BuilderState::Lexer => self.options.lexer_indent = indent,
            BuilderState::Parser => self.options.parser_indent = indent,
        }
        self
    }

    /// **Adds** optional headers, which will be placed in front of the code (even before the `use`
    /// clauses). This can be used to place inner attributes like `#![allow(unused)]` or `#![cfg(...)]`.
    ///
    /// This method can be called several times to add more headers.
    pub fn headers<I: IntoIterator<Item=T>, T: Into<String>>(&mut self, headers: I) -> &mut Self {
        let hdr: Vec<String> = headers.into_iter().map(|s| s.into()).collect();
        match self.state {
            BuilderState::Start => {
                self.options.lexer_headers.extend(hdr.clone());
                self.options.parser_headers.extend(hdr);
            }
            BuilderState::Lexer => self.options.lexer_headers.extend(hdr),
            BuilderState::Parser => self.options.parser_headers.extend(hdr),
        }
        self
    }

    /// **Adds** user crates and modules to the list of `use` dependencies for the parser / wrapper.
    /// This can be used to define the user types needed in the wrapper / listener
    /// (those types can be initially copied from the generated code; they're commented out near the
    /// beginning, after the context type definitions).
    ///
    /// This method can be called several times to add more dependencies.
    pub fn extra_libs<I: IntoIterator<Item=T>, T: Into<String>>(&mut self, libs: I) -> &mut Self {
        self.options.extra_libs.extend(libs.into_iter().map(|s| s.into()));
        self
    }

    /// Sets the boolean option that generates more explicit messages in the parser when a parsing error
    /// is encountered. It requires to generate additional information.
    ///
    /// Default: `true`
    pub fn parser_alts(&mut self, parser_alts: bool) -> &mut Self {
        self.options.gen_parser_alts = parser_alts;
        self
    }

    /// Sets the boolean option that generates the wrapper.
    ///
    /// Default: `true`
    pub fn wrapper(&mut self, wrapper: bool) -> &mut Self {
        self.options.gen_wrapper = wrapper;
        self
    }

    /// Sets the boolean option that generates the extra `span` parameters in the listener callback methods.
    /// These parameters locate the terminals and nonterminals in the source text, so they can be used
    /// for instance to generate report messages with the precise location of symbols that caused an
    /// error.
    ///
    /// Default: `true`
    pub fn span_params(&mut self, span_params: bool) -> &mut Self {
        self.options.gen_span_params = span_params;
        self
    }

    /// Creates an [Options] object with the current options defined earlier by [OptionsBuilder]'s methods.
    ///
    /// **The builder resets the options to their default values** after creating and returning that object,
    /// so it can be reused to generate other [Options] objects, but the options must be set again.
    /// If you want the builder to keep the options, consider cloning it and using [options()](OptionsBuilder::options)
    /// instead.
    pub fn build(&mut self) -> Options {
        self.state = BuilderState::Start;
        std::mem::take(&mut self.options)
    }

    /// Creates an [Options] object with the current options defined earlier by [OptionsBuilder]'s methods.
    ///
    /// This method moves the builder. If you want to reuse the builder, consider cloning it (if you want
    /// to keep the same options) or using [build()](OptionsBuilder::build) instead.
    ///
    /// **Important note**: once [lexer](OptionsBuilder::lexer) has been called, it's not possible to call
    /// it again. Similarly, once [parser](OptionsBuilder::parser) has been called, it's not possible to
    /// call it or [lexer](OptionsBuilder::lexer) again (see the [Options] doc for further details).
    /// It means that **if you clone the builder and generate an option object with this method, the
    /// remaining builder will only be partially reconfigurable**.
    pub fn options(self) -> Options {
        self.options
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates a [Specification](crate::gen_parser::Specification) object:
    /// ```ignore
    /// genspec!(none)
    /// genspec!(string: expr)
    /// genspec!(filename: expr)
    /// genspec!(filename: expr, tag: expr)
    /// ```
    /// where `expr.to_string()` are valid strings
    #[macro_export()]
    macro_rules! genspec {
        (none) => {
            $crate::options::Specification::None
        };
        (string: $string: expr) => {
            $crate::options::Specification::String($string.to_string())
        };
        (filename: $file: expr) => {
            $crate::options::Specification::File { filename: $file.to_string() }
        };
        (filename: $file: expr, tag: $tag: expr) => {
            $crate::options::Specification::FileTag { filename: $file.to_string(), tag: $tag.to_string() }
        };
    }

    /// Generates a [CodeLocation](crate::gen_parser::CodeLocation) object:
    /// ```ignore
    /// gencode!(none)
    /// gencode!(string: expr)
    /// gencode!(filename: expr)
    /// gencode!(filename: expr, tag: expr)
    /// ```
    /// where `expr.to_string()` are valid strings
    #[macro_export()]
    macro_rules! gencode {
        (none) => {
            $crate::options::CodeLocation::None
        };
        (filename: $file: expr) => {
            $crate::options::CodeLocation::File { filename: $file.to_string() }
        };
        (filename: $file: expr, tag: $tag: expr) => {
            $crate::options::CodeLocation::FileTag { filename: $file.to_string(), tag: $tag.to_string() }
        };
        (stdio) => {
            $crate::options::CodeLocation::StdIO
        };
    }
}
