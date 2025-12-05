// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::{Read, Write};
use lexigram_lib::file_utils::{get_tagged_source, replace_tagged_source, SrcTagError};

/// Action performed by the source generator: generates the source or verifies it
/// (verification is only possible with some [CodeLocation] options).
#[derive(Clone, PartialEq, Debug)]
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

// ---------------------------------------------------------------------------------------------

#[derive(Clone, PartialEq, Debug)]
pub struct Options {
    /// Location of the generated/verified lexer code
    pub lexer_code: CodeLocation,
    /// Indentation of lexer source code
    pub lexer_indent: usize,
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
            lexer_code: CodeLocation::None,
            lexer_indent: 0,
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

pub struct OptionsBuilder {
    options: Options,
    state: BuilderState,
}

impl OptionsBuilder {
    pub fn new() -> Self {
        OptionsBuilder { state: BuilderState::Start, options: Options::default() }
    }

    pub fn lexer(&mut self, lexer_code: CodeLocation) -> &mut Self {
        self.state = BuilderState::Lexer;
        self.options.lexer_code = lexer_code;
        self
    }

    pub fn parser(&mut self, parser_code: CodeLocation) -> &mut Self {
        self.state = BuilderState::Parser;
        self.options.parser_code = parser_code;
        self
    }

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

    pub fn extra_libs<I: IntoIterator<Item=T>, T: Into<String>>(&mut self, libs: I) -> &mut Self {
        self.options.extra_libs.extend(libs.into_iter().map(|s| s.into()));
        self
    }

    pub fn parser_alts(&mut self, parser_alts: bool) -> &mut Self {
        self.options.gen_parser_alts = parser_alts;
        self
    }

    pub fn wrapper(&mut self, wrapper: bool) -> &mut Self {
        self.options.gen_wrapper = wrapper;
        self
    }

    pub fn span_params(&mut self, span_params: bool) -> &mut Self {
        self.options.gen_span_params = span_params;
        self
    }

    pub fn build(&mut self) -> Options {
        std::mem::take(&mut self.options)
    }

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