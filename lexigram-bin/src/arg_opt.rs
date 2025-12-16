// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::iter::Peekable;
use lexigram::{gencode, genspec};
use lexigram::options::{Action, CodeLocation, Options, OptionsBuilder, Specification};
use crate::ExeError;

/// Command-line arguments
pub(crate) static HELP_MESSAGE: &str = r##"Usage:    lexigram [options]

Main options. The lexer options must be placed before any parser option.

  -l|--lexer <location>     Location of the generated lexer code, where <location> is
                                <location> = <filename> | <filename> tag <tag> | -
                            "-" instead of a filename outputs the code to stdout.
                            This option is mandatory.

                            When a <tag> is used, the file must already exist and the code
                            will be located between surrounding "[<tag>]", with one empty
                            line of separation between the tag and the code. When the code
                            hasn't been generated yet, lexigram expects to find two tags
                            separated by a couple of empty lines.

                            Examples:
                            --lexer lexer.rs             -> file "lexer.rs" will be created
                            --lexer lexer.rs tag LEXER   -> between "[LEXER]" tags in
                                                            existing file "lexer.rs"
                            --lexer -                    -> code will be output to stdout

  -l|--lexicon <location>   Location of the lexicon, where <location> is the same as for
                            --lexer, exept "-" isn't valid.
                            This option is mandatory.

  -p|--parser <location>    Location of the generated parser code, where <location> is the same
                            as for --lexer. If the --parser and --grammar options aren't
                            present, the parser code isn't generated.

  -g|--grammar <location>   Location of the grammar, where <location> is the same as for
                            --lexer, except "-" isn't valid. This option is mandatory if
                            --parser is set.

Secondary lexer / parser options. Those options can be set before --lexer and --parser if they
apply to both, or after either of them if they only apply to the lexer or the parser:

  --header <string>         Adds a header in front of the generated code.

                            Example: --header "#[cfg(feature = \"parser\")]"

  --indent <number>         Defines the code indentation in number of spaces (default: 0).

Other options related to the generated code:

  --lib <string>            Adds extra libs (crates/modules) to the "use" declarations of the
                            parser / wrapper / listener code.
                            This option can be used multiple times.

                            Example: --lib "super::listener_types::*"

  --spans                   Adds parameters to the listener method that give the locations of
                            the terminals and nonterminals of each rule alternative in the
                            parsed text.

  --no-wrapper              Generates only a parser; doesn't generate the code for the
                            wrapper and the listener.

  --debug-info              Adds extra info in the parser to generate clearer debug messages.

  --use-full-lib            Uses the full crate lexigram_lib in the generated code instead
                            of the smaller lexigram_core with the minimal features required
                            by the lexer and parser. The full crate includes all the code
                            generation features, so it's normally not necessary.

General options:

  -v|--verify               Verifies that the generated code matches what is already in the
                            lexer and parser locations. The files must already exist and
                            aren't modified.

  --log                     Shows the log.

  -h|--help                 Shows this message.

  -V|--version              Shows the program version.

Example:

  lexigram -x lexicon.l -l lexer.rs -g grammar.g -p parser.rs --lib "super::listener_types::*" --spans --log
"##;

#[derive(Clone, Debug)]
pub(crate) struct ArgOptions {
    pub gen_options: Options,
    pub show_log: bool,
}

fn take_argument<'a, I: Iterator<Item=&'a str>, S: Into<String>>(args: &mut I, error_message: S) -> Result<&'a str, ExeError> {
    let Some(value) = args.next() else {
        return Err(ExeError::Option(error_message.into()));
    };
    Ok(value)
}

fn get_code<'a, I: Iterator<Item=&'a str>>(label: &str, args: &mut Peekable<I>) -> Result<CodeLocation, ExeError> {
    let Some(filename) = args.next() else {
        return Err(ExeError::Option(format!("missing filename after --{label}")));
    };
    let code = if matches!(args.peek(), Some(&"tag")) {
        if filename == "-" {
            return Err(ExeError::Option(format!("- (stdout) cannot be used with a tag as {label} code location")))
        }
        args.next();
        let Some(tag) = args.next() else {
            return Err(ExeError::Option(format!("missing tag name in {label} code location")));
        };
        gencode!(filename: filename, tag: tag)
    } else {
        if filename == "-" {
            gencode!(stdout)
        } else {
            gencode!(filename: filename)
        }
    };
    Ok(code)
}

fn get_spec<'a, I: Iterator<Item=&'a str>>(label: &str, spec_name: &str, args: &mut Peekable<I>) -> Result<Specification, ExeError> {
    let filename = take_argument(args, format!("missing filename after --{label}"))?;
    if filename == "-" {
        return Err(ExeError::Option(format!("- (stdout) is not a valid {spec_name} source for {label}")));
    }
    let code = if matches!(args.peek(), Some(&"tag")) {
        if filename == "-" {
            return Err(ExeError::Option(format!("- (stdout) cannot be used with a tag as {label} {spec_name} location")))
        }
        args.next();
        let tag = take_argument(args, format!("missing tag name in {label} {spec_name} location"))?;
        genspec!(filename: filename, tag: tag)
    } else {
        genspec!(filename: filename)
    };
    Ok(code)
}

pub(crate) fn parse_args(all_args: Vec<String>) -> Result<(Action, ArgOptions), ExeError> {
    let mut builder = OptionsBuilder::new();
    let mut action = Action::Generate;
    let mut show_log = false;
    let mut args = all_args.iter().map(|s| s.as_str()).peekable();
    while let Some(arg) = args.next() {
        match arg {
            "-h" | "--help" => {
                return Err(ExeError::Help); // not a real error
            }
            "-l" | "--lexer" => {
                builder.lexer_code(get_code("lexer", &mut args)?);
            },
            "-x" | "--lexicon" => {
                builder.lexer_spec(get_spec("lexer", "lexicon", &mut args)?);
            }
            "-p" | "--parser" => {
                builder.parser_code(get_code("parser", &mut args)?);
            }
            "-g" | "--grammar" => {
                builder.parser_spec(get_spec("parser", "grammar", &mut args)?);
            }
            "--header" => {
                let header = take_argument(&mut args, "missing argument after --header")?;
                builder.headers([header]);
            }
            "--indent" => {
                let indent = take_argument(&mut args, "missing argument after --indent")?;
                let indent_value = usize::from_str_radix(indent, 10)
                    .map_err(|e| ExeError::Option(format!("error while parsing --indent {indent}: {e}")))?;
                builder.indent(indent_value);
            }
            "-v" | "--verify" => {
                action = Action::Verify;
            }
            "--lib" => {
                let lib = take_argument(&mut args, "missing argument after --lib")?;
                builder.extra_libs([lib]);
            }
            "--spans" => {
                builder.span_params(true);
            }
            "--no-wrapper" => {
                builder.wrapper(false);
            }
            "--debug-info" => {
                builder.parser_alts(true);
            }
            "--use-full-lib" => {
                builder.use_full_lib(true);
            }
            "--log" => {
                show_log = true;
            }
            "-V" | "--version" => {
                return Err(ExeError::Version); // not a real error
            }
            s => {
                return Err(ExeError::Option(format!("ERROR: unexpected argument '{s}'")));
            }
        }
    }
    let gen_options = builder.build()
        .map_err(|e| ExeError::Option(e))?;
    let arg_options = ArgOptions {
        gen_options,
        show_log,
    } ;
    Ok((action, arg_options))
}