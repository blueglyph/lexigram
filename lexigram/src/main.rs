// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexi_gram::gen_parser::{try_gen_parser, GenParserError};
use lexi_gram::{lexigram_lib, LEXIGRAM_PKG_VERSION, LEXIGRAM_PKG_NAME};
use lexigram_lib::{LIB_PKG_VERSION, LIB_PKG_NAME};
use lexigram_lib::lexigram_core::{CORE_PKG_VERSION, CORE_PKG_NAME};
use lexigram_lib::log::LogStatus;
use crate::arg_opt::{parse_args, ArgOptions, HELP_MESSAGE};

mod arg_opt;

// package name & version
pub const BIN_PKG_NAME: &str = env!("CARGO_PKG_NAME");
pub const BIN_PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Error returned by [execute].
pub enum ExeError {
    /// The "help" option was met; the [help message](HELP_MESSAGE) should be displayed.
    Help,
    /// The "version" option was met; the versions of the executable and the other
    /// crates it depends on should be displayed.
    Version,
    /// An unknown command-line option was given.
    Option(String),
    /// The source code generator produced an error. The original error is
    /// in the `source` field.
    ///
    /// The `show_log` field determines whether the "log" option was desired or not.
    GenParser { source: GenParserError, show_log: bool },
}

fn main() {
    let all_args: Vec<String> = std::env::args().skip(1).collect();
    let code = match execute(all_args) {
        Ok(message_opt) => {
            if let Some(message) = message_opt {
                println!("{message}");
            }
            eprintln!("OK");
            0
        }
        Err(e) => {
            match e {
                ExeError::Help => {
                    eprintln!("{HELP_MESSAGE}");
                    1
                }
                ExeError::Version => {
                    eprintln!("Versions:\n- {BIN_PKG_NAME}: {BIN_PKG_VERSION}");
                    eprintln!("- {CORE_PKG_NAME}: {CORE_PKG_VERSION}");
                    eprintln!("- {LIB_PKG_NAME}: {LIB_PKG_VERSION}");
                    eprintln!("- {LEXIGRAM_PKG_NAME}: {LEXIGRAM_PKG_VERSION}");
                    1
                }
                ExeError::Option(msg) => {
                    eprintln!("Error while parsing the command arguments: {msg}");
                    2
                }
                ExeError::GenParser { source, show_log } => {
                    eprintln!("Generator error:\n{source}");
                    if let Some(log) = source.get_log() {
                        if show_log {
                            eprintln!("{log}");
                        }
                        if log.num_errors() > 0 { eprintln!("{} error(s)", log.num_errors()) }
                        if log.num_warnings() > 0 { eprintln!("{} error(s)", log.num_warnings()) }
                    }
                    3
                }
            }
        }
    };
    std::process::exit(code);
}

/// Parses the command-line arguments in `all_args` and executes the corresponding actions.
pub fn execute(all_args: Vec<String>) -> Result<Option<String>, ExeError> {
    let (action, arg_options) = parse_args(all_args)?;
    let ArgOptions { gen_options, show_log } = arg_options;
    match try_gen_parser(action, gen_options) {
        Ok(log) => {
            Ok(if show_log { Some(log.to_string()) } else { None })
        }
        Err(source) => {
            Err(ExeError::GenParser { source, show_log })
        }
    }
}