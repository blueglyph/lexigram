// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram::gen_parser::{try_gen_parser, GenParserError};
use lexigram_core::log::LogStatus;
use crate::arg_opt::{parse_args, ArgOptions, HELP_MESSAGE};

mod arg_opt;

enum ExeError {
    Help,
    Version,
    Option(String),
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
                    let version = env!("CARGO_PKG_VERSION");
                    eprintln!("lexigram version {version}");
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

fn execute(all_args: Vec<String>) -> Result<Option<String>, ExeError> {
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