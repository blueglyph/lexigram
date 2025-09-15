// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Read;
use lexigram_lib::dfa::Dfa;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexer::{Lexer, TokenSpliterator};
use lexigram_lib::log::{BufLog, BuildFrom, BuildInto, Logger, LogReader, LogStatus, TryBuildFrom};
use lexigram_lib::parser::Parser;
use lexigram_lib::{BuildError, BuildErrorSource, HasBuildErrorSource, Normalized, SymbolTable};
use lexilexer::build_lexer;
use lexiparser::{build_parser, Wrapper};
use listener::LexiListener;
use crate::lexi::lexiparser::LexiParserListener;

mod lexilexer;
mod lexiparser;
mod listener;
mod tests;

pub struct SymbolicDfa {
    pub dfa: Dfa<Normalized>,
    pub symbol_table: SymbolTable
}

pub struct Lexi<'a, 'b, R: Read> {
    pub lexilexer: Lexer<'a, R>,
    pub lexiparser: Parser<'b>,
    wrapper: Wrapper<LexiListener>,
    is_built: bool
}

impl<R: Read> Lexi<'_, '_, R> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const VERBOSE_LISTENER: bool = false;

    pub fn new(lexicon: CharReader<R>) -> Self {
        let listener = LexiListener::new();
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_listener_mut().set_verbose(Self::VERBOSE_LISTENER);
        let mut lexilexer = build_lexer();
        lexilexer.set_tab_width(4);
        lexilexer.attach_stream(lexicon);
        Lexi {
            lexilexer,
            lexiparser: build_parser(),
            wrapper,
            is_built: false
        }
    }

    pub fn get_listener_mut(&mut self) -> &mut LexiListener {
        self.wrapper.get_listener_mut()
    }

    pub fn get_listener(&self) -> &LexiListener {
        self.wrapper.get_listener()
    }

    fn make(&mut self) {
        if !self.is_built {
            // we keep track of the built state because some unit tests are calling build() directly
            self.is_built = true;
            let tokens = self.lexilexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
                panic!("unexpected channel {ch} from Lexi while parsing a lexicon, line {line} col {col}, \"{text}\"")
            ).inspect(|(tok, text, line, col)| {
                if Self::VERBOSE_DETAILS {
                    println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\"");
                }
            });
            if self.lexiparser.parse_stream(&mut self.wrapper, tokens).is_ok() {
                for s in self.get_listener_mut().rules_to_vecstrings() {
                    self.get_listener_mut().get_mut_log().add_note(s);
                }
            }
        }
    }
}

impl<R: Read> LogReader for Lexi<'_, '_, R> {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        self.get_listener().get_log()
    }

    fn give_log(self) -> Self::Item {
        let listener = self.wrapper.give_listener();
        listener.give_log()
    }
}

impl<R: Read> HasBuildErrorSource for Lexi<'_, '_, R> {
    const SOURCE: BuildErrorSource = BuildErrorSource::Lexi;
}

impl<R: Read> BuildFrom<Lexi<'_, '_, R>> for SymbolicDfa {
    fn build_from(mut lexi: Lexi<R>) -> Self {
        lexi.make();
        let listener = lexi.wrapper.give_listener();
        let symbol_table = listener.make_symbol_table();
        SymbolicDfa {
            dfa: listener.build_into(),
            symbol_table
        }
    }
}

impl<R: Read> TryBuildFrom<Lexi<'_, '_, R>> for SymbolicDfa {
    type Error = BuildError;

    fn try_build_from(source: Lexi<'_, '_, R>) -> Result<Self, Self::Error> {
        if source.get_log().has_no_errors() {
            let symbolic_dfa = SymbolicDfa::build_from(source);
            if symbolic_dfa.dfa.get_log().has_no_errors() {
                Ok(symbolic_dfa)
            } else {
                Err(BuildError::new(symbolic_dfa.dfa.give_log(), BuildErrorSource::Lexi))
            }
        } else {
            Err(BuildError::new(source.give_log(), BuildErrorSource::Lexi))
        }
    }
}
