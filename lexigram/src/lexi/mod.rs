// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Read;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexer::{Lexer, TokenSpliterator};
use lexigram_lib::log::Logger;
use lexigram_lib::parser::{Parser, ParserError};
use lexilexer::build_lexer;
use lexiparser::{build_parser, Wrapper};
use listener::LexiListener;

mod lexilexer;
mod lexiparser;
mod listener;
mod tests;

pub struct Lexi<'a, 'b, R: Read> {
    pub lexilexer: Lexer<'a, R>,
    pub lexiparser: Parser<'b>,
    pub wrapper: Wrapper<LexiListener>
}

impl<R: Read> Lexi<'_, '_, R> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const VERBOSE_LISTENER: bool = false;

    pub fn new() -> Self {
        let listener = LexiListener::new();
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_mut_listener().set_verbose(Self::VERBOSE_LISTENER);
        let mut lexilexer = build_lexer();
        lexilexer.set_tab_width(4);
        Lexi {
            lexilexer,
            lexiparser: build_parser(),
            wrapper
        }
    }

    pub fn get_mut_listener(&mut self) -> &mut LexiListener {
        self.wrapper.get_mut_listener()
    }

    pub fn get_listener(&self) -> &LexiListener {
        self.wrapper.get_listener()
    }

    pub fn build(&mut self, lexicon: CharReader<R>) -> Result<(), ParserError> {
        self.lexilexer.attach_stream(lexicon);
        let mut result_tokens = 0;
        let tokens = self.lexilexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
            panic!("no channel {ch} in this test, line {line} col {col}, \"{text}\"")
        ).inspect(|(tok, text, line, col)| {
            result_tokens += 1;
            if Self::VERBOSE_DETAILS {
                println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\"");
            }
        });
        let result = self.lexiparser.parse_stream(&mut self.wrapper, tokens);
        if Self::VERBOSE_DETAILS {
            println!("Lexilexer: {:?}", self.lexilexer.get_error());
        }
        result.and_then(|r| if self.wrapper.get_listener().get_log().num_errors() > 0 {
            // in case the parser hasn't reported any error but the listener has
            Err(ParserError::EncounteredErrors)
        } else {
            Ok(r)
        } )
    }
}