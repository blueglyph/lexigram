// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Read;
use std::marker::PhantomData;
use lexigram::grammar::ProdRuleSet;
use lexigram::io::CharReader;
use lexigram::log::Logger;
use lexigram::lexer::{Lexer, TokenSpliterator};
use lexigram::LL1;
use lexigram::parser::{Parser, ParserError};
use lexigram::symbol_table::SymbolTable;
use crate::gramlexer::gramlexer::build_lexer;
use crate::gramparser::gramparser::{build_parser, Wrapper};
use crate::listener::GramListener;

pub struct Gram<T, R: Read> {
    pub gramlexer: Lexer<R>,
    pub gramparser: Parser,
    pub wrapper: Wrapper<GramListener>,
    _phantom: PhantomData<T>
}

#[allow(unused)]
impl<T, R: Read> Gram<T, R> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const VERBOSE_LISTENER: bool = false;

    pub fn new(symbol_table: SymbolTable) -> Self {
        let listener = GramListener::new(symbol_table);
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_mut_listener().set_verbose(Self::VERBOSE_LISTENER);
        let mut gramlexer = build_lexer();
        gramlexer.set_tab_width(4);
        Gram {
            gramlexer,
            gramparser: build_parser(),
            wrapper,
            _phantom: PhantomData
        }
    }

    pub fn get_mut_listener(&mut self) -> &mut GramListener {
        self.wrapper.get_mut_listener()
    }

    pub fn get_listener(&self) -> &GramListener {
        self.wrapper.get_listener()
    }

    pub fn build(&mut self, lexicon: CharReader<R>) -> Result<(), ParserError> {
        self.gramlexer.attach_stream(lexicon);
        let mut result_tokens = 0;
        let tokens = self.gramlexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
            panic!("no channel {ch} in this test, line {line} col {col}, \"{text}\"")
        ).inspect(|(tok, text, line, col)| {
            result_tokens += 1;
            if Self::VERBOSE_DETAILS {
                println!("TOKEN: line {line} col {col}, Id {tok:?}, \"{text}\"");
            }
        });
        let result = self.gramparser.parse_stream(&mut self.wrapper, tokens);
        result.and_then(|r| if self.wrapper.get_listener().get_log().num_errors() > 0 {
            // in case the parser hasn't reported any error but the listener has
            Err(ParserError::EncounteredErrors)
        } else {
            Ok(r)
        } )
    }
}

impl<R: Read> Gram<LL1, R> {
    pub fn build_ll1(mut self, lexicon: CharReader<R>) -> ProdRuleSet<LL1> {
        self.build(lexicon).expect("manage errors");
        let listener = self.wrapper.listener();
        let prs = listener.build_prs();
        prs.into()
    }
}
