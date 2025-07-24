// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use gramlexer::build_lexer;
use gramparser::{build_parser, GramParserListener, Wrapper};
use listener::GramListener;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::io::CharReader;
use lexigram_lib::lexer::{Lexer, TokenSpliterator};
use lexigram_lib::log::Logger;
use lexigram_lib::parser::Parser;
use lexigram_lib::{SymbolTable, LL1};
use std::io::Read;
use std::marker::PhantomData;

mod gramlexer;
mod gramparser;
mod listener;
mod tests;

pub struct Gram<'a, 'b, T, R: Read> {
    pub gramlexer: Lexer<'a, R>,
    pub gramparser: Parser<'b>,
    pub wrapper: Wrapper<GramListener>,
    _phantom: PhantomData<T>
}

#[allow(unused)]
impl<T, R: Read> Gram<'_, '_, T, R> {
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

    pub fn build(&mut self, grammar: CharReader<R>) {
        let mut channel_errors = vec![];
        self.gramlexer.attach_stream(grammar);
        let tokens = self.gramlexer.tokens().split_channel0(|(_tok, ch, text, line, col)|
            if channel_errors.len() < 5 {
                channel_errors.push(format!("unexpected channel {ch} from lexer, line {line} col {col}, \"{text}\""));
            }
        );
        if let Err(e) = self.gramparser.parse_stream(&mut self.wrapper, tokens) {
            self.get_mut_listener().get_mut_log().add_error(e.to_string());
        }
        for e in channel_errors {
            // we can't report them earlier because there's already a unique borrow
            self.get_mut_listener().get_mut_log().add_error(e);
        }
    }
}

impl<R: Read> Gram<'_, '_, LL1, R> {
    pub fn build_ll1(mut self, grammar: CharReader<R>) -> ProdRuleSet<LL1> {
        self.build(grammar);
        let listener = self.wrapper.listener();
        let name = listener.get_name().to_string();
        let mut prs = listener.build_prs();
        prs.set_name(Some(name));
        prs.into()
    }
}
