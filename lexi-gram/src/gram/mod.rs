// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use gramlexer::build_lexer;
use gramparser::{build_parser, GramParserListener, Wrapper};
use listener::GramListener;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::lexer::{Lexer, TokenSpliterator};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::{BuildFrom, BuildInto};
use lexigram_lib::parser::Parser;
use lexigram_lib::{General, SymbolTable, LL1};
use std::io::Read;
use lexigram_lib::build::{BuildErrorSource, HasBuildErrorSource};

mod gramlexer;
mod gramparser;
mod listener;
mod tests;

pub struct Gram<'a, 'b, R: Read> {
    pub gramlexer: Lexer<'a, R>,
    pub gramparser: Parser<'b>,
    pub wrapper: Wrapper<GramListener>,
}

impl<R: Read> Gram<'_, '_, R> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_LISTENER: bool = false;

    pub fn new(symbol_table: SymbolTable, grammar: CharReader<R>) -> Self {
        let listener = GramListener::new(symbol_table);
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_listener_mut().set_verbose(Self::VERBOSE_LISTENER);
        let mut gramlexer = build_lexer();
        gramlexer.set_tab_width(4);
        gramlexer.attach_stream(grammar);
        Gram {
            gramlexer,
            gramparser: build_parser(),
            wrapper,
        }
    }

    pub fn get_listener_mut(&mut self) -> &mut GramListener {
        self.wrapper.get_listener_mut()
    }

    pub fn get_listener(&self) -> &GramListener {
        self.wrapper.get_listener()
    }

    fn make(&mut self) -> Result<(), &BufLog> {
        let tokens = self.gramlexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} from Gram while parsing a grammar at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.gramparser.parse_stream(&mut self.wrapper, tokens) {
            self.get_listener_mut().get_mut_log().add_error(e.to_string());
        }
        let log = self.get_listener().get_log();
        if !log.has_no_errors() {
            Ok(())
        } else {
            Err(log)
        }
    }
}

impl<R: Read> LogReader for Gram<'_, '_, R> {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        self.get_listener().get_log()
    }

    fn give_log(self) -> Self::Item {
        let listener = self.wrapper.give_listener();
        listener.give_log()
    }
}

impl<R: Read> HasBuildErrorSource for Gram<'_, '_, R> {
    const SOURCE: BuildErrorSource = BuildErrorSource::Gram;
}

impl<R: Read> BuildFrom<Gram<'_, '_, R>> for ProdRuleSet<LL1> {
    /// Produces a [`ProdRuleSet<LL1>`] from a [`Gram`], by parsing the grammar
    /// and creating the rule set, then transforming the result if necessary for an LL1 grammar.
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn build_from(mut gram: Gram<R>) -> ProdRuleSet<LL1> {
        let _ = gram.make();
        let listener = gram.wrapper.give_listener();
        let name = listener.get_name().to_string();
        let mut prs = ProdRuleSet::<General>::from(listener);
        prs.set_name(Some(name));
        prs.build_into()
    }
}
