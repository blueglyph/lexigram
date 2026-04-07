// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use gramlexer::build_lexer;
use gramparser::{build_parser, GramParserListener, Wrapper};
use listener::GramListener;
use lexigram_lib::grammar::ProdRuleSet;
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::lexer::{CaretCol, Lexer, TokenSpliterator};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::{BuildFrom, BuildInto};
use lexigram_lib::parser::Parser;
use lexigram_lib::{General, SymbolTable, LL1};
use std::io::Cursor;
use lexigram_lib::build::{BuildErrorSource, HasBuildErrorSource};

mod gramlexer;
mod gramparser;
mod listener;
mod tests;

#[derive(Clone, Debug)]
pub struct GramOptions {
    pub tab_width: CaretCol,
    pub ansi: bool,
}

pub struct Gram<'l, 'p, 'ls> {
    pub options: GramOptions,
    pub gramlexer: Lexer<'l, Cursor<&'l str>>,
    pub gramparser: Parser<'p>,
    pub wrapper: Wrapper<GramListener<'ls>>,
    start_nt: Option<String>,
}

impl<'l, 'ls: 'l> Gram<'l, '_, 'ls> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_LISTENER: bool = false;

    pub fn new(symbol_table: SymbolTable, grammar: &'ls str) -> Self {
        let listener = GramListener::new(symbol_table, grammar);
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_listener_mut().set_verbose(Self::VERBOSE_LISTENER);
        let mut gramlexer = build_lexer();
        gramlexer.set_tab_width(4);
        gramlexer.attach_stream(CharReader::new(Cursor::new(grammar)));
        let mut gram = Gram {
            options: GramOptions::default(),
            gramlexer,
            gramparser: build_parser(),
            wrapper,
            start_nt: None,
        };
        gram.apply_options();
        gram
    }

    pub fn set_options(&mut self, options: GramOptions) {
        self.options = options;
        self.apply_options();
    }

    fn apply_options(&mut self) {
        self.gramlexer.set_tab_width(self.options.tab_width);
        let ansi = self.options.ansi;
        self.get_listener_mut().set_ansi(ansi);
    }

    pub fn set_start_nt(&mut self, name_opt: Option<String>) {
        self.start_nt = name_opt;
    }

    pub fn get_listener_mut(&mut self) -> &mut GramListener<'ls> {
        self.wrapper.get_listener_mut()
    }

    pub fn get_listener(&self) -> &GramListener<'ls> {
        self.wrapper.get_listener()
    }

    fn make(&mut self) -> Result<(), &BufLog> {
        let tokens = self.gramlexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
            panic!("unexpected channel {ch} from Gram while parsing a grammar at {pos_span}, \"{text}\"")
        );
        if let Err(e) = self.gramparser.parse_stream(&mut self.wrapper, tokens) {
            self.get_listener_mut().get_log_mut().add_error(e.to_string());
        }
        let log = self.get_listener().get_log();
        if !log.has_no_errors() {
            Ok(())
        } else {
            Err(log)
        }
    }
}

impl LogReader for Gram<'_, '_, '_> {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        self.get_listener().get_log()
    }

    fn give_log(self) -> Self::Item {
        let listener = self.wrapper.give_listener();
        listener.give_log()
    }
}

impl HasBuildErrorSource for Gram<'_, '_, '_> {
    const SOURCE: BuildErrorSource = BuildErrorSource::Gram;
}

impl<'l, 'p, 'ls: 'l> BuildFrom<Gram<'l, 'p, 'ls>> for ProdRuleSet<LL1> {
    /// Produces a [`ProdRuleSet<LL1>`] from a [`Gram`], by parsing the grammar
    /// and creating the rule set, then transforming the result if necessary for an LL1 grammar.
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn build_from(mut gram: Gram<'l, 'p, 'ls>) -> ProdRuleSet<LL1> {
        let _ = gram.make();
        let mut listener = gram.wrapper.give_listener();
        let name = listener.get_name().to_string();
        if let Some(name) = gram.start_nt {
            if let Some(start_nt) = listener.get_symbol_table().find_nt(&name) {
                listener.set_start_nt(start_nt);
                listener.set_disable_warning_unused_nt_t(true);
            } else {
                listener.get_log_mut().add_error(format!("couldn't find nonterminal '{name}' to set the start rule"))
            }
        }
        let mut prs = ProdRuleSet::<General>::from(listener);
        prs.set_name(Some(name));
        prs.build_into()
    }
}

impl Default for GramOptions {
    fn default() -> Self {
        GramOptions {
            tab_width: 4,
            ansi: true,
        }
    }
}
