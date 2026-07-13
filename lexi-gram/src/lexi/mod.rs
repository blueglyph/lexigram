// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::Cursor;
use lexigram_lib::dfa::Dfa;
use lexigram_lib::char_reader::CharReader;
use lexigram_lib::lexer::{CaretCol, Lexer, Pos, TokenSpliterator};
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::build::{BuildFrom, BuildInto, TryBuildFrom};
use lexigram_lib::parser::ll1::LLParser;
use lexigram_lib::{Normalized, SymbolTable, TokenId};
use lexigram_lib::build::{BuildError, BuildErrorSource, HasBuildErrorSource};
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
    pub symbol_table: SymbolTable,
    pub terminal_hooks: Vec<TokenId>,
    pub pos_grammar_opt: Option<Pos>,
}

#[derive(Clone, Debug)]
pub struct LexiOptions {
    pub tab_width: CaretCol,
    pub ansi: bool,
}

pub struct Lexi<'l, 'p, 'ls> {
    pub options: LexiOptions,
    pub lexilexer: Lexer<'l, Cursor<&'l str>>,
    pub lexiparser: LLParser<'p>,
    wrapper: Wrapper<LexiListener<'ls>>,
    is_built: bool
}

impl<'l, 'ls: 'l> Lexi<'l, '_, 'ls> {
    const VERBOSE_WRAPPER: bool = false;
    const VERBOSE_DETAILS: bool = false;
    const VERBOSE_LISTENER: bool = false;

    pub fn new(lexicon: &'ls str) -> Self {
        let listener = LexiListener::new(lexicon);
        let mut wrapper = Wrapper::new(listener, Self::VERBOSE_WRAPPER);
        wrapper.get_listener_mut().set_verbose(Self::VERBOSE_LISTENER);
        let mut lexilexer = build_lexer();
        lexilexer.attach_stream(CharReader::new(Cursor::new(lexicon)));
        let mut lexi = Lexi {
            options: LexiOptions::default(),
            lexilexer,
            lexiparser: build_parser(),
            wrapper,
            is_built: false
        };
        lexi.apply_options();
        lexi
    }

    pub fn set_options(&mut self, options: LexiOptions) {
        self.options = options;
        self.apply_options();
    }

    fn apply_options(&mut self) {
        self.lexilexer.set_tab_width(self.options.tab_width);
        let ansi = self.options.ansi;
        self.get_listener_mut().set_ansi(ansi);
    }

    pub fn get_listener_mut(&mut self) -> &mut LexiListener<'ls> {
        self.wrapper.get_listener_mut()
    }

    pub fn get_listener(&self) -> &LexiListener<'ls> {
        self.wrapper.get_listener()
    }

    pub fn get_tab_width(&self) -> CaretCol {
        self.lexilexer.get_tab_width()
    }

    pub fn set_tab_width(&mut self, width: CaretCol) {
        self.lexilexer.set_tab_width(width);
    }

    fn make(&mut self) {
        if !self.is_built {
            // we keep track of the built state because some unit tests are calling build() directly
            self.is_built = true;
            let tokens = self.lexilexer.tokens().split_channel0(|(_tok, ch, text, pos_span)|
                panic!("unexpected channel {ch} from Lexi while parsing a lexicon at {pos_span}, \"{text}\"")
            ).inspect(|(tok, text, pos_span)| {
                if Self::VERBOSE_DETAILS {
                    println!("TOKEN: pos {pos_span}, Id {tok:?}, \"{text}\"");
                }
            });
            if self.lexiparser.parse_stream(&mut self.wrapper, tokens).is_ok() {
                for s in self.wrapper.get_listener_mut().rules_to_vecstrings() {
                    self.wrapper.get_listener_mut().get_log_mut().add_note(s);
                }
            }
        }
    }
}

impl LogReader for Lexi<'_, '_, '_> {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        self.get_listener().get_log()
    }

    fn give_log(self) -> Self::Item {
        let listener = self.wrapper.give_listener();
        listener.give_log()
    }
}

impl HasBuildErrorSource for Lexi<'_, '_, '_> {
    const SOURCE: BuildErrorSource = BuildErrorSource::Lexi;
}

impl<'l, 'p, 'ls: 'l> BuildFrom<Lexi<'l, 'p, 'ls>> for SymbolicDfa {
    fn build_from(mut lexi: Lexi<'l, 'p, 'ls>) -> Self {
        lexi.make();
        let listener = lexi.wrapper.give_listener();
        let symbol_table = listener.make_symbol_table();
        let terminal_hooks = listener.terminal_hooks.clone();
        let pos_grammar_opt = listener.get_pos_grammar();
        SymbolicDfa {
            dfa: listener.build_into(),
            symbol_table,
            terminal_hooks,
            pos_grammar_opt,
        }
    }
}

impl TryBuildFrom<Lexi<'_, '_, '_>> for SymbolicDfa {
    type Error = BuildError;

    fn try_build_from(source: Lexi<'_, '_, '_>) -> Result<Self, Self::Error> {
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

impl Default for LexiOptions {
    fn default() -> Self {
        LexiOptions {
            tab_width: 4,
            ansi: true,
        }
    }
}
