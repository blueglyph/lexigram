// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::CollectJoin;
use crate::dfa::TokenId;
use crate::grammar::{LLParsingTable, ProdFactor, ruleflag, Symbol, VarId, FactorId};
use crate::lexer::{CaretCol, CaretLine};
use crate::log::{BufLog, Logger};
use crate::symbol_table::SymbolTable;

mod tests;

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum OpCode {
    Empty,              // empty symbol
    T(TokenId),         // terminal
    NT(VarId),          // non-terminal
    Loop(VarId),        // loop to same non-terminal
    Exit(VarId),        // exit non-terminal
    End                 // end of stream
}

#[derive(PartialEq, Debug)]
pub enum Call { Enter, Loop, Exit, End }

pub trait ListenerWrapper {
    /// Calls the listener to execute Enter, Loop, Exit, and End actions.
    fn switch(&mut self, call: Call, nt: VarId, factor_id: FactorId, t_data: Option<Vec<String>>) {}
    /// Checks if the wrapper requests an abort. This happens if an error is too difficult to recover from
    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
    fn check_abort_request(&self) -> bool { false }
    /// Aborts the parsing.
    fn abort(&mut self) {}
    /// Gets access to the listener's log to report possible errors and information about the parsing.
    fn get_mut_log(&mut self) -> &mut impl Logger;
}

// ---------------------------------------------------------------------------------------------

pub type ParserToken = (TokenId, String, CaretCol, CaretLine);

#[derive(PartialEq, Debug)]
pub enum ParserError {
    SyntaxError,
    TooManyErrors,
    Irrecoverable,
    ExtraSymbol,
    UnexpectedEOS,
    UnexpectedError,
    EncounteredErrors,
    AbortRequest,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ParserError::SyntaxError => "syntax error",
            ParserError::TooManyErrors => "too many errors while trying to recover",
            ParserError::Irrecoverable => "irrecoverable syntax error",
            ParserError::ExtraSymbol => "extra symbol after end of parsing",
            ParserError::UnexpectedEOS => "unexpected end of stream",
            ParserError::UnexpectedError => "unexpected error",
            ParserError::EncounteredErrors => "encountered errors",
            ParserError::AbortRequest => "abort request",
        })
    }
}

pub struct Parser {
    num_nt: usize,
    num_t: usize,
    factors: Vec<(VarId, ProdFactor)>,
    opcodes: Vec<Vec<OpCode>>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    table: Vec<FactorId>,
    symbol_table: SymbolTable,
    start: VarId,
    try_recover: bool,          // tries to recover from syntactical errors
}

impl Parser {
    /// Maximum number of error recoveries attempted when meeting a syntax error
    pub const MAX_NBR_RECOVERS: u32 = 5;
    pub fn new(parsing_table: LLParsingTable, symbol_table: SymbolTable, opcodes: Vec<Vec<OpCode>>, start: VarId) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        let mut parser = Parser {
            num_nt: parsing_table.num_nt,
            num_t: parsing_table.num_t,
            factors: parsing_table.factors,
            opcodes,
            flags: parsing_table.flags,
            parent: parsing_table.parent,
            table: parsing_table.table,
            symbol_table,
            start,
            try_recover: true
        };
        parser
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        Some(&self.symbol_table)
    }

    pub fn set_start(&mut self, start: VarId) {
        assert!(self.num_nt > start as usize);
        self.start = start;
    }

    pub fn set_try_recover(&mut self, try_recover: bool) {
        self.try_recover = try_recover;
    }

    pub(crate) fn get_factors(&self) -> &Vec<(VarId, ProdFactor)> {
        &self.factors
    }

    pub(crate) fn get_opcodes(&self) -> &Vec<Vec<OpCode>> {
        &self.opcodes
    }

    /// Parses an entire `stream` using the `listener`, and returns `Ok(())` if the whole stream could
    /// be successfully parsed, or an error if it couldn't.
    ///
    /// All errors are reported to the wrapper's log. Usually, the wrapper simply transmits the
    /// reports to the user listener's log, where the user listener is embedded in the wrapper as one
    /// of its fields and is defined by the user (instead of being generated like the wrapper).
    pub fn parse_stream<I, L>(&mut self, wrapper: &mut L, mut stream: I) -> Result<(), ParserError>
        where I: Iterator<Item=ParserToken>,
              L: ListenerWrapper,
    {
        const VERBOSE: bool = false;
        let sym_table: Option<&SymbolTable> = Some(&self.symbol_table);
        let mut stack = Vec::<OpCode>::new();
        let mut stack_t = Vec::<String>::new();
        let error_skip_factor_id = self.factors.len() as FactorId;
        let error_pop_factor_id = error_skip_factor_id + 1;
        if VERBOSE { println!("skip = {error_skip_factor_id}, pop = {error_pop_factor_id}"); }
        let mut recover_mode = false;
        let mut nbr_recovers = 0;
        let end_var_id = (self.num_t - 1) as VarId;
        stack.push(OpCode::End);
        stack.push(OpCode::NT(self.start));
        let mut stack_sym = stack.pop().unwrap();
        let mut stream_n = 0;
        let mut stream_pos = None;
        let mut stream_sym = Symbol::default(); // must set fake value to comply with borrow checker
        let mut stream_str = String::default(); // must set fake value to comply with borrow checker
        let mut advance_stream = true;
        loop {
            if advance_stream {
                stream_n += 1;
                (stream_sym, stream_str) = stream.next().map(|(t, s, line, col)| {
                    stream_pos = Some((line, col));
                    (Symbol::T(t), s)
                }).unwrap_or((Symbol::End, String::new()));
                advance_stream = false;
            }
            if VERBOSE {
                println!("{:-<40}", "");
                println!("input ({stream_n}{}): {}   stack_t: [{}]   stack: [{}]   current: {}",
                         if let Some((line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() },
                         stream_sym.to_str_ext(sym_table, &stream_str),
                         stack_t.join(", "),
                         stack.iter().map(|s| s.to_str(sym_table)).join(" "),
                         stack_sym.to_str(sym_table));
            }
            match (stack_sym, stream_sym) {
                (OpCode::NT(var), _) | (OpCode::Loop(var), _) => {
                    let sr = if let Symbol::T(sr) = stream_sym { sr } else { end_var_id };
                    let factor_id = self.table[var as usize * self.num_t + sr as usize];
                    if VERBOSE {
                        println!("- table[{var}, {sr}] = {factor_id}: {} -> {}",
                                 Symbol::NT(var).to_str(self.get_symbol_table()),
                                 if factor_id >= error_skip_factor_id {
                                     "ERROR".to_string()
                                 } else {
                                     self.factors[factor_id as usize].1.to_str(sym_table)
                                 });
                    }
                    if !recover_mode && factor_id >= error_skip_factor_id {
                        let expected = (0..self.num_t as VarId).filter(|t| self.table[var as usize * self.num_t + *t as usize] < error_skip_factor_id)
                            .into_iter().map(|t| format!("'{}'", Symbol::T(t).to_str(self.get_symbol_table())))
                            .join(", ");
                        let stream_sym_txt = if stream_sym.is_end() { "end of stream".to_string() } else { format!("input '{}'", stream_sym.to_str(sym_table)) };
                        let msg = format!("syntax error: found {stream_sym_txt} instead of {expected} while parsing '{}'{}",
                                          stack_sym.to_str(sym_table),
                                          if let Some((line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                        if self.try_recover {
                            wrapper.get_mut_log().add_error(msg);
                            if nbr_recovers >= Self::MAX_NBR_RECOVERS {
                                wrapper.get_mut_log().add_error(format!("too many errors ({nbr_recovers}), giving up"));
                                wrapper.abort();
                                return Err(ParserError::TooManyErrors);
                            }
                            nbr_recovers += 1;
                            recover_mode = true;
                        } else {
                            wrapper.get_mut_log().add_error(msg);
                            wrapper.abort();
                            return Err(ParserError::SyntaxError);
                        }
                    }
                    if recover_mode {
                        if VERBOSE { println!("!NT {} <-> {}, factor_id = {factor_id}", stack_sym.to_str(self.get_symbol_table()), stream_sym.to_str(self.get_symbol_table())); }
                        if factor_id == error_skip_factor_id {
                            if stream_sym == Symbol::End {
                                let msg = "irrecoverable error, reached end of stream".to_string();
                                if VERBOSE { println!("(recovering) {msg}"); }
                                wrapper.get_mut_log().add_error(msg);
                                wrapper.abort();
                                return Err(ParserError::Irrecoverable);
                            }
                            if VERBOSE { println!("(recovering) skipping token {}", stream_sym.to_str(self.get_symbol_table())); }
                            advance_stream = true;
                        } else if factor_id == error_pop_factor_id {
                            if VERBOSE { println!("(recovering) popping {}", stack_sym.to_str(self.get_symbol_table())); }
                            stack_sym = stack.pop().unwrap();
                        } else {
                            if factor_id < error_skip_factor_id {
                                recover_mode = false;
                                if VERBOSE { println!("(recovering) resynchronized"); }
                            } else {
                                panic!("illegal factor_id {factor_id}")
                            }
                        }
                    }
                    if !recover_mode {
                        let call = if stack_sym.is_loop() { Call::Loop } else { Call::Enter };
                        let t_data = std::mem::take(&mut stack_t);
                        if VERBOSE {
                            let f = &self.factors[factor_id as usize];
                            println!("- to stack: [{}]", self.opcodes[factor_id as usize].iter().filter(|s| !s.is_empty()).map(|s| s.to_str(sym_table)).join(" "));
                            println!("- {} {} -> {} ({}): [{}]", if stack_sym.is_loop() { "LOOP" } else { "ENTER" },
                                     Symbol::NT(f.0).to_str(sym_table), f.1.to_str(sym_table), t_data.len(), t_data.iter().join(" "));
                        }
                        if nbr_recovers == 0 {
                            wrapper.switch(call, var, factor_id, Some(t_data));
                        }
                        let new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
                        stack.extend(self.opcodes[factor_id as usize].clone());
                        stack_sym = stack.pop().unwrap();
                    }
                }
                (OpCode::Exit(factor_id), _) => {
                    let var = self.factors[factor_id as usize].0;
                    let t_data = std::mem::take(&mut stack_t);
                    if VERBOSE { println!("- EXIT {} syn ({}): [{}]", Symbol::NT(var).to_str(sym_table), t_data.len(), t_data.iter().join(" ")); }
                    if nbr_recovers == 0 {
                        wrapper.switch(Call::Exit, var, factor_id, Some(t_data));
                    }
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::T(sk), Symbol::T(sr)) => {
                    if !recover_mode && sk != sr {
                        let msg = format!("syntax error: found input '{}' instead of '{}'{}", stream_sym.to_str(sym_table), stack_sym.to_str(sym_table),
                                          if let Some((line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                        if self.try_recover {
                            wrapper.get_mut_log().add_error(msg);
                            if nbr_recovers >= Self::MAX_NBR_RECOVERS {
                                wrapper.get_mut_log().add_error(format!("too many errors ({nbr_recovers}), giving up"));
                                wrapper.abort();
                                return Err(ParserError::TooManyErrors);
                            }
                            nbr_recovers += 1;
                            recover_mode = true;
                        } else {
                            wrapper.get_mut_log().add_error(msg);
                            wrapper.abort();
                            return Err(ParserError::SyntaxError);
                        }
                    }
                    if recover_mode {
                        if VERBOSE { println!("!T {} <-> {}", stack_sym.to_str(self.get_symbol_table()), stream_sym.to_str(self.get_symbol_table())); }
                        if sk == sr {
                            recover_mode = false;
                            if VERBOSE { println!("(recovering) resynchronized"); }
                        } else {
                            if VERBOSE { println!("(recovering) popping {}", stack_sym.to_str(self.get_symbol_table())); }
                            stack_sym = stack.pop().unwrap();
                        }
                    }
                    if !recover_mode {
                        if VERBOSE { println!("- MATCH {}", stream_sym.to_str(sym_table)); }
                        if self.symbol_table.is_t_data(sk) {
                            stack_t.push(std::mem::take(&mut stream_str)); // must use take() to comply with borrow checker
                        }
                        stack_sym = stack.pop().unwrap();
                        advance_stream = true;
                    }
                }
                (OpCode::End, Symbol::End) => {
                    if nbr_recovers == 0 {
                        wrapper.switch(Call::End, 0, 0, None);
                    }
                    break;
                }
                (OpCode::End, _) => {
                    wrapper.get_mut_log().add_error(format!("syntax error: found extra symbol '{}' after end of parsing", stream_sym.to_str(sym_table)));
                    wrapper.abort();
                    return Err(ParserError::ExtraSymbol);
                }
                (_, Symbol::End) => {
                    wrapper.get_mut_log().add_error(format!("syntax error: found end of stream instead of '{}'", stack_sym.to_str(sym_table)));
                    wrapper.abort();
                    return Err(ParserError::UnexpectedEOS);
                }
                (_, _) => {
                    wrapper.get_mut_log().add_error(format!("unexpected syntax error: input '{}' while expecting '{}'{}",
                                                            stream_sym.to_str(sym_table), stack_sym.to_str(sym_table),
                                                            if let Some((line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() }));
                    wrapper.abort();
                    return Err(ParserError::UnexpectedError);
                }
            }
            if wrapper.check_abort_request() {
                wrapper.abort();
                return Err(ParserError::AbortRequest);
            }
        }
        assert!(stack_t.is_empty(), "stack_t: {}", stack_t.join(", "));
        assert!(stack.is_empty(), "stack: {}", stack.iter().map(|s| s.to_str(sym_table)).join(", "));
        if nbr_recovers == 0 {
            Ok(())
        } else {
            // when nbr_recovers > 0, we know that at least one error has been reported to the log, no need to add one here
            wrapper.abort();
            Err(ParserError::EncounteredErrors)
        }
    }
}
