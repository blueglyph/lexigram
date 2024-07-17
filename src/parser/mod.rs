#![allow(unused)]

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::CollectJoin;
use crate::dfa::TokenId;
use crate::grammar::{LLParsingTable, ProdFactor, ruleflag, Symbol, VarId};
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

#[derive(PartialEq)]
pub enum Call { Enter, Loop, Exit }

pub trait Listener {
    /// Calls the listener to execute synthesis or inheritance actions.
    ///
    /// The function returns true when `Asm(factor_id)` has to be pushed on the parser stack,
    /// typically to attach parameters to an object being assembled by the listener
    /// (intermediate inheritance).
    fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Vec<String>) { /*false*/ }
}

// ---------------------------------------------------------------------------------------------

pub struct Parser {
    num_nt: usize,
    num_t: usize,
    factors: Vec<(VarId, ProdFactor)>,
    opcodes: Vec<Vec<OpCode>>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    table: Vec<VarId>,
    symbol_table: SymbolTable,
    start: VarId
}

impl Parser {
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
            symbol_table, start };
        parser
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        Some(&self.symbol_table)
    }

    pub fn parse_stream<I, L>(&mut self, listener: &mut L, mut stream: I) -> Result<(), String>
        where I: Iterator<Item=(Symbol, String)>,
              L: Listener,
    {
        const VERBOSE: bool = true;
        let sym_table: Option<&SymbolTable> = Some(&self.symbol_table);
        let mut stack = Vec::<OpCode>::new();
        let mut stack_t = Vec::<String>::new();
        let error = self.factors.len() as VarId;
        let end = (self.num_t - 1) as VarId;
        stack.push(OpCode::End);
        stack.push(OpCode::NT(self.start));
        let mut stack_sym = stack.pop().unwrap();
        let mut stream_n = 1;
        let (mut stream_sym, mut stream_str) = stream.next().unwrap_or((Symbol::End, "".to_string()));
        loop {
            if VERBOSE {
                println!("{:-<40}", "");
                println!("input ({stream_n}): {}   stack_t: [{}]   stack: [{}]   current: {}",
                         stream_sym.to_str_ext(sym_table, &stream_str),
                         stack_t.join(", "),
                         stack.iter().map(|s| s.to_str(sym_table)).join(" "),
                         stack_sym.to_str(sym_table));
            }
            match (stack_sym, stream_sym) {
                (OpCode::NT(var), _) | (OpCode::Loop(var), _) => {
                    let sr = if let Symbol::T(sr) = stream_sym { sr } else { end };
                    let factor_id = self.table[var as usize * self.num_t + sr as usize];
                    if VERBOSE {
                        println!("- table[{var}, {sr}] = {factor_id}: {} -> {}",
                                 Symbol::NT(var).to_str(self.get_symbol_table()),
                                 if factor_id >= error {
                                     "ERROR".to_string()
                                 } else {
                                     self.factors[factor_id as usize].1.to_str(sym_table)
                                 });
                    }
                    if factor_id >= error {
                        return Err(format!("syntax error on input '{}' while parsing '{}'",
                                           stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)
                        ));
                    }
                    let call = if stack_sym.is_loop() { Call::Loop } else { Call::Enter };
                    let t_data = std::mem::take(&mut stack_t);
                    if VERBOSE {
                        let f = &self.factors[factor_id as usize];
                        println!("- to stack: [{}]", self.opcodes[factor_id as usize].iter().filter(|s| !s.is_empty()).map(|s| s.to_str(sym_table)).join(" "));
                        println!("- {} {} -> {} ({}): [{}]", if stack_sym.is_loop() { "LOOP" } else { "ENTER" },
                                 Symbol::NT(f.0).to_str(sym_table), f.1.to_str(sym_table), t_data.len(), t_data.iter().join(" "));
                    }
                    listener.switch(call, var, factor_id, t_data);
                    let new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
                    stack.extend(self.opcodes[factor_id as usize].clone());
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::Exit(factor_id), _) => {
                    let var = self.factors[factor_id as usize].0;
                    let t_data = std::mem::take(&mut stack_t);
                    if VERBOSE { println!("- EXIT {} syn ({}): [{}]", Symbol::NT(var).to_str(sym_table), t_data.len(), t_data.iter().join(" ")); }
                    listener.switch(Call::Exit, var, factor_id, t_data);
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::T(sk), Symbol::T(sr)) => {
                    if sk != sr {
                        return Err(format!("unexpected character: '{}' instead of '{}'", stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)));
                    }
                    if VERBOSE { println!("- MATCH {}", stream_sym.to_str(sym_table)); }
                    if self.symbol_table.is_t_data(sk) {
                        stack_t.push(stream_str);
                    }
                    stack_sym = stack.pop().unwrap();
                    stream_n += 1;
                    (stream_sym, stream_str) = stream.next().unwrap_or((Symbol::End, "".to_string()))
                }
                (OpCode::End, Symbol::End) => {
                    break;
                }
                (OpCode::End, _)  => {
                    return Err(format!("extra symbol '{}' after end of parsing", stream_sym.to_str(sym_table)));
                }
                (_, Symbol::End) => {
                    return Err(format!("end of stream while expecting a '{}'", stack_sym.to_str(sym_table)));
                }
                (_, _) => {
                    return Err(format!("unexpected situation: input '{}' while expecting '{}'",
                                       stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)));
                }
            }
        }
        assert!(stack_t.is_empty(), "stack_t: {}", stack_t.join(", "));
        assert!(stack.is_empty(), "stack: {}", stack.iter().map(|s| s.to_str(sym_table)).join(", "));
        Ok(())
    }
}
