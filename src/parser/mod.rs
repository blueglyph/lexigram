#![allow(unused)]

use crate::CollectJoin;
use crate::grammar::{factor_to_string, LLParsingTable, ProdFactor, Symbol, VarId};
use crate::symbol_table::SymbolTable;

mod tests;

pub struct Parser {
    num_nt: usize,
    num_t: usize,
    factors: Vec<(VarId, ProdFactor)>,
    table: Vec<VarId>,
    symbol_table: SymbolTable,
    start: VarId
}

impl Parser {
    pub fn new(parsing_table: LLParsingTable, symbol_table: SymbolTable, start: VarId) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        Parser {
            num_nt: parsing_table.num_nt,
            num_t: parsing_table.num_t,
            factors: parsing_table.factors,
            table: parsing_table.table,
            symbol_table, start }
    }

    pub fn parse_stream<I: Iterator<Item=(Symbol, String)>>(&mut self, mut stream: I) -> Result<(), String> {
        const VERBOSE: bool = false;
        let sym_table = Some(&self.symbol_table);
        let mut stack = Vec::<Symbol>::new();
        let error = self.factors.len() as VarId;
        stack.push(Symbol::End);
        stack.push(Symbol::NT(self.start));
        let mut stack_sym = stack.pop().unwrap();
        let mut stream_sym = stream.next().map(|(s, _)| s).unwrap_or(Symbol::End);
        loop {
            if VERBOSE {
                println!("{:-<40}", "");
                println!("input: {}  stack: {}  current: {}", stream_sym.to_str(sym_table),
                         stack.iter().map(|s| s.to_str(sym_table)).join(" "), stack_sym.to_str(sym_table));
            }
            match (stack_sym, stream_sym) {
                (Symbol::NT(var), Symbol::T(sr)) => {
                    let factor_id = self.table[var as usize * self.num_t + sr as usize];
                    if VERBOSE {
                        println!("- table[{var}, {sr}] = {factor_id}: {}",
                                 if factor_id >= error {
                                     "ERROR".to_string()
                                 } else {
                                     factor_to_string(&self.factors[factor_id as usize].1, sym_table)
                                 });
                    }
                    if factor_id >= error {
                        return Err(format!("syntax error on input '{}' while parsing '{}'",
                            stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)
                        ))
                    }
                    if VERBOSE {
                        println!("- PUSH {}", self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev()
                            .map(|s| s.to_str(sym_table)).join(" "));
                    }
                    stack.extend(self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned());
                    stack_sym = stack.pop().unwrap();
                }
                (Symbol::T(sk), Symbol::T(sr)) => {
                    if sk != sr {
                        return Err(format!("unexpected character: '{}' instead of '{}'",
                                             stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)));
                    }
                    if VERBOSE { println!("- MATCH {}", stream_sym.to_str(sym_table)); }
                    stack_sym = stack.pop().unwrap();
                    stream_sym = stream.next().map(|(s, _)| s).unwrap_or(Symbol::End);
                }
                (Symbol::End, Symbol::End) => {
                    break;
                }
                (Symbol::End, _)  => {
                    return Err(format!("extra symbol '{}' after end of parsing", stream_sym.to_str(sym_table)));
                } (_, Symbol::End) => {
                    return Err(format!("end of stream while expecting a '{}'", stack_sym.to_str(sym_table)));
                }
                (_, _) => {
                    return Err(format!("unexpected situation: input '{}' while expecting '{}'",
                                       stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)));
                }
            }
        }
        Ok(())
    }
}