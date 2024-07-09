#![allow(unused)]

use crate::CollectJoin;
use crate::grammar::{LLParsingTable, ProdFactor, ruleflag, Symbol, VarId};
use crate::symbol_table::SymbolTable;

mod tests;

#[derive(PartialEq)]
pub enum Call { Enter, Loop, Asm, Exit }

pub trait Listener {
    /// Calls the listener to execute synthesis or inheritance actions.
    ///
    /// The function returns true when `Asm(factor_id)` has to be pushed on the parser stack,
    /// typically to attach parameters to an object being assembled by the listener
    /// (intermediate inheritance).
    fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_str: Vec<String>) { /*false*/ }
}

pub struct Parser {
    num_nt: usize,
    num_t: usize,
    factors: Vec<(VarId, ProdFactor)>,
    opcodes: Vec<Vec<Symbol>>,
    flags: Vec<u32>,            // NT -> flags (+ or * normalization)
    parent: Vec<Option<VarId>>, // NT -> parent NT
    table: Vec<VarId>,
    symbol_table: SymbolTable,
    start: VarId
}

impl Parser {
    pub fn new(parsing_table: LLParsingTable, symbol_table: SymbolTable, start: VarId) -> Self {
        assert!(parsing_table.num_nt > start as usize);
        let mut parser = Parser {
            num_nt: parsing_table.num_nt,
            num_t: parsing_table.num_t,
            factors: parsing_table.factors,
            opcodes: Vec::new(),
            flags: parsing_table.flags,
            parent: parsing_table.parent,
            table: parsing_table.table,
            symbol_table, start };
        parser.build_opcodes();
        parser
    }

    fn build_opcodes(&mut self) {
        for (factor_id, (var_id, factor)) in self.factors.iter().enumerate() {
            let factor_id = factor_id as VarId;
            let flags = self.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            let mut opcode = Vec::<Symbol>::new();
            if new.get(0) == Some(&stack_sym) {
                opcode.push(new[0]);
                if flags & ruleflag::PARENT_L_FACTOR == 0 {
                    opcode.push(Symbol::Exit(factor_id));
                }
                opcode.extend(new.into_iter().skip(1));
            } else {
                if flags & ruleflag::CHILD_L_FACTOR != 0 {
                    let parent = self.parent[*var_id as usize].unwrap();
                    if matches!(new.last(), Some(Symbol::NT(parent))) {
                        opcode.push(Symbol::Loop(parent));
                        new.pop();
                    }
                }
                if flags & ruleflag::PARENT_L_FACTOR == 0 {
                    opcode.push(Symbol::Exit(factor_id)); // will be popped when this NT is completed
                }
                opcode.extend(new);
            }
            opcode.iter_mut().for_each(|o| {
                if let Symbol::NT(v) = o {
                    if v == var_id {
                        *o = Symbol::Loop(*v)
                    }
                }
            });
            self.opcodes.push(opcode);
        }
    }

    pub fn parse_stream<I, L>(&mut self, listener: &mut L, mut stream: I) -> Result<(), String>
        where I: Iterator<Item=(Symbol, String)>,
              L: Listener,
    {
        const VERBOSE: bool = false;
        let num_t_str = self.factors.iter().map(|(v, f)|
            (*v, f.iter().filter(|s| self.symbol_table.is_terminal_variable(s)).count())
        ).to_vec();
        let sym_table: Option<&SymbolTable> = Some(&self.symbol_table);
        let mut stack = Vec::<Symbol>::new();
        let mut stack_t = Vec::<String>::new();
        let error = self.factors.len() as VarId;
        let end = (self.num_t - 1) as VarId;
        stack.push(Symbol::End);
        stack.push(Symbol::NT(self.start));
        let mut stack_sym = stack.pop().unwrap();
        let mut stream_n = 1;
        let (mut stream_sym, mut stream_str) = stream.next().unwrap_or((Symbol::End, "".to_string()));
        loop {
            if VERBOSE {
                println!("{:-<40}", "");
                println!("input ({stream_n}): {}  stack: {}  current: {}", stream_sym.to_str_ext(sym_table, &stream_str),
                         stack.iter().map(|s| s.to_str(sym_table)).join(" "), stack_sym.to_str(sym_table));
                // println!("stack_t: {}", stack_t.join(", "));
            }
            match (stack_sym, stream_sym) {
                (Symbol::NT(var), _) | (Symbol::Loop(var), _) => {
                    let sr = if let Symbol::T(sr) = stream_sym { sr } else { end };
                    let factor_id = self.table[var as usize * self.num_t + sr as usize];
                    if VERBOSE {
                        println!("- table[{var}, {sr}] = {factor_id}: {}",
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
                    listener.switch(call, var, factor_id, vec![]);
                    let new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
                    if VERBOSE {
                        let f = &self.factors[factor_id as usize];
                        println!("- to stack: {}", self.opcodes[factor_id as usize].iter().filter(|s| !s.is_empty()).map(|s| s.to_str(sym_table)).join(", "));
                        println!("- {} {} -> {} ", if stack_sym.is_loop() { "LOOP" } else { "ENTER" },
                                 Symbol::NT(f.0).to_str(sym_table), f.1.to_str(sym_table));
                    }
                    stack.extend(self.opcodes[factor_id as usize].clone());
                    stack_sym = stack.pop().unwrap();
                }
                (Symbol::Exit(factor_id), _) => {
                    let (var, n) = num_t_str[factor_id as usize];
                    let t_str = stack_t.drain(stack_t.len() - n..).to_vec();
                    if VERBOSE { println!("- EXIT {} syn ({}): {}", Symbol::NT(var).to_str(sym_table), t_str.len(), t_str.iter().join(" ")); }
                    listener.switch(Call::Exit, var, factor_id, t_str);
                    stack_sym = stack.pop().unwrap();
                }
                (Symbol::T(sk), Symbol::T(sr)) => {
                    if sk != sr {
                        return Err(format!("unexpected character: '{}' instead of '{}'",
                                             stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)));
                    }
                    if VERBOSE { println!("- MATCH {}", stream_sym.to_str(sym_table)); }
                    if self.symbol_table.is_terminal_variable(&stack_sym) {
                        stack_t.push(stream_str);
                    }
                    stack_sym = stack.pop().unwrap();
                    stream_n += 1;
                    (stream_sym, stream_str) = stream.next().unwrap_or((Symbol::End, "".to_string()))
                }
                (Symbol::End, Symbol::End) => {
                    break;
                }
                (Symbol::End, _)  => {
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
