#![allow(unused)]

use crate::CollectJoin;
use crate::grammar::{factor_to_string, LLParsingTable, ProdFactor, Symbol, VarId};
use crate::symbol_table::SymbolTable;

mod tests;

pub enum Call { Enter, Rec, Exit }

pub trait Listener {
    /// Calls the listener to execute synthesis or inheritance actions.
    ///
    /// The function returns true when `Rec(factor_id)` has to be pushed on the parser stack,
    /// typically to attach parameters to an object being reconstructed by the listener
    /// (intermediate inheritance).
    fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_str: Vec<String>) -> bool { false }
}

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

    #[cfg(test)]
    pub fn print_table(&self) {
        let st = Some(&self.symbol_table);
        println!("factors:\n{}",
                 self.factors.iter().enumerate().map(|(id, (v, f))|
                     format!("// - {id}: {} -> {}", Symbol::NT(*v).to_str(st),
                             f.iter().map(|s| s.to_str(st)).join(" "))
        ).join("\n"));
        println!("table:");
        let indent = 0;
        let error = self.factors.len() as VarId;
        let str_nt = (0..self.num_nt).map(|i| Symbol::NT(i as VarId).to_str(st)).to_vec();
        let max_nt_len = str_nt.iter().map(|s| s.len()).max().unwrap();
        let str_t = (0..self.num_t).map(|j| if j + 1 < self.num_t { Symbol::T(j as VarId).to_str(st) } else { "$".to_string() }).to_vec();
        let max_t_len = str_t.iter().map(|s| s.len()).max().unwrap().max(3);
        println!("{:<i$}// {:<w$} | {}", "", "", (0..self.num_t).map(|j| format!("{:>1$}", str_t[j], max_t_len)).join(" "), w = max_nt_len, i = indent);
        println!("{:<i$}// {:-<w$}-+-{:-<t$}", "", "", "", w = max_nt_len, t = self.num_t * (max_t_len + 1), i = indent);
        for i in 0..self.num_nt {
            print!("{:<i$}// {:>w$} |", "", str_nt[i], w = max_nt_len, i = indent);
            for j in 0..self.num_t {
                let value = self.table[i * self.num_t + j];
                if value < error {
                    print!(" {:3}", value);
                } else {
                    print!("   .");
                }
            }
            println!();
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
        let (mut stream_sym, mut stream_str) = stream.next().unwrap_or((Symbol::End, "".to_string()));
        loop {
            if VERBOSE {
                println!("{:-<40}", "");
                println!("input: {}  stack: {}  current: {}", stream_sym.to_str(sym_table),
                         stack.iter().map(|s| s.to_str(sym_table)).join(" "), stack_sym.to_str(sym_table));
            }
            match (stack_sym, stream_sym) {
                (Symbol::NT(var), _) => {
                    let sr = if let Symbol::T(sr) = stream_sym { sr } else { end };
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
                        ));
                    }
                    if VERBOSE {
                        println!("- PUSH {}", self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev()
                            .map(|s| s.to_str(sym_table)).join(" "));
                    }
                    // TODO: put Rec(f) and Exit(f) directly into the factors
                    let rec_required = listener.switch(Call::Enter, var, factor_id, vec![]);
                    let new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
                    if new.get(0) == Some(&stack_sym) {
                        stack.push(new[0]);
                        stack.push(Symbol::Exit(factor_id));
                        if rec_required {
                            stack.extend(&new[1..new.len() - 1]);
                            stack.push(Symbol::Rec(factor_id));
                            stack.push(new[new.len() - 1]);
                        } else {
                            stack.extend(new.into_iter().skip(1));
                        }
                    } else {
                        stack.push(Symbol::Exit(factor_id)); // will be popped when this NT is completed
                        if rec_required {
                            stack.extend(&new[0..new.len() - 1]);
                            stack.push(Symbol::Rec(factor_id));
                            stack.push(new[new.len() - 1]);
                        } else {
                            stack.extend(new);
                        }
                    }
                    stack_sym = stack.pop().unwrap();
                }
                (Symbol::Rec(factor_id), _) => {
                    let (var, n) = num_t_str[factor_id as usize];
                    let t_str = stack_t.drain(stack_t.len() - n..).to_vec();
                    if VERBOSE { println!("- REC {} syn: {}", Symbol::NT(var).to_str(sym_table), t_str.iter().join(" ")); }
                    listener.switch(Call::Rec, var, factor_id, t_str);
                    stack_sym = stack.pop().unwrap();
                }
                (Symbol::Exit(factor_id), _) => {
                    let (var, n) = num_t_str[factor_id as usize];
                    let t_str = stack_t.drain(stack_t.len() - n..).to_vec();
                    if VERBOSE { println!("- EXIT {} syn: {}", Symbol::NT(var).to_str(sym_table), t_str.iter().join(" ")); }
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
                    (stream_sym, stream_str) = stream.next().unwrap_or((Symbol::End, "".to_string()))
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
