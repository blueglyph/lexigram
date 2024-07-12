#![allow(unused)]

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::CollectJoin;
use crate::dfa::TokenId;
use crate::grammar::{LLParsingTable, ProdFactor, ruleflag, Symbol, VarId};
use crate::symbol_table::SymbolTable;

mod tests;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum OpCode {
    Empty,              // empty symbol
    T(TokenId),         // terminal
    NT(VarId, u16),     // non-terminal and expected number of lexer strings
    Loop(VarId, u16),   // loop to same non-terminal and expected number of lexer strings
    Exit(VarId, u16),   // exit non-terminal and expected number of lexer strings
    End                 // end of stream
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Empty => write!(f, "ε"),
            OpCode::T(t) => write!(f, ":{t}"),
            OpCode::NT(v, n) =>   if *n > 0 { write!(f, "►{v}/{n}") } else { write!(f, "►{v}") } ,
            OpCode::Loop(v, n) => if *n > 0 { write!(f, "●{v}/{n}") } else { write!(f, "●{v}") } ,
            OpCode::Exit(v, n) => if *n > 0 { write!(f, "◄{v}/{n}") } else { write!(f, "◄{v}") },
            OpCode::End => write!(f, "$"),
        }
    }
}

impl OpCode {
    pub fn is_loop(&self) -> bool {
        matches!(self, OpCode::Loop(_, _))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, OpCode::Empty)
    }

    pub fn matches(&self, s: Symbol) -> bool {
        match self {
            OpCode::Empty => s == Symbol::Empty,
            OpCode::T(t) => s == Symbol::T(*t),
            OpCode::NT(v, _) => s == Symbol::NT(*v),
            OpCode::End => s == Symbol::End,
            OpCode::Loop(v, _) => false,
            OpCode::Exit(v, _) => false,
        }
    }

    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        if let Some(t) = symbol_table {
            match self {
                OpCode::Empty => "ε".to_string(),
                OpCode::T(v) => format!("{}", t.get_t_name(*v)),
                OpCode::NT(v, n) => if *n > 0 { format!("►{}/{n}", t.get_nt_name(*v)) } else { format!("►{}", t.get_nt_name(*v)) },
                OpCode::Loop(f, n) => if *n > 0 { format!("●{f}/{n}") } else { format!("●{f}") },
                OpCode::Exit(f, n) => if *n > 0 { format!("◄{f}/{n}") } else { format!("◄{f}") },
                OpCode::End => "$".to_string(),
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_ext(&self, symbol_table: Option<&SymbolTable>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(t) = symbol_table {
            if let OpCode::T(_) = self {
                result.push_str(&format!("({ext})"));
            }
        }
        result
    }
}

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
    opcodes: Vec<Vec<OpCode>>,
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

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        Some(&self.symbol_table)
    }

    fn has_flags(&self, var: VarId, flags: u32) -> bool {
        self.flags[var as usize] & flags == flags
    }

    fn build_opcodes(&mut self) {
        const VERBOSE: bool = false;
        let num_t_str = self.factors.iter().map(|(v, f)| {
            let n = f.iter().filter(|s| self.symbol_table.is_terminal_variable(s)).count();
            assert!(n < 256);
            n as u16
        }
        ).to_vec();
        let mut var_factors = HashMap::<VarId, (VarId, VarId)>::new();
        for (factor_id, (var_id, factor)) in self.factors.iter().enumerate() {
            if VERBOSE {
                println!("{} -> {}",
                         Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                         factor.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "));
            }
            if let Some((a, b)) = var_factors.get_mut(var_id) {
                *b = factor_id as VarId;
            } else {
                var_factors.insert(*var_id, (factor_id as VarId, factor_id as VarId));
            }
            let factor_id = factor_id as VarId;
            let flags = self.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            if VERBOSE { println!("- {}", new.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut opcode = Vec::<OpCode>::new();
            let mut num_stack = num_t_str[factor_id as usize];
            let mut fl = flags;
            let mut child_var = *var_id;
            while fl & ruleflag::CHILD_L_FACTOR != 0 {
                let par_var = self.parent[child_var as usize].unwrap();
                fl = self.flags[par_var as usize];
                let factors = var_factors[&par_var];
                if VERBOSE {
                    println!("  // child var {}: parent var {} has factors {} to {}",
                             Symbol::NT(child_var).to_str(self.get_symbol_table()),
                             Symbol::NT(par_var).to_str(self.get_symbol_table()), factors.0, factors.1);
                }
                let calling_factor = (factors.0 ..= factors.1).find(|f| {
                    let ok = self.factors[*f as usize].1.iter().any(|s| s == &Symbol::NT(child_var));
                    if VERBOSE {
                        println!("  // is factor {f} calling {}? {ok}: {}", Symbol::NT(child_var).to_str(self.get_symbol_table()),
                            self.factors[*f as usize].1.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")
                        )
                    }
                    ok
                }).unwrap();
                if VERBOSE {
                    println!("  - var {} ({child_var}) is called by factor {calling_factor} -> num_stack = {num_stack} + {}",
                             Symbol::NT(par_var).to_str(self.get_symbol_table()),
                             num_t_str[calling_factor as usize]);
                }
                num_stack += num_t_str[calling_factor as usize];
                child_var = par_var;
            }

            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                let parent = self.parent[*var_id as usize].unwrap();
                if new.get(0) == Some(&Symbol::NT(parent)) {
                    opcode.push(OpCode::Loop(parent, num_stack));
                    new.remove(0);
                }
            }
            if flags & ruleflag::PARENT_L_FACTOR == 0 || new.iter().all(|s| if let Symbol::NT(ch) = s { !self.has_flags(*ch, ruleflag::CHILD_L_FACTOR) } else { true }) {
                opcode.push(OpCode::Exit(factor_id, num_stack)); // will be popped when this NT is completed
            }

            todo!();
            // opcode.extend(new);

            if opcode.get(1).map(|op| op.matches(stack_sym)).unwrap_or(false)
                // || matches!(opcode.get(1), Some(Symbol::NT(child)) if self.has_flags(*child, ruleflag::CHILD_L_RECURSION) && !self.has_flags(*child, ruleflag::CHILD_AMBIGUITY))
            {
                opcode.swap(0, 1);
            }

            opcode.iter_mut().for_each(|o| {
                if let OpCode::NT(v, n) = o {
                    if v == var_id {
                        *o = OpCode::Loop(*v, *n)
                    }
                }
            });
            if VERBOSE { println!("- {}", opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            self.opcodes.push(opcode);
        }
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
        stack.push(OpCode::NT(self.start, 0));
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
                (OpCode::NT(var, n), _) | (OpCode::Loop(var, n), _) => {
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
                    let t_str = stack_t.drain(stack_t.len() - n as usize..).to_vec();
                    if VERBOSE {
                        let f = &self.factors[factor_id as usize];
                        println!("- to stack: [{}]", self.opcodes[factor_id as usize].iter().filter(|s| !s.is_empty()).map(|s| s.to_str(sym_table)).join(" "));
                        println!("- {} {} -> {} ({}): [{}]", if stack_sym.is_loop() { "LOOP" } else { "ENTER" },
                                 Symbol::NT(f.0).to_str(sym_table), f.1.to_str(sym_table), t_str.len(), t_str.iter().join(" "));
                    }
                    listener.switch(call, var, factor_id, t_str);
                    let new = self.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
                    stack.extend(self.opcodes[factor_id as usize].clone());
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::Exit(factor_id, n), _) => {
                    let var = self.factors[factor_id as usize].0;
                    let t_str = stack_t.drain(stack_t.len() - n as usize..).to_vec();
                    if VERBOSE { println!("- EXIT {} syn ({}): [{}]", Symbol::NT(var).to_str(sym_table), t_str.len(), t_str.iter().join(" ")); }
                    listener.switch(Call::Exit, var, factor_id, t_str);
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::T(sk), Symbol::T(sr)) => {
                    if sk != sr {
                        return Err(format!("unexpected character: '{}' instead of '{}'", stream_sym.to_str(sym_table), stack_sym.to_str(sym_table)));
                    }
                    if VERBOSE { println!("- MATCH {}", stream_sym.to_str(sym_table)); }
                    if self.symbol_table.is_terminal_variable(&Symbol::T(sk)) {
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

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Generates an `OpCode` instance.
    ///
    /// # Examples
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::opcode;
    /// # use rlexer::grammar::VarId;
    /// # use rlexer::parser::OpCode;
    /// assert_eq!(opcode!(e), OpCode::Empty);
    /// assert_eq!(opcode!(t 2), OpCode::T(2 as TokenId));
    /// assert_eq!(opcode!(nt 3/5), OpCode::NT(3, 5));
    /// assert_eq!(opcode!(loop 2/4), OpCode::Loop(2, 4));
    /// assert_eq!(opcode!(exit 1/3), OpCode::Exit(1, 3));
    /// assert_eq!(opcode!(nt 3), OpCode::NT(3, 0));
    /// assert_eq!(opcode!(loop 2), OpCode::Loop(2, 0));
    /// assert_eq!(opcode!(exit 1), OpCode::Exit(1, 0));
    /// assert_eq!(opcode!(end), OpCode::End);
    #[macro_export(local_inner_macros)]
    macro_rules! opcode {
        (e) => { OpCode::Empty };
        (t $id:literal) => { OpCode::T($id as TokenId) };
        (nt $id:literal / $num:expr) => { OpCode::NT($id as VarId, $num as u16) };
        (loop $id:literal / $num:expr) => { OpCode::Loop($id as VarId, $num as u16) };
        (exit $id:literal / $num:expr) => { OpCode::Exit($id as VarId, $num as u16) };
        (nt $id:literal) => { OpCode::NT($id as VarId, 0) };
        (loop $id:literal) => { OpCode::Loop($id as VarId, 0) };
        (exit $id:literal) => { OpCode::Exit($id as VarId, 0) };
        (end) => { OpCode::End };
    }

    /// Generates an opcode strip. A strip is made up of `OpCode` items separated by a comma.
    ///
    /// # Example
    /// ```
    /// # use rlexer::dfa::TokenId;
    /// # use rlexer::grammar::{ProdFactor, Symbol, VarId};
    /// # use rlexer::{strip, opcode};
    /// # use rlexer::parser::OpCode;
    /// assert_eq!(strip!(nt 1/2, loop 5, t 3, e), vec![opcode!(nt 1/2), opcode!(loop 5/0), opcode!(t 3), opcode!(e)]);
    /// ```
    #[macro_export(local_inner_macros)]
    macro_rules! strip {
        () => { std::vec![] };
        ($($a:ident $($b:literal $(/ $num:expr)?)?,)+) => { strip![$($a $($b $(/ $num)?)?),+] };
        ($($a:ident $($b:literal $(/ $num:expr)?)?),*) => { std::vec![$(opcode!($a $($b $(/ $num)?)?)),*] };
    }
}