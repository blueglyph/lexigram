// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fmt::{Display, Formatter};
use crate::fixed_sym_table::{FixedSymTable, SymInfoTable};
use crate::{AltId, TokenId, VarId};
use crate::lexer::{Pos, PosSpan};
use crate::log::Logger;
use crate::alt::Alternative;

pub(crate) mod tests;

// ---------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Symbol {
    T(TokenId),         // terminal
    NT(VarId),          // non-terminal
    #[default] Empty,   // empty symbol
    End                 // end of stream
}

impl Symbol {
    pub fn is_end(&self) -> bool {
        matches!(self, Symbol::End)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Symbol::Empty)
    }

    pub fn is_t(&self) -> bool {
        matches!(self, Symbol::T(_))
    }

    pub fn is_nt(&self) -> bool {
        matches!(self, Symbol::NT(_))
    }

    pub fn to_str<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        symbol_table.map(|t| t.get_str(self)).unwrap_or(self.to_string())
    }

    /// Converts the symbol to string, using the symbol table if available, and
    /// surrounding it with quotes if it's a string literal.
    pub fn to_str_quote<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        symbol_table.map(|t| t.get_name_quote(self)).unwrap_or(format!("{}", self.to_string()))
    }

    pub fn to_str_name<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        symbol_table.map(|t| t.get_name(self)).unwrap_or(format!("{}", self.to_string()))
    }

    /// Converts the symbol to string, using the symbol table if available.
    pub fn to_str_ext<T: SymInfoTable>(&self, symbol_table: Option<&T>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(t) = symbol_table {
            if t.is_symbol_t_data(self) {
                result.push_str(&format!("({ext})"));
            }
        }
        result
    }

    /// Converts to symbols used in [`sym!`](macro@sym) and other related macros.
    pub fn to_macro_item(&self) -> String {
        match self {
            Symbol::Empty => "e".to_string(),
            Symbol::T(x) => format!("t {x}"),
            Symbol::NT(x) => format!("nt {x}"),
            Symbol::End => "end".to_string(),
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Empty => write!(f, "ε"),
            Symbol::T(id) => write!(f, ":{id}"),
            Symbol::NT(id) => write!(f, "{id}"),
            Symbol::End => write!(f, "$"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum OpCode {
    Empty,              // empty symbol
    T(TokenId),         // terminal
    NT(VarId),          // nonterminal
    Loop(VarId),        // loop to same nonterminal
    Exit(VarId),        // exit nonterminal
    Hook,               // terminal hook callback
    End,                // end of stream
}


impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Empty => write!(f, "ε"),
            OpCode::T(t) => write!(f, ":{t}"),
            OpCode::NT(v) => write!(f, "►{v}"),
            OpCode::Loop(v) => write!(f, "●{v}"),
            OpCode::Exit(v) => write!(f, "◄{v}"),
            OpCode::Hook => write!(f, "▲"),
            OpCode::End => write!(f, "$"),
        }
    }
}

impl OpCode {
    pub fn is_loop(&self) -> bool {
        matches!(self, OpCode::Loop(_))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, OpCode::Empty)
    }

    pub fn has_span(&self) -> bool {
        matches!(self, OpCode::T(_) | OpCode::NT(_))
    }

    pub fn matches(&self, s: Symbol) -> bool {
        match self {
            OpCode::Empty => s == Symbol::Empty,
            OpCode::T(t) => s == Symbol::T(*t),
            OpCode::NT(v) => s == Symbol::NT(*v),
            OpCode::End => s == Symbol::End,
            OpCode::Loop(_)
            | OpCode::Exit(_)
            | OpCode::Hook => false,
        }
    }

    pub fn to_str<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        if let Some(t) = symbol_table {
            match self {
                OpCode::Empty => "ε".to_string(),
                OpCode::T(v) => format!("{}{}", t.get_t_str(*v), if t.is_token_data(*v) { "!" } else { "" }),
                OpCode::NT(v) => format!("►{}", t.get_nt_name(*v)),
                OpCode::Loop(v) => format!("●{}", t.get_nt_name(*v)),
                OpCode::Exit(f) => format!("◄{f}"),
                OpCode::Hook => "▲".to_string(),
                OpCode::End => "$".to_string(),
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_name<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        if let Some(tbl) = symbol_table {
            match self {
                OpCode::T(v) => format!("{}", tbl.get_t_str(*v)),
                _ => self.to_str(symbol_table),
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_quote<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        if let Some(t) = symbol_table {
            match self {
                OpCode::T(v) => format!("{}{}", Symbol::T(*v).to_str_quote(symbol_table), if t.is_token_data(*v) { "!" } else { "" }),
                _ => self.to_str(symbol_table)
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_ext<T: SymInfoTable>(&self, symbol_table: Option<&T>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(t) = symbol_table {
            if let OpCode::T(tok) = self {
                if t.is_symbol_t_data(&Symbol::T(*tok)) {
                    result.push_str(&format!("({ext})"));
                }
            }
        }
        result
    }
}

impl From<Symbol> for OpCode {
    fn from(value: Symbol) -> Self {
        match value {
            Symbol::Empty => OpCode::Empty,
            Symbol::T(t) => OpCode::T(t),
            Symbol::NT(v) => OpCode::NT(v),
            Symbol::End => OpCode::End,
        }
    }
}

#[cfg(feature = "test_utils")]
impl OpCode {
    pub fn to_macro_item(&self) -> String {
        match self {
            OpCode::Empty => "e".to_string(),
            OpCode::T(t) => format!("t {t}"),
            OpCode::NT(v) => format!("nt {v}"),
            OpCode::Loop(v) => format!("loop {v}"),
            OpCode::Exit(v) => format!("exit {v}"),
            OpCode::Hook => "hook".to_string(),
            OpCode::End => "end".to_string(),
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[derive(PartialEq, Debug)]
pub enum Call { Enter, Loop, Exit, End }

pub trait ListenerWrapper {
    /// Calls the listener to execute Enter, Loop, Exit, and End actions.
    fn switch(&mut self, _call: Call, _nt: VarId, _alt_id: AltId, _t_data: Option<Vec<String>>) {}
    /// Checks if the wrapper requests an abort. This happens if an error is too difficult to recover from
    /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
    fn check_abort_request(&self) -> bool { false }
    /// Aborts the parsing.
    fn abort(&mut self) {}
    /// Gets access to the listener's log to report possible errors and information about the parsing.
    fn get_mut_log(&mut self) -> &mut impl Logger;
    /// Pushes a location span onto the (optional) span stack
    fn push_span(&mut self, _span: PosSpan) {}
    /// Checks that the stack is empty (the parser only checks that the stack is empty after successfully parsing a text)
    fn is_stack_empty(&self) -> bool { true }
    /// Checks that the stack_t is empty (the parser only checks that the stack is empty after successfully parsing a text)
    fn is_stack_t_empty(&self) -> bool { true }
    /// Checks that the stack_span is empty (the parser only checks that the stack is empty after successfully parsing a text)
    fn is_stack_span_empty(&self) -> bool { true }
    /// Allows to dynamically translates a token
    #[allow(unused_variables)]
    fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        token
    }
    #[allow(unused_variables)]
    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        token
    }
}

// ---------------------------------------------------------------------------------------------

pub type ParserToken = (TokenId, String, PosSpan);

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

pub struct Parser<'a> {
    num_nt: usize,
    num_t: usize,
    alt_var: &'a [VarId],
    alts: Vec<Alternative>,
    opcodes: Vec<Vec<OpCode>>,
    init_opcodes: Vec<OpCode>,
    table: &'a [AltId],
    symbol_table: FixedSymTable,
    start: VarId,
    try_recover: bool,          // tries to recover from syntactical errors
}

impl<'a> Parser<'a> {
    /// Maximum number of error recoveries attempted when meeting a syntax error
    pub const MAX_NBR_RECOVERS: u32 = 5;
    pub const MAX_NBR_LEXER_ERRORS: u32 = 3;

    pub fn new(
        num_nt: usize,
        num_t: usize,
        alt_var: &'a [VarId],
        alts: Vec<Alternative>,
        opcodes: Vec<Vec<OpCode>>,
        init_opcodes: Vec<OpCode>,
        table: &'a [AltId],
        symbol_table: FixedSymTable,
        start: VarId,
    ) -> Self {
        Parser { num_nt, num_t, alt_var, alts, opcodes, init_opcodes, table, symbol_table, start, try_recover: true }
    }

    pub fn get_symbol_table(&self) -> Option<&FixedSymTable> {
        Some(&self.symbol_table)
    }

    pub fn set_start(&mut self, start: VarId) {
        assert!(self.num_nt > start as usize);
        self.start = start;
    }

    pub fn set_try_recover(&mut self, try_recover: bool) {
        self.try_recover = try_recover;
    }

    /// Determines with a quick simulation if `sym` is accepted by the grammar with the current
    /// `stack` and current stack symbol `stack_sym`.
    fn simulate(&self, stream_sym: Symbol, mut stack: Vec<OpCode>, mut stack_sym: OpCode) -> bool {
        const VERBOSE: bool = false;
        let error_skip_alt_id = self.alt_var.len() as AltId;
        let end_var_id = (self.num_t - 1) as VarId;
        if VERBOSE { print!("  next symbol could be: {}?", stream_sym.to_str(self.get_symbol_table())); }

        let ok = loop {
            match (stack_sym, stream_sym) {
                (OpCode::NT(var), _) | (OpCode::Loop(var), _) => {
                    let sr = if let Symbol::T(sr) = stream_sym { sr } else { end_var_id };
                    let alt_id = self.table[var as usize * self.num_t + sr as usize];
                    if alt_id >= error_skip_alt_id {
                        break false;
                    }
                    stack.extend(self.opcodes[alt_id as usize].clone());
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::Exit(_), _) => {
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::T(sk), Symbol::T(sr)) => {
                    break sk == sr;
                }
                (OpCode::End, Symbol::End) => {
                    break true;
                }
                (_, _) => {
                    break false;
                }
            }
        };
        if VERBOSE { println!(" {}", if ok { "yes" } else { "no" }); }
        ok
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

        /// Delays the capture of the next token and the call to `intercept_token()` if it's possible.
        /// That allows to call as many `exit_*()` methods as possible in the listener, and so to
        /// update any information that may impact the translation of the next token.
        const DELAY_STREAM_INTERCEPTION: bool = cfg!(feature = "delay_stream_interception");

        let sym_table: Option<&FixedSymTable> = Some(&self.symbol_table);
        let mut stack = self.init_opcodes.clone();
        let mut stack_t = Vec::<String>::new();
        let error_skip_alt_id = self.alt_var.len() as AltId;
        let error_pop_alt_id = error_skip_alt_id + 1;
        if VERBOSE { println!("skip = {error_skip_alt_id}, pop = {error_pop_alt_id}"); }
        let mut recover_mode = false;
        let mut nbr_recovers = 0;
        let mut nbr_lexer_errors = 0;
        let end_var_id = (self.num_t - 1) as VarId;
        let mut stack_sym = stack.pop().unwrap();
        let mut stream_n = 0;
        let mut stream_pos = None;
        let mut stream_span = PosSpan::empty();
        let mut stream_sym = Symbol::default(); // must set fake value to comply with borrow checker
        let mut stream_str = String::default(); // must set fake value to comply with borrow checker
        let mut advance_stream = true;
        let mut hook_active = false;
        loop {
            if advance_stream &&
                (!DELAY_STREAM_INTERCEPTION                     // if optimization == false, only checks advance_stream
                    || (!matches!(stack_sym, OpCode::Exit(_))   // exit => needn't advance, unless...
                    || stream_sym == Symbol::Empty))            // Symbol::Empty => must advance no matter what
            {
                stream_n += 1;
                (stream_sym, stream_str) = stream.next().map(|(t, s, span)| {
                    // reads the next token and possibly transforms it in intercept_token() if it's used
                    // (if intercept_token() isn't used, it's optimized away)
                    let new_t = wrapper.intercept_token(t, &s, &span);
                    stream_pos = Some(span.first_forced());
                    stream_span = span;
                    (Symbol::T(new_t), s)
                }).unwrap_or_else(|| {
                    // checks if there's an error code after the end
                    if let Some((_t, s, _span)) = stream.next() {
                        (Symbol::Empty, s)
                    } else {
                        (Symbol::End, String::new())
                    }
                });
                advance_stream = false;
                hook_active = true;
            }
            if VERBOSE {
                println!("{:-<40}", "");
                println!("input ({stream_n}{}): {}   stack_t: [{}]   stack: [{}]   current: {}",
                         if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() },
                         stream_sym.to_str_ext(sym_table, &stream_str),
                         stack_t.join(", "),
                         stack.iter().map(|s| s.to_str(sym_table)).collect::<Vec<_>>().join(" "),
                         stack_sym.to_str_name(sym_table));
            }
            match (stack_sym, stream_sym) {
                (_, Symbol::Empty) => {
                    // lexer couldn't recognize the next symbol
                    if VERBOSE { println!("lexer error: {stream_str}"); }
                    wrapper.get_mut_log().add_error(format!("lexical error: {stream_str}"));
                    nbr_lexer_errors += 1;
                    if nbr_lexer_errors >= Self::MAX_NBR_LEXER_ERRORS {
                        wrapper.get_mut_log().add_note(format!("too many lexical errors ({nbr_lexer_errors}), giving up"));
                        wrapper.abort();
                        return Err(ParserError::TooManyErrors);
                    }
                    advance_stream = true;
                }
                (OpCode::Hook, Symbol::T(t)) => {
                    if hook_active {
                        let new_t = wrapper.hook(t, stream_str.as_str(), &stream_span);
                        stream_sym = Symbol::T(new_t);
                        hook_active = false;
                    }
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::Hook, _) => {
                    // hooks may happen on other alternative symbols, in which case they're irrelevant
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::NT(var), _) | (OpCode::Loop(var), _) => {
                    let sr = if let Symbol::T(sr) = stream_sym { sr } else { end_var_id };
                    let alt_id = self.table[var as usize * self.num_t + sr as usize];
                    if VERBOSE {
                        println!("- table[{var}, {sr}] = {alt_id}: {} -> {}",
                                 Symbol::NT(var).to_str(self.get_symbol_table()),
                                 if alt_id >= error_skip_alt_id {
                                     "ERROR".to_string()
                                 } else {
                                     if let Some(a) = self.alts.get(alt_id as usize) {
                                         a.to_str(sym_table)
                                     } else {
                                         "(alternative)".to_string()
                                     }
                                 });
                    }
                    if !recover_mode && alt_id >= error_skip_alt_id {
                        let expected = (0..self.num_t as VarId).filter(|t| self.table[var as usize * self.num_t + *t as usize] < error_skip_alt_id)
                            .filter(|t| self.simulate(Symbol::T(*t), stack.clone(), stack_sym))
                            .into_iter().map(|t| format!("'{}'", if t < end_var_id { Symbol::T(t).to_str(sym_table) } else { "<EOF>".to_string() }))
                            .collect::<Vec<_>>().join(", ");
                        let stream_sym_txt = if stream_sym.is_end() { "end of stream".to_string() } else { format!("input '{}'", stream_sym.to_str(sym_table)) };
                        let msg = format!("syntax error: found {stream_sym_txt} instead of {expected} while parsing '{}'{}",
                                          stack_sym.to_str(sym_table),
                                          if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                        if self.try_recover {
                            wrapper.get_mut_log().add_error(msg);
                            if nbr_recovers >= Self::MAX_NBR_RECOVERS {
                                wrapper.get_mut_log().add_note(format!("too many errors ({nbr_recovers}), giving up"));
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
                        if VERBOSE { println!("!NT {} <-> {}, alt_id = {alt_id}", stack_sym.to_str(self.get_symbol_table()), stream_sym.to_str(self.get_symbol_table())); }
                        if alt_id == error_skip_alt_id {
                            if stream_sym == Symbol::End {
                                let msg = "irrecoverable error, reached end of stream".to_string();
                                if VERBOSE { println!("(recovering) {msg}"); }
                                wrapper.get_mut_log().add_note(msg);
                                wrapper.abort();
                                return Err(ParserError::Irrecoverable);
                            }
                            if VERBOSE { println!("(recovering) skipping token {}", stream_sym.to_str(self.get_symbol_table())); }
                            advance_stream = true;
                        } else if alt_id == error_pop_alt_id {
                            if VERBOSE { println!("(recovering) popping {}", stack_sym.to_str(self.get_symbol_table())); }
                            stack_sym = stack.pop().unwrap();
                        } else {
                            if alt_id < error_skip_alt_id {
                                recover_mode = false;
                                let pos_str = if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() };
                                wrapper.get_mut_log().add_note(format!("resynchronized on '{}'{pos_str}",
                                                                       stream_sym.to_str(self.get_symbol_table())));
                                if VERBOSE { println!("(recovering) resynchronized{pos_str}"); }
                            } else {
                                panic!("illegal alt_id {alt_id}")
                            }
                        }
                    }
                    if !recover_mode {
                        let call = if stack_sym.is_loop() { Call::Loop } else { Call::Enter };
                        let t_data = std::mem::take(&mut stack_t);
                        if VERBOSE {
                            let f_str = if let Some(f) = &self.alts.get(alt_id as usize) {
                                f.to_str(sym_table)
                            } else {
                                "(alternative)".to_string()
                            };
                            println!(
                                "- to stack: [{}]",
                                self.opcodes[alt_id as usize].iter().filter(|s| !s.is_empty()).map(|s| s.to_str(sym_table))
                                    .collect::<Vec<_>>().join(" "));
                            println!(
                                "- {} {} -> {f_str} ({}): [{}]",
                                if stack_sym.is_loop() { "LOOP" } else { "ENTER" },
                                Symbol::NT(self.alt_var[alt_id as usize]).to_str(sym_table), t_data.len(), t_data.iter()
                                    .cloned().collect::<Vec<_>>().join(" "));
                        }
                        if nbr_recovers == 0 {
                            wrapper.switch(call, var, alt_id, Some(t_data));
                        }
                        stack.extend(self.opcodes[alt_id as usize].clone());
                        stack_sym = stack.pop().unwrap();
                    }
                }
                (OpCode::Exit(alt_id), _) => {
                    let var = self.alt_var[alt_id as usize];
                    let t_data = std::mem::take(&mut stack_t);
                    if VERBOSE {
                        println!(
                            "- EXIT {} syn ({}): [{}]",
                            Symbol::NT(var).to_str(sym_table), t_data.len(), t_data.iter()
                                .cloned().collect::<Vec<_>>().join(" "));
                    }
                    if nbr_recovers == 0 {
                        wrapper.switch(Call::Exit, var, alt_id, Some(t_data));
                    }
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::T(sk), Symbol::T(sr)) => {
                    if !recover_mode && sk != sr {
                        let msg = format!(
                            "syntax error: found input '{}' instead of '{}'{}",
                            stream_sym.to_str(sym_table),
                            Symbol::T(sk).to_str(sym_table),
                            if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                        if self.try_recover {
                            wrapper.get_mut_log().add_error(msg);
                            if nbr_recovers >= Self::MAX_NBR_RECOVERS {
                                wrapper.get_mut_log().add_note(format!("too many errors ({nbr_recovers}), giving up"));
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
                        if VERBOSE { println!("!T {} <-> {}", Symbol::T(sk).to_str(self.get_symbol_table()), stream_sym.to_str(self.get_symbol_table())); }
                        if sk == sr {
                            recover_mode = false;
                            let pos_str = if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() };
                            wrapper.get_mut_log().add_note(format!("resynchronized on '{}'{pos_str}",
                                                                   stream_sym.to_str(self.get_symbol_table())));
                            if VERBOSE { println!("(recovering) resynchronized{pos_str}"); }
                        } else {
                            if VERBOSE { println!("(recovering) popping {}", Symbol::T(sk).to_str(self.get_symbol_table())); }
                            stack_sym = stack.pop().unwrap();
                        }
                    }
                    if !recover_mode {
                        if VERBOSE { println!("- MATCH {}", stream_sym.to_str(sym_table)); }
                        if self.symbol_table.is_token_data(sk) {
                            stack_t.push(std::mem::take(&mut stream_str)); // must use take() to comply with borrow checker
                        }
                        stack_sym = stack.pop().unwrap();
                        wrapper.push_span(stream_span.take());
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
                    wrapper.get_mut_log().add_error(format!("syntax error: found end of stream instead of '{}'", stack_sym.to_str_name(sym_table)));
                    wrapper.abort();
                    return Err(ParserError::UnexpectedEOS);
                }
                (_, _) => {
                    wrapper.get_mut_log().add_error(format!("unexpected syntax error: input '{}' while expecting '{}'{}",
                                                            stream_sym.to_str(sym_table), stack_sym.to_str_name(sym_table),
                                                            if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() }));
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
        assert!(stack.is_empty(), "stack: {}", stack.iter().map(|s| s.to_str(sym_table)).collect::<Vec<_>>().join(", "));
        if nbr_recovers == 0 {
            assert!(wrapper.is_stack_empty(), "symbol stack isn't empty");
            assert!(wrapper.is_stack_t_empty(), "text stack isn't empty");
            assert!(wrapper.is_stack_span_empty(), "span stack isn't empty");
            Ok(())
        } else {
            // when nbr_recovers > 0, we know that at least one error has been reported to the log, no need to add one here
            wrapper.abort();
            Err(ParserError::EncounteredErrors)
        }
    }
}

#[cfg(feature = "test_utils")]
impl<'a> Parser<'a> {
    pub fn get_alt_var(&self) -> &[VarId] {
        self.alt_var
    }

    pub fn get_alts(&self) -> &Vec<Alternative> {
        &self.alts
    }

    pub fn get_opcodes(&self) -> &Vec<Vec<OpCode>> {
        &self.opcodes
    }
}
