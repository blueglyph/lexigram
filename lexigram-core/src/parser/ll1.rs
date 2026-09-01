// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use crate::alt::Alternative;
use crate::parser::{terminal_to_str_type, Call, ListenerWrapper, OpCode, ParserError, ParserToken, Symbol, Terminate};
use crate::{AltId, VarId};
use crate::fixed_sym_table::{FixedSymTable, SymInfoTable};
use crate::lexer::{Pos, PosSpan};
use crate::log::LogMsg;

/// LL(1) parser object. The [new(...)](LLParser::new) method creates a new instance.
pub struct LLParser<'a> {
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

impl<'a> LLParser<'a> {
    /// Maximum number of error recoveries attempted when meeting a syntax error
    pub const MAX_NBR_PARSER_ERRORS: u32 = 5;
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
        LLParser { num_nt, num_t, alt_var, alts, opcodes, init_opcodes, table, symbol_table, start, try_recover: true }
    }

    /// Gets a reference to the symbol table, if one is attached.
    pub fn get_symbol_table(&self) -> Option<&FixedSymTable> {
        Some(&self.symbol_table)
    }

    /// Sets the top nonterminal. The parser ends the parsing once the corresponding rule has been entirely parsed.
    pub fn set_start(&mut self, start: VarId) {
        assert!(self.num_nt > start as usize);
        self.start = start;
    }

    /// Enables or disables the recovery from syntactic or lexical errors.
    ///
    /// See also [ParserError::TooManyErrors] and [ParserError::SyntaxError].
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

    /// Parses the entire `stream`, calling the (listener) [wrapper](ListenerWrapper) with the
    /// [actions](Call) that correspond to the parser events.
    ///
    /// Returns `Ok(())` if the whole stream could be successfully parsed, or an
    /// [error](ParserError) if it couldn't.
    ///
    /// All errors are reported in the wrapper's log. Usually, the wrapper simply transmits the
    /// reports to the user listener's log (done in the generated code).
    pub fn parse_stream<I, L>(&mut self, wrapper: &mut L, mut stream: I) -> Result<(), ParserError>
        where I: Iterator<Item=ParserToken>,
              L: ListenerWrapper,
    {
        /// Outputs debug messages on stdout.
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
        let mut nbr_parser_errors = 0;
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
                    if let Some((_t, s, span)) = stream.next() {
                        stream_span = span;
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
                    wrapper.report(Some(&stream_span), LogMsg::Error(format!("lexical error: {stream_str}")));
                    if nbr_lexer_errors >= Self::MAX_NBR_LEXER_ERRORS {
                        wrapper.report(None, LogMsg::Note(format!("too many lexical errors ({nbr_lexer_errors}), giving up")));
                        wrapper.abort();
                        return Err(ParserError::TooManyErrors);
                    }
                    nbr_lexer_errors += 1;
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
                                 } else if let Some(a) = self.alts.get(alt_id as usize) {
                                     a.to_str(sym_table)
                                 } else {
                                     "(alternative)".to_string()
                                 });
                    }
                    if !recover_mode && alt_id >= error_skip_alt_id {
                        let expected = (0..self.num_t as VarId).filter(|t| self.table[var as usize * self.num_t + *t as usize] < error_skip_alt_id)
                            .filter(|t| self.simulate(Symbol::T(*t), stack.clone(), stack_sym))
                            .map(|t| if t < end_var_id { Symbol::T(t).to_str_quote(sym_table) } else { "<EOF>".to_string() })
                            .collect::<Vec<_>>().join(", ");
                        let msg = format!(
                            "syntax error: found {} instead of {expected} while parsing {}{}",
                            stream_sym.to_str_type(sym_table, &stream_str),
                            stack_sym.to_str(sym_table),
                            if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                        if self.try_recover {
                            wrapper.report(Some(&stream_span), LogMsg::Error(msg));
                            if nbr_parser_errors >= Self::MAX_NBR_PARSER_ERRORS {
                                wrapper.report(None, LogMsg::Note(format!("too many errors ({nbr_parser_errors}), giving up")));
                                wrapper.abort();
                                return Err(ParserError::TooManyErrors);
                            }
                            nbr_parser_errors += 1;
                            recover_mode = true;
                        } else {
                            wrapper.report(Some(&stream_span), LogMsg::Error(msg));
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
                                wrapper.report(None, LogMsg::Note(msg));
                                wrapper.abort();
                                return Err(ParserError::Irrecoverable);
                            }
                            if VERBOSE { println!("(recovering) skipping token {}", stream_sym.to_str(self.get_symbol_table())); }
                            advance_stream = true;
                        } else if alt_id == error_pop_alt_id {
                            if VERBOSE { println!("(recovering) popping {}", stack_sym.to_str(self.get_symbol_table())); }
                            stack_sym = stack.pop().unwrap();
                        } else if alt_id < error_skip_alt_id {
                            recover_mode = false;
                            let pos_str = if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() };
                            wrapper.report(None, LogMsg::Note(format!("resynchronized on '{}'{pos_str}", stream_sym.to_str(self.get_symbol_table()))));
                            if VERBOSE { println!("(recovering) resynchronized{pos_str}"); }
                        } else {
                            panic!("illegal alt_id {alt_id}")
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
                                Symbol::NT(self.alt_var[alt_id as usize]).to_str(sym_table), t_data.len(), t_data.join(" "));
                        }
                        if nbr_parser_errors == 0 {
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
                            Symbol::NT(var).to_str(sym_table), t_data.len(), t_data.join(" "));
                    }
                    if nbr_parser_errors == 0 {
                        wrapper.switch(Call::Exit, var, alt_id, Some(t_data));
                    }
                    stack_sym = stack.pop().unwrap();
                }
                (OpCode::T(sk), Symbol::T(sr)) => {
                    if !recover_mode && sk != sr {
                        let msg = format!(
                            "syntax error: found input {} instead of {}{}",
                            terminal_to_str_type(sr, sym_table, &stream_str),
                            Symbol::T(sk).to_str_quote(sym_table),
                            if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                        if self.try_recover {
                            wrapper.report(Some(&stream_span), LogMsg::Error(msg));
                            if nbr_parser_errors >= Self::MAX_NBR_PARSER_ERRORS {
                                wrapper.report(None, LogMsg::Note(format!("too many errors ({nbr_parser_errors}), giving up")));
                                wrapper.abort();
                                return Err(ParserError::TooManyErrors);
                            }
                            nbr_parser_errors += 1;
                            recover_mode = true;
                        } else {
                            wrapper.report(Some(&stream_span), LogMsg::Error(msg));
                            wrapper.abort();
                            return Err(ParserError::SyntaxError);
                        }
                    }
                    if recover_mode {
                        if VERBOSE { println!("!T {} <-> {}", Symbol::T(sk).to_str(self.get_symbol_table()), stream_sym.to_str(self.get_symbol_table())); }
                        if sk == sr {
                            recover_mode = false;
                            let pos_str = if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() };
                            wrapper.report(Some(&stream_span), LogMsg::Note(format!("resynchronized on '{}'{pos_str}", stream_sym.to_str(self.get_symbol_table()))));
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
                    if nbr_parser_errors == 0 {
                        wrapper.switch(Call::End(Terminate::None), 0, 0, None);
                    }
                    break;
                }
                (OpCode::End, _) => {
                    wrapper.report(Some(&stream_span), LogMsg::Error(format!(
                        "syntax error: found extra symbol {} after end of parsing",
                        stream_sym.to_str_type(sym_table, &stream_str))));
                    wrapper.abort();
                    return Err(ParserError::ExtraSymbol);
                }
                (_, Symbol::End) => {
                    wrapper.report(None, LogMsg::Error(format!("syntax error: found end of stream instead of {}", stack_sym.to_str_name(sym_table))));
                    wrapper.abort();
                    return Err(ParserError::UnexpectedEOS);
                }
                (_, _) => {
                    let text = format!(
                        "unexpected syntax error: {} while expecting {}{}",
                        stream_sym.to_str_type(sym_table, &stream_str), stack_sym.to_str_name(sym_table),
                        if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() });
                    wrapper.report(Some(&stream_span), LogMsg::Error(text));
                    wrapper.abort();
                    return Err(ParserError::UnexpectedError);
                }
            }
            match wrapper.check_abort_request() {
                Terminate::None => {}
                terminate @ (Terminate::Abort | Terminate::Conclude) => {
                    if VERBOSE { println!("detected {terminate:?}"); }
                    stack_t.clear();
                    stack.clear();
                    wrapper.abort();
                    if nbr_parser_errors == 0 {
                        wrapper.switch(Call::End(terminate), 0, 0, None);
                    }
                    if terminate == Terminate::Abort {
                        return Err(ParserError::AbortRequest);
                    } else {
                        break;
                    }
                }
            }
        }
        assert!(stack_t.is_empty(), "stack_t: {}", stack_t.join(", "));
        assert!(stack.is_empty(), "stack: {}", stack.iter().map(|s| s.to_str(sym_table)).collect::<Vec<_>>().join(", "));
        if nbr_parser_errors == 0 && nbr_lexer_errors == 0 {
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
impl<'a> LLParser<'a> {
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