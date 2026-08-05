// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use crate::{AltId, CollectJoin, TokenId, VarId};
use crate::fixed_sym_table::{FixedSymTable, SymInfoTable};
use crate::lexer::{Pos, PosSpan};
use crate::log::LogMsg;
use crate::parser::{terminal_to_str_type, Call, ListenerWrapper, ParserError, ParserToken, Symbol, Terminate};

/// State index
pub type LRStateId = u16;

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub enum LRAction {
    #[default]
    Error,
    Shift(LRStateId),
    ShiftHook(LRStateId),
    Reduce(AltId),
    Accept,
}

impl LRAction {
    pub fn is_hook(&self) -> bool {
        matches!(self, LRAction::ShiftHook(_))
    }

    pub fn is_action(&self) -> bool {
        match self {
            LRAction::Error => false,
            LRAction::Shift(_)
            | LRAction::ShiftHook(_)
            | LRAction::Reduce(_)
            | LRAction::Accept => true,
        }
    }
}

impl Display for LRAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LRAction::Error => write!(f, "-"),
            LRAction::Shift(s) => write!(f, "s{s}"),
            LRAction::ShiftHook(s) => write!(f, "${s}"),
            LRAction::Reduce(a) => write!(f, "r{a}"),
            LRAction::Accept => write!(f, "acc"),
        }
    }
}

/// Parser object. The [new(...)](LRParser::new) method creates a new instance.
pub struct LRParser<'a, T> {
    num_nt: usize,                          // doesn't include the goal NT
    num_t_full: usize,                      // includes the end symbol
    action: &'a [LRAction],
    goto: &'a [LRStateId],
    alt_nt_len: &'a [(VarId, u16, u16)],    // alt_id -> (nt, # symbols in alt, # terminals in alt)
    symbol_table: FixedSymTable,            // must include terminals <$> and <empty> at the end
    init_hook: bool,                        // first terminal must be intercepted
    _phantom: PhantomData<T>,
}

impl<'a, T> LRParser<'a, T> {
    pub const MAX_NBR_PARSER_ERRORS: u32 = 5;
    pub const MAX_NBR_LEXER_ERRORS: u32 = 3;

    pub fn new(
        num_nt: usize,
        num_t_full: usize,
        action: &'a [LRAction],
        goto: &'a [LRStateId],
        alt_nt_len: &'a [(VarId, u16, u16)],
        symbol_table: FixedSymTable,
        init_hook: bool
    ) -> Self {
        LRParser { num_nt, num_t_full, action, goto, alt_nt_len, symbol_table, init_hook, _phantom: PhantomData }
    }

    fn t_to_string(&self, t: TokenId) -> String {
        if self.num_t_full - 1 > t as usize {
            Symbol::T(t as TokenId).to_str_quote(Some(&self.symbol_table))
        } else {
            "<EOF>".to_string()
        }
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
        const VERBOSE: bool = true;
        const BEFORE: &str = "\u{1b}[33m";
        const AFTER : &str = "\u{1b}[0m";
        // const CALL_WRAPPER_AFTER_ERROR: bool = true;
        let sym_table: Option<&FixedSymTable> = Some(&self.symbol_table);
        let token_error = self.num_t_full as TokenId;
        let token_eof = token_error - 1;
        let mut error = None;
        let mut nbr_parser_errors = 0;
        let mut nbr_lexer_errors = 0;
        let mut state: LRStateId = 0;
        let mut stack_state = vec![state];
        let mut stack_t = vec![];
        let mut advance_stream = true;
        let mut stream_pos = None;
        let mut stream_span = PosSpan::empty();
        let mut stream_sym = TokenId::default();
        let mut stream_str = String::default();
        let mut call_wrapper = true;
        let mut hook = self.init_hook;
        loop {
            if advance_stream {
                (stream_sym, stream_str) = if let Some((t, s, span)) = stream.next() {
                    stream_pos = Some(span.first_forced());
                    stream_span = span;
                    if !hook {
                        let new_t = wrapper.intercept_token(t, &s, &stream_span);
                        (new_t, s)
                    } else {
                        hook = false;
                        let new_t = if nbr_parser_errors == 0 {
                            let new_t = wrapper.hook(t, s.as_str(), &stream_span);
                            if VERBOSE {
                                println!(
                                    "{BEFORE}hook changed {} to {}{AFTER}",
                                    Symbol::T(t).to_str(Some(&self.symbol_table)), Symbol::T(new_t).to_str(Some(&self.symbol_table)))
                            }
                            new_t
                        } else {
                            t
                        };
                        (new_t, s)
                    }
                } else {
                    if let Some((_t, s, _span)) = stream.next() {
                        // an error code after the end means an unrecognized sequence: we may try to continue
                        wrapper.report(Some(&stream_span), LogMsg::Error(format!("lexical error: {s}")));
                        nbr_lexer_errors += 1;
                        if nbr_lexer_errors <= Self::MAX_NBR_LEXER_ERRORS {
                            continue;
                        }
                        error = Some(ParserError::TooManyErrors);
                        break;
                    } else {
                        // end of stream
                        (token_eof, String::new())
                    }
                };
                advance_stream = false;
            }
            if VERBOSE {
                println!(
                    "{BEFORE}states [{}] {state}, stack_t [{}], input: {} ({stream_str:?}){AFTER}",
                    stack_state.iter().map(|s| s.to_string()).join(" "),
                    stack_t.iter().map(|s| format!("{s:?}")).join(", "),
                    Symbol::T(stream_sym).to_str(sym_table)
                );
            }
            let action = self.action[stream_sym as usize + state as usize * self.num_t_full];
            match action {
                LRAction::Shift(new_state) | LRAction::ShiftHook(new_state) => {
                    hook = action.is_hook();
                    if VERBOSE { println!("{BEFORE}- shift({new_state}){AFTER}"); }
                    stack_state.push(new_state);
                    if self.symbol_table.is_token_data(stream_sym) {
                        stack_t.push(std::mem::take(&mut stream_str));
                    }
                    if call_wrapper { wrapper.push_span(stream_span.take()); }
                    state = new_state;
                    advance_stream = true;
                }
                LRAction::Reduce(alt) => {
                    // alt: s -> ω
                    let (nt, alt_len, nbr_t) = self.alt_nt_len[alt as usize];   // s
                    stack_state.drain(stack_state.len() - alt_len as usize..);  // pop |ω| states
                    let pop_state = *stack_state.last().unwrap();
                    state = self.goto[nt as usize + pop_state as usize * self.num_nt];
                    stack_state.push(state);
                    if VERBOSE { println!("{BEFORE}- reduce({alt}) -> state {pop_state} -> goto {state}{AFTER}"); }
                    let t_data = stack_t.drain(stack_t.len() - nbr_t as usize..).to_vec();
                    if call_wrapper { wrapper.switch(Call::Exit, nt, alt, Some(t_data)); }
                }
                LRAction::Accept => {
                    if VERBOSE { println!("{BEFORE}- accept{AFTER}"); }
                    if call_wrapper { wrapper.switch(Call::End(Terminate::None), 0, 0, None); }
                    stack_state.pop();
                    stack_state.pop();
                    break;
                }
                LRAction::Error => {
                    let expected = (0..self.num_t_full)
                        .filter(|t| self.action[*t + state as usize * self.num_t_full].is_action())
                        .map(|t| self.t_to_string(t as TokenId))
                        .join(", ");
                    let sym_str = if stream_sym == token_eof {
                        "end of stream".to_string()
                    } else {
                        format!("input {}", terminal_to_str_type(stream_sym, sym_table, &stream_str))
                    };
                    let msg = format!(
                        "syntax error: unexpected {sym_str}{}{}",
                        if expected.is_empty() { String::new() } else { format!(" instead of {expected}") },
                        if let Some(Pos(line, col)) = stream_pos { format!(", line {line}, col {col}") } else { String::new() }
                    );
                    wrapper.report(Some(&stream_span), LogMsg::Error(msg));
                    nbr_parser_errors += 1;
                    if nbr_parser_errors > Self::MAX_NBR_PARSER_ERRORS {
                        error = Some(ParserError::TooManyErrors);
                        break;
                    }
                    let prev_call_wrapper = call_wrapper;
                    if let Some(new_state) = self.recover(
                        &mut stream,
                        &mut stream_sym,
                        &mut stream_str,
                        &mut stream_pos,
                        &mut stream_span,
                        wrapper,
                        &mut stack_state,
                        &mut stack_t,
                        &mut call_wrapper
                    ) {
                        let pos = if let Some(pos) = stream_pos { format!(" at {pos}") } else { String::new() };
                        wrapper.report(None, LogMsg::Note(format!("resynchronized from syntax error on {}{pos}", self.t_to_string(stream_sym))));
                        if prev_call_wrapper && !call_wrapper {
                            wrapper.report(None, LogMsg::Note("the rest of the stream will be parsed, but the listener interface can't be used any more".to_string()));
                        }
                        state = new_state;
                        stack_state.push(state);
                        if call_wrapper {
                            wrapper.syntax_error_recovered();
                        }
                    } else {
                        error = Some(ParserError::SyntaxError);
                        break;
                    }
                }
            }
            match wrapper.check_abort_request() {
                Terminate::None => {}
                terminate @ (Terminate::Abort | Terminate::Conclude) => {
                    if VERBOSE { println!("{BEFORE}detected {terminate:?}{AFTER}"); }
                    stack_state.clear();
                    wrapper.abort();
                    wrapper.switch(Call::End(terminate), 0, 0, None);
                    if terminate == Terminate::Abort {
                        return Err(ParserError::AbortRequest);
                    } else {
                        break;
                    }
                }
            }
        }
        if nbr_parser_errors == 0 && nbr_lexer_errors == 0 && error.is_none() {
            assert!(stack_t.is_empty(), "stack_t: {}", stack_t.join(", "));
            assert!(stack_state.is_empty(), "stack_state: {}", stack_state.iter().map(LRStateId::to_string).collect::<Vec<_>>().join(", "));
            assert!(wrapper.is_stack_empty(), "symbol stack isn't empty");
            assert!(wrapper.is_stack_t_empty(), "text stack isn't empty");
            assert!(wrapper.is_stack_span_empty(), "span stack isn't empty");
            Ok(())
        } else {
            let err = error.unwrap_or(ParserError::EncounteredErrors);
            wrapper.abort();
            Err(err)
        }
    }

    fn recover<I, L>(
        &self,
        stream: &mut I,
        stream_sym: &mut u16,
        stream_str: &mut String,
        stream_pos: &mut Option<Pos>,
        stream_span: &mut PosSpan,
        wrapper: &mut L,
        stack_state: &mut Vec<LRStateId>,
        stack_t: &mut Vec<String>,
        call_wrapper: &mut bool
    ) -> Option<LRStateId>
    where
        I: Iterator<Item=ParserToken>,
        L: ListenerWrapper,
    {
        const VERBOSE: bool = true;
        const BEFORE_ANSI: &str = "\u{1b}[31m";
        const AFTER_ANSI : &str = "\u{1b}[0m";
        if VERBOSE { println!("{BEFORE_ANSI}parser panic-mode recovery:\n- states {stack_state:?}{AFTER_ANSI}"); }
        let mut candidates = Vec::<(VarId, LRStateId)>::new();
        let mut goto_state = 0;
        // finds a state with a GOTO, but don't pop the start state
        while candidates.is_empty() && stack_state.len() > 1 && let Some(state) = stack_state.pop() {
            goto_state = state;
            let goto_row = state as usize * self.num_nt;
            // check the available GOTO states:
            candidates = self.goto.iter().skip(goto_row).take(self.num_nt).enumerate()
                .map(|(v, s)| (v as VarId, *s)).filter(|(_, s)| *s > 0)
                .collect();
            if VERBOSE {
                println!(
                    "{BEFORE_ANSI}- goto_state {goto_state}, candidates = {}{AFTER_ANSI}",
                    candidates.iter().map(|(v, st)| format!("({}, {st})", Symbol::NT(*v).to_str(Some(&self.symbol_table)))).join(", ")
                );
            }
            if candidates.is_empty() {
                let (sym, has_value) = L::get_state_symbol_and_value(state);
                match sym {
                    Symbol::T(_) => {
                        if has_value { stack_t.pop(); }
                    }
                    Symbol::NT(_) => {
                        if has_value && *call_wrapper { wrapper.pop_nt_value(); }
                    }
                    _ => panic!()
                }
            }
        }
        if VERBOSE {
            println!(
                "{BEFORE_ANSI}- candidates = {}{AFTER_ANSI}",
                candidates.iter().map(|(v, st)| format!("({}, {st})", Symbol::NT(*v).to_str(Some(&self.symbol_table)))).join(", "));
        }
        let token_error = self.num_t_full as TokenId;
        let token_eof = token_error - 1;
        let mut err_span = PosSpan::empty();
        while !candidates.is_empty() {
            for &(var, state) in &candidates {
                let action = &self.action[*stream_sym as usize + state as usize * self.num_t_full];
                if action.is_action() {
                    // we put back the goto_state on the stack because it will be required after the action
                    stack_state.push(goto_state);
                    if VERBOSE {
                        println!("{BEFORE_ANSI}- symbol {} is fine for state {state}{AFTER_ANSI}", self.t_to_string(*stream_sym));
                        println!("{BEFORE_ANSI}- states {stack_state:?}{AFTER_ANSI}");
                    }
                    if *call_wrapper {
                        *call_wrapper = wrapper.push_nt_recovery_value(var);
                        wrapper.push_span(err_span);
                    }
                    if VERBOSE && *call_wrapper { println!("{BEFORE_ANSI}{}{AFTER_ANSI}", wrapper.get_status().join("\n")); }
                    return Some(state)
                }
            }
            (*stream_sym, *stream_str) = loop {
                if let Some((t, s, span)) = stream.next() {
                    *stream_pos = Some(span.first_forced());
                    err_span += &span;
                    *stream_span = span;
                    // we can't say which interception to use, so we call both
                    let t1 = wrapper.intercept_token(t, &s, &stream_span);
                    let t2 = wrapper.hook(t1, s.as_str(), &stream_span);
                    break (t2, s)
                } else {
                    *stream_pos = None;
                    if let Some((_t, s, _span)) = stream.next() {
                        // an error code after the end means an unrecognized sequence: we may try to continue,
                        // but we don't count the max lexical errors here for the sake of simplicity
                        wrapper.report(Some(&stream_span), LogMsg::Error(format!("lexical error: {s}")));
                        continue;
                    } else {
                        // end of stream
                        break (token_eof, String::new())
                    }
                }
            };
            if *stream_sym == token_eof { break }
            if VERBOSE { println!("{BEFORE_ANSI}- no symbol was compatible, scanning new symbol {}{AFTER_ANSI}", self.t_to_string(*stream_sym)); }
        }
        if VERBOSE { println!("{BEFORE_ANSI}couldn't recover\n- states {stack_state:?}{AFTER_ANSI}"); }
        None
    }
}
