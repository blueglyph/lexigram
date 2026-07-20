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
        const VERBOSE: bool = false;
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
                            if VERBOSE { println!("  hook changed {} to {}", Symbol::T(t).to_str(Some(&self.symbol_table)), Symbol::T(new_t).to_str(Some(&self.symbol_table))) }
                            new_t
                        } else {
                            t
                        };
                        (new_t, s)
                    }
                } else {
                    if let Some((_t, s, span)) = stream.next() {
                        // an error code after the end means an unrecognized sequence: we may try to continue
                        wrapper.report(Some(&stream_span), LogMsg::Error(format!("lexical error: {s} at {span}")));
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
                    "states [{}] -> {state}, stack_t [{}], input: token {} = {stream_str:?}",
                    stack_state.iter().map(|s| s.to_string()).join(" "),
                    stack_t.iter().map(|s| format!("{s:?}")).join(", "),
                    Symbol::T(stream_sym).to_str(sym_table)
                );
            }
            let action = self.action[stream_sym as usize + state as usize * self.num_t_full];
            match action {
                LRAction::Shift(new_state) | LRAction::ShiftHook(new_state) => {
                    hook = action.is_hook();
                    if VERBOSE { println!("- shift({new_state})"); }
                    stack_state.push(new_state);
                    if self.symbol_table.is_token_data(stream_sym) {
                        stack_t.push(std::mem::take(&mut stream_str));
                    }
                    if nbr_parser_errors == 0 { wrapper.push_span(stream_span.take()); }
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
                    if VERBOSE { println!("- reduce({alt}) -> state {pop_state} -> goto {state}"); }
                    let t_data = stack_t.drain(stack_t.len() - nbr_t as usize..).to_vec();
                    if nbr_parser_errors == 0 { wrapper.switch(Call::Exit, nt, alt, Some(t_data)); }
                }
                LRAction::Accept => {
                    if VERBOSE { println!("- accept"); }
                    if nbr_parser_errors == 0 { wrapper.switch(Call::End(Terminate::None), 0, 0, None); }
                    stack_state.pop();
                    stack_state.pop();
                    break;
                }
                LRAction::Error => {
                    let expected = (0..self.num_t_full)
                        .filter(|t| self.action[*t + state as usize * self.num_t_full].is_action())
                        .map(|t| if t < self.num_t_full - 1 { Symbol::T(t as TokenId).to_str_quote(sym_table) } else { "<EOF>".to_string() })
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
                    // todo!("try and recover")
                    error = Some(ParserError::SyntaxError);
                    break;
                }
            }
            match wrapper.check_abort_request() {
                Terminate::None => {}
                terminate @ (Terminate::Abort | Terminate::Conclude) => {
                    if VERBOSE { println!("detected {terminate:?}"); }
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
        if let Some(err) = error {
            wrapper.abort();
            Err(err)
        } else {
            assert!(stack_t.is_empty(), "stack_t: {}", stack_t.join(", "));
            assert!(stack_state.is_empty(), "stack_state: {}", stack_state.iter().map(LRStateId::to_string).collect::<Vec<_>>().join(", "));
            assert!(wrapper.is_stack_empty(), "symbol stack isn't empty");
            assert!(wrapper.is_stack_t_empty(), "text stack isn't empty");
            assert!(wrapper.is_stack_span_empty(), "span stack isn't empty");
            Ok(())
        }
    }
}
