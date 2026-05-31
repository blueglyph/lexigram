use std::fmt::{Display, Formatter};
use crate::{AltId, CollectJoin, TokenId, VarId};
use crate::fixed_sym_table::{FixedSymTable, SymInfoTable};
use crate::lexer::{Pos, PosSpan};
use crate::log::LogMsg;
use crate::parser::{Call, ListenerWrapper, ParserError, ParserToken, Symbol, Terminate};

/// State index
pub type StateId = u16;

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub enum LRAction {
    #[default]
    Error,
    Shift(StateId),
    Reduce(AltId),
    Accept,
}

impl Display for LRAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LRAction::Error => write!(f, "-"),
            LRAction::Shift(s) => write!(f, "s{s}"),
            LRAction::Reduce(a) => write!(f, "r{a}"),
            LRAction::Accept => write!(f, "acc"),
        }
    }
}

/// Parser object. The [new(...)](LRParser::new) method creates a new instance.
pub struct LRParser {
    num_nt: usize,                      // doesn't include the goal NT
    num_t_full: usize,                  // includes the end symbol
    action: Vec<LRAction>,
    goto: Vec<StateId>,
    alt_nt_len: Vec<(VarId, u16, u16)>, // alt_id -> (nt, # symbols in alt, # terminals in alt)
    symbol_table: FixedSymTable,
}

impl LRParser {
    pub fn new(
        num_nt: usize,
        num_t_full: usize,
        action: Vec<LRAction>,
        goto: Vec<StateId>,
        alt_nt_len: Vec<(VarId, u16, u16)>,
        symbol_table: FixedSymTable
    ) -> Self {
        LRParser { num_nt, num_t_full, action, goto, alt_nt_len, symbol_table }
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
        let mut s: StateId = 0;
        let mut stack_state = vec![s];
        let mut stack_t = vec![];
        let mut advance_stream = true;
        let mut stream_pos = None;
        let mut stream_span = PosSpan::empty();
        let mut stream_sym = TokenId::default();
        let mut stream_str = String::default();
        loop {
            if advance_stream {
                (stream_sym, stream_str) = stream.next().map(|(t, s, span)| {
                    stream_pos = Some(span.first_forced());
                    stream_span = span;
                    (t, s)
                }).unwrap_or_else(|| {
                    // checks if there's an error code after the end
                    if let Some((_t, s, span)) = stream.next() {
                        stream_span = span;
                        error = Some(ParserError::Irrecoverable);
                        (token_error, s)
                    } else {
                        (token_eof, String::new())
                    }
                });
                advance_stream = false;
                if error.is_some() { break }
                if self.symbol_table.is_token_data(stream_sym) {
                    stack_t.push(stream_str.clone());
                }
            }
            match self.action[stream_sym as usize + s as usize * self.num_t_full] {
                LRAction::Shift(new_s) => {
                    stack_state.push(new_s);
                    s = new_s;
                    advance_stream = true;
                }
                LRAction::Reduce(alt) => {                                      // alt: s -> ω
                    let (nt, alt_len, nbr_t) = self.alt_nt_len[alt as usize];   // s
                    stack_state.drain(stack_state.len() - alt_len as usize..);  // pop |ω| states
                    let new_s = stack_state.pop().unwrap();
                    s = new_s;
                    stack_state.push(self.goto[nt as usize + new_s as usize * self.num_nt]);
                    let t_data = stack_t.drain(stack_t.len() - nbr_t as usize ..).to_vec();
                    wrapper.switch(Call::Exit, nt, alt, Some(t_data));
                }
                LRAction::Accept => {
                    wrapper.switch(Call::End(Terminate::None), 0, 0, None);
                    break
                }
                _ => {
                    error = Some(ParserError::SyntaxError);
                    break
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
                        break
                    }
                }
            }
        }
        if let Some(err) = error {
            let mut msg = if stream_sym == token_error {
                format!("lexical error: couldn't recognize {stream_str:?}")
            } else {
                let sym = if stream_sym == token_eof { Symbol::End } else { Symbol::T(stream_sym) };
                format!("syntax error: unexpected token '{}' on {stream_str:?}", sym.to_str(sym_table))
            };
            if let Some(Pos(line, col)) = stream_pos {
                msg.push_str(&format!(", line {line}, col {col}"));
            }
            wrapper.report(Some(&stream_span), LogMsg::Error(msg));
            wrapper.abort();
            Err(err)
        } else {
            Ok(())
        }
    }
}
