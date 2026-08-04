// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fmt::{Display, Formatter};
use crate::fixed_sym_table::SymInfoTable;
use crate::{AltId, TokenId, VarId};
use crate::lexer::PosSpan;
use crate::log::{LogMsg, Logger};
pub(crate) mod tests;
pub mod lr;
pub mod ll1;

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

    pub fn is_t_or_nt(&self) -> bool {
        matches!(self, Symbol::T(_) | Symbol::NT(_))
    }

    pub fn to_str<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        symbol_table.map(|t| t.get_str(self)).unwrap_or_else(|| self.to_string())
    }

    /// Converts the symbol to string, using the symbol table if available, and
    /// surrounding it with quotes if it's a string literal.
    pub fn to_str_quote<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        symbol_table.map(|t| t.get_name_quote(self)).unwrap_or_else(|| self.to_string())
    }

    pub fn to_str_name<T: SymInfoTable>(&self, symbol_table: Option<&T>) -> String {
        symbol_table.map(|t| t.get_name(self)).unwrap_or_else(|| self.to_string())
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

    /// Converts the symbol to a string that's explicit for the user, using the symbol table
    /// if available to display the token name in case of a variable token.
    pub fn to_str_type<T: SymInfoTable>(&self, symbol_table: Option<&T>, ext: &String) -> String {
        match self {
            Symbol::T(t) => format!("input {}", terminal_to_str_type(*t, symbol_table, ext)),
            Symbol::NT(_) => format!("nonterminal {}", self.to_str(symbol_table)),
            Symbol::Empty => "empty symbol".to_string(),
            Symbol::End => "end of stream".to_string(),
        }
    }

    /// Converts to symbols used in `sym!` and other related macros of the `lexigram` crate.
    pub fn to_macro_item(&self) -> String {
        match self {
            Symbol::Empty => "e".to_string(),
            Symbol::T(x) => format!("t {x}"),
            Symbol::NT(x) => format!("nt {x}"),
            Symbol::End => "end".to_string(),
        }
    }
}

/// Converts the terminal to string, using the symbol table if available to display the token name
/// in case of variable token.
pub fn terminal_to_str_type<T: SymInfoTable>(token: TokenId, symbol_table: Option<&T>, ext: &String) -> String {
    let mut result = format!("{ext:?}");
    if let Some(t) = symbol_table {
        if t.is_token_data(token) {
            result.push_str(&format!(" ({})", t.get_t_name(token)));
        }
    }
    result
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
                OpCode::T(v) => tbl.get_t_str(*v),
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

/// Codes returned by the [check_abort_request(...)](ListenerWrapper::check_abort_request) method of
/// the listener (via the wrapper pass-through).
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Terminate {
    /// Normal behaviour: continues parsing the text
    None,
    /// Irrecoverable error: stops parsing, calls the listener abort method, and returns an error
    Abort,
    /// Stops parsing, calls the listener exit method, and returns an Ok
    Conclude,
}

/// Action calls to the wrapper with the method [ListenerWrapper::switch]. The wrapper translates the
/// action accordingly to the current nonterminal and alternative; for example, by calling the
/// appropriate listener callback.
#[derive(PartialEq, Debug)]
pub enum Call {
    /// Enters a new nonterminal rule. The alternative is already known, but the values of the symbols
    /// in that alternative haven't been scanned yet.
    ///
    /// This can be used to initialize the listener's variables when a particular rule is about to be
    /// parsed (the listener methods associated with this action are normally optional since no
    /// information is returned to the wrapper).
    ///
    /// The wrapper also uses this call to initialize stack items like accumulators used in rule loops
    /// like `a -> b*`.
    Enter,
    /// Re-enters a loop nonterminal. This is currently not used in the wrapper.
    Loop,
    /// Exits an alternative, once all the symbols in it have been parsed: nonterminals and terminals.
    ///
    /// This is typically used to call an exit method of the listener and evaluate its value when it
    /// has one.
    Exit,
    /// This action is used in two situations:
    /// * when the parsing of the top rule has completed normally. In that case, the wrapper
    ///   calls the [exit(...)] method of the listener (done in the generated code).
    /// * when the parsing is [aborted](Terminate::Abort) or [concluded](Terminate::Conclude) in
    ///   reaction to an [check_abort_request(...)](ListenerWrapper::check_abort_request) call. In
    ///   that case, the wrapper calls the [abort(...)] method of the listener (done in the generated
    ///   code).
    ///
    /// The [Terminate] value it contains tells the wrapper which of those eventualities has
    /// occurred.
    End(Terminate)
}

pub trait ListenerWrapper {
    /// Calls the listener to execute Enter, Loop, Exit, and End actions.
    #[allow(unused_variables)]
    fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {}

    /// Checks if the listener requests an abort (wrapper pass-through). This method is called at the end of
    /// each parser iteration. If an error is too difficult to recover from, the listener can set a flag that
    /// tells to return a [Terminate::Abort] on the next call, and implement this method to return
    /// the appropriate status.
    ///
    /// In that case, the parser
    /// * calls [abort(...)](ListenerWrapper::abort)
    /// * calls [switch([Call::End]([Terminate::Abort]))](ListenerWrapper::switch) (if there was no syntax error)
    /// * returns [ParserError::AbortRequest].
    fn check_abort_request(&self) -> Terminate { Terminate::None }

    /// Aborts the parsing.
    fn abort(&mut self) {}

    /// Gets access to the listener's log to report possible errors and information about the parsing.
    fn get_log_mut(&mut self) -> &mut impl Logger;

    /// Reports a message (note, info, warning, or error). The default behaviour adds the message to the log.
    #[allow(unused_variables)]
    fn report(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg) {
        self.get_log_mut().add(msg);
    }

    /// Pushes a location span onto the (optional) span stack
    #[allow(unused_variables)]
    fn push_span(&mut self, span: PosSpan) {}

    /// Checks that the stack is empty (the parser only checks that the stack is empty after successfully parsing a text)
    fn is_stack_empty(&self) -> bool { true }

    /// Checks that the stack_t is empty (the parser only checks that the stack is empty after successfully parsing a text)
    fn is_stack_t_empty(&self) -> bool { true }

    /// Checks that the stack_span is empty (the parser only checks that the stack is empty after successfully parsing a text)
    fn is_stack_span_empty(&self) -> bool { true }

    /// Allows to dynamically translate a token in the listener (wrapper pass-through)
    #[allow(unused_variables)]
    fn hook(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        token
    }

    /// Allows to intercept any token in the listener (wrapper pass-through)
    #[allow(unused_variables)]
    fn intercept_token(&mut self, token: TokenId, text: &str, span: &PosSpan) -> TokenId {
        token
    }

    fn get_status(&self) -> Vec<String> { Vec::new() }

    /// Requests the wrapper to push a dummy value to resynchronize its stack in case of error recovery.
    ///
    /// Returns `true` if the stack could be resynchronized. If it couldn't, the parser will continue to parse the text
    /// to detect other parsing errors, but it won't call the wrapper any more, except to intercept tokens.
    #[allow(unused_variables)]
    fn push_nt_recovery_value(&mut self, nt: VarId) -> bool { false }

    /// Returns the symbol on the left of the dot and whether it has a value
    #[allow(unused_variables)]
    fn get_state_symbol_and_value(&mut self, state: crate::parser::lr::LRStateId) -> (Symbol, bool) { (Symbol::Empty, false) }

    fn pop_syn_value(&mut self) {}
}

// ---------------------------------------------------------------------------------------------

pub type ParserToken = (TokenId, String, PosSpan);

/// Code of the error that occurred during the parsing, returned by the
/// [parse_stream(...)](LLParser::parse_stream) method of the parser.
#[derive(PartialEq, Debug)]
pub enum ParserError {
    /// A syntax error was met. Either
    /// * The next terminal of the parsed text doesn't match the expected one in the current rule
    ///   alternative; for example, a rule `assign -> "let" Id "=" expr ";";` has just successfully
    ///   scanned the terminal `"let"`, but the next one isn't `Id`.
    /// * The next symbol doesn't correspond to any correct option for the next nonterminal (
    ///   in other words, there is no entry in the parsing table for that combination). For example,
    ///   in the same rule as above, the terminal `"="` has just been scanned successfully, but `expr`
    ///   doesn't begin with the next one.
    ///
    /// This error is returned only when the parser doesn't try to recover from syntax errors; this
    /// option is set with the [set_try_recover(...)](LLParser::set_try_recover) method and is
    /// enabled by default.
    ///
    /// See also [ParserError::TooManyErrors].
    SyntaxError,
    /// Too many syntax errors were met, either
    /// * during the parsing. The limit is set by the constant [LLParser::MAX_NBR_RECOVERS].
    /// * by the lexer. The limit is set by the constant [LLParser::MAX_NBR_LEXER_ERRORS].
    ///
    /// This error is returned only when the parser tries to recover from syntactic or lexical errors;
    /// this option is set with the [set_try_recover(...)](LLParser::set_try_recover) method and is
    /// enabled by default.
    ///
    /// See also [ParserError::SyntaxError].
    TooManyErrors,
    /// The parser has reached an irrecoverable error, after trying to recover from a syntax error and
    /// encountering the end of the text.
    Irrecoverable,
    /// The parser has reached the end of the top rule, but there are still terminals coming from
    /// the lexer.
    ///
    /// Note that if the text is expected to contain something else after the part that must be parsed,
    /// it is possible to tell the parser to conclude the parsing without looking any further. This
    /// can be done in the listener with the [check_abort_request(...)] performed regularly by the
    /// parser. See the [examples/terminate] parser to see how it can be used.
    ExtraSymbol,
    /// The parser has encountered the end of the text, but the top rule hasn't been fully parsed.
    UnexpectedEOS,
    /// This is an internal error that isn't supposed to happen.
    UnexpectedError,
    /// The text has been fully parsed, but syntax errors were encountered by the parser (and could
    /// be recovered from).
    ///
    /// See also [ParserError::SyntaxError].
    EncounteredErrors,
    /// An [Abort](Terminate::Abort) was returned by the [check_abort_request(...)] method of the
    /// listener.
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
            ParserError::EncounteredErrors => "parsing failed due to previously encountered error(s)",
            ParserError::AbortRequest => "abort request",
        })
    }
}

