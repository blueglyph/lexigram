pub(crate) mod tests;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use crate::dfa::{StateId, Terminal, Token};
use crate::escape_char;
use crate::io::CharReader;
use crate::lexgen::{char_to_group, GroupId, LexGen};

// ---------------------------------------------------------------------------------------------
// Table-based lexer interpreter

#[derive(Clone, PartialEq, Debug)]
pub struct LexScanError {
    pub pos: u64,
    pub curr_char: Option<char>,
    pub group: Option<GroupId>,
    pub token: Option<Token>,
    pub state: StateId,
    pub is_eos: bool,
    pub msg: String
}

impl Display for LexScanError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "pos: {}{}{}{}, state {}: {}{}",
            self.pos,
            self.curr_char.map(|c| format!(" on '{}'", escape_char(c))).unwrap_or("".to_string()),
            self.group.map(|g| format!(", group {g}")).unwrap_or("".to_string()),
            self.token.as_ref().map(|t| format!(", token {}", t.0)).unwrap_or("".to_string()),
            self.state,
            self.msg,
            if self.is_eos { "<END OF STREAM>" } else { "" }
        )
    }
}

pub struct Scanner<R> {
    input: Option<CharReader<R>>,
    error: Option<LexScanError>,     // None = OK, Some(e) = last error
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: Box<HashMap<char, GroupId>>,
    pub nbr_groups: usize,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    pub state_table: Box<[StateId]>,
    pub token_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
}

impl<R: Read> Scanner<R> {
    pub fn new(lexgen: LexGen) -> Self {
        Scanner {
            input: None,
            error: None,
            ascii_to_group: lexgen.ascii_to_group,
            utf8_to_group: lexgen.utf8_to_group,
            nbr_groups: lexgen.nbr_groups,
            initial_state: lexgen.initial_state,
            first_end_state: lexgen.first_end_state,
            nbr_states: lexgen.nbr_states,
            state_table: lexgen.state_table,
            token_table: lexgen.terminal_table,
        }
    }

    pub fn attach_steam(&mut self, input: CharReader<R>) {
        self.input = Some(input);
    }

    pub fn detach_stream(&mut self) -> Option<CharReader<R>> {
        self.input.take()
    }

    pub fn stream(&self) -> Option<&CharReader<R>> {
        self.input.as_ref()
    }

    pub fn is_open(&self) -> bool {
        self.input.as_ref().map(|input| input.is_reading()).unwrap_or(false)
    }

    pub fn skip(&mut self) -> Option<char> {
        if let Some(input) = self.input.as_mut() {
            input.get_char()
        } else {
            None
        }
    }

    pub fn tokens(&mut self) -> LexInterpretIter<'_, R> {
        LexInterpretIter { interpreter: self }
    }

    pub fn get_token(&mut self) -> Result<Token, LexScanError> {
        self.error = None;
        if let Some(input) = self.input.as_mut() {
            const VERBOSE: bool = false;
            let mut state = self.initial_state;
            let mut chars = input.chars();
            let mut pos = 0_u64;
            loop {
                if VERBOSE { print!("- state = {state}"); }
                if let Some(c) = chars.next() {
                    let group = char_to_group(&self.ascii_to_group, &self.utf8_to_group, c.char);
                    if VERBOSE { print!(", char '{}' -> group {}", escape_char(c.char), group); }
                    // we can use the state_table even if group = error = nrb_group (but we must
                    // ignore new_state and detect that the group is illegal):
                    let new_state = self.state_table[self.nbr_groups * state + group];
                    if group >= self.nbr_groups || new_state >= self.nbr_states {
                        if VERBOSE { println!(" <invalid input>"); }
                        input.rewind(c.char).unwrap();
                        self.error = Some(LexScanError {
                            pos,
                            curr_char: Some(c.char),
                            group: Some(group),
                            token: if state >= self.first_end_state { Some(self.token_table[state - self.first_end_state].clone()) } else { None },
                            state,
                            is_eos: false,
                            msg: (if group >= self.nbr_groups { "unrecognized character" } else { "unexpected character" }).to_string(),
                        });
                        return Err(self.error.clone().unwrap());
                    }
                    if VERBOSE { println!(" -> state {new_state}"); }
                    state = new_state;
                } else {
                    if VERBOSE { println!(" <end of input>"); }
                    return if state >= self.first_end_state {
                        Ok(self.token_table[state - self.first_end_state].clone())
                    } else {
                        self.error = Some(LexScanError {
                            pos,
                            curr_char: None,
                            group: None,
                            token: None,
                            state,
                            is_eos: true,
                            msg: "unexpected end of stream".to_string(),
                        });
                        Err(self.error.clone().unwrap())
                    };
                }
                pos += 1;
            }
        } else {
            self.error = Some(LexScanError {
                pos: 0,
                curr_char: None,
                group: None,
                token: None,
                state: 0,
                is_eos: true,
                msg: "no stream attached".to_string(),
            });
            Err(self.error.clone().unwrap())
        }
    }

    pub fn get_error(&self) -> Option<&LexScanError> {
        self.error.as_ref()
    }
}

pub struct LexInterpretIter<'a, R> {
    interpreter: &'a mut Scanner<R>
}

impl<'a, R: Read> Iterator for LexInterpretIter<'a, R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.interpreter.get_token();
        match t {
            Ok(token) | Err(LexScanError { token: Some(token), .. }) => Some(token),
            _ => None
        }
    }
}