pub(crate) mod tests;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use crate::dfa::{ChannelId, StateId, Terminal, Token};
use crate::escape_char;
use crate::segments::SegMap;
use crate::io::{CharReader};
use crate::lexgen::{char_to_group, GroupId};

// ---------------------------------------------------------------------------------------------
// Table-based lexer interpreter

#[derive(Clone, PartialEq, Debug)]
pub struct LexerError {
    pub pos: u64,
    pub curr_char: Option<char>,
    pub group: Option<GroupId>,
    pub token_ch: Option<(Token, ChannelId)>,
    pub state: StateId,
    pub is_eos: bool,
    pub text: String,
    pub msg: String
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "pos: {}{}{}{}, state {}: {}{}",
               self.pos,
               if let Some(c) = self.curr_char { format!(" on '{}'", escape_char(c)) } else { "".to_string() },
               if let Some(g) = self.group { format!(", group {g}") } else { "".to_string() },
               if let Some(t) = self.token_ch.as_ref() { format!(", token {}, channel {}", t.0.0, t.1) } else { "".to_string() },
               self.state,
               self.msg,
               if self.is_eos { "<END OF STREAM>" } else { "" }
        )
    }
}

pub struct Lexer<R> {
    // operating variables
    input: Option<CharReader<R>>,
    error: Option<LexerError>,     // None = OK, Some(e) = last error
    pos: u64,
    state_stack: Vec<StateId>,
    start_state: StateId,
    // parameters
    pub nbr_groups: u32,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    // tables
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: HashMap<char, GroupId>,
    pub seg_to_group: SegMap<GroupId>,
    pub state_table: Box<[StateId]>,
    pub terminal_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
}

impl<R: Read> Lexer<R> {
    pub fn new(
        // parameters
        nbr_groups: u32,
        initial_state: StateId,
        first_end_state: StateId,   // accepting when state >= first_end_state
        nbr_states: StateId,        // error if state >= nbr_states
        // tables
        ascii_to_group: Box<[GroupId]>,
        utf8_to_group: HashMap<char, GroupId>,
        seg_to_group: SegMap<GroupId>,
        state_table: Box<[StateId]>,
        terminal_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
    ) -> Self {
        Lexer {
            input: None,
            error: None,
            pos: 0,
            state_stack: Vec::new(),
            start_state: 0,
            nbr_groups,
            initial_state,
            first_end_state,
            nbr_states,
            ascii_to_group,
            utf8_to_group,
            seg_to_group,
            state_table,
            terminal_table,
        }
    }

    pub fn attach_stream(&mut self, input: CharReader<R>) {
        self.input = Some(input);
        self.pos = 0;
        self.state_stack.clear();
        self.start_state = self.initial_state;
    }

    pub fn detach_stream(&mut self) -> Option<CharReader<R>> {
        // self.pos = None;
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
        LexInterpretIter { lexer: self }
    }

    // if next char returns EOF at the end, we can simplify:
    //
    //      if input.is_none
    //          return error
    //      state = start
    //      loop
    //          next char
    //          group       -> group == nbr_groups => unrecognized
    //          next_state  -> [normal] < first_end_state <= [accepting] <= nbr_states <= [invalid char]
    //          if next_state >= nbr_states || group >= nbr_groups
    //              if EOS
    //                  close
    //              else
    //                  rewind char
    //              if first_end_state <= state < nbr_states (accepting)
    //                  // process skip/push/pop:
    //                  curr_start = start
    //                  if pop
    //                      start = stack.pop()
    //                  if push(n)
    //                      stack.push(curr_start)
    //                      start = n
    //                      state = n
    //                  if !skip
    //                      return (token, channel)
    //                  [else continue]
    //              else
    //                  return error/EOS
    //          else
    //              state = next_state
    //              pos++
    //
    pub fn get_token(&mut self) -> Result<(Token, ChannelId, String), &LexerError> {
        const VERBOSE: bool = false;
        self.error = None;
        let mut text = String::new();
        if let Some(input) = self.input.as_mut() {
            let mut state = self.start_state;
            loop {
                if VERBOSE { print!("- state = {state}"); }
                // let offset = input.get_offset();
                let c_opt = input.get_char();
                let is_eos = c_opt.is_none();
                let group = c_opt.and_then(|c| char_to_group(&self.ascii_to_group, &self.utf8_to_group, &self.seg_to_group, c))
                    .unwrap_or(self.nbr_groups);
                if VERBOSE { print!(", char '{}' group {}", if let Some(c) = c_opt { escape_char(c) } else { "<EOF>".to_string() }, group); }
                // we can use the state_table even if group = error = nrb_group (but we must
                // ignore new_state and detect that the group is illegal):
                let new_state = self.state_table[self.nbr_groups as usize * state + group as usize];
                if new_state >= self.nbr_states || group >= self.nbr_groups { // we can't do anything with the current character
                    if let Some(c) = c_opt {
                        input.rewind(c).expect(&format!("Can't rewind character '{}'", escape_char(c)));
                    }
                    if self.first_end_state <= state && state < self.nbr_states { // accepting
                        let curr_start_state = self.start_state;
                        let terminal = &self.terminal_table[state - self.first_end_state];
                        if terminal.pop {
                            self.start_state = self.state_stack.pop().unwrap();
                            if VERBOSE { print!(", pop to {}", self.start_state); }
                        }
                        if let Some(goto_state) = terminal.push_state {
                            self.state_stack.push(curr_start_state);
                            self.start_state = goto_state;
                            if VERBOSE { print!(", push({})", goto_state); }
                        }
                        if let Some(token) = &terminal.token {
                            if VERBOSE { println!(" => OK: token {}", token.0); }
                            return Ok((token.clone(), terminal.channel, text));
                        }
                        if VERBOSE { println!(" => skip, state {}", self.start_state); }
                        state = self.start_state;
                        text.clear();
                        continue; // todo: maybe not necessary
                    } else { // EOF or invalid character
                        self.error = Some(LexerError {
                            pos: self.pos,
                            curr_char: c_opt,
                            group: Some(group),
                            token_ch: None,
                            state,
                            is_eos,
                            text,
                            msg: (if is_eos { "end of stream" } else { if group >= self.nbr_groups { "unrecognized character" } else { "invalid character" }}).to_string(),
                        });
                        if VERBOSE { println!(" => Err({})", self.error.as_ref().unwrap().msg); }
                        return Err(self.error.as_ref().unwrap());
                    }
                } else {
                    if let Some(c) = c_opt {
                        text.push(c);
                    }
                    if VERBOSE { println!(" => state {new_state}"); }
                    state = new_state;
                    self.pos += 1;
                }
            }
        }
        self.error = Some(LexerError {
            pos: self.pos,
            curr_char: None,
            group: None,
            token_ch: None,
            state: 0,
            is_eos: true,
            text,
            msg: "no stream attached".to_string(),
        });
        if VERBOSE { println!(" => Err({})", self.error.as_ref().unwrap().msg); }
        // self.pos = None;
        Err(self.error.as_ref().unwrap())
    }

    pub fn get_error(&self) -> Option<&LexerError> {
        self.error.as_ref()
    }
}

pub struct LexInterpretIter<'a, R> {
    lexer: &'a mut Lexer<R>
}

impl<'a, R: Read> Iterator for LexInterpretIter<'a, R> {
    type Item = (Token, ChannelId, String);

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.lexer.get_token();
        match t {
            Ok((token, channel, str)) => Some((token, channel, str)),
            Err(&LexerError { token_ch: Some((ref token, channel)), ref text, .. }) => Some((token.clone(), channel, text.clone())),
            _ => None
        }
    }
}
