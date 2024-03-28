pub(crate) mod tests;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use crate::dfa::{ChannelId, StateId, Terminal, Token};
use crate::escape_char;
use crate::io::{CharReader};
use crate::lexgen::{char_to_group, GroupId, LexGen};

// ---------------------------------------------------------------------------------------------
// Table-based lexer interpreter

#[derive(Clone, PartialEq, Debug)]
pub struct LexScanError {
    pub pos: u64,
    pub curr_char: Option<char>,
    pub group: Option<GroupId>,
    pub token_ch: Option<(Token, ChannelId)>,
    pub state: StateId,
    pub is_eos: bool,
    pub msg: String
}

impl Display for LexScanError {
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

pub struct Scanner<R> {
    input: Option<CharReader<R>>,
    error: Option<LexScanError>,     // None = OK, Some(e) = last error
    pos: u64,
    state_stack: Vec<StateId>,
    start_state: StateId,
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: Box<HashMap<char, GroupId>>,
    pub nbr_groups: usize,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    pub state_table: Box<[StateId]>,
    pub terminal_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
}

impl<R: Read> Scanner<R> {
    pub fn new(lexgen: LexGen) -> Self {
        Scanner {
            input: None,
            error: None,
            pos: 0,
            state_stack: Vec::new(),
            start_state: 0,
            ascii_to_group: lexgen.ascii_to_group,
            utf8_to_group: lexgen.utf8_to_group,
            nbr_groups: lexgen.nbr_groups,
            initial_state: lexgen.initial_state,
            first_end_state: lexgen.first_end_state,
            nbr_states: lexgen.nbr_states,
            state_table: lexgen.state_table,
            terminal_table: lexgen.terminal_table,
        }
    }

    pub fn attach_steam(&mut self, input: CharReader<R>) {
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
        LexInterpretIter { scanner: self }
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
    pub fn get_token(&mut self) -> Result<(Token, ChannelId), &LexScanError> {
        const VERBOSE: bool = false;
        const EOF: char = '\x1a';
        self.error = None;
        if let Some(input) = self.input.as_mut() {
            let mut state = self.start_state;
            loop {
                if VERBOSE { print!("- state = {state}"); }
                // let offset = input.get_offset();
                let c = input.get_char().unwrap_or(EOF);
                let group = char_to_group(&self.ascii_to_group, &self.utf8_to_group, c);
                if VERBOSE { print!(", char '{}' group {}", if c == EOF { "<EOF>".to_string() } else { escape_char(c) }, group); }
                // we can use the state_table even if group = error = nrb_group (but we must
                // ignore new_state and detect that the group is illegal):
                let new_state = self.state_table[self.nbr_groups * state + group];
                if new_state >= self.nbr_states || group >= self.nbr_groups { // we can't do anything with the current character
                    if c != EOF {
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
                            return Ok((token.clone(), terminal.channel));
                        }
                        if VERBOSE { println!(" => skip"); }
                        continue; // todo: maybe not necessary
                    } else { // EOF or invalid character
                        self.error = Some(LexScanError {
                            pos: self.pos,
                            curr_char: Some(c),
                            group: Some(group),
                            token_ch: None,
                            state,
                            is_eos: c == EOF,
                            msg: (if c == EOF { "end of stream" } else { if group >= self.nbr_groups { "unrecognized character" } else { "invalid character" }}).to_string(),
                        });
                        if VERBOSE { println!(" => Err({})", self.error.as_ref().unwrap().msg); }
                        return Err(self.error.as_ref().unwrap());
                    }
                } else {
                    if VERBOSE { println!(" => state {new_state}"); }
                    state = new_state;
                    self.pos += 1;
                }
            }
        }
        self.error = Some(LexScanError {
            pos: self.pos,
            curr_char: None,
            group: None,
            token_ch: None,
            state: 0,
            is_eos: true,
            msg: "no stream attached".to_string(),
        });
        if VERBOSE { println!(" => Err({})", self.error.as_ref().unwrap().msg); }
        // self.pos = None;
        Err(self.error.as_ref().unwrap())
    }

    pub fn get_error(&self) -> Option<&LexScanError> {
        self.error.as_ref()
    }
}

pub struct LexInterpretIter<'a, R> {
    scanner: &'a mut Scanner<R>
}

impl<'a, R: Read> Iterator for LexInterpretIter<'a, R> {
    type Item = (Token, ChannelId);

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.scanner.get_token();
        match t {
            Ok((token, channel)) => Some((token, channel)),
            Err(&LexScanError { token_ch: Some((ref token, channel)), .. }) => Some((token.clone(), channel)),
            _ => None
        }
    }
}