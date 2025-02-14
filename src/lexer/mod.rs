pub(crate) mod tests;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use crate::dfa::{ChannelId, StateId, Terminal, TokenId};
use crate::escape_char;
use crate::segments::SegMap;
use crate::io::{CharReader};
use crate::lexergen::{char_to_group, GroupId};

// ---------------------------------------------------------------------------------------------
// Table-based lexer interpreter

// #[derive(Clone, PartialEq, Debug)]
// pub struct LexerError {
//     pub pos: u64,
//     pub curr_char: Option<char>,
//     pub group: Option<GroupId>,
//     pub token_ch: Option<(TokenId, ChannelId)>,
//     pub state: StateId,
//     pub is_eos: bool,
//     pub text: String,
//     pub msg: String
// }

#[derive(Clone, PartialEq, Debug)]
pub struct LexerErrorInfo {
    pub pos: u64,
    pub curr_char: Option<char>,
    pub group: GroupId,
    pub state: StateId,
    pub text: String,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LexerError {
    None,
    NoStreamAttached,
    EndOfStream { info: LexerErrorInfo },
    InvalidChar { info: LexerErrorInfo },
    UnrecognizedChar { info: LexerErrorInfo },
    InfiniteLoop { pos: u64 },
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::None => write!(f, "no error"),
            LexerError::NoStreamAttached => write!(f, "no stream attached"),
            LexerError::EndOfStream { info: LexerErrorInfo { pos, ..} } =>
                write!(f, "end of stream, pos = {pos}"),
            LexerError::InvalidChar { info: LexerErrorInfo { pos, curr_char, .. } } =>
                write!(f, "invalid character, pos = {pos}, chr = '{}'", curr_char.unwrap()),
            LexerError::UnrecognizedChar  { info: LexerErrorInfo { pos, curr_char, .. } } =>
                write!(f, "unrecognized character, pos = {pos}, chr = '{}'", curr_char.unwrap()),
            LexerError::InfiniteLoop { pos } =>
                write!(f, "infinite loop, pos = {pos}"),
        }
    }
    // fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    //     write!(f, "pos: {}{}{}{}, state {}: {}{}",
    //            self.pos,
    //            if let Some(c) = self.curr_char { format!(" on '{}'", escape_char(c)) } else { "".to_string() },
    //            if let Some(g) = self.group { format!(", group {g}") } else { "".to_string() },
    //            if let Some(t) = self.token_ch.as_ref() { format!(", token {}, channel {}", t.0, t.1) } else { "".to_string() },
    //            self.state,
    //            self.msg,
    //            if self.is_eos { "<END OF STREAM>" } else { "" }
    //     )
    // }
}

pub type CaretCol = u64;
pub type CaretLine = u64;

pub type LexerToken = (TokenId, ChannelId, String, CaretLine, CaretCol);

pub struct Lexer<R> {
    // operating variables
    input: Option<CharReader<R>>,
    error: LexerError,
    pos: u64,
    line: CaretLine,
    col: CaretCol,
    tab_width: u8,
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
            error: LexerError::None,
            pos: 0,
            line: 1,
            col: 1,
            tab_width: 8,
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
        self.line = 1;
        self.col = 1;
        self.state_stack.clear();
        self.start_state = self.initial_state;
    }

    pub fn detach_stream(&mut self) -> Option<CharReader<R>> {
        // self.pos = None;
        self.input.take()
    }

    pub fn set_tab_width(&mut self, width: u8) {
        self.tab_width = width;
    }

    pub fn get_tab_width(&self) -> u8 {
        self.tab_width
    }

    pub fn stream(&self) -> Option<&CharReader<R>> {
        self.input.as_ref()
    }

    pub fn is_open(&self) -> bool {
        self.input.as_ref().map(|input| input.is_reading()).unwrap_or(false)
    }

    // pub fn skip(&mut self) -> Option<char> {
    //     if let Some(input) = self.input.as_mut() {
    //         input.get_char()
    //     } else {
    //         None
    //     }
    // }

    pub fn tokens(&mut self) -> LexInterpretIter<'_, R> {
        LexInterpretIter { lexer: self }
    }

    // get_token flow:
    //
    //      if input.is_none
    //          return error
    //      state = start
    //      loop
    //          next char
    //          group       -> group == nbr_groups => unrecognized
    //          next_state  -> [normal] < first_end_state <= [accepting] <= nbr_states <= [invalid char]
    //          line, col = self.(line, col)
    //          if next_state >= nbr_states || group >= nbr_groups (invalid char)
    //              if !EOS
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
    //                      return (token, channel, line, col)
    //                  line, col = self.(line, col)
    //                  if !EOS
    //                      state = start
    //                      continue // skip
    //              return error/EOS
    //          else
    //              update self.(line, col)
    //              state = next_state
    //              pos++
    //
    pub fn get_token(&mut self) -> Result<LexerToken, LexerError> {
        const VERBOSE: bool = false;
        self.error = LexerError::None;
        let mut text = String::new();
        if let Some(input) = self.input.as_mut() {
            let mut state = self.start_state;
            let (mut line, mut col) = (self.line, self.col);
            #[cfg(debug_assertions)] let mut last_state: Option<StateId> = None;
            #[cfg(debug_assertions)] let mut last_offset: Option<u64> = None;
            #[cfg(debug_assertions)] let mut infinite_loop_cnt = 0_u32;
            loop {
                if VERBOSE { print!("- state = {state}"); }
                #[cfg(debug_assertions)] {
                    if last_state.map(|st| st == state).unwrap_or(false) && last_offset.map(|offset| offset == input.get_offset()).unwrap_or(false) {
                        if infinite_loop_cnt > 3 {
                            self.error = LexerError::InfiniteLoop { pos: self.pos };
                            // self.error = Some(LexerError {
                            //     pos: self.pos,
                            //     curr_char: None,
                            //     group: None,
                            //     token_ch: None,
                            //     state: 0,
                            //     is_eos: true,
                            //     text: "".to_string(),
                            //     msg: "infinite loop".to_string(),
                            // });
                            if VERBOSE { println!(" => Err({})", self.error); }
                            return Err(self.error.clone());
                        }
                        infinite_loop_cnt += 1;
                    } else {
                        infinite_loop_cnt = 0;
                    }
                    last_state = Some(state);
                    last_offset = Some(input.get_offset());
                }
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
                        if let Some(token) = terminal.get_token() {
                            if VERBOSE { println!(" => OK: token {}", token); }
                            return Ok((token.clone(), terminal.channel, text, line, col));
                        }
                        (line, col) = (self.line, self.col);
                        if !is_eos { // we can't skip if <EOF> or we'll loop indefinitely
                            if VERBOSE { println!(" => skip, state {}", self.start_state); }
                            state = self.start_state;
                            text.clear();
                            continue;
                        }
                    }
                    // EOF or invalid character
                    let info = LexerErrorInfo {
                        pos: self.pos,
                        curr_char: c_opt,
                        group,
                        state,
                        text,
                    };
                    self.error = if is_eos {
                        LexerError::EndOfStream { info }
                    } else if group >= self.nbr_groups {
                        LexerError::UnrecognizedChar { info }
                    } else {
                        LexerError::InvalidChar { info }
                    };
                    // self.error = Some(LexerError {
                    //     pos: self.pos,
                    //     curr_char: c_opt,
                    //     group: Some(group),
                    //     token_ch: None,
                    //     state,
                    //     is_eos,
                    //     text,
                    //     msg: (if is_eos { "end of stream" } else { if group >= self.nbr_groups { "unrecognized character" } else { "invalid character" }}).to_string(),
                    // });
                    if VERBOSE { println!(" => Err({})", self.error); }
                    return Err(self.error.clone());
                } else {
                    if let Some(c) = c_opt {
                        text.push(c);
                        match c {
                            '\t' => {
                                //            ↓       ↓    (if self.tab_width = 8)
                                //    1234567890123456789
                                // 1) ..↑                  col = 3
                                //    ..→→→→→→↑            col = 3 - 2%8 + 8 = 3 - 2 + 8 = 9
                                // 2) .............↑       col = 14
                                //    .............→→→↑    col = 14 - 13%8 + 8 = 14 - 5 + 8 = 17
                                self.col = self.col - (self.col - 1) % self.tab_width as CaretCol + self.tab_width as CaretCol;
                            }
                            '\n' => {
                                self.line += 1;
                                self.col = 1;
                            }
                            '\r' => {}
                            _ => self.col += 1,
                        }
                    }
                    if VERBOSE { println!(" => state {new_state}"); }
                    state = new_state;
                    self.pos += 1;
                }
            }
        }
        self.error = LexerError::NoStreamAttached;
        // self.error = Some(LexerError {
        //     pos: self.pos,
        //     curr_char: None,
        //     group: None,
        //     token_ch: None,
        //     state: 0,
        //     is_eos: true,
        //     text,
        //     msg: "no stream attached".to_string(),
        // });
        if VERBOSE { println!(" => Err({})", self.error); }
        // self.pos = None;
        Err(self.error.clone())
    }

    pub fn get_error(&self) -> &LexerError {
        &self.error
    }

    pub fn has_error(&self) -> bool {
        self.error != LexerError::None
    }

    pub fn is_eos(&self) -> bool {
        matches!(self.error, LexerError::EndOfStream { .. })
    }
}

pub struct LexInterpretIter<'a, R> {
    lexer: &'a mut Lexer<R>
}

impl<'a, R: Read> Iterator for LexInterpretIter<'a, R> {
    type Item = (TokenId, ChannelId, String, CaretLine, CaretCol);

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.lexer.get_token();
        match t {
            Ok((token, channel, str, line, col)) => Some((token, channel, str, line, col)),
            // Err(&LexerError { token_ch: Some((ref token, channel)), ref text, .. }) => Some((*token, channel, text.clone())),
            _ => None
        }
    }
}

// ---------------------------------------------------------------------------------------------

pub struct TokenSplit<I, F> {
    iter: I,
    ch: ChannelId,
    f: F
}

pub trait TokenSpliterator: Iterator<Item=(TokenId, ChannelId, String, CaretLine, CaretCol)> {
    /// Splits the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, CaretLine, CaretCol)`) based on the channel ID:
    /// * the default channel 0 is output as another iterator on `(token, string, line, column)`, suitable for the parser
    /// * other channels are consummed by the closure `f`, which takes the parameters `(token, channel, string, line, column)`
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().split_channel0(|(tok, ch, text, line, col)|
    ///     println!("TOKEN: channel {ch}, discarded, line {line} col {col}, Id {tok:?}, \"{text}\"")
    /// );
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn split_channel0<F>(self, f: F) -> TokenSplit<Self, F>
    where Self: Sized,
          F: FnMut((TokenId, ChannelId, String, CaretLine, CaretCol))
    {
        TokenSplit { iter: self, ch: 0, f }
    }

    /// Splits the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, CaretLine, CaretCol)`) based on the channel ID:
    /// * the channel `channel` is output as another iterator on `(token, string, line, column)`, suitable for the parser
    /// * other channels are consummed by the closure `f`, which takes the parameters `(token, channel, string, line, column)`
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().split_channels(2, |(tok, ch, text, line, col)|
    ///     println!("TOKEN: channel {ch}, discarded, line {line} col {col}, Id {tok:?}, \"{text}\"")
    /// );
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn split_channels<F>(self, channel: ChannelId, f: F) -> TokenSplit<Self, F>
    where Self: Sized,
          F: FnMut((TokenId, ChannelId, String, CaretLine, CaretCol))
    {
        TokenSplit { iter: self, ch: channel, f }
    }

    /// Filters the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, CaretLine, CaretCol)`) based on the channel ID:
    /// * the default channel 0 is output as another iterator on `(token, string, line, column)`, suitable for the parser
    /// * other channels are discarded.
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().keep_channel0();
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn keep_channel0(self) -> impl Iterator<Item=(TokenId, String, CaretLine, CaretCol)>
    where Self: Sized
    {
        self.filter_map(|(token, ch, str, line, col)| {
            if ch == 0 {
                Some((token, str, line, col))
            } else {
                None
            }
        })
    }

    /// Filters the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, CaretLine, CaretCol)`) based on the channel ID:
    /// * channel `channel` is output as another iterator on `(token, string, line, column)`, suitable for the parser
    /// * other channels are discarded.
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().keep_channel(2);
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn keep_channel(self, channel: ChannelId) -> TokenSplit<Self, fn((TokenId, ChannelId, String, CaretLine, CaretCol))>
    where Self: Sized
    {
        TokenSplit { iter: self, ch: channel, f: |_| {} }
    }

    // or:
    //
    // fn keep_channel(self, channel: ChannelId) -> impl Iterator<Item=(TokenId, String, CaretLine, CaretCol)>
    // where Self: Sized
    // {
    //     self.filter_map(move |(token, ch, str, line, col)| {
    //         if ch == channel {
    //             Some((token, str, line, col))
    //         } else {
    //             None
    //         }
    //     })
    // }
}

impl<I, F> Iterator for TokenSplit<I, F>
    where I: Iterator<Item=(TokenId, ChannelId, String, CaretLine, CaretCol)>,
          F: FnMut((TokenId, ChannelId, String, CaretLine, CaretCol))
{
    type Item = (TokenId, String, CaretLine, CaretCol);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((token, ch, str, line, col)) = self.iter.next() {
            if ch == self.ch {
                Some((token, str, line, col))
            } else {
                (self.f)((token, ch, str, line, col));
                None
            }
        } else {
            None
        }
    }
}

impl<I: Iterator<Item=(TokenId, ChannelId, String, CaretLine, CaretCol)>> TokenSpliterator for I {}
