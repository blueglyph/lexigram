// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(crate) mod tests;

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::io::Read;
use crate::dfa::{ChannelId, StateId, Terminal, TokenId};
use crate::escape_char;
use crate::segments::{SegMap, Segments};
use crate::io::{CharReader, UTF8_HIGH_MIN, UTF8_LOW_MAX, UTF8_MAX};
use crate::lexergen::{char_to_group, GroupId};

// ---------------------------------------------------------------------------------------------
// Table-based lexer interpreter

#[derive(Clone, PartialEq, Debug)]
pub struct LexerErrorInfo {
    pub pos: u64,
    pub line: CaretLine,
    pub col: CaretCol,
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
    EmptyStateStack { info: LexerErrorInfo }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::None => write!(f, "no error"),
            LexerError::NoStreamAttached => write!(f, "no stream attached"),
            LexerError::EndOfStream { info: LexerErrorInfo { pos, line, col, ..} } =>
                write!(f, "end of stream, line {line}, col {col} (stream pos = {pos})"),
            LexerError::InvalidChar { info: LexerErrorInfo { pos, line, col, curr_char, .. } } =>
                write!(f, "invalid character '{}', line {line}, col {col} (stream pos = {pos})", curr_char.unwrap()),
            LexerError::UnrecognizedChar  { info: LexerErrorInfo { pos, line, col, curr_char, .. } } =>
                write!(f, "unrecognized character '{}', line {line}, col {col} (stream pos = {pos})", curr_char.unwrap()),
            LexerError::InfiniteLoop { pos } =>
                write!(f, "infinite loop (stream pos = {pos})"),
            LexerError::EmptyStateStack { info: LexerErrorInfo { pos, line, col, curr_char, .. } } =>
                write!(f, "pop from empty stack, line {line}, col {col}{} (stream pos = {pos})",
                       if let Some(c) = curr_char { format!(", chr = '{c}'") } else { String::new() })
        }
    }
}

impl LexerError {
    pub fn get_pos(&self) -> Option<u64> {
        match &self {
            LexerError::EndOfStream { info: LexerErrorInfo { pos, .. } }
            | LexerError::InvalidChar { info: LexerErrorInfo { pos, .. } }
            | LexerError::UnrecognizedChar { info: LexerErrorInfo { pos, .. } }
            | LexerError::InfiniteLoop { pos }
            | LexerError::EmptyStateStack { info: LexerErrorInfo { pos, .. } } => Some(*pos),
            _ => None
        }
    }

    pub fn get_line_col(&self) -> Option<(CaretLine, CaretCol)> {
        match &self {
            LexerError::EndOfStream { info: LexerErrorInfo { line, col, .. } }
            | LexerError::InvalidChar { info: LexerErrorInfo { line, col, .. } }
            | LexerError::UnrecognizedChar { info: LexerErrorInfo { line, col, .. } }
            | LexerError::EmptyStateStack { info: LexerErrorInfo { line, col, .. } } => Some((*line, *col)),
            _ => None
        }
    }
}

pub struct LexerTables {
    // parameters
    nbr_groups: u32,
    initial_state: StateId,
    first_end_state: StateId,   // accepting when state >= first_end_state
    nbr_states: StateId,        // error if state >= nbr_states
    // tables
    ascii_to_group: Vec<GroupId>,
    utf8_to_group: HashMap<char, GroupId>,
    seg_to_group: SegMap<GroupId>,
    state_table: Vec<StateId>,
    terminal_table: Vec<Terminal>,  // token(state) = token_table[state - first_end_state]
}

impl LexerTables {
    pub fn new(
        // parameters
        nbr_groups: u32,
        initial_state: StateId,
        first_end_state: StateId,   // accepting when state >= first_end_state
        nbr_states: StateId,        // error if state >= nbr_states
        // tables
        ascii_to_group: Vec<GroupId>,
        utf8_to_group: HashMap<char, GroupId>,
        seg_to_group: SegMap<GroupId>,
        state_table: Vec<StateId>,
        terminal_table: Vec<Terminal>,  // token(state) = token_table[state - first_end_state]
    ) -> Self {
        LexerTables {
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

    pub fn to_lexer<R: Read>(self) -> Lexer<R> {
        Lexer::from_tables(self)
    }
}

pub type CaretCol = u64;
pub type CaretLine = u64;

pub type LexerToken = (TokenId, ChannelId, String, CaretLine, CaretCol);

pub struct Lexer<R> {
    // operating variables
    input: Option<CharReader<R>>,
    error: LexerError,
    is_eos: bool,
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
    pub ascii_to_group: Vec<GroupId>,
    pub utf8_to_group: HashMap<char, GroupId>,
    pub seg_to_group: SegMap<GroupId>,
    pub state_table: Vec<StateId>,
    pub terminal_table: Vec<Terminal>,  // token(state) = token_table[state - first_end_state]
}

impl<R: Read> Lexer<R> {
    pub fn new(
        // parameters
        nbr_groups: u32,
        initial_state: StateId,
        first_end_state: StateId,   // accepting when state >= first_end_state
        nbr_states: StateId,        // error if state >= nbr_states
        // tables
        ascii_to_group: Vec<GroupId>,
        utf8_to_group: HashMap<char, GroupId>,
        seg_to_group: SegMap<GroupId>,
        state_table: Vec<StateId>,
        terminal_table: Vec<Terminal>,  // token(state) = token_table[state - first_end_state>]
    ) -> Self {
        Lexer {
            input: None,
            error: LexerError::None,
            is_eos: false,
            pos: 0,
            line: 1,
            col: 1,
            tab_width: 4,
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

    pub fn from_tables(tables: LexerTables) -> Self {
        Lexer {
            input: None,
            error: LexerError::None,
            is_eos: false,
            pos: 0,
            line: 1,
            col: 1,
            tab_width: 4,
            state_stack: Vec::new(),
            start_state: 0,
            nbr_groups: tables.nbr_groups,
            initial_state: tables.initial_state,
            first_end_state: tables.first_end_state,
            nbr_states: tables.nbr_states,
            ascii_to_group: tables.ascii_to_group,
            utf8_to_group: tables.utf8_to_group,
            seg_to_group: tables.seg_to_group,
            state_table: tables.state_table,
            terminal_table: tables.terminal_table,
        }
    }

    pub fn attach_stream(&mut self, input: CharReader<R>) {
        self.input = Some(input);
        self.is_eos = false;
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

    pub fn tokens(&mut self) -> LexInterpretIter<'_, R> {
        LexInterpretIter { lexer: self, error_info: None, mode: LexInterpretIterMode::Normal }
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
    pub fn get_token(&mut self) -> Result<Option<LexerToken>, LexerError> {
        const VERBOSE: bool = false;
        if VERBOSE { println!("lexer state_table: {}, last: {}", self.state_table.len(), self.state_table.iter().last().unwrap()); }
        self.error = LexerError::None;
        let mut text = String::new();
        let mut more_text = String::new();  // keeps previously scanned text if `more` action
        // if let Some(input) = self.input.as_mut() {
        if self.input.is_some() {
            let mut state = self.start_state;
            let (mut line, mut col) = (self.line, self.col);
            #[cfg(debug_assertions)] let mut last_state: Option<StateId> = None;
            #[cfg(debug_assertions)] let mut last_offset: Option<u64> = None;
            #[cfg(debug_assertions)] let mut infinite_loop_cnt = 0_u32;
            loop {
                if VERBOSE { print!("- state = {state}"); }
                let input = self.input.as_mut().unwrap();
                #[cfg(debug_assertions)] {
                    if last_state.map(|st| st == state).unwrap_or(false) && last_offset.map(|offset| offset == input.get_offset()).unwrap_or(false) {
                        if infinite_loop_cnt > 3 {
                            self.error = LexerError::InfiniteLoop { pos: self.pos };
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
                self.is_eos = is_eos;
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
                    let is_accepting = self.first_end_state <= state && state < self.nbr_states;
                    if is_accepting { // accepting
                        let curr_start_state = self.start_state;
                        let terminal = &self.terminal_table[state - self.first_end_state];
                        if terminal.pop {
                            if self.state_stack.is_empty() {
                                self.error = LexerError::EmptyStateStack {
                                    info: LexerErrorInfo {
                                        pos: self.pos,
                                        line: self.line,
                                        col: self.col,
                                        curr_char: c_opt,
                                        group,
                                        state,
                                        text: more_text + &text,
                                    }
                                };
                                if VERBOSE { println!(" => Err({})", self.error); }
                                return Err(self.error.clone());
                            }
                            self.start_state = self.state_stack.pop().unwrap();
                            if VERBOSE { print!(", pop to {}", self.start_state); }
                        }
                        if let Some(goto_state) = terminal.mode_state {
                            if terminal.mode.is_push() {
                                self.state_stack.push(curr_start_state);
                            }
                            self.start_state = goto_state;
                            if VERBOSE { print!(", {}({})", if terminal.mode.is_push() { "push" } else { "mode" }, goto_state); }
                        }
                        if let Some(token) = &terminal.get_token() {
                            if VERBOSE { println!(" => OK: token {}", token); }
                            return Ok(Some((token.clone(), terminal.channel, more_text + &text, line, col)));
                        }
                        if !terminal.action.is_more() {
                            (line, col) = (self.line, self.col);
                        }
                        if !is_eos { // we can't skip if <EOF> or we'll loop indefinitely
                            if VERBOSE { println!(" => {}, state {}", terminal.action, self.start_state); }
                            state = self.start_state;
                            if terminal.action.is_more() {
                                more_text.push_str(&text);
                            }
                            text.clear();
                            continue;
                        }
                    }
                    // EOF or invalid character
                    if is_eos && is_accepting {
                        return Ok(None);
                    }
                    let info = LexerErrorInfo {
                        pos: self.pos,
                        line: self.line,
                        col: self.col,
                        curr_char: c_opt,
                        group,
                        state,
                        text: more_text + &text,
                    };
                    self.error = if is_eos {
                        LexerError::EndOfStream { info }
                    } else if group >= self.nbr_groups {
                        let c = input.get_char().unwrap();   // removing the bad character (not accepting state)
                        self.update_pos(c);
                        LexerError::UnrecognizedChar { info }
                    } else {
                        let c = input.get_char().unwrap();   // removing the bad character (not accepting state)
                        self.update_pos(c);
                        LexerError::InvalidChar { info }
                    };
                    if VERBOSE { println!(" => Err({})", self.error); }
                    return Err(self.error.clone());
                } else {
                    if let Some(c) = c_opt {
                        text.push(c);
                        self.update_pos(c);
                    }
                    if VERBOSE { println!(" => state {new_state}"); }
                    state = new_state;
                }
            }
        }
        self.error = LexerError::NoStreamAttached;
        if VERBOSE { println!(" => Err({})", self.error); }
        Err(self.error.clone())
    }

    pub fn update_pos(&mut self, c: char) {
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
        self.pos += 1;
    }

    pub fn get_error(&self) -> &LexerError {
        &self.error
    }

    pub fn has_error(&self) -> bool {
        self.error != LexerError::None
    }

    pub fn is_eos(&self) -> bool {
        self.is_eos
        // matches!(self.error, LexerError::EndOfStream { .. })
    }

    /// Returns the set of characters that are valid at the given lexer state.
    ///
    /// Note: This function can be quite slow, as it must test every possibility.
    pub fn get_valid_segments(&self, state: StateId) -> Segments {
        if state >= self.first_end_state {
            Segments::dot()     // accepting state
        } else {
            let mut result = Segments::empty();
            let groups = (0..self.nbr_groups)
                .filter_map(|g| if self.state_table[self.nbr_groups as usize * state + g as usize] < self.nbr_states { Some(g) } else { None })
                .collect::<HashSet<_>>();
            // examines all the ASCII codes:
            for c in 0..128 {
                if groups.contains(&self.ascii_to_group[c]) {
                    result.insert_utf8(c as u32, c as u32);
                }
            }
            // examines all the valid UTF8 codepoints:
            for range in [128..=UTF8_LOW_MAX, UTF8_HIGH_MIN..=UTF8_MAX] {
                for utf in range {
                    let char = char::from_u32(utf).unwrap();
                    if let Some(group) = self.utf8_to_group.get(&char) {
                        if groups.contains(group) {
                            result.insert_utf8(utf, utf); // determine start-stop ranges instead of inserting all codes?
                        }
                    }
                }
            }
            // examines all the remaining segments:
            for (seg, group) in &self.seg_to_group {
                if groups.contains(group) {
                    result.insert(*seg);
                }
            }
            result
        }
    }

}

#[derive(Debug)]
enum LexInterpretIterMode { Normal, Error }

pub struct LexInterpretIter<'a, R> {
    lexer: &'a mut Lexer<R>,
    error_info: Option<LexerErrorInfo>,
    mode: LexInterpretIterMode
}

impl<'a, R: Read> Iterator for LexInterpretIter<'a, R> {
    type Item = LexerToken; // (TokenId, ChannelId, String, CaretLine, CaretCol);
/*
    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.is_eos {
            None
        } else {
            let t = self.lexer.get_token();
            match t {
                Ok(Some(token)) => Some(token),
                _ => None
            }
        }
    }
*/
    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.is_eos {
            None
        } else {
            match self.mode {
                LexInterpretIterMode::Normal => {
                    let t = self.lexer.get_token();
                    match t {
                        Ok(Some(token)) => Some(token),
                        Err(LexerError::InvalidChar { info } | LexerError::UnrecognizedChar { info }) => {
                            // in case of invalid or unrecognized character, the stream issues None then a special lexer tokens
                            // that have a TokenId::MAX value and the error message in the text field
                            self.error_info = Some(info);
                            self.mode = LexInterpretIterMode::Error;
                            None
                        }
                        _ => {
                            None
                        }
                    }
                }
                LexInterpretIterMode::Error => {
                    let info = self.error_info.as_ref().unwrap();
                    self.mode = LexInterpretIterMode::Normal;
                    let msg = format!("{}, scanned before = '{}'", self.lexer.get_error().to_string(), self.error_info.as_ref().unwrap().text);
                    Some((TokenId::MAX, 0, msg, info.line, info.col))
                }
            }
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
