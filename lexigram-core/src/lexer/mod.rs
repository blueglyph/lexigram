// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(crate) mod tests;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Read;
use std::ops::{Add, AddAssign};
use crate::segmap::{char_to_group, GroupId, SegMap};
use crate::char_reader::{escape_char, CharReader};
use crate::TokenId;
// ---------------------------------------------------------------------------------------------
// Types used in lexer

pub type StateId = usize;
pub type ChannelId = u16;
pub type ModeId = u16;

/// Terminal instructions for the lexer logic.
///
/// Possible actions:
/// * skip           => doesn't return token, drops current string
/// * more           => doesn't return token, keeps current string for next rule
/// * push(n)        => pushes mode and switches to mode `n`
/// * pop            => pops next mode from the stack
/// * channel #      => defines output channel
///
/// By default, `push`, `pop`, `channel` or no specified action outputs a token (`token = Some(..)`).
/// If a `skip` or `more` action is specified, no token is returned (`token = None`).
#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub struct Terminal {
    pub action: ActionOption,
    pub channel: ChannelId,
    pub mode: ModeOption,
    pub mode_state: Option<StateId>,
    pub pop: bool
}

impl Terminal {
    #[inline]
    pub fn is_only_skip(&self) -> bool {
        self.action.is_skip() && self.mode.is_none() && self.mode_state.is_none() && !self.pop
    }

    #[inline]
    pub fn is_token(&self) -> bool {
        self.action.is_token()
    }

    #[inline]
    pub fn get_token(&self) -> Option<TokenId> {
        self.action.get_token()
    }

    pub fn to_macro(&self) -> String {
        let mut str = Vec::<String>::new();
        match self.action {
            ActionOption::Skip => str.push("term!(skip)".to_string()),
            ActionOption::Token(t) => str.push(format!("term!(={t})")),
            ActionOption::More => str.push("term!(more)".to_string())
        }
        if self.channel != 0 {
            str.push(format!("term!(#{})", self.channel));
        }
        match self.mode {
            ModeOption::None => {}
            ModeOption::Mode(m) => str.push(format!("term!(mode {m})")),
            ModeOption::Push(m) => str.push(format!("term!(push {m})")),
        }
        if let Some(id) = self.mode_state {
            str.push(format!("term!(pushst {})", id));
        }
        if self.pop {
            str.push("term!(pop)".to_string());
        }
        str.join(" + ")
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}", self.action)?;
        if self.channel != 0 { write!(f, ",ch {}", self.channel)?; }
        if !self.mode.is_none() || self.mode_state.is_some() {
            match self.mode {
                ModeOption::None => {}
                ModeOption::Mode(m) => write!(f, ",mode({m}")?,
                ModeOption::Push(m) => write!(f, ",push({m}")?,
            }
            if let Some(s) = self.mode_state { write!(f, ",state {s}")?; }
            write!(f, ")")?;
        }
        if self.pop { write!(f, ",pop")?; }
        write!(f, ">")
    }
}

impl Add for Terminal {
    type Output = Terminal;

    fn add(self, rhs: Self) -> Self::Output {
        Terminal {
            // token: if self.token.is_some() { self.token } else { rhs.token },
            action: self.action + rhs.action,
            channel: self.channel + rhs.channel,
            mode: if !self.mode.is_none() { self.mode } else { rhs.mode },
            mode_state: if self.mode_state.is_some() { self.mode_state } else { rhs.mode_state },
            pop: self.pop || rhs.pop
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum ActionOption {
    #[default] Skip,
    Token(TokenId),
    More
}

impl ActionOption {
    pub fn is_skip(&self) -> bool { self == &ActionOption::Skip }
    pub fn is_token(&self) -> bool { matches!(self, ActionOption::Token(_) ) }
    pub fn is_more(&self) -> bool { self == &ActionOption::More }

    pub fn get_token(&self) -> Option<TokenId> {
        if let ActionOption::Token(token) = self {
            Some(*token)
        } else {
            None
        }
    }
}

impl Add for ActionOption {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            ActionOption::Skip => rhs,
            _ => if rhs.is_skip() { self } else { panic!("can't add {self:?} and {rhs:?}") }
        }
    }
}

impl Display for ActionOption {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionOption::Skip => write!(f, "skip"),
            ActionOption::Token(t) => write!(f, "end:{t}"),
            ActionOption::More => write!(f, "more")
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum ModeOption {
    #[default]
    None,
    Mode(ModeId),
    Push(ModeId)
}

impl ModeOption {
    pub fn is_none(&self) -> bool {
        self == &ModeOption::None
    }

    pub fn is_mode(&self) -> bool {
        matches!(self, &ModeOption::Mode(_))
    }

    pub fn is_push(&self) -> bool {
        matches!(self, &ModeOption::Push(_))
    }
}

// ---------------------------------------------------------------------------------------------
// Locations

pub type CaretCol = u64;
pub type CaretLine = u64;

/// `Pos(line, col)`
#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub struct Pos(pub CaretLine, pub CaretCol);

impl Pos {
    pub fn line(&self) -> CaretLine {
        self.0
    }

    pub fn col(&self) -> CaretCol {
        self.1
    }
}

/// `PosSpan` defines a text selection where `first` and `last` are the [position](Pos) of the first and last character.
/// When `first` > `last`, no text is selected.
#[derive(Clone, PartialEq, Debug)]
pub struct PosSpan {
    pub first: Pos,
    pub last: Pos,
}

impl PosSpan {
    #[inline(always)]
    pub fn new(first: Pos, last: Pos) -> Self {
        PosSpan { first, last }
    }

    #[inline(always)]
    pub fn empty() -> Self {
        PosSpan { first: Pos(1, 1), last: Pos(0, 0) }
    }

    pub fn take(&mut self) -> PosSpan {
        std::mem::take(self)
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.first > self.last
    }

    #[inline(always)]
    pub fn is_not_empty(&self) -> bool {
        self.first <= self.last
    }

    pub fn first(&self) -> Option<Pos> {
        if self.is_not_empty() { Some(self.first) } else { None }
    }

    pub fn first_forced(&self) -> Pos {
        if self.is_not_empty() { self.first } else { panic!("span is empty") }
    }

    pub fn last(&self) -> Option<Pos> {
        if self.is_not_empty() { Some(self.last) } else { None }
    }

    pub fn last_forced(&self) -> Pos {
        if self.is_not_empty() { self.last } else { panic!("span is empty") }
    }
}

impl AddAssign<&PosSpan> for PosSpan {
    fn add_assign(&mut self, rhs: &Self) {
        match (self.is_empty(), rhs.is_empty()) {
            (true, false) => (self.first, self.last) = (rhs.first, rhs.last),
            (false, false) => self.last = rhs.last,
            _ => {}
        }
    }
}

impl Default for PosSpan {
    fn default() -> Self {
        PosSpan::empty()
    }
}

impl Display for PosSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_not_empty() {
            let (first, last) = (&self.first, &self.last);
            if first == last {
                write!(f, "{}:{}", first.0, first.1)
            } else if first.0 == last.0 {
                write!(f, "{}:{}-{}", first.0, first.1, last.1)
            } else {
                write!(f, "{}:{}-{}:{}", first.0, first.1, last.0, last.1)
            }
        } else {
            write!(f, "<empty>")
        }
    }
}

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

pub type LexerToken = (TokenId, ChannelId, String, PosSpan);

/// Lexical analyzer (lexer) based on tables, which scans a `Read` source and produces tokens.
///
/// The tokens can be extracted one by one with [`get_token()`](Lexer::get_token) or from an
/// iterator created by [`tokens()`](Lexer::tokens).
pub struct Lexer<'a, R> {
    // operating variables
    pub(crate) input: Option<CharReader<R>>,
    pub(crate) error: LexerError,
    pub(crate) is_eos: bool,
    pub(crate) pos: u64,
    pub(crate) line: CaretLine,
    pub(crate) col: CaretCol,
    pub(crate) tab_width: u8,
    pub(crate) state_stack: Vec<StateId>,
    pub(crate) start_state: StateId,
    // parameters
    pub nbr_groups: u32,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    // tables
    pub ascii_to_group: &'a [GroupId],
    pub utf8_to_group: HashMap<char, GroupId>,
    pub seg_to_group: SegMap<GroupId>,
    pub state_table: &'a [StateId],
    pub terminal_table: &'a [Terminal],  // token(state) = token_table[state - first_end_state]
}

impl<'a, R: Read> Lexer<'a, R> {
    pub fn new(
        // parameters
        nbr_groups: u32,
        initial_state: StateId,
        first_end_state: StateId,   // accepting when state >= first_end_state
        nbr_states: StateId,        // error if state >= nbr_states
        // tables
        ascii_to_group: &'a [GroupId],
        utf8_to_group: HashMap<char, GroupId>,
        seg_to_group: SegMap<GroupId>,
        state_table: &'a [StateId],
        terminal_table: &'a [Terminal],  // token(state) = token_table[state - first_end_state>]
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

    pub fn tokens(&mut self) -> LexInterpretIter<'_, 'a, R> {
        LexInterpretIter { lexer: self, error_info: None, mode: LexInterpretIterMode::Normal }
    }

    // get_token flow:
    //
    //      if input.is_none
    //          return error
    //      state = start
    //      startpos = endpos = self.(line, col)
    //      loop
    //          next char
    //          group       -> group == nbr_groups => unrecognized
    //          next_state  -> [normal] < first_end_state <= [accepting] <= nbr_states <= [invalid char]
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
    //                      return (token, channel, span(startpos, endpos))
    //                  startpos = self.(line, col)
    //                  if !EOS
    //                      state = start
    //                      continue // skip
    //              return error/EOS
    //          else
    //              endpos = self.(line, col)
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
            let mut first_pos = Pos(self.line, self.col);
            let mut last_pos = first_pos;
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
                                self.state_stack.push(self.start_state);
                            }
                            self.start_state = goto_state;
                            if VERBOSE { print!(", {}({})", if terminal.mode.is_push() { "push" } else { "mode" }, goto_state); }
                        }
                        if let Some(token) = &terminal.get_token() {
                            if VERBOSE { println!(" => OK: token {}", token); }
                            return Ok(Some((token.clone(), terminal.channel, more_text + &text, PosSpan::new(first_pos, last_pos))));
                        }
                        if !terminal.action.is_more() {
                            first_pos = Pos(self.line, self.col);
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
                    last_pos = Pos(self.line, self.col);
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
}

#[derive(Debug)]
enum LexInterpretIterMode { Normal, Error }

pub struct LexInterpretIter<'a, 'b, R> {
    lexer: &'a mut Lexer<'b, R>,
    error_info: Option<LexerErrorInfo>,
    mode: LexInterpretIterMode
}

impl<'a, 'b, R: Read> Iterator for LexInterpretIter<'a, 'b, R> {
    type Item = LexerToken; // (TokenId, ChannelId, String, CaretLine, CaretCol);

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
                    let pos = Pos(info.line, info.col);
                    Some((TokenId::MAX, 0, msg, PosSpan::new(pos, pos)))
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

pub trait TokenSpliterator: Iterator<Item=(TokenId, ChannelId, String, PosSpan)> {
    /// Splits the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, PosSpan)`) based on the channel ID:
    /// * the default channel 0 is output as another iterator on `(token, string, pos_span)`, suitable for the parser
    /// * other channels are consummed by the closure `f`, which takes the parameters `(token, channel, string, pos_span)`
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().split_channel0(|(tok, ch, text, pos_span)|
    ///     println!("TOKEN: channel {ch}, discarded, pos {pos_span}, Id {tok:?}, \"{text}\"")
    /// );
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn split_channel0<F>(self, f: F) -> TokenSplit<Self, F>
    where Self: Sized,
          F: FnMut((TokenId, ChannelId, String, PosSpan))
    {
        TokenSplit { iter: self, ch: 0, f }
    }

    /// Splits the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, PosSpan)`) based on the channel ID:
    /// * the channel `channel` is output as another iterator on `(token, string, pos_span)`, suitable for the parser
    /// * other channels are consummed by the closure `f`, which takes the parameters `(token, channel, string, pos_span)`
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().split_channels(2, |(tok, ch, text, pos_span)|
    ///     println!("TOKEN: channel {ch}, discarded, pos {pos_span}, Id {tok:?}, \"{text}\"")
    /// );
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn split_channels<F>(self, channel: ChannelId, f: F) -> TokenSplit<Self, F>
    where Self: Sized,
          F: FnMut((TokenId, ChannelId, String, PosSpan))
    {
        TokenSplit { iter: self, ch: channel, f }
    }

    /// Filters the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, PosSpan)`) based on the channel ID:
    /// * the default channel 0 is output as another iterator on `(token, string, pos_span)`, suitable for the parser
    /// * other channels are discarded.
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().keep_channel0();
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn keep_channel0(self) -> impl Iterator<Item=(TokenId, String, PosSpan)>
    where Self: Sized
    {
        self.filter_map(|(token, ch, str, pos_span)| {
            if ch == 0 {
                Some((token, str, pos_span))
            } else {
                None
            }
        })
    }

    /// Filters the token iterator out of the lexer (Item: `(TokenId, ChannelId, String, PosSpan)`) based on the channel ID:
    /// * channel `channel` is output as another iterator on `(token, string, pos_span)`, suitable for the parser
    /// * other channels are discarded.
    ///
    /// ## Example
    /// ```ignore
    /// let tokens = lexer.tokens().keep_channel(2);
    /// let result = parser.parse_stream(&mut listener, tokens);
    /// ```
    fn keep_channel(self, channel: ChannelId) -> TokenSplit<Self, fn((TokenId, ChannelId, String, PosSpan))>
    where Self: Sized
    {
        TokenSplit { iter: self, ch: channel, f: |_| {} }
    }

    // or:
    //
    // fn keep_channel(self, channel: ChannelId) -> impl Iterator<Item=(TokenId, String, PosSpan)>
    // where Self: Sized
    // {
    //     self.filter_map(move |(token, ch, str, pos_span)| {
    //         if ch == channel {
    //             Some((token, str, pos_span))
    //         } else {
    //             None
    //         }
    //     })
    // }
}

impl<I, F> Iterator for TokenSplit<I, F>
    where I: Iterator<Item=(TokenId, ChannelId, String, PosSpan)>,
          F: FnMut((TokenId, ChannelId, String, PosSpan))
{
    type Item = (TokenId, String, PosSpan);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((token, ch, str, pos_span)) = self.iter.next() {
            if ch == self.ch {
                Some((token, str, pos_span))
            } else {
                (self.f)((token, ch, str, pos_span));
                None
            }
        } else {
            None
        }
    }
}

impl<I: Iterator<Item=(TokenId, ChannelId, String, PosSpan)>> TokenSpliterator for I {}
