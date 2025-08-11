// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs::File;
use std::io::{BufWriter, Read, Write};
use iter_index::IndexerIterator;
#[cfg(test)]
use crate::dfa::print_graph;
use crate::{CollectJoin, escape_char, Normalized, indent_source, SymbolTable};
use crate::lexer::{Lexer, LexerError};
use crate::log::{BufLog, BuildFrom, LogReader, LogStatus, Logger, TryBuildFrom};
use crate::segments::{Segments, Seg, SegMap};
use super::dfa::*;

// ---------------------------------------------------------------------------------------------

/// Tables and parameters used to create a Lexer. This type is used as a return object from the lexer generator,
/// when the Lexer must be created dynamically; for example, in tests or in situations where the lexicon isn't
/// known in advance. In those situations, the LexerTables object must live as long as the lexer.
///
/// The Lexer itself only uses references to tables whenever possible because, in most situations, the tables are
/// static in generated source files. Only the dictionaries must be created dynamically from (possibly) static
/// tables because they don't exist in static form (yet).
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

    pub fn make_lexer<R: Read>(&self) -> Lexer<'_, R> {
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
            nbr_groups: self.nbr_groups,
            initial_state: self.initial_state,
            first_end_state: self.first_end_state,
            nbr_states: self.nbr_states,
            ascii_to_group: self.ascii_to_group.as_slice(),
            utf8_to_group: self.utf8_to_group.clone(),
            seg_to_group: self.seg_to_group.clone(),
            state_table: self.state_table.as_slice(),
            terminal_table: self.terminal_table.as_slice(),
        }
    }
}

impl BuildFrom<LexerGen> for LexerTables {
    fn build_from(lexer_gen: LexerGen) -> LexerTables {
        assert!(!lexer_gen.state_table.is_empty(), "tables are not built");
        LexerTables::new(
            lexer_gen.nbr_groups,
            lexer_gen.initial_state,
            lexer_gen.first_end_state,
            lexer_gen.nbr_states,
            lexer_gen.ascii_to_group,
            lexer_gen.utf8_to_group,
            lexer_gen.seg_to_group,
            lexer_gen.state_table,
            lexer_gen.terminal_table,
        )
    }
}

// not generated automatically since LexerTables isn't LogReader
impl TryBuildFrom<LexerGen> for LexerTables {
    type Error = BufLog;

    fn try_build_from(source: LexerGen) -> Result<Self, Self::Error> {
        if source.get_log().has_no_errors() {
            Ok(LexerTables::build_from(source))
        } else {
            Err(source.give_log())
        }
    }
}

// ---------------------------------------------------------------------------------------------

pub type GroupId = u32;

pub struct LexerGen {
    // parameters:
    pub max_utf8_chars: u32,
    pub nbr_groups: u32,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    // tables:
    pub ascii_to_group: Vec<GroupId>,
    pub utf8_to_group: HashMap<char, GroupId>,
    pub seg_to_group: SegMap<GroupId>,
    pub state_table: Vec<StateId>,
    pub terminal_table: Vec<Terminal>,  // token(state) = token_table[state - first_end_state]
    pub symbol_table: Option<SymbolTable>,
    // internal
    log: BufLog,
    group_partition: Segments,   // for optimization
}

impl LexerGen {
    pub const DEFAULT_UTF8_TABLE_SIZE: u32 = 128;

    fn new() -> Self {
        LexerGen {
            max_utf8_chars: Self::DEFAULT_UTF8_TABLE_SIZE,
            nbr_groups: 0,
            initial_state: 0,
            first_end_state: 0,
            nbr_states: 0,
            ascii_to_group: vec![GroupId::MAX; 128],
            utf8_to_group: HashMap::default(),
            seg_to_group: SegMap::new(),
            state_table: Vec::new(),
            terminal_table: Vec::new(),
            symbol_table: None,
            log: BufLog::new(),
            group_partition: Segments::empty(),
        }
    }

    // pub fn get_mut_log(&mut self) -> &mut BufLog {
    //     &mut self.log
    // }

    pub fn build_from_dfa(dfa: Dfa<Normalized>, max_utf8_chars: u32) -> Self {
        let mut lexergen = Self::new();
        lexergen.max_utf8_chars = max_utf8_chars;
        lexergen.make_from_dfa(dfa);
        lexergen
    }

    fn make_from_dfa(&mut self, mut dfa: Dfa<Normalized>) {
        self.log.extend(std::mem::replace(&mut dfa.log, BufLog::new()));
        self.create_input_tables(&dfa);
        self.create_state_tables(&dfa);
    }

    fn create_input_tables(&mut self, dfa: &Dfa<Normalized>) {
        /// Max continuous segment size translated to individual UTF8 entries. This prevents large segments
        /// like DOT from cluttering the dictionary.
        const MAX_UTF8_SEG_RANGE: u32 = 16;
        const VERBOSE: bool = false;
        let symbol_part = partition_symbols(dfa.get_state_graph());
        let symbol_to_group = SegMap::from_iter(
            symbol_part.iter().index().flat_map(|(id, i)| i.iter().map(move |ab| (*ab, id)))
        );
        self.group_partition = Segments::from_iter(symbol_to_group.keys().cloned());

        if VERBOSE {
            println!("symbol partition:{}\ntables:", symbol_to_group.iter()
                .map(|(seg, g)| format!("\n- {seg} -> {g}")).collect::<String>());
        }
        self.nbr_groups = symbol_part.len() as GroupId;
        let error_id = self.nbr_groups as GroupId;
        self.ascii_to_group.fill(error_id);
        self.utf8_to_group.clear();
        self.seg_to_group.clear();
        let mut left = self.max_utf8_chars;
        for (seg, group_id) in symbol_to_group {
            if VERBOSE { println!("Seg: {}-{}", seg.0, seg.1); }
            if seg.0 < 128 {
                if VERBOSE {
                    println!("- ASCII: {}-{} ({}-{}) => {group_id}",
                             escape_char(char::from_u32(seg.0).unwrap()), escape_char(char::from_u32(seg.1.min(127)).unwrap()),
                             seg.0, seg.1.min(127));
                }
                for b in seg.0..=seg.1.min(127) {
                    self.ascii_to_group[b as usize] = group_id;
                }
            }
            if seg.1 >= 128 {
                let mut low = 128.max(seg.0);
                let high = seg.1.min(low + left - 1);
                if seg.1 - low < MAX_UTF8_SEG_RANGE {
                    if left > 0 {
                        for u in low..=high {
                            if VERBOSE { println!("- UTF8: {} ({u}) => {group_id}", escape_char(char::from_u32(u).unwrap())); }
                            self.utf8_to_group.insert(char::from_u32(u).unwrap(), group_id);
                        }
                        left -= 1 + high - low;
                    }
                    low = high + 1;
                }
                if low <= seg.1 {
                    if VERBOSE {
                        println!("- SEG: {}-{} ({}-{}) => {group_id}",
                             escape_char(char::from_u32(high + 1).unwrap()), escape_char(char::from_u32(seg.1).unwrap()),
                             low, seg.1);
                    }
                    self.seg_to_group.insert(Seg(low, seg.1), group_id);
                }
            }
        }
    }

    fn create_state_tables(&mut self, dfa: &Dfa<Normalized>) {
        const VERBOSE: bool = false;
        self.initial_state = dfa.get_initial_state().unwrap();
        self.first_end_state = dfa.get_first_end_state().unwrap();
        self.nbr_states = dfa.get_state_graph().len();
        let nbr_states = dfa.get_state_graph().len();
        // we add one extra table index to allow for the 'error group', which equals nbr_group:
        // state_table[nbr_state * nbr_group + nbr_group] must exist; the content will be ignored.
        let mut state_table = vec!(self.nbr_states; self.nbr_groups as usize * nbr_states + 1);
        for (state_from, trans) in dfa.get_state_graph() {
            if VERBOSE { println!("state {state_from}"); }
            for (segments, state_to) in trans {
                if VERBOSE { println!("- {segments} -> state {state_to}"); }
                let mut segments_part = segments.clone();
                segments_part.slice_partitions(&self.group_partition);
                for seg in segments_part.iter() {
                    let symbol = char::from_u32(seg.0).unwrap();
                    let symbol_group = char_to_group(&self.ascii_to_group, &self.utf8_to_group, &self.seg_to_group, symbol).unwrap_or(self.nbr_groups);
                    state_table[self.nbr_groups as usize * state_from + symbol_group as usize] = *state_to;
                }
            }
        }
        self.state_table = state_table;
        let terminal_table = dfa.get_end_states().iter()
            .filter_map(|(&st, t)| if st >= self.first_end_state { Some(t.clone()) } else { None })
            .to_vec();
        self.terminal_table = terminal_table;
        let max_token_maybe = self.terminal_table.iter().fold(None, { |acc, t|
            if let Terminal { action: ActionOption::Token(tok), .. } = t {
                Some(acc.unwrap_or(0).max(*tok))
            } else {
                acc
            }
        });
        match max_token_maybe {
            Some(max_token) => {
                if max_token == TokenId::MAX {
                    self.log.add_error(format!("the token {} is taken, but it's reserved for illegal characters", TokenId::MAX));
                }
            }
            None => {
                self.log.add_error("the lexer returns no tokens");
            }
        }
    }
    
    pub fn write_source_code(&self, file: Option<File>, indent: usize) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };
        let source = self.gen_source_code(indent);
        out.write(source.as_bytes())?;
        // write!(out, "{source}");
        Ok(())
    }

    pub fn gen_source_code(&self, indent: usize) -> String {
        indent_source(vec![self.lexer_source_code()], indent)
    }

    pub fn try_gen_source_code(self, indent: usize) -> Result<String, BufLog> {
        let src = self.gen_source_code(indent);
        if self.log.has_no_errors() {
            Ok(src)
        } else {
            Err(self.give_log())
        }
    }

    fn lexer_source_code(&self) -> Vec<String> {
        let mut source = Vec::<String>::new();

        // Create source code:
        source.push(format!("use std::collections::HashMap;"));
        source.push(format!("use std::io::Read;"));
        source.push(format!("use lexigram_lib::dfa::{{StateId, Terminal, ActionOption, ModeOption}};"));
        source.push(format!("use lexigram_lib::lexer::Lexer;"));
        source.push(format!("use lexigram_lib::lexergen::GroupId;"));
        source.push(format!("use lexigram_lib::segments::{{Seg, SegMap}};"));
        source.push(String::new());
        source.push(format!("const NBR_GROUPS: u32 = {};", self.nbr_groups));
        source.push(format!("const INITIAL_STATE: StateId = {};", self.initial_state));
        source.push(format!("const FIRST_END_STATE: StateId = {};", self.first_end_state));
        source.push(format!("const NBR_STATES: StateId = {};", self.nbr_states));
        let mut groups = vec![BTreeSet::new(); self.nbr_groups as usize];
        source.push(format!("static ASCII_TO_GROUP: [GroupId; 128] = ["));
        for i in 0..8_usize {
            let mut s = "    ".to_string();
            for j in 0..16_usize {
                let ascii = i * 16 + j;
                let group = self.ascii_to_group[i * 16 + j];
                s.push_str(&format!("{:3}, ", group));
                if group < self.nbr_groups {
                    groups[group as usize].insert(char::from(ascii as u8));
                }
            }
            source.push(format!("{s}  // {}-{}", i*16, i*16 + 15));
        }
        source.push(format!("];"));
        source.push(format!("static UTF8_TO_GROUP: [(char, GroupId); {}] = [", self.utf8_to_group.len()));
        for (c, g) in &self.utf8_to_group {
            source.push(format!("    ('{}', {}),", escape_char(*c), g));
        }
        source.push(format!("];"));
        /*
        for (c, g) in lexergen.utf8_to_group.iter() {
            groups[*g as usize].insert(*c);
        }
        for (g, chars) in groups.iter().enumerate() {
            if !chars.is_empty() {
                let set = chars.iter().map(|c| escape_char(*c)).collect::<String>();
                source.push(format!("// group[{g:3}] = [{set}]"));
            };
        }
        */
        source.push(format!("static SEG_TO_GROUP: [(Seg, GroupId); {}] = [", self.seg_to_group.len()));
        for (s, g) in &self.seg_to_group {
            source.push(format!("    (Seg({}, {}), {}),", s.0, s.1, g));
        }
        source.push(format!("];"));
        source.push(format!("static TERMINAL_TABLE: [Terminal;{}] = [", self.terminal_table.len()));
        for t in &self.terminal_table {
            // Terminal { action: TermAction::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            source.push(format!("    Terminal {{ action: ActionOption::{:?}, channel: {}, mode: ModeOption::{:?}, mode_state: {:?}, pop: {} }},",
                                t.action, t.channel, t.mode, t.mode_state, t.pop
            ));
        }
        source.push(format!("];"));
        source.push(format!("static STATE_TABLE: [StateId; {}] = [", self.state_table.len()));
        for i in 0..self.nbr_states as usize {
            source.push(format!("    {}, // state {}{}",
                (0..self.nbr_groups as usize).map(|j| format!("{:3}", self.state_table[i * self.nbr_groups as usize + j])).join(", "),
                i,
                if i >= self.first_end_state { format!(" {}", self.terminal_table[i - self.first_end_state] ) } else { "".to_string() }
            ));
        }
        source.push(format!("    {:3} // error group in [nbr_state * nbr_group + nbr_group]", self.state_table[self.state_table.len() - 1]));
        source.push(format!("];"));
        source.push(String::new());
        source.push(format!("pub fn build_lexer<R: Read>() -> Lexer<'static, R> {{"));
        source.push(format!("    Lexer::new("));
        source.push(format!("        // parameters"));
        source.push(format!("        NBR_GROUPS,"));
        source.push(format!("        INITIAL_STATE,"));
        source.push(format!("        FIRST_END_STATE,"));
        source.push(format!("        NBR_STATES,"));
        source.push(format!("        // tables"));
        source.push(format!("        &ASCII_TO_GROUP,"));
        source.push(format!("        HashMap::<char, GroupId>::from(UTF8_TO_GROUP),"));
        source.push(format!("        SegMap::<GroupId>::from(SEG_TO_GROUP),"));
        source.push(format!("        &STATE_TABLE,"));
        source.push(format!("        &TERMINAL_TABLE,"));
        source.push(format!("    )"));
        source.push(format!("}}"));
        source
    }
}

impl LogReader for LexerGen {
    type Item = BufLog;

    fn get_log(&self) -> &Self::Item {
        &self.log
    }

    fn give_log(self) -> Self::Item {
        self.log
    }
}

impl BuildFrom<Dfa<Normalized>> for LexerGen {
    fn build_from(dfa: Dfa<Normalized>) -> Self {
        let mut lexgen = LexerGen::new();
        lexgen.make_from_dfa(dfa);
        lexgen
    }
}

// ---------------------------------------------------------------------------------------------
// Supporting functions

#[inline]
pub fn char_to_group(ascii_to_group: &[GroupId], utf8_to_group: &HashMap<char, GroupId>, seg_to_group: &SegMap<GroupId>, symbol: char) -> Option<GroupId> {
    if symbol.len_utf8() == 1 {
        Some(ascii_to_group[u8::try_from(symbol).unwrap() as usize])
    } else {
        utf8_to_group.get(&symbol).cloned().or_else(|| seg_to_group.get(symbol as u32))
    }
}

// todo: option to split ASCII range?
fn partition_symbols(g: &BTreeMap<StateId, BTreeMap<Segments, StateId>>) -> Vec<Segments> {
    const VERBOSE: bool = false;
    let mut groups = Vec::new();
    #[cfg(test)] if VERBOSE { print_graph(g, None, 4); }
    for (_state, branches) in g {
        // branches from a given state
        let mut map = BTreeMap::<StateId, Segments>::new();
        for (segments, destination) in branches {
            if let Some(i) = map.get_mut(destination) {
                i.extend(&mut segments.iter());
            } else {
                map.insert(*destination, segments.clone());
            }
        }
        // optimizes the segments, in case it's not already done
        for segments in map.values_mut() {
            segments.normalize();
        }
        #[cfg(test)] if VERBOSE { println!("{_state} => {}", map.values().map(|i| format!("{i:X}")).join(", ")); }
        let mut state_sub = map.into_values().collect::<BTreeSet<Segments>>();
        while let Some(mut sub) = state_sub.pop_first() {
            if VERBOSE { println!("- sub = {sub}"); }
            for i in 0..groups.len() {
                if VERBOSE { println!("  - groups[{i}] = {}", groups[i]); }
                let cmp = sub.intersect(&groups[i]);
                if cmp.common.is_empty() {
                    if VERBOSE { println!("    (disjoints)"); }
                } else {
                    // groups[i] is split { cmp.common, cmp.external }
                    groups[i] = cmp.common;
                    if !cmp.external.is_empty() {
                        if VERBOSE { println!("    -> push {}", cmp.external); }
                        groups.push(cmp.external);
                    }
                    // sub is split { cmp.common, cmp.internal }, and we discard cmp.common since already in groups
                    if VERBOSE { println!("    -> sub = {}", cmp.internal); }
                    sub = cmp.internal;
                    if sub.is_empty() {
                        break;
                    }
                }
            }
            if !sub.is_empty() {
                groups.push(sub);
            }
        }
    }
    #[cfg(test)] if VERBOSE { println!("=> {}", groups.iter().map(|i| format!("{i:X}")).join(", ")); }
    groups
}
