pub(crate) mod tests;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs::File;
use std::io::Read;
use std::io::{BufWriter, Write};
#[cfg(test)]
use crate::dfa::tests::print_graph;
use crate::{CollectJoin, escape_char, Normalized, indent_source};
use crate::lexer::Lexer;
use crate::segments::{Segments, Seg, SegMap};
use super::dfa::*;

pub type GroupId = u32;

pub struct LexerGen {
    // parameters:
    pub max_utf8_chars: u32,
    pub nbr_groups: u32,
    pub initial_state: StateId,
    pub first_end_state: StateId,   // accepting when state >= first_end_state
    pub nbr_states: StateId,        // error if state >= nbr_states
    // tables:
    pub ascii_to_group: Box<[GroupId]>,
    pub utf8_to_group: HashMap<char, GroupId>,
    pub seg_to_group: SegMap<GroupId>,
    pub state_table: Box<[StateId]>,
    pub terminal_table: Box<[Terminal]>,  // token(state) = token_table[state - first_end_state]
    // internal
    group_partition: Segments,   // for optimization
}

impl LexerGen {
    pub fn new() -> Self {
        LexerGen {
            max_utf8_chars: 128,
            nbr_groups: 0,
            initial_state: 0,
            first_end_state: 0,
            nbr_states: 0,
            ascii_to_group: vec![GroupId::MAX; 128].into_boxed_slice(),
            utf8_to_group: HashMap::default(),
            seg_to_group: SegMap::new(),
            state_table: Box::default(),
            terminal_table: Box::default(),
            group_partition: Segments::empty(),
        }
    }

    pub fn from_dfa(dfa: &Dfa<Normalized>) -> Self {
        let mut lexgen = LexerGen::new();
        lexgen.build_tables(dfa);
        lexgen
    }

    pub fn build_tables(&mut self, dfa: &Dfa<Normalized>) {
        self.create_input_tables(dfa);
        self.create_state_tables(dfa);
    }

    pub fn make_lexer<R: Read>(self) -> Lexer<R> {
        assert!(!self.state_table.is_empty(), "tables are not built");
        Lexer::new(
            self.nbr_groups,
            self.initial_state,
            self.first_end_state,
            self.nbr_states,
            self.ascii_to_group,
            self.utf8_to_group,
            self.seg_to_group,
            self.state_table,
            self.terminal_table,
        )
    }

    fn create_input_tables(&mut self, dfa: &Dfa<Normalized>) {
        const VERBOSE: bool = false;
        let symbol_part = partition_symbols(dfa.get_state_graph());
        let symbol_to_group = SegMap::from_iter(
            symbol_part.iter().enumerate().flat_map(|(id, i)| i.iter().map(move |ab| (*ab, id as GroupId)))
        );
        self.group_partition = Segments(BTreeSet::from_iter(symbol_to_group.keys().cloned()));

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
                let low = 128.max(seg.0);
                let high = seg.1.min(low + left - 1);
                if left > 0 {
                    for u in low..=high {
                        if VERBOSE { println!("- UTF8: {} ({u}) => {group_id}", escape_char(char::from_u32(u).unwrap())); }
                        self.utf8_to_group.insert(char::from_u32(u).unwrap(), group_id);
                    }
                    left -= 1 + high - low;
                }
                if high < seg.1 {
                    if VERBOSE {
                        println!("- SEG: {}-{} ({}-{}) => {group_id}",
                             escape_char(char::from_u32(high + 1).unwrap()), escape_char(char::from_u32(seg.1).unwrap()),
                             high + 1, seg.1);
                    }
                    self.seg_to_group.insert(Seg(high + 1, seg.1), group_id);
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
        self.state_table = state_table.into_boxed_slice();
        let terminal_table = dfa.get_end_states().iter()
            .filter_map(|(&st, t)| if st >= self.first_end_state { Some(t.clone()) } else { None })
            .to_vec();
        self.terminal_table = terminal_table.into_boxed_slice();
    }
    
    pub fn write_source_code(&self, file: Option<File>, indent: usize) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };
        let source = self.build_source_code(indent);
        out.write(source.as_bytes())?;
        // write!(out, "{source}");
        Ok(())
    }

    pub fn build_source_code(&self, indent: usize) -> String {
        indent_source(vec![self.lexer_source_code()], indent)
    }

    fn lexer_source_code(&self) -> Vec<String> {
        let mut source = Vec::<String>::new();

        // Create source code:
        source.push(format!("use std::collections::HashMap;"));
        source.push(format!("use std::io::Read;"));
        source.push(format!("use rlexer::dfa::{{StateId, Terminal}};"));
        source.push(format!("use rlexer::lexer::Lexer;"));
        source.push(format!("use rlexer::lexergen::GroupId;"));
        source.push(format!("use rlexer::segments::{{Seg, SegMap}};"));
        source.push(String::new());
        source.push(format!("const NBR_GROUPS: u32 = {};", self.nbr_groups));
        source.push(format!("const INITIAL_STATE: StateId = {};", self.initial_state));
        source.push(format!("const FIRST_END_STATE: StateId = {};", self.first_end_state));
        source.push(format!("const NBR_STATES: StateId = {};", self.nbr_states));
        let mut groups = vec![BTreeSet::new(); self.nbr_groups as usize];
        source.push(format!("const ASCII_TO_GROUP: [GroupId; 128] = ["));
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
        source.push(format!("const UTF8_TO_GROUP: [(char, GroupId); {}] = [", self.utf8_to_group.len()));
        for (c, g) in &self.utf8_to_group {
            source.push(format!("('{}', {}),", escape_char(*c), g));
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
        source.push(format!("const SEG_TO_GROUP: [(Seg, GroupId); {}] = [", self.seg_to_group.len()));
        for (s, g) in &self.seg_to_group {
            source.push(format!("    (Seg({}, {}), {}),", s.0, s.1, g));
        }
        source.push(format!("];"));
        source.push(format!("const TERMINAL_TABLE: [Terminal;{}] = [", self.terminal_table.len()));
        for t in &self.terminal_table {
            source.push(format!("    {t:?},"));
        }
        source.push(format!("];"));
        source.push(format!("const STATE_TABLE: [StateId; {}] = [", self.state_table.len() - 1));
        for i in 0..self.nbr_states as usize {
            source.push(format!("    {}, // state {}{}",
                (0..self.nbr_groups as usize).map(|j| format!("{:3}", self.state_table[i * self.nbr_groups as usize + j])).join(", "),
                i,
                if i >= self.first_end_state { format!(" {}", self.terminal_table[i - self.first_end_state] ) } else { "".to_string() }
            ));
        }
        source.push(format!("];"));
        source.push(String::new());
        source.push(format!("pub(super) fn build_lexer<R: Read>() -> Lexer<R> {{"));
        source.push(format!("    Lexer::new("));
        source.push(format!("        // parameters"));
        source.push(format!("        NBR_GROUPS,"));
        source.push(format!("        INITIAL_STATE,"));
        source.push(format!("        FIRST_END_STATE,"));
        source.push(format!("        NBR_STATES,"));
        source.push(format!("        // tables"));
        source.push(format!("        Box::new(ASCII_TO_GROUP),"));
        source.push(format!("        HashMap::<char, GroupId>::from(UTF8_TO_GROUP),"));
        source.push(format!("        SegMap::<GroupId>::from_iter(SEG_TO_GROUP),"));
        source.push(format!("        Box::new(STATE_TABLE),"));
        source.push(format!("        Box::new(TERMINAL_TABLE)"));
        source.push(format!("    )"));
        source.push(format!("}}"));
        source
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
    #[cfg(test)] if VERBOSE { print_graph(g, None); }
    for (_state, branches) in g {
        // branches from a given state
        let mut map = BTreeMap::<StateId, Segments>::new();
        for (segments, destination) in branches {
            if let Some(i) = map.get_mut(destination) {
                i.extend(&segments.0);
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
