// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Range};
use iter_index::IndexerIterator;
use vectree::VecTree;
use lexigram_lib::{hashset, CollectJoin};
use lexigram_lib::build::BuildFrom;
use lexigram_lib::dfa::{retree_to_str, tree_to_string, Dfa, DfaBuilder, DfaBundle, ReType};
use lexigram_lib::dfa::ReNode;
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::{hashmap, node, segments, General, Normalized, SymbolTable, TokenId};
use lexigram_lib::lexer::{ActionOption, ChannelId, ModeId, ModeOption, Pos, PosSpan, Terminal};
use lexigram_lib::parser::Terminate;
use lexigram_lib::segments::Segments;
use crate::action;
use crate::lexi::lexiparser::*;
use crate::lexi::lexiparser::lexiparser_types::*;

#[derive(Clone, Copy, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum LexActionOption {
    #[default]
    None,
    Skip,
    Token(TokenId),
    More,
}

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
/// Actions:
/// - type(token)   => returns token (if not specified, returns the token of the current rule)
/// - skip          => doesn't return token, current string dropped (modifier)
/// - more          => doesn't return token, current string kept for next rule (modifier)
/// - mode(n)       => returns token + change mode
/// - push(n)       => returns token + push mode
/// - pop           => returns token + pop mode
/// - channel(n)    => returns token + channel
/// - hook          => triggers a listener callback
pub struct LexAction {
    pub option: LexActionOption,
    pub channel: Option<ChannelId>,
    pub mode: ModeOption,
    pub pop: bool,
    pub hook: bool,
}

impl LexAction {
    /// Attempts to convert the action into a `Terminal`. If no action specified what to do - return a token,
    /// `skip`, or `more`, uses the `token_maybe` to return that token or returns an error if that isn't
    /// specified either.
    pub fn to_terminal(&self, token_maybe: Option<TokenId>) -> Result<Terminal, ()> {
        Ok(Terminal {
            action: match self.option {
                LexActionOption::None => if let Some(t) = token_maybe { ActionOption::Token(t) } else { return Err(()) }
                LexActionOption::Skip => ActionOption::Skip,
                LexActionOption::Token(t) => ActionOption::Token(t),
                LexActionOption::More => ActionOption::More
            },
            channel: self.channel.unwrap_or(0),
            mode: self.mode,
            mode_state: None,
            pop: self.pop,
        })
    }

    pub fn try_add(self, rhs: LexAction) -> Result<LexAction, (String, LexAction, LexAction)> {
        if (self.option != LexActionOption::None && rhs.option != LexActionOption::None) ||
            (self.channel.is_some() && rhs.channel.is_some()) ||
            (!self.mode.is_none() && !rhs.mode.is_none())
        {
            return Err((format!("can't add {self:?} and {rhs:?}"), self, rhs))
        }
        Ok(LexAction {
            option: if self.option == LexActionOption::None { rhs.option } else { self.option },
            channel: self.channel.or(rhs.channel),
            mode: if !self.mode.is_none() { self.mode } else { rhs.mode },
            pop: self.pop || rhs.pop,
            hook: self.hook || rhs.hook,
        })
    }

    pub fn to_str<F, G>(&self, tok_to_str: F, mode_to_str: G) -> String
    where F: Fn(TokenId) -> (String, bool),
          G: Fn(ModeId) -> String
    {
        let mut result = Vec::<String>::new();
        if let Some(ch) = self.channel {
            result.push(format!("channel({ch})"))
        }
        match self.mode {
            ModeOption::None => {}
            ModeOption::Mode(m) => result.push(format!("mode({})", mode_to_str(m))),
            ModeOption::Push(m) => result.push(format!("push({})", mode_to_str(m))),
        }
        if self.pop {
            result.push("pop".to_string());
        }
        if self.hook {
            result.push("hook".to_string());
        }
        match self.option {
            LexActionOption::None => {}
            LexActionOption::Skip => result.push("skip".to_string()),
            LexActionOption::Token(t) => {
                let (name, is_self) = tok_to_str(t);
                result.push(if is_self { name } else { format!("type({})", name) })
            },
            LexActionOption::More => result.push("more".to_string()),
        }
        result.join(", ")
    }
}

impl Add for LexAction {
    type Output = Self;

    /// Adds two [LexAction] items. Note that [LexAction::default()] is neutral
    /// for the addition, so it can be used as starting element to fold a list.
    fn add(self, rhs: Self) -> Self::Output {
        match self.try_add(rhs) {
            Ok(a) => a,
            Err(s) => panic!("{}", s.0)
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RuleType {
    Fragment(TokenId),
    Terminal(TokenId)
}

pub struct LexiListener {
    verbose: bool,
    name: String,
    curr: Option<VecTree<ReNode>>,
    curr_mode: ModeId,
    /// Current fragment or terminal name when parsing a rule, otherwise `None`
    curr_name: Option<String>,
    /// Dictionary of terminals and fragments
    pub(crate) rules: HashMap<String, RuleType>,
    /// VecTree of each fragment.
    pub(crate) fragments: Vec<VecTree<ReNode>>,
    /// Optional constant literal for fragments; e.g. `COMMA: ',';` -> Some(',')
    pub(crate) fragment_literals: Vec<Option<String>>,
    /// VecTree of each terminal. Some may be empty: `type(token)` with no corresponding rule.
    pub(crate) terminals: Vec<VecTree<ReNode>>,
    /// Optional constant literal for terminal keywords; e.g. `COMMA: ',';` -> Some(',')
    pub(crate) terminal_literals: Vec<Option<String>>,
    /// Terminals with a hook
    pub(crate) terminal_hooks: Vec<TokenId>,
    /// True if the corresponding terminal is returned by the lexer (either directly by a rule or as the target of a `type(T)`)
    terminal_ret: Vec<bool>,
    /// Future tokens reserved by `type(T)`.
    terminal_reserved: HashSet<String>,
    /// Terminal token IDs that need to be remapped (from key to value)
    terminal_remap: HashMap<TokenId, TokenId>,
    channels: HashMap<String, ChannelId>,
    modes: HashMap<String, ModeId>,
    /// Modes that are actually referenced in actions
    referenced_modes: HashSet<ModeId>,
    /// Range of terminals defined in `fragments` for each mode.
    mode_terminals: Vec<Option<Range<TokenId>>>,
    log: BufLog,
    abort: Terminate,
    /// Starting position of `"grammar" Id ";"`, if detected
    pos_grammar_opt: Option<Pos>,
}

impl LexiListener {
    // WARNING: this method isn't efficient; it's only used for error messages or test results. Don't use
    //          it when performances are required.
    fn token_to_string(&self, token: TokenId) -> (String, bool) {
        for (name, ruletype) in &self.rules {
            if matches!(ruletype, RuleType::Terminal(t) if t == &token) {
                return (name.clone(), name == self.curr_name.as_ref().unwrap())
            }
        }
        ("??".to_string(), false)
    }

    // WARNING: this method isn't efficient; it's only used for error messages or test results. Don't use
    //          it when performances are required.
    fn mode_to_string(&self, mode: ModeId) -> String {
        for (name, m) in &self.modes {
            if m == &mode {
                return name.to_string();
            }
        }
        format!("{mode}?")
    }

    pub fn new() -> Self {
        LexiListener {
            verbose: false,
            name: String::new(),
            curr: None,
            curr_mode: 0,
            curr_name: None,
            rules: HashMap::new(),
            fragments: Vec::new(),
            fragment_literals: Vec::new(),
            terminals: Vec::new(),
            terminal_literals: Vec::new(),
            terminal_hooks: Vec::new(),
            terminal_ret: Vec::new(),
            terminal_reserved: HashSet::new(),
            terminal_remap: HashMap::new(),
            channels: hashmap!("DEFAULT_CHANNEL".to_string() => 0),
            modes: hashmap!("DEFAULT_MODE".to_string() => 0),
            referenced_modes: hashset!(0),
            mode_terminals: vec![Some(0..0)],
            log: BufLog::new(),
            abort: Terminate::None,
            pos_grammar_opt: None,
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_pos_grammar(&self) -> Option<Pos> {
        self.pos_grammar_opt
    }

    fn get_sorted_modes(&self) -> Vec<(ModeId, &str)> {
        let mut sorted_modes = vec![(0, ""); self.modes.len()];
        for (name, id) in &self.modes {
            sorted_modes[*id as usize] = (*id, name);
        }
        sorted_modes
    }

    pub fn make_symbol_table(&self) -> SymbolTable {
        let mut table = SymbolTable::new();
        if self.log.has_no_errors() {
            let num_ret = self.terminal_ret.iter().filter(|&ret| *ret).count();
            let mut symbols = vec![(String::new(), None); num_ret];
            for (s, rt) in &self.rules {
                if let RuleType::Terminal(id) = rt {
                    if self.terminal_ret[*id as usize] {
                        let final_token_id = *self.terminal_remap.get(id).unwrap_or(id);
                        symbols[final_token_id as usize] = (s.clone(), self.terminal_literals[*id as usize].clone())
                    }
                };
            }
            table.extend_terminals(symbols);
        }
        table
    }

    pub(crate) fn rules_to_vecstrings(&self) -> Vec<String> {
        let mut lines = vec!["lexicon summary:".to_string(), "- modes:".to_string()];
        lines.extend(self.get_sorted_modes().into_iter().map(|(id, name)| format!("  {id:2}: {name}")));
        lines.push("- channels:".to_string());
        let mut sorted_channels = self.channels.iter().map(|(name, id)| (id, name)).to_vec();
        sorted_channels.sort();
        lines.extend(sorted_channels.into_iter().map(|(id, name)| format!("  {id:2}: {name}")));
        let mut cols = vec![vec!["  type".to_string(), "name".to_string(), "lit".to_string(), "mode".to_string(),
                                 "ret".to_string(), "token ".to_string(), "tree".to_string()]];
        let mut rules = self.rules.iter().to_vec();
        rules.sort_by(|a, b| (&a.1, &a.0).cmp(&(&b.1, &b.0)));
        for (s, rt) in rules.into_iter() {
            let (t, lit, ret, sym_maybe, _end_maybe) = match rt {
                RuleType::Fragment(id) => (
                    self.fragments.get(*id as usize).unwrap(),
                    self.fragment_literals.get(*id as usize).unwrap(),
                    None,
                    None,
                    None
                ),
                RuleType::Terminal(id) => {
                    let ret = *self.terminal_ret.get(*id as usize).unwrap_or_else(|| panic!("no item {id}"));
                    (
                        self.terminals.get(*id as usize).unwrap_or_else(|| panic!("no item {id}")),
                        self.terminal_literals.get(*id as usize).unwrap_or_else(|| panic!("no item {id}")),
                        Some(ret),
                        if ret { Some(self.terminal_remap.get(&(*id as TokenId)).unwrap_or(id)) } else { None },
                        self.terminals[*id as usize].iter_post_depth_simple().find_map(|n|
                            // unfortunately, we can't destructure entirely because term is a Box
                            if let ReType::End(term) = n.get_type() {
                                if let ActionOption::Token(end) = term.action { Some(end) } else { None }
                            } else {
                                None
                            }
                        )

                    )
                },
            };
            let mode = if let RuleType::Terminal(id) = rt {
                if let Some(imode) = self.mode_terminals.iter()
                    .position(|range_opt| range_opt.as_ref().map(|r| r.contains(id)).unwrap_or(false))
                {
                    imode.to_string()
                } else {
                    "-".to_string()
                }
            } else {
                String::new()
            };
            cols.push(vec![format!("  {rt:?}"),
                           format!("{s}"),
                           format!("{}", lit.as_ref().map(|s| format!("{s:?}")).unwrap_or_default()),
                           format!("{mode}"),
                           format!("{}", if let Some(b) = ret { if b { "Y" } else { "N" } } else { "" }),
                           if let Some(sym) = sym_maybe { format!("{sym}") } else { String::new() },
                           // format!("{}", tree_to_string(t, None, true)),
                           format!("{}", retree_to_str(t, None, None, false)),
            ]);
        }
        lines.push("- definitions:".to_string());
        let mut table = lexigram_lib::columns_to_str(cols, None);
        table.insert(1, format!("  {:-<width$}", "", width = table.first().unwrap().len() + 10));
        lines.extend(table);
        lines
    }

    pub(crate) fn rules_to_string(&self, indent: usize) -> String {
        let mut cols_out = self.rules_to_vecstrings();
        let title = cols_out.remove(0);
        let tab = format!("{: <width$}", "", width = indent);
        format!("{tab}   {title}\n{tab}{:-<width$}{}", "",
                 cols_out.into_iter().map(|l| format!("\n{tab} - {l}")).join(""),
                 width=title.len() + 5
        )
    }

    /// Checks that the reserved IDs don't become lower than terminal IDs, which would yield identical IDs for
    /// different terminals. This method assumes that either of them is about to be added, and so that either
    /// length is about to increase (as such, the sum of both lengths being MAX + 1 would be fine).
    fn check_reserve_boundary(&mut self) {
        if self.terminals.len() + self.terminal_reserved.len() > TokenId::MAX as usize {
            self.log.add_error("collision between terminals and reserved terminals (too many of them)".to_string());
            self.abort = Terminate::Abort;
        }
    }

    fn add_fragment_or_abort(&mut self) -> TokenId {
        TokenId::try_from(self.fragments.len()).unwrap_or_else(|_| {
            self.log.add_error(format!("too many fragments, max {}", TokenId::MAX));
            self.abort = Terminate::Abort;
            0
        })
    }

    fn add_terminal_or_abort(&mut self) -> TokenId {
        self.check_reserve_boundary();
        TokenId::try_from(self.terminals.len()).unwrap_or_else(|_| {
            self.log.add_error(format!("too many terminals, max {}", TokenId::MAX));
            self.abort = Terminate::Abort;
            0
        })
    }

    fn add_terminal_reserved_or_abort(&mut self) -> TokenId {
        self.check_reserve_boundary();
        if self.terminal_reserved.len() > TokenId::MAX as usize {
            self.log.add_error(format!("too many terminals reserved, max {}", TokenId::MAX));
            self.abort = Terminate::Abort;
            0
        } else {
            TokenId::MAX - self.terminal_reserved.len() as TokenId
        }
    }

    /// Gets the mode id if it exists, otherwise adds it. Generates an abort if there are too many of them.
    fn get_add_mode_or_abort(&mut self, mode_name: String) -> ModeId {
        if let Some(id) = self.modes.get(&mode_name) {
            *id
        } else {
            match ModeId::try_from(self.modes.len()) {
                Ok(id) => {
                    self.modes.insert(mode_name, id);
                    id
                }
                Err(_) => {
                    self.log.add_error(format!("too many modes, max {}", ModeId::MAX));
                    self.abort = Terminate::Abort;
                    0
                }
            }
        }
    }

    fn post_process(&mut self) {
        if self.verbose {
            println!("post_process: terminal_reserved = {:?}", self.terminal_reserved);
        }
        for id in &self.terminal_reserved {
            let Some(RuleType::Terminal(reserved_id)) = self.rules.get_mut(id) else { panic!("cannot find reserved {id}") };
            let rule_id = self.terminals.len();
            if *reserved_id as usize >= rule_id {
                let rule_id = rule_id as TokenId; // safe
                if self.verbose { println!("reserved {id} ({reserved_id}) wasn't an existing terminal, creating one in {rule_id}"); }
                self.terminal_remap.insert(*reserved_id, rule_id);
                self.terminals.push(VecTree::new());
                self.terminal_literals.push(None);
                self.terminal_ret.push(true);
                *reserved_id = rule_id;
            }
        }

        // Deletes token IDs that are never returned
        let mut remap: HashMap<TokenId, TokenId> = HashMap::new();
        let mut new_id = 0;
        for (old_id, ret) in self.terminal_ret.iter().index::<TokenId>() {
            if *ret {
                if old_id != new_id {
                    remap.insert(old_id, new_id);
                }
                new_id += 1;
            }
        }
        for (reserved_id, new_id) in std::mem::take(&mut self.terminal_remap) {
            let new_new_id = remap.get(&new_id).cloned().unwrap_or(new_id); // takes new remap into account
            remap.insert(reserved_id, new_new_id);
        }

        // Remaps the token IDs
        if self.verbose {
            let mut bremap = BTreeMap::<TokenId, TokenId>::new();
            bremap.extend(remap.iter().map(|(a, b)| (*a, *b)));
            println!("Remap:{}", bremap.iter().map(|(a, b)| format!("{a} -> {b}")).join(", "));
        }
        for tree in self.terminals.iter_mut() {
            // changes all the temporary references since that first one
            for mut node in tree.iter_post_depth_simple_mut() {
                let x: &mut ReType = node.get_mut_type();
                if let ReType::End(term) = x {
                    if let ActionOption::Token(old_id) = term.action {
                        if let Some(new_id) = remap.get(&old_id) {
                            term.action = ActionOption::Token(*new_id);
                        }
                    }
                }
            }
        }
        // Remaps the hooks
        self.terminal_hooks.iter_mut()
            .for_each(|old| *old = *remap.get(old).unwrap_or(old));
        self.terminal_remap = remap;

        // Checks the modes
        if self.verbose { println!("\nModes: "); }
        let mut mode_errors = vec![];
        let mut mode_warnings = vec![];
        for (mode_id, mode) in self.get_sorted_modes() {
            if !self.referenced_modes.contains(&mode_id) {
                mode_warnings.push(format!("mode '{mode}' is defined but not used"));
            }
            let terminals = match self.mode_terminals.get(mode_id as usize) {
                Some(Some(range)) => format!("{range:?}"),
                _ => {
                    mode_errors.push(format!("mode '{mode}' referenced but not defined"));
                    "## ERROR: undefined".to_string()
                }
            };
            if self.verbose { println!("- {mode_id} - {mode}: terminals = {terminals}"); }
        }
        for err in mode_errors {
            self.log.add_error(err);
        }
        for warn in mode_warnings {
            self.log.add_warning(warn);
        }
    }
}

impl LogReader for LexiListener {
    type Item = BufLog;

    fn get_log(&self) -> &BufLog {
        &self.log
    }

    fn give_log(self) -> BufLog {
        self.log
    }
}

impl BuildFrom<LexiListener> for Dfa<Normalized> {
    /// Makes and optimizes the DFA
    fn build_from(source: LexiListener) -> Self {
        const VERBOSE: bool = false;
        if !source.log.has_no_errors() {
            // if there are errors, we bypass any processing and return a shell containing the log
            return Dfa::<Normalized>::with_log(source.log);
        }
        let num_t = source.terminals.len();
        let mut names = vec![String::new(); num_t];
        for (s, r) in &source.rules {
            if let RuleType::Terminal(token) = r {
                names[*token as usize] = s.clone();
            }
        }
        let mut dfas = vec![];
        let sorted_modes = source.get_sorted_modes();
        for (mode_id, mode_name) in sorted_modes {
            if VERBOSE { println!("mode {mode_id}: {mode_name}"); }
            let range = source.mode_terminals[mode_id as usize].as_ref().unwrap();
            if VERBOSE { println!("* mode {mode_id}: {mode_name} = rules {range:?} ({})", range.clone().map(|n| &names[n as usize]).join(", ")); }
            let range = range.start as usize..range.end as usize;
            let mut tree = VecTree::<ReNode>::new();
            let top = tree.add_root(node!(|));
            for (i, t) in source.terminals[range].iter().enumerate() {
                if VERBOSE { println!("- adding tree for terminal {i}: {t:?}"); }
                if !t.is_empty() {
                    let cc = tree.add(Some(top), node!(&));
                    tree.add_from_tree(Some(cc), t, None);
                }
            }
            if VERBOSE { println!("  => {}", tree_to_string(&tree, None, true)); }
            let dfa_builder = DfaBuilder::build_from(tree);
            assert_eq!(dfa_builder.get_log().num_errors(), 0, "failed to compile mode {mode_id}");
            dfas.push((mode_id, Dfa::<General>::build_from(dfa_builder)));
            if VERBOSE { dfas[dfas.len() - 1].1.print(5); }
        }
        if VERBOSE { println!("merging dfa modes"); }
        let log = source.log;
        let dfa = Dfa::<General>::build_from(DfaBundle::with_log(log, dfas));
        dfa.optimize()
    }
}

impl Debug for LexiListener {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "LexiListener {{")?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  curr = {}", self.curr.as_ref().map(|t| tree_to_string(t, None, true)).unwrap_or_default())?;
        writeln!(f, "  curr_mode = {}", self.curr_mode)?;
        writeln!(f, "  rules: {}\n{}", self.rules.len(), self.rules_to_string(4))?;
        writeln!(f, "  fragments: {}", self.fragments.len())?;
        writeln!(f, "  fragment_literals: {}", self.fragment_literals.len())?;
        writeln!(f, "  terminals: {}", self.terminals.len())?;
        writeln!(f, "  terminal_literals: {}", self.terminal_literals.len())?;
        writeln!(f, "  terminal_ret: {}", self.terminal_ret.len())?;
        writeln!(f, "  terminal_reserved: {}", self.terminal_reserved.iter().map(|s| s.to_string()).join(", "))?;
        let mut bremap = BTreeMap::<TokenId, TokenId>::new();
        bremap.extend(self.terminal_remap.iter().map(|(a, b)| (*a, *b)));
        let s = bremap.iter().map(|(a, b)| format!("{a} -> {b}")).join(", ");
        writeln!(f, "  terminal_remap: {s}")?;
        writeln!(f, "  log:\n{}", self.log)?;
        writeln!(f, "  abort: {:?}", self.abort)?;
        writeln!(f, "  pos_grammar_opt: {:?}", self.pos_grammar_opt)?;
        writeln!(f, "}}")
    }
}

impl LexiParserListener for LexiListener {
    fn check_abort_request(&self) -> Terminate {
        self.abort
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit(&mut self, _file: SynFile, _span: PosSpan) {
        self.post_process();
    }

    fn abort(&mut self, terminate: Terminate) {
        if terminate == Terminate::Conclude {
            self.post_process();
        }
    }

    fn exit_file(&mut self, _ctx: CtxFile, _spans: Vec<PosSpan>) -> SynFile {
        SynFile()
    }

    fn exit_file_item(&mut self, _ctx: CtxFileItem, _spans: Vec<PosSpan>) -> SynFileItem {
        SynFileItem()
    }

    fn exit_header(&mut self, ctx: CtxHeader, _spans: Vec<PosSpan>) -> SynHeader {
        let CtxHeader::V1 { id } = ctx;
        self.name = id;
        SynHeader()
    }

    fn exit_declaration(&mut self, ctx: CtxDeclaration, _spans: Vec<PosSpan>) -> SynDeclaration {
        if self.verbose { print!("- exit_declaration({ctx:?}), modes: {:?}", self.modes); }
        let CtxDeclaration::V1 { id: mode_name } = ctx;    // declaration -> mode Id ;
        let mode_id = self.get_add_mode_or_abort(mode_name);
        if self.abort == Terminate::None {
            self.curr_mode = mode_id;
            let next_token = self.add_terminal_or_abort();
            if self.mode_terminals.len() <= mode_id as usize {
                self.mode_terminals.resize(mode_id as usize + 1, None);
            }
            self.mode_terminals[mode_id as usize] = Some(next_token..next_token);
            if self.verbose { println!(" -> mode {mode_id}, mode_terminals: {:?}", self.mode_terminals) }
        }
        SynDeclaration()
    }

    fn exit_option(&mut self, ctx: CtxOption, _spans: Vec<PosSpan>) -> SynOption {
        if self.verbose { println!("- exit_option({ctx:?})"); }
        let CtxOption::V1 { star } = ctx;       // option -> "channels" "{" Id ("," Id)* "}"
        for ch in star.0 {
            if self.channels.contains_key(&ch) {
                self.log.add_error(format!("channel '{ch}' defined twice"));
            } else {
                self.channels.insert(ch, self.channels.len() as ChannelId);
            }
        }
        SynOption()
    }

    fn exit_rule(&mut self, ctx: CtxRule, spans: Vec<PosSpan>) -> SynRule {
        if self.verbose { println!("- exit_rule({ctx:?})"); }
        let (id, rule_type, action_maybe, const_literal, reserve_only) = match ctx {
            // `rule -> rule_fragment_name ":" match ";"`
            CtxRule::V1 { rule_fragment_name: SynRuleFragmentName(id), match1 } => {
                let rule_id = self.add_fragment_or_abort();
                (id, RuleType::Fragment(rule_id), None, match1.0, false)
            }
            // `rule -> rule_terminal_name ":" match "->" actions ";"`
            CtxRule::V2 { rule_terminal_name: SynRuleTerminalName(id), match1, actions } => {
                let rule_id = self.add_terminal_or_abort();
                (id, RuleType::Terminal(rule_id), Some(actions.0), match1.0, false)
            }
            // `rule -> rule_terminal_name ":" match ";"`
            CtxRule::V3 { rule_terminal_name: SynRuleTerminalName(id), match1 } => {
                let rule_id = self.add_terminal_or_abort();
                (id, RuleType::Terminal(rule_id), None, match1.0, false)
            }
            // `rule -> "(" rule_terminal_name ")" opt_str_lit "->" "hook" ";"`
            CtxRule::V4 { rule_terminal_name: SynRuleTerminalName(id), opt_str_lit: SynOptStrLit(opt_str) } => {
                let rule_id = self.add_terminal_or_abort();
                (id, RuleType::Terminal(rule_id), Some(action!(hook)), opt_str, true)
            }
            // `rule -> "(" rule_terminal_name ")" opt_str_lit ";"`
            CtxRule::V5 { rule_terminal_name: SynRuleTerminalName(id), opt_str_lit: SynOptStrLit(opt_str) } => {
                let rule_id = self.add_terminal_or_abort();
                (id, RuleType::Terminal(rule_id), None, opt_str, true)
            }
            CtxRule::V6 { rule_terminal_name: SynRuleTerminalName(token), id } => {
                if token != "grammar" {
                    let PosSpan { first: Pos(line, col), .. } = &spans[1];
                    self.log.add_error(format!("syntax error: found input '{id}' instead of ':', line {line}, col {col}", ))
                } else {
                    let PosSpan { first, .. } = &spans[0];
                    self.log.add_note(format!("detected grammar beginning at line {}, col {}", first.0, first.1));
                    self.pos_grammar_opt = Some(*first);
                    self.abort = Terminate::Conclude;
                }
                return SynRule();
            }
        };
        if self.abort != Terminate::None { return SynRule() }
        let was_reserved = if let Some(RuleType::Terminal(reserved_id)) = self.rules.get_mut(&id) {
            // checks that it's indeed reserved and not a simple conflict with another terminal name
            if self.terminal_reserved.contains(&id) {
                // reserved_id is a temporary, reserved TokenId introduced by an action `-> type(X)`
                let RuleType::Terminal(rule_id) = rule_type else { panic!() };
                if self.verbose { println!("terminal {id} was reserved as {reserved_id}; will now be {rule_id}"); }
                self.terminal_remap.insert(*reserved_id, rule_id);
                *reserved_id = rule_id;
                true
            } else {
                false   // conflicts with another terminal
            }
        } else {
            false       // fragment or doesn't exist yet
        };
        if !was_reserved && self.rules.contains_key(&id) {
            self.log.add_error(format!("rule {}: symbol '{id}' is already defined", self.curr_name.as_ref().unwrap()));
            self.abort = Terminate::Abort;
        } else {
            let rule_maybe = self.curr.take();
            match rule_type {
                RuleType::Fragment(_) => {
                    self.fragments.push(rule_maybe.unwrap());
                    self.fragment_literals.push(const_literal);
                }
                RuleType::Terminal(rule_id) => {
                    self.terminal_literals.push(const_literal);
                    self.mode_terminals.get_mut(self.curr_mode as usize).unwrap().as_mut().unwrap().end += 1;
                    // if no action, only return
                    let rule_action = action_maybe.unwrap_or(action!(= rule_id));
                    if rule_action.hook {
                        self.terminal_hooks.push(rule_id);
                    }
                    if reserve_only {
                        self.terminals.push(VecTree::new());
                        self.terminal_ret.push(true);
                    } else {
                        let mut rule = rule_maybe.unwrap();
                        let mut root = rule.get_root().unwrap();
                        if *rule.get(root).get_type() != ReType::Concat {
                            let new_root = rule.addci(None, node!(&), root);
                            rule.set_root(new_root);
                            root = new_root;
                        }
                        let terminal = rule_action.to_terminal(Some(rule_id)).unwrap();
                        let ret = was_reserved || matches!(terminal.action, ActionOption::Token(tid) if tid == rule_id);
                        rule.add(Some(root), ReNode::end(terminal));
                        self.terminals.push(rule);
                        self.terminal_ret.push(ret);
                    }
                }
            }
            if !was_reserved {
                self.rules.insert(id, rule_type);
            }
        }
        self.curr_name = None;
        // CAUTION! There are aborts above: don't add code here that could trigger secondary errors
        SynRule()
    }

    fn exit_opt_str_lit(&mut self, ctx: CtxOptStrLit, _spans: Vec<PosSpan>) -> SynOptStrLit {
        SynOptStrLit(match ctx {
            CtxOptStrLit::V1 { strlit } => {
                let s = decode_str(&strlit[1..strlit.len() - 1]).unwrap_or_else(|e| {
                    self.log.add_error(format!("rule {}: cannot decode the string literal {strlit}: {e}", self.curr_name.as_ref().unwrap()));
                    format!("♫{strlit}♫") // we make up the result to leave a chance to the parser to continue
                });
                Some(s)
            }
            CtxOptStrLit::V2 => None
        })
    }

    fn exit_rule_fragment_name(&mut self, ctx: CtxRuleFragmentName, _spans: Vec<PosSpan>) -> SynRuleFragmentName {
        let CtxRuleFragmentName::V1 { id } = ctx;
        self.curr_name = Some(id.clone());
        SynRuleFragmentName(id)
    }

    fn exit_rule_terminal_name(&mut self, ctx: CtxRuleTerminalName, _spans: Vec<PosSpan>) -> SynRuleTerminalName {
        let CtxRuleTerminalName::V1 { id } = ctx;
        self.curr_name = Some(id.clone());
        SynRuleTerminalName(id)
    }

    fn exit_actions(&mut self, ctx: CtxActions, _spans: Vec<PosSpan>) -> SynActions {
        let CtxActions::V1 { star } = ctx;
        let action = star.0.into_iter().fold(LexAction::default(), |acc, SynAction(a)| {
            acc.try_add(a).unwrap_or_else(|(_msg, left, right)| {
                self.log.add_error(format!("can't add actions '{}' and '{}'",
                                           left.to_str(|token| self.token_to_string(token), |mode| self.mode_to_string(mode)),
                                           right.to_str(|token| self.token_to_string(token), |mode| self.mode_to_string(mode))));
                left
            })
        });
        SynActions(action)
    }

    fn exit_action(&mut self, ctx: CtxAction, _spans: Vec<PosSpan>) -> SynAction {
        let action = match ctx {
            CtxAction::V1 { id } => {               // action -> "mode" "(" Id ")"
                let id_val = self.get_add_mode_or_abort(id);
                self.referenced_modes.insert(id_val);
                action!(mode id_val)
            }
            CtxAction::V2 { id } => {               // action -> "push" "(" Id ")"
                let id_val = self.get_add_mode_or_abort(id);
                self.referenced_modes.insert(id_val);
                action!(push id_val)
            },
            CtxAction::V3 => action!(pop),          // action -> "pop"
            CtxAction::V4 => action!(skip),         // action -> "skip"
            CtxAction::V5 => action!(more),         // action -> "more"
            CtxAction::V6 { id } => {               // action -> "type" "(" Id ")"
                // returns the token if that rule exists, otherwise reserves it; the action will have to be changed when we know the ID
                // we won't copy the terminal_literal to Id because
                // - there could be several rules with different literals
                // - most of the time, this wouldn't make sense and could even be misleading
                let token = match self.rules.get(&id) {
                    None => {
                        let reserved_token = self.add_terminal_reserved_or_abort();
                        self.terminal_reserved.insert(id.clone());
                        if self.verbose { println!("-> type({id}) : reserved ID {reserved_token}"); }
                        self.rules.insert(id, RuleType::Terminal(reserved_token));  // temporary token ID
                        reserved_token
                    }
                    Some(rule) => {
                        if let RuleType::Terminal(token) = rule {
                            self.terminal_ret[*token as usize] = true;
                            *token as TokenId
                        } else {
                            self.log.add_error(format!("action in rule {}: '{id}' is not a terminal; it's a fragment", self.curr_name.as_ref().unwrap()));
                            return SynAction(action!(nop));
                        }
                    }
                };
                action!(= token)
            }
            CtxAction::V7 { id } => {               // action -> "channel" "(" Id ")"
                // we don't allow to define new channels here on the fly because typos would induce annoying errors
                let channel = *self.channels.get(&id).unwrap_or_else(|| {
                    self.log.add_error(format!("action in rule {}: channel '{id}' undefined", self.curr_name.as_ref().unwrap()));
                    &0
                });
                action!(# channel)
            }
            CtxAction::V8 => {                      // action -> "hook"
                action!(hook)
            }
        };
        // CAUTION! There are aborts above: don't add code here that could trigger secondary errors
        SynAction(action)
    }

    fn init_match(&mut self) {
        assert!(self.curr.is_none(), "remnant tree in self.curr:\n{self:?}");
        self.curr = Some(VecTree::new());
    }

    fn exit_match(&mut self, ctx: CtxMatch, _spans: Vec<PosSpan>) -> SynMatch {
        // sets the tree root ID, so that any rule using `match` can expect to get a usable VecTree
        if self.verbose { println!("- exit_match({ctx:?})"); }
        let CtxMatch::V1 { alt_items: SynAltItems((id, const_literal)) } = ctx;
        self.curr.as_mut().unwrap().set_root(id);
        SynMatch(const_literal)
    }

    fn exit_alt_items(&mut self, ctx: CtxAltItems, _spans: Vec<PosSpan>) -> SynAltItems {
        if self.verbose { print!("- exit_alt_items({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let CtxAltItems::V1 { star: SynAltItems1(mut alt_items) } = ctx;
        let (id, const_literal) = if alt_items.len() > 1 {
            let id = tree.addci_iter(None, node!(|), alt_items.into_iter().map(|SynAltItem((id_ch, _))| id_ch));
            (id, None)
        } else {
            alt_items.pop().unwrap().0
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        // Item types have both an id and a const_literal. The const_literal serves to determine
        // whether the terminal is (sym, None) or (sym, Some(String)) in the symbol table.
        SynAltItems((id, const_literal))
    }

    fn exit_alt_item(&mut self, ctx: CtxAltItem, _spans: Vec<PosSpan>) -> SynAltItem {
        if self.verbose { print!("- exit_alt_item({ctx:?})"); }
        let CtxAltItem::V1 { plus: SynAltItem1(mut items) } = ctx;
        let tree = self.curr.as_mut().unwrap();
        let (id, const_literal) = if items.len() > 1 {
            let lit_maybe = items.iter().try_fold(String::new(), |acc, next| next.0.1.as_ref().map(|n| acc + n));
            (tree.addci_iter(None, node!(&), items.into_iter().map(|item| item.0.0)), lit_maybe)
        } else {
            items.pop().unwrap().0
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynAltItem((id, const_literal))
    }

    fn exit_repeat_item(&mut self, ctx: CtxRepeatItem, _spans: Vec<PosSpan>) -> SynRepeatItem {
        if self.verbose { print!("- exit_repeat_item({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let (id, const_literal) = match ctx {
            CtxRepeatItem::V1 { item } => {   // repeat_item -> item "*?" (lazy)
                let star = tree.addci(None, node!(*), item.0.0);
                (tree.addci(None, node!(??), star), None)
            }
            CtxRepeatItem::V2 { item } => {   // repeat_item -> item "*"
                (tree.addci(None, node!(*), item.0.0), None)
            }
            CtxRepeatItem::V3 { item } => {   // repeat_item -> item "+?" (lazy)
                let plus = tree.addci(None, node!(+), item.0.0);
                (tree.addci(None, node!(??), plus), None)
            }
            CtxRepeatItem::V4 { item } => {   // repeat_item -> item "+"
                (tree.addci(None, node!(+), item.0.0), None)
            }
            CtxRepeatItem::V5 { item } => {   // repeat_item -> item "?"
                let empty = tree.add(None, node!(e));
                (tree.addci_iter(None, node!(|), [item.0.0, empty]), None)
            }
            CtxRepeatItem::V6 { item } => {   // repeat_item -> item
                item.0
            }
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynRepeatItem((id, const_literal))
    }

    fn exit_item(&mut self, ctx: CtxItem, _spans: Vec<PosSpan>) -> SynItem {
        if self.verbose { print!("- exit_item({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let (id, const_literal) = match ctx {
            CtxItem::V1 { id } => {              // item -> Id
                if let Some(RuleType::Fragment(f)) = self.rules.get(&id) {
                    let subtree = self.fragments.get(*f as usize).unwrap();
                    let const_literal = self.fragment_literals.get(*f as usize).unwrap().clone();
                    (tree.add_from_tree(None, subtree, None), const_literal)
                } else {
                    self.log.add_error(format!("rule {}: unknown fragment '{id}'", self.curr_name.as_ref().unwrap()));
                    let fake = format!("♫{id}♫"); // we make up the result to leave a chance to the parser to continue
                    (tree.add(None, ReNode::string(&fake)), Some(fake))
                }
            }
            CtxItem::V2 { charlit } => {         // item -> CharLit ".." CharLit
                let [first, last] = charlit.map(|clit| {
                    decode_char(&clit[1..clit.len() - 1]).unwrap_or_else(|e| {
                        self.log.add_error(format!("rule {}: cannot decode the character literal '{clit}': {e}", self.curr_name.as_ref().unwrap()));
                        '♫' // we make up the result to leave a chance to the parser to continue
                    })
                });
                (tree.add(None, ReNode::char_range(Segments::from((first, last)))), None)
            }
            CtxItem::V3 { charlit } => {         // item -> CharLit
                // charlit is always sourrounded by quotes:
                // fragment CharLiteral	: '\'' Char '\'';
                let c = decode_char(&charlit[1..charlit.len() - 1]).unwrap_or_else(|e| {
                    self.log.add_error(format!("rule {}: cannot decode the character literal {charlit}: {e}", self.curr_name.as_ref().unwrap()));
                    '♫' // we make up the result to leave a chance to the parser to continue
                });
                (tree.add(None, ReNode::char_range(Segments::from(c))), Some(c.to_string()))
            }
            CtxItem::V4 { strlit } => {          // item -> StrLit
                let s = decode_str(&strlit[1..strlit.len() - 1]).unwrap_or_else(|e| {
                    self.log.add_error(format!("rule {}: cannot decode the string literal {strlit}: {e}", self.curr_name.as_ref().unwrap()));
                    format!("♫{strlit}♫") // we make up the result to leave a chance to the parser to continue
                });
                (tree.add(None, ReNode::string(&s)), Some(s))
            }
            CtxItem::V5 { char_set } => {         // item -> char_set
                (tree.add(None, ReNode::char_range(char_set.0)), None)
            }
            CtxItem::V6 { alt_items } => {       // item -> "(" alt_items ")"
                alt_items.0
            }
            CtxItem::V7 { item } => {            // item -> "~" item
                let node = tree.get_mut(item.0.0);
                if let ReType::CharRange(range) = node.get_mut_type() {
                    **range = range.not();
                } else {
                    self.log.add_error(format!("rule {}: ~ can only be applied to a char set, not to {}", self.curr_name.as_ref().unwrap(), node.get_type()));
                }
                (item.0.0, None)
            }
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynItem((id, const_literal))
    }

    fn exit_char_set(&mut self, ctx: CtxCharSet, _spans: Vec<PosSpan>) -> SynCharSet {
        // char_set:
        //     LSBRACKET (char_set_one)+ RSBRACKET
        // |   DOT
        // |   FIXED_SET;
        if self.verbose { print!("- exit_char_set({ctx:?})"); }
        let seg = match ctx {
            CtxCharSet::V1 { plus } => {              // char_set -> "[" char_set_one+ "]"
                plus.0.into_iter().map(|one| one.0).sum()
            }
            CtxCharSet::V2 => {                       // char_set -> "."
                Segments::dot()
            }
            CtxCharSet::V3 { fixedset } => {          // char_set -> FixedSet
                decode_fixed_set(&fixedset).unwrap_or_else(|e| {
                    self.log.add_error(format!("rule {}: cannot decode the character set [{fixedset}]: {e}", self.curr_name.as_ref().unwrap()));
                    segments!('♫') // we make up the result to leave a chance to the parser to continue
                })
            }
        };
        if self.verbose { println!(" -> {seg}"); }
        SynCharSet(seg)
    }

    fn exit_char_set_one(&mut self, ctx: CtxCharSetOne, _spans: Vec<PosSpan>) -> SynCharSetOne {
        // char_set_one:
        //     SET_CHAR MINUS SET_CHAR
        // |   SET_CHAR
        // |   FIXED_SET;
        if self.verbose { print!("- exit_char_set_one({ctx:?})"); }
        let seg = match ctx {
            CtxCharSetOne::V1 { setchar } => {     // char_set_one -> SetChar "-" SetChar
                let [first, last] = setchar.map(|sc| {
                    decode_set_char(&sc).unwrap_or_else(|e| {
                        self.log.add_error(format!("rule {}: cannot decode the character '{sc}': {e}", self.curr_name.as_ref().unwrap()));
                        '♫' // we make up the result to leave a chance to the parser to continue
                    })
                });
                Segments::from((first, last))
            }
            CtxCharSetOne::V2 { setchar } => {     // char_set_one -> SetChar
                let single = decode_set_char(&setchar).unwrap_or_else(|e| {
                    self.log.add_error(format!("rule {}: cannot decode the character '{setchar}': {e}", self.curr_name.as_ref().unwrap()));
                    '♫' // we make up the result to leave a chance to the parser to continue
                });
                Segments::from(single)
            }
            CtxCharSetOne::V3 { fixedset } => {    // char_set_one -> FixedSet
                decode_fixed_set(&fixedset).unwrap_or_else(|e| {
                    self.log.add_error(format!("rule {}: cannot decode the character set [{fixedset}]: {e}", self.curr_name.as_ref().unwrap()));
                    segments!('♫') // we make up the result to leave a chance to the parser to continue
                })
            }
        };
        if self.verbose { println!(" -> {seg}"); }
        SynCharSetOne(seg)
    }
}

/// Decodes a string literal (without its surrounding quotes). There must be at least two characters in `strlit`.
pub(crate) fn decode_str(strlit: &str) -> Result<String, String> {
    let mut result = String::new();
    let mut chars = strlit.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                result.push(match chars.next().ok_or(format!("'\\' incomplete escape code in string literal '{strlit}'"))? {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\'' => '\'',
                    '\\' => '\\',
                    'u' => {
                        if !matches!(chars.next(), Some('{')) { return Err(format!("malformed unicode literal in string literal '{strlit}' (missing '{{')")); }
                        let mut hex = String::new();
                        loop {
                            let Some(h) = chars.next() else { return Err(format!("malformed unicode literal in string literal '{strlit}' (missing '}}')")); };
                            if h == '}' { break; }
                            hex.push(h);
                        };
                        let code = u32::from_str_radix(&hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                        char::from_u32(code).ok_or_else(|| format!("'{hex}' isn't a valid unicode hexadecimal value"))?
                    }
                    unknown => return Err(format!("unknown escape code '\\{unknown}' in string literal '{strlit}'"))
                });
            }
            _ => result.push(c)
        }
    }
    Ok(result)
}

/// Decodes a single character literal (without its surrounding quotes). There must be exactly one character in `char`,
/// so this function assumes there's at least one byte but can handle malformed character literals.
fn decode_char(char: &str) -> Result<char, String> {
    // fragment Char        : EscChar | ~[\n\r\t'\\];
    // fragment EscChar     : '\\' ([nrt'\\] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    let mut chars = char.chars();
    let c = chars.next();
    if c == Some('\\') {
        match chars.next().ok_or("'\\' incomplete escape code in character literal".to_string())? {
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '\'' => Ok('\''),
            '\\' => Ok('\\'),
            'u' => {
                if !matches!(chars.next(), Some('{')) { return Err(format!("malformed unicode literal in string literal '{char}' (missing '{{')")); }
                let mut hex = String::new();
                loop {
                    let Some(h) = chars.next() else { return Err(format!("malformed unicode literal in string literal '{char}' (missing '}}')")); };
                    if h == '}' { break; }
                    hex.push(h);
                };
                let code = u32::from_str_radix(&hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                let u = char::from_u32(code).ok_or(format!("'{hex}' isn't a valid unicode hexadecimal value"))?;
                Ok(u)
            }
            _ => Err(format!("unknown escape code '{char}'")), // shouldn't happen
        }

    } else {
        c.ok_or(format!("'{char}' is not a valid character literal"))
    }
}

/// Decodes one character that is used inside `[` ... `]`. There must be exactly one character in `setchar`,
/// so this function assumes there's at least one byte but can handle malformed character literals.
fn decode_set_char(setchar: &str) -> Result<char, String> {
    // SET_CHAR             : (EscSetChar | ~[\n\r\t\\\]]);
    // fragment EscSetChar  : '\\' ([nrt\\[\]\-] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    let bytes = setchar.as_bytes();
    if bytes[0] == b'\\' {
        match bytes.get(1).ok_or("'\\' incomplete escape code in set character literal".to_string())? {
            b'n' => Ok('\n'),
            b'r' => Ok('\r'),
            b't' => Ok('\t'),
            b'[' => Ok('['),
            b']' => Ok(']'),
            b'-' => Ok('-'),
            b'\\' => Ok('\\'),
            b'u' => {
                if bytes[2] != b'{' || !matches!(setchar.chars().last(), Some('}')) {
                    return Err(format!("malformed unicode literal '{setchar}'"));
                }
                let hex = &setchar[3..setchar.len()];
                let code = u32::from_str_radix(hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                let u = char::from_u32(code).ok_or(format!("'{hex}' isn't a valid unicode hexadecimal value"))?;
                Ok(u)
            }
            _ => Err(format!("unknown escape code '{setchar}'")), // shouldn't happen
        }
    } else {
        setchar.chars().next().ok_or(format!("'{setchar}' is not a valid set character literal"))
    }
}

fn decode_fixed_set(fixedset: &str) -> Result<Segments, String> {
    // FIXED_SET       : ('\\w' | '\\d');
    let bytes = fixedset.as_bytes();
    if bytes[0] != b'\\' {
        Err(format!("unknown shorthand code '{fixedset}'")) // shouldn't happen
    } else {
        match fixedset.as_bytes()[1] {
            b'd' => Ok(segments!('0'-'9')),
            b'w' => Ok(segments!('0'-'9', '_', 'A'-'Z', 'a'-'z')),
            _ => Err(format!("unknown shorthand code '{fixedset}'")), // shouldn't happen
        }
    }
}

pub mod macros {
    #[macro_export]
    macro_rules! action {
        (= $id:expr) =>      { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::Token($id), channel: None,      mode: ModeOption::None,      pop: false, hook: false } };
        (more) =>            { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::More,       channel: None,      mode: ModeOption::None,      pop: false, hook: false } };
        (skip) =>            { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::Skip,       channel: None,      mode: ModeOption::None,      pop: false, hook: false } };
        (mode $id:expr) =>   { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::None,       channel: None,      mode: ModeOption::Mode($id), pop: false, hook: false } };
        (push $id:expr) =>   { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::None,       channel: None,      mode: ModeOption::Push($id), pop: false, hook: false } };
        (pop) =>             { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::None,       channel: None,      mode: ModeOption::None,      pop: true , hook: false } };
        (hook) =>            { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::None,       channel: None,      mode: ModeOption::None,      pop: false, hook: true  } };
        (# $id:expr) =>      { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::None,       channel: Some($id), mode: ModeOption::None,      pop: false, hook: false } };
        (nop) =>             { $crate::lexi::listener::LexAction { option: $crate::lexi::listener::LexActionOption::None,       channel: None,      mode: ModeOption::None,      pop: false, hook: false } };
    }
}
