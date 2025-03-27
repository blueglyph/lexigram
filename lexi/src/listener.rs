// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use lexigram::dfa::{ChannelId, ModeOption, ReType, ActionOption, Terminal, TokenId, ModeId, Dfa, DfaBuilder, tree_to_string};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Range};
use iter_index::IndexerIterator;
use vectree::VecTree;
use lexigram::dfa::ReNode;
use lexigram::log::{BufLog, Logger};
use lexigram::{hashmap, node, segments, CollectJoin, General};
use lexigram::segments::Segments;
use lexigram::symbol_table::SymbolTable;
use crate::action;
use crate::lexiparser::lexiparser::*;
use crate::lexiparser::lexiparser_types::*;


#[derive(Clone, Copy, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub enum LexActionOption {
    #[default]
    None,
    Skip,
    Token(TokenId),
    More
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
pub struct LexAction {
    pub option: LexActionOption,
    pub channel: Option<ChannelId>,
    pub mode: ModeOption,
    pub pop: bool
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

    pub fn try_add(self, rhs: LexAction) -> Result<LexAction, String> {
        if (self.option != LexActionOption::None && rhs.option != LexActionOption::None) ||
            (self.channel.is_some() && rhs.channel.is_some()) ||
            (!self.mode.is_none() && !rhs.mode.is_none())
        {
            return Err(format!("can't add {self:?} and {rhs:?}"))
        }
        Ok(LexAction {
            option: if self.option == LexActionOption::None { rhs.option } else { self.option },
            channel: self.channel.or_else(|| rhs.channel),
            mode: if !self.mode.is_none() { self.mode } else { rhs.mode },
            pop: self.pop || rhs.pop,
        })
    }
}

impl Add for LexAction {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self.try_add(rhs) {
            Ok(a) => a,
            Err(s) => panic!("{s}")
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
    pub(crate) name: String,
    curr: Option<VecTree<ReNode>>,
    curr_mode: ModeId,
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
    /// true if the corresponding terminal is returned by the lexer (either directly by a rule or as the target of a `type(T)`)
    terminal_ret: Vec<bool>,
    /// future tokens reserved by `type(T)`. The value is the first token referring to it with `type(T)`
    terminal_reserved: HashMap<String, TokenId>, // FIXME: change to HashSet
    /// terminal token IDs that need to be remapped (from key to value)
    terminal_remap: HashMap<TokenId, TokenId>,
    channels: HashMap<String, ChannelId>,
    modes: HashMap<String, ModeId>,
    /// Range of terminals defined in `fragments` for each mode.
    mode_terminals: Vec<Range<TokenId>>,
    pub(crate) log: BufLog,
}

impl LexiListener {
    pub fn new() -> Self {
        LexiListener {
            verbose: false,
            name: String::new(),
            curr: None,
            curr_mode: 0,
            rules: HashMap::new(),
            fragments: Vec::new(),
            fragment_literals: Vec::new(),
            terminals: Vec::new(),
            terminal_literals: Vec::new(),
            terminal_ret: Vec::new(),
            terminal_reserved: HashMap::new(),
            terminal_remap: HashMap::new(),
            channels: hashmap!("DEFAULT_CHANNEL".to_string() => 0),
            modes: hashmap!("DEFAULT_MODE".to_string() => 0),
            mode_terminals: vec![0..0],
            log: BufLog::new()
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn get_log(&self) -> &BufLog {
        &self.log
    }

    pub fn get_sorted_modes(&self) -> Vec<(&ModeId, &String)> {
        let mut sorted_modes = self.modes.iter().map(|(name, id)| (id, name)).to_vec();
        sorted_modes.sort();
        sorted_modes
    }

    pub fn build_symbol_table(&self) -> SymbolTable {
        let mut table = SymbolTable::new();
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
        table
    }

    pub fn make_dfa(&mut self) -> Dfa<General> {
        const VERBOSE: bool = false;
        let num_t = self.terminals.len();
        let mut names = vec![String::new(); num_t];
        for (s, r) in &self.rules {
            if let RuleType::Terminal(token) = r {
                names[*token as usize] = s.clone();
            }
        }
        let mut dfas = vec![];
        let sorted_modes = self.get_sorted_modes();
        for (mode_id, mode_name) in sorted_modes {
            if VERBOSE { println!("mode {mode_id}: {mode_name}"); }
            let range = &self.mode_terminals[*mode_id as usize];
            if VERBOSE { println!("* mode {mode_id}: {mode_name} = rules {range:?} ({})", range.clone().map(|n| &names[n as usize]).join(", ")); }
            let range = range.start as usize..range.end as usize;
            let mut tree = VecTree::<ReNode>::new();
            let top = tree.add_root(node!(|));
            for (i, t) in self.terminals[range].iter().enumerate() {
                if VERBOSE { println!("- adding tree for terminal {i}: {t:?}"); }
                if !t.is_empty() {
                    let cc = tree.add(Some(top), node!(&));
                    tree.add_from_tree(Some(cc), t, None);
                }
            }
            if VERBOSE { println!("  => {}", tree_to_string(&tree, None, true)); }
            let mut dfa_builder = DfaBuilder::from_re(tree);
            assert_eq!(dfa_builder.num_errors(), 0, "failed to compile mode {mode_id}");
            dfas.push((*mode_id as ModeId, dfa_builder.build()));
            if VERBOSE { lexigram::dfa::print_dfa(&dfas[dfas.len() - 1].1, 5); }
        }
        let mut dfa_builder = DfaBuilder::new();
        if VERBOSE { println!("merging dfa modes"); }
        let dfa = dfa_builder.build_from_dfa_modes(dfas).expect(&format!("failed to build lexer\n{}", dfa_builder.get_messages()));
        dfa
    }

    pub fn rules_to_string(&self, indent: usize) -> String {
        let mut cols = vec![vec!["      type".to_string(), "name".to_string(), "tree".to_string(), "lit".to_string(),
                                 "ret".to_string(), "token".to_string(), "end".to_string()]];
        let mut rules = self.rules.iter().to_vec();
        rules.sort_by(|a, b| (&a.1, &a.0).cmp(&(&b.1, &b.0)));
        // rules.sort_by_key(|(s, rt)| (rt, s));
        for (i, (s, rt)) in rules.into_iter().enumerate() {
            let (t, lit, ret, sym_maybe, end_maybe) = match rt {
                RuleType::Fragment(id) => (
                    self.fragments.get(*id as usize).unwrap(),
                    self.fragment_literals.get(*id as usize).unwrap(),
                    None,
                    None,
                    None
                ),
                RuleType::Terminal(id) => {
                    let ret = *self.terminal_ret.get(*id as usize).expect(&format!("no item {id}"));
                    (
                        self.terminals.get(*id as usize).expect(&format!("no item {id}")),
                        self.terminal_literals.get(*id as usize).expect(&format!("no item {id}")),
                        Some(ret),
                        if ret { Some(self.terminal_remap.get(&(*id as TokenId)).unwrap_or(id)) } else { None },
                        self.terminals[*id as usize].iter_depth_simple().find_map(|n|
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
            cols.push(vec![format!("[{i:3}] {rt:?}"),
                           format!("{s}"),
                           format!("{}", tree_to_string(t, None, true)),
                           format!("{lit:?}"),
                           format!("{}", if let Some(b) = ret { b.to_string() } else { String::new() }),
                           if let Some(sym) = sym_maybe { format!("{s} = {sym}") } else { String::new() },
                           if let Some(end) = end_maybe { format!(" {end} ") } else { String::new() },
            ]);
        }
        let mut cols_out = lexigram::columns_to_str(cols, None);
        let title = cols_out.remove(0);
        let tab = format!("{: <width$}", "", width = indent);
        format!("{tab}   {title}\n{tab}{:-<width$}{}", "",
                 cols_out.into_iter().map(|l| format!("\n{tab} - {l}")).join(""),
                 width=title.len() + 5
        )
    }
}

impl Debug for LexiListener {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "LexiListener {{")?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  curr = {}", self.curr.as_ref().map(|t| tree_to_string(t, None, true)).unwrap_or(String::new()))?;
        writeln!(f, "  curr_mode = {}", self.curr_mode)?;
        writeln!(f, "  rules: {}\n{}", self.rules.len(), self.rules_to_string(4))?;
        writeln!(f, "  fragments: {}", self.fragments.len())?;
        writeln!(f, "  fragment_literals: {}", self.fragment_literals.len())?;
        writeln!(f, "  terminals: {}", self.terminals.len())?;
        writeln!(f, "  terminal_literals: {}", self.terminal_literals.len())?;
        writeln!(f, "  terminal_ret: {}", self.terminal_ret.len())?;
        writeln!(f, "  terminal_reserved: {}", self.terminal_reserved.iter().map(|(s, _)| s.to_string()).join(", "))?;
        let mut bremap = BTreeMap::<TokenId, TokenId>::new();
        bremap.extend(self.terminal_remap.iter().map(|(a, b)| (*a, *b)));
        let s = bremap.iter().map(|(a, b)| format!("{a} -> {b}")).join(", ");
        writeln!(f, "  terminal_remap: {s}")?;
        writeln!(f, "  log:{}", self.log.get_messages().map(|s| format!("\n    - {s:?}")).join(""))?;
        writeln!(f, "}}")
    }
}

impl LexiParserListener for LexiListener {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        if self.verbose {
            println!("- exit_file({_ctx:?})");
            println!("terminal_reserved: {:?}", self.terminal_reserved);
        }
        for (id, _first_id) in &self.terminal_reserved {
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
            for mut node in tree.iter_depth_simple_mut() {
                let x: &mut ReType = node.get_mut_type();
                if let ReType::End(ref mut term) = x {
                    if let ActionOption::Token(old_id) = term.action {
                        if let Some(new_id) = remap.get(&old_id) {
                            term.action = ActionOption::Token(*new_id);
                        }
                    }
                }
            }
        }
        self.terminal_remap = remap;

        // TODO: remap the modes...
        if self.verbose {
            println!("\nModes: ");
            for (mode_id, mode) in self.get_sorted_modes() {
                println!("- {mode_id} - {mode}: terminals {:?}", self.mode_terminals[*mode_id as usize]);
            }
        }
        SynFile()
    }

    fn exit_file_item(&mut self, _ctx: CtxFileItem) -> SynFileItem {
        SynFileItem()
    }

    fn exit_header(&mut self, ctx: CtxHeader) -> SynHeader {
        let CtxHeader::Header { id } = ctx;
        self.name = id;
        SynHeader()
    }

    fn exit_declaration(&mut self, ctx: CtxDeclaration) -> SynDeclaration {
        // FIXME: manage errors (may panic)
        if self.verbose { print!("- exit_declaration({ctx:?}), modes: {:?}", self.modes); }
        let CtxDeclaration::Declaration { id: mode_name } = ctx;    // declaration -> mode Id ;
        let n = self.modes.len();
        let mode_id = *self.modes.entry(mode_name.clone()).or_insert_with(||
            ModeId::try_from(n).expect(&format!("max {} modes", ModeId::MAX)));
        self.curr_mode = mode_id;
        if self.verbose { println!(" -> mode {mode_id}, mode_terminals: {:?}", self.mode_terminals)}
        let next_token = TokenId::try_from(self.terminals.len())
            .expect(&format!("no room left for any terminal in mode {mode_name}, max {} allowed", TokenId::MAX));
        self.mode_terminals.insert(mode_id as usize, next_token..next_token);
        SynDeclaration()
    }

    fn exit_option(&mut self, ctx: CtxOption) -> SynOption {
        // FIXME: manage errors (may panic)
        if self.verbose { println!("- exit_option({ctx:?})"); }
        let CtxOption::Option { id, mut star } = ctx;
        star.0.insert(0, id);
        for ch in star.0 {
            assert!(!self.channels.contains_key(&ch), "channel '{ch}' defined twice");
            self.channels.insert(ch, self.channels.len() as ChannelId);
        }
        SynOption()
    }

    fn exit_rule(&mut self, ctx: CtxRule) -> SynRule {
        if self.verbose { println!("- exit_rule({ctx:?})"); }
        let (id, rule_type, mut action_maybe, const_literal) = match ctx {
            // FIXME: manage errors (may panic)
            CtxRule::Rule1 { id, match1 } => {              // rule -> fragment Id : match ;
                let rule_id = TokenId::try_from(self.fragments.len()).expect(&format!("max {} fragments", TokenId::MAX));
                (id, RuleType::Fragment(rule_id), None, match1.0)
            }
            CtxRule::Rule2 { id, match1, actions } => {     // rule -> Id : match -> actions ;
                let rule_id = TokenId::try_from(self.terminals.len()).expect(&format!("max {} rules", TokenId::MAX));
                (id, RuleType::Terminal(rule_id), Some(actions.0), match1.0)
            }
            CtxRule::Rule3 { id, match1 } => {              // rule -> Id : match ;
                let rule_id = TokenId::try_from(self.terminals.len()).expect(&format!("max {} rules", TokenId::MAX));
                (id, RuleType::Terminal(rule_id), None, match1.0)
            }
        };
        let was_reserved = if let Some(RuleType::Terminal(reserved_id)) = self.rules.get_mut(&id) {
            // checks that it's indeed reserved and not a simple conflict with another terminal name
            if let Some(first_ref_id) = self.terminal_reserved.get(&id) {
                // reserved_id is a temporary, reserved TokenId introduced by terminal `first_ref_id` with `-> type(id)`
                assert!(self.terminals.len() <= *reserved_id as usize,
                        "collision between token IDs and reserved IDs: {} > {reserved_id} (for {id})", self.terminals.len());
                let RuleType::Terminal(rule_id) = rule_type else { panic!() };
                if self.verbose { println!("terminal {id} was reserved by {first_ref_id} as {reserved_id}; will now be {rule_id}"); }
                if *first_ref_id == rule_id {
                    // type(T) pointing at itself
                    if self.verbose { println!("    (so it was actually pointing at itself)"); }
                    let Some(ref mut action) = action_maybe else { panic!() };
                    action.option = LexActionOption::Token(rule_id)
                }
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
            self.log.add_error(format!("Symbol '{id}' is already defined"));
            self.curr = None;
        } else {
            let mut rule = self.curr.take().unwrap();
            match rule_type {
                RuleType::Fragment(_) => {
                    self.fragments.push(rule);
                    self.fragment_literals.push(const_literal);
                }
                RuleType::Terminal(rule_id) => {
                    // if no action, only return
                    let rule_action = action_maybe.unwrap_or(action!(= rule_id));
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
                    self.terminal_literals.push(const_literal);
                    self.terminal_ret.push(ret);
                    self.mode_terminals.get_mut(self.curr_mode as usize).unwrap().end += 1;
                }
            }
            if !was_reserved {
                self.rules.insert(id, rule_type);
            }
        }
        SynRule()
    }

    fn exit_actions(&mut self, ctx: CtxActions) -> SynActions {
        let CtxActions::Actions { action, star } = ctx;
        let mut action = action.0;
        for a in star.0 {
            action = action + a.0; // FIXME: manage errors (may panic)
        }
        SynActions(action)
    }

    fn exit_action(&mut self, ctx: CtxAction) -> SynAction {
        let action = match ctx {
            CtxAction::Action1 { id } => {              // action -> mode ( Id )
                let n = self.modes.len();
                let id_val = *self.modes.entry(id).or_insert_with(|| ModeId::try_from(n).expect(&format!("max {} modes", ModeId::MAX)));
                action!(mode id_val)
            }
            CtxAction::Action2 { id } => {              // action -> push ( Id )
                let n = self.modes.len();
                let id_val = *self.modes.entry(id).or_insert_with(|| ModeId::try_from(n).expect(&format!("max {} modes", ModeId::MAX)));
                action!(push id_val)
            },
            CtxAction::Action3 => action!(pop),         // action -> pop
            CtxAction::Action4 => action!(skip),        // action -> skip
            CtxAction::Action5 => action!(more),        // action -> more
            CtxAction::Action6 { id } => {              // action -> type ( Id )
                // returns the token if that rule exists, otherwise reserves it; the action will have to be changed when we know the ID
                // we won't copy the terminal_literal to Id because
                // - there could be several rules with different literals
                // - most of the time, this wouldn't make sense and could even be misleading
                let token = match self.rules.get(&id) {
                    None => {
                        // TODO: we'll have to check at some point that there were no collisions between IDs and reserved IDs,
                        //       in other words, `assert!(self.terminals.len() <= TokenId::MAX as usize - self.terminal_reserved.len())`
                        assert!(self.terminal_reserved.len() < TokenId::MAX as usize, "max {} reserved tokens", TokenId::MAX);
                        let reserved_token = TokenId::MAX - self.terminal_reserved.len() as TokenId;
                        let token = TokenId::try_from(self.terminals.len()).expect(&format!("max {} rules", TokenId::MAX));
                        self.terminal_reserved.insert(id.clone(), token);           // first reference (this terminal rule)
                        if self.verbose { println!("-> type({id}) : reserved ID {reserved_token}"); }
                        self.rules.insert(id, RuleType::Terminal(reserved_token));  // temporary token ID
                        reserved_token
                    }
                    Some(rule) => {
                        if let RuleType::Terminal(token) = rule {
                            self.terminal_ret[*token as usize] = true;
                            *token as TokenId
                        } else {
                            panic!("{id} is not a terminal; it's a fragment")   // FIXME: manage errors (may panic)
                        }
                    }
                };
                action!(= token)
            }
            CtxAction::Action7 { id } => {              // action -> channel ( Id )
                // we don't allow to define new channels here on the fly because typos would induce annoying errors
                let channel = *self.channels.get(&id).unwrap(); // FIXME: manage errors (may panic)
                action!(# channel)
            }
        };
        SynAction(action)
    }

    fn init_match(&mut self) {
        assert!(self.curr.is_none(), "remnant tree in self.curr:\n{self:?}");
        self.curr = Some(VecTree::new());
    }

    fn exit_match(&mut self, ctx: CtxMatch) -> SynMatch {
        // sets the tree root ID, so that any rule using `match` can expect to get a usable VecTree
        if self.verbose { println!("- exit_match({ctx:?})"); }
        let CtxMatch::Match { alt_items } = ctx;
        self.curr.as_mut().unwrap().set_root(alt_items.0.0);
        SynMatch(alt_items.0.1)
    }

    fn exit_alt_items(&mut self, ctx: CtxAltItems) -> SynAltItems {
        if self.verbose { print!("- exit_alt_items({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let CtxAltItems::AltItems { alt_item, star } = ctx;
        let (id, const_literal) = if !star.0.is_empty() {
            let id = tree.addci(None, node!(|), alt_item.0.0);
            tree.attach_children(id, star.0.into_iter().map(|i| i.0.0));
            (id, None)
        } else {
            alt_item.0
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynAltItems((id, const_literal))
    }

    fn exit_alt_item(&mut self, ctx: CtxAltItem) -> SynAltItem {
        if self.verbose { print!("- exit_alt_item({ctx:?})"); }
        let CtxAltItem::AltItem { plus: SynAltItem1(mut items) } = ctx;
        let tree = self.curr.as_mut().unwrap();
        let (id, const_literal) = if items.len() > 1 {
            let lit_maybe = items.iter().fold(Some(String::new()), |acc, next| if let Some(n) = &next.0.1 { acc.map(|a| a + n) } else { None });
            (tree.addci_iter(None, node!(&), items.into_iter().map(|item| item.0.0)), lit_maybe)
        } else {
            items.pop().unwrap().0
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynAltItem((id, const_literal))
    }

    fn exit_repeat_item(&mut self, ctx: CtxRepeatItem) -> SynRepeatItem {
        if self.verbose { print!("- exit_repeat_item({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let (id, const_literal) = match ctx {
            CtxRepeatItem::RepeatItem1 { item } => {   // repeat_item -> item ?
                let empty = tree.add(None, node!(e));
                (tree.addci_iter(None, node!(|), [item.0.0, empty]), None)
            }
            CtxRepeatItem::RepeatItem2 { item } => {   // repeat_item -> item
                item.0
            }
            CtxRepeatItem::RepeatItem3 { item } => {   // repeat_item -> item +? (lazy)
                let plus = tree.addci(None, node!(+), item.0.0);
                (tree.addci(None, node!(??), plus), None)
            }
            CtxRepeatItem::RepeatItem4 { item } => {   // repeat_item -> item +
                (tree.addci(None, node!(+), item.0.0), None)
            }
            CtxRepeatItem::RepeatItem5 { item } => {   // repeat_item -> item *? (lazy)
                let star = tree.addci(None, node!(*), item.0.0);
                (tree.addci(None, node!(??), star), None)
            }
            CtxRepeatItem::RepeatItem6 { item } => {   // repeat_item -> item *
                (tree.addci(None, node!(*), item.0.0), None)
            }
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynRepeatItem((id, const_literal))
    }

    fn exit_item(&mut self, ctx: CtxItem) -> SynItem {
        // FIXME: manage errors (may panic)
        if self.verbose { print!("- exit_item({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let (id, const_literal) = match ctx {
            CtxItem::Item1 { alt_items } => {       // item -> ( alt_items )
                alt_items.0
            }
            CtxItem::Item2 { item } => {            // item -> ~ item
                let node = tree.get_mut(item.0.0);
                if let ReType::CharRange(range) = node.get_mut_type() {
                    *range = Box::new(range.not())
                } else {
                    panic!("~ can only be applied to a char set, not to '{node:?}'");
                }
                (item.0.0, None)
            }
            CtxItem::Item3 { id } => {              // item -> Id
                if let Some(RuleType::Fragment(f)) = self.rules.get(&id) {
                    let subtree = self.fragments.get(*f as usize).unwrap();
                    let const_literal = self.fragment_literals.get(*f as usize).unwrap().clone();
                    (tree.add_from_tree(None, subtree, None), const_literal)
                } else {
                    panic!("unknown fragment '{id}'")
                }
            }
            CtxItem::Item4 { strlit } => {          // item -> StrLit
                let s = &strlit[1..strlit.len() - 1];
                (tree.add(None, ReNode::string(s)), Some(s.to_string()))
            }
            CtxItem::Item5 { char_set } => {         // item -> char_set
                (tree.add(None, ReNode::char_range(char_set.0)), None)
            }
            CtxItem::Item6 { charlit } => {         // item -> CharLit .. CharLit
                let c1 = decode_char(&charlit[0][1..charlit[0].len() - 1]).unwrap_or_else(|s| panic!("{s}"));
                let c2 = decode_char(&charlit[1][1..charlit[1].len() - 1]).unwrap_or_else(|s| panic!("{s}"));
                (tree.add(None, ReNode::char_range(Segments::from((c1, c2)))), None)
            }
            CtxItem::Item7 { charlit } => {         // item -> CharLit
                // charlit is always sourrounded by quotes:
                // fragment CharLiteral	: '\'' Char '\'';
                let c = decode_char(&charlit[1..charlit.len() - 1]).unwrap_or_else(|s| panic!("{s}"));
                (tree.add(None, ReNode::char_range(Segments::from(c))), Some(c.to_string()))
            }
        };
        if self.verbose { println!(" -> {}, {:?}", tree_to_string(tree, Some(id), false), const_literal); }
        SynItem((id, const_literal))
    }

    fn exit_char_set(&mut self, ctx: CtxCharSet) -> SynCharSet {
        // char_set:
        //     LSBRACKET (char_set_one)+ RSBRACKET
        // |   DOT
        // |   FIXED_SET;
        if self.verbose { print!("- exit_char_set({ctx:?})"); }
        let seg = match ctx {
            CtxCharSet::CharSet1 { plus } => {              // char_set -> [ [char_set_one]+ ]
                plus.0.into_iter().map(|one| one.0).sum()
            }
            CtxCharSet::CharSet2 => {                       // char_set -> .
                Segments::dot()
            }
            CtxCharSet::CharSet3 { fixedset } => {          // char_set -> FixedSet
                decode_fixed_set(&fixedset).unwrap_or_else(|s| panic!("{s}"))
            }
        };
        if self.verbose { println!(" -> {seg}"); }
        SynCharSet(seg)
    }

    fn exit_char_set_one(&mut self, ctx: CtxCharSetOne) -> SynCharSetOne {
        // FIXME: manage errors (may panic)
        // char_set_one:
        //     SET_CHAR MINUS SET_CHAR
        // |   SET_CHAR
        // |   FIXED_SET;
        if self.verbose { print!("- exit_char_set_one({ctx:?})"); }
        let seg = match ctx {
            CtxCharSetOne::CharSetOne1 { fixedset } => {    // char_set_one -> FixedSet
                decode_fixed_set(&fixedset).unwrap_or_else(|s| panic!("{s}"))
            }
            CtxCharSetOne::CharSetOne2 { setchar } => {     // char_set_one -> SetChar - SetChar
                let first = decode_set_char(&setchar[0]).unwrap_or_else(|s| panic!("{s}"));
                let last = decode_set_char(&setchar[1]).unwrap_or_else(|s| panic!("{s}"));
                Segments::from((first, last))
            }
            CtxCharSetOne::CharSetOne3 { setchar } => {     // char_set_one -> SetChar
                let single = decode_set_char(&setchar).unwrap_or_else(|s| panic!("{s}"));
                Segments::from(single)
            }
        };
        if self.verbose { println!(" -> {seg}"); }
        SynCharSetOne(seg)
    }
}

fn decode_char(char: &str) -> Result<char, String> {
    // fragment Char        : EscChar | ~[\n\r\t'\\];
    // fragment EscChar     : '\\' ([nrt'\\] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    let bytes = char.as_bytes();
    if bytes[0] == b'\\' {
        match bytes[1] {
            b'n' => Ok('\n'),
            b'r' => Ok('\r'),
            b't' => Ok('\t'),
            b'\'' => Ok('\''),
            b'\\' => Ok('\\'),
            b'u' => {
                let hex = &char[3..char.len()];
                let code = u32::from_str_radix(hex, 16).map_err(|_| format!("{hex} isn't a valid hexadecimal value"))?;
                let u = char::from_u32(code).ok_or(format!("{hex} isn't a valid unicode hexadecimal value"))?;
                Ok(u)
            }
            _ => Err(format!("unknown escape code '{char}'")), // shouldn't happen
        }

    } else {
        Ok(char.chars().next().unwrap())
    }
}

fn decode_set_char(setchar: &str) -> Result<char, String> {
    // SET_CHAR             : (EscSetChar | ~[\n\r\t\\\]]);
    // fragment EscSetChar  : '\\' ([nrt\\[\]\-] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    let bytes = setchar.as_bytes();
    if bytes[0] == b'\\' {
        match bytes[1] {
            b'n' => Ok('\n'),
            b'r' => Ok('\r'),
            b't' => Ok('\t'),
            b'[' => Ok('['),
            b']' => Ok(']'),
            b'-' => Ok('-'),
            b'\\' => Ok('\\'),
            b'u' => {
                let hex = &setchar[3..setchar.len()];
                let code = u32::from_str_radix(hex, 16).map_err(|_| format!("{hex} isn't a valid hexadecimal value"))?;
                let u = char::from_u32(code).ok_or(format!("{hex} isn't a valid unicode hexadecimal value"))?;
                Ok(u)
            }
            _ => Err(format!("unknown escape code '{setchar}'")), // shouldn't happen
        }
    } else {
        Ok(setchar.chars().next().unwrap())
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
    #[macro_export(local_inner_macros)]
    macro_rules! action {
        (= $id:expr) =>      { $crate::listener::LexAction { option: crate::listener::LexActionOption::Token($id), channel: None,      mode: ModeOption::None,      pop: false } };
        (more) =>            { $crate::listener::LexAction { option: crate::listener::LexActionOption::More,       channel: None,      mode: ModeOption::None,      pop: false } };
        (skip) =>            { $crate::listener::LexAction { option: crate::listener::LexActionOption::Skip,       channel: None,      mode: ModeOption::None,      pop: false } };
        (mode $id:expr) =>   { $crate::listener::LexAction { option: crate::listener::LexActionOption::None,       channel: None,      mode: ModeOption::Mode($id), pop: false } };
        (push $id:expr) =>   { $crate::listener::LexAction { option: crate::listener::LexActionOption::None,       channel: None,      mode: ModeOption::Push($id), pop: false } };
        (pop) =>             { $crate::listener::LexAction { option: crate::listener::LexActionOption::None,       channel: None,      mode: ModeOption::None,      pop: true  } };
        (# $id:expr) =>      { $crate::listener::LexAction { option: crate::listener::LexActionOption::None,       channel: Some($id), mode: ModeOption::None,      pop: false } };
    }
}
