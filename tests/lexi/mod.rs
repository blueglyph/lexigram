mod basic_test;
mod tests;

use rlexer::dfa::{ChannelId, ModeOption, ReType, ActionOption, Terminal, TokenId, ModeId, Dfa, DfaBuilder, tree_to_string};
use std::collections::HashMap;
use std::ops::{Add, Range};
use vectree::VecTree;
use rlexer::dfa::ReNode;
use rlexer::log::Logger;
use rlexer::{hashmap, node, segments, CollectJoin, General};
use rlexer::segments::Segments;
use crate::action;
use crate::out::lexiparser::lexiparser::*;
use crate::out::lexiparser::lexiparser_types::*;

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
enum RuleType {
    Fragment(TokenId),
    Terminal(TokenId)
}

struct LexiListener {
    verbose: bool,
    name: String,
    curr: Option<VecTree<ReNode>>,
    curr_mode: ModeId,
    rules: HashMap<String, RuleType>,
    /// VecTree of each fragment.
    fragments: Vec<VecTree<ReNode>>,
    /// VecTree of each terminal. Some may be empty: `type(token)` with no corresponding rule.
    terminals: Vec<VecTree<ReNode>>,
    channels: HashMap<String, ChannelId>,
    modes: HashMap<String, ModeId>,
    /// Range of terminals defined in `fragments` for each mode.
    mode_terminals: Vec<Range<TokenId>>,
    log: Logger,
}

impl LexiListener {
    fn new() -> Self {
        LexiListener {
            verbose: false,
            name: String::new(),
            curr: None,
            curr_mode: 0,
            rules: HashMap::new(),
            fragments: Vec::new(),
            terminals: Vec::new(),
            channels: hashmap!("DEFAULT_CHANNEL".to_string() => 0),
            modes: hashmap!("DEFAULT_MODE".to_string() => 0),
            mode_terminals: vec![0..0],
            log: Logger::new()
        }
    }

    fn get_sorted_modes(&self) -> Vec<(&ModeId, &String)> {
        let mut sorted_modes = self.modes.iter().map(|(name, id)| (id, name)).to_vec();
        sorted_modes.sort();
        sorted_modes
    }

    fn make_dfa(&mut self) -> Dfa<General> {
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
            assert!(dfa_builder.get_errors().is_empty(), "failed to compile mode {mode_id}");
            dfas.push((*mode_id as ModeId, dfa_builder.build()));
            if VERBOSE { rlexer::dfa::print_dfa(&dfas[dfas.len() - 1].1, 5); }
        }
        let mut dfa_builder = DfaBuilder::new();
        if VERBOSE { println!("merging dfa modes"); }
        let dfa = dfa_builder.build_from_dfa_modes(dfas).expect(&format!("failed to build lexer\n{}", dfa_builder.get_messages()));
        dfa
    }
}

impl LexiParserListener for LexiListener {
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
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
        let CtxOption::Option { id, mut star } = ctx;
        star.0.insert(0, id);
        for ch in star.0 {
            assert!(!self.channels.contains_key(&ch), "channel '{ch}' defined twice");
            self.channels.insert(ch, self.channels.len() as ChannelId);
        }
        SynOption()
    }

    fn exit_rule(&mut self, ctx: CtxRule) -> SynRule {
        let (id, rule_type, action_maybe) = match ctx {
            // FIXME: manage errors (may panic)
            CtxRule::Rule1 { id, .. } => {              // rule -> fragment Id : match ;
                let rule_id = TokenId::try_from(self.fragments.len()).expect(&format!("max {} fragments", TokenId::MAX));
                (id, RuleType::Fragment(rule_id), None)
            }
            CtxRule::Rule2 { id, actions, .. } => {     // rule -> Id : match -> actions ;
                let rule_id = TokenId::try_from(self.terminals.len()).expect(&format!("max {} rules", TokenId::MAX));
                (id, RuleType::Terminal(rule_id), Some(actions.0))
            }
            CtxRule::Rule3 { id, .. } => {              // rule -> Id : match ;
                let rule_id = TokenId::try_from(self.terminals.len()).expect(&format!("max {} rules", TokenId::MAX));
                (id, RuleType::Terminal(rule_id), None)
            }
        };
        if self.rules.contains_key(&id) {
            self.log.add_error(format!("Symbol '{id}' is already defined"));
        } else {
            let mut rule = self.curr.take().unwrap();
            match rule_type {
                RuleType::Fragment(_) => {
                    self.fragments.push(rule);
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
                    rule.add(Some(root), ReNode::end(terminal));
                    self.terminals.push(rule);
                    self.mode_terminals.get_mut(self.curr_mode as usize).unwrap().end += 1;
                }
            }
            self.rules.insert(id, rule_type);
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
                // returns the token if that rule exists, otherwise creates it
                let token = match self.rules.get(&id) {
                    None => {
                        let token = TokenId::try_from(self.terminals.len()).expect(&format!("max {} rules", TokenId::MAX));
                        self.rules.insert(id, RuleType::Terminal(token));
                        self.terminals.push(VecTree::new());
                        token
                    }
                    Some(rule) => {
                        if let RuleType::Terminal(token) = rule {
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
        assert!(self.curr.is_none());
        self.curr = Some(VecTree::new());
    }

    fn exit_match(&mut self, ctx: CtxMatch) -> SynMatch {
        // sets the tree root ID, so that any rule using `match` can expect to get a usable VecTree
        if self.verbose { println!("- exit_match({ctx:?})"); }
        let CtxMatch::Match { alt_items } = ctx;
        self.curr.as_mut().unwrap().set_root(alt_items.0);
        SynMatch()
    }

    fn exit_alt_items(&mut self, ctx: CtxAltItems) -> SynAltItems {
        if self.verbose { print!("- exit_alt_items({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let CtxAltItems::AltItems { alt_item, star } = ctx;
        let mut id = alt_item.0;
        if !star.0.is_empty() {
            id = tree.addci(None, node!(|), id);
            tree.attach_children(id, star.0.into_iter().map(|i| i.0))
        }
        if self.verbose { println!(" -> {}", tree_to_string(tree, Some(id), false)); }
        SynAltItems(id)
    }

    fn exit_alt_item(&mut self, ctx: CtxAltItem) -> SynAltItem {
        if self.verbose { print!("- exit_alt_item({ctx:?})"); }
        let CtxAltItem::AltItem { plus } = ctx;
        let tree = self.curr.as_mut().unwrap();
        let id = if plus.0.len() > 1 {
            tree.addci_iter(None, node!(&), plus.0.into_iter().map(|item| item.0))
        } else {
            plus.0[0].0
        };
        if self.verbose { println!(" -> {}", tree_to_string(tree, Some(id), false)); }
        SynAltItem(id)
    }

    fn exit_repeat_item(&mut self, ctx: CtxRepeatItem) -> SynRepeatItem {
        if self.verbose { print!("- exit_repeat_item({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let id = match ctx {
            CtxRepeatItem::RepeatItem1 { item } => {   // repeat_item -> item ?
                let empty = tree.add(None, node!(e));
                tree.addci_iter(None, node!(|), [item.0, empty])
            }
            CtxRepeatItem::RepeatItem2 { item } => {   // repeat_item -> item
                item.0
            }
            CtxRepeatItem::RepeatItem3 { item } => {   // repeat_item -> item +? (lazy)
                let plus = tree.addci(None, node!(+), item.0);
                tree.addci(None, node!(??), plus)
            }
            CtxRepeatItem::RepeatItem4 { item } => {   // repeat_item -> item +
                tree.addci(None, node!(+), item.0)
            }
            CtxRepeatItem::RepeatItem5 { item } => {   // repeat_item -> item *? (lazy)
                let star = tree.addci(None, node!(*), item.0);
                tree.addci(None, node!(??), star)
            }
            CtxRepeatItem::RepeatItem6 { item } => {   // repeat_item -> item *
                tree.addci(None, node!(*), item.0)
            }
        };
        if self.verbose { println!(" -> {}", tree_to_string(tree, Some(id), false)); }
        SynRepeatItem(id)
    }

    fn exit_item(&mut self, ctx: CtxItem) -> SynItem {
        // FIXME: manage errors (may panic)
        if self.verbose { print!("- exit_item({ctx:?})"); }
        let tree = self.curr.as_mut().unwrap();
        let id = match ctx {
            CtxItem::Item1 { alt_items } => {       // item -> ( alt_items )
                alt_items.0
            }
            CtxItem::Item2 { item } => {            // item -> ~ item
                let node = tree.get_mut(item.0);
                if let ReType::CharRange(range) = node.get_mut_type() {
                    *range = Box::new(range.not())
                } else {
                    panic!("~ can only be applied to a char set, not to '{node:?}'");
                }
                item.0
            }
            CtxItem::Item3 { id } => {              // item -> Id
                if let Some(RuleType::Fragment(f)) = self.rules.get(&id) {
                    let subtree = self.fragments.get(*f as usize).unwrap();
                    tree.add_from_tree(None, subtree, None)
                } else {
                    panic!("unknown fragment '{id}'")
                }
            }
            CtxItem::Item4 { strlit } => {          // item -> StrLit
                let s = &strlit[1..strlit.len() - 1];
                tree.add(None, ReNode::string(s))
            }
            CtxItem::Item5 { char_set } => {         // item -> char_set
                tree.add(None, ReNode::char_range(char_set.0))
            }
            CtxItem::Item6 { charlit } => {         // item -> CharLit .. CharLit
                let c1 = decode_char(&charlit[0][1..charlit[0].len() - 1]).unwrap_or_else(|s| panic!("{s}"));
                let c2 = decode_char(&charlit[1][1..charlit[1].len() - 1]).unwrap_or_else(|s| panic!("{s}"));
                tree.add(None, ReNode::char_range(Segments::from((c1, c2))))
            }
            CtxItem::Item7 { charlit } => {         // item -> CharLit
                // charlit is always sourrounded by quotes:
                // fragment CharLiteral	: '\'' Char '\'';
                let c = decode_char(&charlit[1..charlit.len() - 1]).unwrap_or_else(|s| panic!("{s}"));
                tree.add(None, ReNode::char_range(Segments::from(c)))
            }
        };
        if self.verbose { println!(" -> {}", tree_to_string(tree, Some(id), false)); }
        SynItem(id)
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
        (= $id:expr) =>      { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::Token($id), channel: None,      mode: ModeOption::None,      pop: false } };
        (more) =>            { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::More,       channel: None,      mode: ModeOption::None,      pop: false } };
        (skip) =>            { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::Skip,       channel: None,      mode: ModeOption::None,      pop: false } };
        (mode $id:expr) =>   { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::None,       channel: None,      mode: ModeOption::Mode($id), pop: false } };
        (push $id:expr) =>   { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::None,       channel: None,      mode: ModeOption::Push($id), pop: false } };
        (pop) =>             { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::None,       channel: None,      mode: ModeOption::None,      pop: true  } };
        (# $id:expr) =>      { $crate::lexi::LexAction { option: $crate::lexi::LexActionOption::None,       channel: Some($id), mode: ModeOption::None,      pop: false } };
    }
}
