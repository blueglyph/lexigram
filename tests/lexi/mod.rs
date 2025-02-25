mod basic_test;

use rlexer::dfa::{ChannelId, ModeOption, ReType, ActionOption, Terminal, TokenId, ModeId};
use std::collections::HashMap;
use std::ops::{Add, Range};
use vectree::VecTree;
use rlexer::dfa::ReNode;
use rlexer::log::Logger;
use rlexer::{hashmap, node};
use rlexer::segments::{Seg, Segments};
use crate::action;
use crate::gen::lexiparser::lexiparser::*;
use crate::gen::lexiparser::lexiparser_types::*;

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

#[derive(Debug)]
enum RuleType {
    Terminal(TokenId),
    Fragment(TokenId)
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
}

impl LexiParserListener for LexiListener {
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        todo!()
    }

    fn exit_file_item(&mut self, _ctx: CtxFileItem) -> SynFileItem {
        todo!()
    }

    fn exit_header(&mut self, ctx: CtxHeader) -> SynHeader {
        let CtxHeader::Header { id } = ctx;
        self.name = id;
        SynHeader()
    }

    fn exit_declaration(&mut self, ctx: CtxDeclaration) -> SynDeclaration {
        // FIXME: manage errors (may panic)
        let CtxDeclaration::Declaration { id: mode_name } = ctx;    // declaration -> mode Id ;
        let n = self.modes.len();
        let mode_id = *self.modes.entry(mode_name.clone()).or_insert_with(||
            ModeId::try_from(n).expect(&format!("max {} modes", ModeId::MAX)));
        self.curr_mode = mode_id;
        let next_token = TokenId::try_from(self.terminals.len())
            .expect(&format!("no room left for any terminal in mode {mode_name}, max {} allowed", TokenId::MAX));
        *self.mode_terminals.get_mut(mode_id as usize).unwrap() = next_token..next_token;
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

    fn init_rule(&mut self) {
        assert!(self.curr.is_none());
        self.curr = Some(VecTree::new());
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

    fn exit_match(&mut self, ctx: CtxMatch) -> SynMatch {
        let CtxMatch::Match { alt_items } = ctx;

        SynMatch()
    }

    fn exit_alt_items(&mut self, ctx: CtxAltItems) -> SynAltItems {
        match ctx {
            CtxAltItems::AltItems1 { alt_item } => {                // alt_items -> alt_item
            }
            CtxAltItems::AltItems2 { alt_items, alt_item } => {     // alt_items -> alt_items | alt_item
            }
            CtxAltItems::AltItems3 { alt_items } => {               // end of iterations in alt_items -> alt_items | alt_item
            }
        }
        SynAltItems()
    }

    fn exit_alt_item(&mut self, ctx: CtxAltItem) -> SynAltItem {
        let CtxAltItem::AltItem { plus } = ctx;

        SynAltItem()
    }

    fn exit_repeat_item(&mut self, ctx: CtxRepeatItem) -> SynRepeatItem {
        match ctx {
            CtxRepeatItem::RepeatItem1 { repeat_item } => {
                // end of iterations in repeat_item -> repeat_item + ? | repeat_item + | repeat_item * ? | repeat_item *
            }
            CtxRepeatItem::RepeatItem2 { item } => {            // repeat_item -> item ?
            }
            CtxRepeatItem::RepeatItem3 { item } => {            // repeat_item -> item
            }
            CtxRepeatItem::RepeatItem4 { repeat_item } => {     // repeat_item -> repeat_item + ?
            }
            CtxRepeatItem::RepeatItem5 { repeat_item } => {     // repeat_item -> repeat_item +
            }
            CtxRepeatItem::RepeatItem6 { repeat_item } => {     // repeat_item -> repeat_item * ?
            }
            CtxRepeatItem::RepeatItem7 { repeat_item } => {     // repeat_item -> repeat_item *
            }
        }

        SynRepeatItem()
    }

    fn exit_item(&mut self, ctx: CtxItem) -> SynItem {
        // FIXME: manage errors (may panic)
        let tree = self.curr.as_mut().unwrap();
        match ctx {
            CtxItem::Item1 { alt_items } => {       // item -> ( alt_items )
                SynItem(todo!())
            }
            CtxItem::Item2 { item } => {            // item -> ~ item
                SynItem(todo!())
            }
            CtxItem::Item3 { id } => {              // item -> Id
                if let Some(RuleType::Fragment(f)) = self.rules.get(&id) {
                    let subtree = self.fragments.get(*f as usize).unwrap();
                    SynItem(tree.add_from_tree(None, subtree, None))
                } else {
                    panic!("unknown fragment '{id}'")
                }
            }
            CtxItem::Item4 { charset } => {         // item -> CharSet
                SynItem(todo!())
            }
            CtxItem::Item5 { strlit } => {          // item -> StrLit
                let s = &strlit[1..strlit.len()];
                SynItem(tree.add(None, ReNode::string(s)))
            }
            CtxItem::Item6 { charlit } => {         // item -> CharLit .. CharLit
                let c1 = decode_char(&charlit[0][1..charlit[0].len() - 2]).unwrap_or_else(|s| panic!("{s}"));
                let c2 = decode_char(&charlit[1][1..charlit[1].len() - 2]).unwrap_or_else(|s| panic!("{s}"));
                SynItem(tree.add(None, ReNode::char_range(Segments::new(Seg(c1 as u32, c2 as u32)))))
            }
            CtxItem::Item7 { charlit } => {         // item -> CharLit
                // charlit is always sourrounded by quotes:
                // fragment CharLiteral	: '\'' Char '\'';
                let c = decode_char(&charlit[1..charlit.len() - 2]).unwrap_or_else(|s| panic!("{s}"));
                SynItem(tree.add(None, ReNode::char(c)))
            }
        }
    }
}

fn decode_char(char: &str) -> Result<char, String> {
    // fragment Char        : EscChar | ~[\n\r\t'\\];
    // fragment EscChar     : '\\' ([nrt'\\] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    if char.as_bytes()[0] == b'\\' {
        match char.as_bytes()[1] {
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
            _ => Err(format!("Unknown escape code '{char}'")), // shouldn't happen
        }

    } else {
        Ok(char.chars().nth(1).unwrap())
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