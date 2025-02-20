mod basic_test;

use rlexer::dfa::{ChannelId, ModeOption, ReType, ActionOption, Terminal, TokenId};
use std::collections::HashMap;
use std::ops::{Add, Range};
use vectree::VecTree;
use rlexer::dfa::ReNode;
use rlexer::log::Logger;
use rlexer::{hashmap, node};
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
    Terminal(usize),
    Fragment(usize)
}

struct LexiListener {
    verbose: bool,
    curr: Option<VecTree<ReNode>>,
    rules: HashMap<String, RuleType>,
    fragments: Vec<VecTree<ReNode>>,
    terminals: Vec<VecTree<ReNode>>,
    modes: HashMap<String, usize>,
    mode_range: Vec<Range<usize>>,    // exclusive range
    log: Logger,
}

impl LexiListener {
    fn new() -> Self {
        LexiListener {
            verbose: false,
            curr: None,
            rules: HashMap::new(),
            fragments: Vec::new(),
            terminals: Vec::new(),
            modes: hashmap!("DEFAULT_MODE".to_string() => 0),
            mode_range: vec![0..0], // empty
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

    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader {
        todo!()
    }

    fn exit_declaration(&mut self, _ctx: CtxDeclaration) -> SynDeclaration {
        todo!()
    }

    fn exit_option(&mut self, _ctx: CtxOption) -> SynOption {
        todo!()
    }

    fn init_rule(&mut self) {
        assert!(self.curr.is_none());
        self.curr = Some(VecTree::new());
    }

    fn exit_rule(&mut self, ctx: CtxRule) -> SynRule {
        let (id, rule_type, action_maybe) = match ctx {
            CtxRule::Rule1 { id, .. } => {
                (id, RuleType::Fragment(self.fragments.len()), None)
            }
            CtxRule::Rule2 { id, actions, .. } => {
                (id, RuleType::Terminal(self.terminals.len()), Some(actions.0))
            }
            CtxRule::Rule3 { id, .. } => {
                (id, RuleType::Terminal(self.terminals.len()), None)
            }
        };
        if self.rules.contains_key(&id) {
            self.log.add_error(format!("Symbol '{id}' is already defined"));
        } else {
            let mut rule = self.curr.take().unwrap();
            if let RuleType::Fragment(_) = rule_type {
                self.fragments.push(rule);
            } else {
                // if no action, only return
                let rule_action = action_maybe.unwrap_or(action!(= self.terminals.len() as TokenId));
                let mut root = rule.get_root().unwrap();
                if *rule.get(root).get_type() != ReType::Concat {
                    let new_root = rule.addci(None, node!(&), root);
                    rule.set_root(new_root);
                    root = new_root;
                }
                let terminal = rule_action.to_terminal(Some(self.terminals.len() as TokenId)).unwrap(); // FIXME: manage errors (may panic)
                rule.add(Some(root), ReNode::end(terminal));
                self.terminals.push(rule);
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
            CtxAction::Action1 { id } => {
                let n = self.modes.len();
                let id_val = *self.modes.entry(id).or_insert({
                    self.mode_range.push(0..0);
                    n
                }) as u16;
                action!(mode id_val)
            }
            CtxAction::Action2 { id } => {
                let n = self.modes.len();
                let id_val = *self.modes.entry(id).or_insert({
                    self.mode_range.push(0..0);
                    n
                }) as u16;
                action!(push id_val)
            },
            CtxAction::Action3 => action!(pop),
            CtxAction::Action4 => action!(skip),
            CtxAction::Action5 => action!(more),
            CtxAction::Action6 { id } => todo!("type(id)"),
            CtxAction::Action7 { id } => todo!("channel(id)")
        };
        SynAction(action)
    }

    fn exit_match(&mut self, _ctx: CtxMatch) -> SynMatch {
        SynMatch()
    }

    fn exit_alt_items(&mut self, _ctx: CtxAltItems) -> SynAltItems {
        todo!()
    }

    fn exit_alt_item(&mut self, _ctx: CtxAltItem) -> SynAltItem {
        todo!()
    }

    fn exit_repeat_item(&mut self, _ctx: CtxRepeatItem) -> SynRepeatItem {
        todo!()
    }

    fn exit_item(&mut self, _ctx: CtxItem) -> SynItem {
        todo!()
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