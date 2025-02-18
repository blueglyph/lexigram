mod basic_test;

use rlexer::dfa::{ReType, Terminal, TokenId};
use std::collections::HashMap;
use std::ops::Range;
use vectree::VecTree;
use rlexer::dfa::ReNode;
use rlexer::log::Logger;
use rlexer::{hashmap, node, term};
use crate::gen::lexiparser::lexiparser::*;
use crate::gen::lexiparser::lexiparser_types::*;

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
                let rule_exit = action_maybe.unwrap_or(term!(= self.terminals.len() as TokenId));
                let mut root = rule.get_root().unwrap();
                if *rule.get(root).get_type() != ReType::Concat {
                    let new_root = rule.addci(None, node!(&), root);
                    rule.set_root(new_root);
                    root = new_root;
                }
                rule.add(Some(root), ReNode::end(rule_exit));
                self.terminals.push(rule);
            }
            self.rules.insert(id, rule_type);
        }
        SynRule()
    }

    fn exit_actions(&mut self, ctx: CtxActions) -> SynActions {
        // actions:
        // - skip           => doesn't return token, current string dropped (modifier)
        // - more           => doesn't return token, current string kept for next rule (modifier)
        // - push(n)        => returns token + push
        // - pop            => returns token + pop
        // - channel #      => returns token + channel
        let CtxActions::Actions { action, star } = ctx;
        let mut terminal = action.0;
        for action in star.0 {
            todo!();
            // terminal = terminal + action.0;
        }

        SynActions(terminal)
    }

    fn exit_action(&mut self, ctx: CtxAction) -> SynAction {
        let action = match ctx {
            CtxAction::
            Action1 { id } => {
                let n = self.modes.len();
                let id_val = *self.modes.entry(id).or_insert({
                    self.mode_range.push(0..0);
                    n
                }) as u16;
                term!(push id_val)
            },
            CtxAction::Action2 => term!(pop),
            CtxAction::Action3 => term!(skip),
            CtxAction::Action4 => term!(= self.terminals.len() as TokenId),
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