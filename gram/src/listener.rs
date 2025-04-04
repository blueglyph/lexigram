// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use iter_index::IndexerIterator;
use vectree::VecTree;
use lexigram::{CollectJoin, General};
use lexigram::grammar::{GrTree, GrNode, GrTreeExt, RuleTreeSet, Symbol, VarId, ProdRuleSet};
use lexigram::log::{BufLog, Logger};
use lexigram::symbol_table::SymbolTable;
use crate::gramparser::gramparser::*;
use crate::gramparser::gramparser_types::*;

pub struct GramListener {
    verbose: bool,
    name: String,
    log: BufLog,
    curr: Option<GrTree>,
    curr_rulename: Option<String>,
    curr_nt: Option<VarId>,
    rules: Vec<VecTree<GrNode>>,
    start_rule: Option<VarId>,
    symbol_table: SymbolTable,
    /// T symbols pre-defined in the symbol table; the listener adds the NT symbols.
    symbols: HashMap<String, Symbol>,
    /// NT references found in the grammar that haven't been defined yet (future rules).
    /// They get a VarId > num_nt, starting at VarId::MAX, VarId::MAX - 1, ... and must
    /// be renumbered in the rules later on when the true VarId is known.
    /// OPTIMIZE: because of this 1-pass system that preserves the ID order of the grammar file,
    /// we use more space in the VarId range: |defined| + |reserved| instead of |defined|.
    nt_reserved: HashMap<String, VarId>,
    num_nt: usize,
}

impl GramListener {
    /// Gram listener used for parsing grammar files.
    ///
    /// `symbol_table` must contain the terminal symbols from the lexicon corresponding to the grammar.
    pub fn new(symbol_table: SymbolTable) -> Self {
        // copies the NT and T from the symbol table
        let symbols = symbol_table.get_terminals()
            .into_iter().index::<VarId>()
            .map(|(t, (s, _))| (s.clone(), Symbol::T(t)))
            .collect::<HashMap<_,_>>();
        assert_eq!(symbol_table.get_num_nt(), 0, "the symbol table cannot contain non-terminals");
        GramListener {
            verbose: false,
            name: String::new(),
            log: BufLog::new(),
            curr: None,
            curr_rulename: None,
            curr_nt: None,
            rules: Vec::new(),
            start_rule: None,
            symbol_table,
            symbols,
            nt_reserved: HashMap::new(),
            num_nt: 0,
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_log(&self) -> &BufLog {
        &self.log
    }

    pub fn build_prs(mut self) -> ProdRuleSet<General> {
        if self.log.num_errors() > 0 {
            panic!("Errors when parsing grammar:{}", self.log.get_messages().map(|m| format!("\n- {m:?}")).join(""));
        }
        let mut rts = RuleTreeSet::<General>::new();
        for (v, rule) in self.rules.into_iter().index::<VarId>() {
            rts.set_tree(v, rule);
        }
        rts.set_symbol_table(std::mem::take(&mut self.symbol_table));
        rts.into()
    }

    fn reserve_nt_symbol(&mut self, id: String) -> VarId {
        let len = self.nt_reserved.len();
        if let Some(v) = self.nt_reserved.get(&id) {
            *v
        } else {
            let v = VarId::MAX - VarId::try_from(len).expect("too many reserved symbols");
            if self.num_nt > v as usize { panic!("not enough space for defined + reserved non-terminals") }
            self.nt_reserved.insert(id, v);
            v
        }
    }

    fn add_nt_symbol(&mut self, name: &str) -> VarId {
        let nt = VarId::try_from(self.num_nt).expect("too many non-terminals");
        assert_eq!(self.symbols.insert(name.to_string(), Symbol::NT(nt)), None, "non-terminal '{name}' already defined");
        self.symbol_table.add_non_terminal(name);
        self.num_nt += 1;
        nt
    }
}

impl Debug for GramListener {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GramListener {{")?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  log:{}", self.log.get_messages().map(|s| format!("\n    - {s:?}")).join(""))?;
        writeln!(f, "  curr: {} -> {}",
                 if let Some(nm) = &self.curr_rulename { nm } else { "?" },
                 if let Some(t) = &self.curr { format!("{t:?}") } else { "none".to_string() })?;
        let symb_nt = self.symbols.iter().filter_map(|(name, s)| if let Symbol::NT(nt) = s { Some((nt, name)) } else { None }).collect::<BTreeMap<_, _>>();
        let symb_t = self.symbols.iter().filter_map(|(name, s)| if let Symbol::T(t) = s { Some((t, name)) } else { None }).collect::<BTreeMap<_, _>>();
        writeln!(f, "  rules:{}",
                 self.rules.iter().index::<VarId>().map(|(v, t)|
                     format!("\n  - {}: {}", symb_nt.get(&v).unwrap(), t.to_str(None, Some(&self.symbol_table)))
                 ).join(""))?;
        writeln!(f, "  symbols:\n  - NT: {}\n  - T : {}",
                 symb_nt.into_iter().map(|(nt, s)| format!("{nt}={s}")).join(", "), symb_t.into_iter().map(|(t, s)| format!("{t}={s}")).join(", "))?;
        writeln!(f, "}}")
    }
}

impl GramParserListener for GramListener {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    // file:
    //     header rules SymEOF?
    // ;
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        let mut old_new = HashMap::new();
        for (name, old_nt) in &self.nt_reserved {
            if let Some(Symbol::NT(new_nt)) = self.symbols.get(name) {
                old_new.insert(old_nt, *new_nt);
            } else {
                self.log.add_error(format!("'{name}' has been used but is not defined, neither as terminal or non-terminal"));
            };
        }
        // OPTIMIZE: we could have tagged the rules containing reserved NTs; here, we'll have to scan everything
        for rule in self.rules.iter_mut() {
            for mut node in rule.iter_depth_simple_mut() {
                if let GrNode::Symbol(Symbol::NT(old)) = *node {
                    if let Some(new) = old_new.get(&old) {
                        *node = GrNode::Symbol(Symbol::NT(*new));
                    }
                }
            }
        }
        SynFile()
    }

    // header:
    //     Grammar Id Semicolon
    // ;
    fn exit_header(&mut self, ctx: CtxHeader) -> SynHeader {
        let CtxHeader::Header { id } = ctx;
        self.name = id;
        SynHeader()
    }

    // rules:
    //     rule
    // |   rules rule
    // ;
    fn exit_rules(&mut self, _ctx: CtxRules) -> SynRules {
        SynRules()
    }

    // rule:
    //     Id Colon prod Semicolon
    // ;
    fn exit_rule(&mut self, ctx: CtxRule) -> SynRule {
        let CtxRule::Rule { prod: SynProd(id), .. } = ctx;
        let mut tree = self.curr.take().expect("self.curr should have a tree");
        tree.set_root(id);
        self.rules.push(tree);
        self.curr_nt = None;
        SynRule()
    }

    fn exit_rule_name(&mut self, ctx: CtxRuleName) -> SynRuleName {
        let CtxRuleName::RuleName { id: name } = ctx;
        self.curr_rulename = Some(name.clone());
        let nt = self.add_nt_symbol(&name);
        self.curr_nt = Some(nt);
        if self.start_rule.is_none() {
            // the start rule is the first to be defined
            self.start_rule = Some(nt);
        }
        SynRuleName(name)
    }

    fn init_prod(&mut self) {
        assert!(self.curr.is_none(), "remnant tree in self.curr: {self:?}");
        self.curr = Some(GrTree::new());
    }

    // prod:
    //     prodFactor
    // |   prod Or prodFactor
    // ;
    fn exit_prod(&mut self, ctx: CtxProd) -> SynProd {
        let tree = self.curr.as_mut().expect("no current tree");
        let id = match ctx {
            CtxProd::Prod1 { prod_factor } => prod_factor.0,            // first iteration
            CtxProd::Prod2 { prod, prod_factor } => {                   // next iterations
                if matches!(tree.get(prod.0), &GrNode::Or) {
                    // if there's already an |, adds another child
                    tree.attach_child(prod.0, prod_factor.0);
                    prod.0
                } else {
                    // creates an | with the previous and current factors as children
                    tree.addci_iter(None, GrNode::Or, [prod.0, prod_factor.0])
                }
            }
            CtxProd::Prod3 { prod } => prod.0,                          // end of iterations
        };
        SynProd(id)
    }

    // prodFactor:
    //     prodTerm*
    // ;
    fn exit_prod_factor(&mut self, ctx: CtxProdFactor) -> SynProdFactor {
        let tree = self.curr.as_mut().expect("no current tree");
        let CtxProdFactor::ProdFactor { star: SynProdFactor1(terms) } = ctx;
        let id = if terms.len() > 1 {
            tree.addci_iter(None, GrNode::Concat, terms.iter().map(|t| t.0))
        } else {
            terms[0].0
        };
        SynProdFactor(id)
    }

    // prodTerm:
    //     termItem (Plus | Star | Question)?
    // ;
    fn exit_prod_term(&mut self, ctx: CtxProdTerm) -> SynProdTerm {
        let tree = self.curr.as_mut().expect("no current tree");
        let id = match ctx {
            CtxProdTerm::ProdTerm1 { term_item } => tree.addci(None, GrNode::Plus, term_item.0),    // termItem +
            CtxProdTerm::ProdTerm2 { term_item } => tree.addci(None, GrNode::Maybe, term_item.0),   // termItem ?
            CtxProdTerm::ProdTerm3 { term_item } => tree.addci(None, GrNode::Star, term_item.0),    // termItem *
            CtxProdTerm::ProdTerm4 { term_item } => term_item.0,                                    // termItem
        };
        SynProdTerm(id)
    }

    // termItem:
    //     Id
    // |   Lform
    // |   Rform
    // |   Lparen prod Rparen
    // ;
    fn exit_term_item(&mut self, ctx: CtxTermItem) -> SynTermItem {
        let id = match ctx {
            CtxTermItem::TermItem1 { id } => {
                match self.symbols.get(&id) {
                    Some(s @ Symbol::NT(_)) |
                    Some(s @ Symbol::T(_)) => self.curr.as_mut().unwrap().add(None, GrNode::Symbol(*s)),
                    Some(unexpected) => panic!("unexpected symbol: {unexpected:?}"),
                    None => {
                        // reserve new NT
                        let nt = self.reserve_nt_symbol(id);
                        self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::NT(nt)))
                    }
                }
            }
            CtxTermItem::TermItem2 { lform } => {
                let bytes = lform.as_bytes();
                let name_maybe = if bytes[2] == b'=' {
                    let name = lform[3..lform.len() - 1].to_string();
                    if &name == self.curr_rulename.as_ref().expect("self.curr_rulename should contain the rule name") {
                        // that must be a right-recursive rule (to check later)
                        None
                    } else {
                        Some(name)
                    }
                } else {
                    None
                };
                let nt = if let Some(name) = name_maybe {
                    // this form is used with * and +, and it defines the name of the iterative NT.
                    // In RuleTreeSet::normalize_plus_or_star(), the NT index in LForm(NT) is used when the
                    // iterative NT is created.
                    assert!(!self.nt_reserved.contains_key(&name), "the rule name in <L={name}> has already been used as non-terminal in a rule");
                    assert!(!self.symbols.contains_key(&name), "the rule name in <L={name}> is already defined");
                    self.add_nt_symbol(&name)
                } else {
                    // this form is used with right-recursive rules, so it points to the current rule
                    self.curr_nt.expect("curr_nt must be defined")
                };
                self.curr.as_mut().unwrap().add(None, GrNode::LForm(nt))
            }
            CtxTermItem::TermItem3 => {
                self.curr.as_mut().unwrap().add(None, GrNode::RAssoc)
            }
            CtxTermItem::TermItem4 { prod } => prod.0,
        };
        SynTermItem(id)
    }
}

