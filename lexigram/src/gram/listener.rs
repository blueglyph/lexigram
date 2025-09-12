// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use crate::gram::gramparser::*;
use crate::gram::gramparser::gramparser_types::*;
use iter_index::IndexerIterator;
use lexigram_lib::grammar::{grtree_to_str, GrNode, GrTree, GrTreeExt, ProdRuleSet, RuleTreeSet, Symbol, VarId};
use lexigram_lib::log::{BufLog, BuildFrom, LogReader, LogStatus, Logger};
use lexigram_lib::{BuildErrorSource, CollectJoin, General, HasBuildErrorSource, SymbolTable};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use vectree::VecTree;

enum PostCheck {
    RepeatChildLform { node: usize, var: VarId }
}

pub struct GramListener {
    verbose: bool,
    name: String,
    log: BufLog,
    abort: bool,
    curr: Option<GrTree>,
    curr_name: Option<String>,
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
    post_check: Vec<PostCheck>,
    num_nt: usize,
}

impl GramListener {
    /// Gram listener used for parsing grammar files.
    ///
    /// `symbol_table` must contain the terminal symbols from the lexicon corresponding to the grammar.
    pub fn new(symbol_table: SymbolTable) -> Self {
        // copies the NT and T from the symbol table
        let symbols = symbol_table.get_terminals()
            .index::<VarId>()
            .map(|(t, (s, _))| (s.clone(), Symbol::T(t)))
            .collect::<HashMap<_,_>>();
        assert_eq!(symbol_table.get_num_nt(), 0, "the symbol table cannot contain nonterminals");
        GramListener {
            verbose: false,
            name: String::new(),
            abort: false,
            log: BufLog::new(),
            curr: None,
            curr_name: None,
            curr_nt: None,
            rules: Vec::new(),
            start_rule: None,
            symbol_table,
            symbols,
            nt_reserved: HashMap::new(),
            post_check: Vec::new(),
            num_nt: 0,
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_start_rule(&self) -> Option<VarId> {
        self.start_rule
    }

    fn reserve_nt_symbol(&mut self, id: String) -> Option<VarId> {
        if let Some(v) = self.nt_reserved.get(&id) {
            Some(*v)
        } else {
            match VarId::try_from(self.nt_reserved.len()) {
                Ok(len) => {
                    let v = VarId::MAX - len;
                    if self.num_nt > v as usize {
                        self.log.add_error(format!("not enough space for defined ({}) + reserved nonterminals ({len}): can't reserve '{id}'", self.num_nt));
                        None
                    } else {
                        self.nt_reserved.insert(id, v);
                        Some(v)
                    }
                }
                Err(_) => {
                    self.log.add_error(format!("too many reserved symbols: can't reserve '{id}'"));
                    None
                }
            }
        }
    }

    fn add_nt_symbol(&mut self, name: &str) -> Option<VarId> {
        let nt = VarId::try_from(self.num_nt).map_err(|_| self.log.add_error("too many nonterminals")).ok()?;
        match self.symbols.insert(name.to_string(), Symbol::NT(nt)) {
            Some(Symbol::NT(_)) => {
                self.log.add_error(format!("rule {}: non-terminal '{name}' already defined", self.curr_name.as_ref().unwrap()));
                None
            },
            Some(Symbol::T(_)) => {
                self.log.add_error(format!("rule {}: '{name}' is a terminal and cannot be used as a rule name", self.curr_name.as_ref().unwrap()));
                None
            }
            Some(sym) => {
                self.log.add_error(format!("rule {}: '{}' cannot be used as rule name", self.curr_name.as_ref().unwrap(), sym.to_str(Some(&self.symbol_table))));                None
            }
            None => {
                self.symbol_table.add_nonterminal(name);
                self.num_nt += 1;
                Some(nt)
            }
        }
    }

    fn do_post_checks(&mut self) {
        for check in &self.post_check {
            match check {
                PostCheck::RepeatChildLform { node, var } => {
                    let tree = &self.rules[*var as usize];
                    if tree.iter_depth_at(*node).any(|node| if let GrNode::LForm(v) = *node { v == *var } else { false }) {
                        let symtab = Some(&self.symbol_table);
                        self.log.add_error(
                            format!("in {}, {}:  <L> points to the same nonterminal. It must be a new one, created for the loop.",
                                    Symbol::NT(*var).to_str(symtab),
                                    grtree_to_str(&tree, Some(*node), None, symtab, false)));
                    }
                }
            }
        }
    }
}

impl LogReader for GramListener {
    type Item = BufLog;

    fn get_log(&self) -> &BufLog {
        &self.log
    }

    fn give_log(self) -> BufLog {
        self.log
    }
}

impl HasBuildErrorSource for GramListener {
    const SOURCE: BuildErrorSource = BuildErrorSource::Gram;
}

impl From<GramListener> for ProdRuleSet<General> {
    /// Builds a [`ProdRuleSet<General>`] from a [`GramListener`].
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn from(mut gram_listener: GramListener) -> ProdRuleSet<General> {
        gram_listener.do_post_checks();
        let mut rts = RuleTreeSet::<General>::with_log(gram_listener.log);
        let no_error = rts.get_log().has_no_errors();
        if no_error {
            for (v, rule) in gram_listener.rules.into_iter().index::<VarId>() {
                rts.set_tree(v, rule);
            }
            rts.set_symbol_table(gram_listener.symbol_table);
        }
        let mut prs = ProdRuleSet::<General>::build_from(rts);
        if no_error {
            prs.set_start(gram_listener.start_rule.unwrap());
        }
        prs
    }
}

impl Debug for GramListener {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GramListener {{")?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  log:{}", self.log.get_messages().map(|s| format!("\n    - {s:?}")).join(""))?;
        writeln!(f, "  curr: {} -> {}",
                 if let Some(nm) = &self.curr_name { nm } else { "?" },
                 if let Some(t) = &self.curr { format!("{t:?}") } else { "none".to_string() })?;
        let symb_nt = self.symbols.iter().filter_map(|(name, s)| if let Symbol::NT(nt) = s { Some((nt, name)) } else { None }).collect::<BTreeMap<_, _>>();
        let symb_t = self.symbols.iter().filter_map(|(name, s)| if let Symbol::T(t) = s { Some((t, name)) } else { None }).collect::<BTreeMap<_, _>>();
        writeln!(f, "  rules:{}",
                 self.rules.iter().index::<VarId>().map(|(v, t)|
                     format!("\n  - {}: {}", symb_nt.get(&v).unwrap(), if t.is_empty() { String::new() } else { t.to_str(None, Some(&self.symbol_table)) })
                 ).join(""))?;
        writeln!(f, "  symbols:\n  - NT: {}\n  - T : {}",
                 symb_nt.into_iter().map(|(nt, s)| format!("{nt}={s}")).join(", "), symb_t.into_iter().map(|(t, s)| format!("{t}={s}")).join(", "))?;
        writeln!(f, "  nt_reserved: {}", self.nt_reserved.iter().map(|(n, v)| format!("{n}={v}")).join(", "))?;
        writeln!(f, "}}")
    }
}

impl GramParserListener for GramListener {
    fn check_abort_request(&self) -> bool {
        self.abort
    }

    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    // file:
    //     header rules SymEOF?
    // ;
    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        if self.verbose { println!("- exit_file({_ctx:?})"); }
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
        if self.verbose { println!("- exit_header({ctx:?}"); }
        let CtxHeader::Header { id } = ctx;
        self.name = id;
        SynHeader()
    }

    // rules:
    //     rule
    // |   rules rule
    // ;
    fn exit_rules(&mut self, _ctx: CtxRules) -> SynRules {
        if self.verbose { println!("exit_rules({_ctx:?})"); }
        SynRules()
    }

    fn init_rule(&mut self) {
        if self.verbose { println!("init_rule()"); }
        assert!(self.curr.is_none(), "remnant tree in self.curr: {self:?}");
        self.curr = Some(GrTree::new());
    }

    // rule:
    //     Id Colon prod Semicolon
    // ;
    fn exit_rule(&mut self, ctx: CtxRule) -> SynRule {
        if self.verbose { println!("exit_rule({ctx:?})"); }
        let mut tree = self.curr.take().expect("self.curr should have a tree");
        let curr_nt = self.curr_nt.take().unwrap();
        let id = match ctx {
            CtxRule::Rule1 { prod: SynProd(id), .. } => id,      // rule -> rule_name : prod ;
            CtxRule::Rule2 { prod: SynProd(id), .. } => {        // rule -> rule_name : prod EOF ;
                if curr_nt > 0 {
                    self.log.add_error(format!("rule '{}': EOF can only be put in the top rule", self.curr_name.as_ref().unwrap()));
                }
                // we don't add Symbol::End to the tree because it's not necessary nor, in fact, even allowed)
                id
            }
        };
        tree.set_root(id);
        if self.rules.len() < curr_nt as usize {
            self.rules.resize(curr_nt as usize, VecTree::new());
        }
        self.rules.push(tree);
        self.curr_name = None;
        SynRule()
    }

    fn exit_rule_name(&mut self, ctx: CtxRuleName) -> SynRuleName {
        if self.verbose { println!("exit_rule_name({ctx:?})"); }
        let CtxRuleName::RuleName { id: name } = ctx;
        self.curr_name = Some(name.clone());
        let Some(nt) = self.add_nt_symbol(&name) else {
            self.abort = true;
            return SynRuleName(String::new());
        };
        self.curr_nt = Some(nt);
        if self.start_rule.is_none() {
            // the start rule is the first to be defined
            self.start_rule = Some(nt);
        }
        SynRuleName(name)
    }

    // prod:
    //     prodTerm
    // |   prod Or prodTerm
    // ;
    fn exit_prod(&mut self, ctx: CtxProd) -> SynProd {
        if self.verbose { println!("exit_prod({ctx:?})"); }
        let tree = self.curr.as_mut().expect("no current tree");
        let id = match ctx {
            CtxProd::Prod1 { prod_term } => prod_term.0,            // first iteration
            CtxProd::Prod2 { prod, prod_term } => {                   // next iterations
                if matches!(tree.get(prod.0), &GrNode::Or) {
                    // if there's already an |, adds another child
                    tree.attach_child(prod.0, prod_term.0);
                    prod.0
                } else {
                    // creates an | with the previous and current factors as children
                    tree.addci_iter(None, GrNode::Or, [prod.0, prod_term.0])
                }
            }
        };
        SynProd(id)
    }

    // prodTerm:
    //     prodFactor*
    // ;
    fn exit_prod_term(&mut self, ctx: CtxProdTerm) -> SynProdTerm {
        if self.verbose { println!("exit_prod_term({ctx:?})"); }
        let tree = self.curr.as_mut().expect("no current tree");
        let CtxProdTerm::ProdTerm { star: SynProdTerm1(factors) } = ctx;
        let pt = factors.into_iter().map(|SynProdFactor(t)| t).to_vec();
        let id = match pt.len() {
            0 => tree.add(None, GrNode::Symbol(Symbol::Empty)),
            1 => pt[0],
            _ => tree.addci_iter(None, GrNode::Concat, pt)
        };
        SynProdTerm(id)
    }

    // prodFactor:
    //     prodAtom (Plus | Star | Question)?
    // ;
    fn exit_prod_factor(&mut self, ctx: CtxProdFactor) -> SynProdFactor {
        if self.verbose { println!("exit_prod_factor_rep({ctx:?})"); }
        let tree = self.curr.as_mut().expect("no current tree");
        let (id, l_check) = match ctx {
            CtxProdFactor::ProdFactor1 { prod_atom: SynProdAtom(factor_item) } => (tree.addci(None, GrNode::Plus, factor_item), true),   // prodAtom +
            CtxProdFactor::ProdFactor2 { prod_atom: SynProdAtom(factor_item) } => (tree.addci(None, GrNode::Maybe, factor_item), false), // prodAtom ?
            CtxProdFactor::ProdFactor3 { prod_atom: SynProdAtom(factor_item) } => (tree.addci(None, GrNode::Star, factor_item), true),   // prodAtom *
            CtxProdFactor::ProdFactor4 { prod_atom: SynProdAtom(factor_item) } => (factor_item, false),                                  // prodAtom
        };
        if l_check {
            self.post_check.push(PostCheck::RepeatChildLform { node: id, var: self.curr_nt.unwrap() });
        }
        SynProdFactor(id)
    }

    // prodAtom:
    //     Id
    // |   Lform
    // |   Rform
    // |   Pform
    // |   Lparen prod Rparen
    // ;
    fn exit_prod_atom(&mut self, ctx: CtxProdAtom) -> SynProdAtom {
        if self.verbose { println!("exit_prod_atom({ctx:?})"); }
        let id = match ctx {
            CtxProdAtom::ProdAtom1 { id } => {                  // factor_item -> Id
                match self.symbols.get(&id) {
                    Some(s @ Symbol::NT(_)) |
                    Some(s @ Symbol::T(_)) => self.curr.as_mut().unwrap().add(None, GrNode::Symbol(*s)),
                    Some(unexpected) => panic!("unexpected symbol: {unexpected:?}"),
                    None => {
                        // reserve new NT
                        if let Some(nt) = self.reserve_nt_symbol(id) {
                            self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::NT(nt)))
                        } else {
                            // failure
                            self.abort = true;
                            return SynProdAtom(0 /* don't care */);
                        }
                    }
                }
            }
            CtxProdAtom::ProdAtom2 { lform } => {               // factor_item -> Lform
                let bytes = lform.as_bytes();
                let name_maybe = if bytes[2] == b'=' {
                    let name = lform[3..lform.len() - 1].to_string();
                    if &name == self.curr_name.as_ref().unwrap() {
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
                    if let Some(sym @ Symbol::NT(_)) | Some(sym @ Symbol::T(_)) = self.symbols.get(&name) {
                        self.log.add_error(format!("rule {}: the rule name in <L={name}> is already defined as {}terminal",
                                                   self.curr_name.as_ref().unwrap(), if sym.is_nt() { "non-" } else { "" }));
                        self.abort = true;
                        return SynProdAtom(0 /* don't care */);
                    } else if self.nt_reserved.contains_key(&name) {
                        self.log.add_error(format!("rule {}: the rule name in <L={name}> has already been used as non-terminal in a rule",
                                                   self.curr_name.as_ref().unwrap()));
                        self.abort = true;
                        return SynProdAtom(0 /* don't care */);
                    }
                    match self.add_nt_symbol(&name) {
                        Some(nt) => nt,
                        None => {
                            self.abort = true;
                            return SynProdAtom(0 /* don't care */)
                        }
                    }
                } else {
                    // this form is used with right-recursive rules, so it points to the current rule
                    self.curr_nt.expect("curr_nt must be defined")
                };
                self.curr.as_mut().unwrap().add(None, GrNode::LForm(nt))
            }
            CtxProdAtom::ProdAtom3 => {                         // factor_item -> <R>
                self.curr.as_mut().unwrap().add(None, GrNode::RAssoc)
            }
            CtxProdAtom::ProdAtom4 => {                         // factor_item -> <P>
                self.curr.as_mut().unwrap().add(None, GrNode::PrecEq)
            }
            CtxProdAtom::ProdAtom5 { prod } => prod.0,          // factor_item -> ( prod )
        };
        SynProdAtom(id)
    }
}

