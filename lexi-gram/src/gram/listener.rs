// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use crate::gram::gramparser::*;
use crate::gram::gramparser::gramparser_types::*;
use iter_index::IndexerIterator;
use lexigram_lib::grammar::{grtree_to_str, GrNode, GrTree, GrTreeExt, ProdRuleSet, RuleTreeSet};
use lexigram_lib::build::BuildFrom;
use lexigram_lib::log::{BufLog, LogReader, LogStatus, Logger};
use lexigram_lib::parser::{Symbol, Terminate};
use lexigram_lib::{General, SymbolTable, VarId};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use vectree::VecTree;
use lexigram_lib::CollectJoin;
use lexigram_lib::build::{BuildErrorSource, HasBuildErrorSource};
use lexigram_lib::lexer::PosSpan;
use lexigram_lib::lexigram_core::text_span::{GetLine, GetTextSpan};

pub struct GramListener<'ls> {
    verbose: bool,
    ansi: bool,
    lines: Vec<&'ls str>,
    name: String,
    log: BufLog,
    abort: Terminate,
    curr: Option<GrTree>,
    curr_name: Option<String>,
    curr_nt: Option<VarId>,
    stack_lform: Vec<PosSpan>,
    rules: Vec<VecTree<GrNode>>,
    start_nt: Option<VarId>,
    disable_warning_unused_nt_t: bool,
    symbol_table: SymbolTable,
    /// T symbols pre-defined in the symbol table; the listener adds the NT symbols.
    symbols: HashMap<String, Symbol>,
    /// NT references found in the grammar that haven't been defined yet (future rules).
    /// They get a VarId > num_nt, starting at VarId::MAX, VarId::MAX - 1, ... and must
    /// be renumbered in the rules later on when the true VarId is known.
    /// OPTIMIZE: because of this 1-pass system that preserves the ID order of the grammar file,
    /// we use more space in the VarId range: |defined| + |reserved| instead of |defined|.
    nt_reserved: HashMap<String, VarId>,
    // post_check: Vec<PostCheck>,
    num_nt: usize,
}

impl<'ls> GramListener<'ls> {
    /// Gram listener used for parsing grammar files.
    ///
    /// `symbol_table` must contain the terminal symbols from the lexicon corresponding to the grammar.
    pub fn new(symbol_table: SymbolTable, grammar: &'ls str) -> Self {
        // copies the NT and T from the symbol table
        let symbols = symbol_table.get_terminals()
            .index::<VarId>()
            .map(|(t, (s, _))| (s.clone(), Symbol::T(t)))
            .collect::<HashMap<_,_>>();
        assert_eq!(symbol_table.get_num_nt(), 0, "the symbol table cannot contain nonterminals");
        GramListener {
            verbose: false,
            ansi: true,
            lines: grammar.lines().collect(),
            name: String::new(),
            abort: Terminate::None,
            log: BufLog::new(),
            curr: None,
            curr_name: None,
            curr_nt: None,
            stack_lform: Vec::new(),
            rules: Vec::new(),
            start_nt: None,
            disable_warning_unused_nt_t: false,
            symbol_table,
            symbols,
            nt_reserved: HashMap::new(),
            // post_check: Vec::new(),
            num_nt: 0,
        }
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn set_ansi(&mut self, ansi: bool) {
        self.ansi = ansi;
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_start_rule(&self) -> Option<VarId> {
        self.start_nt
    }

    pub fn set_disable_warning_unused_nt_t(&mut self, flag: bool) {
        self.disable_warning_unused_nt_t = flag;
    }

    pub fn get_symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn set_start_nt(&mut self, start_nt: VarId) {
        self.start_nt = Some(start_nt);
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

    fn annotate(&self, span: &PosSpan) -> String {
        if self.ansi {
            self.annotate_text(span)
        } else {
            self.annotate_text_ascii(span)
        }
    }

    fn log_error(&mut self, span: &PosSpan, message: &str) {
        let text = self.annotate(span);
        self.log.add_error(format!("at {span}, {message}:\n\n{text}\n"));
    }
}

impl LogReader for GramListener<'_> {
    type Item = BufLog;

    fn get_log(&self) -> &BufLog {
        &self.log
    }

    fn give_log(self) -> BufLog {
        self.log
    }
}

impl GetLine for GramListener<'_> {
    fn get_line(&self, n: usize) -> &str {
        self.lines[n - 1]
    }
}

impl HasBuildErrorSource for GramListener<'_> {
    const SOURCE: BuildErrorSource = BuildErrorSource::Gram;
}

impl From<GramListener<'_>> for ProdRuleSet<General> {
    /// Builds a [`ProdRuleSet<General>`] from a [`GramListener`].
    ///
    /// If an error is encountered or was already encountered before, an empty shell object
    /// is built with the log detailing the error(s).
    fn from(gram_listener: GramListener) -> ProdRuleSet<General> {
        const VERBOSE: bool = false;
        if VERBOSE { println!("{gram_listener:?}"); }
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
            prs.set_start(gram_listener.start_nt.unwrap());
            prs.set_disable_warning_unused_nt_t(gram_listener.disable_warning_unused_nt_t);
        }
        prs
    }
}

impl Debug for GramListener<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GramListener {{")?;
        writeln!(f, "  abort = {:?}", self.abort)?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  curr_name = {:?}", self.curr_name)?;
        writeln!(f, "  curr_nt = {:?}", self.curr_nt)?;
        writeln!(f, "  stack_lform = {}", self.stack_lform.iter().map(|s| s.to_string()).join(", "))?;
        writeln!(f, "  log:{}", self.log.get_messages().map(|s| format!("\n    - {s:?}")).join(""))?;
        writeln!(f, "  curr: {} ", if let Some(t) = &self.curr {
            grtree_to_str(t, self.start_nt.map(|nt| nt as usize), None, self.curr_nt, Some(&self.symbol_table), false)
        } else { "none".to_string() })?;
        let symb_nt = self.symbols.iter().filter_map(|(name, s)| if let Symbol::NT(nt) = s { Some((nt, name)) } else { None }).collect::<BTreeMap<_, _>>();
        let symb_t = self.symbols.iter().filter_map(|(name, s)| if let Symbol::T(t) = s { Some((t, name)) } else { None }).collect::<BTreeMap<_, _>>();
        writeln!(f, "  rules:{}",
                 self.rules.iter().index::<VarId>().map(|(v, t)|
                     format!("\n  - {}: {}", symb_nt.get(&v).unwrap(), if t.is_empty() { String::new() } else { t.to_str(None, Some(&self.symbol_table)) })
                 ).join(""))?;
        writeln!(f, "  symbols:\n  - NT: {}\n  - T : {}",
                 symb_nt.into_iter().map(|(nt, s)| format!("{nt}={s}")).join(", "), symb_t.into_iter().map(|(t, s)| format!("{t}={s}")).join(", "))?;
        writeln!(f, "  start_nt: {:?}", self.start_nt)?;
        writeln!(f, "  num_nt: {}", self.num_nt)?;
        writeln!(f, "  nt_reserved: {}", self.nt_reserved.iter().map(|(n, v)| format!("{n}={v}")).join(", "))?;
        writeln!(f, "  symbol_table:\n{}", self.symbol_table.dump_str())?;
        writeln!(f, "}}")
    }
}

impl GramParserListener for GramListener<'_> {
    fn check_abort_request(&self) -> Terminate {
        self.abort
    }

    fn get_log_mut(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    // file:
    //     header rules
    // ;
    fn exit_file(&mut self, _ctx: CtxFile, _spans: Vec<PosSpan>) -> SynFile {
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
            for mut node in rule.iter_post_depth_simple_mut() {
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
    fn exit_header(&mut self, ctx: CtxHeader, _spans: Vec<PosSpan>) -> SynHeader {
        if self.verbose { println!("- exit_header({ctx:?}"); }
        let CtxHeader::V1 { id } = ctx;
        self.name = id;
        SynHeader()
    }

    // rules:
    //     rule
    // |   rules rule
    // ;
    fn exit_rules(&mut self, _ctx: CtxRules, _spans: Vec<PosSpan>) -> SynRules {
        if self.verbose { println!("exit_rules({_ctx:?})"); }
        SynRules()
    }

    fn init_rule(&mut self) {
        if self.verbose { println!("init_rule()"); }
        assert!(self.curr.is_none(), "remnant tree in self.curr: {self:?}");
        self.curr = Some(GrTree::new());
    }

    // rule:
    //     rule_name Colon prod SymEof? Semicolon
    // ;
    fn exit_rule(&mut self, ctx: CtxRule, _spans: Vec<PosSpan>) -> SynRule {
        if self.verbose { println!("exit_rule({ctx:?})"); }
        let mut tree = self.curr.take().expect("self.curr should have a tree");
        let curr_nt = self.curr_nt.take().unwrap();
        let (id, lform) = match ctx {
            CtxRule::V1 { prod: SynProd(id, lform), .. } => {        // rule -> rule_name : prod EOF ;
                if curr_nt > 0 {
                    self.log.add_error(format!("rule '{}': EOF can only be put in the top rule", self.curr_name.as_ref().unwrap()));
                }
                // we don't add Symbol::End to the tree because it's not necessary nor, in fact, even allowed)
                (id, lform)
            }
            CtxRule::V2 { prod: SynProd(id, lform), .. } => (id, lform),      // rule -> rule_name : prod ;
        };
        if lform.is_some() {
            self.stack_lform.pop();
        }
        tree.set_root(id);
        self.rules.push(tree);
        if self.rules.len() < self.num_nt {
            self.rules.resize(self.num_nt, VecTree::new());
        }
        self.curr_name = None;
        SynRule()
    }

    // rule_name:
    //     Id
    // ;
    fn exit_rule_name(&mut self, ctx: CtxRuleName, _spans: Vec<PosSpan>) -> SynRuleName {
        if self.verbose { println!("exit_rule_name({ctx:?})"); }
        let CtxRuleName::V1 { id: name } = ctx;
        self.curr_name = Some(name.clone());
        let Some(nt) = self.add_nt_symbol(&name) else {
            self.abort = Terminate::Abort;
            return SynRuleName(String::new());
        };
        self.curr_nt = Some(nt);
        if self.start_nt.is_none() {
            // the start rule is the first to be defined
            self.start_nt = Some(nt);
        }
        SynRuleName(name)
    }

    // prod:
    //     prod_alt
    // |   prod Or prod_alt
    // ;
    fn exit_prod(&mut self, ctx: CtxProd, _spans: Vec<PosSpan>) -> SynProd {
        if self.verbose { println!("exit_prod({ctx:?})"); }
        let tree = self.curr.as_mut().expect("no current tree");
        let (id, lform) = match ctx {
            CtxProd::V1 { prod_alt: SynProdAlt(id, lform) } => (id, lform),           // first iteration
            CtxProd::V2 { prod: SynProd(ip, lp), prod_alt: SynProdAlt(i, l) } => {  // next iterations
                let id = if matches!(tree.get(ip), &GrNode::Or) {
                    // if there's already an |, adds another child
                    tree.attach_child(ip, i);
                    ip
                } else {
                    // creates an | with the previous and current alternatives as children
                    tree.addci_iter(None, GrNode::Or, [ip, i])
                };
                let lform = if let Some(lp_var) = lp {
                    if let Some(l_var) = l {
                        let span = self.stack_lform.pop().unwrap();
                        self.log_error(&span, &format!(
                            "extra <L={}>, <L={}> was already declared in this scope",
                            Symbol::NT(l_var).to_str(Some(self.get_symbol_table())),
                            Symbol::NT(lp_var).to_str(Some(self.get_symbol_table()))));
                    }
                    lp
                } else {
                    l
                };
                (id, lform)
            }
        };
        SynProd(id, lform)
    }

    // prod_alt:
    //     prod_factor*
    // ;
    fn exit_prod_alt(&mut self, ctx: CtxProdAlt, _spans: Vec<PosSpan>) -> SynProdAlt {
        if self.verbose { println!("exit_prod_alt({ctx:?})"); }
        let tree = self.curr.as_mut().expect("no current tree");
        let CtxProdAlt::V1 { star: SynProdAlt1(factors) } = ctx;
        let mut lforms = vec![];
        let pt = factors.into_iter().map(|SynProdFactor(t, lform)| {
            if let Some(lf_var) = lform { lforms.push(lf_var) }
            t
        }).to_vec();
        let id = match pt.len() {
            0 => tree.add(None, GrNode::Symbol(Symbol::Empty)),
            1 => pt[0],
            _ => tree.addci_iter(None, GrNode::Concat, pt)
        };
        let lform = match lforms.len() {
            0 | 1 => lforms.pop(),
            nl => {
                // self.stack_lform has 5 items, (<L=i1> <L=i2> <L=i3> A)* -> n = 5, nl = 3
                // we take last two => (n + 1 - nl = 3)..
                let n = self.stack_lform.len();
                let lform_var = lforms.remove(0);
                let lform_spans = self.stack_lform.drain((n + 1 - nl)..).to_vec();
                let at_text = lform_spans.iter().map(|s| s.to_string()).join(", ");
                let annot_text = lform_spans.into_iter().map(|s| self.annotate(&s)).join("\n");
                self.log.add_error(format!(
                    "at {at_text}, extra <L>: <L={}> was already declared in this scope:\n\n{annot_text}\n",
                    Symbol::NT(lform_var).to_str(Some(self.get_symbol_table()))
                ));
                Some(lform_var)
            }
        };
        SynProdAlt(id, lform)
    }

    // prod_factor:
    //     prod_atom (Plus | Star | Question)?
    // ;
    fn exit_prod_factor(&mut self, ctx: CtxProdFactor, _spans: Vec<PosSpan>) -> SynProdFactor {
        if self.verbose { println!("exit_prod_factor_rep({ctx:?})"); }
        let tree = self.curr.as_mut().expect("no current tree");
        let (id, l_check, mut lform) = match ctx {
            CtxProdFactor::V1 { prod_atom: SynProdAtom(factor_item, lform) } => (tree.addci(None, GrNode::Plus, factor_item), true, lform),   // prodAtom +
            CtxProdFactor::V2 { prod_atom: SynProdAtom(factor_item, lform) } => (tree.addci(None, GrNode::Star, factor_item), true, lform),   // prodAtom *
            CtxProdFactor::V3 { prod_atom: SynProdAtom(factor_item, lform) } => (tree.addci(None, GrNode::Maybe, factor_item), false, lform), // prodAtom ?
            CtxProdFactor::V4 { prod_atom: SynProdAtom(factor_item, lform) } => (factor_item, false, lform),                                  // prodAtom
        };
        if l_check {
            if let Some(lform_var) = lform {
                let span = self.stack_lform.pop().unwrap();
                if lform_var == self.curr_nt.unwrap() {
                    self.log_error(&span, &format!("<L={}> uses the rule nonterminal instead of a new one for the loop", self.curr_name.as_ref().unwrap()));
                }
            }
            lform = None;
        }
        SynProdFactor(id, lform)
    }

    // prod_atom:
    //     Id
    // |   Lform
    // |   Rform
    // |   Pform
    // |   Greedy
    // |   Lparen prod Rparen
    // ;
    fn exit_prod_atom(&mut self, ctx: CtxProdAtom, spans: Vec<PosSpan>) -> SynProdAtom {
        if self.verbose { println!("exit_prod_atom({ctx:?})"); }
        let (id, lform) = match ctx {
            CtxProdAtom::V1 { id } => {                  // prod_atom -> Id
                match self.symbols.get(&id) {
                    Some(s @ Symbol::NT(_)) |
                    Some(s @ Symbol::T(_)) => (self.curr.as_mut().unwrap().add(None, GrNode::Symbol(*s)), None),
                    Some(unexpected) => panic!("unexpected symbol: {unexpected:?}"),
                    None => {
                        // reserve new NT
                        if let Some(nt) = self.reserve_nt_symbol(id) {
                            (self.curr.as_mut().unwrap().add(None, GrNode::Symbol(Symbol::NT(nt))), None)
                        } else {
                            // failure
                            self.abort = Terminate::Abort;
                            return SynProdAtom(0, None /* don't care */);
                        }
                    }
                }
            }
            CtxProdAtom::V2 { lform } => {               // prod_atom -> Lform
                let name_maybe = if lform.len() > 3 {
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
                        self.abort = Terminate::Abort;
                        return SynProdAtom(0, None /* don't care */);
                    } else if self.nt_reserved.contains_key(&name) {
                        self.log.add_error(format!("rule {}: the rule name in <L={name}> has already been used as non-terminal in a rule",
                                                   self.curr_name.as_ref().unwrap()));
                        self.abort = Terminate::Abort;
                        return SynProdAtom(0, None /* don't care */);
                    }
                    match self.add_nt_symbol(&name) {
                        Some(nt) => nt,
                        None => {
                            self.abort = Terminate::Abort;
                            return SynProdAtom(0, None /* don't care */)
                        }
                    }
                } else {
                    // this form is used with right-recursive rules, so it points to the current rule
                    self.curr_nt.expect("curr_nt must be defined")
                };
                self.stack_lform.push(spans[0].clone());
                (self.curr.as_mut().unwrap().add(None, GrNode::LForm(nt)), Some(nt))
            }
            CtxProdAtom::V3 => {                         // prod_atom -> <R>
                (self.curr.as_mut().unwrap().add(None, GrNode::RAssoc), None)
            }
            CtxProdAtom::V4 => {                         // prod_atom -> <P>
                (self.curr.as_mut().unwrap().add(None, GrNode::PrecEq), None)
            }
            CtxProdAtom::V5 => {                        // prod_atom -> "<G>"
                (self.curr.as_mut().unwrap().add(None, GrNode::Greedy), None)
            }
            CtxProdAtom::V6 { prod: SynProd(id, lform) } => (id, lform),  // prod_atom -> ( prod )
        };
        SynProdAtom(id, lform)
    }
}

