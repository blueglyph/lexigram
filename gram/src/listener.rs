// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use lexigram::{CollectJoin, General};
use lexigram::grammar::{GrTree, GrNode, GrTreeExt, RuleTreeSet, Symbol, VarId};
use lexigram::log::{BufLog, Logger};
use lexigram::symbol_table::SymbolTable;
use crate::gramparser::gramparser::*;
use crate::gramparser::gramparser_types::*;

pub struct GramListener {
    verbose: bool,
    name: String,
    log: BufLog,
    curr: Option<GrTree>,
    curr_has_or: bool,
    rules: RuleTreeSet<General>,
    symbols: HashMap<String, Symbol>,
    nt_reserved: HashMap<String, VarId>,
    num_nt: VarId,
    num_t: VarId,
}

impl GramListener {
    pub fn new(symbol_table: SymbolTable) -> Self {
        let mut rules = RuleTreeSet::new();
        rules.set_symbol_table(symbol_table);
        GramListener {
            verbose: false,
            name: String::new(),
            log: BufLog::new(),
            curr: None,
            curr_has_or: false,
            rules,
            symbols: HashMap::new(),
            nt_reserved: HashMap::new(),
            num_nt: 0,
            num_t: 0,
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

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        self.rules.get_symbol_table()
    }

}

impl Debug for GramListener {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GramListener {{")?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  log:{}", self.log.get_messages().map(|s| format!("\n    - {s:?}")).join(""))?;
        writeln!(f, "  curr: {}", if let Some(t) = &self.curr { format!("{t:?}") } else { "none".to_string() })?;
        let symb_nt = self.symbols.iter().filter_map(|(name, s)| if let Symbol::NT(nt) = s { Some((nt, name)) } else { None }).collect::<BTreeMap<_, _>>();
        let symb_t = self.symbols.iter().filter_map(|(name, s)| if let Symbol::T(t) = s { Some((t, name)) } else { None }).collect::<BTreeMap<_, _>>();
        writeln!(f, "  rules:{}",
                 self.rules.get_trees_iter().map(|(v, t)|
                     format!("\n  - {}: {}", symb_nt.get(&v).unwrap(), t.to_str(None, self.get_symbol_table()))
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
    fn exit_rules(&mut self, ctx: CtxRules) -> SynRules {
        match ctx {
            CtxRules::Rules1 { rule } => {}
            CtxRules::Rules2 { rules, rule } => {}
            CtxRules::Rules3 { rules } => { /* end of iterations */ }
        }
        SynRules()
    }

    // rule:
    //     Id Colon prod Semicolon
    // ;
    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule {
        SynRule()
    }

    fn init_prod(&mut self) {
        assert!(self.curr.is_none(), "remnant tree in self.curr: {self:?}");
        self.curr = Some(GrTree::new());
        self.curr_has_or = false;
    }

    // prod:
    //     prodFactor
    // |   prod Or prodFactor
    // ;
    fn exit_prod(&mut self, ctx: CtxProd) -> SynProd {
        match ctx {
            CtxProd::Prod1 { prod_factor } => {}
            CtxProd::Prod2 { prod, prod_factor } => {}
            CtxProd::Prod3 { prod } => { /* end of iterations */ }
        }
        SynProd(todo!())
    }

    // prodFactor:
    //     prodTerm*
    // ;
    fn exit_prod_factor(&mut self, _ctx: CtxProdFactor) -> SynProdFactor {
        SynProdFactor(todo!())
    }

    // prodTerm:
    //     termItem (Plus | Star | Question)?
    // ;
    fn exit_prod_term(&mut self, _ctx: CtxProdTerm) -> SynProdTerm {
        SynProdTerm(todo!())
    }

    // termItem:
    //     Id
    // |   Lform
    // |   Rform
    // |   Lparen prod Rparen
    // ;
    fn exit_term_item(&mut self, ctx: CtxTermItem) -> SynTermItem {
        let mut tree = self.curr.as_mut().expect("no current tree");
        let id = match ctx {
            CtxTermItem::TermItem1 { id } => {
                match self.symbols.get(&id) {
                    Some(s @ Symbol::NT(_)) |
                    Some(s @ Symbol::T(_)) => tree.add(None, GrNode::Symbol(*s)),
                    Some(unexpected) => panic!("unexpected symbol: {unexpected:?}"),
                    None => {
                        // new NT
                        let nt = *self.nt_reserved.entry(id).or_insert(VarId::MAX - self.nt_reserved.len().try_into().expect("too many reserved symbols"));
                        tree.add(None, GrNode::Symbol(Symbol::NT(nt)))
                    }
                }
            }
            CtxTermItem::TermItem2 { lform } => {
                tree.add(None, GrNode::LForm(todo!()))
            }
            CtxTermItem::TermItem3 => {}
            CtxTermItem::TermItem4 { prod } => {}
        };
        SynTermItem(id)
    }
}

