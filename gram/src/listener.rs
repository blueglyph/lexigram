// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::fmt::{Debug, Formatter};
use lexigram::CollectJoin;
use lexigram::log::{BufLog, Logger};
use crate::gramparser::gramparser::*;
use crate::gramparser::gramparser_types::*;

pub struct GramListener {
    verbose: bool,
    name: String,
    log: BufLog,
}

impl GramListener {
    pub fn new() -> Self {
        GramListener {
            verbose: false,
            name: String::new(),
            log: BufLog::new(),
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
}

impl Debug for GramListener {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GramListener {{")?;
        writeln!(f, "  name = {}", self.name)?;
        writeln!(f, "  log:{}", self.log.get_messages().map(|s| format!("\n    - {s:?}")).join(""))?;
        writeln!(f, "}}")
    }
}

impl GramParserListener for GramListener {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }

    fn exit_file(&mut self, _ctx: CtxFile) -> SynFile {
        SynFile()
    }

    fn exit_header(&mut self, _ctx: CtxHeader) -> SynHeader {
        SynHeader()
    }

    fn exit_rules(&mut self, _ctx: CtxRules) -> SynRules {
        SynRules()
    }

    fn exit_rule(&mut self, _ctx: CtxRule) -> SynRule {
        SynRule()
    }

    fn exit_prod(&mut self, _ctx: CtxProd) -> SynProd {
        SynProd()
    }

    fn exit_prod_factor(&mut self, _ctx: CtxProdFactor) -> SynProdFactor {
        SynProdFactor()
    }

    fn exit_prod_term(&mut self, _ctx: CtxProdTerm) -> SynProdTerm {
        SynProdTerm()
    }

    fn exit_term_item(&mut self, _ctx: CtxTermItem) -> SynTermItem {
        SynTermItem()
    }
}
