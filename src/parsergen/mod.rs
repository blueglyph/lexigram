#![allow(dead_code)]  // TODO: remove

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use crate::grammar::{LLParsingTable, ProdRuleSet, ruleflag, RuleTreeSet, Symbol, VarId, FactorId};
use crate::{CollectJoin, General, LL1, Normalized};
use crate::parser::{OpCode, Parser};
use crate::symbol_table::SymbolTable;

mod tests;

// ---------------------------------------------------------------------------------------------

pub(crate) fn symbol_to_code(s: &Symbol) -> String {
    match s {
        Symbol::Empty => "Symbol::Empty".to_string(),
        Symbol::T(t) => format!("Symbol::T({t})"),
        Symbol::NT(nt) => format!("Symbol::NT({nt})"),
        Symbol::End => "Symbol::End".to_string(),
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Empty => write!(f, "ε"),
            OpCode::T(t) => write!(f, ":{t}"),
            OpCode::NT(v) => write!(f, "►{v}"),
            OpCode::Loop(v) => write!(f, "●{v}"),
            OpCode::Exit(v) => write!(f, "◄{v}"),
            OpCode::End => write!(f, "$"),
        }
    }
}

impl OpCode {
    pub fn is_loop(&self) -> bool {
        matches!(self, OpCode::Loop(_))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, OpCode::Empty)
    }

    pub fn matches(&self, s: Symbol) -> bool {
        match self {
            OpCode::Empty => s == Symbol::Empty,
            OpCode::T(t) => s == Symbol::T(*t),
            OpCode::NT(v) => s == Symbol::NT(*v),
            OpCode::End => s == Symbol::End,
            OpCode::Loop(_) => false,
            OpCode::Exit(_) => false,
        }
    }

    pub fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
        if let Some(t) = symbol_table {
            match self {
                OpCode::Empty => "ε".to_string(),
                OpCode::T(v) => format!("{}{}", t.get_t_name(*v), if t.is_t_data(*v) { "!" } else { "" }),
                OpCode::NT(v) => format!("►{}", t.get_nt_name(*v)),
                OpCode::Loop(v) => format!("●{}", t.get_nt_name(*v)),
                OpCode::Exit(f) => format!("◄{f}"),
                OpCode::End => "$".to_string(),
            }
        } else {
            self.to_string()
        }
    }

    pub fn to_str_ext(&self, symbol_table: Option<&SymbolTable>, ext: &String) -> String {
        let mut result = self.to_str(symbol_table);
        if let Some(_) = symbol_table {
            if let OpCode::T(_) = self {
                result.push_str(&format!("({ext})"));
            }
        }
        result
    }

    fn to_symbol(&self) -> Option<Symbol> {
        match self {
            OpCode::Empty => Some(Symbol::Empty),
            OpCode::T(t) => Some(Symbol::T(*t)),
            OpCode::NT(v) => Some(Symbol::NT(*v)),
            OpCode::End => Some(Symbol::End),
            OpCode::Loop(v) => Some(Symbol::NT(*v)),
            OpCode::Exit(_) => None,
        }
    }
}

impl From<Symbol> for OpCode {
    fn from(value: Symbol) -> Self {
        match value {
            Symbol::Empty => OpCode::Empty,
            Symbol::T(t) => OpCode::T(t),
            Symbol::NT(v) => OpCode::NT(v),
            Symbol::End => OpCode::End,
        }
    }
}

// ---------------------------------------------------------------------------------------------

#[allow(unused)]
pub struct ParserBuilder {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
    name: String,
    nt_value: Vec<bool>,
    opcodes: Vec<Vec<OpCode>>,
    start: VarId
}

impl ParserBuilder {
    pub fn from_tree(tree: RuleTreeSet<General>, name: String) -> Self {
        let normalized = RuleTreeSet::<Normalized>::from(tree);
        let lr_rules = ProdRuleSet::from(normalized);
        Self::from_rules(lr_rules, name)
    }

    pub fn from_rules<T>(rules: ProdRuleSet<T>, name: String) -> Self where ProdRuleSet<LL1>: From<ProdRuleSet<T>>, T: std::fmt::Debug {
        let mut ll1_rules = ProdRuleSet::<LL1>::from(rules);
        let num_nt = ll1_rules.get_num_nt();
        let start = ll1_rules.get_start().unwrap();
        let parsing_table = ll1_rules.create_parsing_table();
        let symbol_table = ll1_rules.symbol_table().expect(stringify!("symbol table is requires to create a {}", std::any::type_name::<Self>()));
        let mut builder = ParserBuilder {
            parsing_table,
            symbol_table,
            name,
            nt_value: vec![false; num_nt],
            opcodes: Vec::new(),
            start
        };
        builder.build_opcodes();
        builder
    }

    #[inline]
    fn nt_has_flags(&self, var: VarId, flags: u32) -> bool {
        self.parsing_table.flags[var as usize] & flags == flags
    }

    #[inline]
    fn sym_has_flags(&self, s: &Symbol, flags: u32) -> bool {
        if let Symbol::NT(nt) = s { self.nt_has_flags(*nt, flags) } else { false }
    }

    #[inline]
    fn sym_has_value(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::T(t) => self.symbol_table.is_t_data(*t),
            Symbol::NT(nt) => self.nt_value[*nt as usize],
            _ => false
        }
    }

    pub fn get_symbol_table(&self) -> Option<&SymbolTable> {
        Some(&self.symbol_table)
    }

    #[cfg(for_later)]
    /// Number of data terminals in factor `f`
    fn num_t_data(&self, f: VarId) -> usize {
        self.factors[f as usize].1.iter().filter(|s| self.symbol_table.is_symbol_t_data(s)).count()
    }

    #[cfg(for_later)]
    /// Number of data terminals for factors
    fn get_num_stack(&self, factor_id: VarId) -> usize {
        const VERBOSE: bool = false;
        let var_id = self.factors[factor_id as usize].0;
        let flags = self.flags[var_id as usize];

        // todo!("Normally, this should be precompiled before inspecting all the factors:")
        let mut var_factors = HashMap::<VarId, (VarId, VarId)>::new();
        for (factor_id, (var_id, factor)) in self.factors.iter().enumerate() {
            if VERBOSE {
                println!("{} -> {}",
                         Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                         factor.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "));
            }
            if let Some((a, b)) = var_factors.get_mut(var_id) {
                *b = factor_id as VarId;
            } else {
                var_factors.insert(*var_id, (factor_id as VarId, factor_id as VarId));
            }
        }

        let mut num_stack = self.num_t_data(factor_id);
        let mut fl = flags;
        let mut child_var = var_id;
        while fl & ruleflag::CHILD_L_FACTOR != 0 {
            let par_var = self.parent[child_var as usize].unwrap();
            fl = self.flags[par_var as usize];
            let factors = var_factors[&par_var];
            if VERBOSE {
                println!("  // child var {}: parent var {} has factors {} to {}",
                         Symbol::NT(child_var).to_str(self.get_symbol_table()),
                         Symbol::NT(par_var).to_str(self.get_symbol_table()), factors.0, factors.1);
            }
            let calling_factor = (factors.0 ..= factors.1).find(|f| {
                let ok = self.factors[*f as usize].1.iter().any(|s| s == &Symbol::NT(child_var));
                if VERBOSE {
                    println!("  // is factor {f} calling {}? {ok}: {}", Symbol::NT(child_var).to_str(self.get_symbol_table()),
                        self.factors[*f as usize].1.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")
                    )
                }
                ok
            }).unwrap();
            if VERBOSE {
                println!("  - var {} ({child_var}) is called by factor {calling_factor} -> num_stack = {num_stack} + {}",
                         Symbol::NT(par_var).to_str(self.get_symbol_table()),
                         self.num_t_data(calling_factor));
            }
            num_stack += self.num_t_data(calling_factor);
            child_var = par_var;
        }
        num_stack
    }

    fn build_opcodes(&mut self) {
        const VERBOSE: bool = false;
        for (factor_id, (var_id, factor)) in self.parsing_table.factors.iter().enumerate() {
            if VERBOSE {
                println!("{} -> {}",
                         Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                         factor.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "));
            }
            let factor_id = factor_id as FactorId;
            let flags = self.parsing_table.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.parsing_table.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            if VERBOSE { println!("- {}", new.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut opcode = Vec::<OpCode>::new();
            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                let parent = self.parsing_table.parent[*var_id as usize].unwrap();
                // replaces Enter by Loop when going back to left-factorization parent, typically when coupled with + or *
                // (per construction, there can't be any factor going back to the grand-parent or further up in a left factorization, so
                //  we don't check that)
                if new.get(0) == Some(&Symbol::NT(parent)) {
                    opcode.push(OpCode::Loop(parent));
                    new.remove(0);
                }
            }
            if flags & ruleflag::PARENT_L_FACTOR == 0 || new.iter().all(|s| if let Symbol::NT(ch) = s { !self.nt_has_flags(*ch, ruleflag::CHILD_L_FACTOR) } else { true }) {
                // if it's not a parent of left factorization, or
                // if none of the NT in the factor is a child of left factorization,
                // => adds an Exit
                // (said otherwise: we don't want an exit in a parent or in the middle of a chain of left factorizations;
                //  the exit should be only at the end of left factorizations, or in factors that aren't left-factorized)
                opcode.push(OpCode::Exit(factor_id)); // will be popped when this NT is completed
            }
            opcode.extend(new.into_iter().map(|s| OpCode::from(s)));
            let r_form_right_rec = flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
            if opcode.get(1).map(|op| op.matches(stack_sym)).unwrap_or(false) && !r_form_right_rec {
                // swaps Exit(self) when it's in 2nd position (only happens in [Loop(_), Exit(self), ...],
                // except right recursions that aren't left-form, because we let them unfold naturally (uses more stack)
                opcode.swap(0, 1);
            }
            opcode.iter_mut().for_each(|o| {
                if let OpCode::NT(v) = o {
                    // replaces Enter by Loop when back to self,
                    // except right recursions that aren't left-form, because we let them unfold naturally (uses more stack)
                    if v == var_id && !r_form_right_rec {
                        *o = OpCode::Loop(*v)
                    }
                }
            });
            if VERBOSE { println!("- {}", opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            self.opcodes.push(opcode);
        }
    }

    pub fn make_parser(self) -> Parser {
        Parser::new(self.parsing_table, self.symbol_table, self.opcodes, self.start)
    }

    // Building the source code as we do below is not the most efficient, but it's done that way to
    // - be able to build only a part of the parser, and
    // - get the sources for the validation tests or print them / write them into a file.
    // The whole code isn't that big, so it's not a major issue.

    pub fn write_source_code(&mut self, file: Option<File>, indent: usize) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };
        let source = self.build_source_code(indent, true);
        out.write(source.as_bytes())?;
        // write!(out, "{source}");
        Ok(())
    }

    fn build_source_code(&mut self, indent: usize, wrapper: bool) -> String {
        let s = String::from_utf8(vec![32; indent]).unwrap();
        let mut parts = vec![];
        parts.push(vec![
            "// -------------------------------------------------------------------------".to_string(),
            "// Automatically generated".to_string(),
        ]);
        parts.push(self.source_build_parser());
        if wrapper {
            let items = self.build_item_ops();
            parts.push(self.source_wrapper(items));
        }
        parts.push(vec![
            "// -------------------------------------------------------------------------".to_string(),
        ]);
        // Create source code:
        let mut source = String::new();
        let mut first = true;
        for part in parts {
            if !first {
                source.push('\n');
            }
            first = false;
            for line in part {
                source.push_str(&s);
                source.push_str(&line);
                source.push('\n');
            }
        }
        source
    }

    fn source_build_parser(&self) -> Vec<String> {
        let num_nt = self.symbol_table.get_non_terminals().len();
        let num_t = self.symbol_table.get_terminals().len();
        let mut symbol_names = self.symbol_table.get_names().to_vec(); // hashmap: we want predictable outcome, so we sort names
        symbol_names.sort();
        vec![
            format!("use rlexer::grammar::{{ProdFactor, Symbol, VarId, FactorId}};"),
            format!("use rlexer::parser::{{OpCode, Parser}};"),
            format!("use rlexer::symbol_table::SymbolTable;\n"),

            format!("const PARSER_NUM_T: usize = {num_t};"),
            format!("const PARSER_NUM_NT: usize = {num_nt};"),
            format!("const SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [{}];",
                     self.symbol_table.get_terminals().iter().map(|(s, os)|
                         format!("(\"{s}\", {})", os.as_ref().map(|s| format!("Some(\"{s}\")")).unwrap_or("None".to_string()))).join(", ")),
            format!("const SYMBOLS_NT: [&str; PARSER_NUM_NT] = [{}];",
                     self.symbol_table.get_non_terminals().iter().map(|s| format!("\"{s}\"")).join(", ")),
            format!("const SYMBOLS_NAMES: [(&str, VarId); {}] = [{}];",
                     symbol_names.len(),
                     symbol_names.into_iter().map(|(s, v)| format!("(\"{s}\", {v})")).join(", ")),
            format!("const PARSING_FACTORS: [(VarId, &[Symbol]); {}] = [{}];",
                     self.parsing_table.factors.len(),
                     self.parsing_table.factors.iter().map(|(v, f)| format!("({v}, &[{}])", f.iter().map(|s| symbol_to_code(s)).join(", "))).join(", ")),
            format!("const PARSING_TABLE: [FactorId; {}] = [{}];",
                     self.parsing_table.table.len(),
                     self.parsing_table.table.iter().map(|v| format!("{v}")).join(", ")),
            format!("const FLAGS: [u32; {}] = [{}];",
                     self.parsing_table.flags.len(), self.parsing_table.flags.iter().join(", ")),
            format!("const PARENT: [Option<VarId>; {}] = [{}];",
                     self.parsing_table.parent.len(), self.parsing_table.parent.iter().map(|p| if let Some(par) = p { format!("Some({par})") } else { format!("None") }).join(", ")),
            format!("const OPCODES: [&[OpCode]; {}] = [{}];", self.opcodes.len(),
                     self.opcodes.iter().map(|strip| format!("&[{}]", strip.into_iter().map(|op| format!("OpCode::{op:?}")).join(", "))).join(", ")),
            format!("const START_SYMBOL: VarId = {};\n", self.start),

            format!("pub(super) fn build_parser() -> Parser {{"),
            format!("    let mut symbol_table = SymbolTable::new();"),
            format!("    symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));"),
            format!("    symbol_table.extend_non_terminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));"),
            format!("    symbol_table.extend_names(SYMBOLS_NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));"),
            format!("    let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();"),
            format!("    let table: Vec<FactorId> = PARSING_TABLE.into();"),
            format!("    let parsing_table = rlexer::grammar::LLParsingTable {{"),
            format!("        num_nt: PARSER_NUM_NT,"),
            format!("        num_t: PARSER_NUM_T + 1,"),
            format!("        factors,"),
            format!("        table,"),
            format!("        flags: FLAGS.into(),"),
            format!("        parent: PARENT.into(),"),
            format!("    }};"),
            format!("    Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)"),
            format!("}}"),
        ]
    }

    fn build_item_ops(&mut self) -> HashMap::<FactorId, Vec<Symbol>> {
        const VERBOSE: bool = true;
        let info = &self.parsing_table;
        let mut items = HashMap::<FactorId, Vec<Symbol>>::new();
        let mut var_factors: Vec::<Vec<FactorId>> = vec![vec![]; info.num_nt];
        for (factor_id, (var_id, _)) in info.factors.iter().enumerate() {
            var_factors[*var_id as usize].push(factor_id as FactorId);
            items.insert(factor_id as FactorId, vec![]);
        }
        for nt in 0..info.num_nt {
            if self.parsing_table.flags[nt] & ruleflag::CHILD_REPEAT != 0 {
                if let Some(parent) = self.parsing_table.parent[nt] {
                    // parent has value and child has any data?
                    if self.nt_value[parent as usize] && info.factors[var_factors[nt][0] as usize].1.iter().any(|s| { self.sym_has_value(s) }) {
                        self.nt_value[nt] = true;
                    }
                }
            }
        }
        for (factor_id, opcode) in self.opcodes.iter().enumerate() {
            let (var_id, factor) = &info.factors[factor_id];
            if VERBOSE {
                print!("- {factor_id}: {} -> {}   [{}]",
                         Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                         factor.to_str(self.get_symbol_table()),
                         opcode.iter().map(|op| op.to_str(self.get_symbol_table())).join(" "));
            }
            let factor_id = factor_id as FactorId;
            let flags = info.flags[*var_id as usize];
            // Default values are taken from opcodes. Loop(nt) is only taken if the parent is l-rec;
            // we look at the parent's flags instead of the factor's because left factorization could
            // displace the Loop(nt) to another non-l-rec child factor.
            let mut values = self.opcodes[factor_id as usize].iter().rev()
                .filter_map(|s| {
                    let sym_maybe = match s {
                        OpCode::T(t) => Some(Symbol::T(*t)),
                        OpCode::NT(nt) => Some(Symbol::NT(*nt)),
                        /*
                        OpCode::Loop(nt) => {
                            if let Some(parent) = info.parent[*nt as usize] {
                                if self.nt_has_flags(parent, ruleflag::PARENT_L_RECURSION) {
                                    if VERBOSE { print!("| {} -> {}", s.to_str(self.get_symbol_table()), Symbol::NT(parent).to_str(self.get_symbol_table())); }
                                    Some(Symbol::NT(parent))
                                } else {
                                    if VERBOSE { print!("| {} dropped", s.to_str(self.get_symbol_table())); }
                                    None
                                }
                            } else {
                                if VERBOSE { print!("| {} dropped", s.to_str(self.get_symbol_table())); }
                                None
                            }
                        }
                        */
                        _ => {
                            if VERBOSE { print!("| {} dropped", s.to_str(self.get_symbol_table())); }
                            None
                        }
                    };
                    sym_maybe.and_then(|s| if self.sym_has_value(&s) { Some(s) } else { None })
                }).to_vec();

            if !values.is_empty() {
                // Loop NTs which carry values are kept on the stack, too
                let sym_maybe = if flags & ruleflag::CHILD_REPEAT != 0 {
                    Some(Symbol::NT(*var_id))
                } else if flags & ruleflag::CHILD_L_RECURSION != 0 {
                    let parent = info.parent[*var_id as usize].unwrap();
                    Some(Symbol::NT(parent))
                } else {
                    None
                };
                if let Some(s) = sym_maybe {
                    if self.sym_has_value(&s) {
                        if VERBOSE { print!("| loop => {}", s.to_str(self.get_symbol_table())); }
                        values.insert(0, s);
                    }
                }
            }

            if flags & (ruleflag::R_RECURSION | ruleflag::L_FORM) == ruleflag::R_RECURSION | ruleflag::L_FORM  {
                if VERBOSE { print!("| <L> r-rec => {}", Symbol::NT(*var_id).to_str(self.get_symbol_table())); }
                values.insert(0, Symbol::NT(*var_id));
            } /*else if flags & ruleflag::CHILD_REPEAT != 0 && !values.is_empty() {
                // all forms need to update the loop item, except on the last iteration of * because it's empty
                if VERBOSE { print!("| <L> child * + => {}", Symbol::NT(*var_id).to_str(self.get_symbol_table())); }
                values.push(Symbol::NT(*var_id));
            }*/
            if VERBOSE { println!(" ==> [{}]", values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            if let Some(OpCode::NT(nt)) = opcode.first() {
                // Take the values except the last NT
                let backup = if matches!(values.last(), Some(Symbol::NT(x)) if x == nt) {
                    Some(values.pop().unwrap())
                } else {
                    None
                };
                if nt != var_id && self.nt_has_flags(*nt, ruleflag::CHILD_L_RECURSION) {
                    if VERBOSE { println!("  CHILD_L_RECURSION"); }
                    // exit_<var_id>(context = values) before entering child loop
                    items.get_mut(&factor_id).unwrap().extend(values);
                    continue;
                }
                if flags & ruleflag::PARENT_L_FACTOR != 0 {
                    if VERBOSE { println!("  PARENT_L_FACTOR: moving {} to child {}",
                                          values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                          Symbol::NT(*nt).to_str(self.get_symbol_table())); }
                    // factorization reports all the values to the children
                    if let Some(pre) = items.get_mut(&factor_id) {
                        // pre-pends values that already exist for factor_id (and empties factor_id)
                        values.splice(0..0, std::mem::take(pre));
                    }
                    for f_id in var_factors[*nt as usize].iter() {
                        items.get_mut(f_id).unwrap().extend(values.clone());
                    }
                    continue;
                }
                if let Some(sym) = backup {
                    values.push(sym);
                }
            }
            if let Some(current) = items.get_mut(&factor_id) {
                current.extend(values);
            } else {
                items.get_mut(&factor_id).unwrap().extend(values);
            }

        }
        items
    }

    #[allow(unused)]
    fn source_wrapper(&mut self, items: HashMap<FactorId, Vec<Symbol>>) -> Vec<String> {
        const VERBOSE: bool = true;

        #[derive(Debug)]
        struct ItemInfo {
            name: String,
            sym: Symbol,            // NT(var) or T(token)
            owner: VarId,           // NT owning this item; for ex. owner = `A` for `sym = b` in `A -> a b+ c`
            is_vec: bool,           // for ex. `b: Vec<String>` in `A -> a b+ c`
            index: Option<usize>    // when several identical symbols in the same factor: `A -> id := id ( id )`
        }

        impl ItemInfo {
            fn to_str(&self, symbol_table: Option<&SymbolTable>) -> String {
                format!("{} ({}{}{}, ◄{})",
                        self.name,
                        self.sym.to_str(symbol_table),
                        if self.is_vec { ", is_vec" } else { "" },
                        if let Some(n) = self.index { format!(", [{n}]") } else { "".to_string() },
                        Symbol::NT(self.owner).to_str(symbol_table))
            }
        }

        let pinfo = &self.parsing_table;
        let mut var_factors: Vec<Vec<FactorId>> = vec![vec![]; pinfo.num_nt];
        for (factor_id, (var_id, _)) in pinfo.factors.iter().enumerate() {
            var_factors[*var_id as usize].push(factor_id as FactorId);
        }

        let mut nt_upper_fixer = NameFixer::new();
        let mut nt_lower_fixer = NameFixer::new();
        let mut nt_name = (0..pinfo.num_nt).map(|v| if self.nt_value[v] && pinfo.parent[v].is_none() {
            let nu = nt_upper_fixer.get_unique_name(self.symbol_table.get_nt_name(v as VarId).to_camelcase());
            let nl = nt_lower_fixer.get_unique_name(nu.to_underscore());
            Some((nu, nl))
        } else {
            None
        }).to_vec();
        let mut nt_info: Vec<Vec<(FactorId, String)>> = vec![vec![]; pinfo.num_nt];

        let mut item_info: Vec<Vec<ItemInfo>> = (0..pinfo.factors.len()).map(|i| {
            let factor_id = i as FactorId;
            let nt = pinfo.factors[i].0 as usize;
            if let Some(item_ops) = items.get(&factor_id) {
                // Adds a suffix to the names of different symbols that would otherwise collide in the same context option:
                // - identical symbols are put in a vector (e.g. `id: [String; 2]`)
                // - different symbols, which means T vs NT, must have different names (e.g. `NT(A)` becomes "a",
                //   `T(a)` becomes "a", too => one is renamed to "a1" to avoid the collision: `{ a: SynA, a1: String }`)
                let mut indices = HashMap::<Symbol, (String, Option<usize>)>::new();
                let mut fixer = NameFixer::new();
                for s in item_ops {
                    if let Symbol::NT(v) = s {
                        if nt_name[*v as usize].is_none() {
                            let name = self.symbol_table.get_nt_name(*v);
                            nt_name[*v as usize] = Some((nt_upper_fixer.get_unique_name(name.to_camelcase()),
                                                         nt_lower_fixer.get_unique_name(name.to_underscore())));
                        }
                    }
                    if let Some((s, c)) = indices.get_mut(s) {
                        *c = Some(0);
                    } else {
                        let name = if let Symbol::NT(vs) = s {
                            let flag = pinfo.flags[*vs as usize];
                            if flag & ruleflag::CHILD_REPEAT != 0 {
                                let inside_factor_id = var_factors[*vs as usize][0];
                                let inside_factor = &pinfo.factors[inside_factor_id as usize].1;
                                let mut plus_name = inside_factor.symbols()[0].to_str(self.get_symbol_table()).to_underscore();
                                plus_name.push_str(if flag & ruleflag::REPEAT_PLUS != 0 { "_plus" } else { "_star" });
                                plus_name
                            } else {
                                nt_name[*vs as usize].clone().unwrap().1
                            }
                        } else {
                            s.to_str(self.get_symbol_table()).to_lowercase()
                        };
                        indices.insert(*s, (fixer.get_unique_name(name), None));
                    }
                }
                let mut owner = pinfo.factors[i].0;
                while let Some(parent) = pinfo.parent[owner as usize] {
                    if pinfo.flags[owner as usize] & ruleflag::CHILD_REPEAT != 0 {
                        // a child + * is owner
                        // - if <L>, it has its own public context and a user-defined return type
                        // - if not <L>, it has no context and a generator-defined return type (like Vec<String>)
                        // (we keep the loop for +, which has a left factorization, too)
                        break;
                    }
                    owner = parent;
                }
                if (!item_ops.is_empty() || (self.nt_value[nt] && pinfo.flags[nt] & ruleflag::R_RECURSION != 0)) &&
                    pinfo.flags[owner as usize] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) != ruleflag::CHILD_REPEAT
                {
                    let len = nt_info[owner as usize].len();
                    if len == 1 {
                        nt_info[owner as usize][0].1.push('1');
                    }
                    let mut name = Symbol::NT(owner).to_str(self.get_symbol_table()).to_camelcase();
                    if len > 0 { name.push_str(&(len + 1).to_string()) };
                    nt_info[owner as usize].push((factor_id, name));
                }
                item_ops.into_iter().map(|s| {
                    let index = if let Some((_, Some(index))) = indices.get_mut(s) {
                        let idx = *index;
                        *index += 1;
                        Some(idx)
                    } else {
                        None
                    };
                    ItemInfo {
                        name: indices[&s].0.clone(),
                        sym: s.clone(),
                        owner,
                        is_vec: false,
                        index,
                    }
                }).to_vec()
            } else {
                vec![]
            }
        }).to_vec();

        if VERBOSE {
            println!("NT names: {}", nt_name.iter()
                .filter_map(|n| if let Some((u, l)) = n { Some(format!("{u}/{l}")) } else { None })
                .join(", "));
            println!("NT info:");
            for (v, factor_names) in nt_info.iter().enumerate() {
                if !factor_names.is_empty() {
                    println!("- {}:", Symbol::NT(v as VarId).to_str(self.get_symbol_table()));
                    for f in factor_names {
                        // TODO: - fetch all parents' information to rebuild after factorization
                        //       - process other transformations
                        // let factor = &info.factors[f.0 as usize];
                        // println!("  - // {} -> {}",
                        //          Symbol::NT(factor.0).to_str(self.get_symbol_table()),
                        //          factor.1.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                        // );
                        println!("  - {}: {} {{ {} }}", f.0, f.1,
                            item_info[f.0 as usize].iter().map(|info| info.to_str(self.get_symbol_table())).join(", ")
                        );
                    }
                }
            }
            println!();
        }

        let mut src = vec![];

        // Writes contexts
        if let Some((nu, nl)) = &nt_name[self.start as usize] {
            src.push(format!("pub enum Ctx {{ {nu} {{ {nl}: Syn{nu} }} }}"));
        }
        for (v, factor_names) in nt_info.iter().enumerate().filter(|(_, f)| !f.is_empty()) {
            src.push(format!("pub enum Ctx{} {{", nt_name[v].clone().unwrap().0));
            for (f_id, f_name) in factor_names {
                let ctx_content = item_info[*f_id as usize].iter().filter_map(|info| {
                    if info.index.is_none() || info.index == Some(0) {
                        let type_name_base = match info.sym {
                            Symbol::T(t) => "String".to_string(),
                            Symbol::NT(vs) => format!("Syn{}", nt_name[vs as usize].clone().unwrap().0),
                            _ => panic!("unexpected symbol {}", info.sym)
                        };
                        let type_name = if info.index.is_some() {
                            let nbr = item_info[*f_id as usize].iter()
                                .map(|nfo| if nfo.sym == info.sym { nfo.index.unwrap() } else { 0 })
                                .max().unwrap() + 1;
                            format!("[{type_name_base}; {nbr}]")
                        } else {
                            type_name_base
                        };
                        Some(format!("{}: {}", info.name, type_name))
                    } else {
                        None
                    }
                }).join(", ");
                if ctx_content.is_empty() {
                    src.push(format!("    {f_name},", ))
                } else {
                    src.push(format!("    {f_name} {{ {ctx_content} }},", ))
                }
            }
            src.push(format!("}}"));
        }

        // Writes intermediate Syn types
        src.add_space();
        let mut user_names = vec![];
        for (v, name) in nt_name.iter().enumerate().filter_map(|(v, n)| if let Some(nm) = n { Some((v, &nm.0)) } else { None }) {
            if pinfo.flags[v] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {
                // TODO: <String> is only valid for a single T; there could be
                //       - several T/NT => tuple, like `Vec<(String, SynB, String)>`
                //                      => or struct, like `Vec<APlusItem { c: String, b: SynB, d: String }>`
                //       - alternative factors => enum
                //       Information on that format should be stored for each +/*.
                //       Another possibility is forcing a dedicated NT for what's inside the +/* (lot of changes).
                src.push(format!("struct Syn{}(Vec<String>);", name.clone()));
            } else {
                user_names.push(format!("Syn{name}"));
            }
        }
        if !user_names.is_empty() {
            src.push(format!("// User-defined: {}", user_names.join(", ")));
        }

        // Writes SynValue type and implementation
        src.add_space();
        let syns = nt_name.iter().filter_map(|(n)|
            if let Some((nu, nl)) = n { Some((nu.clone(), nl.clone())) } else { None }
        ).to_vec();
        // SynValue type
        src.push(format!("enum SynValue {{ {} }}", syns.iter().map(|(nu, _)| format!("{nu}(Syn{nu})")).join(", ")));
        if !syns.is_empty() {
            // SynValue getters
            src.add_space();
            src.push("impl SynValue {".to_string());
            for (nu, nl) in &syns {
                src.push(format!("    fn get_{nl}(self) -> Syn{nu} {{"));
                if syns.len() == 1 {
                    src.push(format!("        let SynValue::{nu}(val) = self;"));
                    src.push(format!("        val"));
                } else {
                    src.push(format!("        if let SynValue::{nu}(val) = self {{ val }} else {{ panic!() }}"));
                }
                src.push(format!("    }}"));
            }
            src.push("}".to_string());
        }

        // Writes the listener trait declaration
        src.add_space();
        src.push(format!("pub trait {}Listener {{", self.name));
        src.push(format!("    fn exit(&mut self, _ctx: Ctx) {{}}"));
        src.push(format!("}}"));

        src
    }
}

/// Dictionary-based helper that adapts names so they are unique
struct NameFixer {
    dic: HashSet<String>
}

impl NameFixer {
    pub fn new() -> Self {
        NameFixer { dic: HashSet::new() }
    }

    /// Returns `name` if it's unique, or adds a suffix first to make sure it's unique.
    pub fn get_unique_name(&mut self, mut name: String) -> String {
        let len = name.len();
        let mut index = 0;
        while self.dic.contains(&name) {
            name.truncate(len);
            index += 1;
            name.push_str(&index.to_string());
        }
        self.dic.insert(name.clone());
        name
    }
}

/// Transforms names into CamelCase or underscore_parts
trait NameTransformer {
    fn to_camelcase(&self) -> Self;
    fn to_underscore(&self) -> Self;
}

impl NameTransformer for String {
    fn to_camelcase(&self) -> Self {
        let mut upper = true;
        let result: String = self.chars().filter_map(|c| {
            if c == '_' {
                upper = true;
                None
            } else {
                if upper {
                    upper = false;
                    Some(c.to_ascii_uppercase())
                } else {
                    Some(c.to_ascii_lowercase())
                }
            }
        }).collect();
        assert!(!result.is_empty());
        result
    }

    fn to_underscore(&self) -> Self {
        let mut result = String::new();
        for c in self.chars() {
            if !result.is_empty() && c.is_ascii_uppercase() {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        }
        result
    }
}

#[test]
fn parsergen_camel_case() {
    let tests = vec![
        ("A", "A"),
        ("AA", "Aa"),
        ("AB1", "Ab1"),
        ("a", "A"),
        ("ab_cd_ef", "AbCdEf"),
    ];
    for (str, expected) in tests {
        let result = str.to_string().to_camelcase();
        assert_eq!(result, expected);
    }
}

#[test]
fn parsergen_underscore() {
    let tests = vec![
        ("a", "a"),
        ("AA", "a_a"),
        ("A1", "a1"),
        ("aB1", "a_b1"),
        ("A", "a"),
        ("AbCdEf", "ab_cd_ef"),
        ("ANewTest", "a_new_test"),
    ];
    for (str, expected) in tests {
        let result = str.to_string().to_underscore();
        assert_eq!(result, expected);
    }
}

/// Adds empty lines between blocks
trait SourceSpacer {
    fn add_space(&mut self);
}

impl SourceSpacer for Vec<String> {
    fn add_space(&mut self) {
        if let Some(line) = self.last() {
            if !line.is_empty() {
                self.push("".to_string());
            }
        }
    }
}