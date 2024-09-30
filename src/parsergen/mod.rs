#![allow(dead_code)]  // TODO: remove

use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use crate::grammar::{LLParsingTable, ProdRuleSet, ruleflag, RuleTreeSet, Symbol, VarId, FactorId};
use crate::{CollectJoin, General, LL1, Normalized, SourceSpacer, NameTransformer, NameFixer};
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

#[derive(Clone, Debug)]
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

// ---------------------------------------------------------------------------------------------

#[allow(unused)]
pub struct ParserBuilder {
    parsing_table: LLParsingTable,
    symbol_table: SymbolTable,
    name: String,
    nt_value: Vec<bool>,
    nt_parent: Vec<Vec<VarId>>,
    var_factors: Vec<Vec<FactorId>>,
    item_ops: HashMap<FactorId, Vec<Symbol>>,
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
            nt_parent: Vec::new(),
            var_factors: Vec::new(),
            item_ops: HashMap::new(),
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
            let parent = self.parsing_table.parent[*var_id as usize];
            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                let parent = parent.unwrap();
                // replaces Enter by Loop when going back to left-factorization parent, typically when coupled with + or *
                // (per construction, there can't be any factor going back to the grandparent or further up in a left factorization, so
                //  we don't check that)
                if new.get(0) == Some(&Symbol::NT(parent)) {
                    opcode.push(OpCode::Loop(parent));
                    new.remove(0);
                }
            }
            if flags & ruleflag::PARENT_L_FACTOR == 0 ||
                flags & ruleflag::PARENT_L_RECURSION != 0 && parent.is_none() ||
                new.iter().all(|s| if let Symbol::NT(ch) = s { !self.nt_has_flags(*ch, ruleflag::CHILD_L_FACTOR) } else { true })
            {
                // if it's not a parent of left factorization, or
                // if none of the NT in the factor is a child of left factorization, or
                // if it's the top parent of left recursion + left factorization,
                // => adds an Exit
                // (said otherwise: we don't want an exit in a parent or in the middle of a chain of left factorizations;
                //  the exit should be only at the end of left factorizations, or in factors that aren't left-factorized -
                //  except the special case of left recursion + left factorization, which needs the final exit)
                opcode.push(OpCode::Exit(factor_id)); // will be popped when this NT is completed
            }
            opcode.extend(new.into_iter().map(|s| OpCode::from(s)));
            let r_form_right_rec = flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
            if opcode.get(1).map(|op| op.matches(stack_sym)).unwrap_or(false) && !r_form_right_rec {
                // swaps Exit(self) when it's in 2nd position (only happens in [Loop(_), Exit(self), ...],
                // except right recursions that aren't left-form, because we let them unfold naturally (uses more stack)
                opcode.swap(0, 1);
            } else if flags & ruleflag::PARENT_L_RECURSION != 0 {
                // swaps Exit(self) and call to left recursive item so that the wrapper can issue an exit_NT
                // with the correct context
                opcode.swap(0, 1);

            }
            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                if opcode.len() >= 2 {
                    let mut fact_top = *var_id;
                    while let Some(parent) = self.parsing_table.parent[fact_top as usize] {
                        fact_top = parent;
                    }
                    if VERBOSE {
                        println!("  - check for initial exit swap: opcode = [{}], daddy = {}",
                                 opcode.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                 Symbol::NT(fact_top).to_str(self.get_symbol_table()));
                    }
                    if self.parsing_table.flags[fact_top as usize] & ruleflag::PARENT_L_RECURSION != 0 &&
                        matches!(opcode[0], OpCode::Exit(_)) &&
                        matches!(opcode[1], OpCode::NT(v) if self.parsing_table.flags[v as usize] & ruleflag::CHILD_L_RECURSION != 0)
                    {
                        if VERBOSE {
                            println!("    swapping for initial exit_{}: {} <-> {}",
                                Symbol::NT(fact_top).to_str(self.get_symbol_table()).to_lowercase(),
                                opcode[0].to_str(self.get_symbol_table()),
                                opcode[1].to_str(self.get_symbol_table())
                            );
                        }
                        opcode.swap(0, 1);
                    }
                }
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

    fn get_group_factors(&self, g: &Vec<VarId>) -> Vec<(VarId, FactorId)> {
        g.iter().flat_map(|c|
            self.var_factors[*c as usize].iter().map(|f| (*c, *f))
        ).collect::<Vec<_>>()
    }

    fn build_item_ops(&mut self) {
        const VERBOSE: bool = false;
        let info = &self.parsing_table;
        let mut items = HashMap::<FactorId, Vec<Symbol>>::new();
        self.var_factors = vec![vec![]; info.num_nt];
        for (factor_id, (var_id, _)) in info.factors.iter().enumerate() {
            self.var_factors[*var_id as usize].push(factor_id as FactorId);
        }
        let mut nt_parent: Vec<Vec<VarId>> = vec![vec![]; info.num_nt];
        for var_id in 0..info.num_nt {
            let mut top_var_id = var_id;
            while let Some(parent) = info.parent[top_var_id] {
                top_var_id = parent as usize;
            }
            nt_parent[top_var_id].push(var_id as VarId);
        }
        if VERBOSE {
            println!("Groups:");
            for (parent_id, group) in nt_parent.iter().enumerate().filter(|(_, vf)| !vf.is_empty()) {
                let group = self.get_group_factors(group);
                let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                println!("{}: {}, factors {}",
                         Symbol::NT(parent_id as VarId).to_str(self.get_symbol_table()),
                    ids.iter().map(|v| Symbol::NT(*v).to_str(self.get_symbol_table())).join(", "),
                    group.iter().map(|(_, f)| f.to_string()).join(", ")
                );
            }
        }
        // we proceed by var parent, then all factors in each parent/children group
        for (_parent_id, g) in nt_parent.iter().enumerate() {
            // takes all the factors in the group (and their NT ID):
            let group = self.get_group_factors(g);
            let mut change = true;
            while change {
                change = false;
                let mut nt_used = HashSet::<VarId>::new();
                if VERBOSE {
                    let ids = group.iter().map(|(v, _)| *v).collect::<BTreeSet<VarId>>();
                    println!("parent: {}, NT with value: {}",
                             Symbol::NT(_parent_id as VarId).to_str(self.get_symbol_table()),
                             ids.into_iter().filter_map(|v|
                                 if self.nt_value[v as usize] { Some(Symbol::NT(v as VarId).to_str(self.get_symbol_table())) } else { None }
                             ).join(", "));
                }
                for (_, factor_id) in &group {
                    items.insert(*factor_id, vec![]);
                }
                for (var_id, factor_id) in &group {
                    let opcode = &self.opcodes[*factor_id as usize];
                    let (_, factor) = &info.factors[*factor_id as usize];
                    if VERBOSE {
                        print!("- {factor_id}: {} -> {}   [{}]",
                               Symbol::NT(*var_id).to_str(self.get_symbol_table()),
                               factor.to_str(self.get_symbol_table()),
                               opcode.iter().map(|op| op.to_str(self.get_symbol_table())).join(" "));
                    }
                    let flags = info.flags[*var_id as usize];

                    // Default values are taken from opcodes. Loop(nt) is only taken if the parent is l-rec;
                    // we look at the parent's flags instead of the factor's because left factorization could
                    // displace the Loop(nt) to another non-l-rec child factor.
                    let mut values = self.opcodes[*factor_id as usize].iter().rev()
                        .filter_map(|s| {
                            let sym_maybe = match s {
                                OpCode::T(t) => Some(Symbol::T(*t)),
                                OpCode::NT(nt) => {
                                    nt_used.insert(*nt);
                                    Some(Symbol::NT(*nt))
                                },
                                _ => {
                                    if VERBOSE { print!(" | {} dropped", s.to_str(self.get_symbol_table())); }
                                    None
                                }
                            };
                            sym_maybe.and_then(|s| if self.sym_has_value(&s) { Some(s) } else { None })
                        }).to_vec();

                    // Looks if a child_repeat has a value
                    if !values.is_empty() && self.parsing_table.parent[*var_id as usize].is_some() {
                        let mut top_nt = *var_id as usize;
                        // print!(" [{}, so {}?", values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "), Symbol::NT(top_nt as VarId).to_str(self.get_symbol_table()));
                        while self.parsing_table.flags[top_nt] & ruleflag::CHILD_REPEAT == 0 {
                            if let Some(parent) = self.parsing_table.parent[top_nt] {
                                top_nt = parent as usize;
                            } else {
                                break;
                            }
                        }
                        if self.parsing_table.flags[top_nt] & ruleflag::CHILD_REPEAT != 0 {
                            if VERBOSE && !self.nt_value[top_nt] {
                                print!(" | {} is now valued {}",
                                       Symbol::NT(top_nt as VarId).to_str(self.get_symbol_table()),
                                       if nt_used.contains(&(top_nt as VarId)) { "and was used before" } else { "but wasn't used before" }
                                );
                            }
                            change |= !self.nt_value[top_nt] && nt_used.contains(&(top_nt as VarId));
                            self.nt_value[top_nt] = true;
                        }
                        // print!("]");
                    }
                    if change {
                        // the nt_value of one item has been set.
                        if VERBOSE { println!("\nnt_value changed, redoing this group"); }
                        break;
                    }

                    // Loop NTs which carry values are kept on the stack, too
                    let sym_maybe = if flags & ruleflag::CHILD_REPEAT != 0 && !values.is_empty() {
                        Some(Symbol::NT(*var_id))
                    } else if flags & ruleflag::CHILD_L_RECURSION != 0 && !values.is_empty() {
                        let parent = info.parent[*var_id as usize].unwrap();
                        Some(Symbol::NT(parent))
                    } else if flags & (ruleflag::R_RECURSION | ruleflag::L_FORM) == ruleflag::R_RECURSION | ruleflag::L_FORM {
                        Some(Symbol::NT(*var_id))
                    } else {
                        None
                    };
                    if let Some(s) = sym_maybe {
                        if self.sym_has_value(&s) {
                            if VERBOSE { print!(" | loop => {}", s.to_str(self.get_symbol_table())); }
                            values.insert(0, s);
                        }
                    }

                    if VERBOSE { println!(" ==> [{}]", values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
                    if let Some(OpCode::NT(nt)) = opcode.get(0) {
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
                            if VERBOSE {
                                println!("  PARENT_L_FACTOR: moving {} to child {}",
                                         values.iter().map(|s| s.to_str(self.get_symbol_table())).join(" "),
                                         Symbol::NT(*nt).to_str(self.get_symbol_table()));
                            }
                            // factorization reports all the values to the children
                            if let Some(pre) = items.get_mut(&factor_id) {
                                // pre-pends values that already exist for factor_id (and empties factor_id)
                                values.splice(0..0, std::mem::take(pre));
                            }
                            for f_id in self.var_factors[*nt as usize].iter() {
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
            }
        }
        self.nt_parent = nt_parent;
        self.item_ops = items;
    }

    /// Calculates nt_name, nt_info, item_info
    ///
    /// * `nt_name[var]: Option<(String, String)>` contains upper-case and lower-case unique identifiers for each parent NT
    /// * `nt_info[var]: Vec<(FactorId, String)>` contains the enum variant names for each context
    /// * `item_info[factor]: Vec<ItemInfo>` contains the data available on the stacks when exiting the factor
    /// * `nt_repeat[var]: HashMap<VarId, Vec<ItemInfo>>` contains the data of + and * items
    ///
    /// For example:
    ///
    /// ```text
    /// // A -> (B c)* b | a; B -> b
    /// //
    /// //  0: A -> A_1 b     | ◄0 b! ►A_1    | A_1 b
    /// //  1: A -> a         | ◄1 a!         | a
    /// //  2: B -> b         | ◄2 b!         | b
    /// //  3: A_1 -> B c A_1 | ●A_1 ◄3 c! ►B | A_1 B c
    /// //  4: A_1 -> ε       | ◄4            |
    ///
    /// pub enum Ctx { A { a: SynA } }
    /// pub enum CtxA {
    ///     A1 { star: SynA1, b: String },
    ///     A2 { a: String },
    /// }
    /// pub enum CtxB {
    ///     B { b: String },
    /// }
    ///
    /// struct SynA1(Vec<SynA1Item>);
    /// struct SynA1Item { b: SynB, c: String }
    /// // User-defined: SynA, SynB
    ///
    /// nt_name = [Some(("A", "a")), Some(("B", "b")), Some(("A1", "a1"))]
    /// nt_info = [[(0, "A1"), (1, "A2")], [(2, "B")], []]
    /// item_info = [
    ///     [ItemInfo { name: "star", sym: NT(2), .. }, ItemInfo { name: "b", sym: T(1), .. }],
    ///     [ItemInfo { name: "a", sym: T(0), .. }],
    ///     [ItemInfo { name: "b", sym: T(1), .. }],
    ///     [ItemInfo { name: "star_it", sym: NT(2), .. }, ItemInfo { name: "b", sym: NT(1), .. }, ItemInfo { name: "c", sym: T(2), .. }],
    ///     []
    /// ]
    /// nt_repeat = {
    ///     2: [ItemInfo { name: "b", sym: NT(1), .. }, ItemInfo { name: "c", sym: T(2), .. }]
    /// }
    /// ```
    fn get_type_info(&self) -> (Vec<Option<(String, String)>>, Vec<Vec<(FactorId, String)>>, Vec<Vec<ItemInfo>>, HashMap<VarId, Vec<ItemInfo>>) {
        const VERBOSE: bool = false;

        let pinfo = &self.parsing_table;
        let mut var_factors: Vec<Vec<FactorId>> = vec![vec![]; pinfo.num_nt];
        for (factor_id, (var_id, _)) in pinfo.factors.iter().enumerate() {
            var_factors[*var_id as usize].push(factor_id as FactorId);
        }

        let mut nt_upper_fixer = NameFixer::new();
        let mut nt_lower_fixer = NameFixer::new();
        let mut nt_name: Vec<Option<(String, String)>> = (0..pinfo.num_nt).map(|v| {
            let iter = self.nt_has_flags(v as VarId, ruleflag::CHILD_REPEAT | ruleflag::L_FORM);
            if self.nt_value[v] || pinfo.parent[v].is_none() || iter {
                let name = if iter {
                    // use the name "<parent>_iter" for +* + l-form children
                    let mut top_var_id = v;
                    let mut n = 0;
                    while let Some(parent) = pinfo.parent[top_var_id] {
                        top_var_id = parent as usize;
                        n += 1;
                    }
                    format!("{}_iter{}", self.symbol_table.get_nt_name(top_var_id as VarId), if n > 1 { n.to_string() } else { "".to_string() })
                } else {
                    // normal NT name
                    self.symbol_table.get_nt_name(v as VarId)
                };
                let nu = nt_upper_fixer.get_unique_name(name.to_camelcase());
                let nl = nt_lower_fixer.get_unique_name(nu.to_underscore_lowercase());
                Some((nu, nl))
            } else {
                None
            }
        }).to_vec();

        let mut nt_info: Vec<Vec<(FactorId, String)>> = vec![vec![]; pinfo.num_nt];
        let mut nt_repeat = HashMap::<VarId, Vec<ItemInfo>>::new();
        let item_info: Vec<Vec<ItemInfo>> = (0..pinfo.factors.len()).map(|i| {
            let factor_id = i as FactorId;
            let nt = pinfo.factors[i].0 as usize;
            if let Some(item_ops) = self.item_ops.get(&factor_id) {
                // Adds a suffix to the names of different symbols that would otherwise collide in the same context option:
                // - identical symbols are put in a vector (e.g. `id: [String; 2]`)
                // - different symbols, which means T vs NT, must have different names (e.g. `NT(A)` becomes "a",
                //   `T(a)` becomes "a", too => one is renamed to "a1" to avoid the collision: `{ a: SynA, a1: String }`)
                let mut indices = HashMap::<Symbol, (String, Option<usize>)>::new();
                let mut fixer = NameFixer::new();
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
                let is_nt_repeat = pinfo.flags[owner as usize] & ruleflag::CHILD_REPEAT != 0;
                for s in item_ops {
                    if let Symbol::NT(v) = s {
                        if nt_name[*v as usize].is_none() {
                            let name = self.symbol_table.get_nt_name(*v);
                            nt_name[*v as usize] = Some((nt_upper_fixer.get_unique_name(name.to_camelcase()),
                                                         nt_lower_fixer.get_unique_name(name.to_underscore_lowercase())));
                        }
                    }
                    if let Some((_, c)) = indices.get_mut(s) {
                        *c = Some(0);
                    } else {
                        let name = if let Symbol::NT(vs) = s {
                            let flag = pinfo.flags[*vs as usize];
                            if flag & ruleflag::CHILD_REPEAT != 0 {
                                let inside_factor_id = var_factors[*vs as usize][0];
                                let inside_factor = &pinfo.factors[inside_factor_id as usize].1;
                                if false {
                                    let mut plus_name = inside_factor.symbols()[0].to_str(self.get_symbol_table()).to_underscore_lowercase();
                                    plus_name.push_str(if flag & ruleflag::REPEAT_PLUS != 0 { "_plus" } else { "_star" });
                                    plus_name
                                } else {
                                    if is_nt_repeat && indices.is_empty() {
                                        // iterator variable in a + * loop (visible with <L>, for ex)
                                        if flag & ruleflag::REPEAT_PLUS != 0 { "plus_it".to_string() } else { "star_it".to_string() }
                                    } else {
                                        // reference to a + * result
                                        if flag & ruleflag::REPEAT_PLUS != 0 { "plus".to_string() } else { "star".to_string() }
                                    }
                                }
                            } else {
                                nt_name[*vs as usize].clone().unwrap().1
                            }
                        } else {
                            s.to_str(self.get_symbol_table()).to_lowercase()
                        };
                        indices.insert(*s, (fixer.get_unique_name(name), None));
                    }
                }
                let has_no_exit = pinfo.flags[nt] & ruleflag::PARENT_L_FACTOR != 0 ||
                    (pinfo.flags[nt] & (ruleflag::CHILD_REPEAT | ruleflag::CHILD_L_RECURSION) != 0 &&
                     self.opcodes[i].len() == 1 && self.opcodes[i][0] == OpCode::Exit(i as FactorId));
                let has_data = (!item_ops.is_empty() || (self.nt_value[nt] && pinfo.flags[nt] & ruleflag::R_RECURSION != 0)) &&
                    pinfo.flags[owner as usize] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) != ruleflag::CHILD_REPEAT
                    || pinfo.flags[nt] & ruleflag::CHILD_L_RECURSION != 0;
                let has_context = has_data || (!has_no_exit && pinfo.parent[nt].is_none());
                if VERBOSE {
                    println!("NT {nt}, factor {factor_id}: has_data = {has_data}, has_no_exit = {} = {} && {} ({}), parent_left_fact = {} => has_context = {has_context}",
                             has_no_exit,
                             pinfo.flags[nt] & (ruleflag::CHILD_REPEAT | ruleflag::CHILD_L_RECURSION) != 0,
                             self.opcodes[i].len() == 1 && self.opcodes[i][0] == OpCode::Exit(i as VarId),
                             self.opcodes[i].iter().map(|op| op.to_str(self.get_symbol_table())).join(" "),
                             pinfo.flags[nt] & ruleflag::PARENT_L_FACTOR != 0
                    );
                }
                if has_context {
                    let len = nt_info[owner as usize].len();
                    if len == 1 {
                        NameFixer::add_number(&mut nt_info[owner as usize][0].1, 1);
                    }
                    let mut name = Symbol::NT(owner).to_str(self.get_symbol_table()).to_camelcase();
                    if len > 0 { NameFixer::add_number(&mut name, len + 1); };
                    nt_info[owner as usize].push((factor_id, name));
                }
                if item_ops.is_empty() && pinfo.flags[nt] & ruleflag::CHILD_L_RECURSION != 0 {
                    // we put here the return context for the final exit of left recursive rule
                    // let parent = pinfo.parent[nt].unwrap();
                    vec![ItemInfo {
                        name: nt_name[owner as usize].as_ref().map(|n| n.1.clone()).unwrap(),
                        sym: Symbol::NT(owner as VarId),
                        owner: owner,
                        is_vec: false,
                        index: None,
                    }]
                } else {
                    let infos = item_ops.into_iter().map(|s| {
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
                    }).to_vec();
                    if is_nt_repeat && infos.len() > 1 && !nt_repeat.contains_key(&owner) {
                        let iter = infos.iter().skip(1).cloned().to_vec();
                        if iter.len() > 1 || !iter[0].sym.is_t() {
                            nt_repeat.insert(owner, iter);
                        }
                    }
                    infos
                }
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
        // println!("nt_name: {nt_name:?}");
        // println!("nt_info: {nt_info:?}");
        // println!("item_info: {item_info:?}");
        // println!("nt_repeat: {nt_repeat:?}");
        (nt_name, nt_info, item_info, nt_repeat)
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
            self.build_item_ops();
            parts.push(self.source_wrapper());
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

    /// Structure elements used in a context or in a +* child type
    fn source_infos(infos: &Vec<ItemInfo>, nt_name: &Vec<Option<(String, String)>>) -> String {
        infos.iter().filter_map(|info| {
            if info.index.is_none() || info.index == Some(0) {
                let type_name_base = match info.sym {
                    Symbol::T(_) => "String".to_string(),
                    Symbol::NT(vs) => format!("Syn{}", nt_name[vs as usize].clone().unwrap().0),
                    _ => panic!("unexpected symbol {}", info.sym)
                };
                let type_name = if info.index.is_some() {
                    let nbr = infos.iter()
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
        }).join(", ")
    }

    #[allow(unused)]
    fn source_wrapper(&mut self) -> Vec<String> {
        const VERBOSE: bool = false;

        let (nt_name, nt_info, item_info, nt_repeat) = self.get_type_info();
        let pinfo = &self.parsing_table;

        let mut src = vec![];

        // Writes contexts
        if let Some((nu, nl)) = &nt_name[self.start as usize] {
            src.push(format!("pub enum Ctx {{ {nu} {{ {nl}: Syn{nu} }} }}"));
        } else {
            panic!("{} has no name", Symbol::NT(self.start).to_str(self.get_symbol_table()));
        }
        for (v, factor_names) in nt_info.iter().enumerate().filter(|(_, f)| !f.is_empty()) {
            src.push(format!("pub enum Ctx{} {{", nt_name[v].clone().unwrap().0));
            for (f_id, f_name) in factor_names {
                let ctx_content = Self::source_infos(&item_info[*f_id as usize], &nt_name);
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
        let mut syns = Vec::<(&str, &str)>::new();
        for (v, name) in nt_name.iter().enumerate().filter(|(v, _)| self.nt_value[*v]) {
            let (nu, nl) = name.as_ref().map(|(nu, nl)| (nu.as_str(), nl.as_str())).unwrap();
            if pinfo.flags[v] & (ruleflag::CHILD_REPEAT | ruleflag::L_FORM) == ruleflag::CHILD_REPEAT {
                if let Some(infos) = nt_repeat.get(&(v as VarId)) {
                    // complex + * items; for ex. A -> (B b)+
                    src.push(format!("struct Syn{nu}(Vec<Syn{nu}Item>);"));
                    src.push(format!("struct Syn{nu}Item {{ {} }}", Self::source_infos(&infos, &nt_name)));
                } else {
                    // + * item is only a terminal
                    src.push(format!("struct Syn{nu}(Vec<String>);"));
                }
            } else {
                if self.nt_value[v] {
                    user_names.push(format!("Syn{nu}"));
                } else {
                    src.push(format!("struct Syn{nu}();"))
                }
            }
            // syns.push((nu.to_string(), nl.to_string()));
            syns.push((nu, nl));
        }
        if !user_names.is_empty() {
            src.push(format!("// User-defined: {}", user_names.join(", ")));
        }

        // Writes SynValue type and implementation
        src.add_space();
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

        // Prepares the data for the following sections
        let mut src_init = Vec::<String>::new();
        let mut src_exit = Vec::<String>::new();
        let mut src_listener_decl = Vec::<String>::new();
        let mut src_wrapper_impl = Vec::<String>::new();

        for nt in 0..self.parsing_table.num_nt {
            let flags = self.parsing_table.flags[nt];
            let has_value = self.nt_value[nt];
            if self.parsing_table.parent[nt].is_none() {
                let (nu, nl) = nt_name[nt].as_ref().unwrap();
                if has_value && self.nt_has_flags(nt as VarId, ruleflag::R_RECURSION | ruleflag::L_FORM) {
                    src_listener_decl.push(format!("    fn init_{nl}() -> Syn{nu};"));
                    src_init.push(format!("                    {nt} => self.init_{nl}(),"));
                    src_wrapper_impl.push(format!("    fn init_{nl}() {{"));
                    src_wrapper_impl.push(format!("        let val = self.listener.init_{nl}();"));
                    src_wrapper_impl.push(format!("        self.stack.push(SynValue::{nu}(val));"));
                    src_wrapper_impl.push(format!("    }}"));
                } else {
                    src_listener_decl.push(format!("    fn init_{nl}() {{}}"));
                    src_init.push(format!("                    {nt} => self.listener.init_{nl}(),"));
                }
            } else {
                if flags & ruleflag::CHILD_REPEAT != 0 {
                    if has_value {
                        let (nu, nl) = nt_name[nt].as_ref().unwrap();
                        src_init.push(format!("                    {nt} => self.init_{nl}(),"));
                        src_wrapper_impl.push(format!("    fn init_{nl}() {{"));
                        if flags & ruleflag::L_FORM != 0 {
                            src_wrapper_impl.push(format!("        let val = self.listener.init_{nl}();"));
                            src_listener_decl.push(format!("    fn init_{nl}() -> Syn{nu};"));
                        } else {
                            src_wrapper_impl.push(format!("        let val = Syn{nu}(Vec::new());"));
                        }
                        src_wrapper_impl.push(format!("        self.stack.push(SynValue::{nu}(val));"));
                        src_wrapper_impl.push(format!("    }}"));
                    } else {
                        if flags & ruleflag::L_FORM != 0 {
                            let (nu, nl) = nt_name[nt].as_ref().unwrap();
                            src_init.push(format!("                    {nt} => self.listener.init_{nl}(),"));
                            src_listener_decl.push(format!("    fn init_{nl}() {{}}"));
                        } else {
                            src_init.push(format!("                    {nt} => {{}}"));
                        }
                    }
                }
            }

        }
        // TODO: populate the src_*

        // Writes the listener trait declaration
        src.add_space();
        src.push(format!("pub trait {}Listener {{", self.name));
        src.push(format!("    fn exit(&mut self, _ctx: Ctx) {{}}"));
        src.extend(src_listener_decl);
        src.push(format!("}}"));

        // Writes the switch() function
        src.add_space();
        src.push(format!("struct ListenerWrapper<T> {{"));
        src.push(format!("    verbose: bool,"));
        src.push(format!("    listener: T,"));
        src.push(format!("    stack: Vec<SynValue>,"));
        src.push(format!("    max_stack: usize,"));
        src.push(format!("    stack_t: Vec<String>,"));
        src.push(format!("}}"));
        src.push(format!(""));
        src.push(format!("impl<T: LeftRecListener> ListenerWrapper<T> {{"));
        src.push(format!("    pub fn new(listener: T, verbose: bool) -> Self {{"));
        src.push(format!("        ListenerWrapper {{ verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }}"));
        src.push(format!("    }}"));
        src.push(format!(""));
        src.push(format!("    pub fn listener(self) -> T {{"));
        src.push(format!("        self.listener"));
        src.push(format!("    }}"));
        src.push(format!("}}"));
        src.push(format!(""));
        src.push(format!("impl<T: LeftRecListener> Listener for ListenerWrapper<T> {{"));
        src.push(format!("    fn switch(&mut self, call: Call, nt: VarId, factor_id: VarId, t_data: Option<Vec<String>>) {{"));
        src.push(format!("        if let Some(mut t_data) = t_data {{"));
        src.push(format!("            self.stack_t.append(&mut t_data);"));
        src.push(format!("        }}"));
        src.push(format!("        match call {{"));
        src.push(format!("            Call::Enter => {{"));
        src.push(format!("                match nt {{"));
        /*
        src.push(format!("                    0 => self.listener.init_a(),    // A"));
        src.push(format!("                    1 => {{}}                         // A_1"));
        src.push(format!("                    2 => {{}}                         // A_2"));
        */
        src.extend(src_init);
        src.push(format!("                    _ => panic!(\"unexpected exit non-terminal id: {{nt}}\")"));
        src.push(format!("                }}"));
        src.push(format!("            }}"));
        src.push(format!("            Call::Loop => {{}}"));
        src.push(format!("            Call::Exit => {{"));
        src.push(format!("                match factor_id {{"));
        /*
        src.push(format!("                    0 => self.exit_a(),             //  0: A -> b A_2   | ◄0 ►A_2 b! |"));
        src.push(format!("                    1 => self.exit_a_1(),           //  1: A_1 -> a A_1 | ●A_1 ◄1 a! | A a"));
        src.push(format!("                    2 => {{}}                         //  2: A_1 -> ε     | ◄2         |"));
        src.push(format!("                    3 |                             //  3: A_2 -> c A_1 | ►A_1 ◄3 c! | b c"));
        src.push(format!("                    4 => self.exit_a_2(factor_id),  //  4: A_2 -> d A_1 | ►A_1 ◄4 d! | b d"));
        */
        src.extend(src_exit);
        src.push(format!("                    _ => panic!(\"unexpected exit factor id: {{factor_id}}\")"));
        src.push(format!("                }}"));
        src.push(format!("            }}"));
        src.push(format!("            Call::End => {{"));
        src.push(format!("                self.exit();"));
        src.push(format!("            }}"));
        src.push(format!("        }}"));
        src.push(format!("        self.max_stack = std::cmp::max(self.max_stack, self.stack.len());"));
        src.push(format!("        if self.verbose {{"));
        src.push(format!("            println!(\"> stack_t:   {{}}\", self.stack_t.join(\", \"));"));
        src.push(format!("            println!(\"> stack:     {{}}\", self.stack.iter().map(|it| format!(\"{{it:?}}\")).join(\", \"));"));
        src.push(format!("        }}"));
        src.push(format!("    }}"));
        src.push(format!("}}"));

        src.add_space();
        src.push(format!("impl<T: LeftRecListener> ListenerWrapper<T> {{"));
        src.push(format!("    fn exit(&mut self, _ctx: Ctx) {{"));
        if let Some((nu, nl)) = &nt_name[self.start as usize] {
            if self.nt_value[self.start as usize] {
                src.push(format!("        let {nl} = self.stack.pop().unwrap().get_{nl}();"));
                src.push(format!("        self.listener.exit(Ctx::{nu} {{ {nl} }});"));
            } else {
                src.push(format!("        self.listener.exit(Ctx::{nu}{{ {nl}: Syn{nu}() }});"));
            }
        }
        src.push(format!("    }}"));
        src.extend(src_wrapper_impl);
        src.push(format!("}}"));

        /*
                                let a = self.stack.pop().unwrap().get_a();
                                self.listener.exit(Ctx::A { a });
        */
        src
    }
}
