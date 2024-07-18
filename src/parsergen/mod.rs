use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use crate::grammar::{LLParsingTable, ProdRuleSet, ruleflag, RuleTreeSet, Symbol, VarId};
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
    opcodes: Vec<Vec<OpCode>>,
    start: VarId
}

impl ParserBuilder {
    pub fn from_tree(tree: RuleTreeSet<General>) -> Self {
        let normalized = RuleTreeSet::<Normalized>::from(tree);
        let lr_rules = ProdRuleSet::from(normalized);
        Self::from_rules(lr_rules)
    }

    pub fn from_rules<T>(rules: ProdRuleSet<T>) -> Self where ProdRuleSet<LL1>: From<ProdRuleSet<T>>, T: std::fmt::Debug {
        let mut ll1_rules = ProdRuleSet::<LL1>::from(rules);
        let start = ll1_rules.get_start().unwrap();
        let parsing_table = ll1_rules.create_parsing_table();
        let symbol_table = ll1_rules.symbol_table().expect(stringify!("symbol table is requires to create a {}", std::any::type_name::<Self>()));
        let mut builder = ParserBuilder {
            parsing_table,
            symbol_table,
            opcodes: Vec::new(),
            start
        };
        builder.build_opcodes();
        builder
    }

    fn has_flags(&self, var: VarId, flags: u32) -> bool {
        self.parsing_table.flags[var as usize] & flags == flags
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
            let factor_id = factor_id as VarId;
            let flags = self.parsing_table.flags[*var_id as usize];
            let stack_sym = Symbol::NT(*var_id);
            let mut new = self.parsing_table.factors[factor_id as usize].1.iter().filter(|s| !s.is_empty()).rev().cloned().to_vec();
            if VERBOSE { println!("- {}", new.iter().map(|s| s.to_str(self.get_symbol_table())).join(" ")); }
            let mut opcode = Vec::<OpCode>::new();
            if flags & ruleflag::CHILD_L_FACTOR != 0 {
                let parent = self.parsing_table.parent[*var_id as usize].unwrap();
                if new.get(0) == Some(&Symbol::NT(parent)) {
                    opcode.push(OpCode::Loop(parent));
                    new.remove(0);
                }
            }
            if flags & ruleflag::PARENT_L_FACTOR == 0 || new.iter().all(|s| if let Symbol::NT(ch) = s { !self.has_flags(*ch, ruleflag::CHILD_L_FACTOR) } else { true }) {
                opcode.push(OpCode::Exit(factor_id)); // will be popped when this NT is completed
            }
            opcode.extend(new.into_iter().map(|s| OpCode::from(s)));
            let r_form_right_rec = flags & ruleflag::R_RECURSION != 0 && flags & ruleflag::L_FORM == 0;
            if opcode.get(1).map(|op| op.matches(stack_sym)).unwrap_or(false) && !r_form_right_rec {
                opcode.swap(0, 1);
            }
            opcode.iter_mut().for_each(|o| {
                if let OpCode::NT(v) = o {
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

    pub fn write_source_code(self, file: Option<File>, indent: usize) -> Result<(), std::io::Error> {
        let mut out: BufWriter<Box<dyn Write>> = match file {
            Some(file) => BufWriter::new(Box::new(file)),
            None => BufWriter::new(Box::new(std::io::stdout().lock()))
        };
        let num_nt = self.symbol_table.get_non_terminals().len();
        let num_t = self.symbol_table.get_terminals().len();
        let s = String::from_utf8(vec![32; indent]).unwrap();
        // Create source code:
        writeln!(out, "{s}// -------------------------------------------------------------------------")?;
        writeln!(out, "{s}// Automatically generated\n")?;
        writeln!(out, "{s}use rlexer::grammar::{{ProdFactor, Symbol, VarId}};")?;
        writeln!(out, "{s}use rlexer::parser::{{OpCode, Parser}};")?;
        writeln!(out, "{s}use rlexer::symbol_table::SymbolTable;\n")?;

        writeln!(out, "{s}const PARSER_NUM_T: usize = {num_t};")?;
        writeln!(out, "{s}const PARSER_NUM_NT: usize = {num_nt};")?;
        writeln!(out, "{s}const SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [{}];",
                 self.symbol_table.get_terminals().iter().map(|(s, os)|
                     format!("(\"{s}\", {})", os.as_ref().map(|s| format!("Some(\"{s}\")")).unwrap_or("None".to_string()))).join(", "))?;
        writeln!(out, "{s}const SYMBOLS_NT: [&str; PARSER_NUM_NT] = [{}];",
                 self.symbol_table.get_non_terminals().iter().map(|s| format!("\"{s}\"")).join(", "))?;
        writeln!(out, "{s}const SYMBOLS_NAMES: [(&str, VarId); {}] = [{}];",
                 self.symbol_table.get_names().count(),
                 self.symbol_table.get_names().map(|(s, v)| format!("(\"{s}\", {v})")).join(", "))?;
        writeln!(out, "{s}const PARSING_FACTORS: [(VarId, &[Symbol]); {}] = [{}];",
                 self.parsing_table.factors.len(),
                 self.parsing_table.factors.iter().map(|(v, f)| format!("({v}, &[{}])", f.iter().map(|s| symbol_to_code(s)).join(", "))).join(", "))?;
        writeln!(out, "{s}const PARSING_TABLE: [VarId; {}] = [{}];",
                 self.parsing_table.table.len(),
                 self.parsing_table.table.iter().map(|v| format!("{v}")).join(", "))?;
        writeln!(out, "{s}const FLAGS: [u32; {}] = [{}];",
                 self.parsing_table.flags.len(), self.parsing_table.flags.iter().join(", "))?;
        writeln!(out, "{s}const PARENT: [Option<VarId>; {}] = [{}];",
                 self.parsing_table.parent.len(), self.parsing_table.parent.iter().map(|p| if let Some(par) = p { format!("Some({par})") } else { format!("None") }).join(", "))?;
        writeln!(out, "{s}const OPCODES: [&[OpCode]; {}] = [{}];", self.opcodes.len(),
                 self.opcodes.into_iter().map(|strip| format!("&[{}]", strip.into_iter().map(|op| format!("OpCode::{op:?}")).join(", "))).join(", "))?;
        writeln!(out, "{s}const START_SYMBOL: VarId = {};\n", self.start)?;

        writeln!(out, "{s}pub(super) fn build_parser() -> Parser {{")?;
        writeln!(out, "{s}    let mut symbol_table = SymbolTable::new();")?;
        writeln!(out, "{s}    symbol_table.extend_terminals(SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))));")?;
        writeln!(out, "{s}    symbol_table.extend_non_terminals(SYMBOLS_NT.into_iter().map(|s| s.to_string()));")?;
        writeln!(out, "{s}    symbol_table.extend_names(SYMBOLS_NAMES.into_iter().map(|(s, v)| (s.to_string(), v)));")?;
        writeln!(out, "{s}    let factors: Vec<(VarId, ProdFactor)> = PARSING_FACTORS.into_iter().map(|(v, s)| (v, ProdFactor::new(s.to_vec()))).collect();")?;
        writeln!(out, "{s}    let table: Vec<VarId> = PARSING_TABLE.into();")?;
        writeln!(out, "{s}    let parsing_table = rlexer::grammar::LLParsingTable {{")?;
        writeln!(out, "{s}        num_nt: PARSER_NUM_NT,")?;
        writeln!(out, "{s}        num_t: PARSER_NUM_T + 1,")?;
        writeln!(out, "{s}        factors,")?;
        writeln!(out, "{s}        table,")?;
        writeln!(out, "{s}        flags: FLAGS.into(),")?;
        writeln!(out, "{s}        parent: PARENT.into(),")?;
        writeln!(out, "{s}    }};")?;
        writeln!(out, "{s}    Parser::new(parsing_table, symbol_table, OPCODES.into_iter().map(|strip| strip.to_vec()).collect(), START_SYMBOL)")?;
        writeln!(out, "{s}}}")?;
        writeln!(out, "{s}// -------------------------------------------------------------------------")?;

        Ok(())
    }
}
