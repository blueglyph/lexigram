use std::collections::HashMap;
use crate::grammar::{Symbol, VarId};

/// Stores the names of the terminal and non-terminal symbols.
///
/// Terminals are defined in the lexicon and don't change. They have two parts to their name:
/// - the identifier in the lexicon
/// - the source string they represent (optional)
///
/// For example:
/// ```lexicon
/// Arrow : '->';
/// ID    : [a-zA-Z][a-zA-Z_0-9]*;
/// ```
///
/// If `Arrow` is 0 and `ID` is 24 (simplifying the strings to &str),
/// ```ignore
/// t[0] = ("Arrow", Some("->"));
/// t[24] = ("ID", None);
/// ```
///
/// Non-terminals are first taken from the grammar, then completed when processing the
/// production rules.
///
/// ```grammar
/// expr: expr Plus term | term;
/// ```
/// If `expr` is 0 and `term` is 1,
/// ```ignore
/// nt[0] = "expr";
/// nt[1] = "term";
/// ```
/// After removing the left recursion, this production rule is modified:
/// ```grammar
/// expr: term expr_0;
/// expr_0: Plus term expr_0 | ε;
/// ```
/// and this non-terminal is added:
/// ```ignore
/// nt[2] = "expr_0";
/// ```
///
#[derive(Clone)]
pub struct SymbolTable {
    // todo: find more efficient storage for terminals
    t: Vec<(String, Option<String>)>,
    nt: Vec<String>,
    names: HashMap<String, VarId>
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            t: Vec::new(),
            nt: Vec::new(),
            names: HashMap::new(),
        }
    }

    pub fn extend_terminals<I: IntoIterator<Item=(String, Option<String>)>>(&mut self, iter: I) {
        self.t.extend(iter);
    }

    pub fn extend_non_terminals<I: IntoIterator<Item=String>>(&mut self, iter: I) {
        self.nt.extend(iter);
    }

    /// Adds a new variable `var_prime` derived from another `var`. Use the name
    /// of `var` and adds a numeric suffix, making sure the new name doesn't already
    /// exist.
    pub fn add_var_prime_name(&mut self, var: VarId, var_prime: VarId) {
        assert_eq!(self.nt.len(), var_prime as usize, "symbol table incomplete");
        let name = &self.nt[var as usize].clone();
        for i in 1.. {
            let name_prime = format!("{name}_{i}");
            if !self.names.contains_key(&name_prime) {
                self.names.insert(name_prime.clone(), var_prime);
                self.nt.push(name_prime);
                break;
            }
        }
        assert_eq!(self.nt.len(), var_prime as usize + 1, "couldn't find a associated name for '{name}'");
    }

    pub fn get_name(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Empty | Symbol::End => symbol.to_string(),
            Symbol::T(token) => {
                // if *token as usize >= self.t.len() { return format!("??T({token})") }
                let name = &self.t[*token as usize];
                name.1.as_ref().unwrap_or(&name.0).clone()
            }
            Symbol::NT(var) => {
                // if *var as usize >= self.nt.len() { return format!("??NT({var})") }
                self.nt[*var as usize].clone()
            }
        }
    }
}
