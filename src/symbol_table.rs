use std::collections::HashMap;
use crate::dfa::TokenId;
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
#[derive(Clone, Debug)]
pub struct SymbolTable {
    // todo: find more efficient storage for terminals
    t: Vec<(String, Option<String>)>,
    nt: Vec<String>,
    names: HashMap<String, VarId>,
    primes: HashMap<VarId, VarId>, // primes[A_1] = A
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            t: Vec::new(),
            nt: Vec::new(),
            names: HashMap::new(),
            primes: HashMap::new(),
        }
    }

    /// Does `Symbol::T(token)` hold lexer string data?
    pub fn is_t_data(&self, token: TokenId) -> bool {
        self.t[token as usize].1.is_none()
    }

    /// Is `symbol` a terminal holding lexer string data?
    pub fn is_symbol_t_data(&self, symbol: &Symbol) -> bool {
        if let Symbol::T(token) = symbol {
            self.t[*token as usize].1.is_none()
        } else {
            false
        }
    }

    pub fn get_terminals(&self) -> &[(String, Option<String>)] {
        &self.t
    }

    pub fn extend_terminals<I: IntoIterator<Item=(String, Option<String>)>>(&mut self, iter: I) {
        self.t.extend(iter);
    }

    pub fn get_non_terminals(&self) -> &[String] {
        &self.nt
    }

    pub fn extend_non_terminals<I: IntoIterator<Item=String>>(&mut self, iter: I) {
        self.nt.extend(iter);
    }

    pub fn remove_non_terminal(&mut self, v: VarId) {
        self.nt.remove(v as usize);
        for old_v in self.names.values_mut() {
            if *old_v >= v { *old_v -= 1; }
        }
        self.primes = self.primes.iter()
            .filter(|(&child, &parent)| child != v && parent != v)
            .map(|(&child, &parent)| (if child > v { child - 1 } else { child }, if parent > v { parent - 1 } else { parent }))
            .collect::<HashMap<_, _>>();
    }

    pub fn get_names(&self) -> impl Iterator<Item=(&String, &VarId)> {
        self.names.iter()
    }

    pub fn extend_names<I: IntoIterator<Item=(String, VarId)>>(&mut self, iter: I) {
        self.names.extend(iter);
    }

    /// Adds a new variable `var_prime` derived from another `var`. If `var` itself is
    /// a prime, find the original ancestor as `var`. Use the name of `var` (or its ancestor)
    /// and adds a numeric suffix, making sure the new name doesn't already exist.
    pub fn add_var_prime_name(&mut self, mut var: VarId, var_prime: VarId) {
        assert_eq!(self.nt.len(), var_prime as usize, "bad var_prime index {var_prime} (should be {})", self.nt.len());
        while let Some(ancestor) = self.primes.get(&var) {
            var = *ancestor;
        }
        let name = &self.nt[var as usize].clone();
        for i in 1.. {
            let name_prime = format!("{name}_{i}");
            if !self.names.contains_key(&name_prime) {
                self.names.insert(name_prime.clone(), var_prime);
                self.primes.insert(var_prime, var);
                self.nt.push(name_prime);
                break;
            }
        }
        assert_eq!(self.nt.len(), var_prime as usize + 1, "couldn't find an associated name for '{name}'");
    }

    pub fn get_t_name(&self, token: TokenId) -> String {
        // if *token as usize >= self.t.len() { return format!("??T({token})") }
        let name = &self.t[token as usize];
        name.1.as_ref().unwrap_or(&name.0).clone()
    }

    pub fn set_t_name(&mut self, token: TokenId, name_maybe: Option<String>) {
        self.t[token as usize].1 = name_maybe
    }

    pub fn get_nt_name(&self, var: VarId) -> String {
        // if *var as usize >= self.nt.len() { return format!("??NT({var})") }
        self.nt[var as usize].clone()
    }

    pub fn get_name(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Empty | Symbol::End => symbol.to_string(),
            Symbol::T(token) => self.get_t_name(*token),
            Symbol::NT(var) => self.get_nt_name(*var),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::hashmap;
    use super::*;

    #[test]
    fn primes() {
        let mut st = SymbolTable::new();
        st.extend_non_terminals(["A".to_string(), "NOT_USED".to_string(), "E".to_string()]);
        let mut st2 = st.clone();
        st.add_var_prime_name(0, 3);
        assert_eq!(st.get_name(&Symbol::NT(3)), "A_1");
        st.add_var_prime_name(3, 4);
        assert_eq!(st.get_name(&Symbol::NT(4)), "A_2");
        st.add_var_prime_name(2, 5);
        assert_eq!(st.get_name(&Symbol::NT(5)), "E_1");
        st.add_var_prime_name(1, 6);
        assert_eq!(st.primes, hashmap![3 => 0, 4 => 0, 5 => 2, 6 => 1]);
        st.remove_non_terminal(1);
        assert_eq!(st.primes, hashmap![2 => 0, 3 => 0, 4 => 1]);
        assert_eq!(st.get_name(&Symbol::NT(2)), "A_1");
        assert_eq!(st.get_name(&Symbol::NT(3)), "A_2");
        assert_eq!(st.get_name(&Symbol::NT(4)), "E_1");

        st2.add_var_prime_name(1, 3);
        st2.add_var_prime_name(0, 4);
        st2.add_var_prime_name(2, 5);
        assert_eq!(st2.primes, hashmap![3 => 1, 4 => 0, 5 => 2]);
        st2.remove_non_terminal(3);
        assert_eq!(st2.primes, hashmap![        3 => 0, 4 => 2]);
    }

    #[test]
    #[should_panic(expected="bad var_prime index 2")]
    fn overwrite_prime() {
        let mut st = SymbolTable::new();
        st.extend_non_terminals(["A".to_string(), "B".to_string()]);
        st.add_var_prime_name(0, 2);
        st.add_var_prime_name(1, 2);
    }
}