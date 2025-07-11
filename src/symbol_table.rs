// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#[cfg(any())]
use std::collections::HashMap;
use crate::{NameFixer, FixedSymTable, SymInfoTable};
use crate::dfa::TokenId;
use crate::grammar::{Symbol, VarId};

// NOTE: nonterminal-to-ID functionality currently disabled by #[cfg(any())]

/// Stores the names of the terminal and nonterminal symbols when building a parser.
///
/// Terminals are defined in the lexicon and don't change. They have two parts to their name:
/// - the identifier in the lexicon
/// - the source string they represent (optional)
///
/// For example:
/// ```lexicon
/// Plus : '+';
/// ...
/// ID    : [a-zA-Z][a-zA-Z_0-9]*;
/// ```
///
/// If `Arrow`'s token ID is 0 and `ID`'s is 24,
/// ```ignore
/// t[0] = ("Plus".to_string(), Some("+".to_string()));
/// t[24] = ("ID".to_string(), None);
/// ```
///
/// They're added to the symbol table with [`add_terminal()`](SymbolTable::add_terminal).
///
/// Nonterminals are defined in the grammar, and possibly completed by new ones when
/// the rules are adapted to the target parser. For example, recursive rules are
/// transformed for LL(1) parsers, which usually adds extra rules.
///
/// ```grammar
/// expr: expr Plus term | term;
/// ```
/// If `expr` is 0 and `term` is 1,
/// ```ignore
/// nt[0] = "expr".to_string();
/// nt[1] = "term".to_string();
/// ```
/// They're added with [`add_nonterminal`](SymbolTable::add_nonterminal).
///
/// The new rules are called "children" of the transformed rules, and can be added
/// with [`add_child_nonterminal()`](SymbolTable::add_child_nonterminal). The name
/// is the parent's name followed by "_<number>". For example, adding a child to
/// nonterminal 0 creates
///
/// ```ignore
/// nt[2] = "expr_1".to_string()
/// ```
///
#[derive(Clone, Debug)]
pub struct SymbolTable {
    t: Vec<(String, Option<String>)>,   // terminal identifiers and optional representation
    fixer_t: NameFixer,                 // keeps terminal identifiers unique
    nt: Vec<String>,                    // nt to nonterminal identifier
    #[cfg(any())]
    names: HashMap<String, VarId>,      // nonterminal identifier to nt
    fixer_nt: NameFixer,                // keeps nonterminal identifiers unique
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            t: Vec::new(),
            fixer_t: NameFixer::new_empty(),
            nt: Vec::new(),
            #[cfg(any())]
            names: HashMap::new(),
            fixer_nt: NameFixer::new_empty(),
        }
    }

    pub fn to_fixed_sym_table(self) -> FixedSymTable {
        FixedSymTable::new(self.t, self.nt)
    }

    // -------------------------------------------------------------------------

    pub fn add_terminal<T: Into<String>>(&mut self, name: T, name_maybe: Option<T>) -> TokenId {
        let token = self.t.len();
        assert!(token < TokenId::MAX as usize);
        let unique_name = self.fixer_t.get_unique_name(name.into());
        self.t.push((unique_name, name_maybe.map(|n| n.into())));
        token as TokenId
    }

    pub fn extend_terminals<I: IntoIterator<Item=(T, Option<T>)>, T: Into<String>>(&mut self, terminals: I) {
        for (s, maybe) in terminals {
            self.add_terminal(s, maybe);
        }
    }

    pub fn get_terminals(&self) -> impl Iterator<Item = &(String, Option<String>)> {
        self.t.iter()
    }

    pub fn get_num_t(&self) -> usize {
        self.t.len()
    }

    pub fn get_t_name(&self, token: TokenId) -> String {
        if token as usize >= self.t.len() {
            format!("??T({token})")
        } else {
            self.t[token as usize].0.clone()
        }
    }

    pub fn set_t_name(&mut self, token: TokenId, name_maybe: Option<String>) {
        self.t[token as usize].1 = name_maybe
    }

    // -------------------------------------------------------------------------

    fn add_nt(&mut self, unique_name: String) -> VarId {
        let var = self.nt.len();
        assert!(var < VarId::MAX as usize);
        #[cfg(any())]
        self.names.insert(unique_name.clone(), var as VarId);
        self.nt.push(unique_name);
        var as VarId
    }

    pub fn add_nonterminal<T: Into<String>>(&mut self, name: T) -> VarId {
        let unique_name = self.fixer_nt.get_unique_name(name.into());
        self.add_nt(unique_name)
    }

    pub fn add_child_nonterminal(&mut self, var: VarId) -> VarId {
        let unique_name = self.fixer_nt.get_unique_name_unum(self.nt[var as usize].clone());
        self.add_nt(unique_name)
    }
    
    pub fn extend_nonterminals<I: IntoIterator<Item=T>, T: Into<String>>(&mut self, nonterminals: I) {
        for s in nonterminals {
            self.add_nonterminal(s);
        }
    }

    #[cfg(any())]
    pub fn find_nonterminal(&self, name: &str) -> Option<VarId> {
        self.names.get(name).cloned()
    }

    pub fn get_nonterminals(&self) -> impl Iterator<Item = &String> {
        self.nt.iter()
    }

    pub fn get_num_nt(&self) -> usize {
        self.nt.len()
    }

    pub fn remove_nonterminal(&mut self, v: VarId) {
        let name = self.nt.remove(v as usize);
        #[cfg(any())]
        {
            for old_v in self.names.values_mut() {
                if *old_v >= v { *old_v -= 1; }
            }
            self.names.remove(&name);
        }
        self.fixer_nt.remove(&name);
    }

    pub fn set_nt_name(&mut self, var: VarId, name: String) {
        self.nt[var as usize] = name;
    }

    /// Removes the name assigned to NT `var` and returns it. Internally, the name of the NT is
    /// replaced by another unique string. The NT is expected to be removed later.
    pub fn remove_nt_name(&mut self, var: VarId) -> String {
        let mut removed = self.fixer_nt.get_unique_name_num(format!("{var}_removed"));
        std::mem::swap(&mut self.nt[var as usize], &mut removed);
        #[cfg(any())]
        self.names.remove(&removed);
        self.fixer_nt.remove(&removed);
        removed
    }
}

impl SymInfoTable for SymbolTable {
    fn is_token_data(&self, token: TokenId) -> bool {
        self.t[token as usize].1.is_none()
    }

    fn is_symbol_t_data(&self, symbol: &Symbol) -> bool {
        if let Symbol::T(token) = symbol {
            self.t[*token as usize].1.is_none()
        } else {
            false
        }
    }

    fn get_t_str(&self, token: TokenId) -> String {
        match token {
            _ if (token as usize) < self.t.len() => {
                let (name, literal) = &self.t[token as usize];
                literal.as_ref().unwrap_or(name).clone()
            }
            TokenId::MAX => "<bad character>".to_string(),
            _ => format!("T({token}?)")
        }
    }

    fn get_nt_name(&self, var: VarId) -> String {
        if var as usize >= self.nt.len() { return format!("??NT({var})") }
        self.nt[var as usize].clone()
    }

    fn get_name(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Empty | Symbol::End => symbol.to_string(),
            Symbol::T(token) => self.get_t_str(*token),
            Symbol::NT(var) => self.get_nt_name(*var),
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn general() {
        let mut st = SymbolTable::new();
        st.extend_nonterminals(["A".to_string(), "NOT_USED".to_string(), "E".to_string()]);
        assert_eq!(st.add_child_nonterminal(0), 3);
        assert_eq!(st.get_name(&Symbol::NT(3)), "A_1");
        assert_eq!(st.add_child_nonterminal(0), 4);
        assert_eq!(st.get_name(&Symbol::NT(4)), "A_2");
        assert_eq!(st.add_child_nonterminal(2), 5);
        assert_eq!(st.get_name(&Symbol::NT(5)), "E_1");
        assert_eq!(st.add_child_nonterminal(1), 6);
        st.remove_nonterminal(1);
        assert_eq!(st.get_name(&Symbol::NT(2)), "A_1");
        assert_eq!(st.get_name(&Symbol::NT(3)), "A_2");
        assert_eq!(st.get_name(&Symbol::NT(4)), "E_1");
        #[cfg(any())]
        assert_eq!(st.find_nonterminal("A_1"), Some(2));
        assert!(st.nt.contains(&"A_2".to_string()));
        #[cfg(any())]
        assert!(st.names.contains_key("A_2"));
        assert!(st.fixer_nt.contains("A_2"));
        st.remove_nt_name(3);
        assert!(!st.nt.contains(&"A_2".to_string()));
        #[cfg(any())]
        assert!(!st.names.contains_key("A_2"));
        assert!(!st.fixer_nt.contains("A_2"));
    }
}