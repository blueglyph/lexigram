// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use crate::parser::Symbol;
use crate::{TokenId, VarId};

/// Stores the names of the terminal and nonterminal symbols used by a parser.
///
/// Terminals are defined in the lexicon. They have two parts to their name:
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
/// nt[2] = "expr_1".to_string(); // generated when removing the left recursion
/// ```
#[derive(Clone, Debug)]
pub struct FixedSymTable {
    t: Vec<(String, Option<String>)>,   // terminal identifiers and optional representation
    nt: Vec<String>,                    // nt to nonterminal identifier
}

impl FixedSymTable {
    pub fn new(t: Vec<(String, Option<String>)>, nt: Vec<String>) -> Self {
        FixedSymTable { t, nt }
    }

    // -------------------------------------------------------------------------

    pub fn get_terminals(&self) -> impl Iterator<Item = &(String, Option<String>)> {
        self.t.iter()
    }

    pub fn get_num_t(&self) -> usize {
        self.t.len()
    }

    // -------------------------------------------------------------------------

    pub fn get_nonterminals(&self) -> impl Iterator<Item = &String> {
        self.nt.iter()
    }

    pub fn get_num_nt(&self) -> usize {
        self.nt.len()
    }

    // -------------------------------------------------------------------------

    #[cfg(test)]
    pub fn dump(&self, title: &str) {
        if !title.is_empty() {
            println!("{title}");
        }
        println!(
            "- nonterminals:\n{}",
            self.get_nonterminals().enumerate().map(|(v, s)| format!("  - NT[{v}]: {s}")).collect::<Vec<_>>().join("\n"));
        println!(
            "- terminals:\n{}",
            self.get_terminals().enumerate()
                .map(|(t, (n, v_maybe))| format!("  - T[{t}]: {n}{}", if let Some(v) = v_maybe { format!(" = {v:?}") } else { String::new() }))
                .collect::<Vec<_>>().join("\n"));
    }
}

pub trait SymInfoTable {
    /// Does `Symbol::T(token)` hold lexer string data?
    ///
    /// Terminals are divided into two categories: fixed and variable content. When the
    /// terminal is defined with choices and ranges of characters, like `ID: [a-z]+`, it
    /// contains variable content: data like the ID specifier.
    fn is_token_data(&self, token: TokenId) -> bool;

    /// Is `symbol` a terminal holding lexer string data?
    ///
    /// Terminals are divided into two categories: fixed and variable content. When the
    /// terminal is defined with choices and ranges of characters, like `ID: [a-z]+`, it
    /// contains variable content: data like the ID specifier.
    fn is_symbol_t_data(&self, symbol: &Symbol) -> bool;

    fn is_symbol_t_fixed(&self, symbol: &Symbol) -> bool;

    fn get_t_str(&self, token: TokenId) -> String;

    fn get_t_name(&self, token: TokenId) -> String;

    fn get_nt_name(&self, var: VarId) -> String;

    /// Gets the symbol's name: the nonterminal identifier, the terminal identifier,
    /// or "ε", "$", ...
    fn get_name(&self, symbol: &Symbol) -> String;

    /// Gets the symbol's representation string: the nonterminal identifier, the
    /// terminal string value (if it exists), or "ε", "$", ...
    fn get_str(&self, symbol: &Symbol) -> String;

    fn get_name_quote(&self, symbol: &Symbol) -> String;
}

impl SymInfoTable for FixedSymTable {
    fn is_token_data(&self, token: TokenId) -> bool {
        self.t[token as usize].1.is_none()
    }

    fn is_symbol_t_data(&self, symbol: &Symbol) -> bool {
        if let Symbol::T(token) = symbol {
            self.t.get(*token as usize).map(|t| t.1.is_none()).unwrap_or(false)
        } else {
            false
        }
    }

    fn is_symbol_t_fixed(&self, symbol: &Symbol) -> bool {
        if let Symbol::T(token) = symbol {
            self.t.get(*token as usize).map(|t| t.1.is_some()).unwrap_or(false)
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

    fn get_t_name(&self, token: TokenId) -> String {
        if token as usize >= self.t.len() {
            format!("T({token}?)")
        } else {
            self.t[token as usize].0.clone()
        }
    }

    fn get_nt_name(&self, var: VarId) -> String {
        if var as usize >= self.nt.len() { return format!("NT({var}?)") }
        self.nt[var as usize].clone()
    }

    fn get_name(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Empty | Symbol::End => symbol.to_string(),
            Symbol::T(token) => self.get_t_name(*token),
            Symbol::NT(var) => self.get_nt_name(*var),
        }
    }

    fn get_str(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Empty | Symbol::End => symbol.to_string(),
            Symbol::T(token) => self.get_t_str(*token),
            Symbol::NT(var) => self.get_nt_name(*var),
        }
    }

    fn get_name_quote(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Empty | Symbol::End => symbol.to_string(),
            Symbol::T(token) => if self.is_symbol_t_fixed(symbol) { format!("{:?}", self.get_t_str(*token)) } else { self.get_t_str(*token) },
            Symbol::NT(var) => self.get_nt_name(*var),
        }
    }
}
