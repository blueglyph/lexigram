// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::HashSet;

/// Dictionary-based helper that adapts names to guarantee they're unique.
#[derive(Clone, Debug)]
pub struct NameFixer {
    dic: HashSet<String>
}

impl NameFixer {
    const RUST_KEYWORDS: [&'static str; 51] = [
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in",
        "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super",
        "trait", "true", "type", "unsafe", "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box",
        "do", "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try"];

    /// Creates a new instance of [NameFixer] pre-filled with reserved words of the Rust language, so that variables
    /// don't have a forbidden name.
    ///
    /// See [new_empty()](NameFixer::new_empty) for a version that isn't pre-filled with reserved words.
    pub fn new() -> Self {
        let mut dic = HashSet::<String>::new();
        dic.extend(Self::RUST_KEYWORDS.iter().map(|s| s.to_string()));
        NameFixer { dic }
    }

    /// Creates a new instance of [NameFixer] that is not pre-filled with Rust reserved words. This name fixer can be used
    /// when the names will be prefixed or postfixed in a way that guarantees they're not forbidden.
    ///
    /// See [new()](NameFixer::new) for the safer version pre-filled with reserved words.
    pub fn new_empty() -> Self {
        let dic = HashSet::<String>::new();
        NameFixer { dic }
    }

    /// Adds a name to the internal dictionary without checking whether it already existed or not. Use this
    /// method to pre-fill existing names.
    pub fn add(&mut self, name: String) {
        self.dic.insert(name);
    }

    /// Removes a name from the internal dictionary. Returns `true` if the name was in the dictionary.
    pub fn remove(&mut self, name: &str) -> bool {
        self.dic.remove(name)
    }

    /// Checks if a name is already in the internal dictionary.
    pub fn contains(&self, name: &str) -> bool {
        self.dic.contains(name)
    }

    /// Returns `name` if it's unique, or adds a suffix number first to make sure it's unique.
    pub fn get_unique_name(&mut self, mut name: String) -> String {
        let len = name.len();
        let mut index = 0;
        while self.dic.contains(&name) {
            name.truncate(len);
            index += 1;
            Self::add_number(&mut name, index);
        }
        self.dic.insert(name.clone());
        name
    }

    /// Returns `name` followed by "1" if it's unique, or adds another incremental suffix number to make sure it's unique.
    pub fn get_unique_name_num(&mut self, mut name: String) -> String {
        let len = name.len();
        let mut index = 1;
        Self::add_number(&mut name, index);
        while self.dic.contains(&name) {
            name.truncate(len);
            index += 1;
            Self::add_number(&mut name, index);
        }
        self.dic.insert(name.clone());
        name
    }

    /// Returns `name` followed by "_1" if it's unique, or finds another incremental suffix number to make sure it's unique.
    pub fn get_unique_name_unum(&mut self, mut name: String) -> String {
        name.push('_');
        self.get_unique_name_num(name)
    }

    /// Adds `_{num}` or `{num}` to the string depending on its last character, whether it's respectivelly a digit or not.
    /// Used if we want to make sure a digit isn't added to an identifier ending with a number, which would make it confusing.
    pub fn add_number(s: &mut String, num: usize) {
        if s.ends_with(|c: char| c.is_digit(10)) {
            s.push('_');
        }
        s.push_str(&format!("{num}"));
    }
}

/// Transforms names into CamelCase or underscore_parts (lower or upper case)
pub trait NameTransformer {
    /// Transforms the string or string slice into a string with the camelcase equivalent.
    /// ```
    /// # use lexigram::NameTransformer;
    /// assert_eq!("statement".to_camelcase(), "Statement");
    /// assert_eq!("NUM_VAL".to_camelcase(), "NumVal");
    /// assert_eq!("expr_1".to_string().to_camelcase(), "Expr1");
    /// ```
    fn to_camelcase(&self) -> String;

    /// Transforms the camelcase string slice into a string with lowercase words separated by underscores.
    /// Note that numbers are not separated.
    /// ```
    /// # use lexigram::NameTransformer;
    /// assert_eq!("NumVal".to_underscore_lowercase(), "num_val");
    /// assert_eq!("Expr1".to_underscore_lowercase(), "expr1");
    /// assert_eq!("XAndY".to_string().to_underscore_lowercase(), "x_and_y");
    /// ```
    fn to_underscore_lowercase(&self) -> String;

    /// Transforms the camelcase string or string slice into a string with uppercase words separated by underscores.
    /// Note that numbers are not separated.
    /// ```
    /// # use lexigram::NameTransformer;
    /// assert_eq!("NumVal".to_underscore_uppercase(), "NUM_VAL");
    /// assert_eq!("Expr1".to_underscore_uppercase(), "EXPR1");
    /// assert_eq!("XAndY".to_string().to_underscore_uppercase(), "X_AND_Y");
    /// ```
    fn to_underscore_uppercase(&self) -> String;
}

impl NameTransformer for str {
    fn to_camelcase(&self) -> String {
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

    fn to_underscore_lowercase(&self) -> String {
        let mut result = String::new();
        for c in self.chars() {
            if !result.is_empty() && c.is_ascii_uppercase() {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        }
        result
    }

    fn to_underscore_uppercase(&self) -> String {
        let mut result = String::new();
        for c in self.chars() {
            if !result.is_empty() && c.is_ascii_uppercase() {
                result.push('_');
            }
            result.push(c.to_ascii_uppercase());
        }
        result
    }
}
