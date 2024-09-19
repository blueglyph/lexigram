// Copyright 2023 Redglyph

use std::collections::HashSet;

mod macros;
mod take_until;
mod cproduct;
mod vectree;
pub mod segments;
pub mod io;
pub mod dfa;
pub mod lexgen;
pub mod lexer;
pub mod regexgen;
pub mod grammar;
pub mod symbol_table;
pub mod parsergen;
pub mod parser;
pub mod log;

// Regular expressions / DFA, See:
// - https://blog.burntsushi.net/regex-internals/
// - https://en.wikipedia.org/wiki/Tagged_Deterministic_Finite_Automaton
// - https://arxiv.org/abs/2206.01398
//
// See also:
// - Ref: https://en.wikipedia.org/wiki/Comparison_of_parser_generators
//
// UTF-8:
// - https://www.ibm.com/docs/en/db2/11.5?topic=support-unicode-character-encoding
// - std::char::encode_utf8_raw(...)
//
// Misc
// - https://re2c.org/
// - https://www.genivia.com/get-reflex.html
// - https://nothings.org/computer/lexing.html
// - https://github.com/maciejhirsz/logos
// - https://alic.dev/blog/fast-lexing

// ---------------------------------------------------------------------------------------------
// Shared types

/// Unit type used as generic parameter to indicate general, non-normalized form.
///
/// - `Dfa<General>` may have accepting states with IDs smaller than non-accepting states' IDs, or non-incremntal IDs.
/// - `RuleTreeSet<General>` may include any operators like `*`, `+`, and `?`, and doesn't have a restriction on depth.
/// - `ProdRuleSet<General>` may be ambiguous, left-recursive, and/or need left factorization, depending on the target.
#[derive(Clone, Debug)]
pub struct General;

/// - `ProdRuleSet<LR>` have no ambiguity.
#[derive(Clone, Debug)]
pub struct LR;

/// - `ProdRuleSet<LL>` aren't left-recursive and are left-factorized.
#[derive(Clone, Debug)]
pub struct LL1;

/// Unit type used as generic parameter to indicate normalized form.
///
/// - `Dfa<Normalized>` always has incremental state numbers, starting at 0, with all the accepting states at the end.
/// - `RuleTreeSet<Normalized>` only has `|`, `&`, and symbols, and must have one of the 3 following patterns:
///   - a symbol
///   - a `&` with only symbols as children
///   - a `|` with only `&(symbols)` or symbols as children
pub struct Normalized;

// ---------------------------------------------------------------------------------------------
// General helper functions

pub(crate) fn escape_char(c: char) -> String {
    match c {
        // '\x00'..='\x7f' => c.escape_debug().to_string(),
              '\u{0}' => "MIN".to_string(),
           '\u{d7ff}' => "LOW_MAX".to_string(),
           '\u{e000}' => "HIGH_MIN".to_string(),
         '\u{10ffff}' => "MAX".to_string(),
        // '\u{f7ff}' | '\u{e000}' | '\u{10ffff}' => c.escape_unicode().to_string(),
        _ => c.escape_debug().to_string(),
    }
}

pub(crate) fn escape_string(s: &str) -> String {
    s.chars().map(|c| escape_char(c)).collect::<String>()
}

#[allow(unused)]
pub(crate) fn vadd<T>(v: &mut Vec<T>, item: T) -> usize {
    let new_index = v.len();
    v.push(item);
    new_index
}

pub(crate) fn vaddi<I, T>(v: &mut Vec<Vec<T>>, item: I) -> usize
    where I: IntoIterator<Item=T> + Clone
{
    let new_index = v.len();
    v.push(Vec::from_iter(item));
    new_index
}

// fn vadd<T>(v: &mut Vec<Vec<Dup>>, item: T) -> usize where T: IntoIterator<Item=Dup> + Clone {
//     let new_index = v.len();
//     if VERBOSE_CC { print!("_{}=dup [{}], ", new_index, item.clone().into_iter().map(|i| i.peek().to_string()).join(", ")); }
//     v.push(Vec::from_iter(item));
//     new_index
// }


// ---------------------------------------------------------------------------------------------
// General helper traits

pub trait CollectJoin {
    fn join(&mut self, separator: &str) -> String
        where Self: Iterator,
              <Self as Iterator>::Item: ToString
    {
        self.map(|x| x.to_string()).collect::<Vec<_>>().join(separator)
    }

    fn to_vec(self) -> Vec<<Self as Iterator>::Item>
        where Self: Iterator + Sized
    {
        self.collect::<Vec<_>>()
    }
}

impl<I: Iterator> CollectJoin for I {}

pub trait CharLen {
    fn charlen(&self) -> usize;
}

impl CharLen for String {
    fn charlen(&self) -> usize {
        self.chars().count()
    }
}

/// Dictionary-based helper that adapts names so they are unique
pub struct NameFixer {
    dic: HashSet<String>
}

impl NameFixer {
    pub fn new() -> Self {
        NameFixer { dic: HashSet::new() }
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
    /// # use rlexer::NameTransformer;
    /// assert_eq!("statement".to_camelcase(), "Statement");
    /// assert_eq!("NUM_VAL".to_camelcase(), "NumVal");
    /// assert_eq!("expr_1".to_string().to_camelcase(), "Expr1");
    /// ```
    fn to_camelcase(&self) -> String;

    /// Transforms the camelcase string slice into a string with lowercase words separated by underscores.
    /// Note that numbers are not separated.
    /// ```
    /// # use rlexer::NameTransformer;
    /// assert_eq!("NumVal".to_underscore_lowercase(), "num_val");
    /// assert_eq!("Expr1".to_underscore_lowercase(), "expr1");
    /// assert_eq!("XAndY".to_string().to_underscore_lowercase(), "x_and_y");
    /// ```
    fn to_underscore_lowercase(&self) -> String;

    /// Transforms the camelcase string or string slice into a string with uppercase words separated by underscores.
    /// Note that numbers are not separated.
    /// ```
    /// # use rlexer::NameTransformer;
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

/// Adds empty lines between blocks of text
pub trait SourceSpacer {
    /// Adds an empty string to the vector, but only if the last vector string isn't empty. If the vector is
    /// empty, doesn't add anything.
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

#[cfg(test)]
mod libtests {
    use super::*;

    #[test]
    fn test_col_to_string() {
        let x = std::collections::BTreeSet::<u32>::from([10, 20, 25]);
        assert_eq!(x.iter().join(", "), "10, 20, 25");
    }

    #[test]
    fn test_to_vec() {
        assert_eq!((0..5).to_vec(), vec![0, 1, 2, 3, 4]);
    }

    #[test]
    fn test_charlen() {
        assert_eq!("".to_string().charlen(), 0);
        assert_eq!("12345".to_string().charlen(), 5);
        assert_eq!("◄123►".to_string().charlen(), 5);
    }

    #[test]
    fn test_name_fixer() {
        let mut fixer = NameFixer::new();
        assert_eq!(fixer.get_unique_name("a".to_string()), "a");
        assert_eq!(fixer.get_unique_name("a".to_string()), "a1");
        assert_eq!(fixer.get_unique_name("b".to_string()), "b");
        assert_eq!(fixer.get_unique_name("a".to_string()), "a2");
        assert_eq!(fixer.get_unique_name("U2".to_string()), "U2");
        assert_eq!(fixer.get_unique_name("U2".to_string()), "U2_1");
    }

    #[test]
    fn test_to_camel_case() {
        let tests = vec![
            ("A", "A"),
            ("AA", "Aa"),
            ("AB1", "Ab1"),
            ("A_1", "A1"),
            ("NUM_VAL", "NumVal"),
            ("a", "A"),
            ("ab_cd_ef", "AbCdEf"),
        ];
        for (str, expected) in tests {
            let result = str.to_string().to_camelcase();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_to_underscore() {
        let tests = vec![
            ("a", "a", "A"),
            ("AA", "a_a", "A_A"),
            ("A1", "a1", "A1"),
            ("aB1", "a_b1", "A_B1"),
            ("A", "a", "A"),
            ("AbCdEf", "ab_cd_ef", "AB_CD_EF"),
            ("ANewTest", "a_new_test", "A_NEW_TEST"),
        ];
        for (str, expected_lower, expected_upper) in tests {
            let result_lower = str.to_string().to_underscore_lowercase();
            let result_upper = str.to_string().to_underscore_uppercase();
            assert_eq!(result_lower, expected_lower);
            assert_eq!(result_upper, expected_upper);
        }
    }

    #[test]
    fn test_add_space() {
        let mut src = Vec::<String>::new();
        src.add_space();
        assert!(src.is_empty());
        src.push("1".to_string());
        assert_eq!(src, vec!["1".to_string()]);
        src.add_space();
        assert_eq!(src, vec!["1".to_string(), "".to_string()]);
        src.add_space();
        assert_eq!(src, vec!["1".to_string(), "".to_string()]);
        src.push("2".to_string());
        assert_eq!(src, vec!["1".to_string(), "".to_string(), "2".to_string()]);
        src.add_space();
        assert_eq!(src, vec!["1".to_string(), "".to_string(), "2".to_string(), "".to_string()]);
        src.add_space();
        assert_eq!(src, vec!["1".to_string(), "".to_string(), "2".to_string(), "".to_string()]);
    }
}

// ---------------------------------------------------------------------------------------------
