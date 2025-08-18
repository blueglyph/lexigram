// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::collections::BTreeSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use vectree::VecTree;

mod macros;
mod take_until;
mod cproduct;
pub mod segments;
pub mod io;
pub mod dfa;
pub mod lexergen;
pub mod lexer;
pub mod lexi;
pub mod grammar;
pub mod parsergen;
pub mod parser;
pub mod log;
pub mod test_tools;

mod name_fixer;
pub use name_fixer::{NameFixer, NameTransformer};
mod symbol_table;
mod fixed_sym_table;

pub use symbol_table::SymbolTable;
pub use fixed_sym_table::{FixedSymTable, SymInfoTable};
use crate::log::{BufLog, LogStatus};

// package name & version
pub const LIB_PKG_NAME: &str = env!("CARGO_PKG_NAME");
pub const LIB_PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

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
#[derive(Clone, Debug)]
pub struct Normalized;

#[derive(Clone, Debug)]
pub enum BuildErrorSource {
    RuleTreeSet,
    Dfa,
    DfaBuilder,
    LexerGen,
    Lexi,
    ProdRuleSet,
    ParserGen,
    Gram,
}

#[derive(Clone, Debug)]
pub struct BuildError {
    log: BufLog,
    source: BuildErrorSource,
}

impl BuildError {
    pub fn new(log: BufLog, source: BuildErrorSource) -> Self {
        BuildError { log, source }
    }
}

impl Display for BuildError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Errors have occurred in {:?}:\n{}", self.source, self.log.get_messages_str())
    }
}

impl Error for BuildError {
}

pub trait HasBuildErrorSource {
    const SOURCE: BuildErrorSource;

    fn get_build_error_source() -> BuildErrorSource {
        Self::SOURCE
    }
}

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

pub fn escape_string(s: &str) -> String {
    s.chars().map(|c| escape_char(c)).collect::<String>()
}

pub(crate) fn vaddi<I, T>(v: &mut Vec<Vec<T>>, item: I) -> usize
    where I: IntoIterator<Item=T> + Clone
{
    let new_index = v.len();
    v.push(Vec::from_iter(item));
    new_index
}

/// Takes `lines` of columns and outputs lines of strings in which the columns
/// are aligned. The minimum width of each column can be preset with the optional `min_widths` vector.
///
/// The final width of each column is the 1 + maximum number of characters - not bytes - of the strings
/// representing that column in all the lines (the +1 makes sure columns are separated by at least one
/// space). The last column is left as-is; no spaces are added to adjust its width.
pub fn columns_to_str(lines: Vec<Vec<String>>, min_widths: Option<Vec<usize>>) -> Vec<String> {
    let min_widths = min_widths.unwrap_or(vec![0; lines.get(0).map(|v| v.len()).unwrap_or(0)]);
    let ncol = min_widths.len();
    let mut width = lines.iter().fold(min_widths, |acc, s| {
        assert_eq!(s.len(), ncol, "number of columns is not consistently {ncol}");
        acc.into_iter().zip(s).map(|(a, s)| a.max(s.charlen() + 1)).collect()
    });
    if let Some(x) = width.last_mut() { *x = 0 };
    lines.into_iter().map(|v| v.into_iter().zip(&width).map(|(mut s, w)| {
        for _ in 0..w.saturating_sub(s.charlen()) { s.push(' ') }
        s
    }).collect::<String>()).collect()
}

pub(crate) fn indent_source(parts: Vec<Vec<String>>, indent: usize) -> String {
    let s = String::from_utf8(vec![32; indent]).unwrap();
    let mut source = String::new();
    let mut first = true;
    for part in parts {
        if !first {
            source.push('\n');
        }
        first = false;
        for string in part {
            for line in string.split("\n") {
                let cured_line = line.trim_end();
                if cured_line.len() > 0 {
                    source.push_str(&s);
                }
                source.push_str(cured_line);
                source.push('\n');
            }
        }
    }
    source
}

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
    /// Returns the length in characters (not bytes).
    fn charlen(&self) -> usize;
}

impl CharLen for str {
    fn charlen(&self) -> usize {
        self.chars().count()
    }
}

// ---------------------------------------------------------------------------------------------
// Source generation helper traits and types

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

#[derive(Debug)]
struct StructLibs {
    libs: BTreeSet<String>
}

impl StructLibs {
    pub fn new() -> Self {
        StructLibs { libs: BTreeSet::new() }
    }

    pub fn add(&mut self, lib: &str) {
        self.libs.insert(lib.to_string());
    }

    pub fn extend<I: IntoIterator<Item=J>, J: Into<String>>(&mut self, libs: I) {
        self.libs.extend(libs.into_iter().map(|s| s.into()));
    }

    #[cfg(test)]
    fn tree_to_string(t: &VecTree<String>, idx: usize) -> String {
        let children = t.children(idx).iter().map(|child| {
            if t.children(*child).len() > 0 {
                format!("{}[{}]", t.get(*child), Self::tree_to_string(t, *child))
            } else {
                t.get(*child).to_string()
            }
        }).to_vec();
        children.join(", ")
    }

    fn to_tree(&self) -> VecTree<String> {
        let mut tree = VecTree::new();
        let root = tree.add_root(String::new());
        for lib in &self.libs {
            let mut idx = root;
            for md in lib.split("::") {
                idx = tree.children(idx).iter()
                    .find_map(|&i| {
                        if tree.get(i) == md { Some(i) } else { None }
                    })
                    .unwrap_or_else(|| {
                        tree.add(Some(idx), md.to_string())
                    });
            }
            tree.add(Some(idx), "self".to_string());
        }
        tree
    }

    pub fn gen_source_code(&self) -> Vec<String> {
        let mut stack = Vec::<Vec<String>>::new();
        let tree = self.to_tree();
        for node in tree.iter_depth_simple() {
            if node.depth as usize >= stack.len() && node.depth > 0 {
                while node.depth as usize > stack.len() {
                    stack.push(vec![]);
                }
                stack.last_mut().unwrap().push(node.to_string())
            } else if node.depth > 0 {
                let sub = stack.pop().unwrap();
                stack.last_mut().unwrap().push(
                    if sub.len() > 1 {
                        format!("{}::{{{}}}", node.deref(), sub.join(", "))
                    } else if sub.last().unwrap() == "self" {
                        format!("{}", node.deref())
                    } else {
                        format!("{}::{}", node.deref(), sub[0])
                    });
            }
        }
        stack.pop().unwrap_or(vec![]).into_iter().map(|s| format!("use {s};")).to_vec()
    }
}

// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod libtests {
    use super::*;
    use crate::log::{Logger, BufLog};

    #[test]
    fn test_column_to_str() {
        let a = vec![
            vec!["1".to_string(), "2".to_string()],
            vec!["◄10".to_string(), "20".to_string()],
            vec!["100".to_string(), "200".to_string()],
        ];
        let b = vec![
            vec!["1".to_string(), "◄20".to_string(), "3000".to_string()],
            vec!["10".to_string(), "2000".to_string(), "3".to_string()],
            vec!["1000".to_string(), "2".to_string(), "300".to_string()],
        ];
        let tests = vec![
            (a.clone(), Some(vec![2, 0]), "1   2\n◄10 20\n100 200"),
            (a, None, "1   2\n◄10 20\n100 200"),
            (b.clone(), Some(vec![3, 2, 0]), "1    ◄20  3000\n10   2000 3\n1000 2    300"),
            (b, Some(vec![8, 2, 0]), "1       ◄20  3000\n10      2000 3\n1000    2    300"),
            (vec![], Some(vec![]), ""),
            (vec![], Some(vec![1, 2, 3]), ""),
            (vec![], None, ""),
        ];
        for (i, (v, w, expected)) in tests.into_iter().enumerate() {
            let result_v = columns_to_str(v, w);
            let result = result_v.join("\n");
            assert_eq!(result, expected, "failed with test {i}")
        }
    }

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

    #[test]
    fn test_struct_libs() {
        const VERBOSE: bool = false;
        let mut l = StructLibs::new();
        let l1 = ["a", "a::a1", "a::b1", "a::a1::a2", "a::a1::b2"];
        let l2 = vec!["a::a1::a2::a3"];
        let l3 = vec!["a::c1::a2".to_string()];
        l.extend(l1);
        l.add("b");
        l.extend(l2);
        l.extend(l3);
        let tree = l.to_tree();
        if VERBOSE {
            println!("{}", tree.iter_depth_simple().map(|n| format!("({}){}", n.depth, n.deref())).join(", "));
            println!("{}", StructLibs::tree_to_string(&tree, tree.get_root().unwrap()));
        }
        let src = l.gen_source_code();
        if VERBOSE {
            println!("{}", src.join("\n"));
        }
        assert_eq!(src, ["use a::{self, a1::{self, a2::{self, a3}, b2}, b1, c1::a2};", "use b;"]);

        let src_empty = StructLibs::new().gen_source_code();
        assert_eq!(src_empty, Vec::<String>::new());
    }

    #[test]
    fn test_build_error() {
        fn build() -> Result<(), BuildError> {
            let mut log = BufLog::new();
            log.add_error("the test generated a fake error successfully");
            Err(BuildError { source: BuildErrorSource::ParserGen, log })
        }
        let err = build().err().expect("build() should return an error");
        assert_eq!(err.to_string(), "Errors have occurred in ParserGen:\n- ERROR  : the test generated a fake error successfully\n");
    }
}

// ---------------------------------------------------------------------------------------------
