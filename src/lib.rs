// Copyright 2023 Redglyph

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
// General types

/// Marker for general tree form (not normalized).
///
/// - For `Dfa`, this form may have accepting states with IDs smaller than non-accepting states' IDs, or non-incremntal IDs.
/// - For `RuleTreeSet`, this form may include any operators like `*`, `+`, and `?`, and doesn't have a restriction on depth.
pub struct General;
/// Marker for normalized form.
///
/// - For `Dfa`, this form always has incremental state numbers, starting at 0, with all the accepting states at the end.
/// - For `RuleTreeSet`, this form only has `|`, `&`, and symbols, and must have one of the 3 following patterns:
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

pub(crate) trait CollectJoin {
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
}

// ---------------------------------------------------------------------------------------------
