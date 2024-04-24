// Copyright 2023 Redglyph

mod macros;
mod take_until;
mod vectree;
pub mod segments;
pub mod io;
pub mod dfa;
pub mod lexgen;
pub mod lexer;

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

pub(crate) trait CollectJoin {
    fn join(&mut self) -> String;
}

impl<T: std::fmt::Display, I: Iterator<Item=T>> CollectJoin for I {
    fn join(&mut self) -> String {
        self.map(|x| x.to_string()).collect::<Vec<_>>().join(", ")
    }
}

#[test]
fn test_col_to_string() {
    let x = std::collections::BTreeSet::<u32>::from([10, 20, 25]);
    assert_eq!(x.iter().join(), "10, 20, 25");
}
