// Copyright 2023 Redglyph

mod vectree;
mod take_until;
mod macros;
pub mod io;
pub mod lexgen;
pub mod dfa;
pub mod scan;
mod intervals;

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
        '\x00'..='\x7f' => c.escape_default().to_string(),
        _ => c.to_string(),
    }
}

pub(crate) fn escape_string(s: &str) -> String {
    s.chars().map(|c| escape_char(c)).collect::<String>()
}
