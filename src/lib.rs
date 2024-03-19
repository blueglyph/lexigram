// Copyright 2023 Redglyph

mod vectree;
mod take_until;
mod macros;
pub mod io;
pub mod lexgen;
pub mod dfa;

// Regular expressions / DFA, See:
// - https://blog.burntsushi.net/regex-internals/
// - https://en.wikipedia.org/wiki/Tagged_Deterministic_Finite_Automaton
// - https://arxiv.org/abs/2206.01398
//
// See also:
// - Ref: https://en.wikipedia.org/wiki/Comparison_of_parser_generators
//
// Misc
// - https://re2c.org/
// - https://www.genivia.com/get-reflex.html
// - https://nothings.org/computer/lexing.html
// - https://github.com/maciejhirsz/logos
// - https://alic.dev/blog/fast-lexing
