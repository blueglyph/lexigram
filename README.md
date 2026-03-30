[![crate](https://img.shields.io/crates/v/lexigram_lib.svg)](https://crates.io/crates/lexigram-lib)
[![documentation](https://docs.rs/lexigram-lib/badge.svg)](https://docs.rs/lexigram-lib)
[![crate](https://img.shields.io/crates/l/lexigram_lib.svg)](https://github.com/blueglyph/lexigram/blob/master/LICENSE-MIT)

<hr/>

# Lexigram

This crate is part of the [Lexigram](https://github.com/blueglyph/lexigram) project, a lexer/parser generator.

Distinctive features:
* Predictive, non-recursive LL(1) parser.
* Automatic transformation of left-recursive and ambiguous rules, with automatic left factorization.
* Grammar-driven listener pattern for better flexibility and to avoid mixing grammar with Rust source code.
* Listener methods called directly during parsing, rather than after reconstructing a concrete syntax tree.
* Low-latency variants for right-recursive rules and `*`/`+` repetitions.
* Able to parse continuous input streams.

You can dive into the [Lexigram book](https://www.unscript.net/lexigram-book) for an introduction, a tutorial, or a reference. You can also browse the [crate documentation](https://docs.rs/lexigram-lib/latest/lexigram_lib/).

If you're looking for the command-line executable, you'll find it in the [`lexigram`](https://crates.io/crates/lexigram) crate, along with a quick overview.

# The `lexigram-lib` Crate

The [`lexigram-lib`](https://crates.io/crates/lexigram-lib) crate contains the low-level code that generates a lexer and a parser.

It's normally not meant to be used for other purposes than a dependency of the following crates, unless you want to create the lexicon and the grammar programmatically.
* [`lexigram`](https://crates.io/crates/lexigram), the command-line lexer & parser generator
* [`lexi-gram`](https://crates.io/crates/lexi-gram), the high-level methods and objects to generate a lexer & parser from Rust code

# Status

This project is still under development and shouldn't be considered fully stable yet (hence the 0.x version). The main upcoming changes required before a stable version are:
* for the **tools**
  * check robustness in corner cases, in particular by detecting problematic syntactic and lexical rules
  * improve error messages
  * refactor big methods
* for the **generated code**
  * improve performances

_The code and the documentation were entirely written by a human._

# Releases

[RELEASES.md](RELEASES.md) keeps a log of all the releases (most are on the [GitHub release page](https://github.com/blueglyph/lexigram/releases), too). 

# Licence

This code is licenced under either [MIT License](https://choosealicense.com/licenses/mit/) or [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/), at your option.
