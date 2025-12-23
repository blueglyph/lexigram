# 0.8.0

* split lexigram full library into two crates:
  * `lexigram-core`: minimum required by generated lexer/parser code
  * `lexigram-lib`: required by lexer/parser generator
* rename crates to get a more convenient binary name:
  * `lexigram` -> `lexi-gram`
  * `lexigram-bin` -> `lexigram`
* added publishing details to toml files; there are 4 crates to publish:
  * `lexigram-core`, the core library required by generated code (and the other crates)
  * `lexigram-lib`, the generator library
  * `lexi-gram`, the generator top-levels, Lexi and Gram, which can use lexicon and grammar sources 
  * `lexigram`, the CLI executable for Lexi and Gram

# 0.7.0

* refactor crates

# 0.6.0

* add optional span information in listener for each parsed terminal / nonterminal

