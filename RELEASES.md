# 0.9.0

* token interception with new listener method `intercept_token(...) -> TokenId`. This method is called on each new terminal scanned by the lexer, so it might be more time-consumming.
  * see examples/typedef/src/typedef_match.rs for an example
* token hook with new listener method `hook(...) -> TokenId`. This method is called when the declared hook terminals are susceptible to be scanned next in a rule. It's ideal to transform a token dynamically before it's used by the parser to determine the next rule alternative to select. The typical case is the C's typedef problem. 
  * e.g. `(Type) -> hook;` in lexicon
  * see examples/typedef/src/typedef_type.rs and typedef_id_type.rs for examples
* new `token-enums` command-line option to generate enums of the terminals and nonterminals (could be helpful with hooks and token interception)
* new `--nt-value` command-line option to specify which nonterminals have a value
* new `<G>` greedy attribute to help with parsing table ambiguities
* optional spans argument in all listener "exit" methods, to locate the position of the text corresponding to terminals and nonterminals
* new `--spans` command-line option to generate the spans
* possibility to regroup the lexicon and the grammar in a single source file/tag. If the keyword `grammar` is found by Lexi when parsing the lexicon, Gram is called to parse the remaining text
* `Terminate` return type for `check_abort_request(...)`, allowing `Abort` (abort, as before), `Conclude` (end of the parsing as if everything was parsed), and `None` (no interruption, as before). This can be used to stop the parsing "normally" if more unrelated text follows what we want to parse. For example, this is what makes it possible to regroup the lexicon and the grammar in the same source.
* automatic recognition of token-separated items like `Id ("," Id)*`. The pattern can have more separator tokens; the criterion is identical list of symbols before the `(...)*` and inside it.
  * `α (β α)*` now provides a context with `{ star: Synα1, ... }` (instead of `{ α: Synα, star: Synα1, ... }`) where `star` contains the first `α` in `star[0]` and the remaining ones in `star[1..n]`, so the values are in the right order; no need to insert the first one in the list any more. 
  * `α (<L=i> β α)*` now provides the first `α` in `init_i(&mut self, ctx: CtxInitI)`, before the `exit_i(&mut self, ctx: CtxI)` methods are called with the values inside the `(β a)*`. Again, the `α` values are received in the right order.
* `&mut acc` for all `<L>` constructions, instead of having to pop the value, give it to the listener method that returns the new value. Before, this was only done for right-recursive `<L>` rules. Example:
  * before: `fn exit_i(&mut self, ctx: CtxI) -> SynI;`
  * after: `fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI);` (`acc` not in `CtxI` any more)
* removed some info from logs and added other results instead.
* new "info" category in logs for useful information

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

