# 0.9.3

* add `--tab-width <number>` option to change the tab width in lexicon/grammar files (default: 4)
* add `--ansi on/off/passive` option for ANSI colours in error messages and logs
  * on (default): enables ANSI codes and adds support for Windows console
  * off: disables ANSI codes
  * passive: enables ANSI codes but doesn't add specific support for Windows console (use if "on" creates problems)
* add `fn handle_msg(&mut self, span_opt: Option<&PosSpan>, msg: LogMsg)` to listener (+ default implementation) to intercept messages from parser
* fix line:col values when reporting errors in combined lexicon/grammar sources

# 0.9.2

* fix Gram: crashes when `<L>` repetition in last rule
* add annotated input in error messages, both in Lexi and Gram

# 0.9.1

* minor doc update

# 0.9.0

* token interception with new listener method `intercept_token(...) -> TokenId`. This method is called on each new terminal scanned by the lexer, so it might be more time-consumming.
  * see examples/typedef/src/typedef_match.rs for an example
* token hook with new listener method `hook(...) -> TokenId`. This method is called when the declared hook terminals are susceptible to be scanned next in a rule. It's ideal to transform a token dynamically before it's used by the parser to determine the next rule alternative to select. The typical case is the C's typedef problem. 
  * e.g. `(Type) -> hook;` in lexicon
  * see examples/typedef/src/typedef_type.rs and typedef_id_type.rs for examples
* `delay_stream_interception` feature in `lexigram-core` to postpone the capture of the next token, effectively reducing the latency between reading the parsed text and calling the `exit` listener methods
* new `token-enums` command-line option to generate enums of the terminals and nonterminals (could be helpful with hooks and token interception)
* new `--nt-value` command-line option to specify which nonterminals have a value
* new `--start-nt` command-line option to specify the start nonterminal
* new `<G>` greedy attribute to help with parsing table ambiguities
* optional spans argument in all listener "exit" methods, to locate the position of the text corresponding to terminals and nonterminals
* new `--spans` command-line option to generate the spans
* possibility to regroup the lexicon and the grammar in a single source file/tag. If the keyword `grammar` is found by Lexi when parsing the lexicon, Gram is called to parse the remaining text
* removed some info from logs and added other results instead.
* new "info" category in logs for useful information
* template code for the listener implementation in log and optionally written to a file
* template code for the user types in log and optionally written to a file
* user types now given in log instead of commented code in the wrapper
* `SynValue`, a type used internally by the wrapper, has been renamed to `EnumSynValue` to avoid name collisions
* a few more examples

### Compatibility-breaking changes

* `Terminate` return type for `check_abort_request(...)`, allowing `Abort` (abort, as before), `Conclude` (end of the parsing as if everything was parsed), and `None` (no interruption, as before). This can be used to stop the parsing "normally" if more unrelated text follows what we want to parse. For example, this is what makes it possible to regroup the lexicon and the grammar in the same source.
* automatic recognition of token-separated items like `Id ("," Id)*`. The pattern can have more separator tokens; the criterion is identical list of symbols before the `(...)*` and inside it.
  * `α (β α)*` now provides a context with `{ star: Synα1, ... }` (instead of `{ α: Synα, star: Synα1, ... }`) where `star` contains the first `α` in `star[0]` and the remaining ones in `star[1..n]`, so the values are in the right order; no need to insert the first one in the list any more. 
  * `α (<L=i> β α)*` now provides the first `α` in `init_i(&mut self, ctx: InitCtxI)`, before the `exit_i(&mut self, ctx: CtxI)` methods are called with the values inside the `(β a)*`. Again, the `α` values are received in the right order.
* `&mut acc` for all `<L>` constructions, instead of having to pop the value, give it to the listener method that returns the new value. Before, this was only done for right-recursive `<L>` rules. Example:
  * before: `fn exit_i(&mut self, ctx: CtxI) -> SynI;`
  * after: `fn exit_i(&mut self, acc: &mut SynI, ctx: CtxI);` (`acc` not in `CtxI` any more)
* `extra_libs` has been renamed to `libs` in `lexi_gram::Options` (both field and method)
* renamed the name `get_mut_log()` of the listener method to `get_log_mut()`, for the sake of consistency. 

# 0.8.0

* split lexigram full library into two crates:
  * `lexigram-core`: minimum required by generated lexer/parser code
  * `lexigram-lib`: required by lexer/parser generator
* renamed crates to get a more convenient binary name:
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

* added optional span information in listener for each parsed terminal / nonterminal

