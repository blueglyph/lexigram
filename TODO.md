# Lexigram

## General

- [ ] clean-up of the unit & integration tests
- [x] ~~more coherency in functions/methods that create source code (names, location)~~
  - Name conventions:
    - build:  creates object by consuming self (or non-method that creates object)
    - make:   sets fields or creates object without consuming self
    - gen:    creates string source code
    - write:  writes source code into file
- [ ] split huge files (
  - [ ] dfa
  - [ ] grammar 
  - [ ] lexergen 
  - [ ] parsergen
- error reporting
  - [ ] create info/warning/error ID numbers
  - [x] ~~return Result instead of objects in from<...> when errors in the log~~
  - [ ] make messages parse-friendly and consistent (indentation structure, casing, ...)

## Lexer

- basic lexi
  - [ ] move 'make' methods from listener to Lexi 
  - [ ] make binary application
  - [ ] verify it doesn't crash with erroneous lexicons
- code generation
  - [x] ~~don't pour big ranges (.) into utf8 table~~
- [ ] independence from reader type (Lexer<R: Read>)
- robustness
  - [ ] detect dead ends without a terminal
  - [ ] detect missing terminals (present in vectree but not in dfa)
  - [ ] detect potential infinite loops for some illegal characters (with skip, if that can still happen)
  - [ ] error reporting when creating the lexer
    - [x] ~~Lexi's listener~~
      - [ ] pretty print tree for errors and reporting
    - [x] Logger in DfaBuilder, Dfa 
      - [ ] populate
    - [x] Logger in LexerGen
      - [ ] populate
- improve performances / memory
  - [x] ~~use statics instead of consts in generated code~~ 
  - [x] ~~use reference to static tables instead of Box<[]>~~
  - [ ] try to put all the data in static? (not sure it's interesting)
  - [ ] static HashMap / SegMap (BTreeMap)?
  - [ ] ReNode size
  - [ ] buffer <> Strings returned by get_token()
  - terminal table
    - [ ] redundancy (2.5x larger than necessary)
    - [ ] Terminal is 32 bytes long

## Parser

- basic gram
  - parser text -> RuleTree
  - [ ] make binary application
  - [ ] verify it doesn't crash with erroneous grammars
- code generation
  - [x] ~~use statics instead of consts for tables, etc~~
  - [ ] add option for loop value
  - [ ] opcode + gram syntax for early rule attribute callback
    - `A -> a B # C` -> at `#`, callback with values of `a` and `B`
  - [ ] attach token information (text, line / col) to context items
    - [ ] replace SynVal by CtxVal in wrapper, with struct CtxVal { val: SynVal, text: String, line: usize, col: usize, .. }?
  - [ ] allow for references in Syn types (=> SynNT<'a>(&'a ValType), CtxRule<'a> { nt: SynNT<'a>, .. }, ...)
    - if possible at all, we need to verify that the lifetime >= wrapper's lifetime (and make the borrow checked understand it)
  - [ ] better names when creating enum options in contexts (ParserBuilder::source_wrapper)
    - based on T names and the number of NTs? E * E = Mul2, E - E = Sub2, - E = Sub1, ... => easier if collected before transformation
  - [ ] better names when creating children NTs (grammar/symbol_table)
- robustness
  - [ ] error reporting when creating the parser
    - [ ] check there aren't multiple <L=...> (see lform_nt in normalize_plus_or_star)
    - [ ] check that flags are not out of place
    - [ ] check that rules are supported (cyclic recursive, ...?)
    - [ ] ParserGen
      - [ ] populate log with info (& warnings, errors?)
  - [ ] check if LL(1)
    - [ ] check indirect left recursion
  - [ ] error recovery
    - [x] basic resynchronization
    - [ ] points of reactivation of the semantic analysis (instead of stopping it altogether)
    - [ ] option to stop parsing on first error
- improve performances / memory
  - [x] ~~use reference to static tables when possible instead of building collections from them~~
    - [x] ~~FixedSymbolTable for generated parser <-> SymbolTable for lib~~ 
  - [ ] try to put all the data in static? (not sure it's interesting)

## Bottom-up parser
  - [ ] decide algorithm (LALR?)
  - [ ] LR/CELR-attributed grammar
  - [ ] remove ambiguity: A -> A β A | δ
    - check left-/right-associativity
    - left-associative:  A -> A β δ | δ
    - right-associative: A -> δ β A | δ

## Packages
- [ ] lexigram bin
- [ ] separate libs
  - small lib for parser/lexer (for generated parser)
  - gen lib? other?

## Potential Features
- [ ] handle token ambiguity in context (e.g. to allow IDs with names of keywords)
  - dfa::calc_states: when terminals.len() > 1, returns the 1st defined + alternatives
  - parser: if var/token not in table, check alternatives to recover
- [ ] lexer hooks? (using traits?)
- AST tree builder
  - [ ] create code (nodes, listener) that returns a VecTree
  - [ ] add pre-order, depth-first search?
  - [ ] procedural macro
- dynamic parser
  - [ ] create a universal Listener implementation to simulate / test grammar 
- [ ] Put alternative token in lexer to allow parser use fallback (ex: language keyword -> ID)
  - see when accepting state has conflicts between end symbols

## Project Structure
- [ ] build.rs to compile generated code?
  - currently done with tests

## Performances (general)
- [ ] clone iterators instead of cloning Vec in cproduct
- [ ] check BTreeMaps
 
## Tests
- [ ] code coverage
  - [ ] check everything is fine when deleting NT in grammar (in prods, flags, factor_flags, symbol_table, start)
  - [ ] check everything is fine when inserting NT in grammar (in prods, flags, factor_flags, symbol_table, start)
  - [ ] [Tarpaulin](https://github.com/xd009642/tarpaulin) (automate?)
  - [ ] check for missing corner cases ([Kani](https://github.com/model-checking/kani)?)
- [ ] profile on big inputs
  - [ ] profile perfs 
  - [ ] profile memory

# Other
- [ ] README.md
- [ ] rustdoc & comments
- [ ] doc on main algorithms
