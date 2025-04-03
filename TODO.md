# Lexigram

- basic lexi
  - [x] ~~create regex for rlexer's lexicon (manually)~~
  - [x] ~~replace struct Token(pub TokenId) by just a TokenId~~
  - [x] ~~create simple top-down parser for rlexer's grammar (manually)~~
  - [x] ~~transform regex AST to vectree~~
  - [x] ~~make binary application (lexicon file -> AST -> vectree -> dfa -> lexgen -> sources)~~
- [ ] don't pour big ranges (.) into utf8 table
- improve robustness
  - [ ] detect dead ends without a terminal
  - [ ] detect missing terminals (present in vectree but not in dfa)
  - [ ] detect potential infinite loops for some illegal characters (with skip, if that can still happen)
- improve performances / memory
  - [ ] ReNode size
  - [ ] buffer <> Strings returned by get_token()
  - terminal table
    - [ ] redundancy (2.5x larger than necessary)
    - [ ] Terminal is 32 bytes long
- [x] ~~ranges~~
- [x] ~~lazy/non-greedy repeaters and ranges~~
- [x] ~~how to retrieve original text behind tokens?~~
- [ ] lexer hooks? (using traits?)
- [x] ~~define lexer file basic syntax (1st step)~~
- error reporting
  - [ ] when creating the lexer
    - [x] Logger in DfaBuilder, Dfa 
      - [ ] populate
    - [x] Logger in LexerGen
      - [ ] populate
  - [ ] when creating the parser
    - [x] PargerGen
      - [ ] populate log with info (& warnings, errors?)
  - [ ] when using the lexer / parser
    - ~~Lexer~~ not necessary: returns an error code 
    - [x] ~~Lexer - Parser - wrapper - listener~~
  - [ ] create info/warning/error ID numbers
  - [ ] make messages parse-friendly and consistent (indentation structure, casing, ...)
- [ ] independence from reader type (Lexer<R: Read>)

# Packages

- [x] lexigram lib
- [x] lexi
- [x] make-lexi
- ~~[ ] dependancies point to git + version instead of directories~~ Too cumbersome and problematic
- [ ] gram
- [ ] make-gram
- [ ] lexigram bin
- [ ] separate libs
  - small lib for parser/lexer (for generated parser)
  - gen lib? other?

# Parser

- parser lib
  - RuleTreeSet to store the parsed production rules (as `VecTree<GrNode>`)
    - [x] ~~normalize~~
    - [x] ~~transforms to | of &s~~ 
    - [x] ~~translate *, +, ? -> tree~~
    - [x] ~~transform normalized tree rules -> production rules~~
  - RuleProdSet to store the normalized production rules (as `Vec<Vec<Symbol>>`)
    - [x] ~~remove left recursion -> rules~~
    - [x] ~~left factoring -> rules~~
    - [x] ~~manage symbol table~~
  - top-down
    - [x] ~~calc first~~
    - [x] ~~calc follow~~
    - [x] ~~remove ambiguity: `A -> A β A | δ`~~
      - ~~special case of left recursivity. Don't translate to `A -> δ A1; A1 -> β A A1 | ε`, but `A -> δ A1; A1 -> β A1 | ε` + tag left-/right-associativity~~
    - [ ] break polymorphic ambiguities
      - ```
        E -> E ^ E | - E | E * E | E + E | F  
        =>  
        E -> E * E | E + E | - E_1 | E_1
        E_1 -> E_1 ^ E_1 | F
        =>
        ... (remove ambiguity and left recursion in E, E_1
        ```
    - [ ] reconstruct semantic synthesis attributes during parsing
      - [x] ~~tags when transforming grammar (remove_left_recursion, left_factorize)~~
      - [x] ~~stack-based reconstruction~~
      - [ ] left/right-associativity
      - [x] ~~opcode builder based on flags~~
      - [ ] add option for loop value/end-of-loop call (+, *, r-rec)
      - [ ] opcode + gram syntax for early rule attribute callback
        - `A -> a B # C` -> at `#`, callback with values of `a` and `B`
    - [ ] attach token information (text, line / col) to context items
      - [ ] replace SynVal by CtxVal in wrapper, with struct CtxVal { val: SynVal, text: String, line: usize, col: usize, .. }?
      - [x] ~~adapt lexer~~
      - [ ] adapt parser
      - [ ] adapt parser code generation
    - [ ] check if LL(1)
      - [ ] check indirect left recursion
    - [x] ~~build parsing table~~
    - [ ] warning/error log system (for parsing table, lexer, ...)
      - [x] ~~add log~~
      - [x] ~~check log and error issued in the whole chain (RTS -> PRS)~~
      - [ ] check if log reports errors correctly for each error type
    - [x] ~~predictive parsing algorithm~~
    - [x] ~~semantic analysis: how to link back to original RuleTree after removing left recursion/left factoring?~~
    - proper adapter link to lexer
      - [ ] adapter for tokens
      - [ ] SymbolTable
    - [ ] generate code
      - [x] ~~context~~
      - [x] ~~SynValue~~
      - [x] ~~trait~~
      - [x] ~~switch~~
      - [x] ~~impl wrapper~~
      - [ ] special case of ambiguous left recursions (A α A)
      - [ ] allow for references in Syn types (=> SynNT<'a>(&'a ValType), CtxRule<'a> { nt: SynNT<'a>, .. }, ...)
        - if possible at all, we need to verify that the lifetime >= wrapper's lifetime (and make the borrow checked understand it)
      - [x] ~~NT whose nt_value is false but which carry other valuable NTs or T data must have an internal Syn type~~ 
    - [ ] loose ends / to check
      - [ ] name fixer / ...: prevent reserved words ("type", "const", ...) 
      - [ ] name fixer when creating children NTs (grammar/symbol_table) 
      - [ ] better names when creating children NTs (grammar/symbol_table)
      - [ ] name fixer when creating enum options in contexts (ParserBuilder::source_wrapper) 
      - [ ] better names when creating enum options in contexts (ParserBuilder::source_wrapper)
      - [ ] option to generate or not last call after iterations in left_rec?
    - [ ] error recovery
  - bottom-up
    - [ ] decide algorithm (LALR?)
    - [ ] LR/CELR-attributed grammar
    - [ ] remove ambiguity: A -> A β A | δ
      - check left-/right-associativity
      - left-associative:  A -> A β δ | δ
      - right-associative: A -> δ β A | δ
- basic gram
  - parser text -> RuleTree
    - [ ] generate lexer tree with basic rlexer (lexicon file)
    - [ ] adapt rlexer's top-down parser (manually)
    - [ ] transform AST to top-down (or LR?) data
  - [ ] make binary application
- dynamic parser
  - [ ] create a universal Listener implementation to simulate / test grammar 

## Potential Features

- [ ] Put alternative token in lexer to allow parser use fallback (ex: language keyword -> ID)
  - see when accepting state has conflicts between end symbols

## Project Structure

- [ ] build.rs to compile generated code?
  - currently done with tests
- [x] ~~extract vectree into another lib~~
- [x] ~~move macros~~

## Performances

- [ ] table storage in lexer, lexgen, and parser (either generated dynamically or from const)
- [ ] clone iterators instead of cloning Vec in cproduct
- [ ] check BTreeMap vs HashMap
 
## Tests

- [ ] code coverage
  - [ ] check everything is fine when deleting NT in grammar (in prods, flags, factor_flags, symbol_table, start)
  - [ ] check everything is fine when inserting NT in grammar (in prods, flags, factor_flags, symbol_table, start)
  - [ ] [Tarpaulin](https://github.com/xd009642/tarpaulin) (automate?)
  - [ ] check for missing corner cases ([Kani](https://github.com/model-checking/kani)?)
- [ ] profile on big inputs
  - [ ] profile perfs 
  - [ ] profile memory

## Other

- [ ] README.md
- [ ] rustdoc & comments
- [ ] doc on main algorithms
