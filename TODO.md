# RLexer

- basic rlexer
  - [x] ~~create regex for rlexer's lexicon (manually)~~
  - [ ] create simple top-down parser for rlexer's grammar (manually)
  - [ ] transform regex AST to vectree
  - [ ] make binary application (lexicon file -> AST -> vectree -> dfa -> lexgen -> sources)
- [ ] don't pour big ranges (.) into utf8 table
- improve robustness
  - [ ] detect dead ends without a terminal
  - [ ] detect missing terminals (present in vectree but not in dfa)
  - [ ] detect potential infinite loops for some illegal characters (with skip, if that can still happen)
- improve performances / memory
  - [ ] table storage in lexer and lexgen (either generated dynamically or from const)
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
- [ ] error reporting
- [ ] independence from reader type (Lexer<R: Read>)

## Structure

- [ ] extract vectree into another lib
- [x] ~~move macros~~

## Tests

- [ ] code coverage
  - [ ] [Tarpaulin](https://github.com/xd009642/tarpaulin) (automate?)
  - [ ] check for missing corner cases ([Kani](https://github.com/model-checking/kani)?)
- [ ] profile on big inputs
  - [ ] profile perfs 
  - [ ] profile memory
- [ ] check BTreeMap vs HashMap

## Other

- [ ] README.md
- [ ] rustdoc & comments
- [ ] doc on main algorithms

# RParser

- basic rparser
  - [ ] generate lexer with basic rlexer (lexicon file)
  - [ ] adapt rlexer's top-down parser (manually)
  - [ ] transform AST to top-down (or LR?) data
  - [ ] make binary application
