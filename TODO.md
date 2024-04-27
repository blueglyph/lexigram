# RLexer

- basic rlexer
  - [x] ~~create regex for rlexer's lexicon (manually)~~
  - [ ] create simple top-down parser for rlexer's grammar (manually)
  - [ ] transform regex AST to vectree
  - [ ] make binary application (lexicon file -> AST -> vectree -> dfa -> lexgen -> sources)
- [x] ~~ranges~~
- [x] ~~lazy/non-greedy repeaters and ranges~~
- [x] ~~how to retrieve original text behind tokens?~~
- [ ] lexer hooks? (using traits?)
- [x] ~~define lexer file basic syntax (1st step)~~
- [ ] error reporting

## Structure

- [ ] extract vectree into another lib
- [ ] move macros

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
