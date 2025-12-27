[![crate](https://img.shields.io/crates/v/lexigram.svg)](https://crates.io/crates/lexigram)
[![documentation](https://docs.rs/lexigram/badge.svg)](https://docs.rs/lexigram)
[![crate](https://img.shields.io/crates/l/lexigram.svg)](https://github.com/blueglyph/lexigram/blob/master/LICENSE-MIT)

<hr/>

**Important note: This crate is still under development and shouldn't be considered stable yet (hence the 0.x version). I'm currently adding some documentation and examples, and refactoring the code.** 

# The `lexigram` Crate

This crate provides a lexer/parser generator, which parses a plain lexicon and grammar from a source file, and generates the Rust source code for the corresponding lexer and nonrecursive, predictive LL(1) parser.

**lexigram** is inspired by ANTLR's lexicon and grammar language, and by its listener interface, which is simple, direct, and efficient.

Its main particularities are: 

* Most of the LL(1) restrictions that can be worked around by rule transformation, like left-recursive rules and ambiguous expression-like rules with precedence and associativity, are handled automatically and as transparently as possible by **lexigram**.
* It doesn't mix Rust code with the grammar or the lexicon.
* The generated code includes a trait that must be implemented by the user listener object, and which provides callback methods for each initialization and exit of nonterminals (grammar rules).
* Contrary to ANTLR, the callbacks are called immediately during the parsing phase, not after the construction of a concrete syntax tree. This allows for a minimal latency; for example if the application must parse a virtually infinite log and produce a real-time output, or if the parsing may require the remapping of some tokens before parsing the rest of the text (C's _typedef_). 

## Simplified Example

Here is a lexicon and a grammar for a basic expression parser:
```
lexicon Calc;
Add                     : '+';
Div                     : '/';
Exp                     : '^';
Mul                     : '*';
Sub                     : '-';
Num                     : [0-9]+;
WhiteSpace              : [ \n\r\t]+    -> skip;
```
```
grammar Calc;
expr:
    Sub expr
|   expr <R> Exp expr           // <R>: right-associative
|   expr (Mul | <P> Div) expr   // <P>: Div has the same priority as Mul
|   expr (Add | <P> Sub) expr
|   Num
;
```
In such ambiguous rules, 
* the priority is implicitly defined by the order of the rule alternatives: the first has the highest priority. A `<P>` attribute marks a rule alternative that must have the same priority as the previous one.
* the binary operators are left-associative by default; a `<R>` attribute marks a right-associative operator.  

(Those attributes can be placed anywhere in the alternative.)

**lexigram** will create the tables for the lexer and parser, and all the wrapper code that interfaces the parser and the user listener. We show only the few illustrative bits below.

Each rule as an initialization method, called when the parser has predicted it was the next rule, and an exit method, called once the rule has been fully parsed.

```rust
pub trait CalcListener {
    // ... (other general methods)
    fn exit(&mut self, expr: SynExpr) {}
    fn init_expr(&mut self) {}
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}    
```
Here, the type `SynExpr` represents the `expr` nonterminal value, and it must be defined by the user (a template is provided).

The `exit_expr` method must return the value that has been calculated in function of the context, `ctx`, which contains the value of the terminals (tokens) and nonterminals (rules). Note that this is optional: nonterminals may hold no value, in which case the method has a default, empty definition.

The context reflects the original grammar rules, regardless of how they were tranformed to accommodate an LL(1) parser: 
```rust
pub enum CtxExpr {
    /// `expr -> "-" expr`
    V1 { expr: SynExpr },
    /// `expr -> expr <R> "^" expr`
    V2 { expr: [SynExpr; 2] },
    /// `expr -> expr "*" expr`
    V3 { expr: [SynExpr; 2] },
    /// `expr -> expr <P> "/" expr`
    V4 { expr: [SynExpr; 2] },
    /// `expr -> expr "+" expr`
    V5 { expr: [SynExpr; 2] },
    /// `expr -> expr <P> "-" expr`
    V6 { expr: [SynExpr; 2] },
    /// `expr -> Num`
    V7 { num: String },
}
```

The user then creates a listener object, that will typically hold the AST, and implements the `CtxExpr` trait for that object. Here, we don't build an AST but just illustrate how the callback can be used for a basic calculator.

```rust
pub struct SynExpr(pub f64);

pub struct ExprListener {
    value: Option<f64>,
}

impl TestListener for ExprListener {
    // ...
    
    fn exit(&mut self, expr: SynExpr) {
        self.value = Some(expr.0);
    }
    
    fn exit_e(&mut self, ctx: CtxExpr) -> SynExpr {
        SynExpr(match ctx {
            CtxExpr::V1 { expr: SynExpr(left) } => - left,
            CtxExpr::V2 { expr: [SynExpr(l), SynExpr(r)] } => l.powf(r),
            CtxExpr::V3 { expr: [SynExpr(l), SynExpr(r)] } => l * r,
            CtxExpr::V4 { expr: [SynExpr(l), SynExpr(r)] } => l / r,
            CtxExpr::V5 { expr: [SynExpr(l), SynExpr(r)] } => l + r,
            CtxExpr::V6 { expr: [SynExpr(l), SynExpr(r)] } => l - r,
            CtxExpr::V7 { num } => f64::from_str(&num).unwrap(),
        })
    }
}
```

# Usage

`lexigram -h` shows the usage summary.

In a nutshell, you must provide the location of the lexicon, the grammar, and specify where to generate the code.

The lexicon and the grammar are usually stored as stand-alone files, like "expr.l" and "expr.g". They may also be included between tags in any file.

The same stands for the output of **lexigram**: it can create stand-alone files or insert it between tags in an existing file.

For example, if you create a file "calc.rs" with the following content, everything will be in a single file:

````rust
/*!
```
// [lexicon]

lexicon Calc;
Add                     : '+';
Div                     : '/';
Exp                     : '^';
Mul                     : '*';
Sub                     : '-';
Num                     : [0-9]+;
WhiteSpace              : [ \n\r\t]+    -> skip;

// [lexicon]

// [grammar]

grammar Calc;
expr:
    Sub expr
|   expr <R> Exp expr           // <R>: right-associative
|   expr (Mul | <P> Div) expr   // <P>: Div has the same priority as Mul
|   expr (Add | <P> Sub) expr
|   Num
;

// [grammar]
```
*/

#[derive(Debug)]
pub struct SynExpr(pub f64);

mod lexer {
    // [lexer_source]
    // [lexer_source]
}

mod parser {
    // [parser_source]
    // [parser_source]
}


````

Launch the following command:

```
lexigram --indent 4 -x calc.rs tag lexicon -l calc.rs tag lexer_source \
    -g calc.rs tag grammar -p calc.rs tag parser_source \
    --lib "super::SynExpr" 
```

It will insert the generated code of the lexer and parser inside the [lexer_source] and [parser_source] tags, respectively. Launching the same command will replace any existing code between the tags, so it can be used iteratively (useful if you need to fix or change the grammar!). 

You'll just need to add a dependency to `lexigram-core` with the version that corresponds to the **lexigram** binary ("lexigram -V" will show its version).

Using the `lexi-gram` crate allows you to generate the code programmatically from Rust code, which is another option.

# Where to Go From Here

You can find some examples in the repository (https://github.com/blueglyph/lexigram)
* in [./examples](https://github.com/blueglyph/lexigram/tree/master/examples)
* in [./build-rtsgen](https://github.com/blueglyph/lexigram/tree/master/build-rtsgen), which is a [lexer/parser](https://github.com/blueglyph/lexigram/blob/master/src/rtsgen/mod.rs) for simplified grammar language used in the unit tests.

**lexigram** itself uses a couple of lexers/parsers (that it generated itself) to parse the lexicon (Lexi) and the grammar (Gram)—hence the tool name, aside from the obvious pun. Lexi and Gram, however, are a little harder to trace in the source code because they're generated in two steps, to keep them from breaking and to regenerate them more easily when the code evolves.

[Rudimentary documentation](https://github.com/blueglyph/lexigram/tree/master/doc) can be found in the GitHub project.

Some `#[ignore]` tests can be found in the code base to regenerate the multiple lexers and parsers, including in the examples.

# Lexigram

The [lexigram](https://github.com/blueglyph/lexigram) project includes the following crates:
* [`lexigram-core`](https://crates.io/crates/lexigram-core), the minimum library required by the code generated by **lexigram**
* [`lexigram-lib`](https://crates.io/crates/lexigram-lib), the full library used by `lexi-gram` and `lexigram`
* [`lexi-gram`](https://crates.io/crates/lexi-gram), the Lexi and Gram parsers, which parse the lexicon and grammar language for the `lexigram` tool
* [`lexigram`](https://crates.io/crates/lexigram), the lexer/parser generator tool

# Releases

[RELEASES.md](https://github.com/blueglyph/lexigram/blob/master/RELEASES.md) keeps a log of all the releases (most are on the [GitHub release page](https://github.com/blueglyph/lexigram/releases), too). 

# Licence

This code is licenced under either [MIT License](https://choosealicense.com/licenses/mit/) or [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/), at your option.
