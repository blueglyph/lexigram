[![crate](https://img.shields.io/crates/v/lexigram.svg)](https://crates.io/crates/lexigram)
[![documentation](https://docs.rs/lexigram/badge.svg)](https://docs.rs/lexigram)
[![crate](https://img.shields.io/crates/l/lexigram.svg)](https://github.com/blueglyph/lexigram/blob/master/LICENSE-MIT)

<hr/>

# Lexigram

This crate is part of the [Lexigram](https://github.com/blueglyph/lexigram) project, a lexer/parser generator.

You can dive into the [Lexigram book](https://www.unscript.net/lexigram-book) for an introduction, a tutorial, or a reference. You can also browse the [crate documentation](https://docs.rs/lexigram).

You can install and update the executable with this command:

```shell
cargo install lexigram
```

# The `lexigram` Crate

The [`lexigram`](https://crates.io/crates/lexigram) crate contains the implementation of the command-line lexer & parser generator.

It takes a grammar and generates the Rust source code to parse the corresponding language, providing a custom listener trait as user interace. This interface allows your code to interact with the parser by executing user-implemented callback methods when the rules are reduced, like ANTLR. It is also somewhat similar to the code written next to each rule in tools like Yacc and Bison, but it's cleaner and more flexible.

Two types of parser can be generated: top-down and bottom-up.

The top-down option generate a predictive, non-recursive LL(1) parser. You may write non-LL(1) grammars as long as Lexigram can transform it into LL(1): left-recursive and ambiguous rules such as binary expressions are supported, and left factorization is performed automatically. The grammar also supports extended syntax such as repetition, token-separated repetitions, and so on. The generated interface hides those transformation by referring to the original rules as much as possible.

The bottom-up option currently generates an LALR(1) parser. Ambiguous rules are resolved with the same simple grammar notation used in LL(1) grammars. In general, Lexigram makes it relatively straightforward to move from LL to LALR.

## Simplified Example

Here's the grammar for a basic expression parser:

```text
lexicon Calc;

Add                     : '+';
Div                     : '/';
Exp                     : '^';
Mul                     : '*';
Sub                     : '-';
Num                     : [0-9]+;
WhiteSpace              : [ \n\r\t]+    -> skip;

grammar Calc;

expr:
    '-' expr
|   expr <R> '^' expr           // <R>: right-associative
|   expr ('*' | <P> '/') expr   // <P>: Div has the same priority as Mul
|   expr ('+' | <P> '-') expr
|   Num
;
```

`expr` is an ambiguous rule that Lexigram has to transform:
* The priority is implicitly defined by the order of the rule alternatives: the first has the highest priority. 
* The `<P>` attribute marks an alternative that must have the same priority as the previous one.
* The `<R>` attribute marks a right-associative operator; binary operators are left-associative by default.  

Lexigram creates the tables for the lexer and parser, and the wrapper code that interfaces the parser and the user listener. We show only a few interesting bits below.

Each rule has an initialization method, called when the parser has predicted it was the next rule, and an exit method, called once the rule has been fully parsed.

```rust,ignore
pub trait CalcListener {
    // ... (other general methods)
    fn exit(&mut self, expr: SynExpr) {}
    fn init_expr(&mut self) {}
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}    
```

The `exit_expr(…)` method is called once `expr` is fully parsed, providing the values of the terminals and nonterminals in the context argument `ctx`. It returns the value of the nonterminal; `SynExpr` is the type of the `expr` nonterminal value, and is defined by you (a template is provided).

To interact with the parser, you create a listener object that typically holds the AST and some intermediate data, and you implement the `CalcListener` trait for that object. Here's a quick example showing how the method can be implemented for a basic calculator: 

```rust,ignore
pub struct SynExpr(pub f64);

pub struct Listener {
    value: Option<f64>,
    log: BufLog,
}

impl CalcListener for Listener {
    // ...
    
    fn exit(&mut self, expr: SynExpr) {
        // final result is the value of the top `expr`
        self.value = Some(expr.0);
    }
    
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr {
        SynExpr(match ctx {
            // expr -> "-" expr
            CtxExpr::V1 { expr: SynExpr(left) } => - left,
            // expr -> expr <R> "^" expr
            CtxExpr::V2 { expr: [SynExpr(l), SynExpr(r)] } => l.powf(r),
            // expr -> expr "*" expr
            CtxExpr::V3 { expr: [SynExpr(l), SynExpr(r)] } => l * r,
            // expr -> expr <P> "/" expr
            CtxExpr::V4 { expr: [SynExpr(l), SynExpr(r)] } => l / r,
            // expr -> expr "+" expr
            CtxExpr::V5 { expr: [SynExpr(l), SynExpr(r)] } => l + r,
            // expr -> expr <P> "-" expr
            CtxExpr::V6 { expr: [SynExpr(l), SynExpr(r)] } => l - r,
            // expr -> Num
            CtxExpr::V7 { num } => f64::from_str(&num).unwrap(),
        })
    }
}
```

As you can see, the code preserves the structure of the original grammar, even though it had to be modified by Lexigram for an LL(1) parser. 

# Usage

`lexigram -h` shows the usage summary.

In a nutshell, you must provide the location of the lexicon, the grammar, and specify where to generate the code.

The lexicon and the grammar are usually stored as stand-alone files, like `expr.l` and `expr.g`, or one combined file `expr.lg`. They can also be included between tags in any file. Here, we'll combine the lexicon and the grammar, and we'll keep it in the same file as the generated code.

The same is true for the generated code; Lexigram can create stand-alone files or insert it between tags in an existing file.

Create a file `calc.rs` with the following content:

````rust,ignore
/*!
```
// [grammar]

lexicon Calc;

Add                     : '+';
Div                     : '/';
Exp                     : '^';
Mul                     : '*';
Sub                     : '-';
Num                     : [0-9]+;
WhiteSpace              : [ \n\r\t]+    -> skip;

grammar Calc;

expr:
    '-' expr
|   expr <R> '^' expr           // <R>: right-associative
|   expr ('*' | <P> '/') expr   // <P>: Div has the same priority as Mul
|   expr ('+' | <P> '-') expr
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

```shell
lexigram --indent 4 \
  -c calc.rs tag grammar -l calc.rs tag lexer_source \
  -p calc.rs tag parser_source \
  --types types.txt \
  --listener listener.txt \
  --lib "super::SynExpr"
```

It will insert the generated code of the lexer and LL(1) parser inside the `[lexer_source]` and `[parser_source]` tags, respectively. Launching the same command will replace any existing code between the tags, so it can be used iteratively (useful if you need to fix or change the grammar!).

In `types.txt` and `listener.txt`, you'll find a template for the user types and a basic implementation of the listener. You can copy their content the first time to make it easier. You can also add those files to your repository to see what must be updated in your code each time the grammar is modified.

The only dependency you need in Cargo.toml is the [`lexigram-core`](https://crates.io/crates/lexigram-core) crate, with a version compatible with the `lexigram` executable (`lexigram -V`).

# Switching to LALR(1)

To generate an LALR parser, you just need to add a `<resolve>` before the `expr:` rule, to tell Lexigram it can resolve the shift/reduce conflicts due to the ambiguities by using the same precedence and associativity notation as before. This attribute can be used for LL(1), too, but it's optional.

The command must specify the parser type, since it generates LL(1) by default:

```shell
lexigram --indent 4 \
  -c calc.rs tag grammar -l calc.rs tag lexer_source \
  -p calc.rs tag parser_source \
  --types types.txt \
  --listener listener.txt \
  --lib "super::SynExpr" \
  -t LALR
```

The generated listener interface is the same, except for the missing `init_*` methods, which are incompatible with the way bottom-up parsers operate.  

# Where to Go From Here

The [Lexigram book](https://www.unscript.net/lexigram-book) has an introduction, a tutorial, and a reference that should provide all the necessary information to start building your own parser.

You can also find some examples in the project repository:
* in [./examples](https://github.com/blueglyph/lexigram/tree/master/examples)
* in [./build-rtsgen](https://github.com/blueglyph/lexigram/tree/master/build-rtsgen), which is a [lexer/parser](https://github.com/blueglyph/lexigram/blob/master/src/rtsgen/mod.rs) for simplified grammar language used in the unit tests.

There are `#[ignore]` tests in the `./examples/gen_*` crates to regenerate the examples. They show how to generate a parser programmatically, if you prefer that to the command-line executable.

Lexigram itself uses a couple of lexers/parsers (that it generated itself) to parse the lexicon (Lexi) and the grammar (Gram). Lexi and Gram, however, are a little harder to trace in the source code because they're generated in two stages.

# Status

This project is still under development and shouldn't be considered fully stable yet (hence the 0.x version). See the main project's [README](https://github.com/blueglyph/lexigram/blob/master/README.md).

_The code and the documentation were entirely written by a human._

# Releases

[RELEASES.md](https://github.com/blueglyph/lexigram/blob/master/RELEASES.md) keeps a log of all the releases (most are also on the [GitHub release page](https://github.com/blueglyph/lexigram/releases)). 

# Licence

This code is licenced under either [MIT License](https://choosealicense.com/licenses/mit/) or [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/), at your option.
