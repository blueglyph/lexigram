[![crate](https://img.shields.io/crates/v/lexigram.svg)](https://crates.io/crates/lexigram)
[![documentation](https://docs.rs/lexigram/badge.svg)](https://docs.rs/lexigram)
[![crate](https://img.shields.io/crates/l/lexigram.svg)](https://github.com/blueglyph/lexigram/blob/master/LICENSE-MIT)

---

# Lexigram

This crate is part of the [Lexigram](https://github.com/blueglyph/lexigram) project, a lexer/parser generator.

You can dive into the [Lexigram book](https://www.unscript.net/lexigram-book) for an introduction, a tutorial, or a reference. You can also browse the [crate documentation](https://docs.rs/lexigram).

You can install and update the executable with this command:

```shell
cargo install lexigram
```

# The `lexigram` Crate

The [`lexigram`](https://crates.io/crates/lexigram) crate contains the command-line lexer/parser generator.

It takes a lexicon and a grammar, and generates the Rust source code to parse the corresponding language, providing a custom listener trait as user interface. This interface allows your code to interact with the parser by executing user-implemented callback methods when the rules are reduced, as in ANTLR. It is also somewhat similar to the code written next to each rule in tools like Yacc and Bison, but it's cleaner and more flexible.

Two parser types can be generated: top-down and bottom-up.

The top-down option generates a predictive, non-recursive LL(1) parser. You may write non-LL(1) grammars as long as Lexigram can transform it into LL(1): left-recursive and ambiguous rules such as binary expressions are supported, and left factorization is performed automatically. The grammar also supports extended syntax such as repetition, token-separated repetitions, and so on. The generated interface hides those transformations by referring to the original rules as much as possible.

The bottom-up option currently generates an LALR(1) parser. Ambiguous rules are resolved with the same simple grammar notation used in LL(1) grammars. In general, Lexigram makes it relatively straightforward to move from LL to LALR.

## Simplified Example

Here's the lexicon and the grammar for a basic expression parser:

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

<resolve>
expr:
    '-' expr
|   expr <R> '^' expr           // <R>: right-associative
|   expr ('*' | <P> '/') expr   // <P>: '/' has the same priority as '*'
|   expr ('+' | <P> '-') expr
|   Num
;
```

`expr` is an ambiguous rule that Lexigram has to transform:
* The `<resolve>` attribute explicitly requests to resolve the ambiguity in `expr`. It's optional for LL(1), but necessary for LALR(1).
* The priority is implicitly defined by the order of the rule alternatives: the first one has the highest priority. 
* The `<P>` attribute marks an alternative that has the same priority as the previous one.
* The `<R>` attribute marks a right-associative operator; binary operators are left-associative by default.  

Among other things, Lexigram creates a custom trait for the listener, which the user implements to interact with the parser:

```rust,ignore
pub trait CalcListener {
    // ... (other general methods)
    fn exit(&mut self, expr: SynExpr) {}
    fn init_expr(&mut self) {}
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}    
```

When the input is parsed, the `exit_expr(…)` method is called each time an `expr` has been fully parsed. The context argument `ctx` provides the values associated with the symbols, and the method returns the `SynExpr` value of the expression; for example, a numerical value, or a reference to a node if an AST is built (`SynExpr` is user-defined). 

Here's an example showing how the method can be implemented for a basic calculator: 

```rust,ignore
pub type SynExpr = f64; // we don't need a new type

pub struct Listener {
    value: Option<f64>,
    log: BufLog,
}

impl CalcListener for Listener {
    // ...
    
    fn exit(&mut self, expr: SynExpr) {
        // final result is the value of the top `expr`
        self.value = Some(expr);
    }
    
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr {
        SynExpr(match ctx {
            // expr -> "-" expr
            CtxExpr::V1 { expr: left } => - left,
            // expr -> expr <R> "^" expr
            CtxExpr::V2 { expr: [left, right] } => left.powf(right),
            // expr -> expr "*" expr
            CtxExpr::V3 { expr: [left, right] } => left * right,
            // expr -> expr <P> "/" expr
            CtxExpr::V4 { expr: [left, right] } => left / right,
            // expr -> expr "+" expr
            CtxExpr::V5 { expr: [left, right] } => left + right,
            // expr -> expr <P> "-" expr
            CtxExpr::V6 { expr: [left, right] } => left - right,
            // expr -> Num
            CtxExpr::V7 { num } => f64::from_str(&num).unwrap(),
        })
    }
}
```

As you can see, the code preserves the structure of the original grammar, even though it had to be modified by Lexigram for an LL(1) parser. 

In LL(1), each rule also provides an optional initialization method, like `init_expr(…)`, which is called before its content is parsed. This isn't available for LALR parsers, since it's incompatible with how they operate. 

# Usage

`lexigram -h` shows all the commands and options.

In a nutshell, you must provide the location of the lexicon, the grammar, and specify where to write the generated code.

Create an empty project and copy the combined lexicon / grammar above in a file `calc.lg`.

Create a file `calc.rs` with the following content:

````rust,ignore
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

Execute the following command:

```shell
$ lexigram --indent 4 \
  -c calc.lg -l calc.rs tag lexer_source \
  -p calc.rs tag parser_source \
  --types types.txt \
  --listener listener.txt \
  --lib "super::SynExpr"
```

It inserts the generated code of the lexer and LL(1) parser inside the `[lexer_source]` and `[parser_source]` tags, respectively. Running the same command again will replace any existing code between the tags, so it can be used iteratively. Useful if you need to fix or change the grammar!

In `types.txt` and `listener.txt`, you'll find a template for the user types and a basic implementation of the listener. You can copy their contents the first time to make things easier. You can also add those files to your repository to see what must be updated in your code each time the grammar is modified.

The only dependency you need in Cargo.toml is the [`lexigram-core`](https://crates.io/crates/lexigram-core) crate.

# Switching to LALR(1)

Lexigram generates an LL(1) parser by default. If you want another type, you must specify it explicitly:

```shell
lexigram --indent 4 \
  -c calc.lg -l calc.rs tag lexer_source \
  -p calc.rs tag parser_source \
  --types types.txt \
  --listener listener.txt \
  --lib "super::SynExpr" \
  -t LALR
```

The generated listener interface is the same, except for `init_expr(…)` and a few extra optional methods used to recover from syntax errors.  

# Where to Go From Here

The [Lexigram book](https://www.unscript.net/lexigram-book) has an introduction, a tutorial, and a reference with all the necessary information to start building your own parser.

You can also find some examples in the project repository:
* in [./examples](https://github.com/blueglyph/lexigram/tree/master/examples)
* in [./build-rtsgen](https://github.com/blueglyph/lexigram/tree/master/build-rtsgen), which is a [lexer/parser](https://github.com/blueglyph/lexigram/blob/master/src/rtsgen/mod.rs) for simplified grammar language used in the unit tests.

There are tests marked with `#[ignore]` in the `./examples/gen_*` crates that can be used to regenerate the examples. They show how to generate a parser programmatically, if you prefer that to the command-line executable.

Lexigram itself uses a couple of self-generated lexers/parsers to parse the lexicon (Lexi) and the grammar (Gram).

# Status

This project is still under development and shouldn't be considered fully stable yet (hence the 0.x version). See the main project's [README](https://github.com/blueglyph/lexigram/blob/master/README.md).

_The code and the documentation were entirely written by a human._

# Releases

[RELEASES.md](https://github.com/blueglyph/lexigram/blob/master/RELEASES.md) keeps a log of all the releases (most are also on the [GitHub release page](https://github.com/blueglyph/lexigram/releases)). 

# Licence

This code is licensed under either [MIT License](https://choosealicense.com/licenses/mit/) or [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/), at your option.
