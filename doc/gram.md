# Grammar Syntax and Code Generation

## Overview

Rules that define the grammatical syntax of the parser, based on the tokens defined by the lexer.

Each production rule defines one or more production factors:

```
rule:
    rule_name Colon prod SymEof? Semicolon
;

rule_name:
    Id
;

prod:
    prod_term
|   prod Or prod_term
;
```

The first production rule, `rule` in the example above, defines the top element of the text to parse. The parser doesn't expect anything else in the stream once the top production rule has been matched with the stream content, so it will trigger an error if there are extra tokens in the stream. The `EOF` symbol can be explicitly stated in the production rules, as if it had to match the end of the stream, but it's optional since it's automatically done by the parser by default.

Before and after parsing each production rule, a specific user callback function is called: `init_<name>(...)` before and `exit_<name>(...)` after, where `<name>` is usually the name of the rule. If the production rule has a value, meaning if the user wants to associate that grammatical element with a custom value, the exit (and sometimes the init)function must return a value, that will be used to represent that element when it's present in upper production rules.

In the example above, `init_rule(...)` is called before parsing the `prod` item, which lets the user setup any data; for example if it is required when parsing the `prod_term` rules - maybe a custom vector in which they will be stored. After the content of `prod` and the final `Semicolon` have been parsed, `exit_rule(...)` is called, giving in argument the contextual information about items parsed or scanned in the production rule; here, `Id` because it has a variable content (the lexer gives the actual text of that ID), and `prod`, which has been given a value by the user when it was parsed and the `exit_prod(...)` function was called. When a production rules has alternative production factors, like `prod` above, the contextual information given by the `exit_prod(...)` function specifies which alternative has been parsed, for the user's convenience.

The stream may be an infinite one; for example if the parser is processing a log file out of a process running indefinitely. In that case, one production rule must feature a repetition over the items that repeat indefinitely:a `+`, `*`, or a left / right recursion. In the case of `+`, `*`, and right recursion over very long or infinite series of items, it's advised to use an `<L>`-form, which directly processes each item without intermediate storage to comply with the grammar priority:
 * by default, `+` and `*` parse all the items before calling the user code back with the array of all those items
 * by default, a right recursion parses all the items, stacking them, then calls the user code back starting with the last item (since the tree is built bottom-up)

Using the `<L>`-form tells the parser that those rules are implemented a little differently for the benefit of a low latency (and memory footprint):
 * `+` and `*` parse each item, calling the user code back with the item and the result from the previous iteration (or the initial call). No array is used. The name of the callback for the individual items can be specified with the form `<L=Id>`. By default, the name is formed by adding a postfix to the production rule name where the `+` or `*` is used (for example, in `List: (Id)+`, it could be `id1`).
 * a right recursion parses each item, calling the user code back with the item and the result from the previous iteration (or the initial call).

## Gram's Lexicon

```
lexicon GramLexer;

fragment COMMENT        : '/*' .*? '*/';
fragment LINECOMMENT    : '//' ~[\r\n]*;
fragment WHITESPACE     : [ \n\r\t]+;
fragment ID             : [a-zA-Z][a-zA-Z_0-9]*;

Comment         : COMMENT                   -> skip;
LineComment     : LINECOMMENT               -> skip;
WhiteSpace      : WHITESPACE                -> skip;

Colon           : ':';
Lparen          : '(';
Or              : '|';
Plus            : '+';
Question        : '?';
Rparen          : ')';
Semicolon       : ';';
Star            : '*';

Grammar         : 'grammar';
SymEof          : 'EOF';

Lform           : '<L' ('=' ID)? '>';
Rform           : '<R>';
Pform           : '<P>';

Id              : ID;
```

## Gram's Grammar

```
grammar GramParser;

file: header rules;

header:
    Grammar Id Semicolon
;

rules:
    rule
|   rules rule
;

rule:
    rule_name Colon prod SymEof? Semicolon
;

rule_name:
    Id
;

prod:
    prod_term
|   prod Or prod_term
;

prod_term:
    prod_factor*
;

prod_factor:
    prod_atom (Plus | Star | Question)?
;

prod_atom:
    Id
|   Lform
|   Rform
|   Pform
|   Lparen prod Rparen
;
```

# Code Generation

### Notation

Outside proper lexicon and grammar Lexigram sources, we often represent rules with a slightly different syntax than Lexigram's grammar syntax, for the sake of simplicity. These examples can be distinguished by the `->` symbol used when a nonterminal rule is defined:

```
expr -> expr "+" expr | <P> expr "-" expr | Id | Num
```

* As in Lexigram's grammar, the nonterminals are in lowercase and the terminals (tokens) begin with an uppercase letter. The attributes, like `<P>`, are the same.
* Unlike Lexigram's grammar, the terminals don't need to be defined separately: the string literals represent a fixed token, like `+`, and identifiers beginning with an uppercase letter implicitly represent variable tokens, like `Id` and `Num`.

The equivalent Lexigram definitions for the above example could be:

```
// lexicon file
Add   : "+";
Sub   : "-";
Id    : [a-z][a-z0-9]*;
Num   : [1-9][0-9]*;

// grammar file
expr: expr Add expr | <P> expr Sub expr | Id | Num;
```

## Overview of the Listener Interface

Lexigram produces the source code of a parser from the language lexicon and grammar. The parser interfaces with the user code through a listener object, which is written by the user and which must implement a custom trait generated together with the parser code. That listener is given to the parser before processing the text and can be recovered after.

Each nonterminal (rule name) can be associated to a value of a user-defined type. The parser calls a trait method bound to each nonterminal once its rule has been fully parsed, and the user implementation code of the method returns the nonterminal value based on the given context, which includes the values of the rule items: other nonterminals and variable tokens like string and numerical literals.

As an example, let's consider a simple lexicon and grammar. `Id` and `Num` are variable tokens representing a identifier name and a numerical value, respectively, and the grammar defines two rules for the nonterminals `s` and `val`:

```
// lexicon file
Equal : "=";                // fixed token
Exit  : "exit";             // fixed token
Return: "return";           // fixed token
Id    : [a-z][a-z0-9]*;     // variable token
Num   : [1-9][0-9]*;        // variable token
 
// grammar file
s:   Id Equal val | Exit | Return val;
val: Id | Num;
```

### Init and exit Methods

Gram generates the following listener trait, that the user must implement on a listener object:

```rust
pub trait TestListener {
    // ...
    fn init_s(&mut self) {}
    fn exit_s(&mut self, ctx: CtxS) -> SynS;
    fn init_val(&mut self) {}
    fn exit_val(&mut self, ctx: CtxVal) -> SynVal;
}
```

It also generates the following types, used in the callback methods:

```rust
pub enum CtxS {
    /// `s -> Id "=" val`
    V1 { id: String, val: SynVal },
    /// `s -> "exit"`
    V2,
    /// `s -> "return" val`
    V3 { val: SynVal },
}

pub enum CtxVal {
    /// `val -> Id`
    V1 { id: String },
    /// `val -> Num`
    V2 { num: String },
}
```

To make it easier to read, we omit the Rust attributes generated for each type, typically `#[derive(Debug)])`, which might be necessary for the user.

For simple rules like above, a first call is made when the next rule is determined by the parser; for example, if the "return" token is received when `s` is one of the potential next nonterminals, the rule `s -> Return val` is selected and the `init_s(&mut self)` method is called. It can be used to initialize any data before the rule is actually parsed, so before any subrules is triggered, like `val`. There are default empty implementations for the initialization methods, so they're optional.

Another call is made once the rule has been entirely parsed, including all the nonterminals it contains (here, `val`). This second call gives all the necessary information about what has been parsed in a context object, which specifies which rule alternative was parsed and the values of the relevant nonterminals and tokens. In the example above, the `exit_s(&mut self, ctx: CtxS)` method is called with `ctx` equals to `CtxS::V3 { val }`, `val` being the value of the nonterminal. 

If the parsed text is "return 5", the following calls are made:

```rust
listener.init_s();   // "return" selected the rule `s -> Return val`
listener.init_val(); // "5" selected the rule `val -> Num`
let val = listener.exit_val(CtxVal::V2 { num: "5".to_string() });
let s = listener.exit_s(CtxS::V3 { val });
```

Each nonterminal can have a value, but it's not mandatory. This choice is specified in the options given to Lexigram when it generates the parser source code. If the nonterminal `s` has a value, `exit_s` is generated with a return value of type `SynS`, this type being defined by the user. If `s` has no value, `exit_s` has no return value and the trait definition includes a default empty implementation.

### Other general methods

There are a few other methods that the parser calls in some circumstances:

```rust
pub trait TestListener {
    fn check_abort_request(&self) -> bool { false }
    fn get_mut_log(&mut self) -> &mut impl Logger;
    fn exit(&mut self, _s: SynS) {}
    // ...
}
```

The `check_abort_request(&self)` method is called after each init and exit call. It can be used to return `true` if the user detects an error; for example, if `Num` cannot be parsed as a number or `Id` hasn't been declared first. This requests the parser to abort its parsing procedure immediately. It's the user's choice to use this request or to manage the errors on their own, but it's often easier to abort if there's a risk that an undefined symbol generates too much problems down the line.

A simple way to use the abort request is to set an `abort` flag in the listener object and to return its value in that method:

```
impl TestListener for MyTestListener {
    fn check_abort_request(&self) -> bool {
        self.abort
    }
    
    // ...
}
```

For practical reasons, the listener must own a log object, in which all the warnings and errors will be recorded. The `get_mut_log(&mut self)` method returns a reference to that log object whenever the parser needs to record something.

```
pub struct MyTestListener {
    log: BufLog,
    
    // ...
}

impl TestListener for MyTestListener {
    fn get_mut_log(&mut self) -> &mut impl Logger {
        &mut self.log
    }
    
    // ...
]
```

Finally, the `exit(&mut self, s: SynS)` method is called once the top rule exits, and gives the value of its nonterminal, if it exists, as a `SynS` type. There is a default empty implementation, which makes it optional. It can be used to store the final result of the parsing in the listener object or to call any post-processing required at that level, like finalizing the AST tree.

So the entire series of calls, if the parsed text is "return 5", is actually closer to the following:

```rust
listener.init_s();   // "return" selected the rule `s -> Return val`
listener.init_val(); // "5" selected the rule `val -> Num`
let val = listener.exit_val(CtxVal::V2 { num: "5".to_string() });
if listener.check_abort_request() { return Err(ParserError::AbortRequest) }
let s = listener.exit_s(CtxS::V3 { val });
if listener.check_abort_request() { return Err(ParserError::AbortRequest) };
listener.exit(s);
return Ok(());
```

### Wrapper

The listener isn't given directly to the parser; it's given to a wrapper, which is part of the generated source code, then a mutable reference of that wrapper is given to the `parse` method. This is covered in details later.  

### Nonterminal with/without a value

In a [previous section](#overview-of-the-listener-interface), we said that nonterminals could have a value, but this isn't mandatory. When a nonterminal has no value, the generated code still includes `init` and `exit` methods, but the nonterminals are not represented in the contexts of other nonterminals that include them.

For example, consider the following rules:

```
a -> b c | c
b -> Op c
c -> Id
```

where `Op` and `Id` are variable tokens that contain string data.

If both `b` and `c` have a value, the following code is generated:

```rust
pub enum CtxA {
    /// `a -> b c`
    V1 { b: SynB, c: SynC },
    /// `a -> c`
    V2 { c: SynC },
}

pub enum CtxB {
    /// `b -> Op c`
    V1 { op: String, c: SynC },
}

pub enum CtxC {
    /// `c -> Id`
    V1 { id: String },
}

pub trait TestListener {
    // ... (other general methods)
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
    fn init_b(&mut self) {}
    fn exit_b(&mut self, ctx: CtxB) -> SynB;
    fn init_c(&mut self) {}
    fn exit_c(&mut self, ctx: CtxC) -> SynC;
}
```

As expected, the context given for the first alternative of `a` includes the data for the nonterminals `b` and `c`, with user-defined types `SynB` and `SynC`, while the second alternative only includes `c`. Similarly, `CtxB` includes a field for the `c` nonterminal.

If `c` has no value, the code becomes:

```rust
pub enum CtxA {
    /// `a -> b c`
    V1 { b: SynB },
    /// `a -> c`
    V2,
}

pub enum CtxB {
    /// `b -> c Op`
    V1 { op: String },
}

pub enum CtxC {
    /// `c -> Id`
    V1 { id: String },
}

pub trait TestListener {
    // ... (other general methods)
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
    fn init_b(&mut self) {}
    fn exit_b(&mut self, ctx: CtxB) -> SynB;
    fn init_c(&mut self) {}
    fn exit_c(&mut self, _ctx: CtxC) {}
}
```

The variant `CtxA::V2` is empty since there is no data, but it still exists so that the user knows which alternative of the `a` rule was parsed.

The trait still allows for callbacks before and after `c` is parsed, but there is no return value. This also makes the implementation of `exit_c` optional, so a default empty implementation is provided.

If both `b` and `c` have no value, the code becomes:

```rust
pub enum CtxA {
    /// `a -> b c`
    V1,
    /// `a -> c`
    V2,
}

pub enum CtxB {
    /// `b -> c Op`
    V1 { op: String },
}

pub enum CtxC {
    /// `c -> Id`
    V1 { id: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
    fn init_b(&mut self) {}
    fn exit_b(&mut self, _ctx: CtxB) {}
    fn init_c(&mut self) {}
    fn exit_c(&mut self, _ctx: CtxC) {}
}
```

Here, all the variants of the `CtxA` context are empty. If `a` only had one alternative, the context would still have one empty variant, even though it carries no information at all. It would be generated to keep the API consistent, but it could be ignored entirely.

## Grammar Transformations

Lexigram accepts non-LL(1) grammars if it's able to transforme them to LL(1). It tries to provide some degree of transparency, but some patterns require specific listener calls.

The transformations done by Lexigram are:
* `?` operator: `a -> X?` becomes `X | ε`
* `*` operators: `a -> X*` becomes `a -> a1`  `a1 -> X a1 | ε`
* `+` operators: `a -> X+` becomes `a -> a1`  `a1 -> X a1 | X`
* left recursion: 
  * `a -> a X | Y` becomes `a -> Y a1` `a1 -> X a1 | ε`
  * `a -> a X a | Y` and more complex ambiguities, typically used in expression parsing, are transformed with the Clarke method, which is detailed later
* left factorization: `a -> A B | A C` becomes `a -> A a1` `a1 -> B | C`

A few rules have two implementations: normal or low-latency (L). Firstly in the `+` and `*` repetitions:
* `a -> X*` becomes `a -> a1`  `a1 -> X a1 | ε`, but the `a1` iterator nonterminal is not visible to the user. The generated code gathers all the values and puts them in the context of the `exit_a` method.
* `a -> (<L=i> X)*` becomes `a -> i` `i -> X i | ε`, as above (note that the iterator nonterminal is explicitly named by the user), but a callback method is generated for `i` so that the user can manage the iterations manually, almost as if the user had written the transformed rules in the original grammar. 
* The same applies to `+`.

Secondly, in right-recursive rules, which don't need to be transformed since it's LL(1).
* `a -> X a | Y` behaves as expected: since it's right recursive, a text "XXY" is decomposed as `a -> X (a -> X (a -> Y))`. The first `exit_a` call occurs on the end of the iterations, with `a -> Y`, and the next two calls backtrack on the 2nd "X" then on the first "X". This means that the first call occurs after the parser has already parsed the entire sequence and put all the intermediate values on the stack, which it empties one element at a time when calling `exit_a`.
* `a -> X <L> a | Y` is offering a shortcut to avoid the latency of the normal behaviour, by calling `exit_a` on each newly found value, a little as if it was a left-recursive rule. The first `exit_a` call occurs on the first "X", the 2nd on the next "X", and the last on the "Y". It is then up to the user to rebuild the AST in the desired order.

The motivation behind the `<L>` versions is offering the ability to parse a potentially infinite stream without storing all the intermediate items, or being able to produce an immediate output before reaching the end of the sequence in the stream.

## Base cases

### Repetitions with *

```
a -> A B* C
```

is transformed into

```
a -> A a_1 C
a_1 -> B a_1 | ε    
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> A B* C`
    V1 { a: String, star: SynA1, c: String },
}

pub struct SynA1(pub Vec<String>);

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

Sequencing of the operations:
* `init_a` is called before the rule is parsed.
* `exit_a` is called after `a -> A B* C` is parsed. The repetition is available in the `star` field, which wraps a vector containing all the parsed values.

### Repetitions with +

```
a -> A B+ C
```

is transformed into

```
a -> A a_1 C
a_1 -> B a_2
a_2 -> a_1 | ε    
```

The generated code is very similar to the `*` repetition:

```rust
pub enum CtxA {
    /// `a -> A B+ C`
    V1 { a: String, plus: SynA1, c: String },
}

/// Computed `B+` array in `a -> A  ►► B+ ◄◄  C`
pub struct SynA1(pub Vec<String>);

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

### Repetitions with * and `<L>` attribute

```
a -> A (<L=i> B)* C
```

is transformed into

```
a -> A i C
i -> B i | ε    
```

Note that the nonterminal used to represent the iterations must be defined by the user, since its type is user-defined (here, `i` of type `SynI`).

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> A (<L> B)* C`
    V1 { a: String, star: SynI, c: String },
}

pub enum CtxI {
    /// `<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )* C`
    V1 { star_it: SynI, b: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, ctx: CtxI) -> SynI;
    fn exitloop_i(&mut self, _star_it: &mut SynI) {}
}
```

* `init_a` is called before the rule `a` is parsed.
* `init_i` is called before the rule `i` is parsed for the first time. This is a good place to initialize the whole series; the method must return the accumulator variable that will be updated at each iteration.
* `exit_i` is called after parsing each item of the repetition, if there are any item. It receives the current value of the accumulator in `star_it` and the values of the parsed items, here `b`. It must return the updated accumulator.
* `exitloop_i` is called after the last iteration. It can be used if the accumulator requires post-processing once the whole series has been parsed.

### Repetitions with + and `<L>` attribute

```
a -> A (<L=i> B)+ C
```

is transformed into

```
a -> A i C
i -> B a_1
a_1 -> i | ε  
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> A (<L> B)+ C`
    V1 { a: String, plus: SynI, c: String },
}

pub enum CtxI {
    /// `<L> B` iteration in `a -> A ( ►► <L> B ◄◄ )+ C`
    V1 { plus_it: SynI, b: String, last_iteration: bool },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynMyA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, ctx: CtxI) -> SynI;
}
```

The differences from a repetition with `<L>` * are:
* A `last_iteration` field that flags the last iteration, when the `a_1 -> ε` alternative is reached. 
* No `exitloop_i` method, which has been replaced by the flag above because the last iteration contains the same data as the previous iterations.
* `exit_i` is sure to be called at least once.
* The accumulator field is named `plus_it` instead of `star_it`.

### Right recursion

```
expr -> Id "." expr | "(" Num ")"
```

Right-recursive rules don't need to be modified. The generated code is as follows:

```rust
pub enum CtxExpr {
    /// `expr -> Id "." expr`
    V1 { id: String, expr: SynExpr },
    /// `expr -> "(" Num ")"`
    V2 { num: String },
}

pub trait TestListener {
    // ...
    fn init_expr(&mut self) {}
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}
```

### Right recursion with `<L>` attribute

```
expr -> <L> Id "." expr | "(" Num ")"
```

The generated code is as follows:

```rust
pub enum CtxExpr {
    /// `expr -> <L> Id "." expr`
    V1 { expr: SynExpr, id: String },
    /// `expr -> "(" Num ")"`
    V2 { expr: SynExpr, num: String },
}

pub trait TestListener {
    // ...
    fn init_expr(&mut self) -> SynExpr;
    fn exit_expr(&mut self, ctx: CtxExpr) -> SynExpr;
}
```

Contrary to repetitions, the user mustn't define a name in the attribute, since there is no transformation nor any associated nonterminal (said differently, the nonterminal associated with the attribute is the nonterminal of the rule, so there's no need to specify it).

### Left recursion

```
e -> f | e "." Id
```

is transformed into 

```
e -> f e_1       
e_1 -> "." Id e_1 | ε         
```

The generated code is as follows:

```rust
pub enum CtxE {
    /// `e -> f`
    V1 { f: SynF },
    /// `e -> e "." Id`
    V2 { e: SynE, id: String },
}

pub trait TestListener {
    // ...
    fn init_e(&mut self) {}
    fn exit_e(&mut self, ctx: CtxE) -> SynE;
    fn exitloop_e(&mut self, _e: &mut SynE) {}
}
```

This is similar to an `<L>` repetition, except the initialization part. The accumulator is not returned by `init_e`, but it's created at the first iteration, when the rule matches a non-recursive alternative of the rule; in this case, `e -> f`.

* `init_e` is called before the rule is parsed.
* `exit_e` is called after each variant is parsed.
  * The first time, it's a non-recursive alternative; here, `e -> f`. It must return the initial value of the accumulator (which may be its final value if there are no iterations).
  * If there are iterations of the recursive alternative, `exit_e` is called for each iteration, with the current value of the accumulator in context and the parsed values of that iteration. It must return the updated accumulator. 
* `exitloop_e` is called after the last iteration. It can be used if the accumulator requires post-processing once the whole series has been parsed.

### Left factorization

```
a -> A | A B | A B C | A B D | E
```

which can be rewritten as

```
a -> A (ε | B (ε | C | D)) | E
              ------------ a2
       ------------------- a1
```

is transformed into

```
a -> A a_1  
a -> E      
a_1 -> B a_2
a_1 -> ε    
a_2 -> C    
a_2 -> D    
a_2 -> ε    
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> A`
    V1 { a: String },
    /// `a -> A B`
    V2 { a: String, b: String },
    /// `a -> A B C`
    V3 { a: String, b: String, c: String },
    /// `a -> A B D`
    V4 { a: String, b: String, d: String },
    /// `a -> E`
    V5 { e: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

## Advanced cases

### Ambiguous left and right recursion

Rules that are both left- and right-recursive are ambiguous in an LL(1) grammar, since they don't define the precedence nor the associativity. The typical use of such rules are for expressions containing binary operators.

Lexigram automatically transforms those rules using the Clarke method, which preserves the precedence and left-/right-associativity. The same rule can contain binary and prefixed / suffixed unary operators (e.g. `expr "+" expr`, `"-" expr` and `expr "?"`, respectively).

The precedence is determined by the order in which the operators are defined in the rule:

* The alternatives defined first have higher precedence; in this example, `*` has higher precedence than `+`

  `expr -> expr "*" expr | expr "+" expr | Num`


* The `<P>` attribute indicates an operator with the same level of precedence as the previous one on its left. For example, it can be used in a rule like

  `expr -> expr "+" expr | <P> expr "-" expr | Id | Num`

By default, binary operators are left-associative. The `<R>` attribute indicates a right-associative operator. For example,

  `expr -> <R> expr "^" expr | expr "*" expr | expr "/" expr | Num`


The attributes can be placed anywhere in the rule alternative, but we usually place them at the beginning to be consistent.

The rule

```
e -> e "*" e | <R> e "!" e | e "+" e | Num
```

is transformed into 

```
e -> e_4 e_1      
e_1 -> "*" e_4 e_1 | "!" e_2 e_1 | "+" e_2 e_1 | ε          
e_2 -> e_4 e_3    
e_3 -> "*" e_4 e_3 | "!" e_2 e_3 | ε          
e_4 -> Num        
```

The generated code is as follows:

```rust
pub enum CtxE {
    /// `e -> e "*" e`
    V1 { e: [SynE; 2] },
    /// `e -> <R> e "!" e`
    V2 { e: [SynE; 2] },
    /// `e -> e "+" e`
    V3 { e: [SynE; 2] },
    /// `e -> Num`
    V4 { num: String },
}

pub trait TestListener {
    // ...
    fn init_e(&mut self) {}
    fn exit_e(&mut self, ctx: CtxE) -> SynE;
}
```

Since there are two identical symbols in the same alternatives, they are regrouped in an array `e: [SynE; 2]`, where `e[0]` is the leftmost nonterminal and `e[1]` is the rightmost one.

The first variant `V1` is related to the operator of highest precedence, and the last variant is related to the lowest precedence.

### Repetitions of longer strings of symbols

```
a -> (b A b B A)+
```

is transformed into

```
a -> a_1            
a_1 -> b A b B A a_2
a_2 -> a_1 | ε            
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> (b A b B A)+`
    V1 { plus: SynA1 },
}

/// Computed `(b A b B A)+` array in `a ->  ►► (b A b B A)+ ◄◄ `
pub struct SynA1(pub Vec<SynA1Item>);

/// `b A b B A` item in `a -> ( ►► b A b B A ◄◄ )+`
pub struct SynA1Item { pub b: [SynB; 2], pub a: [String; 2], pub b1: String }

pub trait TestListener {
    // ..
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

In comparison to the base `*` repetition, we see that the type of the items in the list, `SynA1Item`, is a `struct` rather than a string. The same symbols have been regrouped in arrays: 
* `b: [SynB; 2]` for the two nonterminals `b`.
* `a: [String; 2]` for the two tokens `A` (note that the field must be lowercase, despite representing a token), which contain a string.

Also, the token `B` is represented by `b1`, since the name `b` was already taken. The fields are in the same order as the first symbol they represent in the rule; here, `b` is first, `A` second, and `B` third. Their type is another indication that helps set them apart: the type of a token is a string, while a nonterminal has a custom type whose name begins by "Syn", for _synthesized_.

### Nested repetitions

```
a -> (A (b ",")* ";")* C
```

is transformed into

```
a -> a_2 C          
b -> B              
a_1 -> b "," a_1 | ε            
a_2 -> A a_1 ";" a_2 | ε            
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> (A (b ",")* ";")* C`
    V1 { star: SynA2, c: String },
}

/// Computed `(b ",")*` array in `a -> (A  ►► (b ",")* ◄◄  ";")* C`
pub struct SynA1(pub Vec<SynB>);

/// Computed `(A (b ",")* ";")*` array in `a ->  ►► (A (b ",")* ";")* ◄◄  C`
pub struct SynA2(pub Vec<SynA2Item>);

/// `A (b ",")* ";"` item in `a -> ( ►► A (b ",")* ";" ◄◄ )* C`
pub struct SynA2Item { pub a: String, pub star: SynA1 }

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

When a repetitions is nested in another, the resulting vector becomes a field of the outer repetition's type. Here, `SynA2Item::star` of type `SynA1` contains a `Vec<SynB>`, where `SynB` contains the only data of the inner loop, which is the nonterminal `b`. Each generated type has a doc comment that emphasizes the part of the original rule it represents between "►►" and "◄◄" arrows.

It's also possible to nest any combination of non-`<L>` and `<L>` repetitions. The principle is always the same: non-`<L>` repetitions are represented by a `Vec` of values gathered automatically by the parser, and an `<L>` repetition is managed manually by the user through extra nonterminal `init`/`exit` methods and is represented by a user-defined type.

### Alternatives in repetitions

```
a -> A (B | b C b B C | E)* F
```

is transformed into

```
a -> A a_1 F        
a_1 -> B a_1 | b C b B C a_1 | E a_1 | ε            
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> A (B | b C b B C | E)* F`
    V1 { a: String, star: SynA1, f: String },
}

pub enum CtxB {
    /// `b -> D`
    V1 { d: String },
}

/// Computed `(B | b C b B C | E)*` array in `a -> A  ►► (B | b C b B C | E)* ◄◄  F`
pub struct SynA1(pub Vec<SynA1Item>);

pub enum SynA1Item {
    /// `B` item in `a -> A ( ►► B ◄◄  | b C b B C | E)* F`
    V1 { b: String },
    /// `b C b B C` item in `a -> A (B |  ►► b C b B C ◄◄  | E)* F`
    V2 { b: [SynB; 2], c: [String; 2], b1: String },
    /// `E` item in `a -> A (B | b C b B C |  ►► E ◄◄ )* F`
    V3 { e: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
}
```

Since each iteration can be one of 3 strings of symbols, its type `SynA1Item` is an enum with 3 variants, each containing the corresponding data.

### Alternatives in repetitions with `<L>` attribute

```
a -> A (<L=i> (<L=j> b C b B C | D)* E | F)* G
```

is transformed into:

```
a -> A i G      
i -> j E i | F i | ε          
j -> b C b B C j | D j | ε          
```

The generated code is as follows:

```rust
pub enum CtxA {
    /// `a -> A (<L> (<L> b C b B C | D)* E | F)* G`
    V1 { a: String, star: SynI, g: String },
}

pub enum CtxI {
    /// `<L> (<L> b C b B C | D)* E` iteration in `a -> A ( ►► <L> (<L> b C b B C | D)* E ◄◄  | F)* G`
    V1 { star_it: SynI, star: SynJ, e: String },
    /// `F` iteration in `a -> A (<L> (<L> b C b B C | D)* E |  ►► F ◄◄ )* G`
    V2 { star_it: SynI, f: String },
}

pub enum CtxJ {
    /// `<L> b C b B C` iteration in `a -> A (<L> ( ►► <L> b C b B C ◄◄  | D)* E | F)* G`
    V1 { star_it: SynJ, b: [SynB; 2], c: [String; 2], b1: String },
    /// `D` iteration in `a -> A (<L> (<L> b C b B C |  ►► D ◄◄ )* E | F)* G`
    V2 { star_it: SynJ, d: String },
}

pub trait TestListener {
    // ...
    fn init_a(&mut self) {}
    fn exit_a(&mut self, ctx: CtxA) -> SynA;
    fn init_i(&mut self) -> SynI;
    fn exit_i(&mut self, ctx: CtxI) -> SynI;
    fn exitloop_i(&mut self, _star_it: &mut SynI) {}
    fn init_j(&mut self) -> SynJ;
    fn exit_j(&mut self, ctx: CtxJ) -> SynJ;
    fn exitloop_j(&mut self, _star_it: &mut SynJ) {}
}
```

Notes:
* In case of `<L> +` repetitions, a `last_iteration` boolean field flags the last repetition instead of a `exitloop_*` method call, as seen before. 
