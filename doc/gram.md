# Grammar Syntax

## Overview

Rules that define the grammatical syntax of the parser, based on the tokens defined by the lexer.

Each production rule defines one or more production factors:

```
rule:
    Id Colon prod Semicolon
;

prod:
    prodFactor
|   prod Or prodFactor
;
```

The first production rule, `rule` in the example above, defines the top element of the text to parse. The parser
doesn't expect anything else in the stream once the top production rule has been matched with the stream content,
so it will trigger an error if there are extra tokens in the stream. The `EOF` symbol can be explicitly stated
in the production rules, as if it had to match the end of the stream, but it's optional since it's automatically
done by the parser by default.

Before and after parsing each production rule, a specific user callback function is called: `init_<name>(...)` before
and `exit_<name>(...)` after, where `<name>` is usually the name of the rule. If the production rule has a value, 
meaning if the user wants to associate that grammatical element with a custom value, the exit (and sometimes the init)
function must return a value, that will be used to represent that element when it's present in upper production rules.

In the example above, `init_rule(...)` is called before parsing the `prod` item, which lets the user setup any data;
for example if it is required when parsing the `prodFactor` rules - maybe a custom vector in which they will be
stored. After the content of `prod` and the final `Semicolon` have been parsed, `exit_rule(...)` is called, giving in 
argument the contextual information about items parsed or scanned in the production rule; here, `Id` because it has a 
variable content (the lexer gives the actual text of that ID), and `prod`, which has been given a value by the user when
it was parsed and the `exit_prod(...)` function was called. When a production rules has alternative production factors,
like `prod` above, the contextual information given by the `exit_prod(...)` function specifies which alternative
has been parsed, for the user's convenience.

The stream may be an infinite one; for example if the parser is processing a log file out of a process running
indefinitely. In that case, one production rule must feature a repetition over the items that repeat indefinitely:
a `+`, `*`, or a left / right recursion. In the case of `+`, `*`, and right recursion over very long or infinite
series of items, it's advised to use an `<L>`-form, which directly processes each item without intermediate
storage to comply with the grammar priority:
 * by default, `+` and `*` parse all the items before calling the user code back with the array of all those items
 * by default, a right recursion parses all the items, stacking them, then calls the user code back starting with the last item (since the tree is built bottom-up)

Using the `<L>`-form tells the parser that those rules are implemented a little differently for the benefit of a low
latency (and memory footprint):
 * `+` and `*` parse each item, calling the user code back with the item and the result from the previous iteration (or the initial call). No array is used. The name of the callback for the individual items can be specified with the form `<L=Id>`. By default, the name is formed by adding a postfix to the production rule name where the `+` or `*` is used (for example, in `List: (Id)+`, it could be `id1`).
 * a right recursion parses each item, calling the user code back with the item and the result from the previous iteration (or the initial call).

## Gram Lexer Rules

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
Id              : ID;
```

## Gram Parser Rules

```
grammar GramParser;

file: header rules SymEOF?;

header:
    Grammar Id Semicolon
;

rules:
    rule
|   rules rule
;

rule:
    ruleName Colon prod Semicolon
;

ruleName:
    Id
;

prod:
    prodFactor
|   prod Or prodFactor
;

prodFactor:
    prodTerm*
;

prodTerm:
    termItem (Plus | Star | Question)?
;

termItem:
    Id
|   Lform
|   Rform
|   Lparen prod Rparen
;
```
