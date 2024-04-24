# Lexicon Syntax

## Overview

Lexer rules defined the tokens that are returned by the lexer:

```
ASSIGN		    : '=';
ADD             : '+';
IF              : 'if';
WHILE           : 'while';
ID              : [_a-z][_a-z0-9]+;
STRING_LITERAL  : '"' (~('"'|'\n'|'\r') | '""')* '"';
```

The default action of a rule is to return the token, but other actions can be specified:

- return the token on a specific channel (which must be defined in a `channels` rule):
```
channels { CH_COMMENTS, CH_WHITESPACES };
COMMENT_LINE    : '//' ~[\r\n]*     -> channel(CH_COMMENTS);
```
- discard the match:
```
WHITESPACES     : [ \t\r\n]*        -> skip;
```
- change the mode (mode sections are defined with the `mode` rule; `DEFAULT_MODE` is the mode name of the rules that aren't in another mode section):
```
SCRIPT_OPEN     : '<script'         -> push(script);
ID              : [A-Za-z][A-Za-z0-9]+;

mode script;
SCRIPT_CLOSE    : '>'               -> pop;
SCRIPT_INCLUDE  : 'include';
SCRIPT_EXECUTE  : 'execute';
SCRIPT_ID       : [a-z][a-z0-9]+;
```

The `channel` action returns the token, but the other actions don't. To force the return of a token, the `return` action can be added:

```
SCRIPT_OPEN     : '<script'         -> push(script), return;
```

Fragments of lexemes that are used several times or that should be isolated can be defined by `fragment`.

```
fragment DIGIT      : [0-9];
fragment HEX_DIGIT  : ( DIGIT | [A-Fa-f] );
fragment DEC_INTEGER: DIGIT ( '_' | DIGIT )* DIGIT | DIGIT;
fragment HEX_INTEGER: HEX_DIGIT ('_' | HEX_DIGIT)* HEX_DIGIT | HEX_DIGIT;

INTEGER             : DEC_INTEGER | '0x' HEX_INTEGER;
```

## Regular Expressions

The regular expressions used on the right of the colon, in each matching or fragment rule, can include the following elements.

- `'a'`, `'script'`: literals that must be matched entirely
  - `\n \r \t \' \\` are escape codes for literals that cannot be directly put into quotes
  - the general UNICODE `'\u{0}'-'\u{d7ff}', '\u{e000}'-'\u{10ffff}'`
- `'a'..'z'`: literal range (single character only)
- `[abc]`, `[a-z0-9_]`, `[+\-*/]`: literal set (ranges are defined with a `-`, that must otherwise be escaped to include the minus character)
  - `\w` represents a single word character: `[a-zA-Z]`
  - `\s` represents a space character: `[ \t\r\n]`
  - `\d` represents a decimal digit: `[0-9]`
  - `.` represents any character
  - `\n \r \t \\ \[ \] \- \.` are escape codes for literals that cannot be directly put into square brackets
- `~`: negates the character set on the right
  - `~[ \t\n\r]`: any non-space character
  - `~( 'a' | 'b' | 'c' )`: any character but 'a', 'b', or 'c', equivalent to `~'a'..'c'`
- `*`: zero or more instances of the left item
  - `[a-z]*`: zero or several letters
  - `ab*`: 'a' followed by zero or more 'b' characters
  - `('--'|'==')*`: zero or more pairs of '-' or '='
- `+`: one or more instances of the left item (same principle as `*`)
- `?`: zero or one instances of the left item (same principle as `*`)
- `|`: alternative between several items; for example, `( 'if' | 'while' | [a-z]+ )`
- fragment; for example, `( DIGIT | [a-f] )+` where `DIGIT' has been defined as a fragment
