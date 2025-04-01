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

- `channel` returns the token on a specific channel (which must be defined in a `channels` rule). `DEFAULT_CHANNEL` is the name of channel 0, which is the default output channel for all tokens:
```
channels { CH_COMMENTS, CH_WHITESPACES };
COMMENT_LINE    : '//' ~[\r\n]*     -> channel(CH_COMMENTS);
```
- `skip` discards the match:
```
WHITESPACES     : [ \t\r\n]*        -> skip;
```
- `mode` changes the mode; `push` changes the mode, too, but pushes the current mode on a stack first (mode sections are defined with the `mode` rule; `DEFAULT_MODE` is the mode name of the rules that aren't in another mode section):
```
SCRIPT_OPEN     : '<script'         -> push(SCRIPT);
ID              : [A-Za-z][A-Za-z0-9]+;

mode SCRIPT;
SCRIPT_CLOSE    : '>'               -> pop;
SCRIPT_INCLUDE  : 'include';
SCRIPT_EXECUTE  : 'execute';
SCRIPT_ID       : [a-z][a-z0-9]+;
```
- `more` continues evaluating the rules but keep all the characters that have been matched so far:
```
TAG_OPEN        : '<'                   -> mode(TAG), more;

mode TAG:
TAG_MORE        : [a-zA-Z][a-zA-Z0-9]*  -> more;
TAG             : '>'                   -> mode(DEFAULT_MODE);
```

If `skip` or `more` is in the list of actions, no token is returned. Otherwise, the rule returns the token related to the rule name or, if specified, to the `type(token)` action.

```
STRING          : '"'         -> push(STRING), more;

mode STRING;
STRING_CLOSE    : '"'         -> pop, type(STRING);
STRING_CONTENT  : [ -z]*      -> more;
```

Fragments of lexemes that are used several times or that should be isolated can be defined by `fragment`. They're used as an alias inside rules, and they don't return any token nor can accept actions on their own.

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
  - `\n \r \t \\ \] \- \.` are escape codes for literals that cannot be directly put into square brackets (both `[` and `\[` are accepted, though)
- `~`: negates the character set on the right
  - `~[ \t\n\r]`: any non-space character
  - `~( 'a' | 'b' | 'c' )`: any character but 'a', 'b', or 'c', equivalent to `~'a'..'c'`
- `*`: zero or more instances of the left item
  - `[a-z]*`: zero or several letters
  - `ab*`: 'a' followed by zero or more 'b' characters
  - `('--'|'==')*`: zero or more pairs of '-' or '='
- `+`: one or more instances of the left item (same principle as `*`)
- `?`: zero or one instances of the left item (same principle as `*`)
  - after `*` or `+`: lazy repeater; for example, `'/*' .*? '*/'`
- `|`: alternative between several items; for example, `( 'if' | 'while' | [a-z]+ )`
- fragment; for example, `( DIGIT | [a-f] )+` where `DIGIT' has been defined as a fragment

## Lexi Lexer Rules

```
lexicon LexiLexer;

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;
fragment HexDigit       : [0-9a-fA-F];
fragment UnicodeEsc     : 'u{' HexDigit+ '}';
fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
fragment Char           : EscChar | ~[\n\r\t'\\];
fragment CharLiteral    : '\'' Char '\'';
fragment StrLiteral     : '\'' Char Char+ '\'';

// Char inside a '[' ']' set
fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);

Arrow           : '->'; /* // first token // */
Colon           : ':';
Comma           : ',';
Dot             : '.';
Ellipsis        : '..';
Lbracket        : '{';
Lparen          : '(';
Negate          : '~';
Minus           : '-';
Plus            : '+';
Or              : '|';
Question        : '?';
Rbracket        : '}';
Rparen          : ')';
Semicolon       : ';';
Star            : '*';

Channels        : 'channels';
Fragment        : 'fragment';
Lexicon         : 'lexicon';
Mode            : 'mode';
Pop             : 'pop';
Push            : 'push';
More            : 'more';
Skip            : 'skip';
Type            : 'type';
Channel         : 'channel';
SymEof          : 'EOF';

SComment        : BlockComment              -> skip;
SLineComment    : LineComment               -> skip;
SWhiteSpace     : [ \n\r\t]+                -> skip;

Id              : [a-zA-Z][a-zA-Z_0-9]*;

CharLit         : CharLiteral;
StrLit          : StrLiteral;

FixedSet        : ('\\w' | '\\d');

LSbracket       : '['                       -> push(MODE_SET_CHAR);

mode MODE_SET_CHAR;

RSbracket       : ']'                       -> pop;
Minus2          : '-'                       -> type(Minus);
SetChar         : (EscSetChar | ~[\n\r\t\\\]\-]);
FixedSet2       : ('\\w' | '\\d')           -> type(FixedSet);
```

## Lexi Parser Rules

```
grammar LexiParser;

file: header? file_item*;

file_item:
    option | declaration | rule
;

header:
    LEXICON ID SEMICOLON
;

declaration:
    MODE ID SEMICOLON
;

option:
    CHANNELS LBRACKET ID (COMMA ID)* RBRACKET
;

rule:
    FRAGMENT ID COLON match SEMICOLON
|   ID COLON match (ARROW actions)? SEMICOLON
;

actions:
    action (COMMA action)*
;

action:
    MODE LPAREN ID RPAREN
|   PUSH LPAREN ID RPAREN
|   POP
|   SKiP
|   MORE
|   TYPE LPAREN ID RPAREN
|   CHANNEL LPAREN ID RPAREN
;

match:
    alt_items
;

alt_items:
    alt_item (OR alt_item)*
;

alt_item:
	repeat_item+
;

repeat_item:
    item STAR QUESTION?
|   item PLUS QUESTION?
|   item QUESTION?
;

item:
    ID
|   CHAR_LIT (ELLIPSIS CHAR_LIT)?
|   STR_LIT
|   char_set
|   LPAREN alt_items RPAREN
|   NEGATE item
;

char_set:
    LSBRACKET (char_set_one)+ RSBRACKET
|   DOT
|   FIXED_SET
;

char_set_one:
    SET_CHAR MINUS SET_CHAR
|   SET_CHAR
|   FIXED_SET
;
```
