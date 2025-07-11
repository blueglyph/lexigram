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
    Lexicon Id Semicolon
;

declaration:
    Mode Id Semicolon
;

option:
    Channels Lbracket Id (Comma Id)* Rbracket
;

rule:
    rule_fragment_name Colon match Semicolon
|   rule_terminal_name Colon match (Arrow actions)? Semicolon
;

rule_fragment_name:
    Fragment Id
;

rule_terminal_name:
    Id
;

actions:
    action (Comma action)*
;

action:
    Mode Lparen Id Rparen
|   Push Lparen Id Rparen
|   Pop
|   Skip
|   More
|   Type Lparen Id Rparen
|   Channel Lparen Id Rparen
;

match:
    alt_items
;

alt_items:
    alt_item (Or alt_item)*
;

alt_item:
	repeat_item+
;

repeat_item:
    item Star Question?
|   item Plus Question?
|   item Question?
;

item:
    Id
|   CharLit (Ellipsis CharLit)?
|   StrLit
|   char_set
|   Lparen alt_items Rparen
|   Negate item
;

char_set:
    LSbracket (char_set_one)+ RSbracket
|   Dot
|   FixedSet
;

char_set_one:
    SetChar Minus SetChar
|   SetChar
|   FixedSet
;
```

Ops:
```
//  0: file -> header file_1                     | ◄0 ►file_1 ►header                   | header file_1
//  1: file -> file_1                            | ◄1 ►file_1                           | file_1
//  2: file_item -> option                       | ◄2 ►option                           | option
//  3: file_item -> declaration                  | ◄3 ►declaration                      | declaration
//  4: file_item -> rule                         | ◄4 ►rule                             | rule
//  5: header -> lexicon Id ;                    | ◄5 ; Id! lexicon                     | Id
//  6: declaration -> mode Id ;                  | ◄6 ; Id! mode                        | Id
//  7: option -> channels { Id option_1 }        | ◄7 } ►option_1 Id! { channels        | Id option_1
//  8: rule -> rule_fragment_name : match ;      | ◄8 ; ►match : ►rule_fragment_name    | rule_fragment_name match
//  9: rule -> rule_terminal_name : match rule_1 | ►rule_1 ►match : ►rule_terminal_name | 
// 10: rule_fragment_name -> fragment Id         | ◄10 Id! fragment                     | Id
// 11: rule_terminal_name -> Id                  | ◄11 Id!                              | Id
// 12: actions -> action actions_1               | ◄12 ►actions_1 ►action               | action actions_1
// 13: action -> mode ( Id )                     | ◄13 ) Id! ( mode                     | Id
// 14: action -> push ( Id )                     | ◄14 ) Id! ( push                     | Id
// 15: action -> pop                             | ◄15 pop                              | 
// 16: action -> skip                            | ◄16 skip                             | 
// 17: action -> more                            | ◄17 more                             | 
// 18: action -> type ( Id )                     | ◄18 ) Id! ( type                     | Id
// 19: action -> channel ( Id )                  | ◄19 ) Id! ( channel                  | Id
// 20: match -> alt_items                        | ◄20 ►alt_items                       | alt_items
// 21: alt_items -> alt_item alt_items_1         | ◄21 ►alt_items_1 ►alt_item           | alt_item alt_items_1
// 22: alt_item -> alt_item_1                    | ◄22 ►alt_item_1                      | alt_item_1
// 23: repeat_item -> item repeat_item_1         | ►repeat_item_1 ►item                 | 
// 24: item -> ( alt_items )                     | ◄24 ) ►alt_items (                   | alt_items
// 25: item -> ~ item                            | ◄25 ►item ~                          | item
// 26: item -> Id                                | ◄26 Id!                              | Id
// 27: item -> CharLit item_1                    | ►item_1 CharLit!                     | 
// 28: item -> StrLit                            | ◄28 StrLit!                          | StrLit
// 29: item -> char_set                          | ◄29 ►char_set                        | char_set
// 30: char_set -> [ char_set_1 ]                | ◄30 ] ►char_set_1 [                  | char_set_1
// 31: char_set -> .                             | ◄31 .                                | 
// 32: char_set -> FixedSet                      | ◄32 FixedSet!                        | FixedSet
// 33: char_set_one -> FixedSet                  | ◄33 FixedSet!                        | FixedSet
// 34: char_set_one -> SetChar char_set_one_1    | ►char_set_one_1 SetChar!             | 
// 35: file_1 -> file_item file_1                | ●file_1 ◄35 ►file_item               | file_1 file_item
// 36: file_1 -> ε                               | ◄36                                  | 
// 37: option_1 -> , Id option_1                 | ●option_1 ◄37 Id! ,                  | option_1 Id
// 38: option_1 -> ε                             | ◄38                                  | 
// 39: actions_1 -> , action actions_1           | ●actions_1 ◄39 ►action ,             | actions_1 action
// 40: actions_1 -> ε                            | ◄40                                  | 
// 41: alt_items_1 -> | alt_item alt_items_1     | ●alt_items_1 ◄41 ►alt_item |         | alt_items_1 alt_item
// 42: alt_items_1 -> ε                          | ◄42                                  | 
// 43: alt_item_1 -> repeat_item alt_item_2      | ►alt_item_2 ►repeat_item             | 
// 44: char_set_1 -> char_set_one char_set_2     | ►char_set_2 ►char_set_one            | 
// 45: rule_1 -> -> actions ;                    | ◄45 ; ►actions ->                    | rule_terminal_name match actions
// 46: rule_1 -> ;                               | ◄46 ;                                | rule_terminal_name match
// 47: repeat_item_1 -> + repeat_item_2          | ►repeat_item_2 +                     | 
// 48: repeat_item_1 -> ?                        | ◄48 ?                                | item
// 49: repeat_item_1 -> * repeat_item_3          | ►repeat_item_3 *                     | 
// 50: repeat_item_1 -> ε                        | ◄50                                  | item
// 51: item_1 -> .. CharLit                      | ◄51 CharLit! ..                      | CharLit CharLit
// 52: item_1 -> ε                               | ◄52                                  | CharLit
// 53: char_set_one_1 -> - SetChar               | ◄53 SetChar! -                       | SetChar SetChar
// 54: char_set_one_1 -> ε                       | ◄54                                  | SetChar
// 55: alt_item_2 -> alt_item_1                  | ●alt_item_1 ◄55                      | alt_item_1 repeat_item
// 56: alt_item_2 -> ε                           | ◄56                                  | alt_item_1 repeat_item
// 57: char_set_2 -> char_set_1                  | ●char_set_1 ◄57                      | char_set_1 char_set_one
// 58: char_set_2 -> ε                           | ◄58                                  | char_set_1 char_set_one
// 59: repeat_item_2 -> ?                        | ◄59 ?                                | item
// 60: repeat_item_2 -> ε                        | ◄60                                  | item
// 61: repeat_item_3 -> ?                        | ◄61 ?                                | item
// 62: repeat_item_3 -> ε                        | ◄62                                  | item
```

Parsing table:
```
//                    | ->   :   ,   .  ..   {   (   ~   -   +   |   ?   }   )   ;   *  channels fragment lexicon mode pop push more skip type channel EOF Id  CharLit StrLit FixedSet  [   ]  SetChar  $ 
// -------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// file               |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     1        1        0     1    .   .    .    .    .      .     .   1     .      .       .      .   .     .     1 
// file_item          |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     2        4        .     3    .   .    .    .    .      .     .   4     .      .       .      .   .     .     p 
// header             |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     p        p        5     p    .   .    .    .    .      .     .   p     .      .       .      .   .     .     p 
// declaration        |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     p        p        .     6    .   .    .    .    .      .     .   p     .      .       .      .   .     .     p 
// option             |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     7        p        .     p    .   .    .    .    .      .     .   p     .      .       .      .   .     .     p 
// rule               |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     p        8        .     p    .   .    .    .    .      .     .   9     .      .       .      .   .     .     p 
// rule_fragment_name |  .   p   .   .   .   .   .   .   .   .   .   .   .   .   .   .     .        10       .     .    .   .    .    .    .      .     .   .     .      .       .      .   .     .     . 
// rule_terminal_name |  .   p   .   .   .   .   .   .   .   .   .   .   .   .   .   .     .        .        .     .    .   .    .    .    .      .     .  11     .      .       .      .   .     .     . 
// actions            |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   p   .     .        .        .     12  12   12   12   12   12    12     .   .     .      .       .      .   .     .     . 
// action             |  .   .   p   .   .   .   .   .   .   .   .   .   .   .   p   .     .        .        .     13  15   14   17   16   18    19     .   .     .      .       .      .   .     .     . 
// match              |  p   .   .  20   .   .  20  20   .   .   .   .   .   .   p   .     .        .        .     .    .   .    .    .    .      .     .  20    20      20      20    20   .     .     . 
// alt_items          |  p   .   .  21   .   .  21  21   .   .   .   .   .   p   p   .     .        .        .     .    .   .    .    .    .      .     .  21    21      21      21    21   .     .     . 
// alt_item           |  p   .   .  22   .   .  22  22   .   .   p   .   .   p   p   .     .        .        .     .    .   .    .    .    .      .     .  22    22      22      22    22   .     .     . 
// repeat_item        |  p   .   .  23   .   .  23  23   .   .   p   .   .   p   p   .     .        .        .     .    .   .    .    .    .      .     .  23    23      23      23    23   .     .     . 
// item               |  p   .   .  29   .   .  24  25   .   p   p   p   .   p   p   p     .        .        .     .    .   .    .    .    .      .     .  26    27      28      29    29   .     .     . 
// char_set           |  p   .   .  31   .   .   p   p   .   p   p   p   .   p   p   p     .        .        .     .    .   .    .    .    .      .     .   p     p      p       32    30   .     .     . 
// char_set_one       |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       33     .   p    34     . 
// file_1             |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     35       35       .     35   .   .    .    .    .      .     .  35     .      .       .      .   .     .    36 
// option_1           |  .   .  37   .   .   .   .   .   .   .   .   .  38   .   .   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       .      .   .     .     . 
// actions_1          |  .   .  39   .   .   .   .   .   .   .   .   .   .   .  40   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       .      .   .     .     . 
// alt_items_1        | 42   .   .   .   .   .   .   .   .   .  41   .   .  42  42   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       .      .   .     .     . 
// alt_item_1         |  p   .   .  43   .   .  43  43   .   .   p   .   .   p   p   .     .        .        .     .    .   .    .    .    .      .     .  43    43      43      43    43   .     .     . 
// char_set_1         |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       44     .   p    44     . 
// rule_1             | 45   .   .   .   .   .   .   .   .   .   .   .   .   .  46   .     p        p        .     p    .   .    .    .    .      .     .   p     .      .       .      .   .     .     p 
// repeat_item_1      | 50   .   .  50   .   .  50  50   .  47  50  48   .  50  50  49     .        .        .     .    .   .    .    .    .      .     .  50    50      50      50    50   .     .     . 
// item_1             | 52   .   .  52  51   .  52  52   .  52  52  52   .  52  52  52     .        .        .     .    .   .    .    .    .      .     .  52    52      52      52    52   .     .     . 
// char_set_one_1     |  .   .   .   .   .   .   .   .  53   .   .   .   .   .   .   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       54     .  54    54     . 
// alt_item_2         | 56   .   .  55   .   .  55  55   .   .  56   .   .  56  56   .     .        .        .     .    .   .    .    .    .      .     .  55    55      55      55    55   .     .     . 
// char_set_2         |  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .     .        .        .     .    .   .    .    .    .      .     .   .     .      .       57     .  58    57     . 
// repeat_item_2      | 60   .   .  60   .   .  60  60   .   .  60  59   .  60  60   .     .        .        .     .    .   .    .    .    .      .     .  60    60      60      60    60   .     .     . 
// repeat_item_3      | 62   .   .  62   .   .  62  62   .   .  62  61   .  62  62   .     .        .        .     .    .   .    .    .    .      .     .  62    62      62      62    62   .     .     . 
```