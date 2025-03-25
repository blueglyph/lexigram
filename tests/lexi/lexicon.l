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
LSbracket       : '[';
Lparen          : '(';
Negate          : '~';
Minus           : '-';
Plus            : '+';
Or              : '|';
Question        : '?';
Rbracket        : '}';
RSbracket       : ']';
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
SetChar         : (EscSetChar | ~[\n\r\t\\\]\-]);
