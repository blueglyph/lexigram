lexicon LexiLexer;

fragment BLOCK_COMMENT  : '/*' .*? '*/';
fragment LINE_COMMENT   : '//' ~[\r\n]*;
fragment HEX_DIGIT      : [0-9a-fA-F];
fragment UNICODE_ESC    : 'u{' HEX_DIGIT+ '}';
fragment ESC_CHAR       : '\\' ([nrt'\\] | UNICODE_ESC);
fragment CHAR           : ESC_CHAR | ~[\n\r\t'\\];
fragment CHAR_LITERAL   : '\'' CHAR '\'';
fragment STR_LITERAL    : '\'' CHAR CHAR+ '\'';

// Char inside a '[' ']' set
fragment ESC_SET_CHAR   : '\\' ([nrt\\[\]\-] | UNICODE_ESC);

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
Hook            : 'hook';
//SymEof          : 'EOF';

SComment        : BLOCK_COMMENT             -> skip;
SLineComment    : LINE_COMMENT              -> skip;
SWhiteSpace     : [ \n\r\t]+                -> skip;

Id              : [a-zA-Z][a-zA-Z_0-9]*;

CharLit         : CHAR_LITERAL;
StrLit          : STR_LITERAL;

FixedSet        : ('\\w' | '\\d');

LSbracket       : '['                       -> push(SET_CHAR_MODE);

mode SET_CHAR_MODE;

RSbracket       : ']'                       -> pop;
Minus2          : '-'                       -> type(Minus);
SetChar         : (ESC_SET_CHAR | ~[\n\r\t\\\]\-]);
FixedSet2       : ('\\w' | '\\d')           -> type(FixedSet);
