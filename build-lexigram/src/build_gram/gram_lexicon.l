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
