lexicon GramLexer;

fragment COMMENT        : '/*' .*? '*/';
fragment LINECOMMENT    : '//' ~[\r\n]*;
fragment WHITESPACE     : [ \n\r\t]+;
fragment ID             : [a-zA-Z][a-zA-Z_0-9]*;
fragment HEX_DIGIT      : [0-9a-fA-F];
fragment UNICODE_ESC    : 'u{' HEX_DIGIT+ '}';
fragment ESC_CHAR       : '\\' ([nrt'\\] | UNICODE_ESC);
fragment CHAR           : ESC_CHAR | ~[\n\r\t'\\];
fragment STR_LITERAL    : '\'' CHAR+ '\'';

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
Sep             : '/';
Star            : '*';
StrLit          : STR_LITERAL;

Grammar         : 'grammar';
SymEof          : 'EOF';

Lform           : '<L' ('=' ID)? '>';
Rform           : '<R>';
Pform           : '<P>';
Greedy          : '<G>';
ResolveTag      : '<resolve>';

Id              : ID;
