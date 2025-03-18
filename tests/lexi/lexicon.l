lexicon LexiLexer;

fragment BlockComment	: '/*' .*? '*/';
fragment LineComment	: '//' ~[\r\n]*;
fragment HexDigit		: [0-9a-fA-F];
fragment UnicodeEsc		: 'u{' HexDigit+ '}';
fragment EscChar		: '\\' ([nrt'\\] | UnicodeEsc);
fragment Char			: EscChar | ~[\n\r\t'\\];
fragment CharLiteral	: '\'' Char '\'';
fragment StrLiteral		: '\'' Char Char+ '\'';

// Char inside a '[' ']' set
fragment EscSetChar		: '\\' ([nrt\\[\]\-] | UnicodeEsc);

ARROW			: '->'; /* // first token // */
COLON			: ':';
COMMA			: ',';
DOT             : '.';
ELLIPSIS		: '..';
LBRACKET    	: '{';
LSBRACKET       : '[';
LPAREN			: '(';
NEGATE			: '~';
MINUS           : '-';
PLUS			: '+';
OR				: '|';
QUESTION		: '?';
RBRACKET    	: '}';
RSBRACKET       : ']';
RPAREN			: ')';
SEMICOLON		: ';';
STAR			: '*';

CHANNELS		: 'channels';
FRAGMENT		: 'fragment';
LEXICON			: 'lexicon';
MODE			: 'mode';
POP				: 'pop';
PUSH			: 'push';
MORE			: 'more';
SKiP			: 'skip';
TYPE            : 'type';
CHANNEL         : 'channel';
SYM_EOF			: 'EOF';

COMMENT			: BlockComment 				-> skip;
LINECOMMENT		: LineComment				-> skip;
WHITESPACE		: [ \n\r\t]+				-> skip;

ID				: [a-zA-Z][a-zA-Z_0-9]*;

CHAR_LIT		: CharLiteral;
STR_LIT			: StrLiteral;

FIXED_SET       : ('\\w' | '\\d');
SET_CHAR        : (EscSetChar | ~[\n\r\t\\\]\-]);
