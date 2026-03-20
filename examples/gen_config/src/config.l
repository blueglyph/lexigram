lexicon Config;

fragment ESC_CHAR       : '\\' [nrt"\\];
fragment CHAR           : ESC_CHAR | ~[\n\r\t"\\];

// punctuation and operators
Colon                   : ':';
Comma                   : ',';
Equal                   : '=';
Lbracket                : '{';
LSbracket               : '[';
Rbracket                : '}';
RSbracket               : ']';
Semicolon               : ';';

// keywords
Combined                : 'combined';
Def                     : 'def';
Default                 : 'default';
Headers                 : 'headers';
Indent                  : 'indent';
Input                   : 'input';
Lexer                   : 'lexer';
Libs                    : 'libs';
None                    : 'none';
NTValue                 : 'nt-value';
Options                 : 'options';
Output                  : 'output';
Parents                 : 'parents';
Parser                  : 'parser';
Set                     : 'set';
Spans                   : 'spans';
Stdout                  : 'stdout';

// variable terminals
BoolLiteral             : 'false' | 'true';
Id                      : [a-zA-Z][a-zA-Z_0-9]*;
NumLiteral              : [0-9]+;
StrLiteral              : '"' CHAR+ '"';

SComment                : '/*' .*? '*/'             -> skip;
SLineComment            : '//' ~[\r\n]*             -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;
