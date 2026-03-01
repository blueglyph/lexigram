lexicon Config;

fragment EscChar        : '\\' ([nrt"\\] | UnicodeEsc);
fragment Char           : EscChar | ~[\n\r\t"\\];
fragment StrLiteral     : '"' Char+ '"';

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
NTValue                 : 'nt_value';
Options                 : 'options';
Output                  : 'output';
Parents                 : 'parents';
Parser                  : 'parser';
Spans                   : 'spans';

// variable terminals
Id                      : [a-zA-Z][a-zA-Z_0-9]*;
BoolLiteral             : 'false' | 'true';
NumLiteral              : [0-9]+;
StrLiteral              : '"' Char+ '"';

SComment                : '/*' .*? '*/'             -> skip;
SLineComment            : '//' ~[\r\n]*             -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

