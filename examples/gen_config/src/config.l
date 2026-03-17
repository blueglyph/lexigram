lexicon Config;

fragment EscChar        : '\\' [nrt"\\];
fragment Char           : EscChar | ~[\n\r\t"\\];

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
StrLiteral_open         : '"'                       -> skip, mode(STR_MODE);

SComment                : '/*' .*? '*/'             -> skip;
SLineComment            : '//' ~[\r\n]*             -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

mode STR_MODE;

StrLiteral              : Char+                     -> mode(STR_CLOSE_MODE);

mode STR_CLOSE_MODE;

StrLiteral_close        : '"'                       -> skip, mode(DEFAULT_MODE);
