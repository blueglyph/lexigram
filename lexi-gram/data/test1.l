lexicon Test1;

fragment DecInteger     : [1-9] ( '_' | [0-9])*;

Add                     : '+';
Div                     : '/';
Equal                   : '=';
Mul                     : '*';
Lpar                    : '(';
Rpar                    : ')';
Semi                    : ';';
Shl                     : '<<';
Shr                     : '>>';
Sub                     : '-';

Let                     : 'let';
Print                   : 'print';

SComment                : '/*' .*? '*/'             -> skip;
SLineComment            : '//' ~[\r\n]*             -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;
Num                     : DecInteger;
