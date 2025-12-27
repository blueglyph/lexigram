lexicon MicroCalc;

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;

Eq                      : '==';
Ne                      : '!=';
Lt                      : '<';
Gt                      : '>';
Le                      : '<=';
Ge                      : '>=';
Not                     : '!';

Add                     : '+';
Comma                   : ',';
Div                     : '/';
Equal                   : '=';
Exp                     : '^';
Mul                     : '*';
Lbracket                : '{';
Lpar                    : '(';
Rbracket                : '}';
Rpar                    : ')';
Semi                    : ';';
Sub                     : '-';

Def                     : 'def';
Else                    : 'else';
If                      : 'if';
Let                     : 'let';
Print                   : 'print';
Return                  : 'return';
While                   : 'while';

SComment                : BlockComment              -> skip;
SLineComment            : LineComment               -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;

fragment BinDigit       : [0-1];
fragment DecDigit       : [0-9];
fragment HexDigit       : DecDigit | 'A'..'F' | 'a'..'f';
fragment NZHexDigit     : DecDigit | 'A'..'F' | 'a'..'f';
fragment BinInteger     : BinDigit ( '_' | BinDigit)*;
fragment DecInteger     : DecDigit ( '_' | DecDigit)*;
fragment HexInteger     : HexDigit ( '_' | HexDigit)*;

Num                     : '0b' BinInteger | '0x' HexInteger | DecInteger;
