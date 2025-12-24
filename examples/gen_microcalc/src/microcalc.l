lexicon MicroCalc;

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;

Add                     : '+';
Comma                   : ',';
Div                     : '/';
//Dot                     : '.';
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
Let                     : 'let';
Print                   : 'print';
Return                  : 'return';

SComment                : BlockComment              -> skip;
SLineComment            : LineComment               -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;

fragment BinDigit       : [0-1];
fragment NZBinDigit     : [1];
fragment DecDigit       : [0-9];
fragment NZDecDigit     : [1-9];
fragment HexDigit       : DecDigit | 'A'..'F' | 'a'..'f';
fragment NZHexDigit     : NZDecDigit | 'A'..'F' | 'a'..'f';
fragment BinInteger     : NZBinDigit ( '_' | BinDigit)*;
fragment DecInteger     : NZDecDigit ( '_' | DecDigit)*;
fragment HexInteger     : NZHexDigit ( '_' | HexDigit)*;

Num                     : '0b' BinInteger | '0x' HexInteger | DecInteger;
