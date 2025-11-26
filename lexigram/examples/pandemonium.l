lexicon Pandemonium;

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;
fragment DecDigit       : [0-9];
fragment NZDecDigit     : [1-9];
fragment DecInteger     : NZDecDigit DecDigit*;

Add                     : '+';
Div                     : '/';
Equal                   : '=';
Exp                     : '^';
Lpar                    : '(';
Mul                     : '*';
Rpar                    : ')';
Sub                     : '-';

Comma                   : ',';
Semi                    : ';';

Star                    : 'star';
Plus                    : 'plus';
L_Star                  : 'l-star';
L_Plus                  : 'l-plus';
L_Rrec                  : 'l-rrec';
Rrec                    : 'rrec';
Lrec                    : 'lrec';
Amb                     : 'amb';

SComment                : BlockComment              -> skip;
SLineComment            : LineComment               -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;
Num                     : DecInteger;
