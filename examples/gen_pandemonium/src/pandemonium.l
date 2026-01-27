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
Lsbracket               : '[';
Mul                     : '*';
Rpar                    : ')';
Rsbracket               : ']';
Sub                     : '-';

Colon                   : ':';
Comma                   : ',';
Semi                    : ';';

Then                    : 'then';

Star                    : 'star';
Plus                    : 'plus';
L_Star                  : 'l-star';
L_Plus                  : 'l-plus';
L_Rrec                  : 'l-rrec';
Rrec                    : 'rrec';
Lrec                    : 'lrec';
Amb                     : 'amb';
Star_A                  : 'star-a';
Plus_A                  : 'plus-a';
L_Star_A                : 'l-star-a';
L_Plus_A                : 'l-plus-a';
SepList                 : 'sep-list';
SepList_Opt             : 'sep-list-opt';

SComment                : BlockComment              -> skip;
SLineComment            : LineComment               -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;
Num                     : DecInteger;
