lexicon Pandemonium;

fragment BLOCK_COMMENT  : '/*' .*? '*/';
fragment LINE_COMMENT   : '//' ~[\r\n]*;
fragment DEC_DIGIT      : [0-9];
fragment NZ_DEC_DIGIT   : [1-9];
fragment DEC_INTEGER    : NZ_DEC_DIGIT DEC_DIGIT*;

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
L_SepList               : 'l-sep-list';
L_SepList_Opt           : 'l-sep-list-opt';

SComment                : BLOCK_COMMENT              -> skip;
SLineComment            : LINE_COMMENT               -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Id                      : [a-zA-Z][a-zA-Z_0-9]*;
Num                     : DEC_INTEGER;
