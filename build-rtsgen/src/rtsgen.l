lexicon RtsGen;

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;
fragment HexDigit       : [0-9a-fA-F];
fragment UnicodeEsc     : 'u{' HexDigit+ '}';
fragment EscChar        : '\\' ([nrt"\\] | UnicodeEsc);
fragment Char           : EscChar | ~[\n\r\t"\\];
fragment StrLiteral     : '"' Char+ '"';
fragment NonterminalId  : [a-z][a-z0-9_]*;
fragment TerminalId     : [A-Z][a-zA-Z0-9_]*;
fragment Num            : [1-9][0-9]*;

SComment                : BlockComment              -> skip;
SLineComment            : LineComment               -> skip;
SWhiteSpace             : [ \n\r\t]+                -> skip;

Arrow                   : '->';
DArrow                  : '=>';

Concat                  : '&';
Or                      : '|';
Plus                    : '+';
Star                    : '*';
Question                : '?';

Empty                   : 'ε' | '€' | '{}';

Equal                   : '=';
Comma                   : ',';
LPar                    : '(';
RPar                    : ')';
Semicolon               : ';';

LTag                    : '<L' ('=' NonterminalId)? '>';
PTag                    : '<P>';
RTag                    : '<R>';
GTag                    : '<G>';

Token                   : 'token';

TerminalCst             : StrLiteral;
Terminal                : TerminalId;
Nonterminal             : NonterminalId;
Tx                      : 'T(' Num ')';
NTx                     : 'NT(' Num ')';
