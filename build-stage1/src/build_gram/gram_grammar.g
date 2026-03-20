grammar GramParser;

file: header rules;

header:
    Grammar Id Semicolon
;

rules:
    rule
|   rules rule
;

rule:
    rule_name Colon prod SymEof? Semicolon
;

rule_name:
    Id
;

prod:
    prod_alt
|   prod Or prod_alt
;

prod_alt:
    prod_factor*
;

prod_factor:
    prod_atom (Plus | Star | Question)?
;

prod_atom:
    Id
|   Lform
|   Rform
|   Pform
|   Greedy
|   Lparen prod Rparen
;
