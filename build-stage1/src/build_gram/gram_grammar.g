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
    prod_term
|   prod Or prod_term
;

prod_term:
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
|   Lparen prod Rparen
;
