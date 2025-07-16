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
    prod_factor
|   prod Or prod_factor
;

prod_factor:
    prod_term*
;

prod_term:
    term_item (Plus | Star | Question)?
;

term_item:
    Id
|   Lform
|   Rform
|   Lparen prod Rparen
;
