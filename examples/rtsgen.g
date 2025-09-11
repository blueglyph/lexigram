grammar RtsGen;

ruleset:
    (<L=rule_iter> rule)+
;

rule:
    Nonterminal DArrow rts_expr Semicolon
|   Nonterminal Arrow prs_expr Semicolon
;

rts_expr:
    Concat rts_children
|   Or rts_children
|   Plus rts_children
|   Star rts_children
|   Question rts_children
|   item
;

rts_children:
    LPar rts_expr* RPar
;

prs_expr:
    prs_expr Plus
|   prs_expr Star
|   prs_expr Question
|   prs_expr prs_expr
|   prs_expr Or prs_expr
|   LPar prs_expr RPar
|   item
;

item:
    Nonterminal
|   Terminal
|   TerminalCst
|   Empty
|   LTag
|   PTag
|   RTag
;