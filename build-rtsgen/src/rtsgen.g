grammar RtsGen;

ruleset:
    (<L=rule_iter> rule)+
;

rule:
    rule_nt DArrow rts_expr Semicolon
|   rule_nt Arrow prs_expr Semicolon
;

rule_nt:
    Nonterminal
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
|   NTx
|   Terminal
|   TerminalCst
|   Tx
|   Empty
|   LTag
|   PTag
|   RTag
;