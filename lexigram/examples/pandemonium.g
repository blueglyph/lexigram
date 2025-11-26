grammar Pandemonium;

text: (<L=i> example)*;

example:
    Star star
|   Plus plus
|   L_Star l_star
|   L_Plus l_plus
|   Rrec rrec
|   L_Rrec l_rrec
|   Lrec lrec
|   Amb amb
;

star:   Id Equal Num (Comma Num)* Semi;
plus:   Id Equal Num (Comma Num)+ Semi;
l_star: Id Equal Num (<L=l_star_i> Comma Num)* Semi;
l_plus: Id Equal Num (<L=l_plus_i> Comma Num)+ Semi;
rrec:   Id Equal Num rrec_i;
l_rrec: Id Equal Num l_rrec_i;
lrec:   Id Equal lrec_i Semi;
amb:    Id Equal amb_i Semi;

rrec_i:
    Comma Num rrec_i
|   Semi
;

l_rrec_i:
    <L> Comma Num l_rrec_i
|   Semi
;

lrec_i:
    lrec_i Comma Num
|   Num
;

amb_i:
    <R> amb_i Exp amb_i
|   amb_i (Mul | <P> Div) amb_i
|   amb_i (Add | <P> Sub) amb_i
|   Sub amb_i
|   Lpar amb_i Rpar
|   Id
|   Num
;
