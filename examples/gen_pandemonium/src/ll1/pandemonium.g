grammar Pandemonium;

text: (<L=i> example)* Semi (<L=nv_i> nv_example)*;

example:
    Star star
|   Plus plus
|   L_Star l_star
|   L_Plus l_plus
|   Rrec rrec
|   L_Rrec l_rrec
|   Lrec lrec
|   Amb amb
|   Star_A star_a
|   Plus_A plus_a
|   L_Star_A l_star_a
|   L_Plus_A l_plus_a
|   SepList sep_list
|   SepList_Opt sep_list_opt
|   L_SepList l_sep_list
|   L_SepList_Opt l_sep_list_opt
;

// with value ------------------------------------------------------------------

star:   Id Equal Id (Comma Num)* Semi;
plus:   Id Equal Num (Comma Num)+ Semi;
l_star: Id Equal Num (<L=l_star_i> Comma Num)* Semi;
l_plus: Id Equal Num (<L=l_plus_i> Comma Num)+ Semi;
rrec:   Id Equal Num rrec_i;
l_rrec: Id Equal Num l_rrec_i;
lrec:   Id Equal lrec_i Semi;
amb:    Id Equal amb_i Semi;

star_a:   Id Equal Lsbracket (Id | Num Colon Id)* Rsbracket Semi;
plus_a:   Id Equal Lsbracket (Id | Num Colon Id)+ Rsbracket Semi;
l_star_a: Id Equal Lsbracket (<L=l_star_a_i> Id | Num Colon Id)* Rsbracket Semi;
l_plus_a: Id Equal Lsbracket (<L=l_plus_a_i> Id | Num Colon Id)+ Rsbracket Semi;

sep_list:       Id Equal (Id Colon Num / Comma Then)+ Semi;
sep_list_opt:   Id Equal ((Id Colon Num / Comma Then)+)? Semi;
l_sep_list:     Id Equal (<L=l_sep_list_i>      Id Colon Num / Comma Then)+ Semi;
l_sep_list_opt: Id Equal ((<L=l_sep_list_opt_i> Id Colon Num / Comma Then)+)? Semi;

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
    Sub amb_i
|   <R> amb_i Exp amb_i
|   amb_i (Mul | <P> Div) amb_i
|   amb_i (Add | <P> Sub) amb_i
|   Lpar amb_i Rpar
|   Id
|   Num
;

// without value ---------------------------------------------------------------

nv_example:
    Star nv_star
|   Plus nv_plus
|   L_Star nv_l_star
|   L_Plus nv_l_plus
|   Rrec nv_rrec
|   L_Rrec nv_l_rrec
|   Lrec nv_lrec
|   Star_A nv_star_a
|   Plus_A nv_plus_a
|   L_Star_A nv_l_star_a
|   L_Plus_A nv_l_plus_a
|   SepList nv_sep_list
|   SepList_Opt nv_sep_list_opt
|   L_SepList nv_l_sep_list
|   L_SepList_Opt nv_l_sep_list_opt
;

nv_star:   Id Equal Add (Comma Mul)* Semi;
nv_plus:   Id Equal Add (Comma Mul)+ Semi;
nv_l_star: Id Equal Add (<L=nv_l_star_i> Comma Mul)* Semi;
nv_l_plus: Id Equal Add (<L=nv_l_plus_i> Comma Mul)+ Semi;
nv_rrec:   Id Equal Add nv_rrec_i;
nv_l_rrec: Id Equal Add nv_l_rrec_i;
nv_lrec:   Id Equal nv_lrec_i Semi;

nv_star_a:   Id Equal Lsbracket (Add | Mul Sub)* Rsbracket Semi;
nv_plus_a:   Id Equal Lsbracket (Add | Mul Sub)+ Rsbracket Semi;
nv_l_star_a: Id Equal Lsbracket (<L=nv_l_star_a_i> Add | Mul Sub)* Rsbracket Semi;
nv_l_plus_a: Id Equal Lsbracket (<L=nv_l_plus_a_i> Add | Mul Sub)+ Rsbracket Semi;

nv_sep_list:       Id Equal (Mul / Comma Then)+ Semi;
nv_sep_list_opt:   Id Equal ((Mul / Comma Then)+)? Semi;
nv_l_sep_list:     Id Equal (<L=nv_l_sep_list_i>      Mul / Comma Then)+ Semi;
nv_l_sep_list_opt: Id Equal ((<L=nv_l_sep_list_opt_i> Mul / Comma Then)+)? Semi;

nv_rrec_i:
    Comma Mul nv_rrec_i
|   Semi
;

nv_l_rrec_i:
    <L> Comma Mul nv_l_rrec_i
|   Semi
;

nv_lrec_i:
    nv_lrec_i Comma Mul
|   Add
;
