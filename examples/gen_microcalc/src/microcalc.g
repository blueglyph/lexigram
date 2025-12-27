grammar MicroCalc;

program:
    function+
;

function:
    Def Id Lpar fun_params Rpar instruction
;

fun_params:
    Id (Comma Id)*
|
;

block:
    Lbracket instruction* Rbracket
;

instruction:
    Let Id Equal expr Semi
|   Id Equal expr Semi
|   Return expr Semi
|   Print expr Semi
|   Id Lpar fun_args Rpar Semi
|   If expr instruction (<G> Else instruction)?
|   While expr block
|   block
;

expr:
    Sub expr
|   expr <R> Exp expr
|   expr (Mul | <P> Div) expr
|   expr (Add | <P> Sub) expr
|   Not expr
|   expr (Eq | <P> Ne | <P> Lt | <P> Gt | <P> Le | <P> Ge) expr
|   Lpar expr Rpar
|   Id Lpar fun_args Rpar
|   Id
|   Num
;

fun_args:
    expr (Comma expr)*
|
;
