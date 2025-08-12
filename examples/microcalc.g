grammar MicroCalc;

program:
    function+
;

function:
    Def Id Lpar fun_params Rpar Lbracket
    instruction+
    Rbracket
;

fun_params:
//    Id (Comma Id)*
    (Id Comma)* Id
|
;

instruction:
    Let Id Equal expr Semi
|   Return expr Semi
|   Print expr Semi
;

expr:
    Sub expr
|   expr (Mul | <P> Div) expr
|   expr (Add | <P> Sub) expr
|   Lpar expr Rpar
|   Id Lpar fun_args Rpar
|   Id
|   Num
;

fun_args:
    expr (Comma expr)*
|
;
