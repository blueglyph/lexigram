grammar Test1;

program:
    (<L=inst> instruction)*
;

instruction:
    Let Id Equal expr Semi
|   Print expr Semi
;

expr:
    Sub expr
|   expr (Mul | <P> Div) expr
|   expr (Add | <P> Sub) expr
|   expr (Shl | <P> Shr) expr
|   Lpar expr Rpar
|   Id
|   Num
;
