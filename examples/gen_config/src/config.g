grammar Config;

config:
    definitions
    lexer
    parser
    options
;

definitions:
    (<L=i_def> Def Id Equal value Semicolon)*
;

lexer:
    Lexer Lbracket io_options Rbracket
;

parser:
    (Parser Lbracket io_options Rbracket)?
;

options:
    (Options Lbracket global_options Rbracket)?
;

io_options:
    (<L=i_io_opt> io_option / Comma)+
;

io_option:
    Combined Colon value tag_opt
|   Input    Colon value tag_opt
|   Output   Colon value tag_opt
|   Indent   Colon value
|   Headers  Colon Lbracket (value / Comma)+ Rbracket // string
;

tag_opt:
    (LSbracket value RSbracket)?
;

global_options:
     (<L=i_global_opt> global_option / Comma)+
;

global_option:
    Headers Colon Lbracket (value / Comma)+ Rbracket
|   Indent  Colon value
|   Libs    Colon Lbracket (value / Comma)+ Rbracket
|   NTValue Colon nt_value
|   Spans   Colon value
;

value:
    BoolLiteral
|   NumLiteral
|   StrLiteral
|   Id
|   Stdout
;

nt_value:
    Default
|   None
|   Parents
|   Set Lbracket (value / Comma)+ Rbracket
;
