grammar Config;

config:
    definitions
    lexer
    parser
    options
;

definitions:
    (<L=i_def> Def Id Equal value Semicolon)*   // any type
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
    io_option (<L=i_io_opt> Comma io_option)*
;

io_option:
    Combined Colon value tag_opt            // string
|   Input    Colon value tag_opt            // string
|   Output   Colon value tag_opt            // string
|   Indent   Colon value                    // num
|   Headers  Colon Lbracket value (Comma value)* Rbracket // string
;

tag_opt:
    (LSbracket value RSbracket)?            // string
;

global_options:
     global_option (<L=i_global_opt> Comma global_option)*
;

global_option:
    Headers Colon Lbracket value (Comma value)* Rbracket    // string
|   Indent  Colon value                                     // num
|   Libs    Colon Lbracket value (Comma value)* Rbracket    // string
|   NTValue Colon nt_value
|   Spans   Colon value                                     // bool
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
|   Set Lbracket Id (Comma Id)* Rbracket
;
