grammar LexiParser;

file: header? file_item*;

file_item:
    option | declaration | rule
;

header:
    Lexicon Id Semicolon
;

declaration:
    Mode Id Semicolon
;

option:
    Channels Lbracket Id (Comma Id)* Rbracket
;

rule:
    rule_fragment_name Colon match Semicolon
|   rule_terminal_name Colon match (Arrow actions)? Semicolon
    // only reserve the token, but doesn't add a rule to scan it:
|   Lparen rule_terminal_name Rparen opt_str_lit (Arrow Hook)? Semicolon
;

opt_str_lit:
    (Colon StrLit)?
;

rule_fragment_name:
    Fragment Id
;

rule_terminal_name:
    Id
;

actions:
    action (Comma action)*
;

action:
    Mode Lparen Id Rparen
|   Push Lparen Id Rparen
|   Pop
|   Skip
|   More
|   Type Lparen Id Rparen
|   Channel Lparen Id Rparen
|   Hook
;

match:
    alt_items
;

alt_items:
    alt_item (Or alt_item)*
;

alt_item:
	repeat_item+
;

repeat_item:
    item Star Question?
|   item Plus Question?
|   item Question?
;

item:
    Id
|   CharLit (Ellipsis CharLit)?
|   StrLit
|   char_set
|   Lparen alt_items Rparen
|   Negate item
;

char_set:
    LSbracket (char_set_one)+ RSbracket
|   Dot
|   FixedSet
;

char_set_one:
    SetChar Minus SetChar
|   SetChar
|   FixedSet
;
