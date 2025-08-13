// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(super) mod tests;

use iter_index::IndexerIterator;
use vectree::VecTree;
use crate::dfa::{ModeId, ReNode, TokenId};
use crate::{node, term};

// ---------------------------------------------------------------------------------------------
// Lexer

#[allow(dead_code)]
#[repr(u16)]
enum T {
    Arrow     = 0, // 0
    Colon    ,     // 1
    Comma    ,     // 2
    Dot      ,     // 3
    Ellipsis ,     // 4
    Lbracket ,     // 5
    Lparen   ,     // 6
    Negate   ,     // 7
    Minus    ,     // 8
    Plus     ,     // 9
    Or       ,     // 10
    Question ,     // 11
    Rbracket ,     // 12
    Rparen   ,     // 13
    Semicolon,     // 14
    Star     ,     // 15
    Channels ,     // 16
    Fragment ,     // 17
    Lexicon  ,     // 18
    Mode     ,     // 19
    Pop      ,     // 20
    Push     ,     // 21
    More     ,     // 22
    Skip     ,     // 23
    Type     ,     // 24
    Channel  ,     // 25
    SymEof   ,     // 26
    Id       ,     // 27
    CharLit  ,     // 28
    StrLit   ,     // 29
    FixedSet ,     // 30
    LSbracket,     // 31
    RSbracket,     // 32
    SetChar  ,     // 33
}

pub const TERMINALS: [(&str, Option<&str>); 34] = [
    ("Arrow",    Some("->")),       // 0
    ("Colon",    Some(":")),        // 1
    ("Comma",    Some(",")),        // 2
    ("Dot",      Some(".")),        // 3
    ("Ellipsis", Some("..")),       // 4
    ("Lbracket", Some("{")),        // 5
    ("Lparen",   Some("(")),        // 6
    ("Negate",   Some("~")),        // 7
    ("Minus",    Some("-")),        // 8
    ("Plus",     Some("+")),        // 9
    ("Or",       Some("|")),        // 10
    ("Question", Some("?")),        // 11
    ("Rbracket", Some("}")),        // 12
    ("Rparen",   Some(")")),        // 13
    ("Semicolon",Some(";")),        // 14
    ("Star",     Some("*")),        // 15
    ("Channels", Some("channels")), // 16
    ("Fragment", Some("fragment")), // 17
    ("Lexicon",  Some("lexicon")),  // 18
    ("Mode",     Some("mode")),     // 19
    ("Pop",      Some("pop")),      // 20
    ("Push",     Some("push")),     // 21
    ("More",     Some("more")),     // 22
    ("Skip",     Some("skip")),     // 23
    ("Type",     Some("type")),     // 24
    ("Channel",  Some("channel")),  // 25
    ("SymEof",   Some("EOF")),      // 26
    ("Id",       None),             // 27
    ("CharLit",  None),             // 28
    ("StrLit",   None),             // 29
    ("FixedSet", None),             // 30
    ("LSbracket",Some("[")),        // 31
    ("RSbracket",Some("]")),        // 32
    ("SetChar",  None),             // 33
];

pub fn build_re() -> Vec<(ModeId, VecTree<ReNode>)> {
    // MODE_DEFAULT:
    let mut re1 = VecTree::new();
    let top1 = re1.add_root(node!(|));

    // All symbols
    for (id, (_, text_op)) in TERMINALS.iter().index() {
        if let Some(text) = text_op {
            if *text != "[" && *text != "]" {
                re1.addc_iter(Some(top1), node!(&), [node!(str *text), node!(=id)]);
            }
        }
    }

    // Comment: '/' '*' .*? '*' '/'
    let comment = re1.add(Some(top1), node!(&));
    re1.add(Some(comment), node!(str "/*"));
    let l = re1.add(Some(comment), node!(??));
    re1.addc(Some(l), node!(*), node!([DOT]));
    re1.add_iter(Some(comment), [node!(str "*/"), node!(term!(skip))]);

    // LineComment: '//' ~[\r\n]*
    let line_comment = re1.add(Some(top1), node!(&));
    re1.add(Some(line_comment), node!(str "//"));
    re1.addc(Some(line_comment), node!(*), node!(~['\r', '\n']));
    re1.add(Some(line_comment), node!(term!(skip)));

    // Whitespace: [ \n\r\t]+
    let whitespace = re1.add(Some(top1), node!(&));
    re1.addc(Some(whitespace), node!(+), node!([' ', '\n', '\r', '\t']));
    re1.add(Some(whitespace), node!(term!(skip)));

    // Id: [a-zA-Z][a-zA-Z_0-9]*
    let id = re1.add(Some(top1), node!(&));
    re1.add(Some(id), node!(['a'-'z', 'A'-'Z']));
    re1.addc(Some(id), node!(*), node!(['_', '0'-'9', 'a'-'z', 'A'-'Z']));
    re1.add(Some(id), node!(=T::Id as TokenId));

    // Staggered form of Char (easier to decompose):
    //
    // CharLit: '\'' Char '\''
    // Char: (                                        |             )
    //        '\\' (         |                      )   ~[\n\r\t'\\]
    //              [nrt'\\]   'u{' [0-9a-fA-F]+ '}'
    //                      or3                p     or1
    //
    fn add_char(re: &mut VecTree<ReNode>, parent: usize) {
        let or1 = re.add(Some(parent), node!(|));
            let cc2 = re.add(Some(or1), node!(&));
                re.add(Some(cc2), node!(chr '\\'));
                let or3 = re.add(Some(cc2), node!(|));
                    re.add(Some(or3), node!(['n', 'r', 't', '\'', '\\']));
                    let cc4 = re.add(Some(or3), node!(&));
                        re.add(Some(cc4), node!(str "u{"));
                        re.addc(Some(cc4), node!(+), node!(['0'-'9', 'a'-'f', 'A'-'F']));
                        re.add(Some(cc4), node!(chr '}'));
            re.add(Some(or1), node!(~['\n', '\r', '\t', '\'', '\\']));
    }
    let char_lit = re1.add(Some(top1), node!(&));
    re1.add(Some(char_lit), node!(chr '\''));
    add_char(&mut re1, char_lit);
    re1.add_iter(Some(char_lit), [node!(chr '\''), node!(=T::CharLit as TokenId)]);

    // StrLit: '\'' Char Char+ '\''
    let str_lit = re1.add(Some(top1), node!(&));
    re1.add(Some(str_lit), node!(chr '\''));
    add_char(&mut re1, str_lit);
    let p = re1.add(Some(str_lit), node!(+));
    add_char(&mut re1, p);
    re1.add_iter(Some(str_lit), [node!(chr '\''), node!(=T::StrLit as TokenId)]);

    // FixedSet: ('\\w' | '\\d')
    let fixed_set = re1.add(Some(top1), node!(&));
        let or1 = re1.add(Some(fixed_set), node!(|));
            re1.add_iter(Some(or1), [node!(str "\\w"), node!(str "\\d")]);
    re1.add(Some(fixed_set), node!(=T::FixedSet as TokenId));

    // '[' -> push(MODE_SET_CHAR)
    re1.addc_iter(Some(top1), node!(&), [node!(chr '['), node!(term!(=T::LSbracket as TokenId) + term!(push 1))]);

    // MODE_SET_CHAR:
    let mut re2 = VecTree::new();
    let top2 = re2.add_root(node!(|));

    // ']' -> pop
    re2.addc_iter(Some(top2), node!(&), [node!(chr ']'), node!(term!(=T::RSbracket as TokenId) + term!(pop))]);

    // Minus: '-'
    re2.addc_iter(Some(top2), node!(&), [node!(chr '-'), node!(term!(=T::Minus as TokenId))]);

    // SetChar: ( '\\' ([nrt\\[\]\-] | 'u{' [0-9a-fA-F]+ '}') | ~[\n\r\t\\\]\-] )
    let set_char = re2.add(Some(top2), node!(&));
        let or1 = re2.add(Some(set_char), node!(|));
            let cc2 = re2.add(Some(or1), node!(&));
                re2.add(Some(cc2), node!(chr '\\'));
                let or3 = re2.add(Some(cc2), node!(|));
                    re2.add(Some(or3), node!(['n', 'r', 't', '\'', '\\', '[', ']', '-']));
                    let cc4 = re2.add(Some(or3), node!(&));
                        re2.add(Some(cc4), node!(str "u{"));
                        re2.addc(Some(cc4), node!(+), node!(['0'-'9', 'a'-'f', 'A'-'F']));
                        re2.add(Some(cc4), node!(chr '}'));
            // Note: we accept the opening bracket within brackets, with or without escaping: [a-z[] or [a-z\[]
            re2.add(Some(or1), node!(~['\n', '\r', '\t', '\\', ']', '-']));
    re2.add(Some(set_char), node!(=T::SetChar as TokenId));

    // FixedSet: ('\\w' | '\\d')
    let fixed_set = re2.add(Some(top1), node!(&));
        let or1 = re2.add(Some(fixed_set), node!(|));
            re2.add_iter(Some(or1), [node!(str "\\w"), node!(str "\\d")]);
    re2.add(Some(fixed_set), node!(=T::FixedSet as TokenId));

    vec![(0, re1), (1, re2)]
}

/// The lexicon of the lexer's regular expression implemented manually above.
/// It's also used in the tests to see if it can scan itself (hence the dummy lines).
/// The expected results are defined in `LEXICON_TOKENS` and `LEXICON_TEXT` below.
pub const LEXICON: &str = r#"
lexicon LexiLexer;

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;
fragment HexDigit       : [0-9a-fA-F];
fragment UnicodeEsc     : 'u{' HexDigit+ '}';
fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
fragment Char           : EscChar | ~[\n\r\t'\\];
fragment CharLiteral    : '\'' Char '\'';
fragment StrLiteral     : '\'' Char Char+ '\'';

// Char inside a '[' ']' set
fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);

Arrow           : '->'; /* // first token // */
Colon           : ':';
Comma           : ',';
Dot             : '.';
Ellipsis        : '..';
Lbracket        : '{';
Lparen          : '(';
Negate          : '~';
Minus           : '-';
Plus            : '+';
Or              : '|';
Question        : '?';
Rbracket        : '}';
Rparen          : ')';
Semicolon       : ';';
Star            : '*';

Channels        : 'channels';
Fragment        : 'fragment';
Lexicon         : 'lexicon';
Mode            : 'mode';
Pop             : 'pop';
Push            : 'push';
More            : 'more';
Skip            : 'skip';
Type            : 'type';
Channel         : 'channel';
SymEof          : 'EOF';

SComment        : BlockComment              -> skip;
SLineComment    : LineComment               -> skip;
SWhiteSpace     : [ \n\r\t]+                -> skip;

Id              : [a-zA-Z][a-zA-Z_0-9]*;

CharLit         : CharLiteral;
StrLit          : StrLiteral;

FixedSet        : ('\\w' | '\\d');

LSbracket       : '['                       -> push(MODE_SET_CHAR);

mode MODE_SET_CHAR;

RSbracket       : ']'                       -> pop;
Minus2          : '-'                       -> type(Minus);
SetChar         : (EscSetChar | ~[\n\r\t\\\]\-]);
FixedSet2       : ('\\w' | '\\d')           -> type(FixedSet);
"#;

// ---------------------------------------------------------------------------------------------
// Parser

pub const GRAMMAR: &str = r#"
grammar LexiParser;

file: header? file_item*;

file_item:
    option | declaration | rule
;

header:
    LEXICON ID SEMICOLON
;

declaration:
    MODE ID SEMICOLON
;

option:
    CHANNELS LBRACKET ID (COMMA ID)* RBRACKET
;

rule:
    FRAGMENT ID COLON match SEMICOLON
|   ID COLON match (ARROW actions)? SEMICOLON
;

actions:
    action (COMMA action)*
;

action:
    MODE LPAREN ID RPAREN
|   PUSH LPAREN ID RPAREN
|   POP
|   SKiP
|   MORE
|   TYPE LPAREN ID RPAREN
|   CHANNEL LPAREN ID RPAREN
;

match:
    alt_items
;

alt_items:
    alt_items OR alt_item
|   alt_item
;

alt_item:
    repeat_item+
;

repeat_item:
    item STAR QUESTION?
|   item PLUS QUESTION?
|   item QUESTION?
;

item:
    ID
|   CHAR_LIT (ELLIPSIS CHAR_LIT)?
|   STR_LIT
|   char_set
|   LPAREN alt_items RPAREN
|   NEGATE item
;

char_set:
    LSBRACKET (char_set_one)+ RSBRACKET
|   DOT
|   FIXED_SET;
;

char_set_one:
    SET_CHAR MINUS SET_CHAR | SET_CHAR | FIXED_SET;

"#;

