// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![allow(unused)]

pub(super) mod tests;

use iter_index::IndexerIterator;
use vectree::VecTree;
use crate::dfa::{ModeId, ReNode, ReType, TokenId};
use crate::segments::{Seg, Segments};
use crate::{gnode, node, term, General};
use crate::grammar::{RuleTreeSet, VarId, GrNode, Symbol};
use crate::symbol_table::SymbolTable;

// ---------------------------------------------------------------------------------------------
// Lexer

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

// TODO: We must make a few constants public because they're used in the integration tests.
//       Using #cfg(test) to guard the public declaration doesn't seem to work as expected
//       (fails to compile where it's also used with the same guard). Other solution?
//       We could write them into the generated source file as a last resort.

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


pub const LEXICON_TOKENS: [TokenId; 338] = [
    18, 27, 14,                                                     // lexicon LexiLexer;
    17, 27, 1, 29, 3, 15, 11, 29, 14,                               // fragment BlockComment   : '/*' .*? '*/';
    17, 27, 1, 29, 7, 31, 33, 33, 32, 15, 14,                       // fragment LineComment    : '//' ~[\r\n]*;
    17, 27, 1, 31, 33, 8, 33, 33, 8, 33, 33, 8, 33, 32, 14,         // fragment HexDigit       : [0-9a-fA-F];
    17, 27, 1, 29, 27, 9, 28, 14,                                   // fragment UnicodeEsc     : 'u{' HexDigit+ '}';
    17, 27, 1, 28, 6, 31, 33, 33, 33, 33, 33, 32, 10, 27, 13, 14,   // fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
    17, 27, 1, 27, 10, 7, 31, 33, 33, 33, 33, 33, 32, 14,           // fragment Char           : EscChar | ~[\n\r\t'\\];
    17, 27, 1, 28, 27, 28, 14,                                      // fragment CharLiteral    : '\'' Char '\'';
    17, 27, 1, 28, 27, 27, 9, 28, 14,                               // fragment StrLiteral     : '\'' Char Char+ '\'';
    17, 27, 1, 28, 6, 31, 33, 33, 33, 33, 33, 33, 33, 32, 10, 27,   // fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);
               13, 14,                                              //
    27, 1, 29, 14,                                                  // Arrow           : '->'; /* // first token // */
    27, 1, 28, 14,                                                  // Colon           : ':';
    27, 1, 28, 14,                                                  // Comma           : ',';
    27, 1, 28, 14,                                                  // Dot             : '.';
    27, 1, 29, 14,                                                  // Ellipsis        : '..';
    27, 1, 28, 14,                                                  // Lbracket        : '{';
    27, 1, 28, 14,                                                  // Lparen          : '(';
    27, 1, 28, 14,                                                  // Negate          : '~';
    27, 1, 28, 14,                                                  // Minus           : '-';
    27, 1, 28, 14,                                                  // Plus            : '+';
    27, 1, 28, 14,                                                  // Or              : '|';
    27, 1, 28, 14,                                                  // Question        : '?';
    27, 1, 28, 14,                                                  // Rbracket        : '}';
    27, 1, 28, 14,                                                  // Rparen          : ')';
    27, 1, 28, 14,                                                  // Semicolon       : ';';
    27, 1, 28, 14,                                                  // Star            : '*';
    27, 1, 29, 14,                                                  // Channels        : 'channels';
    27, 1, 29, 14,                                                  // Fragment        : 'fragment';
    27, 1, 29, 14,                                                  // Lexicon         : 'lexicon';
    27, 1, 29, 14,                                                  // Mode            : 'mode';
    27, 1, 29, 14,                                                  // Pop             : 'pop';
    27, 1, 29, 14,                                                  // Push            : 'push';
    27, 1, 29, 14,                                                  // More            : 'more';
    27, 1, 29, 14,                                                  // Skip            : 'skip';
    27, 1, 29, 14,                                                  // Type            : 'type';
    27, 1, 29, 14,                                                  // Channel         : 'channel';
    27, 1, 29, 14,                                                  // SymEof          : 'EOF';
    27, 1, 27, 0, 23, 14,                                           // SComment        : BlockComment              -> skip;
    27, 1, 27, 0, 23, 14,                                           // SLineComment    : LineComment               -> skip;
    27, 1, 31, 33, 33, 33, 33, 32, 9, 0, 23, 14,                    // SWhiteSpace     : [ \n\r\t]+                -> skip;
    27, 1, 31, 33, 8, 33, 33, 8, 33, 32, 31, 33, 8, 33, 33, 8,      // Id              : [a-zA-Z][a-zA-Z_0-9]*;
               33, 33, 33, 8, 33, 32, 15, 14,                       //
    27, 1, 27, 14,                                                  // CharLit         : CharLiteral;
    27, 1, 27, 14,                                                  // StrLit          : StrLiteral;
    27, 1, 6, 29, 10, 29, 13, 14,                                   // FixedSet        : ('\\w' | '\\d');
    27, 1, 28, 0, 21, 6, 27, 13, 14,                                // LSbracket       : '['                       -> push(MODE_SET_CHAR);

    19, 27, 14,                                                     // mode MODE_SET_CHAR;
    27, 1, 28, 0, 20, 14,                                           // RSbracket       : ']'                       -> pop;
    27, 1, 28, 0, 24, 6, 27, 13, 14,                                // Minus2          : '-'                       -> type(Minus);
    27, 1, 6, 27, 10, 7, 31, 33, 33, 33, 33, 33, 33, 32, 13, 14,    // SetChar         : (EscSetChar | ~[\n\r\t\\\]\-]);
    27, 1, 6, 29, 10, 29, 13, 0, 24, 6, 27, 13, 14                  // FixedSet2       : ('\\w' | '\\d')           -> type(FixedSet);
];

pub const LEXICON_TEXT: [&str; 338] = [
    "lexicon", "LexiLexer", ";", "fragment", "BlockComment", ":", "'/*'", ".", "*", "?", "'*/'", ";", 
    "fragment", "LineComment", ":", "'//'", "~", "[", "\\r", "\\n", "]", "*", ";",
    "fragment", "HexDigit", ":", "[", "0", "-", "9", "a", "-", "f", "A", "-", "F", "]", ";", "fragment", "UnicodeEsc", ":", "'u{'", "HexDigit", "+", "'}'", ";",
    "fragment", "EscChar", ":", "'\\\\'", "(", "[", "n", "r", "t", "'", "\\\\", "]", "|", "UnicodeEsc", ")", ";",
    "fragment", "Char", ":", "EscChar", "|", "~", "[", "\\n", "\\r", "\\t", "'", "\\\\", "]", ";", "fragment", "CharLiteral", ":", "'\\''", "Char", "'\\''", ";",
    "fragment", "StrLiteral", ":", "'\\''", "Char", "Char", "+", "'\\''", ";",
    "fragment", "EscSetChar", ":", "'\\\\'", "(", "[", "n", "r", "t", "\\\\", "[", "\\]", "\\-", "]", "|", "UnicodeEsc", ")", ";", "Arrow", ":", "'->'", ";",
    "Colon", ":", "':'", ";", "Comma", ":", "','", ";", "Dot", ":", "'.'", ";", "Ellipsis", ":", "'..'", ";", "Lbracket", ":", "'{'", ";", "Lparen", ":", "'('", ";",
    "Negate", ":", "'~'", ";", "Minus", ":", "'-'", ";", "Plus", ":", "'+'", ";", "Or", ":", "'|'", ";", "Question", ":", "'?'", ";", "Rbracket", ":", "'}'", ";",
    "Rparen", ":", "')'", ";", "Semicolon", ":", "';'", ";", "Star", ":", "'*'", ";", "Channels", ":", "'channels'", ";", "Fragment", ":", "'fragment'", ";", "Lexicon",
    ":", "'lexicon'", ";", "Mode", ":", "'mode'", ";", "Pop", ":", "'pop'", ";", "Push", ":", "'push'", ";", "More", ":", "'more'", ";", "Skip", ":", "'skip'", ";",
    "Type", ":", "'type'", ";", "Channel", ":", "'channel'", ";", "SymEof", ":", "'EOF'", ";", "SComment", ":", "BlockComment", "->", "skip", ";", "SLineComment", ":",
    "LineComment", "->", "skip", ";", "SWhiteSpace", ":", "[", " ", "\\n", "\\r", "\\t", "]", "+", "->", "skip", ";", "Id", ":", "[", "a", "-", "z", "A", "-", "Z", "]",
    "[", "a", "-", "z", "A", "-", "Z", "_", "0", "-", "9", "]", "*", ";", "CharLit", ":", "CharLiteral", ";", "StrLit", ":", "StrLiteral", ";", "FixedSet", ":", "(",
    "'\\\\w'", "|", "'\\\\d'", ")", ";", "LSbracket", ":", "'['", "->", "push", "(", "MODE_SET_CHAR", ")", ";", "mode", "MODE_SET_CHAR", ";", "RSbracket", ":", "']'",
    "->", "pop", ";", "Minus2", ":", "'-'", "->", "type", "(", "Minus", ")", ";", "SetChar", ":", "(", "EscSetChar", "|", "~", "[", "\\n", "\\r", "\\t", "\\\\", "\\]",
    "\\-", "]", ")", ";", "FixedSet2", ":", "(", "'\\\\w'", "|", "'\\\\d'", ")", "->", "type", "(", "FixedSet", ")", ";"];

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

#[repr(u16)]
enum NT {
    File = 0,           // 0
    FileItem,           // 1
    Header,             // 2
    Declaration,        // 3
    Option,             // 4
    Rule,               // 5
    Actions,            // 6
    Action,             // 7
    Match,              // 8
    AltItems,           // 9
    AltItem,            // 10
    RepeatItem,         // 11
    Item,               // 12
    CharSet,            // 13
    CharSetOne,         // 14
}

const NON_TERMINALS: [&str; 15] = [
    "file",             // 0
    "file_item",        // 1
    "header",           // 2
    "declaration",      // 3
    "option",           // 4
    "rule",             // 5
    "actions",          // 6
    "action",           // 7
    "match",            // 8
    "alt_items",        // 9
    "alt_item",         // 10
    "repeat_item",      // 11
    "item",             // 12
    "char_set",         // 13
    "char_set_one",     // 14
];

pub(crate) fn build_rts() -> RuleTreeSet<General> {
    let mut symbol_table = SymbolTable::new();
    symbol_table.extend_nonterminals(NON_TERMINALS);
    symbol_table.extend_terminals(TERMINALS);
    let mut rules = RuleTreeSet::new();
    rules.set_symbol_table(symbol_table);

    // grammar LexiParser;
    //
    // file: header? file_item* EOF;
    //
    let tree = rules.get_tree_mut(NT::File as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.addc(Some(cc), gnode!(?), gnode!(nt NT::Header));
    tree.addc(Some(cc), gnode!(*), gnode!(nt NT::FileItem));

    // file_item:
    //     option | declaration | rule
    // ;
    //
    let tree = rules.get_tree_mut(NT::FileItem as VarId);
    let or = tree.add_root(gnode!(|));
    tree.add_iter(Some(or), [gnode!(nt NT::Option), gnode!(nt NT::Declaration), gnode!(nt NT::Rule)]);

    // header:
    //     LEXICON ID SEMICOLON
    // ;
    //
    let tree = rules.get_tree_mut(NT::Header as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Lexicon), gnode!(t T::Id), gnode!(t T::Semicolon)]);

    // declaration:
    //     MODE ID SEMICOLON
    // ;
    //
    let tree = rules.get_tree_mut(NT::Declaration as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Mode), gnode!(t T::Id), gnode!(t T::Semicolon)]);

    // option:
    //     CHANNELS LBRACKET ID (COMMA ID)* RBRACKET
    // ;
    //
    let tree = rules.get_tree_mut(NT::Option as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add_iter(Some(cc), [gnode!(t T::Channels), gnode!(t T::Lbracket), gnode!(t T::Id)]);
    let star1 = tree.add(Some(cc), gnode!(*));
    tree.addc_iter(Some(star1), gnode!(&), [gnode!(t T::Comma), gnode!(t T::Id)]);
    tree.add(Some(cc), gnode!(t T::Rbracket));

    // rule:
    //     FRAGMENT ID COLON match SEMICOLON
    // |   ID COLON match (ARROW actions)? SEMICOLON
    // ;
    //
    let tree = rules.get_tree_mut(NT::Rule as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Fragment), gnode!(t T::Id), gnode!(t T::Colon), gnode!(nt NT::Match), gnode!(t T::Semicolon)]);
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Id), gnode!(t T::Colon), gnode!(nt NT::Match)]);
    let maybe2 = tree.add(Some(cc1), gnode!(?));
    tree.addc_iter(Some(maybe2), gnode!(&), [gnode!(t T::Arrow), gnode!(nt NT::Actions)]);
    tree.add(Some(cc1), gnode!(t T::Semicolon));

    // actions:
    //     action (COMMA action)*
    // ;
    //
    let tree = rules.get_tree_mut(NT::Actions as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add(Some(cc), gnode!(nt NT::Action));
    let star1 = tree.add(Some(cc), gnode!(*));
    tree.addc_iter(Some(star1), gnode!(&), [gnode!(t T::Comma), gnode!(nt NT::Action)]);

    // action:
    //     MODE LPAREN ID RPAREN
    // |   PUSH LPAREN ID RPAREN
    // |   POP
    // |   SKiP
    // |   MORE
    // |   TYPE LPAREN ID RPAREN
    // |   CHANNEL LPAREN ID RPAREN
    // ;
    //
    let tree = rules.get_tree_mut(NT::Action as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Mode), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Push), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);
    tree.add_iter(Some(or), [gnode!(t T::Pop), gnode!(t T::Skip), gnode!(t T::More)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Type), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::Channel), gnode!(t T::Lparen), gnode!(t T::Id), gnode!(t T::Rparen)]);

    // match:
    //     alt_items
    // ;
    //
    let tree = rules.get_tree_mut(NT::Match as VarId);
    tree.add_root(gnode!(nt NT::AltItems));

    // alt_items:
    //     alt_item (OR alt_item)*
    // ;
    let tree = rules.get_tree_mut(NT::AltItems as VarId);
    let cc = tree.add_root(gnode!(&));
    tree.add(Some(cc), gnode!(nt NT::AltItem));
    let star = tree.add(Some(cc), gnode!(*));
    tree.addc_iter(Some(star), gnode!(&), [gnode!(t T::Or), gnode!(nt NT::AltItem)]);

    // alt_item:
    // 	repeat_item+
    // ;
    //
    let tree = rules.get_tree_mut(NT::AltItem as VarId);
    let plus = tree.add_root(gnode!(+));
    tree.add(Some(plus), gnode!(nt NT::RepeatItem));

    // repeat_item:
    //     item STAR QUESTION?
    // |   item PLUS QUESTION?
    // |   item QUESTION?
    // ;
    let tree = rules.get_tree_mut(NT::RepeatItem as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item), gnode!(t T::Star)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item), gnode!(t T::Plus)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));

    //
    // item:
    //     ID
    // |   CHAR_LIT (ELLIPSIS CHAR_LIT)?
    // |   STR_LIT
    // |   char_set
    // |   LPAREN alt_item RPAREN
    // |   NEGATE item
    // ;
    let tree = rules.get_tree_mut(NT::Item as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1s = tree.add_iter(Some(or), [
        gnode!(t T::Id),        // 0: ID
        gnode!(&),              // 2: CHAR_LIT (ELLIPSIS CHAR_LIT)?
        gnode!(t T::StrLit),    // 3: STR_LIT
        gnode!(nt NT::CharSet), // 4: char_set
        gnode!(&),              // 5: LPAREN alt_items RPAREN
        gnode!(&),              // 6: NEGATE item
    ]);
    tree.add(Some(cc1s[1]), gnode!(t T::CharLit));
    let maybe2 = tree.add(Some(cc1s[1]), gnode!(?));
    tree.addc_iter(Some(maybe2), gnode!(&), [gnode!(t T::Ellipsis), gnode!(t T::CharLit)]);
    tree.add_iter(Some(cc1s[4]), [gnode!(t T::Lparen), gnode!(nt NT::AltItems), gnode!(t T::Rparen)]);
    tree.add_iter(Some(cc1s[5]), [gnode!(t T::Negate), gnode!(nt NT::Item)]);

    // char_set:
    //     LSBRACKET (char_set_one)+ RSBRACKET
    // |   DOT
    // |   FIXED_SET;
    let tree = rules.get_tree_mut(NT::CharSet as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1 = tree.addc(Some(or), gnode!(&), gnode!(t T::LSbracket));
    tree.addc(Some(cc1), gnode!(+), gnode!(nt NT::CharSetOne));
    tree.add(Some(cc1), gnode!(t T::RSbracket));
    tree.add(Some(or), gnode!(t T::Dot));
    tree.add(Some(or), gnode!(t T::FixedSet));

    // char_set_one:
    //     SET_CHAR MINUS SET_CHAR | SET_CHAR | FIXED_SET;
    let tree = rules.get_tree_mut(NT::CharSetOne as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(t T::SetChar), gnode!(t T::Minus), gnode!(t T::SetChar)]);
    tree.add_iter(Some(or), [gnode!(t T::SetChar), gnode!(t T::FixedSet)]);

    rules
}
