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
    Arrow = 0,  // 0
    Colon,      // 1
    Comma,      // 2
    Dot,        // 3
    Ellipsis,   // 4
    Lbracket,   // 5
    LSbracket,  // 6
    Lparen,     // 7
    Negate,     // 8
    Minus,      // 9
    Plus,       // 10
    Or,         // 11
    Question,   // 12
    Rbracket,   // 13
    RSbracket,  // 14
    Rparen,     // 15
    Semicolon,  // 16
    Star,       // 17
    Channels,   // 18
    Fragment,   // 19
    Lexicon,    // 20
    Mode,       // 21
    Pop,        // 22
    Push,       // 23
    More,       // 24
    Skip,       // 25
    Type,       // 26
    Channel,    // 27
    SymEof,     // 28
    Id,         // 29
    CharLit,    // 30
    StrLit,     // 31
    FixedSet,   // 32
    SetChar,    // 33
}

pub const TERMINALS: [(&str, Option<&str>); 34] = [
    ("Arrow",     Some("->")),          // 0
    ("Colon",     Some(":")),           // 1
    ("Comma",     Some(",")),           // 2
    ("Dot",       Some(".")),           // 3
    ("Ellipsis",  Some("..")),          // 4
    ("Lbracket",  Some("{")),           // 5
    ("LSbracket", Some("[")),           // 6
    ("Lparen",    Some("(")),           // 7
    ("Negate",    Some("~")),           // 8
    ("Minus",     Some("-")),           // 9
    ("Plus",      Some("+")),           // 10
    ("Or",        Some("|")),           // 11
    ("Question",  Some("?")),           // 12
    ("Rbracket",  Some("}")),           // 13
    ("RSbracket", Some("]")),           // 14
    ("Rparen",    Some(")")),           // 15
    ("Semicolon", Some(";")),           // 16
    ("Star",      Some("*")),           // 17
    ("Channels",  Some("channels")),    // 18
    ("Fragment",  Some("fragment")),    // 19
    ("Lexicon",   Some("lexicon")),     // 20
    ("Mode",      Some("mode")),        // 21
    ("Pop",       Some("pop")),         // 22
    ("Push",      Some("push")),        // 23
    ("More",      Some("more")),        // 24
    ("Skip",      Some("skip")),        // 25
    ("Type",      Some("type")),        // 26
    ("Channel",   Some("channel")),     // 27
    ("SymEof",    Some("EOF")),         // 28
    ("Id",        None),                // 29
    ("CharLit",   None),                // 30
    ("StrLit",    None),                // 31
    ("FixedSet",  None),                // 32
    ("SetChar",   None),                // 33
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
channels { CH_WHITESPACE, CH_COMMENTS }	// dummy

fragment BlockComment	: '/*' .*? '*/';
fragment LineComment	: '//' ~[\r\n]*;
fragment HexDigit		: [0-9a-fA-F];
fragment UnicodeEsc		: 'u{' HexDigit+ '}';
fragment EscChar		: '\\' ([nrt'\\] | UnicodeEsc);
fragment Char			: EscChar | ~[\n\r\t'\\];
fragment CharLiteral	: '\'' Char '\'';
fragment StrLiteral		: '\'' Char Char+ '\'';
// Char inside a '[' ']' set
fragment EscSetChar		: '\\' ([nrt\\[\]\-] | UnicodeEsc);
fragment Letter			: 'a'..'z';  // dummy
fragment NonLetter		: ~'a'..'z'; // dummy

ARROW			: '->'; /* // first token // */
COLON			: ':';
COMMA			: ',';
DOT             : '.';
ELLIPSIS		: '..';
LBRACKET    	: '{';
LSBRACKET       : '[';
LPAREN			: '(';
NEGATE			: '~';
MINUS           : '-';
PLUS			: '+';
OR				: '|';
QUESTION		: '?';
RBRACKET    	: '}';
RSBRACKET       : ']';
RPAREN			: ')';
SEMICOLON		: ';';
STAR			: '*';

CHANNELS		: 'channels';
FRAGMENT		: 'fragment';
LEXICON			: 'lexicon';
MODE			: 'mode';
POP				: 'pop';
PUSH			: 'push';
MORE			: 'more';
SKiP			: 'skip';
TYPE            : 'type';
CHANNEL         : 'channel';
SYM_EOF			: 'EOF';

COMMENT			: BlockComment 				-> skip;
LINECOMMENT		: LineComment				-> skip;
WHITESPACE		: [ \n\r\t]+				-> skip;

ID				: [a-zA-Z][a-zA-Z_0-9]*;

CHAR_LIT		: CharLiteral;
STR_LIT			: StrLiteral;

FIXED_SET       : ('\\w' | '\\d');
SET_CHAR        : (EscSetChar | ~[\n\r\t\\\]\-]);
"#;

pub const LEXICON_TOKENS: [TokenId; 327] = [
    20, 29, 16,                                                 // lexicon RLLexer;
    18, 5, 29, 2, 29, 13,                                       // channels { CH_WHITESPACE, CH_COMMENTS } // dummy
    19, 29, 1, 31, 3, 17, 12, 31, 16,                           // fragment BlockComment   : '/*' .*? '*/';
    19, 29, 1, 31, 8, 6, 33, 33, 14, 17, 16,                    // fragment LineComment    : '//' ~[\r\n]*;
    19, 29, 1, 6, 33, 9, 33, 33, 9, 33, 33, 9, 33, 14, 16,      // fragment HexDigit       : [0-9a-fA-F];
    19, 29, 1, 31, 29, 10, 30, 16,                              // fragment UnicodeEsc     : 'u{' HexDigit+ '}';
    19, 29, 1, 30, 7, 6, 33, 33, 33, 33, 33, 14, 11, 29, 15, 16,// fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
    19, 29, 1, 29, 11, 8, 6, 33, 33, 33, 33, 33, 14, 16,        // fragment Char           : EscChar | ~[\n\r\t'\\];
    19, 29, 1, 30, 29, 30, 16,                                  // fragment CharLiteral    : '\'' Char '\'';
    19, 29, 1, 30, 29, 29, 10, 30, 16,                          // fragment StrLiteral     : '\'' Char Char+ '\'';
    19, 29, 1, 30, 7, 6, 33, 33, 33, 33, 33, 33, 33, 14, 11, 29,
               15, 16,                                          // fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);
    19, 29, 1, 30, 4, 30, 16,                                   // fragment Letter         : 'a'..'z';  // dummy
    19, 29, 1, 8, 30, 4, 30, 16,                                // fragment NonLetter      : ~'a'..'z'; // dummy

    29, 1, 31, 16,                                              // ARROW       : '->';
    29, 1, 30, 16,                                              // COLON       : ':';
    29, 1, 30, 16,                                              // COMMA       : ',';
    29, 1, 30, 16,                                              // DOT         : '.';
    29, 1, 31, 16,                                              // ELLIPSIS    : '..';
    29, 1, 30, 16,                                              // LBRACKET    : '{';
    29, 1, 30, 16,                                              // LSBRACKET   : '[';
    29, 1, 30, 16,                                              // LPAREN      : '(';
    29, 1, 30, 16,                                              // NEGATE      : '~';
    29, 1, 30, 16,                                              // MINUS       : '-';
    29, 1, 30, 16,                                              // PLUS        : '+';
    29, 1, 30, 16,                                              // OR          : '|';
    29, 1, 30, 16,                                              // QUESTION    : '?';
    29, 1, 30, 16,                                              // RBRACKET    : '}';
    29, 1, 30, 16,                                              // RSBRACKET   : ']';
    29, 1, 30, 16,                                              // RPAREN      : ')';
    29, 1, 30, 16,                                              // SEMICOLON   : ';';
    29, 1, 30, 16,                                              // STAR        : '*';
    29, 1, 31, 16,                                              // CHANNELS    : 'channels';
    29, 1, 31, 16,                                              // FRAGMENT    : 'fragment';
    29, 1, 31, 16,                                              // LEXICON     : 'lexicon';
    29, 1, 31, 16,                                              // MODE        : 'mode';
    29, 1, 31, 16,                                              // POP         : 'pop';
    29, 1, 31, 16,                                              // PUSH        : 'push';
    29, 1, 31, 16,                                              // MORE        : 'more';
    29, 1, 31, 16,                                              // SKiP        : 'skip';
    29, 1, 31, 16,                                              // TYPE        : 'type';
    29, 1, 31, 16,                                              // CHANNEL     : 'channel';
    29, 1, 31, 16,                                              // SYM_EOF     : 'EOF';
    29, 1, 29, 0, 25, 16,                                       // COMMENT     : BlockComment           -> skip;
    29, 1, 29, 0, 25, 16,                                       // LINECOMMENT : LineComment            -> skip;
    29, 1, 6, 33, 33, 33, 33, 14, 10, 0, 25, 16,                // WHITESPACE  : [ \n\r\t]+             -> skip;
    29, 1, 6, 33, 9, 33, 33, 9, 33, 14, 6, 33, 9, 33, 33,
              9, 33, 33, 33, 9, 33, 14, 17, 16,                 // ID          : [a-zA-Z][a-zA-Z_0-9]*;
    29, 1, 29, 16,                                              // CHAR_LIT    : CharLiteral;
    29, 1, 29, 16,                                              // STR_LIT     : StrLiteral;
    29, 1, 7, 31, 11, 31, 15, 16,                               // FIXED_SET   : ('\\w' | '\\d');
    29, 1, 7, 29, 11, 8, 6, 33, 33, 33, 33, 33, 33, 14, 15, 16  // SET_CHAR    : (EscSetChar | ~[\n\r\t\\\]\-]);
];

pub const LEXICON_TEXT: [&str; 327] = [
    "lexicon", "LexiLexer", ";", "channels", "{", "CH_WHITESPACE", ",", "CH_COMMENTS", "}", "fragment", "BlockComment",
    ":", "'/*'", ".", "*", "?", "'*/'", ";", "fragment", "LineComment", ":", "'//'", "~", "[", "\\r", "\\n", "]",
    "*", ";", "fragment", "HexDigit", ":", "[", "0", "-", "9", "a", "-", "f", "A", "-", "F", "]", ";",
    "fragment", "UnicodeEsc", ":", "'u{'", "HexDigit", "+", "'}'", ";", "fragment", "EscChar",
    ":", "'\\\\'", "(", "[", "n", "r", "t", "'", "\\\\", "]", "|", "UnicodeEsc", ")", ";", "fragment", "Char", ":", "EscChar", "|", "~",
    "[", "\\n", "\\r", "\\t", "'", "\\\\", "]", ";", "fragment", "CharLiteral", ":", "'\\''", "Char", "'\\''", ";", "fragment", "StrLiteral",
    ":", "'\\''", "Char", "Char", "+", "'\\''", ";", "fragment", "EscSetChar", ":", "'\\\\'", "(", "[", "n", "r", "t", "\\\\", "[", "\\]", "\\-", "]",
    "|", "UnicodeEsc", ")", ";", "fragment", "Letter", ":", "'a'", "..", "'z'", ";", "fragment", "NonLetter", ":", "~", "'a'", "..", "'z'", ";",
    "ARROW", ":", "'->'", ";", "COLON", ":", "':'", ";", "COMMA", ":", "','", ";", "DOT", ":", "'.'", ";", "ELLIPSIS", ":", "'..'", ";", "LBRACKET",
    ":", "'{'", ";", "LSBRACKET", ":", "'['", ";", "LPAREN", ":", "'('", ";", "NEGATE", ":", "'~'", ";", "MINUS", ":", "'-'", ";", "PLUS", ":",
    "'+'", ";", "OR", ":", "'|'", ";", "QUESTION", ":", "'?'", ";", "RBRACKET", ":", "'}'", ";", "RSBRACKET", ":", "']'", ";", "RPAREN", ":", "')'",
    ";", "SEMICOLON", ":", "';'", ";", "STAR", ":", "'*'", ";", "CHANNELS", ":", "'channels'", ";", "FRAGMENT", ":", "'fragment'", ";", "LEXICON", ":",
    "'lexicon'", ";", "MODE", ":", "'mode'", ";", "POP", ":", "'pop'", ";", "PUSH", ":", "'push'", ";", "MORE", ":", "'more'", ";", "SKiP", ":", "'skip'",
    ";", "TYPE", ":", "'type'", ";", "CHANNEL", ":", "'channel'", ";", "SYM_EOF", ":", "'EOF'", ";", "COMMENT", ":", "BlockComment", "->", "skip", ";",
    "LINECOMMENT", ":", "LineComment", "->", "skip", ";", "WHITESPACE", ":", "[", " ", "\\n", "\\r", "\\t", "]", "+", "->", "skip", ";", "ID", ":", "[",
    "a", "-", "z", "A", "-", "Z", "]", "[", "a", "-", "z", "A", "-", "Z", "_", "0", "-", "9", "]", "*", ";", "CHAR_LIT", ":", "CharLiteral", ";", "STR_LIT",
    ":", "StrLiteral", ";", "FIXED_SET", ":", "(", "'\\\\w'", "|", "'\\\\d'", ")", ";", "SET_CHAR", ":", "(", "EscSetChar", "|", "~", "[", "\\n", "\\r", "\\t",
    "\\\\", "\\]", "\\-", "]", ")", ";"];

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
    symbol_table.extend_non_terminals(NON_TERMINALS);
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
    let star2 = tree.addc(Some(cc1), gnode!(+), gnode!(nt NT::CharSetOne));
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
