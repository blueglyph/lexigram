#![allow(unused)]

pub(super) mod tests;

use iter_index::IndexerIterator;
use vectree::VecTree;
use crate::dfa::{ReNode, ReType, TokenId};
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
    Ellipsis,   // 3
    Lbracket,   // 4
    Lparen,     // 5
    Negate,     // 6
    Plus,       // 7
    Or,         // 8
    Question,   // 9
    Rbracket,   // 10
    Rparen,     // 11
    Semicolon,  // 12
    Star,       // 13
    Channels,   // 14
    Fragment,   // 15
    Lexicon,    // 16
    Mode,       // 17
    Pop,        // 18
    Push,       // 19
    More,       // 20
    Skip,       // 21
    Type,       // 22
    Channel,    // 23
    SymEof,     // 24
    Id,         // 25
    CharLit,    // 26
    CharSet,    // 27
    StrLit,     // 28
}

pub const TERMINALS: [(&str, Option<&str>); 29] = [
    ("Arrow",     Some("->")),          // 0
    ("Colon",     Some(":")),           // 1
    ("Comma",     Some(",")),           // 2
    ("Ellipsis",  Some("..")),          // 3
    ("Lbracket",  Some("{")),           // 4
    ("Lparen",    Some("(")),           // 5
    ("Negate",    Some("~")),           // 6
    ("Plus",      Some("+")),           // 7
    ("Or",        Some("|")),           // 8
    ("Question",  Some("?")),           // 9
    ("Rbracket",  Some("}")),           // 10
    ("Rparen",    Some(")")),           // 11
    ("Semicolon", Some(";")),           // 12
    ("Star",      Some("*")),           // 13
    ("Channels",  Some("channels")),    // 14
    ("Fragment",  Some("fragment")),    // 15
    ("Lexicon",   Some("lexicon")),     // 16
    ("Mode",      Some("mode")),        // 17
    ("Pop",       Some("pop")),         // 18
    ("Push",      Some("push")),        // 19
    ("More",      Some("more")),        // 20
    ("Skip",      Some("skip")),        // 21
    ("Type",      Some("type")),        // 22
    ("Channel",   Some("channel")),     // 23
    ("SymEof",    Some("EOF")),         // 24
    ("Id",        None),                // 25
    ("CharLit",   None),                // 26
    ("CharSet",   None),                // 27
    ("StrLit",    None),                // 28
];

pub fn build_re() -> VecTree<ReNode> {
    let mut re = VecTree::new();
    let top = re.add_root(node!(|));

    // All symbols
    for (id, (_, text_op)) in TERMINALS.iter().index() {
        if let Some(text) = text_op {
            re.addc_iter(Some(top), node!(&), [node!(str *text), node!(=id)]);
        }
    }

    // Comment: '/' '*' .*? '*' '/'
    let comment = re.add(Some(top), node!(&));
    re.add(Some(comment), node!(str "/*"));
    let l = re.add(Some(comment), node!(??));
    re.addc(Some(l), node!(*), node!([DOT]));
    re.add_iter(Some(comment), [node!(str "*/"), node!(term!(skip))]);

    // LineComment: '//' ~[\r\n]*
    let line_comment = re.add(Some(top), node!(&));
    re.add(Some(line_comment), node!(str "//"));
    re.addc(Some(line_comment), node!(*), node!(~['\r', '\n']));
    re.add(Some(line_comment), node!(term!(skip)));

    // Whitespace: [ \n\r\t]+
    let whitespace = re.add(Some(top), node!(&));
    re.addc(Some(whitespace), node!(+), node!([' ', '\n', '\r', '\t']));
    re.add(Some(whitespace), node!(term!(skip)));

    // Id: [a-zA-Z][a-zA-Z_0-9]*
    let id = re.add(Some(top), node!(&));
    re.add(Some(id), node!(['a'-'z', 'A'-'Z']));
    re.addc(Some(id), node!(*), node!(['_', '0'-'9', 'a'-'z', 'A'-'Z']));
    re.add(Some(id), node!(=T::Id as TokenId));

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
    let char_lit = re.add(Some(top), node!(&));
    re.add(Some(char_lit), node!(chr '\''));
    add_char(&mut re, char_lit);
    re.add_iter(Some(char_lit), [node!(chr '\''), node!(=T::CharLit as TokenId)]);

    // CharSet:                                                       |     |
    //          '[' (                         |                )* ']'   '.'   ('\\w' | '\\d')
    //               SetChar ( '-' SetChar )?   ('\\w' | '\\d')
    //                                       or4                s3   or1   or1
    //                    or3----------------
    // SetChar: ( '\\' ([nrt\\[\]\-] | 'u{' [0-9a-fA-F]+ '}') | ~[\n\r\t\\] )
    fn add_set_char(re: &mut VecTree<ReNode>, parent: usize) {
        let or1 = re.add(Some(parent), node!(|));
            let cc2 = re.add(Some(or1), node!(&));
                re.add(Some(cc2), node!(chr '\\'));
                let or3 = re.add(Some(cc2), node!(|));
                    re.add(Some(or3), node!(['n', 'r', 't', '\'', '\\', '[', ']', '-']));
                    let cc4 = re.add(Some(or3), node!(&));
                        re.add(Some(cc4), node!(str "u{"));
                        re.addc(Some(cc4), node!(+), node!(['0'-'9', 'a'-'f', 'A'-'F']));
                        re.add(Some(cc4), node!(chr '}'));
            // Note: we accept the opening bracket within brackets, with or without escaping: [a-z[] or [a-z\[]
            re.add(Some(or1), node!(~['\n', '\r', '\t', '\\', ']']));
    }
    let char_set = re.add(Some(top), node!(&));
        let or1 = re.add(Some(char_set), node!(|));
            let cc2 = re.add(Some(or1), node!(&));
                re.add(Some(cc2), node!(chr '['));
                let s3 = re.add(Some(cc2), node!(*));
                    let or4 = re.add(Some(s3), node!(|));
                        let cc5 = re.add(Some(or4), node!(&));
                            add_set_char(&mut re, cc5);
                            // Since x? isn't defined, ('-' SetChar )? = ('-' SetChar | <empty>)
                            let or6 = re.add(Some(cc5), node!(|));
                                let cc7 = re.add(Some(or6), node!(&));
                                    re.add(Some(cc7), node!(chr '-'));
                                    add_set_char(&mut re, cc7);
                                re.add(Some(or6), node!(e));
                        let or5 = re.add(Some(or4), node!(|)); // FixedSet
                            re.add_iter(Some(or5), [node!(str "\\w"), node!(str "\\d")]);
                re.add(Some(cc2), node!(chr ']'));
            re.add(Some(or1), node!(chr '.'));
            let or2 = re.add(Some(or1), node!(|)); // FixedSet
                re.add_iter(Some(or2), [node!(str "\\w"), node!(str "\\d")]);
    re.add(Some(char_set), node!(=T::CharSet as TokenId));

    // StrLit: '\'' Char Char+ '\''
    let str_lit = re.add(Some(top), node!(&));
    re.add(Some(str_lit), node!(chr '\''));
    add_char(&mut re, str_lit);
    let p = re.add(Some(str_lit), node!(+));
    add_char(&mut re, p);
    re.add_iter(Some(str_lit), [node!(chr '\''), node!(=T::StrLit as TokenId)]);

    re
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
fragment FixedSet		: ('\\w' | '\\d');
// Char inside a '[' ']' set
fragment EscSetChar		: '\\' ([nrt\\[\]\-] | UnicodeEsc);
fragment SetChar		: (EscSetChar | ~[\n\r\t\\\]]);
fragment Letter			: 'a'..'z';  // dummy
fragment NonLetter		: ~'a'..'z'; // dummy

ARROW			: '->'; /* // first token // */
COLON			: ':';
COMMA			: ',';
ELLIPSIS		: '..';
LBRACKET    	: '{';
LPAREN			: '(';
NEGATE			: '~';
PLUS			: '+';
OR				: '|';
QUESTION		: '?';
RBRACKET    	: '}';
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

CHAR_SET		: '[' (SetChar '-' SetChar | SetChar | FixedSet)+ ']'
                | '.'
                | FixedSet;

STR_LIT			: StrLiteral;
"#;

pub const LEXICON_TOKENS: [TokenId; 269] = [
    16, 25,                                             // lexicon RLLexer;
    12, 14, 4, 25, 2, 25, 10,                           // channels { CH_WHITESPACE, CH_COMMENTS } // dummy
    15, 25, 1, 28, 27, 13, 9, 28, 12,                   // fragment BlockComment   : '/*' .*? '*/';
    15, 25, 1, 28, 6, 27, 13, 12,                       // fragment LineComment    : '//' ~[\r\n]*;
    15, 25, 1, 27, 12,                                  // fragment HexDigit       : [0-9a-fA-F];
    15, 25, 1, 28, 25, 7, 26, 12,                       // fragment UnicodeEsc     : 'u{' HexDigit+ '}';
    15, 25, 1, 26, 5, 27, 8, 25, 11, 12,                // fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
    15, 25, 1, 25, 8, 6, 27, 12,                        // fragment Char           : EscChar | ~[\n\r\t'\\];
    15, 25, 1, 26, 25, 26, 12,                          // fragment CharLiteral    : '\'' Char '\'';
    15, 25, 1, 26, 25, 25, 7, 26, 12,                   // fragment StrLiteral     : '\'' Char Char+ '\'';
    15, 25, 1, 5, 28, 8, 28, 11, 12,                    // fragment FixedSet       : ('\\w' | '\\d');
    15, 25, 1, 26, 5, 27, 8, 25, 11, 12,                // fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);
    15, 25, 1, 5, 25, 8, 6, 27, 11, 12,                 // fragment SetChar        : (EscSetChar | ~[\n\r\t\\\]]);
    15, 25, 1, 26, 3, 26, 12,                           // fragment Letter         : 'a'..'z';  // dummy
    15, 25, 1, 6, 26, 3, 26, 12,                        // fragment NonLetter      : ~'a'..'z'; // dummy
    25, 1, 28, 12,                                      // ARROW       : '->';
    25, 1, 26, 12,                                      // COLON       : ':';
    25, 1, 26, 12,                                      // COMMA       : ',';
    25, 1, 28, 12,                                      // ELLIPSIS    : '..';
    25, 1, 26, 12,                                      // LBRACKET    : '{';
    25, 1, 26, 12,                                      // LPAREN      : '(';
    25, 1, 26, 12,                                      // NEGATE      : '~';
    25, 1, 26, 12,                                      // PLUS        : '+';
    25, 1, 26, 12,                                      // OR          : '|';
    25, 1, 26, 12,                                      // QUESTION    : '?';
    25, 1, 26, 12,                                      // RBRACKET    : '}';
    25, 1, 26, 12,                                      // RPAREN      : ')';
    25, 1, 26, 12,                                      // SEMICOLON   : ';';
    25, 1, 26, 12,                                      // STAR        : '*';
    25, 1, 28, 12,                                      // CHANNELS    : 'channels';
    25, 1, 28, 12,                                      // FRAGMENT    : 'fragment';
    25, 1, 28, 12,                                      // LEXICON     : 'lexicon';
    25, 1, 28, 12,                                      // MODE        : 'mode';
    25, 1, 28, 12,                                      // POP         : 'pop';
    25, 1, 28, 12,                                      // PUSH        : 'push';
    25, 1, 28, 12,                                      // MORE        : 'more';
    25, 1, 28, 12,                                      // SKiP        : 'skip';
    25, 1, 28, 12,                                      // TYPE        : 'type';
    25, 1, 28, 12,                                      // CHANNEL     : 'channel';
    25, 1, 28, 12,                                      // SYM_EOF     : 'EOF';
    25, 1, 25, 0, 21, 12,                               // COMMENT     : BlockComment           -> skip;
    25, 1, 25, 0, 21, 12,                               // LINECOMMENT : LineComment            -> skip;
    25, 1, 27, 7, 0, 21, 12,                            // WHITESPACE  : [ \n\r\t]+             -> skip;
    25, 1, 27, 27, 13, 12,                              // ID          : [a-zA-Z][a-zA-Z_0-9]*;
    25, 1, 25, 12,                                      // CHAR_LIT    : CharLiteral;
    25, 1, 26, 5, 25, 26, 25, 8, 25, 8, 25, 11, 7, 26,  // CHAR_SET : '[' (SetChar '-' SetChar | SetChar | FixedSet)+ ']'
        8, 26,                                          //          | '.'
        8, 25, 12,                                      //          | FixedSet;
    25, 1, 25, 12,                                      // STR_LIT     : StrLiteral;
];

pub const LEXICON_TEXT: [&str; 269] = [
    "lexicon", "LexiLexer", ";", "channels", "{", "CH_WHITESPACE", ",", "CH_COMMENTS", "}", "fragment", "BlockComment", ":", "'/*'", ".", "*", "?",
    "'*/'", ";", "fragment", "LineComment", ":", "'//'", "~", r#"[\r\n]"#, "*", ";", "fragment", "HexDigit", ":", "[0-9a-fA-F]", ";", "fragment",
    "UnicodeEsc", ":", "'u{'", "HexDigit", "+", "'}'", ";", "fragment", "EscChar", ":", r#"'\\'"#, "(", r#"[nrt'\\]"#, "|", "UnicodeEsc", ")", ";", "fragment",
    "Char", ":", "EscChar", "|", "~", r#"[\n\r\t'\\]"#, ";", "fragment", "CharLiteral", ":", r#"'\''"#, "Char", r#"'\''"#, ";", "fragment", "StrLiteral",
    ":", r#"'\''"#, "Char", "Char", "+", r#"'\''"#, ";", "fragment", "FixedSet", ":", "(", r#"'\\w'"#, "|", r#"'\\d'"#, ")", ";", "fragment", "EscSetChar",
    ":", r#"'\\'"#, "(", r#"[nrt\\[\]\-]"#, "|", "UnicodeEsc", ")", ";", "fragment", "SetChar", ":", "(", "EscSetChar", "|", "~", r#"[\n\r\t\\\]]"#, ")",
    ";", "fragment", "Letter", ":", "'a'", "..", "'z'", ";", "fragment", "NonLetter", ":", "~", "'a'", "..", "'z'", ";", "ARROW", ":", "'->'", ";", "COLON",
    ":", "':'", ";", "COMMA", ":", "','", ";", "ELLIPSIS", ":", "'..'", ";", "LBRACKET", ":", "'{'", ";", "LPAREN", ":", "'('", ";", "NEGATE", ":", "'~'",
    ";", "PLUS", ":", "'+'", ";", "OR", ":", "'|'", ";", "QUESTION", ":", "'?'", ";", "RBRACKET", ":", "'}'", ";", "RPAREN", ":", "')'", ";", "SEMICOLON",
    ":", "';'", ";", "STAR", ":", "'*'", ";", "CHANNELS", ":", "'channels'", ";", "FRAGMENT", ":", "'fragment'", ";", "LEXICON", ":", "'lexicon'", ";",
    "MODE", ":", "'mode'", ";", "POP", ":", "'pop'", ";", "PUSH", ":", "'push'", ";", "MORE", ":", "'more'", ";", "SKiP", ":", "'skip'", ";",
    "TYPE", ":", "'type'", ";", "CHANNEL", ":", "'channel'", ";", "SYM_EOF", ":", "'EOF'", ";",
    "COMMENT", ":", "BlockComment", "->", "skip", ";", "LINECOMMENT", ":", "LineComment", "->",
    "skip", ";", "WHITESPACE", ":", r#"[ \n\r\t]"#, "+", "->", "skip", ";", "ID", ":", "[a-zA-Z]", "[a-zA-Z_0-9]", "*", ";", "CHAR_LIT", ":", "CharLiteral",
    ";", "CHAR_SET", ":", "'['", "(", "SetChar", "'-'", "SetChar", "|", "SetChar", "|", "FixedSet", ")", "+", "']'", "|", "'.'", "|", "FixedSet", ";",
    "STR_LIT", ":", "StrLiteral", ";"
];

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
    PUSH LPAREN ID RPAREN
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
    repeat_item STAR QUESTION?
|   repeat_item PLUS QUESTION?
|   item QUESTION?
;

item:
    ID
|   SYM_EOF
|   CHAR_LIT (ELLIPSIS CHAR_LIT)?
|   STR_LIT
|   CHAR_SET
|   LPAREN alt_items RPAREN
|   NEGATE item
;
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
}

const NON_TERMINALS: [&str; 13] = [
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
    //     PUSH LPAREN ID RPAREN
    // |   POP
    // |   SKiP
    // |   MORE
    // |   TYPE LPAREN ID RPAREN
    // |   CHANNEL LPAREN ID RPAREN
    // ;
    //
    let tree = rules.get_tree_mut(NT::Action as VarId);
    let or = tree.add_root(gnode!(|));
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
    //     alt_items OR alt_item
    // |   alt_item
    // ;
    let tree = rules.get_tree_mut(NT::AltItems as VarId);
    let or = tree.add_root(gnode!(|));
    tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::AltItems), gnode!(t T::Or), gnode!(nt NT::AltItem)]);
    tree.add(Some(or), gnode!(nt NT::AltItem));

    // alt_item:
    // 	repeat_item+
    // ;
    //
    let tree = rules.get_tree_mut(NT::AltItem as VarId);
    let plus = tree.add_root(gnode!(+));
    tree.add(Some(plus), gnode!(nt NT::RepeatItem));

    // repeat_item:
    //     repeat_item STAR QUESTION?
    // |   repeat_item PLUS QUESTION?
    // |   item QUESTION?
    // ;
    let tree = rules.get_tree_mut(NT::RepeatItem as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::RepeatItem), gnode!(t T::Star)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::RepeatItem), gnode!(t T::Plus)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));
    let cc1 = tree.addc_iter(Some(or), gnode!(&), [gnode!(nt NT::Item)]);
    tree.addc(Some(cc1), gnode!(?), gnode!(t T::Question));

    //
    // item:
    //     ID
    // |   SYM_EOF
    // |   CHAR_LIT (ELLIPSIS CHAR_LIT)?
    // |   STR_LIT
    // |   CHAR_SET
    // |   LPAREN alt_item RPAREN
    // |   NEGATE item
    // ;
    let tree = rules.get_tree_mut(NT::Item as VarId);
    let or = tree.add_root(gnode!(|));
    let cc1s = tree.add_iter(Some(or), [
        gnode!(t T::Id),        // 0: ID
        gnode!(t T::SymEof),    // 1: SYM_EOF
        gnode!(&),              // 2: CHAR_LIT (ELLIPSIS CHAR_LIT)?
        gnode!(t T::StrLit),    // 3: STR_LIT
        gnode!(t T::CharSet),   // 4: CHAR_SET
        gnode!(&),              // 5: LPAREN alt_items RPAREN
        gnode!(&),              // 6: NEGATE item
    ]);
    tree.add(Some(cc1s[2]), gnode!(t T::CharLit));
    let maybe2 = tree.add(Some(cc1s[2]), gnode!(?));
    tree.addc_iter(Some(maybe2), gnode!(&), [gnode!(t T::Ellipsis), gnode!(t T::CharLit)]);
    tree.add_iter(Some(cc1s[5]), [gnode!(t T::Lparen), gnode!(nt NT::AltItems), gnode!(t T::Rparen)]);
    tree.add_iter(Some(cc1s[6]), [gnode!(t T::Negate), gnode!(nt NT::Item)]);

    rules
}
