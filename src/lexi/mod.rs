pub(super) mod tests;

use crate::dfa::{ReNode, ReType, Terminal, TokenId};
use crate::segments::{Seg, Segments};
use crate::{node, term};
use crate::vectree::VecTree;

#[repr(u16)]
pub enum Id {
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
    Return,     // 20
    Skip,       // 21
    SymEof,     // 22
    Id,         // 23
    CharLit,    // 24
    CharSet,    // 25
    StrLit,     // 26
}

pub const SYMBOLS: [(&str, Option<&str>); 27] = [
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
    ("Return",    Some("return")),      // 20
    ("Skip",      Some("skip")),        // 21
    ("SymEof",    Some("EOF")),         // 22
    ("Id",        None),                // 23
    ("CharLit",   None),                // 24
    ("CharSet",   None),                // 25
    ("StrLit",    None),                // 26
];

pub fn build_re() -> VecTree<ReNode> {
    let mut re = VecTree::new();
    let top = re.add_root(node!(|));

    // All symbols
    for (id, (_, text_op)) in SYMBOLS.iter().enumerate() {
        if let Some(text) = text_op {
            re.addc_iter(Some(top), node!(&), [node!(str *text), node!(=id as TokenId)]);
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
    re.add(Some(id), node!(=Id::Id as TokenId));

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
    re.add_iter(Some(char_lit), [node!(chr '\''), node!(=Id::CharLit as TokenId)]);

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
    re.add(Some(char_set), node!(=Id::CharSet as TokenId));

    // StrLit: '\'' Char Char+ '\''
    let str_lit = re.add(Some(top), node!(&));
    re.add(Some(str_lit), node!(chr '\''));
    add_char(&mut re, str_lit);
    let p = re.add(Some(str_lit), node!(+));
    add_char(&mut re, p);
    re.add_iter(Some(str_lit), [node!(chr '\''), node!(=Id::StrLit as TokenId)]);

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
lexicon RLLexer;
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
RETURN			: 'return';
SKiP			: 'skip';
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

pub const LEXICON_TOKENS: [TokenId; 261] = [
    16, 23,                                             // lexicon RLLexer;
    12, 14, 4, 23, 2, 23, 10,                           // channels { CH_WHITESPACE, CH_COMMENTS } // dummy
    15, 23, 1, 26, 25, 13, 9, 26, 12,                   // fragment BlockComment   : '/*' .*? '*/';
    15, 23, 1, 26, 6, 25, 13, 12,                       // fragment LineComment    : '//' ~[\r\n]*;
    15, 23, 1, 25, 12,                                  // fragment HexDigit       : [0-9a-fA-F];
    15, 23, 1, 26, 23, 7, 24, 12,                       // fragment UnicodeEsc     : 'u{' HexDigit+ '}';
    15, 23, 1, 24, 5, 25, 8, 23, 11, 12,                // fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
    15, 23, 1, 23, 8, 6, 25, 12,                        // fragment Char           : EscChar | ~[\n\r\t'\\];
    15, 23, 1, 24, 23, 24, 12,                          // fragment CharLiteral    : '\'' Char '\'';
    15, 23, 1, 24, 23, 23, 7, 24, 12,                   // fragment StrLiteral     : '\'' Char Char+ '\'';
    15, 23, 1, 5, 26, 8, 26, 11, 12,                    // fragment FixedSet       : ('\\w' | '\\d');
    15, 23, 1, 24, 5, 25, 8, 23, 11, 12,                // fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);
    15, 23, 1, 5, 23, 8, 6, 25, 11, 12,                 // fragment SetChar        : (EscSetChar | ~[\n\r\t\\\]]);
    15, 23, 1, 24, 3, 24, 12,                           // fragment Letter         : 'a'..'z';  // dummy
    15, 23, 1, 6, 24, 3, 24, 12,                        // fragment NonLetter      : ~'a'..'z'; // dummy
    23, 1, 26, 12,                                      // ARROW       : '->';
    23, 1, 24, 12,                                      // COLON       : ':';
    23, 1, 24, 12,                                      // COMMA       : ',';
    23, 1, 26, 12,                                      // ELLIPSIS    : '..';
    23, 1, 24, 12,                                      // LBRACKET    : '{';
    23, 1, 24, 12,                                      // LPAREN      : '(';
    23, 1, 24, 12,                                      // NEGATE      : '~';
    23, 1, 24, 12,                                      // PLUS        : '+';
    23, 1, 24, 12,                                      // OR          : '|';
    23, 1, 24, 12,                                      // QUESTION    : '?';
    23, 1, 24, 12,                                      // RBRACKET    : '}';
    23, 1, 24, 12,                                      // RPAREN      : ')';
    23, 1, 24, 12,                                      // SEMICOLON   : ';';
    23, 1, 24, 12,                                      // STAR        : '*';
    23, 1, 26, 12,                                      // CHANNELS    : 'channels';
    23, 1, 26, 12,                                      // FRAGMENT    : 'fragment';
    23, 1, 26, 12,                                      // LEXICON     : 'lexicon';
    23, 1, 26, 12,                                      // MODE        : 'mode';
    23, 1, 26, 12,                                      // POP         : 'pop';
    23, 1, 26, 12,                                      // PUSH        : 'push';
    23, 1, 26, 12,                                      // RETURN      : 'return';
    23, 1, 26, 12,                                      // SKiP        : 'skip';
    23, 1, 26, 12,                                      // SYM_EOF     : 'EOF';
    23, 1, 23, 0, 21, 12,                               // COMMENT     : BlockComment           -> skip;
    23, 1, 23, 0, 21, 12,                               // LINECOMMENT : LineComment            -> skip;
    23, 1, 25, 7, 0, 21, 12,                            // WHITESPACE  : [ \n\r\t]+             -> skip;
    23, 1, 25, 25, 13, 12,                              // ID          : [a-zA-Z][a-zA-Z_0-9]*;
    23, 1, 23, 12,                                      // CHAR_LIT    : CharLiteral;
    23, 1, 24, 5, 23, 24, 23, 8, 23, 8, 23, 11, 7, 24,  // CHAR_SET : '[' (SetChar '-' SetChar | SetChar | FixedSet)+ ']'
        8, 24,                                          //          | '.'
        8, 23, 12,                                      //          | FixedSet;
    23, 1, 23, 12,                                      // STR_LIT     : StrLiteral;
];

pub const LEXICON_TEXT: [&str; 261] = [
    "lexicon", "RLLexer", ";", "channels", "{", "CH_WHITESPACE", ",", "CH_COMMENTS", "}", "fragment", "BlockComment", ":", "'/*'", ".", "*", "?",
    "'*/'", ";", "fragment", "LineComment", ":", "'//'", "~", r#"[\r\n]"#, "*", ";", "fragment", "HexDigit", ":", "[0-9a-fA-F]", ";", "fragment",
    "UnicodeEsc", ":", "'u{'", "HexDigit", "+", "'}'", ";", "fragment", "EscChar", ":", r#"'\\'"#, "(", r#"[nrt'\\]"#, "|", "UnicodeEsc", ")", ";", "fragment",
    "Char", ":", "EscChar", "|", "~", r#"[\n\r\t'\\]"#, ";", "fragment", "CharLiteral", ":", r#"'\''"#, "Char", r#"'\''"#, ";", "fragment", "StrLiteral",
    ":", r#"'\''"#, "Char", "Char", "+", r#"'\''"#, ";", "fragment", "FixedSet", ":", "(", r#"'\\w'"#, "|", r#"'\\d'"#, ")", ";", "fragment", "EscSetChar",
    ":", r#"'\\'"#, "(", r#"[nrt\\[\]\-]"#, "|", "UnicodeEsc", ")", ";", "fragment", "SetChar", ":", "(", "EscSetChar", "|", "~", r#"[\n\r\t\\\]]"#, ")",
    ";", "fragment", "Letter", ":", "'a'", "..", "'z'", ";", "fragment", "NonLetter", ":", "~", "'a'", "..", "'z'", ";", "ARROW", ":", "'->'", ";", "COLON",
    ":", "':'", ";", "COMMA", ":", "','", ";", "ELLIPSIS", ":", "'..'", ";", "LBRACKET", ":", "'{'", ";", "LPAREN", ":", "'('", ";", "NEGATE", ":", "'~'",
    ";", "PLUS", ":", "'+'", ";", "OR", ":", "'|'", ";", "QUESTION", ":", "'?'", ";", "RBRACKET", ":", "'}'", ";", "RPAREN", ":", "')'", ";", "SEMICOLON",
    ":", "';'", ";", "STAR", ":", "'*'", ";", "CHANNELS", ":", "'channels'", ";", "FRAGMENT", ":", "'fragment'", ";", "LEXICON", ":", "'lexicon'", ";",
    "MODE", ":", "'mode'", ";", "POP", ":", "'pop'", ";", "PUSH", ":", "'push'", ";", "RETURN", ":", "'return'", ";",
    "SKiP", ":", "'skip'", ";", "SYM_EOF", ":", "'EOF'", ";", "COMMENT", ":", "BlockComment", "->", "skip", ";", "LINECOMMENT", ":", "LineComment", "->",
    "skip", ";", "WHITESPACE", ":", r#"[ \n\r\t]"#, "+", "->", "skip", ";", "ID", ":", "[a-zA-Z]", "[a-zA-Z_0-9]", "*", ";", "CHAR_LIT", ":", "CharLiteral",
    ";", "CHAR_SET", ":", "'['", "(", "SetChar", "'-'", "SetChar", "|", "SetChar", "|", "FixedSet", ")", "+", "']'", "|", "'.'", "|", "FixedSet", ";",
    "STR_LIT", ":", "StrLiteral", ";"
];
