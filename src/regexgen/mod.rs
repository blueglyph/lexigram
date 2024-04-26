mod tests;

use crate::dfa::{ReNode, ReType, Token, TokenId, Terminal};
use crate::segments::{Segments, Seg};
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
    Grammar,    // 16
    Lexer,      // 17
    Mode,       // 18
    Pop,        // 19
    Push,       // 20
    Return,     // 21
    Sikp,       // 22
    SymEof,     // 23
    Id,         // 24
    CharLit,    // 25
    CharSet,    // 26
    StrLit,     // 27
}

pub fn build_re() -> VecTree<ReNode> {
    let mut re = VecTree::new();
    let top = re.add(None, node!(|));
    re.addc_iter(Some(top), node!(&), [node!(str "->"), node!(=Id::Arrow as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ':'       ), node!(=Id::Colon     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ','       ), node!(=Id::Comma     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str ".."      ), node!(=Id::Ellipsis  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '{'       ), node!(=Id::Lbracket  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '('       ), node!(=Id::Lparen    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '~'       ), node!(=Id::Negate    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '+'       ), node!(=Id::Plus      as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '|'       ), node!(=Id::Or        as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '?'       ), node!(=Id::Question  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '}'       ), node!(=Id::Rbracket  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ')'       ), node!(=Id::Rparen    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ';'       ), node!(=Id::Semicolon as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '*'       ), node!(=Id::Star      as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "channels"), node!(=Id::Channels  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "fragment"), node!(=Id::Fragment  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "grammar" ), node!(=Id::Grammar   as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "lexer"   ), node!(=Id::Lexer     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "mode"    ), node!(=Id::Mode      as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "pop"     ), node!(=Id::Pop       as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "push"    ), node!(=Id::Push      as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "return"  ), node!(=Id::Return    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "skip"    ), node!(=Id::Sikp      as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "EOF"     ), node!(=Id::SymEof    as TokenId)]);

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
            re.add(Some(or1), node!(~['\n', '\r', '\t', '\\', '[', ']']));
    }
    let char_set = re.add(Some(top), node!(&));
        let or1 = re.add(Some(char_set), node!(|));
            let cc2 = re.add(Some(or1), node!(&));
                re.add(Some(cc2), node!(chr '['));
                let s3 = re.add(Some(cc2), node!(*));
                    let or4 = re.add(Some(s3), node!(|));
                        let cc5 = re.add(Some(or4), node!(&));
                            add_set_char(&mut re, cc5);
                            let or6 = re.add(Some(cc5), node!(|)); // ('-' SetChar )? = ('-' SetChar | <empty>)
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

#[cfg(test)]
const LEXICON: &str = r#"
lexer grammar RLLexer;
channels { CH_WHITESPACE, CH_COMMENTS }  // dummy

fragment BlockComment   : '/*' .*? '*/';
fragment LineComment    : '//' ~[\r\n]*;
fragment HexDigit       : [0-9a-fA-F];
fragment UnicodeEsc     : 'u{' HexDigit+ '}';
fragment EscChar        : '\\' ([nrt'\\] | UnicodeEsc);
fragment Char           : EscChar | ~[\n\r\t'\\];
fragment CharLiteral    : '\'' Char '\'';
fragment StrLiteral     : '\'' Char Char+ '\'';
fragment FixedSet       : ('\\w' | '\\d');
// Char inside a '[' ']' set
fragment EscSetChar     : '\\' ([nrt\\[\]\-] | UnicodeEsc);
fragment SetChar        : EscSetChar | ~[\n\r\t\\\[\]];
fragment Letter         : 'a'..'z';  // dummy
fragment NonLetter      : ~'a'..'z'; // dummy

ARROW           : '->';  /* // first token // */
COLON           : ':';
COMMA           : ',';
ELLIPSIS        : '..';
LBRACKET        : '{';
LPAREN          : '(';
NEGATE          : '~';
PLUS            : '+';
OR              : '|';
QUESTION        : '?';
RBRACKET        : '}';
RPAREN          : ')';
SEMICOLON       : ';';
STAR            : '*';
CHANNELS        : 'channels';
FRAGMENT        : 'fragment';
GRAMMAR         : 'grammar';
LEXER           : 'lexer';
MODE            : 'mode';
POP             : 'pop';
PUSH            : 'push';
RETURN          : 'return';
SiKP            : 'skip';
SYM_EOF         : 'EOF';

COMMENT         : BlockComment              -> skip;
LINECOMMENT     : LineComment               -> skip;
WHITESPACE      : [ \n\r\t]+                -> skip;

ID              : [a-zA-Z][a-zA-Z_0-9]*;

CHAR_LIT        : CharLiteral;
CHAR_SET        : '[' (SetChar '-' SetChar | SetChar | FixedSet)* ']'
                | '.'
                | FixedSet;
STR_LIT         : StrLiteral;
"#;
