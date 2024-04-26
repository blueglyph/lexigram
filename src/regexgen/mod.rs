mod tests;

use crate::dfa::{ReNode, ReType, Token, TokenId, Terminal};
use crate::segments::{Segments, Seg};
use crate::{node, term};
use crate::vectree::VecTree;

#[repr(u16)]
pub enum Id {
    Arrow = 0,
    Backslash,
    Colon,
    Comma,
    Ellipsis,
    Lbracket,
    Lsbracket,
    Lparen,
    Minus,
    Negate,
    Plus,
    Or,
    Question,
    Quote,
    Rbracket,
    Rsbracket,
    Rparen,
    Semicolon,
    Squote,
    Star,
    Channels,
    Fragment,
    Grammar,
    Lexer,
    Mode,
    Pop,
    Push,
    Return,
    Sikp,
    SymEof,
    Comment,
    Linecomment,
    Whitespace,
    Id,
    CharLit,
    CharSet,
    StrLit,
}

pub fn build_re() -> VecTree<ReNode> {
    let mut re = VecTree::new();
    let top = re.add(None, node!(|));
    re.addc_iter(Some(top), node!(&), [node!(str "->"), node!(=Id::Arrow as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str "\\"), node!(=Id::Backslash as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ':'       ), node!(=Id::Colon     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ','       ), node!(=Id::Comma     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(str ".."      ), node!(=Id::Ellipsis  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '{'       ), node!(=Id::Lbracket  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '['       ), node!(=Id::Lsbracket as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '('       ), node!(=Id::Lparen    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '-'       ), node!(=Id::Minus     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '~'       ), node!(=Id::Negate    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '+'       ), node!(=Id::Plus      as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '|'       ), node!(=Id::Or        as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '?'       ), node!(=Id::Question  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '"'       ), node!(=Id::Quote     as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '}'       ), node!(=Id::Rbracket  as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ']'       ), node!(=Id::Rsbracket as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ')'       ), node!(=Id::Rparen    as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr ';'       ), node!(=Id::Semicolon as TokenId)]);
    re.addc_iter(Some(top), node!(&), [node!(chr '\''      ), node!(=Id::Squote    as TokenId)]);
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
/*
    // LineComment: '//' ~[\r\n]*
    let line_comment = re.add(Some(top), node!(&));
    re.addc(Some(line_comment), node!(*), node!(~['\r', '\n']));
    re.add(Some(line_comment), node!(term!(skip)));
*/
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
    //                      or2                p     or1
    //
    fn add_char(re: &mut VecTree<ReNode>, parent: usize) {
        let or1 = re.add(Some(parent), node!(|));
        re.add(Some(or1), node!(chr '\\'));
        let or2 = re.add(Some(or1), node!(|));
        re.add(Some(or2), node!(['n', 'r', 't', '\'', '\\']));
        let cc = re.add(Some(or2), node!(&));
        re.add(Some(cc), node!(str "u{"));
        re.addc(Some(cc), node!(+), node!(['0'-'9', 'a'-'f', 'A'-'F']));
        re.add(Some(cc), node!(chr '}'));
        re.add(Some(or1), node!(~['\n', '\r', '\t', '\'', '\\']));
    }
    let char_lit = re.add(Some(top), node!(&));
    re.add(Some(char_lit), node!(chr '\''));
    add_char(&mut re, char_lit);
    re.add_iter(Some(char_lit), [node!(chr '\''), node!(=Id::CharLit as TokenId)]);

    // CharSet:                                                       |     |
    //          '[' (                         |                )* ']'   '.'   ('\\w' | '\\d')
    //               SetChar ( '-' SetChar )?   ('\\w' | '\\d')
    //                                       or2                s    or1   or1
    //                    or3----------------
    // SetChar: ( '\\' ([nrt\\[\]\-] | 'u{' [0-9a-fA-F]+ '}') | ~[\n\r\t\\] )
    fn add_set_char(re: &mut VecTree<ReNode>, parent: usize) {
        let or1 = re.add(Some(parent), node!(|));
        re.add(Some(or1), node!(chr '\\'));
        let or2 = re.add(Some(or1), node!(|));
        re.add(Some(or2), node!(['n', 'r', 't', '\'', '\\', '[', ']', '-']));
        let cc = re.add(Some(or2), node!(&));
        re.add(Some(cc), node!(str "u{"));
        re.addc(Some(cc), node!(+), node!(['0'-'9', 'a'-'f', 'A'-'F']));
        re.add(Some(cc), node!(chr '}'));
        re.add(Some(or1), node!(~['\n', '\r', '\t', '\\']));
    }
    let char_set = re.add(Some(top), node!(&));
    let or1 = re.add(Some(char_set), node!(|));
    let cc2 = re.add(Some(or1), node!(&));
    re.add(Some(cc2), node!(chr '['));
    let s = re.add(Some(cc2), node!(*));
    let or2 = re.add(Some(s), node!(|));
    let cc3 = re.add(Some(or2), node!(&));
    add_set_char(&mut re, cc3);
    let or3 = re.add(Some(cc3), node!(|)); // ('-' SetChar )? = ('-' SetChar | <empty>)
    let cc4 = re.add(Some(or3), node!(&));
    re.add(Some(cc4), node!(chr '-'));
    add_set_char(&mut re, cc4);
    re.add(Some(or3), node!(e));
    let or3 = re.add(Some(cc3), node!(|)); // FixedSet
    re.add_iter(Some(or3), [node!(str "\\w"), node!(str "\\d")]);
    re.add(Some(cc2), node!(chr ']'));
    re.add(Some(or1), node!(chr '.'));
    let or3 = re.add(Some(or1), node!(|)); // FixedSet
    re.add_iter(Some(or3), [node!(str "\\w"), node!(str "\\d")]);
    re.add(Some(char_set), node!(=Id::CharSet as TokenId));

    // StrLit: '"' Char Char+ '"'
    let str_lit = re.add(Some(top), node!(&));
    re.add(Some(str_lit), node!(chr '"'));
    add_char(&mut re, str_lit);
    let p = re.add(Some(str_lit), node!(+));
    add_char(&mut re, p);
    re.add_iter(Some(str_lit), [node!(chr '"'), node!(=Id::StrLit as TokenId)]);

    re
}

/*
|(
    &(&(1:['-'],2:['>']),3:<end:0>),
    &(4:['\\'],5:<end:1>),
    &(6:[':'],7:<end:2>),
    &(8:[','],9:<end:3>),
    &(&(10:['.'],11:['.']),12:<end:4>),
    &(13:['{'],14:<end:5>),
    &(15:['['],16:<end:6>),
    &(17:['('],18:<end:7>),
    &(19:['-'],20:<end:8>),
    &(21:['~'],22:<end:9>),
    &(23:['+'],24:<end:10>),
    &(25:['|'],26:<end:11>),
    &(27:['?'],28:<end:12>),
    &(29:['\"'],30:<end:13>),
    &(31:['}'],32:<end:14>),
    &(33:[']'],34:<end:15>),
    &(35:[')'],36:<end:16>),
    &(37:[';'],38:<end:17>),
    &(39:['\''],40:<end:18>),
    &(41:['*'],42:<end:19>),
    &(&(43:['c'],44:['h'],45:['a'],46:['n'],47:['n'],48:['e'],49:['l'],50:['s']),51:<end:20>),
    &(&(52:['f'],53:['r'],54:['a'],55:['g'],56:['m'],57:['e'],58:['n'],59:['t']),60:<end:21>),
    &(&(61:['g'],62:['r'],63:['a'],64:['m'],65:['m'],66:['a'],67:['r']),68:<end:22>),
    &(&(69:['l'],70:['e'],71:['x'],72:['e'],73:['r']),74:<end:23>),
    &(&(75:['m'],76:['o'],77:['d'],78:['e']),79:<end:24>),
    &(&(80:['p'],81:['o'],82:['p']),83:<end:25>),
    &(&(84:['p'],85:['u'],86:['s'],87:['h']),88:<end:26>)
    &(&(89:['r'],90:['e'],91:['t'],92:['u'],93:['r'],94:['n']),95:<end:27>),
    &(&(96:['s'],97:['k'],98:['i'],99:['p']),100:<end:28>),
    &(&(101:['E'],102:['O'],103:['F']),104:<end:29>),
    &(&(105:['/'],106:['*']),??(*(107:[DOT])),&(108:['*'],109:['/']),110:<skip>),
    &(*(111:[~ '\n', '\r']),112:<skip>),
    &(+(113:['\t'-'\n', '\r', ' ']),114:<skip>),
    &(115:['A'-'Z', 'a'-'z'],*(116:['0'-'9', 'A'-'Z', '_', 'a'-'z']),117:<end:33>),
    &(118:['\''],|(119:['\\'],|(120:['\'', '\\', 'n', 'r', 't'],&(&(121:['u'],122:['{']),+(123:['0'-'9', 'A'-'F', 'a'-'f']),124:['}'])),125:[~ '\t'-'\n', '\r', '\'', '\\']),126:['\''],127:<end:34>),
    &(|(&(128:['['],*(|(&(|(129:['\\'],|(130:['\'', '-', '['-']', 'n', 'r', 't'],&(&(131:['u'],132:['{']),+(133:['0'-'9', 'A'-'F', 'a'-'f']),134:['}'])),135:[~ '\t'-'\n', '\r', '\\']),|(&(136:['-'],|(137:['\\'],|(138:['\'', '-', '['-']', 'n', 'r', 't'],&(&(139:['u'],140:['{']),+(141:['0'-'9', 'A'-'F', 'a'-'f']),142:['}'])),143:[~ '\t'-'\n', '\r', '\\'])),144:-),|(&(145:['\\'],146:['w']),&(147:['\\'],148:['d']))))),149:[']']),150:['.'],|(&(151:['\\'],152:['w']),&(153:['\\'],154:['d']))),155:<end:35>),
    &(156:['\"'],|(157:['\\'],|(158:['\'', '\\', 'n', 'r', 't'],&(&(159:['u'],160:['{']),+(161:['0'-'9', 'A'-'F', 'a'-'f']),162:['}'])),163:[~ '\t'-'\n', '\r', '\'', '\\']),+(|(164:['\\'],|(165:['\'', '\\', 'n', 'r', 't'],&(&(166:['u'],167:['{']),+(168:['0'-'9', 'A'-'F', 'a'-'f']),169:['}'])),170:[~ '\t'-'\n', '\r', '\'', '\\'])),171:['\"'],172:<end:36>))
*/

// fragment BlockComment   : '/*' .*? '*/';
// fragment LineComment    : '//' ~[\r\n]*;
// fragment HexDigit       : [0-9a-fA-F];
// fragment UnicodeEsc     : 'u{' HexDigit+ '}';
// fragment EscChar        : BACKSLASH ([nrt'\\] | UnicodeEsc);
// fragment Char           : EscChar | ~[\n\r\t'\\];
// fragment CharLiteral    : SQUOTE Char SQUOTE;
// fragment StrLiteral     : SQUOTE Char Char+ SQUOTE;
// fragment FixedSet       : ('\\w' | '\\d');
// // Char inside a '[' ']' set
// fragment EscSetChar     : BACKSLASH ([nrt\\[\]\-] | UnicodeEsc);
// fragment SetChar        : EscSetChar | ~[\n\r\t\\];
//
// ARROW           : '->';
// BACKSLASH       : '\\';
// COLON           : ':';
// COMMA           : ',';
// ELLIPSIS        : '..';
// LBRACKET        : '{';
// LSBRACKET       : '[';
// LPAREN          : '(';
// MINUS           : '-';
// NEGATE          : '~';
// PLUS            : '+';
// OR              : '|';
// QUESTION        : '?';
// QUOTE           : '"';
// RBRACKET        : '}';
// RSBRACKET       : ']';
// RPAREN          : ')';
// SEMICOLON       : ';';
// SQUOTE          : '\'';
// STAR            : '*';
//
// CHANNELS        : 'channels';
// FRAGMENT        : 'fragment';
// GRAMMAR         : 'grammar';
// LEXER           : 'lexer';
// MODE            : 'mode';
// POP             : 'pop';
// PUSH            : 'push';
// RETURN          : 'return';
// SiKP            : 'skip';
// SYM_EOF         : 'EOF';
//
// COMMENT         : BlockComment              -> skip;
// LINECOMMENT     : LineComment               -> skip;
// WHITESPACE      : [ \n\r\t]+                -> skip;
//
// ID              : [a-zA-Z][a-zA-Z_0-9]*;
//
// CHAR_LIT        : CharLiteral;
// CHAR_SET        : '[' (SetChar '-' SetChar | SetChar | FixedSet)* ']'
//                 | '.'
//                 | FixedSet;
// STR_LIT         : StrLiteral;
