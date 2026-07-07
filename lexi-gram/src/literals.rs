// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_lib::segments;
use lexigram_lib::segments::Segments;

/// Decodes a string literal (without its surrounding quotes). There must be at least two characters in `strlit`.
pub(crate) fn decode_str(strlit: &str) -> Result<String, String> {
    let mut result = String::new();
    let mut chars = strlit.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                result.push(match chars.next().ok_or(format!("'\\' incomplete escape code in string literal '{strlit}'"))? {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\'' => '\'',
                    '\\' => '\\',
                    'u' => {
                        if !matches!(chars.next(), Some('{')) { return Err(format!("malformed unicode literal in string literal '{strlit}' (missing '{{')")); }
                        let mut hex = String::new();
                        loop {
                            let Some(h) = chars.next() else { return Err(format!("malformed unicode literal in string literal '{strlit}' (missing '}}')")); };
                            if h == '}' { break; }
                            hex.push(h);
                        };
                        let code = u32::from_str_radix(&hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                        char::from_u32(code).ok_or_else(|| format!("'{hex}' isn't a valid unicode hexadecimal value"))?
                    }
                    unknown => return Err(format!("unknown escape code '\\{unknown}' in string literal '{strlit}'"))
                });
            }
            _ => result.push(c)
        }
    }
    Ok(result)
}

/// Decodes a single character literal (without its surrounding quotes). There must be exactly one character in `char`,
/// so this function assumes there's at least one byte but can handle malformed character literals.
pub fn decode_char(char: &str) -> Result<char, String> {
    // fragment Char        : EscChar | ~[\n\r\t'\\];
    // fragment EscChar     : '\\' ([nrt'\\] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    let mut chars = char.chars();
    let c = chars.next();
    if c == Some('\\') {
        match chars.next().ok_or("'\\' incomplete escape code in character literal".to_string())? {
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '\'' => Ok('\''),
            '\\' => Ok('\\'),
            'u' => {
                if !matches!(chars.next(), Some('{')) { return Err(format!("malformed unicode literal in string literal '{char}' (missing '{{')")); }
                let mut hex = String::new();
                loop {
                    let Some(h) = chars.next() else { return Err(format!("malformed unicode literal in string literal '{char}' (missing '}}')")); };
                    if h == '}' { break; }
                    hex.push(h);
                };
                let code = u32::from_str_radix(&hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                let u = char::from_u32(code).ok_or(format!("'{hex}' isn't a valid unicode hexadecimal value"))?;
                Ok(u)
            }
            _ => Err(format!("unknown escape code '{char}'")), // shouldn't happen
        }

    } else {
        c.ok_or(format!("'{char}' is not a valid character literal"))
    }
}

/// Decodes one character that is used inside `[` ... `]`. There must be exactly one character in `setchar`,
/// so this function assumes there's at least one byte but can handle malformed character literals.
pub fn decode_set_char(setchar: &str) -> Result<char, String> {
    // SET_CHAR             : (EscSetChar | ~[\n\r\t\\\]]);
    // fragment EscSetChar  : '\\' ([nrt\\[\]\-] | UnicodeEsc);
    // fragment UnicodeEsc  : 'u{' HexDigit+ '}';
    // fragment HexDigit    : [0-9a-fA-F];
    let bytes = setchar.as_bytes();
    if bytes[0] == b'\\' {
        match bytes.get(1).ok_or("'\\' incomplete escape code in set character literal".to_string())? {
            b'n' => Ok('\n'),
            b'r' => Ok('\r'),
            b't' => Ok('\t'),
            b'[' => Ok('['),
            b']' => Ok(']'),
            b'-' => Ok('-'),
            b'\\' => Ok('\\'),
            b'u' => {
                if bytes[2] != b'{' || !matches!(setchar.chars().last(), Some('}')) {
                    return Err(format!("malformed unicode literal '{setchar}'"));
                }
                let hex = &setchar[3..setchar.len() - 1];
                let code = u32::from_str_radix(hex, 16).map_err(|_| format!("'{hex}' isn't a valid hexadecimal value"))?;
                let u = char::from_u32(code).ok_or(format!("'{hex}' isn't a valid unicode hexadecimal value"))?;
                Ok(u)
            }
            _ => Err(format!("unknown escape code '{setchar}'")), // shouldn't happen
        }
    } else {
        setchar.chars().next().ok_or(format!("'{setchar}' is not a valid set character literal"))
    }
}

pub fn decode_fixed_set(fixedset: &str) -> Result<Segments, String> {
    // FIXED_SET       : ('\\w' | '\\d');
    let bytes = fixedset.as_bytes();
    if bytes[0] != b'\\' {
        Err(format!("unknown shorthand code '{fixedset}'")) // shouldn't happen
    } else {
        match fixedset.as_bytes()[1] {
            b'd' => Ok(segments!('0'-'9')),
            b'w' => Ok(segments!('0'-'9', '_', 'A'-'Z', 'a'-'z')),
            _ => Err(format!("unknown shorthand code '{fixedset}'")), // shouldn't happen
        }
    }
}