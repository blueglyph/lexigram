// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::{BufReader, Read};

// Note on UTF-8 encoding
//
//                         |     (hexa)    |                 UTF-8                  |                UTF-16
//     Codepoint Value     |   min    max  | 1st byte  2nd byte  3rd byte  4th byte |     1st word           2nd word
// ------------------------+---------------+----------------------------------------+--------------------------------------
//       00000000_0xxxxxxx |   0000   007F | 0xxxxxxx                               | 00000000_0xxxxxxx
//                         |               |                                        |
//       00000yyy_yyxxxxxx |   0080   07FF | 110yyyyy  10xxxxxx                     | 00000yyy_yyxxxxxx
//                         |               |                                        |
//       zzzzyyyy_yyxxxxxx |   0800   FFFF | 1110zzzz  10yyyyyy  10xxxxxx           | zzzzyyyy_yyxxxxxx
//                         |               |                                        |
// uuuuu_zzzzyyyy_yyxxxxxx | 010000 10FFFF | 11110uuu  10uuzzzz  10yyyyyy  10xxxxxx | 110110ww_wwzzzzyy  110111yy_yyxxxxxx
// (uuuuu: max 10000)      |               |                                        | (wwww = uuuuu-1)
//
// Valid codepoint values:
//
// 000000 - 00007f: 1 byte
// 000080 - 0007ff: 2 bytes
// 000800 - 00d7ff: 3 bytes
// --------------------------
// 00d800 - 00dfff: forbidden
// --------------------------
// 00e000 - 00ffff: 3 bytes
// 010000 - 10ffff: 4 bytes

pub const UTF8_MIN: u32      =        0;
pub const UTF8_LOW_MAX: u32  =   0xd7ff;
pub const UTF8_GAP_MIN: u32  =   0xd800;
pub const UTF8_GAP_MAX: u32  =   0xdfff;
pub const UTF8_HIGH_MIN: u32 =   0xe000;
pub const UTF8_MAX: u32      = 0x10ffff;

const UTF8_LENGTH: [u8; 256] = [
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,
];

// Note: both utf8_len* functions below may be exchanged to avoid using a table
// in production code.

#[inline]
/// Determines the number of bytes required to encode a UTF-8 character from its first byte.
pub fn utf8_len(byte: u8) -> usize {
    return UTF8_LENGTH[byte as usize] as usize;
}

#[inline]
/// Determines the number of bytes required to encode a UTF-8 character from its first byte.
pub fn utf8_len_notable(byte: u8) -> usize {
    match byte {
        0x00..=0x7f => 1,
        0xc2..=0xdf => 2,
        0xe0..=0xef => 3,
        0xf0..=0xf4 => 4,
        _ => 0
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum CharReaderStatus {
    #[default]
    Reading,
    Error(String),
    Closed
}

#[derive(Debug)]
pub enum CharReaderError {
    NoRoomToRewind
}

pub struct CharReader<R> {
    reader: BufReader<R>,
    /// offset of next character, in bytes
    offset: u64,
    status: CharReaderStatus,
    peek: Option<(Option<char>, u64, CharReaderStatus)>,
}

impl<R: Read> CharReader<R> {
    pub fn new(source: R) -> Self {
        CharReader {
            reader: BufReader::new(source),
            offset: 0,
            status: CharReaderStatus::Reading,
            peek: None,
        }
    }

    pub fn is_reading(&self) -> bool {
        matches!(self.status, CharReaderStatus::Reading)
    }

    pub fn get_offset(&self) -> u64 {
        self.offset
    }

    pub fn get_status(&self) -> &CharReaderStatus {
        &self.status
    }
    
    pub fn chars(&mut self) -> CharReaderIter<'_, R> {
        CharReaderIter { creader: self }
    }

    pub fn get_char(&mut self) -> Option<char> {
        if let Some(peek) = std::mem::take(&mut self.peek) {
            self.offset = peek.1;
            self.status = peek.2;
            peek.0
        } else {
            let (c, len, status) = self.read_char();
            self.offset += len as u64;
            self.status = status;
            c
        }
    }

    pub fn rewind(&mut self, chr: char) -> Result<(), CharReaderError> {
        if self.peek.is_none() {
            let new_offset = self.offset - chr.len_utf8() as u64;
            self.peek = Some((Some(chr), self.offset, std::mem::take(&mut self.status)));
            self.offset = new_offset;
            self.status = CharReaderStatus::Reading;
            Ok(())
        } else {
            Err(CharReaderError::NoRoomToRewind)
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        if let Some(peek) = &self.peek {
            peek.0
        } else {
            let (c, len, status) = self.read_char();
            self.peek = Some((c, self.offset + len as u64, status));
            c
        }
    }
    
    fn read_char(&mut self) -> (Option<char>, usize, CharReaderStatus) {
        if let CharReaderStatus::Reading = self.status {
            let mut buffer = [0; 4];
            let s = self.reader.read(&mut buffer[0..=0]);
            match s {
                Ok(0) => (None, 0, CharReaderStatus::Closed), // TODO: take 'live', expandable sources into account with an option
                Ok(1) => {
                    let len = utf8_len(buffer[0]);
                    match len {
                        0 => {
                            return (None, 0, CharReaderStatus::Error(format!("UTF-8 encoding error at offset {}", self.offset)));
                        }
                        1 => {}
                        2..=4 => {
                            match self.reader.read(&mut buffer[1..len]) {
                                Ok(n) => assert_eq!(n, len - 1),
                                Err(e) => return (None, 0, CharReaderStatus::Error(e.to_string())),
                            }
                        }
                        _ => panic!("Unexpected UTF-8 length {} at offset {}", len, self.offset),
                    }
                    let c = std::str::from_utf8(&buffer[..len]).unwrap()
                        .chars()
                        .next().unwrap();
                    (Some(c), len, CharReaderStatus::Reading)
                }
                Ok(n) => panic!("Unexpected Read::read() result: Ok({}) at offset {}", n, self.offset),
                Err(e) => {
                    (None, 0, CharReaderStatus::Error(e.to_string()))
                }
            }
        } else {
            (None, 0, CharReaderStatus::Closed)
        }
    }
}

pub struct CharReaderIter<'a, R> {
    creader: &'a mut CharReader<R>
}

pub struct IterChar {
    /// next character from the stream
    pub char: char,
    /// offset of `char` in the stream, in bytes
    pub offset: u64
}

impl<'a, R: Read> Iterator for CharReaderIter<'a, R> {
    type Item = IterChar;

    fn next(&mut self) -> Option<Self::Item> {
        let offset = self.creader.offset;
        let c = self.creader.get_char();
        c.map(|c| IterChar { char: c, offset })
    }
}

// ---------------------------------------------------------------------------------------------
// Macros

pub mod macros {
    /// Replaces a few identifiers by their codepoint value, and casts character / integer literals to `u32`.
    #[macro_export]
    macro_rules! utf8 {
        ( MIN )        => { 0_u32 };
        ( LOW_MAX )    => { 0xd7ff_u32 };
        ( GAP_MIN )    => { 0xd800_u32 };
        ( GAP_MAX )    => { 0xdfff_u32 };
        ( HIGH_MIN )   => { 0xe000_u32 };
        ( MAX )        => { 0x10ffff_u32 };
        ( $a:literal ) => { $a as u32 }
    }
}

// ---------------------------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod char_reader {
    use std::io::Cursor;
    use crate::CollectJoin;
    use crate::char_reader::escape_char;
    use super::*;

    fn get_tests() -> Vec::<(&'static str, Vec<u64>)> {
        vec![
            ("012顠abc©345𠃐ab",          vec![0, 1, 2, 3, 6, 7, 8, 9, 11, 12, 13, 14, 18, 19]),
            ("1234567890123456789顠abc",  vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 22, 23, 24]),
            ("",                          vec![]),
            ("1",                         vec![0]),
            ("12",                        vec![0, 1]),
            ("©",                         vec![0]),
            ("𠃐𠃐",                      vec![0, 4])
        ]
    }

    #[test]
    fn utf8_length() {
        for i in 0_u8..128 {
            assert_eq!(utf8_len(i), utf8_len_notable(i), "length of {i} (0x{i:x}) differs");
        }
    }

    #[test]
    fn read_rewind() {
        let text = "aαbβgΔs∑z";
        let mut reader = CharReader::new(Cursor::new(text));
        assert!(reader.is_reading());
        let mut counter = 0;
        while reader.is_reading() {
            counter += 1;
            let c = reader.get_char().unwrap_or('!');
            if c == '!' {
                assert_eq!(reader.status, CharReaderStatus::Closed);
            }
            let reader_offset = reader.offset;
            let reader_status = reader.status.clone();
            // rewinding
            assert!(reader.peek.is_none());
            reader.rewind(c).expect("rewind should be fine");
            assert!(reader.peek.is_some());
            if let Some((pc, po, ps)) = &reader.peek {
                assert_eq!(pc, &Some(c), "failed rewinding '{}'", escape_char(c));
                assert_eq!(po, &reader_offset, "failed rewinding '{}'", escape_char(c));
                assert_eq!(ps, &reader_status, "failed rewinding '{}'", escape_char(c));
            }
            // forward again
            let c_again = reader.get_char();
            assert!(reader.peek.is_none(), "failed reading after rewind for '{}'", escape_char(c));
            assert_eq!(c_again, Some(c), "failed reading after rewind for '{}'", escape_char(c));
            assert_eq!(&reader.offset, &reader_offset, "failed reading after rewind for '{}'", escape_char(c));
            assert_eq!(&reader.status, &reader_status, "failed reading after rewind for '{}'", escape_char(c));
        }
        assert_eq!(counter, text.chars().count() + 1);
        assert_eq!(reader.status, CharReaderStatus::Closed);
        assert_eq!(reader.get_char(), None);
    }


    #[test]
    fn char_iterator() {
        let tests = get_tests();
        for (index, (text, expected_pos)) in tests.iter().enumerate() {
            let mut result = String::new();
            let mut result_pos = Vec::new();
            let mut reader = CharReader::new(Cursor::new(text));
            for c in reader.chars() {
                result.push(c.char);
                result_pos.push(c.offset);
            }
            assert_eq!(result, *text, "test #{index}");
            assert_eq!(result_pos, *expected_pos, "test #{index}");
            assert_eq!(reader.get_status(), &CharReaderStatus::Closed);
        }
    }

    #[test]
    fn char_iterator_peek() {
        for early_peek in [false, true] {
            let tests = get_tests();
            for (index, (text, expected_pos)) in tests.iter().enumerate() {
                let mut result = String::new();
                let mut result_pos = Vec::new();
                let mut reader = CharReader::new(Cursor::new(text));
                let mut result_peek = Vec::new();
                let mut i = 0;
                if early_peek {
                    result_peek.push(reader.peek());
                }
                while let (offset, Some(c)) = (reader.get_offset(), reader.get_char()) {
                    if i & 1 == 1 {
                        result_peek.push(reader.peek());
                    }
                    result.push(c);
                    result_pos.push(offset);
                    i += 1;
                }
                let expected_peek = if early_peek {
                    text.chars().map(|c| Some(c)).chain([None])
                        .enumerate()
                        .filter_map(|(i, c)| if i & 1 == 0 { Some(c) } else { None })
                        .to_vec()
                } else {
                    text.chars().map(|c| Some(c)).chain([None])
                        .skip(1)// no initial peek
                        .enumerate()
                        .filter_map(|(i, c)| if i & 1 == 1 { Some(c) } else { None })
                        .to_vec()
                };
                let error = format!("test #{index} for early_peek={early_peek}");
                assert_eq!(result, *text, "{error}");
                assert_eq!(result_pos, *expected_pos, "{error}");
                assert_eq!(reader.get_status(), &CharReaderStatus::Closed, "{error}");
                assert_eq!(result_peek, expected_peek, "{error}");
            }
        }
    }

    #[test]
    fn partial_iterations() {
        let tests = get_tests();
        for (index, (text, _)) in tests.into_iter().enumerate() {
            let mut reader = CharReader::new(Cursor::new(text));
            let length = text.chars().count();
            let mut result = reader.chars().take(length/2).map(|it| it.char).collect::<String>();
            while let Some(c) = reader.get_char() {
                result.push(c);
            }
            assert_eq!(result, text, "test #{index}");
        }
    }
}

pub fn escape_char(c: char) -> String {
    match c {
        // '\x00'..='\x7f' => c.escape_debug().to_string(),
              '\u{0}' => "MIN".to_string(),
           '\u{d7ff}' => "LOW_MAX".to_string(),
           '\u{e000}' => "HIGH_MIN".to_string(),
         '\u{10ffff}' => "MAX".to_string(),
        // '\u{f7ff}' | '\u{e000}' | '\u{10ffff}' => c.escape_unicode().to_string(),
        _ => c.escape_debug().to_string(),
    }
}

pub fn escape_string(s: &str) -> String {
    s.chars().map(|c| escape_char(c)).collect::<String>()
}
