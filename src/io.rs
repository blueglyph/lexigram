use std::io::{BufReader, Read};

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

/// Determines the number of bytes required to encode a UTF-8 character from its first byte.
pub fn utf8_len(b: u8) -> usize {
    return UTF8_LENGTH[b as usize] as usize;
}

#[derive(Debug, PartialEq)]
pub enum CharReaderStatus {
    Reading,
    Error(String),
    Closed
}

pub struct CharReader<R> {
    reader: BufReader<R>,
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
                Ok(0) => (None, 0, CharReaderStatus::Closed),
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
                                Err(e) => panic!("{}", e)
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
    pub char: char,
    pub offset: u64
}

impl<'a, R: Read> Iterator for CharReaderIter<'a, R> {
    type Item = IterChar;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.creader.get_char();
        c.map(|c| IterChar { char: c, offset: self.creader.offset })
    }
}

#[cfg(test)]
mod char_reader {
    use std::io::{BufReader, Cursor, Read, Seek};
    use super::*;

    fn get_tests() -> Vec::<(&'static str, Vec<u64>)> {
        vec![
            ("012顠abc©345𠃐ab",          vec![1, 2, 3, 6, 7, 8, 9, 11, 12, 13, 14, 18, 19, 20]),
            ("1234567890123456789顠abc",  vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 22, 23, 24, 25]),
            ("",                          vec![]),
            ("1",                         vec![1]),
            ("12",                        vec![1, 2]),
            ("©",                         vec![2]),
            ("𠃐𠃐",                      vec![4, 8])
        ]
    }

    #[test]
    fn utf8_length() {
        fn utf8_exp_len(byte: u8) -> usize {
            match byte {
                0x00..=0x7f => 1,
                0xc2..=0xdf => 2,
                0xe0..=0xef => 3,
                0xf0..=0xf4 => 4,
                _ => 0
            }
        }
        for i in 0_u8..128 {
            assert_eq!(utf8_len(i), utf8_exp_len(i), "length of {i} (0x{i:x}) differs");
        }
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
                let mut iter = reader.chars();
                let mut i = 0;
                if early_peek {
                    result_peek.push(iter.creader.peek());
                }
                while let Some(c) = iter.next() {
                    if i & 1 == 1 {
                        result_peek.push(iter.creader.peek());
                    }
                    result.push(c.char);
                    result_pos.push(c.offset);
                    i += 1;
                }
                let expected_peek = if early_peek {
                    text.chars().map(|c| Some(c)).chain([None])
                        .enumerate()
                        .filter_map(|(i, c)| if i & 1 == 0 { Some(c) } else { None })
                        .collect::<Vec<_>>()
                } else {
                    text.chars().map(|c| Some(c)).chain([None])
                        .skip(1)// no initial peek
                        .enumerate()
                        .filter_map(|(i, c)| if i & 1 == 1 { Some(c) } else { None })
                        .collect::<Vec<_>>()
                };
                let error = format!("test #{index} for early_peek={early_peek}");
                assert_eq!(result, *text, "{error}");
                assert_eq!(result_pos, *expected_pos, "{error}");
                assert_eq!(reader.get_status(), &CharReaderStatus::Closed, "{error}");
                assert_eq!(result_peek, expected_peek, "{error}");
            }
        }
    }
}
