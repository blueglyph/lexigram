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
    status: CharReaderStatus
}

impl<R: Read> CharReader<R> {
    pub fn new(source: R) -> Self {
        CharReader { reader: BufReader::new(source), offset: 0, status: CharReaderStatus::Reading }
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
        if let CharReaderStatus::Reading = self.creader.status {
            let mut buffer = [0; 4];
            let s = self.creader.reader.read(&mut buffer[0..=0]);
            match s {
                Ok(0) => {
                    self.creader.status = CharReaderStatus::Closed;
                    None
                }
                Ok(1) => {
                    let len = utf8_len(buffer[0]);
                    match len {
                        0 => {
                            self.creader.status = CharReaderStatus::Error(format!("UTF-8 encoding error at offset {}", self.creader.offset));
                            return None;
                        }
                        1 => {}
                        2..=4 => {
                            match self.creader.reader.read(&mut buffer[1..len]) {
                                Ok(n) => assert_eq!(n, len - 1),
                                Err(e) => panic!("{}", e)
                            }
                        }
                        _ => panic!("Unexpected UTF-8 length {} at offset {}", len, self.creader.offset),
                    }
                    self.creader.offset += len as u64;
                    let c = std::str::from_utf8(&buffer[..len]).unwrap()
                        .chars()
                        .next().unwrap();
                    Some(IterChar { char: c, offset: self.creader.offset })
                }
                Ok(n) => panic!("Unexpected Read::read() result: Ok({}) at offset {}", n, self.creader.offset),
                Err(e) => {
                    self.creader.status = CharReaderStatus::Error(e.to_string());
                    None
                }
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod char_reader {
    use std::io::{BufReader, Cursor, Read, Seek};
    use super::*;

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
        let tests = [
            ("012顠abc©345𠃐ab",          vec![1, 2, 3, 6, 7, 8, 9, 11, 12, 13, 14, 18, 19, 20]),
            ("1234567890123456789顠abc",  vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 22, 23, 24, 25]),
            ("",                          vec![]),
            ("1",                         vec![1]),
            ("12",                        vec![1, 2]),
            ("©",                         vec![2]),
        ];

        for (index, (text, expected_pos)) in tests.into_iter().enumerate() {
            let mut result = String::new();
            let mut result_pos = Vec::new();
            let mut reader = CharReader::new(Cursor::new(text));
            for c in reader.chars() {
                result.push(c.char);
                result_pos.push(c.offset);
            }
            assert_eq!(result, text, "test #{index}");
            assert_eq!(result_pos, expected_pos, "test #{index}");
            assert_eq!(reader.get_status(), &CharReaderStatus::Closed);
        }
    }

    #[test]
    fn read() {
        // let f = File::open("tests/read_test.txt").unwrap();
        // let mut reader = BufReader::new(f);
        let text = "1234567890123456789顠abc";
        let mut reader = BufReader::new(Cursor::new(text));
        let mut buffer = [0; 20];
        let mut pos = 0;
        let mut result = String::new();
        print!("{} {pos}:", reader.stream_position().unwrap());
        while let Ok(size) = reader.read(&mut buffer) {
            if size == 0 {
                break;
            }
            let (taken, s) = match std::str::from_utf8(&buffer[..size]) {
                Ok(s) => (size, &s[..size]),
                Err(e) => {
                    let taken = e.valid_up_to();
                    let s = unsafe { std::str::from_utf8_unchecked(&buffer[..taken]) };
                    print!("[error at byte {}]", pos + taken + 1);
                    if taken < size {
                        print!("[rewind {}]", size-taken);
                        let _ = reader.seek_relative(-((size - taken) as i64));
                    }
                    (taken, s)
                }
            };
            result.push_str(s);
            println!("|{s}|");
            pos += taken;
            print!("{} {pos}:", reader.stream_position().unwrap());
        }
        println!();
        println!("read {} bytes", pos);
        assert_eq!(text, result);
    }

    #[test]
    fn to_char() {
        let text = "012顠abc©345𠃐ab";
        let mut buffer = [0; 4];
        let str_reader = Cursor::new(text);
        let mut reader = BufReader::new(str_reader);
        let mut pos = 0;
        let mut result = String::new();
        loop {
            let s = reader.read(&mut buffer[0..=0]);
            if !matches!(s, Ok(1)) {
                break;
            }
            let len = utf8_len(buffer[0]);
            match len {
                0 => panic!("error at pos {pos}"),
                1 => {}
                2..=4 => {
                    match reader.read(&mut buffer[1..len]) {
                        Ok(n) => assert_eq!(n, len - 1),
                        Err(e) => panic!("{}", e)
                    }
                }
                _ => panic!("internal error")
            }
            let c = std::str::from_utf8(&buffer[..len]).unwrap()
                .chars()
                .next().unwrap();
            result.push(c);
            println!("{pos}:<{len}>{c}");
            pos += len;
        }
        assert_eq!(result, text);
    }
}
