// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, Write};
use std::fs::{File, OpenOptions};
use lexigram_core::CollectJoin;

#[derive(Debug)]
pub enum SrcTagError {
    Io(std::io::Error),
    NoTag,
    NoClosingTag,
}

impl From<std::io::Error> for SrcTagError {
    fn from(err: std::io::Error) -> Self {
        SrcTagError::Io(err)
    }
}

impl Display for SrcTagError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcTagError::Io(e) => e.fmt(f),
            SrcTagError::NoTag => write!(f, "opening tag not found"),
            SrcTagError::NoClosingTag => write!(f, "closing tag not found"),
        }
    }
}

impl Error for SrcTagError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            SrcTagError::Io(e) => Some(e),
            SrcTagError::NoTag => None,
            SrcTagError::NoClosingTag => None,
        }
    }
}

/// Reads the file `filename` and finds the part included between two tags `tag`.
/// Returns the text between the tags or `None` if they couldn't be found.
///
/// Each line is trimmed from any ending space characters and ends with `\n`.
pub fn get_tagged_source(filename: &str, tag: &str) -> Result<String, SrcTagError> {
    let file_tag = format!("[{tag}]");
    let file = File::open(filename)?;
    let mut opening_tag_found = false;
    let mut closing_tag_found = false;
    let result = BufReader::new(file).lines()
        .map_while(Result::ok)
        .skip_while(|l| !l.contains(&file_tag))
        .inspect(|_| opening_tag_found = true)
        .skip(2)
        .take_while(|l| !l.contains(&file_tag))
        .inspect(|_| closing_tag_found = true)
        .map(|mut s| {
            s.truncate(s.trim_end().len());
            s
        })
        .join("\n"); // the last line won't end by `\n`, which removes the last empty line
    if closing_tag_found {
        Ok(result)
    } else if opening_tag_found {
        Err(SrcTagError::NoClosingTag)
    } else {
        Err(SrcTagError::NoTag)
    }
}

/// Replaces the text between two tags `tag` by `new_src` in the file `filename`. Returns `Ok` on
/// success, or `Err` on failure, either I/O or if the tags couldn't be found.
pub fn replace_tagged_source(filename: &str, tag: &str, new_src: &str) -> Result<(), SrcTagError> {
    let file_tag = format!("[{tag}]");
    let file = File::open(filename)?;
    let mut buf = BufReader::new(file);
    let mut count = 0;
    let mut line = String::new();
    let mut after = String::new();
    let mut position = 0;
    loop {
        line.clear();
        match buf.read_line(&mut line) {
            Ok(n) => if n == 0 {
                return if count == 0 { Err(SrcTagError::NoTag) } else { Err(SrcTagError::NoClosingTag) };
            }
            Err(e) => return Err(SrcTagError::Io(e)),
        }
        if line.contains(&file_tag) {
            count += 1;
            match count {
                1 => {
                    position = buf.stream_position()?;
                }
                2 => {
                    after.push_str(&line);
                    buf.read_to_string(&mut after)?;
                    break;
                }
                _ => panic!()
            }
        }
    }
    let file = OpenOptions::new().write(true).open(filename)?;
    file.set_len(position)?;
    let mut buf = BufWriter::new(file);
    buf.seek(std::io::SeekFrom::End(0))?;
    write!(&mut buf, "\n{new_src}\n{after}")?;
    Ok(())
}

/// Result of a line-by-line comparison of two strings. Reports
/// either that everything was equal or the first line mismatch.
pub enum DiffResult {
    /// The two strings are equal line by line (EOL may differ in nature: Linux vs Windows)
    Equal,
    /// The two strings differ, starting at line `line_num` (differing lines included)
    Mismatch { line_num: usize, line1: String, line2: String }
}

/// Compares two strings line by line.
pub fn simple_diff(text1: &str, text2: &str) -> DiffResult {
    for (line_num, (line1, line2)) in text1.lines().zip(text2.lines()).enumerate() {
        if line1 != line2 {
            return DiffResult::Mismatch { line_num, line1: line1.to_string(), line2: line2.to_string() }
        }
    }
    DiffResult::Equal
}

/// Expands to the file name in which it was invoked, like [file!](file!()),
/// but transforms Windows path separators to `/`, so that they match on a
/// Linux system (useful when generated text is verified in unit tests).
#[macro_export]
macro_rules! filename {
    () => { file!().replace("\\", "/") };
}
