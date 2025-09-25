// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::io::{BufRead, BufReader, BufWriter, Read, Write, Seek};
use std::fs::{File, OpenOptions};
use crate::CollectJoin;

/// Reads the file `filename` and finds the part included between two tags `tag`.
/// Returns the text between the tags or `None` if they couldn't be found.
///
/// Each line is trimmed from any ending space characters and ends with `\n`.
pub fn get_tagged_source(filename: &str, tag: &str) -> Option<String> {
    let file_tag = format!("[{tag}]");
    let file = File::open(filename).ok()?;
    let mut found = false;
    let result = BufReader::new(file).lines()
        .filter_map(|l| l.ok())
        .skip_while(|l| !l.contains(&file_tag))
        .skip(2)
        .take_while(|l| !l.contains(&file_tag))
        .inspect(|_| found = true)
        .map(|mut s| {
            s.truncate(s.trim_end().len());
            s
        })
        .join("\n"); // the last line won't end by `\n`, which removes the last empty line
    if found { Some(result) } else { None }
}

/// Replaces the text between two tags `tag` by `new_src` in the file `filename`. Returns `Ok` on
/// success, or `Err` on failure, either I/O or if the tags couldn't be found.
pub fn replace_tagged_source(filename: &str, tag: &str, new_src: &str) -> std::io::Result<()> {
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
            Ok(n) => if n == 0 { return Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("tag {file_tag} not found"))); }
            Err(e) => return Err(e),
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
