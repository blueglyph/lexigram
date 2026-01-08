// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use crate::lexer::{Pos, PosSpan};

pub trait GetLine {
    /// Gets line number n, where n = 1 for the first line.
    fn get_line(&self, n: usize) -> &str;
}

pub trait GetTextSpan: GetLine {
    /// Extracts the text corresponding to `span`
    fn extract_text(&self, span: &PosSpan) -> String {
        if span.is_empty() { return String::new() }
        let &PosSpan { first: Pos(l1, c1), last: Pos(l2, c2) } = span;
        if l1 == l2 {
            self.get_line(l1 as usize).chars().skip(c1 as usize - 1).take((c2 - c1) as usize + 1).collect()
        } else {
            let mut result = self.get_line(l1 as usize).chars().skip(c1 as usize - 1).collect::<String>();
            for i in (l1 as usize) + 1..(l2 as usize) {
                result.push('\n');
                result.push_str(&self.get_line(i));
            }
            result.push('\n');
            result.push_str(&self.get_line(l2 as usize).chars().take(c2 as usize).collect::<String>());
            result
        }
    }

    /// Returns all the line(s) corresponding to `span`, annotating them with line numbers and
    /// coloured text for the actual span content.
    fn annotate_text(&self, span: &PosSpan) -> String {
        const BEFORE_ANSI: &str = "\u{1b}[1;36m";
        const AFTER_ANSI : &str = "\u{1b}[0m";

        if span.is_empty() { return String::new() }
        let &PosSpan { first: Pos(l1, c1), last: Pos(l2, c2) } = span;
        let (mut l1, c1, l2, c2) = (l1 as usize, c1 as usize - 1, l2 as usize, c2 as usize - 1);
        let line = self.get_line(l1);
        if l1 == l2 {
            format!("{l1:4}: {}{BEFORE_ANSI}{}{AFTER_ANSI}{}", &line[..c1], &line[c1..=c2], &line[c2 + 1..])
        } else {
            let mut result = format!("{l1:4}: {}{BEFORE_ANSI}{}{AFTER_ANSI}", &line[..c1], &line[c1..]);
            while l1 + 1 < l2 {
                l1 += 1;
                result.push_str(&format!("\n{l1:4}: {BEFORE_ANSI}{}{AFTER_ANSI}", self.get_line(l1)));
            }
            let line = self.get_line(l2);
            result.push_str(&format!("\n{l2:4}: {BEFORE_ANSI}{}{AFTER_ANSI}{}", &line[..=c2], &line[c2 + 1..]));
            result
        }
    }
}

impl<T: GetLine> GetTextSpan for T {}
