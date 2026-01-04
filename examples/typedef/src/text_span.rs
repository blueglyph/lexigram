// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_core::lexer::{Pos, PosSpan};

pub trait GetLine {
    fn get_line(&self, n: usize) -> &str;
}

pub trait GetTextSpan: GetLine {
    fn get_text_span(&self, span: &PosSpan) -> String {
        if span.is_empty() { return String::new() }
        let &PosSpan { first: Pos(l1, c1), last: Pos(l2, c2) } = span;
        if l1 == l2 {
            self.get_line(l1 as usize - 1).chars().skip(c1 as usize - 1).take((c2 - c1) as usize + 1).collect()
        } else {
            let mut result = self.get_line(l1 as usize - 1).chars().skip(c1 as usize - 1).collect::<String>();
            for i in (l1 as usize)..(l2 as usize) {
                result.push('\n');
                result.push_str(&self.get_line(i));
            }
            result.push('\n');
            result.push_str(&self.get_line(l2 as usize - 1).chars().take(c2 as usize).collect::<String>());
            result
        }
    }
}

impl<T: GetLine> GetTextSpan for T {}
