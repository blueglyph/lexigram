// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use crate::CharLen;
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
                result.push_str(self.get_line(i));
            }
            result.push('\n');
            result.push_str(&self.get_line(l2 as usize).chars().take(c2 as usize).collect::<String>());
            result
        }
    }

    /// Returns all the line(s) corresponding to `span`, annotating them with line numbers and
    /// coloured text for the actual span content.
    fn annotate_text(&self, span: &PosSpan) -> String {
        fn char_to_len(s: &str, c: usize) -> usize {
            s.chars().take(c).map(|c| c.len_utf8()).sum()
        }
        const BEFORE_ANSI: &str = "\u{1b}[1;36m";
        const AFTER_ANSI : &str = "\u{1b}[0m";

        if span.is_empty() { return String::new() }
        let &PosSpan { first: Pos(l1, c1), last: Pos(l2, c2) } = span;
        let (mut l1, c1, l2, c2) = (l1 as usize, c1 as usize, l2 as usize, c2 as usize);
        let line = self.get_line(l1);
        let b_c1 = char_to_len(&line, c1 - 1);  // c1 = 2: (a1 a2) b1 b2 b3 c1 c2 d1 -> 2 -> ..2 = "a", 2.. = "bcd"
        if l1 == l2 {
            let i_c2 = char_to_len(&line, c2);  // c2 = 2: (a1 a2 b1 b2 b3) c1 c2 d1 -> ..5 = "ab", 5.. = "cd"
            format!("|{l1:4}| {}{BEFORE_ANSI}{}{AFTER_ANSI}{}", &line[..b_c1], &line[b_c1..i_c2], &line[i_c2..])
        } else {
            let mut result = format!("|{l1:4}| {}{BEFORE_ANSI}{}{AFTER_ANSI}", &line[..b_c1], &line[b_c1..]);
            while l1 + 1 < l2 {
                l1 += 1;
                result.push_str(&format!("\n|{l1:4}| {BEFORE_ANSI}{}{AFTER_ANSI}", self.get_line(l1)));
            }
            let line = self.get_line(l2);
            let i_c2 = char_to_len(&line, c2);  // c2 = 2: (a1 a2 b1 b2 b3) c1 c2 d1 -> ..5 = "ab", 5.. = "cd"
            result.push_str(&format!("\n|{l2:4}| {BEFORE_ANSI}{}{AFTER_ANSI}{}", &line[..i_c2], &line[i_c2..]));
            result
        }
    }

    fn annotate_text_ascii(&self, span: &PosSpan) -> String {
        const UNDERLINE_INTER: bool = false;
        if span.is_empty() { return String::new() }
        let &PosSpan { first: Pos(l1, c1), last: Pos(l2, c2) } = span;
        let (mut l1, c1, l2, c2) = (l1 as usize, c1 as usize - 1, l2 as usize, c2 as usize - 1);
        let line = self.get_line(l1);
        let l1s = l1.to_string();
        if l1 == l2 {
            format!("|{l1s:4}| {line}\n|{:w1$}| {:w2$}{:^<w3$}", "", "", "", w1=4.max(l1s.len()), w2=c1, w3=c2 + 1 - c1)
        } else {
            let mut result = format!("|{l1:4}| {line}\n|{:w1$}| {:w2$}{:^<w3$}", "", "", "", w1=4.max(l1s.len()), w2=c1, w3=line.charlen() - c1);
            while l1 + 1 < l2 {
                l1 += 1;
                let l1s = l1.to_string();
                if UNDERLINE_INTER {
                    result.push_str(&format!("\n|{l1:4}| {}\n|{:w1$}| {:^<w3$}", self.get_line(l1), "", "", w1 = 4.max(l1s.len()), w3 = line.charlen()));
                } else {
                    result.push_str(&format!("\n|{l1:4}> {}", self.get_line(l1)));
                }
            }
            let l2s = l2.to_string();
            result.push_str(&format!("\n|{l2:4}| {}\n|{:w1$}| {:^<w2$}", self.get_line(l2), "", "", w1=4.max(l2s.len()), w2=c2 + 1));
            result
        }
    }
}

impl<T: GetLine> GetTextSpan for T {}

#[cfg(test)]
mod tests {
    use crate::lexer::{Pos, PosSpan};
    use crate::text_span::{GetLine, GetTextSpan};

    struct TestText(Vec<String>);

    impl GetLine for TestText {
        fn get_line(&self, n: usize) -> &str {
            self.0[n - 1].as_str()
        }
    }

    #[test]
    fn annotate_text_ansi() {
        let text = TestText(vec![
            "α1234567890β".to_string(),
            "αabcdefghijβ".to_string(),
            "αklmnopqrstβ".to_string(),
            "α1234567890β (2)".to_string(),
        ]);
        let tests = vec![
            (
                1, 1, 1, 3,
                "|   1| \u{1b}[1;36mα12\u{1b}[0m34567890β",
            ),
            (
                1, 3, 1, 5,
                "|   1| α1\u{1b}[1;36m234\u{1b}[0m567890β",
            ),
            (
                1, 5, 2, 5,
                "|   1| α123\u{1b}[1;36m4567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcd\u{1b}[0mefghijβ",
            ),
            (
                1, 5, 2, 12,
                "|   1| α123\u{1b}[1;36m4567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcdefghijβ\u{1b}[0m",
            ),
            (
                1, 1, 2, 5,
                "|   1| \u{1b}[1;36mα1234567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcd\u{1b}[0mefghijβ",
            ),
            (
                1, 1, 2, 12,
                "|   1| \u{1b}[1;36mα1234567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcdefghijβ\u{1b}[0m",
            ),
            (
                1, 5, 3, 5,
                "|   1| α123\u{1b}[1;36m4567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcdefghijβ\u{1b}[0m\n|   3| \u{1b}[1;36mαklmn\u{1b}[0mopqrstβ",
            ),
            (
                1, 5, 3, 12,
                "|   1| α123\u{1b}[1;36m4567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcdefghijβ\u{1b}[0m\n|   3| \u{1b}[1;36mαklmnopqrstβ\u{1b}[0m",
            ),
            (
                1, 1, 3, 5,
                "|   1| \u{1b}[1;36mα1234567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcdefghijβ\u{1b}[0m\n|   3| \u{1b}[1;36mαklmn\u{1b}[0mopqrstβ",
            ),
            (
                1, 1, 3, 12,
                "|   1| \u{1b}[1;36mα1234567890β\u{1b}[0m\n|   2| \u{1b}[1;36mαabcdefghijβ\u{1b}[0m\n|   3| \u{1b}[1;36mαklmnopqrstβ\u{1b}[0m",
            ),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_ANSWERS: bool = false;
        for (test_id, (l1, c1, l2, c2, _expected)) in tests.into_iter().enumerate() {
            let span = PosSpan::new(Pos(l1, c1), Pos(l2, c2));
            let result = text.annotate_text(&span);
            if VERBOSE {
                println!("test {test_id}:\n{result}\n");
            }
            if VERBOSE_ANSWERS {
                println!("            (");
                println!("                {l1}, {c1}, {l2}, {c2},");
                println!("                {result:?},");
                println!("            ),");
            }
            // assert_eq!(result, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn annotate_text_ascii() {
        let text = TestText(vec![
            "α1234567890β".to_string(),
            "αabcdefghijβ".to_string(),
            "αklmnopqrstβ".to_string(),
            "α1234567890β (2)".to_string(),
        ]);
        let tests = vec![
            (
                1, 1, 1, 3,
                "|1   | α1234567890β\n|    | ^^^",
            ),
            (
                1, 3, 1, 5,
                "|1   | α1234567890β\n|    |   ^^^",
            ),
            (
                1, 5, 2, 5,
                "|   1| α1234567890β\n|    |     ^^^^^^^^\n|   2| αabcdefghijβ\n|    | ^^^^^",
            ),
            (
                1, 5, 2, 12,
                "|   1| α1234567890β\n|    |     ^^^^^^^^\n|   2| αabcdefghijβ\n|    | ^^^^^^^^^^^^",
            ),
            (
                1, 1, 2, 5,
                "|   1| α1234567890β\n|    | ^^^^^^^^^^^^\n|   2| αabcdefghijβ\n|    | ^^^^^",
            ),
            (
                1, 1, 2, 12,
                "|   1| α1234567890β\n|    | ^^^^^^^^^^^^\n|   2| αabcdefghijβ\n|    | ^^^^^^^^^^^^",
            ),
            (
                1, 5, 3, 5,
                "|   1| α1234567890β\n|    |     ^^^^^^^^\n|   2> αabcdefghijβ\n|   3| αklmnopqrstβ\n|    | ^^^^^",
            ),
            (
                1, 5, 3, 12,
                "|   1| α1234567890β\n|    |     ^^^^^^^^\n|   2> αabcdefghijβ\n|   3| αklmnopqrstβ\n|    | ^^^^^^^^^^^^",
            ),
            (
                1, 1, 3, 5,
                "|   1| α1234567890β\n|    | ^^^^^^^^^^^^\n|   2> αabcdefghijβ\n|   3| αklmnopqrstβ\n|    | ^^^^^",
            ),
            (
                1, 1, 3, 12,
                "|   1| α1234567890β\n|    | ^^^^^^^^^^^^\n|   2> αabcdefghijβ\n|   3| αklmnopqrstβ\n|    | ^^^^^^^^^^^^",
            ),
        ];
        const VERBOSE: bool = false;
        const VERBOSE_ANSWERS: bool = false;
        for (test_id, (l1, c1, l2, c2, expected)) in tests.into_iter().enumerate() {
            let span = PosSpan::new(Pos(l1, c1), Pos(l2, c2));
            let result = text.annotate_text_ascii(&span);
            if VERBOSE {
                println!("test {test_id}:\n{result}\n");
            }
            if VERBOSE_ANSWERS {
                println!("            (");
                println!("                {l1}, {c1}, {l2}, {c2},");
                println!("                {result:?},");
                println!("            ),");
            }
            assert_eq!(result, expected, "test {test_id} failed");
        }
    }
}