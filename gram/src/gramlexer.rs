// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

pub(super) mod gramlexer {
    // -------------------------------------------------------------------------
    // [gramlexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram::dfa::{StateId, Terminal, ActionOption, ModeOption};
    use lexigram::lexer::Lexer;
    use lexigram::lexergen::GroupId;
    use lexigram::segments::{Seg, SegMap};

    const NBR_GROUPS: u32 = 26;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 9;
    const NBR_STATES: StateId = 33;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         19,  19,  19,  19,  19,  19,  19,  19,  19,   0,  22,  19,  19,  22,  19,  19,   // 0-15
         19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,  19,   // 16-31
          0,  19,  19,  19,  19,  19,  19,  19,   1,   2,   3,   4,  19,  19,  19,   5,   // 32-47
         18,  18,  18,  18,  18,  18,  18,  18,  18,  18,   6,   7,   8,  16,  17,   9,   // 48-63
         19,  15,  15,  15,  15,  11,  25,  15,  15,  15,  15,  15,  10,  15,  15,  20,   // 64-79
         15,  15,  14,  15,  15,  15,  15,  15,  15,  15,  15,  19,  19,  19,  19,  18,   // 80-95
         19,  23,  15,  15,  15,  15,  15,  12,  15,  15,  15,  15,  15,  24,  15,  15,   // 96-111
         15,  15,  21,  15,  15,  15,  15,  15,  15,  15,  15,  19,  13,  19,  19,  19,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 19),
        (Seg(57344, 1114111), 19),
    ];
    static TERMINAL_TABLE: [Terminal;24] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 859] = [
          9,  10,  11,  12,  13,   1,  14,  15,   2,  16,  17,  18,  19,  20,  17,  17,  33,  33,  33,  33,  17,  17,   9,  17,  17,  17, // state 0
         33,  33,  33,   3,  33,  21,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 1
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,   5,  33,  33,  33,   6,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 2
          3,   3,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 3
          3,   3,   3,   4,   3,  22,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 4
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,   7,  31,  33,  33,  33,  33,  33,  33,  33,  33, // state 5
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  32,  33,  33,  33,  33,  33,  33,  33,  33, // state 6
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,   8,   8,   8,  33,   8,   8,  33,  33,  33,  33,   8,   8,  33,   8,   8,   8, // state 7
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,   8,   8,   8,  33,   8,   8,  33,  31,   8,  33,   8,   8,  33,   8,   8,   8, // state 8
          9,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,   9,  33,  33,  33, // state 9 <skip>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 10 <end:1>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 11 <end:5>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 12 <end:7>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 13 <end:3>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 14 <end:0>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 15 <end:6>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 16 <end:4>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  17,  17,  17, // state 17 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  29,  17,  33,  17,  17,  17, // state 18 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  23,  33,  17,  17,  17, // state 19 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 20 <end:2>
         21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  33,  21,  21,  21, // state 21 <skip>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 22 <skip>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  24,  17,  17, // state 23 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  17,  25,  17, // state 24 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  17,  26,  17, // state 25 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  27,  17,  17, // state 26 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  28,  33,  17,  17,  17, // state 27 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  17,  17,  17, // state 28 <end:8>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  17,  17,  30, // state 29 <end:12>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  17,  17,  17,  33,  17,  17,  33,  33,  17,  33,  17,  17,  33,  17,  17,  17, // state 30 <end:9>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 31 <end:10>
         33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33,  33, // state 32 <end:11>
         33 // error group in [nbr_state * nbr_group + nbr_group]
    ];

    pub fn build_lexer<R: Read>() -> Lexer<'static, R> {
        Lexer::new(
            // parameters
            NBR_GROUPS,
            INITIAL_STATE,
            FIRST_END_STATE,
            NBR_STATES,
            // tables
            &ASCII_TO_GROUP,
            HashMap::<char, GroupId>::from(UTF8_TO_GROUP),
            SegMap::<GroupId>::from(SEG_TO_GROUP),
            &STATE_TABLE,
            &TERMINAL_TABLE,
        )
    }

    // [gramlexer]
    // -------------------------------------------------------------------------
}

#[cfg(test)]
mod test {
    use std::io::Cursor;
    use lexigram::dfa::TokenId;
    use lexigram::escape_string;
    use lexigram::io::CharReader;
    use crate::gramlexer::gramlexer::build_lexer;

    #[test]
    pub fn check_lexer_tokens() {
        const VERBOSE: bool = false;
        let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
            // TODO
        ];
        let mut lexer = build_lexer();
        for (test_id, inputs) in tests {
            if VERBOSE { println!("test {test_id}:"); }
            for (input, expected_tokens, expected_texts) in inputs {
                //let expected_texts = expected_texts.iter().map(|s| s.escape_default());
                if VERBOSE { print!("\"{}\":", escape_string(input)); }
                let stream = CharReader::new(Cursor::new(input));
                lexer.attach_stream(stream);
                let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text, _col, _line)| {
                    assert_eq!(ch, 0, "test {} failed for input {}", test_id, escape_string(input));
                    (tok, text)
                }).unzip();
                let err_msg = format!("test {} failed for input '{}'", test_id, escape_string(input));
                assert!(!lexer.has_error() || lexer.is_eos(), "{err_msg}:\n{}", lexer.get_error());
                assert_eq!(tokens, expected_tokens, "{err_msg}");
                assert_eq!(texts, expected_texts, "{err_msg}");
            }
            if VERBOSE { println!("--------------------------------------\n"); }
        }
    }
}