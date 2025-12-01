// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [gramlexer]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::dfa::{StateId, Terminal, ActionOption, ModeOption};
use lexigram_lib::lexer::Lexer;
use lexigram_lib::lexergen::GroupId;
use lexigram_lib::segments::{Seg, SegMap};

const NBR_GROUPS: u32 = 27;
const INITIAL_STATE: StateId = 0;
const FIRST_END_STATE: StateId = 10;
const NBR_STATES: StateId = 35;
static ASCII_TO_GROUP: [GroupId; 128] = [
     20,  20,  20,  20,  20,  20,  20,  20,  20,   0,  23,  20,  20,  23,  20,  20,   // 0-15
     20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   // 16-31
      0,  20,  20,  20,  20,  20,  20,  20,   1,   2,   3,   4,  20,  20,  20,   5,   // 32-47
     19,  19,  19,  19,  19,  19,  19,  19,  19,  19,   6,   7,   8,  17,  18,   9,   // 48-63
     20,  16,  16,  16,  16,  11,  26,  16,  16,  16,  16,  16,  10,  16,  16,  21,   // 64-79
     14,  16,  15,  16,  16,  16,  16,  16,  16,  16,  16,  20,  20,  20,  20,  19,   // 80-95
     20,  24,  16,  16,  16,  16,  16,  12,  16,  16,  16,  16,  16,  25,  16,  16,   // 96-111
     16,  16,  22,  16,  16,  16,  16,  16,  16,  16,  16,  20,  13,  20,  20,  20,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 20),
    (Seg(57344, 1114111), 20),
];
static TERMINAL_TABLE: [Terminal;25] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [StateId; 946] = [
     10,  11,  12,  13,  14,   1,  15,  16,   2,  17,  18,  19,  20,  21,  18,  18,  18,  35,  35,  35,  35,  18,  18,  10,  18,  18,  18, // state 0
     35,  35,  35,   3,  35,  22,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 1
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,   5,  35,  35,  35,   6,   7,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 2
      3,   3,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 3
      3,   3,   3,   4,   3,  23,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 4
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,   8,  32,  35,  35,  35,  35,  35,  35,  35,  35, // state 5
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  34,  35,  35,  35,  35,  35,  35,  35,  35, // state 6
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  33,  35,  35,  35,  35,  35,  35,  35,  35, // state 7
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,   9,   9,   9,  35,   9,   9,   9,  35,  35,  35,  35,   9,   9,  35,   9,   9,   9, // state 8
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,   9,   9,   9,  35,   9,   9,   9,  35,  32,   9,  35,   9,   9,  35,   9,   9,   9, // state 9
     10,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  10,  35,  35,  35, // state 10 <skip>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 11 <end:1>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 12 <end:5>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 13 <end:7>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 14 <end:3>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 15 <end:0>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 16 <end:6>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 17 <end:4>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  18,  18,  18, // state 18 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  30,  18,  35,  18,  18,  18, // state 19 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  24,  35,  18,  18,  18, // state 20 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 21 <end:2>
     22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,  35,  22,  22,  22, // state 22 <skip>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 23 <skip>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  25,  18,  18, // state 24 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  18,  26,  18, // state 25 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  18,  27,  18, // state 26 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  28,  18,  18, // state 27 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  29,  35,  18,  18,  18, // state 28 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  18,  18,  18, // state 29 <end:8>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  18,  18,  31, // state 30 <end:13>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  18,  18,  18,  35,  18,  18,  18,  35,  35,  18,  35,  18,  18,  35,  18,  18,  18, // state 31 <end:9>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 32 <end:10>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 33 <end:11>
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35, // state 34 <end:12>
     35 // error group in [nbr_state * nbr_group + nbr_group]
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

#[cfg(test)]
mod test {
    use std::io::Cursor;
    use lexigram_lib::dfa::TokenId;
    use lexigram_lib::escape_string;
    use lexigram_lib::char_reader::CharReader;
    use crate::gram::gramlexer::build_lexer;

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
                let (tokens, texts): (Vec<TokenId>, Vec<String>) = lexer.tokens().map(|(tok, ch, text, _pos_span)| {
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