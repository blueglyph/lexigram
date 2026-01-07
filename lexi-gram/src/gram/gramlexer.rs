// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [gramlexer]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
use lexigram_lib::segmap::{GroupId, Seg, SegMap};

const NBR_GROUPS: u32 = 28;
const INITIAL_STATE: StateId = 0;
const FIRST_END_STATE: StateId = 11;
const NBR_STATES: StateId = 37;
static ASCII_TO_GROUP: [GroupId; 128] = [
     21,  21,  21,  21,  21,  21,  21,  21,  21,   0,  24,  21,  21,  24,  21,  21,   // 0-15
     21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,   // 16-31
      0,  21,  21,  21,  21,  21,  21,  21,   1,   2,   3,   4,  21,  21,  21,   5,   // 32-47
     20,  20,  20,  20,  20,  20,  20,  20,  20,  20,   6,   7,   8,  19,  18,   9,   // 48-63
     21,  17,  17,  17,  17,  11,  27,  10,  17,  17,  17,  17,  14,  17,  17,  22,   // 64-79
     15,  17,  16,  17,  17,  17,  17,  17,  17,  17,  17,  21,  21,  21,  21,  20,   // 80-95
     21,  25,  17,  17,  17,  17,  17,  12,  17,  17,  17,  17,  17,  26,  17,  17,   // 96-111
     17,  17,  23,  17,  17,  17,  17,  17,  17,  17,  17,  21,  13,  21,  21,  21,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 21),
    (Seg(57344, 1114111), 21),
];
static TERMINAL_TABLE: [Terminal;26] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [StateId; 1037] = [
     11,  12,  13,  14,  15,   1,  16,  17,   2,  18,  19,  20,  21,  22,  19,  19,  19,  19,  37,  37,  37,  37,  19,  19,  11,  19,  19,  19, // state 0
     37,  37,  37,   3,  37,  23,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 1
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,   5,  37,  37,  37,   6,   7,   8,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 2
      3,   3,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 3
      3,   3,   3,   4,   3,  24,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 4
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  36,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 5
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  33,   9,  37,  37,  37,  37,  37,  37,  37,  37, // state 6
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  35,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 7
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  34,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 8
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  10,  10,  10,  37,  10,  10,  10,  10,  37,  37,  37,  37,  10,  10,  37,  10,  10,  10, // state 9
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  10,  10,  10,  37,  10,  10,  10,  10,  33,  37,  10,  37,  10,  10,  37,  10,  10,  10, // state 10
     11,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  11,  37,  37,  37, // state 11 <skip>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 12 <end:1>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 13 <end:5>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 14 <end:7>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 15 <end:3>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 16 <end:0>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 17 <end:6>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 18 <end:4>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  19,  19,  19, // state 19 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  31,  19,  37,  19,  19,  19, // state 20 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  25,  37,  19,  19,  19, // state 21 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 22 <end:2>
     23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  37,  23,  23,  23, // state 23 <skip>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 24 <skip>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  26,  19,  19, // state 25 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  19,  27,  19, // state 26 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  19,  28,  19, // state 27 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  29,  19,  19, // state 28 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  30,  37,  19,  19,  19, // state 29 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  19,  19,  19, // state 30 <end:8>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  19,  19,  32, // state 31 <end:14>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  19,  19,  19,  37,  19,  19,  19,  19,  37,  37,  19,  37,  19,  19,  37,  19,  19,  19, // state 32 <end:9>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 33 <end:10>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 34 <end:11>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 35 <end:12>
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37, // state 36 <end:13>
     37 // error group in [nbr_state * nbr_group + nbr_group]
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
    use lexigram_lib::TokenId;
    use lexigram_lib::char_reader::{CharReader, escape_string};
    use crate::gram::gramlexer::build_lexer;

    #[test]
    pub fn check_lexer_tokens() {
        const VERBOSE: bool = false;
        let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
            (1, vec![
                // no error
                (": ( | + ? ) ; * grammar EOF <L> <L=a> <R> <P> <G> a bc d_e1",
                 vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 11, 12, 13, 14, 14, 14],
                 vec![":", "(", "|", "+", "?", ")", ";", "*", "grammar", "EOF", "<L>", "<L=a>", "<R>", "<P>", "<G>", "a", "bc", "d_e1"]),
            ]),
        ];
        let mut lexer = build_lexer();
        for (test_id, inputs) in tests {
            if VERBOSE { println!("test {test_id}:"); }
            for (input, expected_tokens, expected_texts) in inputs {
                //let expected_texts = expected_texts.iter().map(|s| s.escape_default());
                if VERBOSE { print!("\"{}\":", escape_string(input)); }
                let stream = CharReader::new(input.as_bytes());
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