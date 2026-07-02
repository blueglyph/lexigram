// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [gramlexer]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::lexer::{ActionOption, Lexer, ModeOption, LexStateId, Terminal};
use lexigram_lib::segmap::{GroupId, Seg, SegMap};

const NBR_GROUPS: u32 = 33;
const INITIAL_STATE: LexStateId = 0;
const FIRST_END_STATE: LexStateId = 17;
const NBR_STATES: LexStateId = 45;
static ASCII_TO_GROUP: [GroupId; 128] = [
     23,  23,  23,  23,  23,  23,  23,  23,  23,   0,  29,  23,  23,  29,  23,  23,   // 0-15
     23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,  23,   // 16-31
      0,  23,  23,  23,  23,  23,  23,  23,   1,   2,   3,   4,  23,  23,  23,   5,   // 32-47
     21,  21,  21,  21,  21,  21,  21,  21,  21,  21,   6,   7,   8,  20,  19,   9,   // 48-63
     23,  27,  27,  27,  27,  11,  32,  10,  27,  27,  27,  27,  14,  27,  27,  28,   // 64-79
     15,  27,  16,  27,  27,  27,  27,  27,  27,  27,  27,  23,  23,  23,  23,  21,   // 80-95
     23,  30,  27,  27,  27,  18,  27,  12,  27,  27,  27,  27,  25,  31,  27,  24,   // 96-111
     27,  27,  17,  22,  27,  27,  26,  27,  27,  27,  27,  23,  13,  23,  23,  23,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 23),
    (Seg(57344, 1114111), 23),
];
static TERMINAL_TABLE: [Terminal;28] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [LexStateId; 1486] = [
     17,  18,  19,  20,  21,  22,  23,  24,   1,  25,  26,  27,  28,  29,  26,  26,  26,  26,  26,  45,  45,  45,  26,  45,  26,  26,  26,  26,  26,  17,  26,  26,  26, // state 0
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,   4,  45,  45,  45,   5,   6,   7,   8,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 1
      2,   2,   2,   3,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 2
      2,   2,   2,   3,   2,  31,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2, // state 3
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  43,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 4
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  40,   9,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 5
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  42,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 6
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  41,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 7
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  11,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 8
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  10,  10,  10,  45,  10,  10,  10,  10,  10,  45,  45,  45,  10,  45,  10,  10,  10,  10,  10,  45,  10,  10,  10, // state 9
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  10,  10,  10,  45,  10,  10,  10,  10,  10,  40,  45,  10,  10,  45,  10,  10,  10,  10,  10,  45,  10,  10,  10, // state 10
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  12,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 11
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  13,  45,  45,  45,  45,  45,  45,  45,  45, // state 12
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  14,  45,  45,  45,  45,  45,  45,  45, // state 13
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  16,  45,  45,  45,  45,  45,  45, // state 14
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  44,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 15
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  15,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 16
     17,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  17,  45,  45,  45, // state 17 <skip>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 18 <end:1>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 19 <end:5>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 20 <end:8>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 21 <end:3>
     45,  45,  45,   2,  45,  30,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 22 <end:7>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 23 <end:0>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 24 <end:6>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 25 <end:4>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  26,  26, // state 26 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  38,  45,  26,  26,  26, // state 27 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  32,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  26,  26, // state 28 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 29 <end:2>
     30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  45,  30,  30,  30, // state 30 <skip>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 31 <skip>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  33,  26,  26, // state 32 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  34,  26, // state 33 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  35,  26, // state 34 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  36,  26,  26, // state 35 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  37,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  26,  26, // state 36 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  26,  26, // state 37 <end:9>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  26,  39, // state 38 <end:16>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  26,  26,  26,  45,  26,  26,  26,  26,  26,  45,  45,  26,  26,  45,  26,  26,  26,  26,  26,  45,  26,  26,  26, // state 39 <end:10>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 40 <end:11>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 41 <end:12>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 42 <end:13>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 43 <end:14>
     45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45,  45, // state 44 <end:15>
     45 // error group in [nbr_state * nbr_group + nbr_group]
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
                (": ( | + ? ) ; * grammar EOF <L> <L=a> <R> <P> <G> <resolve> / a bc d_e1",
                 vec![0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 11, 12, 13, 14, 15, 7, 16, 16, 16],
                 vec![":", "(", "|", "+", "?", ")", ";", "*", "grammar", "EOF", "<L>", "<L=a>", "<R>", "<P>", "<G>", "<resolve>", "/", "a", "bc", "d_e1"]),
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