#![allow(unused)]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
use lexigram_lib::segmap::{GroupId, Seg, SegMap};

const NBR_GROUPS: u32 = 23;
const INITIAL_STATE: StateId = 0;
const FIRST_END_STATE: StateId = 5;
const NBR_STATES: StateId = 28;
static ASCII_TO_GROUP: [GroupId; 128] = [
     16,  16,  16,  16,  16,  16,  16,  16,  16,   0,  19,  16,  16,  19,  16,  16,   // 0-15
     16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,   // 16-31
      0,  16,  16,  16,  16,  16,  16,  16,   1,   2,   3,   4,  16,   5,  16,   6,   // 32-47
     15,   7,   7,   7,   7,   7,   7,   7,   7,   7,  16,   8,   9,  10,  11,  16,   // 48-63
     16,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,   // 64-79
     12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  16,  16,  16,  16,  15,   // 80-95
     16,  12,  12,  12,  12,  17,  12,  12,  12,  21,  12,  12,  13,  12,  22,  12,   // 96-111
     14,  12,  18,  12,  20,  12,  12,  12,  12,  12,  12,  16,  16,  16,  16,  16,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 16),
    (Seg(57344, 1114111), 16),
];
static TERMINAL_TABLE: [Terminal;23] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [StateId; 645] = [
      5,   6,   7,   8,   9,  10,  11,  12,  13,   1,  14,   2,  15,  16,  17,  28,  28,  15,  15,   5,  15,  15,  15, // state 0
     28,  28,  28,  28,  28,  28,  28,  28,  28,  19,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 1
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  20,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 2
      3,   3,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 3
      3,   3,   3,   4,   3,   3,  27,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 4
      5,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,   5,  28,  28,  28, // state 5 <skip>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 6 <end:4>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 7 <end:5>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 8 <end:3>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 9 <end:0>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 10 <end:9>
     28,  28,  28,   3,  28,  28,  18,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 11 <end:1>
     28,  28,  28,  28,  28,  28,  28,  12,  28,  28,  28,  28,  28,  28,  28,  12,  28,  28,  28,  28,  28,  28,  28, // state 12 <end:13>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 13 <end:6>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 14 <end:2>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  15, // state 15 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  21,  15,  28,  15,  15,  15, // state 16 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  23,  28,  15,  15,  15, // state 17 <end:12>
     18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  18,  28,  18,  18,  18, // state 18 <skip>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 19 <end:7>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 20 <end:8>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  22,  15,  15, // state 21 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  15, // state 22 <end:10>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  24,  15, // state 23 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  25, // state 24 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  26,  15,  15, // state 25 <end:12>
     28,  28,  28,  28,  28,  28,  28,  15,  28,  28,  28,  28,  15,  15,  15,  15,  28,  15,  15,  28,  15,  15,  15, // state 26 <end:11>
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28, // state 27 <skip>
     28 // error group in [nbr_state * nbr_group + nbr_group]
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
