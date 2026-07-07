// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [gramlexer]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::lexer::{ActionOption, Lexer, ModeOption, LexStateId, Terminal};
use lexigram_lib::segmap::{GroupId, Seg, SegMap};

const NBR_GROUPS: u32 = 42;
const INITIAL_STATE: LexStateId = 0;
const FIRST_END_STATE: LexStateId = 23;
const NBR_STATES: LexStateId = 52;
static ASCII_TO_GROUP: [GroupId; 128] = [
     32,  32,  32,  32,  32,  32,  32,  32,  32,  15,  38,  32,  32,  38,  32,  32,   // 0-15
     32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,   // 16-31
      0,  32,  32,  32,  32,  32,  32,   1,   2,   3,   4,   5,  32,  32,  32,   6,   // 32-47
     25,  25,  25,  25,  25,  25,  25,  25,  25,  25,   7,   8,   9,  29,  28,  10,   // 48-63
     32,  31,  31,  31,  31,  12,  41,  11,  36,  36,  36,  36,  18,  36,  36,  37,   // 64-79
     19,  36,  20,  36,  36,  36,  36,  36,  36,  36,  36,  32,  17,  32,  32,  30,   // 80-95
     32,  39,  31,  31,  31,  24,  31,  13,  36,  36,  36,  36,  34,  40,  22,  33,   // 96-111
     36,  36,  21,  26,  22,  23,  35,  36,  36,  36,  36,  16,  14,  27,  32,  32,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 32),
    (Seg(57344, 1114111), 32),
];
static TERMINAL_TABLE: [Terminal;29] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [LexStateId; 2185] = [
     23,   1,  24,  25,  26,  27,  28,  29,  30,   2,  31,  32,  33,  34,  35,  23,  52,  52,  32,  32,  32,  32,  32,  32,  32,  52,  32,  52,  52,  52,  52,  32,  52,  32,  32,  32,  32,  32,  23,  32,  32,  32, // state 0
      5,  52,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  52,   5,   6,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  52,   5,   5,   5, // state 1
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  10,  52,  52,  52,  52,  52,  52,  11,  12,  13,  14,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 2
      3,   3,   3,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 3
      3,   3,   3,   3,   4,   3,  37,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, // state 4
      5,  38,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  52,   5,   6,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,  52,   5,   5,   5, // state 5
     52,   5,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,   5,  52,  52,  52,   5,   5,   7,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 6
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,   8,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 7
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,   9,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,   9,   9,  52,  52,  52,  52,  52,   9,  52,  52,  52,  52,  52,  52,  52,   9,  52,   9, // state 8
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,   9,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,   9,   9,  52,   5,  52,  52,  52,   9,  52,  52,  52,  52,  52,  52,  52,   9,  52,   9, // state 9
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  50,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 10
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  47,  15,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 11
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  49,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 12
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  48,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 13
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  17,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 14
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  16,  16,  16,  52,  52,  52,  52,  16,  16,  16,  16,  16,  16,  16,  52,  16,  52,  52,  52,  52,  16,  52,  16,  16,  16,  16,  16,  52,  16,  16,  16, // state 15
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  16,  16,  16,  52,  52,  52,  52,  16,  16,  16,  16,  16,  16,  16,  16,  16,  52,  47,  52,  16,  16,  52,  16,  16,  16,  16,  16,  52,  16,  16,  16, // state 16
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  18,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 17
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  19,  52,  52,  52,  52,  52,  52,  52,  52, // state 18
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  20,  52,  52,  52,  52,  52,  52,  52, // state 19
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  22,  52,  52,  52,  52,  52,  52, // state 20
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  51,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 21
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  21,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 22
     23,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  23,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  23,  52,  52,  52, // state 23 <skip>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 24 <end:1>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 25 <end:5>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 26 <end:8>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 27 <end:3>
     52,  52,  52,  52,   3,  52,  36,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 28 <end:7>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 29 <end:0>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 30 <end:6>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 31 <end:4>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  32,  32, // state 32 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  45,  52,  32,  32,  32, // state 33 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  39,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  32,  32, // state 34 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 35 <end:2>
     36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  36,  52,  36,  36,  36, // state 36 <skip>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 37 <skip>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 38 <end:9>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  40,  32,  32, // state 39 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  41,  32, // state 40 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  42,  32, // state 41 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  43,  32,  32, // state 42 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  44,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  32,  32, // state 43 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  32,  32, // state 44 <end:10>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  32,  46, // state 45 <end:17>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  32,  32,  32,  52,  52,  52,  52,  32,  32,  32,  32,  32,  32,  32,  32,  32,  52,  52,  52,  32,  32,  52,  32,  32,  32,  32,  32,  52,  32,  32,  32, // state 46 <end:11>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 47 <end:12>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 48 <end:13>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 49 <end:14>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 50 <end:15>
     52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, // state 51 <end:16>
     52 // error group in [nbr_state * nbr_group + nbr_group]
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
                (r#": ( | + ? ) ; / * '*' '**' grammar EOF <L> <L=a> <R> <P> <G> <resolve> a bc d_e1"#,
                 vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 11, 12, 12, 13, 14, 15, 16, 17, 17, 17],
                 vec![":", "(", "|", "+", "?", ")", ";", "/", "*", "'*'", "'**'", "grammar", "EOF", "<L>", "<L=a>", "<R>", "<P>", "<G>", "<resolve>", "a", "bc", "d_e1"]),
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