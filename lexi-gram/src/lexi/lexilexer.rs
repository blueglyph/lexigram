// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// -------------------------------------------------------------------------
// [lexilexer]

use std::collections::HashMap;
use std::io::Read;
use lexigram_lib::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
use lexigram_lib::segmap::{GroupId, Seg, SegMap};

const NBR_GROUPS: u32 = 49;
const INITIAL_STATE: StateId = 0;
const FIRST_END_STATE: StateId = 21;
const NBR_STATES: StateId = 96;
static ASCII_TO_GROUP: [GroupId; 128] = [
     38,  38,  38,  38,  38,  38,  38,  38,  38,  28,  48,  38,  38,  48,  38,  38,   // 0-15
     38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,   // 16-31
      0,  38,  38,  38,  38,  38,  38,   1,   2,   3,   4,   5,   6,   7,   8,   9,   // 32-47
     29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  10,  11,  38,  38,  34,  12,   // 48-63
     38,  32,  32,  32,  32,  32,  32,  35,  35,  35,  35,  35,  35,  35,  35,  35,   // 64-79
     35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  14,  15,  36,  38,  37,   // 80-95
     38,  44,  32,  16,  13,  41,  17,  45,  18,  47,  35,  42,  19,  20,  30,  40,   // 96-111
     21,  35,  39,  22,  23,  31,  35,  33,  46,  43,  35,  24,  25,  26,  27,  38,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 38),
    (Seg(57344, 1114111), 38),
];
static TERMINAL_TABLE: [Terminal;75] = [
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(31), channel: 0, mode: ModeOption::Push(1), mode_state: Some(12), pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(33), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(32), channel: 0, mode: ModeOption::None, mode_state: None, pop: true },
    Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [StateId; 4705] = [
     21,   1,  22,  23,  24,  25,  26,  27,  28,   2,  29,  30,  31,  32,  33,   3,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  21,  96,  32,  32,  32,  32,  96,  32,  96,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  21, // state 0
      6,  96,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   7,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  96,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  96, // state 1
     96,  96,  96,  96,   4,  96,  96,  96,  96,  87,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 2
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  91,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  91,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 3
      4,   4,   4,   4,   5,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4, // state 4
      4,   4,   4,   4,   5,   4,   4,   4,   4,  88,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4, // state 5
     11,  89,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  15,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  96,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  96, // state 6
     96,   6,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,   6,  96,  96,  96,  96,  96,  96,  96,   6,  96,  96,  96,  96,  96,  96,   6,   8,  96,  96,  96,  96,  96,  96,  96,   6,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 7
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,   9,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 8
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  10,  96,  96,  10,  10,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  10,  96,  96,  10,  96,  96,  96,  96,  96,  96,  96,  96,  10,  96,  96,  10,  96,  96,  96,  96, // state 9
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  10,  96,  96,  10,  10,  96,  96,  96,  96,  96,  96,  96,  96,   6,  96,  96,  10,  96,  96,  10,  96,  96,  96,  96,  96,  96,  96,  96,  10,  96,  96,  10,  96,  96,  96,  96, // state 10
     11,  90,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  15,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  96,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  96, // state 11
     92,  92,  92,  92,  92,  92,  92,  93,  92,  92,  92,  92,  92,  92,  92,  13,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  96,  92,  92,  92,  92,  92,  92,  92,  94,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  96, // state 12
     96,  96,  96,  96,  96,  96,  96,  92,  96,  96,  96,  96,  96,  95,  92,  92,  96,  96,  96,  96,  96,  96,  96,  92,  96,  96,  96,  96,  96,  96,  92,  18,  96,  95,  96,  96,  92,  96,  96,  92,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 13
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  14,  96,  96,  14,  14,  96,  96,  96,  96,  96,  96,  96,  96,  92,  96,  96,  14,  96,  96,  14,  96,  96,  96,  96,  96,  96,  96,  96,  14,  96,  96,  14,  96,  96,  96,  96, // state 14
     96,  11,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  11,  96,  96,  96,  96,  96,  96,  96,  11,  96,  96,  96,  96,  96,  96,  11,  20,  96,  96,  96,  96,  96,  96,  96,  11,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 15
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  14,  96,  96,  14,  14,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  14,  96,  96,  14,  96,  96,  96,  96,  96,  96,  96,  96,  14,  96,  96,  14,  96,  96,  96,  96, // state 16
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  17,  96,  96,  17,  17,  96,  96,  96,  96,  96,  96,  96,  96,  11,  96,  96,  17,  96,  96,  17,  96,  96,  96,  96,  96,  96,  96,  96,  17,  96,  96,  17,  96,  96,  96,  96, // state 17
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  16,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 18
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  17,  96,  96,  17,  17,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  17,  96,  96,  17,  96,  96,  96,  96,  96,  96,  96,  96,  17,  96,  96,  17,  96,  96,  96,  96, // state 19
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  19,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 20
     21,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  21,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  21, // state 21 <skip>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 22 <end:6>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 23 <end:13>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 24 <end:15>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 25 <end:9>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 26 <end:2>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  46,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 27 <end:8>
     96,  96,  96,  96,  96,  96,  96,  96,  47,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 28 <end:3>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 29 <end:1>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 30 <end:14>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 31 <end:11>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 32 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 33 <end:31,push(1,state 12)>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  48,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 34 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  55,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 35 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  84,  32,  32,  32,  32,  32,  32,  32,  96, // state 36 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  62,  32,  32,  32,  32,  32,  32,  96, // state 37 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  68,  32,  32,  32,  32,  32,  32,  32,  96, // state 38 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  73,  32,  32,  96,  32,  96,  32,  96,  32,  72,  32,  32,  32,  32,  32,  32,  32,  96, // state 39 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  78,  32,  32,  32,  32,  32,  96, // state 40 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  81,  32,  32,  32,  32,  96, // state 41 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 42 <end:5>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 43 <end:10>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 44 <end:12>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 45 <end:7>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 46 <end:0>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 47 <end:4>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  49,  32,  32,  32,  96, // state 48 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  50,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 49 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  51,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 50 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  52,  32,  32,  32,  32,  32,  32,  96, // state 51 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  53,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 52 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  54,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 53 <end:25>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 54 <end:16>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  56,  32,  32,  32,  96, // state 55 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  57,  32,  32,  96, // state 56 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  58,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 57 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  59,  32,  32,  32,  32,  32,  32,  96, // state 58 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  60,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 59 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  61,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 60 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 61 <end:17>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  63,  32,  96, // state 62 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  64,  96, // state 63 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  65,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 64 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  66,  32,  32,  32,  32,  32,  32,  32,  96, // state 65 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  67,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 66 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 67 <end:18>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  69,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  70,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 68 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  71,  32,  32,  32,  32,  32,  32,  96, // state 69 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  77,  32,  32,  32,  32,  32,  32,  96, // state 70 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 71 <end:19>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  74,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 72 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  75,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 73 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 74 <end:20>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  76,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 75 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 76 <end:21>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 77 <end:22>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  79,  96, // state 78 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  80,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 79 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 80 <end:23>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  82,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 81 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  83,  32,  32,  32,  32,  32,  32,  96, // state 82 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 83 <end:24>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  85,  32,  32,  32,  32,  32,  32,  32,  96, // state 84 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  86,  32,  32,  32,  32,  32,  96, // state 85 <end:27>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  32,  96,  96,  32,  32,  32,  32,  32,  32,  32,  32,  96,  96,  96,  96,  96,  32,  32,  32,  32,  32,  96,  32,  96,  32,  96,  32,  32,  32,  32,  32,  32,  32,  32,  32,  96, // state 86 <end:26>
     87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  96, // state 87 <skip>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 88 <skip>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 89 <end:28>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 90 <end:29>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 91 <end:30>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 92 <end:33>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 93 <end:8>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 94 <end:32,pop>
     96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96, // state 95 <end:30>
     96 // error group in [nbr_state * nbr_group + nbr_group]
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

// [lexilexer]
// -------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use std::io::Cursor;
    use lexigram_lib::TokenId;
    use lexigram_lib::char_reader::{CharReader, escape_string};
    use crate::lexi::lexilexer::build_lexer;

    #[test]
    pub fn check_lexer_tokens() {
        const VERBOSE: bool = false;
        let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
            (1, vec![
                // no error
                ("-> : , . .. { ( ~ - + | ? } ) ; * channels fragment lexicon mode pop push more skip type channel hook \\w[a-z.\\t\\w]",
                 vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 30, 31, 33, 8, 33, 33, 33, 30, 32],
                 vec!["->", ":", ",", ".", "..", "{", "(", "~", "-", "+", "|", "?", "}", ")", ";", "*",
                      "channels", "fragment", "lexicon", "mode", "pop", "push", "more", "skip", "type", "channel", "hook", "\\w", "[", "a", "-", "z", ".", "\\t", "\\w", "]"]),
            ]),
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