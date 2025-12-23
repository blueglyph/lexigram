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
const NBR_STATES: StateId = 92;
static ASCII_TO_GROUP: [GroupId; 128] = [
     37,  37,  37,  37,  37,  37,  37,  37,  37,  27,  48,  37,  37,  48,  37,  37,   // 0-15
     37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,   // 16-31
      0,  37,  37,  37,  37,  37,  37,   1,   2,   3,   4,   5,   6,   7,   8,   9,   // 32-47
     28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  10,  11,  37,  37,  33,  12,   // 48-63
     37,  31,  31,  31,  31,  31,  31,  34,  34,  34,  34,  34,  34,  34,  34,  34,   // 64-79
     34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  14,  15,  35,  37,  36,   // 80-95
     37,  44,  31,  16,  13,  40,  17,  45,  38,  47,  34,  42,  18,  19,  29,  41,   // 96-111
     20,  34,  39,  21,  22,  30,  34,  32,  46,  43,  34,  23,  24,  25,  26,  37,   // 112-127
];
static UTF8_TO_GROUP: [(char, GroupId); 0] = [
];
static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
    (Seg(128, 55295), 37),
    (Seg(57344, 1114111), 37),
];
static TERMINAL_TABLE: [Terminal;71] = [
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
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::Push(1), mode_state: Some(12), pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(32), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    Terminal { action: ActionOption::Token(31), channel: 0, mode: ModeOption::None, mode_state: None, pop: true },
    Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
];
static STATE_TABLE: [StateId; 4509] = [
     21,   1,  22,  23,  24,  25,  26,  27,  28,   2,  29,  30,  31,  32,  33,   3,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  21,  92,  32,  32,  32,  32,  92,  32,  92,  92,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  21, // state 0
      6,  92,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   7,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  92,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  92, // state 1
     92,  92,  92,  92,   4,  92,  92,  92,  92,  83,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 2
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  87,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  87,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 3
      4,   4,   4,   4,   5,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4, // state 4
      4,   4,   4,   4,   5,   4,   4,   4,   4,  84,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4, // state 5
     11,  85,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  15,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  92,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  92, // state 6
     92,   6,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,   6,  92,  92,  92,  92,  92,  92,   6,  92,  92,  92,  92,  92,  92,   6,   8,  92,  92,  92,  92,  92,  92,  92,  92,   6,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 7
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,   9,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 8
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  10,  92,  92,  10,  10,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  10,  92,  92,  10,  92,  92,  92,  92,  92,  92,  92,  92,  10,  92,  92,  92,  10,  92,  92,  92,  92, // state 9
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  10,  92,  92,  10,  10,  92,  92,  92,  92,  92,  92,  92,   6,  92,  92,  10,  92,  92,  10,  92,  92,  92,  92,  92,  92,  92,  92,  10,  92,  92,  92,  10,  92,  92,  92,  92, // state 10
     11,  86,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  15,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  92,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  92, // state 11
     88,  88,  88,  88,  88,  88,  88,  89,  88,  88,  88,  88,  88,  88,  88,  13,  88,  88,  88,  88,  88,  88,  88,  88,  88,  88,  88,  92,  88,  88,  88,  88,  88,  88,  88,  90,  88,  88,  88,  88,  88,  88,  88,  88,  88,  88,  88,  88,  92, // state 12
     92,  92,  92,  92,  92,  92,  92,  88,  92,  92,  92,  92,  92,  91,  88,  88,  92,  92,  92,  92,  92,  92,  88,  92,  92,  92,  92,  92,  92,  88,  18,  92,  91,  92,  92,  88,  92,  92,  92,  88,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 13
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  14,  92,  92,  14,  14,  92,  92,  92,  92,  92,  92,  92,  88,  92,  92,  14,  92,  92,  14,  92,  92,  92,  92,  92,  92,  92,  92,  14,  92,  92,  92,  14,  92,  92,  92,  92, // state 14
     92,  11,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  11,  92,  92,  92,  92,  92,  92,  11,  92,  92,  92,  92,  92,  92,  11,  20,  92,  92,  92,  92,  92,  92,  92,  92,  11,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 15
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  14,  92,  92,  14,  14,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  14,  92,  92,  14,  92,  92,  92,  92,  92,  92,  92,  92,  14,  92,  92,  92,  14,  92,  92,  92,  92, // state 16
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  17,  92,  92,  17,  17,  92,  92,  92,  92,  92,  92,  92,  11,  92,  92,  17,  92,  92,  17,  92,  92,  92,  92,  92,  92,  92,  92,  17,  92,  92,  92,  17,  92,  92,  92,  92, // state 17
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  16,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 18
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  17,  92,  92,  17,  17,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  17,  92,  92,  17,  92,  92,  92,  92,  92,  92,  92,  92,  17,  92,  92,  92,  17,  92,  92,  92,  92, // state 19
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  19,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 20
     21,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  21,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  21, // state 21 <skip>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 22 <end:6>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 23 <end:13>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 24 <end:15>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 25 <end:9>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 26 <end:2>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  45,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 27 <end:8>
     92,  92,  92,  92,  92,  92,  92,  92,  46,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 28 <end:3>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 29 <end:1>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 30 <end:14>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 31 <end:11>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 32 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 33 <end:30,push(1,state 12)>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  47,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 34 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  54,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 35 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  61,  32,  32,  32,  32,  32,  32,  32,  92, // state 36 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  67,  32,  32,  32,  32,  32,  32,  92, // state 37 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  72,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  71,  32,  32,  32,  32,  32,  32,  92, // state 38 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  77,  32,  32,  32,  32,  32,  92, // state 39 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  80,  32,  32,  32,  32,  92, // state 40 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 41 <end:5>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 42 <end:10>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 43 <end:12>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 44 <end:7>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 45 <end:0>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 46 <end:4>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  48,  32,  32,  32,  92, // state 47 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  49,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 48 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  50,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 49 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  51,  32,  32,  32,  32,  32,  32,  32,  92, // state 50 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  52,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 51 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  53,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 52 <end:25>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 53 <end:16>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  55,  32,  32,  32,  92, // state 54 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  56,  32,  32,  92, // state 55 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  57,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 56 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  58,  32,  32,  32,  32,  32,  32,  32,  92, // state 57 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  59,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 58 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  60,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 59 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 60 <end:17>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  62,  32,  92, // state 61 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  63,  92, // state 62 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  64,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 63 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  65,  32,  32,  32,  32,  32,  32,  92, // state 64 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  66,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 65 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 66 <end:18>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  68,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  69,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 67 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  70,  32,  32,  32,  32,  32,  32,  32,  92, // state 68 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  76,  32,  32,  32,  32,  32,  32,  32,  92, // state 69 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 70 <end:19>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  73,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 71 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  74,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 72 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 73 <end:20>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  75,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 74 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 75 <end:21>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 76 <end:22>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  78,  92, // state 77 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  79,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 78 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 79 <end:23>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  81,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 80 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  82,  32,  32,  32,  32,  32,  32,  32,  92, // state 81 <end:26>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  32,  92,  92,  32,  32,  32,  32,  32,  32,  32,  92,  92,  92,  92,  92,  32,  32,  32,  32,  32,  92,  32,  92,  32,  92,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  92, // state 82 <end:24>
     83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  83,  92, // state 83 <skip>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 84 <skip>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 85 <end:27>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 86 <end:28>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 87 <end:29>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 88 <end:32>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 89 <end:8>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 90 <end:31,pop>
     92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, // state 91 <end:29>
     92 // error group in [nbr_state * nbr_group + nbr_group]
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
                ("-> : , . .. { ( ~ - + | ? } ) ; * channels fragment lexicon mode pop push more skip type channel \\w[a-z.\\t\\w]",
                 vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 29, 30, 32, 8, 32, 32, 32, 29, 31],
                 vec!["->", ":", ",", ".", "..", "{", "(", "~", "-", "+", "|", "?", "}", ")", ";", "*",
                      "channels", "fragment", "lexicon", "mode", "pop", "push", "more", "skip", "type", "channel", "\\w", "[", "a", "-", "z", ".", "\\t", "\\w", "]"]),
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