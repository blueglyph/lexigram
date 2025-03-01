#![cfg(test)]

pub(super) mod lexilexer {
    // -------------------------------------------------------------------------
    // [lexilexer]

    use std::collections::HashMap;
    use std::io::Read;
    use rlexer::dfa::{StateId, Terminal, ActionOption, ModeOption};
    use rlexer::lexer::Lexer;
    use rlexer::lexergen::GroupId;
    use rlexer::segments::{Seg, SegMap};

    const NBR_GROUPS: u32 = 52;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 21;
    const NBR_STATES: StateId = 93;
    const ASCII_TO_GROUP: [GroupId; 128] = [
         38,  38,  38,  38,  38,  38,  38,  38,  38,  28,  51,  38,  38,  51,  38,  38,   // 0-15
         38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,   // 16-31
          0,  38,  38,  38,  38,  38,  38,   1,   2,   3,   4,   5,   6,   7,   8,   9,   // 32-47
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  10,  11,  38,  38,  34,  12,   // 48-63
         38,  32,  32,  32,  32,  14,  50,  35,  35,  35,  35,  35,  35,  35,  35,  39,   // 64-79
         35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  35,  15,  16,  36,  38,  37,   // 80-95
         38,  46,  32,  17,  13,  42,  18,  47,  40,  49,  35,  44,  19,  20,  30,  43,   // 96-111
         21,  35,  41,  22,  23,  31,  35,  33,  48,  45,  35,  24,  25,  26,  27,  38,   // 112-127
    ];
    const UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    const SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 38),
        (Seg(57344, 1114111), 38),
    ];
    const TERMINAL_TABLE: [Terminal;72] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::Push(1), mode_state: Some(12), pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(29), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(28), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(30), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(31), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(32), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(33), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: true },
    ];
    const STATE_TABLE: [StateId; 4837] = [
         21,   1,  22,  23,  24,  25,  26,  27,  28,   2,  29,  30,  31,  32,  33,  34,   3,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  21,  93,  32,  32,  32,  32,  93,  32,  93,  93,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  21, // state 0
          6,  93,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   7,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  93,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,  93, // state 1
         93,  93,  93,  93,   4,  93,  93,  93,  93,  86,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 2
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  90,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  90,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 3
          4,   4,   4,   4,   5,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4, // state 4
          4,   4,   4,   4,   5,   4,   4,   4,   4,  87,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   4, // state 5
         11,  88,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  15,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  93,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  93, // state 6
         93,   6,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,   6,  93,  93,  93,  93,  93,  93,   6,  93,  93,  93,  93,  93,  93,   6,   8,  93,  93,  93,  93,  93,  93,  93,  93,  93,   6,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 7
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,   9,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 8
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  10,  10,  93,  93,  10,  10,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  10,  93,  93,  10,  93,  93,  93,  93,  93,  93,  93,  93,  93,  10,  93,  93,  93,  10,  93,  93,  93,  10,  93, // state 9
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  10,  10,  93,  93,  10,  10,  93,  93,  93,  93,  93,  93,  93,   6,  93,  93,  10,  93,  93,  10,  93,  93,  93,  93,  93,  93,  93,  93,  93,  10,  93,  93,  93,  10,  93,  93,  93,  10,  93, // state 10
         11,  89,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  15,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  93,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,  93, // state 11
         91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  13,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  93,  91,  91,  91,  91,  91,  91,  91,  92,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  91,  93, // state 12
         93,  91,  93,  93,  93,  93,  93,  91,  93,  93,  93,  93,  93,  93,  93,  91,  91,  93,  93,  93,  93,  93,  93,  91,  93,  93,  93,  93,  93,  93,  91,  18,  93,  93,  93,  93,  91,  93,  93,  93,  93,  91,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 13
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  14,  14,  93,  93,  14,  14,  93,  93,  93,  93,  93,  93,  93,  91,  93,  93,  14,  93,  93,  14,  93,  93,  93,  93,  93,  93,  93,  93,  93,  14,  93,  93,  93,  14,  93,  93,  93,  14,  93, // state 14
         93,  11,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  11,  93,  93,  93,  93,  93,  93,  11,  93,  93,  93,  93,  93,  93,  11,  20,  93,  93,  93,  93,  93,  93,  93,  93,  93,  11,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 15
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  14,  14,  93,  93,  14,  14,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  14,  93,  93,  14,  93,  93,  93,  93,  93,  93,  93,  93,  93,  14,  93,  93,  93,  14,  93,  93,  93,  14,  93, // state 16
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  17,  17,  93,  93,  17,  17,  93,  93,  93,  93,  93,  93,  93,  11,  93,  93,  17,  93,  93,  17,  93,  93,  93,  93,  93,  93,  93,  93,  93,  17,  93,  93,  93,  17,  93,  93,  93,  17,  93, // state 17
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  16,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 18
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  17,  17,  93,  93,  17,  17,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  17,  93,  93,  17,  93,  93,  93,  93,  93,  93,  93,  93,  93,  17,  93,  93,  93,  17,  93,  93,  93,  17,  93, // state 19
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  19,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 20
         21,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  21,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  21, // state 21 <skip>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 22 <end:7>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 23 <end:15>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 24 <end:17>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 25 <end:10>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 26 <end:2>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  46,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 27 <end:9>
         93,  93,  93,  93,  93,  93,  93,  93,  47,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 28 <end:3>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 29 <end:1>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 30 <end:16>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 31 <end:12>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 32 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  84,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 33 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 34 <end:6,push(1,state 12)>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  48,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 35 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  55,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 36 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  62,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 37 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  68,  32,  32,  32,  32,  32,  32,  32,  93, // state 38 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  73,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  72,  32,  32,  32,  32,  32,  32,  32,  93, // state 39 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  78,  32,  32,  32,  32,  32,  32,  93, // state 40 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  81,  32,  32,  32,  32,  32,  93, // state 41 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 42 <end:5>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 43 <end:11>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 44 <end:13>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 45 <end:8>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 46 <end:0>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 47 <end:4>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  49,  32,  32,  32,  32,  93, // state 48 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  50,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 49 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  51,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 50 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  52,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 51 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  53,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 52 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  54,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 53 <end:27>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 54 <end:18>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  56,  32,  32,  32,  32,  93, // state 55 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  57,  32,  32,  32,  93, // state 56 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  58,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 57 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  59,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 58 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  60,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 59 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  61,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 60 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 61 <end:19>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  63,  32,  32,  93, // state 62 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  64,  32,  93, // state 63 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  65,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 64 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  66,  32,  32,  32,  32,  32,  32,  32,  93, // state 65 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  67,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 66 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 67 <end:20>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  69,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  70,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 68 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  71,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 69 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  77,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 70 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 71 <end:21>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  74,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 72 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  75,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 73 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 74 <end:22>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  76,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 75 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 76 <end:23>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 77 <end:24>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  79,  32,  93, // state 78 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  80,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 79 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 80 <end:25>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  82,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 81 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  83,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 82 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 83 <end:26>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  85,  93, // state 84 <end:29>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  32,  32,  93,  93,  32,  32,  32,  32,  32,  32,  32,  93,  93,  93,  93,  93,  32,  32,  32,  32,  32,  93,  32,  93,  32,  93,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  93, // state 85 <end:28>
         86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  93, // state 86 <skip>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 87 <skip>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 88 <end:30>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 89 <end:31>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 90 <end:32>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 91 <end:33>
         93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93,  93, // state 92 <end:14,pop>
         93 // error group in [nbr_state * nbr_group + nbr_group]
    ];

    pub fn build_lexer<R: Read>() -> Lexer<R> {
        Lexer::new(
            // parameters
            NBR_GROUPS,
            INITIAL_STATE,
            FIRST_END_STATE,
            NBR_STATES,
            // tables
            Box::new(ASCII_TO_GROUP),
            HashMap::<char, GroupId>::from(UTF8_TO_GROUP),
            SegMap::<GroupId>::from_iter(SEG_TO_GROUP),
            Box::new(STATE_TABLE),
            Box::new(TERMINAL_TABLE)
        )
    }

    // [lexilexer]
    // -------------------------------------------------------------------------
}

#[cfg(test)]
mod test {
    use std::io::Cursor;
    use rlexer::dfa::TokenId;
    use rlexer::escape_string;
    use rlexer::io::CharReader;
    use rlexer::lexi::{LEXICON, LEXICON_TEXT, LEXICON_TOKENS};
    use crate::out::lexilexer::lexilexer::build_lexer;

    #[test]
    pub fn check_lexer_tokens() {
        const VERBOSE: bool = false;
        let tests: Vec<(i32, Vec<(&str, Vec<u16>, Vec<&str>)>)> = vec![
            (1, vec![
                // no error
                ("-> : , . .. { ( ~ - + | ? } ) ; * channels fragment lexicon mode pop push more skip type channel [a-z.\\t]",
                 vec![0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 6, 33, 33, 33, 33, 33, 14],
                 vec!["->", ":", ",", ".", "..", "{", "(", "~", "-", "+", "|", "?", "}", ")", ";", "*",
                      "channels", "fragment", "lexicon", "mode", "pop", "push", "more", "skip", "type", "channel", "[", "a", "-", "z", ".", "\\t", "]"]),
            ]),
            (2, vec![(LEXICON, LEXICON_TOKENS.to_vec(), LEXICON_TEXT.to_vec())]),
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
                assert_eq!(tokens, expected_tokens, "{err_msg}");
                assert_eq!(texts, expected_texts, "{err_msg}");
                assert!(!lexer.has_error() || lexer.is_eos(), "{err_msg}");
            }
            if VERBOSE { println!("--------------------------------------\n"); }
        }
    }
}