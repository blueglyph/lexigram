// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

#[cfg(feature = "gen_tests")]
mod base_lexer_tests {
    /*!
    ```
    // [lexer1_lexicon]
    
    lexicon Test1;
    channels { CH_1, CH_2 }

    fragment Chat           : '(' [ 0-9A-Za-z,.!?]* ')';

    Star                    : '*';
    Keyword                 : 'keyword';
    Id                      : [a-z][a-z0-9]*;

    ModeA                   : 'mode_a'                  -> mode(MODE_A);
    PushA                   : 'push_a'                  -> push(MODE_A);
    Ch1                     : '1' Chat                  -> channel(CH_1);
    Ch2                     : '2' Chat                  -> channel(CH_2);

    SkipWhiteSpace          : [ \n\r\t]+                -> skip;

    //------------------------------------------------------------------
    mode MODE_A;

    StarA                   : '*';
    KeywordA                : 'keyword'                 -> type(Keyword);
    ModeB                   : 'smode_b'                 -> skip, mode(MODE_B);
    MoreModeB               : 'b'                       -> more, mode(MODE_B);
    PushA2                  : 'push_a'                  -> push(MODE_A);
    PushB                   : 'spush_b'                 -> skip, push(MODE_B);
    PopFromA                : 'pop'                     -> pop;
    PopFromAPushB           : 'pop_push_b'              -> pop, push(MODE_B);
    SkipWhiteSpaceA         : [ \n\r\t]+                -> skip;

    //------------------------------------------------------------------
    mode MODE_B;

    StarB                   : '*';
    KeywordB                : 'keyword'                 -> type(Keyword);
    PushB2                  : 'push_b'                  -> push(MODE_B);
    PopFromB                : 'pop'                     -> pop;
    SkipWhiteSpaceB         : [ \n\r\t]+                -> skip;

    // [lexer1_lexicon]
    ```
    */

    use crate::lexer::{ChannelId, ModeId};

    // generated code in a mod so that the IDE won't change the use declarations
    mod lex {
        // -------------------------------------------------------------------------
        // [lexer1_source]

        // This code is generated from lexigram\src\tests.rs
        // and corresponds to the lexicon between tags [lexer1_lexicon]

        use std::collections::HashMap;
        use std::io::Read;
        use crate::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
        use crate::segmap::{GroupId, Seg, SegMap};

        const NBR_GROUPS: u32 = 25;
        const INITIAL_STATE: StateId = 0;
        const FIRST_END_STATE: StateId = 50;
        const NBR_STATES: StateId = 86;
        static ASCII_TO_GROUP: [GroupId; 128] = [
             25,  25,  25,  25,  25,  25,  25,  25,  25,  10,  10,  25,  25,  10,  25,  25,   // 0-15
             25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,   // 16-31
              0,  24,  25,  25,  25,  25,  25,  25,   8,  12,   1,  25,  24,  25,  24,  25,   // 32-47
             11,   2,   3,  11,  11,  11,  11,  11,  11,  11,  25,  25,  25,  25,  25,  24,   // 48-63
             25,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,   // 64-79
             24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,  25,  25,  25,  25,  22,   // 80-95
             25,   4,   9,  23,  20,  14,  23,  23,  21,  23,  23,   5,  23,   6,  23,  15,   // 96-111
              7,  23,  19,  13,  23,  16,  23,  18,  23,  17,  23,  25,  25,  25,  25,  25,   // 112-127
        ];
        static UTF8_TO_GROUP: [(char, GroupId); 0] = [
        ];
        static SEG_TO_GROUP: [(Seg, GroupId); 0] = [
        ];
        static TERMINAL_TABLE: [Terminal;36] = [
            Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::Mode(1), mode_state: Some(6), pop: false },
            Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::Push(1), mode_state: Some(6), pop: false },
            Terminal { action: ActionOption::Token(5), channel: 1, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(6), channel: 2, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::More, channel: 0, mode: ModeOption::Mode(2), mode_state: Some(26), pop: false },
            Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Mode(2), mode_state: Some(26), pop: false },
            Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::Push(1), mode_state: Some(6), pop: false },
            Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::Push(2), mode_state: Some(26), pop: false },
            Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: true },
            Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::Push(2), mode_state: Some(26), pop: true },
            Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
            Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::Push(2), mode_state: Some(26), pop: false },
            Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: true },
        ];
        static STATE_TABLE: [StateId; 2151] = [
             50,  51,   1,  30,  52,  53,  54,  55,  86,  52,  50,  86,  86,  52,  52,  52,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 0
             86,  86,  86,  86,  86,  86,  86,  86,   4,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 1
             86,  86,  86,  86,  68,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 2
             86,  86,  86,  86,  69,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 3
              4,  86,   4,   4,   4,   4,   4,   4,  86,   4,  86,   4,  70,   4,   4,   4,   4,   4,   4,   4,   4,   4,  86,   4,   4, // state 4
              5,  86,   5,   5,   5,   5,   5,   5,  86,   5,  86,   5,  71,   5,   5,   5,   5,   5,   5,   5,   5,   5,  86,   5,   5, // state 5
             72,  73,  86,  86,  86,   7,  86,   8,  86,  74,  72,  86,  86,   9,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 6
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  10,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 7
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  19,  20,  86,  86,  86,  86,  86,  86,  86,  86, // state 8
             86,  86,  86,  86,  86,  86,  33,  15,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 9
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  11,  86,  86,  86,  86,  86,  86,  86, // state 10
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  12,  86,  86,  86,  86,  86,  86, // state 11
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  13,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 12
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  14,  86,  86,  86,  86,  86, // state 13
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  75,  86,  86,  86,  86, // state 14
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  44,  86,  86,  86,  86,  86,  86,  86,  86, // state 15
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  31,  86,  86,  86,  86, // state 16
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  18,  86,  86, // state 17
             86,  86,  86,  86,  86,  86,  86,  86,  86,  76,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 18
             86,  86,  86,  86,  86,  86,  86,  79,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 19
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  21,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 20
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  35,  86,  86,  86, // state 21
             86,  86,  86,  86,  77,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 22
             86,  86,  86,  86,  86,  86,  86,  86,  86,  78,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 23
             86,  86,  86,  86,  86,  86,  86,  48,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 24
             86,  86,  86,  86,  86,  86,  86,  86,  86,  80,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 25
             81,  82,  86,  86,  86,  49,  86,  32,  86,  86,  81,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 26
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  83,  86,  86,  86,  86, // state 27
             86,  86,  86,  86,  86,  86,  86,  85,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 28
             86,  86,  86,  86,  86,  86,  86,  86,  86,  84,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 29
             86,  86,  86,  86,  86,  86,  86,  86,   5,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 30
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  17,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 31
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  28,  46,  86,  86,  86,  86,  86,  86,  86,  86, // state 32
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  16,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 33
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  27,  86,  86,  86,  86,  86, // state 34
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  22,  86,  86, // state 35
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  23,  86,  86, // state 36
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  25,  86,  86, // state 37
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  29,  86,  86, // state 38
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  34,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 39
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  36,  86,  86,  86, // state 40
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  37,  86,  86,  86, // state 41
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  38,  86,  86,  86, // state 42
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  39,  86,  86,  86,  86,  86,  86, // state 43
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  40,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 44
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  41,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 45
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  42,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 46
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  43,  86,  86,  86,  86,  86,  86,  86, // state 47
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  45,  86,  86,  86,  86,  86,  86,  86,  86, // state 48
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  47,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 49
             50,  86,  86,  86,  86,  86,  86,  86,  86,  86,  50,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 50 <skip>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 51 <end:0>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 52 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  56,  52,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 53 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  62,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 54 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  65,  52,  52,  52,  52,  52,  86,  52,  86, // state 55 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  57,  52,  52,  52,  52,  86,  52,  86, // state 56 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  58,  52,  52,  52,  86,  52,  86, // state 57 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  59,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 58 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  60,  52,  52,  86,  52,  86, // state 59 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  61,  52,  86,  52,  86, // state 60 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 61 <end:1>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  63,  52,  86,  52,  86, // state 62 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  64,  52,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 63 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  52,  52,   2,  52,  86, // state 64 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  66,  52,  52,  52,  52,  52,  52,  52,  52,  86,  52,  86, // state 65 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  52,  67,  86,  52,  86, // state 66 <end:2>
             86,  86,  52,  52,  52,  52,  52,  52,  86,  52,  86,  52,  86,  52,  52,  52,  52,  52,  52,  52,  52,  52,   3,  52,  86, // state 67 <end:2>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 68 <end:3,mode(1,state 6)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 69 <end:4,push(1,state 6)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 70 <end:5,ch 1>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 71 <end:6,ch 2>
             72,  86,  86,  86,  86,  86,  86,  86,  86,  86,  72,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 72 <skip>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 73 <end:7>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 74 <more,mode(2,state 26)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 75 <end:1>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 76 <skip,mode(2,state 26)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 77 <end:8,push(1,state 6)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 78 <skip,push(2,state 26)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  24,  86,  86, // state 79 <end:9,pop>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 80 <end:10,push(2,state 26),pop>
             81,  86,  86,  86,  86,  86,  86,  86,  86,  86,  81,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 81 <skip>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 82 <end:11>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 83 <end:1>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 84 <end:12,push(2,state 26)>
             86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86,  86, // state 85 <end:13,pop>
             86 // error group in [nbr_state * nbr_group + nbr_group]
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

        // [lexer1_source]
        // -------------------------------------------------------------------------
    }

    const CH_DEFAULT    : ChannelId = 0;
    const CH_1          : ChannelId = 1;
    const CH_2          : ChannelId = 2;

    mod tok {
        use crate::TokenId;

        pub const STAR              : TokenId = 0 ; //          "*"
        pub const KEYWORD           : TokenId = 1 ; //          "keyword"
        pub const ID                : TokenId = 2 ; //          [a-z][a-z0-9]*
        pub const MODE_A            : TokenId = 3 ; //          "mode_a"
        pub const PUSH_A            : TokenId = 4 ; //          "push_a"
        pub const CH1               : TokenId = 5 ; //          "1(" [ 0-9A-Za-z,.!?] ")"
        pub const CH2               : TokenId = 6 ; //          "2(" [ 0-9A-Za-z,.!?] ")"
        pub const STAR_A            : TokenId = 7 ; //  MODE_A  "*"
        pub const PUSH_A2           : TokenId = 8 ; //  MODE_A  "push_a"
        pub const POP_FROM_A        : TokenId = 9 ; //  MODE_A  "pop"
        pub const POP_FROM_A_PUSH_B : TokenId = 10; //  MODE_A  "pop_push_b"
        pub const STAR_B            : TokenId = 11; //  MODE_B  "*"
        pub const PUSH_B2           : TokenId = 12; //  MODE_B  "push_b"
        pub const POP_FROM_B        : TokenId = 13; //  MODE_B  "pop"

        pub static NAMES: &[&str; 14] = &[
            "STAR", "KEYWORD", "ID", "MODE_A", "PUSH_A", "CH1", "CH2", "STAR_A",
            "PUSH_A2", "POP_FROM_A", "P ","STAR_B", "PUSH_B2", "POP_FROM_B"
        ];
    }

    mod lexicon {
        use std::io::Cursor;
        use crate::char_reader::CharReader;
        use crate::lexer::tests::base_lexer_tests::lex::build_lexer;
        use crate::{CollectJoin, TokenId};
        use crate::lexer::{Lexer, LexerError, LexerErrorInfo, Pos, PosSpan};
        use crate::lexer::LexerError::UnrecognizedChar;
        use super::*;
        use super::tok::*;

        #[test]
        fn test_iter() {
            let tests: Vec<(&str, &str, Vec<TokenId>, Vec<TokenId>, Vec<TokenId>, Vec<&str>, Vec<&str>, Vec<&str>)> = vec![
                // input, success, ch[0], ch[1], ch[2], ch_s[0], ch_s[1], ch_s[2]
                ( // 0
                    "*", "",
                    vec![STAR], vec![], vec![],
                    vec!["*"], vec![], vec![],
                ),
                ( // 1
                    " * ", "",
                    vec![STAR], vec![], vec![],
                    vec!["*"], vec![], vec![],
                ),
                ( // 2
                    "mode_a *", "",
                    vec![MODE_A, STAR_A], vec![], vec![],
                    vec!["mode_a", "*"], vec![], vec![],
                ),
                ( // 3
                    "push_a * spush_b * pop * pop *", "",
                    vec![PUSH_A, STAR_A, STAR_B, POP_FROM_B, STAR_A, POP_FROM_A, STAR], vec![], vec![],
                    vec!["push_a", "*", "*", "pop", "*", "pop", "*"], vec![], vec![],
                ),
                ( // 4
                    "mode_a b *", "",
                    vec![MODE_A, STAR_B], vec![], vec![],
                    vec!["mode_a", "b*"], vec![], vec![],
                ),
                ( // 5
                    "mode_a * keyword smode_b * keyword", "",
                    vec![MODE_A, STAR_A, KEYWORD, STAR_B, KEYWORD], vec![], vec![],
                    vec!["mode_a", "*", "keyword", "*", "keyword"], vec![], vec![],
                ),
                (
                    "1(hello)2(world)", "",
                    vec![], vec![CH1], vec![CH2],
                    vec![], vec!["1(hello)"], vec!["2(world)"],
                ),
                (
                    "*+", "unrecognized character '+', line 1, col 2 (stream pos = 1)",
                    vec![STAR], vec![], vec![],
                    vec!["*"], vec![], vec![],
                ),
                (
                    "", "end of stream, line 1, col 1 (stream pos = 0)",
                    vec![], vec![], vec![],
                    vec![], vec![], vec![],
                ),

                /* template
                (
                    "", true,
                    vec![], vec![], vec![],
                    vec![], vec![], vec![],
                ),
                */
            ];
            const VERBOSE: bool = true;
            static INDENT1: &str = "                ";
            static INDENT2: &str = "                    ";
            for (id, (text, exp_err, exp_ch0, exp_ch1, exp_ch2, exp_ch_s0, exp_ch_s1, exp_ch_s2)) in tests.into_iter().enumerate() {
                // if VERBOSE { println!("{:-<80}\ntest {id}\ninput: {text}", "")}
                let mut lexer = build_lexer();
                let stream = CharReader::new(Cursor::new(text));
                lexer.attach_stream(stream);
                let mut ch = vec![vec![], vec![], vec![]];
                let mut ch_s = vec![vec![], vec![], vec![]];
                for (tokenid, channelid, string, posspan) in lexer.tokens() {
                    assert!(channelid <= CH_2, "unexpected channel id {channelid}");
                    ch[channelid as usize].push(tokenid);
                    ch_s[channelid as usize].push(string);
                };
                // note that get_error().to_string() is "no error" when there's no error
                let err = if lexer.has_error() { format!("{}", lexer.get_error()) } else { String::new() };
                if VERBOSE {
                    println!("{INDENT1}( // {id}\n{INDENT2}{text:?}, {err:?},");
                    println!("{INDENT2}{},", (0..3).map(|i| format!("vec![{}]", ch[i].iter().map(|&t| NAMES[t as usize]).join(", "))).join(", "));
                    println!("{INDENT2}{},", (0..3).map(|i| format!("vec![{}]", ch_s[i].iter().map(|s| format!("{s:?}")).join(", "))).join(", "));
                    println!("{INDENT1}),");
                }
                let msg = format!("## ERROR in test {id}");
                assert_eq!(
                    err, exp_err,
                    "{msg}: lexer should have {}{err}", if err.is_empty() { "no error" } else { "this error: " });
                assert_eq!(ch[0], exp_ch0, "{msg}: mismatch in channel 0");
                assert_eq!(ch[1], exp_ch1, "{msg}: mismatch in channel 1");
                assert_eq!(ch[2], exp_ch2, "{msg}: mismatch in channel 2");
                assert_eq!(ch_s[0], exp_ch_s0, "{msg}: mismatch in channel 0");
                assert_eq!(ch_s[1], exp_ch_s1, "{msg}: mismatch in channel 1");
                assert_eq!(ch_s[2], exp_ch_s2, "{msg}: mismatch in channel 2");
            }
        }

        #[test]
        fn test_get_token() {
            fn get_lexer_state(lexer: &Lexer<Cursor<&str>>) -> (bool, LexerError, bool, bool) {
                (lexer.has_error(), lexer.get_error().clone(), lexer.is_eos(), lexer.is_open())
            }
            let mut lexer = build_lexer();
            let text = "* id 1()";
            let stream = CharReader::new(Cursor::new(text));
            lexer.attach_stream(stream);
            // (tokenid, channelid, string, posspan)
            assert_eq!(lexer.get_token(), Ok(Some((STAR, CH_DEFAULT, "*".to_string(), PosSpan::new(Pos(1, 1), Pos(1, 1))))));
            // has_error, error, is_eos, is_open
            assert_eq!(get_lexer_state(&lexer), (false, LexerError::None, false, true));
            assert_eq!(lexer.get_token(), Ok(Some((ID, CH_DEFAULT, "id".to_string(), PosSpan::new(Pos(1, 3), Pos(1, 4))))));
            assert_eq!(get_lexer_state(&lexer), (false, LexerError::None, false, true));
            assert_eq!(lexer.get_token(), Ok(Some((CH1, CH_1, "1()".to_string(), PosSpan::new(Pos(1, 6), Pos(1, 8))))));
            assert_eq!(get_lexer_state(&lexer), (false, LexerError::None, true, false));

            let text = "\n *+";
            let stream = CharReader::new(Cursor::new(text));
            lexer.attach_stream(stream);
            // (tokenid, channelid, string, posspan)
            assert_eq!(lexer.get_token(), Ok(Some((STAR, CH_DEFAULT, "*".to_string(), PosSpan::new(Pos(2, 2), Pos(2, 2))))));
            // has_error, error, is_eos, is_open
            assert_eq!(get_lexer_state(&lexer), (false, LexerError::None, false, true));
            assert_eq!(lexer.get_token().err().map(|e| e.to_string()), Some("unrecognized character '+', line 2, col 3 (stream pos = 3)".to_string()));
            let error = lexer.get_error().clone();
            assert_eq!(get_lexer_state(&lexer), (true, error, false, true));
            assert_eq!(lexer.get_token().err().map(|e| e.to_string()), Some("end of stream, line 2, col 4 (stream pos = 4)".to_string()));
            let error = lexer.get_error().clone();
            assert_eq!(get_lexer_state(&lexer), (true, error, true, false));
        }
    }
}