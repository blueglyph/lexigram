#[allow(unused)]
mod listener_types {
    /// User-defined type for `config`
    #[derive(Debug, PartialEq)]
    pub struct SynConfig();

    /// User-defined type for `item`
    #[derive(Debug, PartialEq)]
    pub struct SynItem();

    /// User-defined type for `definition`
    #[derive(Debug, PartialEq)]
    pub struct SynDefinition();

    /// User-defined type for `lexer`
    #[derive(Debug, PartialEq)]
    pub struct SynLexer();

    /// User-defined type for `parser`
    #[derive(Debug, PartialEq)]
    pub struct SynParser();

    /// User-defined type for `options`
    #[derive(Debug, PartialEq)]
    pub struct SynOptions();

    /// User-defined type for `io_options`
    #[derive(Debug, PartialEq)]
    pub struct SynIoOptions();

    /// User-defined type for `io_option`
    #[derive(Debug, PartialEq)]
    pub struct SynIoOption();

    /// User-defined type for `tag_opt`
    #[derive(Debug, PartialEq)]
    pub struct SynTagOpt();

    /// User-defined type for `global_options`
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOptions();

    /// User-defined type for `global_option`
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOption();

    /// User-defined type for `value`
    #[derive(Debug, PartialEq)]
    pub struct SynValue();

    /// User-defined type for `nt_value`
    #[derive(Debug, PartialEq)]
    pub struct SynNtValue();
}

#[allow(unused)]
mod config_lexer {
    // [config_lexer]

    use std::collections::HashMap;
    use std::io::Read;
    use lexigram_core::lexer::{ActionOption, Lexer, ModeOption, StateId, Terminal};
    use lexigram_core::segmap::{GroupId, Seg, SegMap};

    const NBR_GROUPS: u32 = 38;
    const INITIAL_STATE: StateId = 0;
    const FIRST_END_STATE: StateId = 7;
    const NBR_STATES: StateId = 108;
    static ASCII_TO_GROUP: [GroupId; 128] = [
         29,  29,  29,  29,  29,  29,  29,  29,  29,  24,  37,  29,  29,  37,  29,  29,   // 0-15
         29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,   // 16-31
          0,  29,   1,  29,  29,  29,  29,  29,  29,  29,  25,  29,   2,  29,  29,   3,   // 32-47
          4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   5,   6,  29,   7,  29,  29,   // 48-63
         29,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,   // 64-79
         28,  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,   9,  26,  10,  29,  27,   // 80-95
         29,  31,  34,  11,  12,  30,  13,  28,  14,  15,  28,  28,  16,  33,  17,  18,   // 96-111
         19,  28,   8,  20,  21,  32,  36,  28,  35,  28,  28,  22,  29,  23,  29,  29,   // 112-127
    ];
    static UTF8_TO_GROUP: [(char, GroupId); 0] = [
    ];
    static SEG_TO_GROUP: [(Seg, GroupId); 2] = [
        (Seg(128, 55295), 29),
        (Seg(57344, 1114111), 29),
    ];
    static TERMINAL_TABLE: [Terminal;101] = [
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(1), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(26), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(0), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(7), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(2), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(4), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(6), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(3), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(5), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(8), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(9), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(10), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(11), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(12), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(13), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(14), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(15), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(16), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(17), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(18), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(19), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(20), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(21), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(22), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(23), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(24), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(25), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Token(27), channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
        Terminal { action: ActionOption::Skip, channel: 0, mode: ModeOption::None, mode_state: None, pop: false },
    ];
    static STATE_TABLE: [StateId; 4105] = [
          7,   1,   8,   2,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,   7, 108, 108, 108,  13, 108,  13,  13,  13,  13,  13,  13,  13,   7, // state 0
          3, 108,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, 108,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, 108, // state 1
        108, 108, 108, 106, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108,   5, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 2
          3, 105,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, 108,   3,   4,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3, 108, // state 3
        108,   3, 108, 108, 108, 108, 108, 108,   3, 108, 108, 108, 108, 108, 108, 108, 108,   3, 108, 108, 108,   3, 108, 108, 108, 108,   3, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 4
          5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   6,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5, // state 5
          5,   5,   5, 107,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   6,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   5, // state 6
          7, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108,   7, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108,   7, // state 7 <skip>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 8 <end:1>
        108, 108, 108, 108,   9, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 9 <end:26>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 10 <end:0>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 11 <end:7>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 12 <end:2>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 13 <end:25>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 14 <end:4>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 15 <end:6>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  29,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 16 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  36,  13,  13,  13,  13,  13,  13, 108, // state 17 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  99,  13,  13,  13,  13,  13, 108, // state 18 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  42,  13,  13,  13,  13,  13,  13, 108, // state 19 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  48,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 20 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  57,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  56,  13,  13,  13,  13,  13,  13, 108, // state 21 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  63,  13,  13,  64, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 22 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  73,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  74,  13,  13,  13,  13, 108, // state 23 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  84,  13,  13,  13,  13,  13, 108, // state 24 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  94,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  93,  13,  13,  13,  13,  13,  13, 108, // state 25 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108, 103, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 26 <end:25>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 27 <end:3>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 28 <end:5>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  30,  13,  13,  13, 108, // state 29 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  31,  13,  13, 108, // state 30 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  32,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 31 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  33,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 32 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  34,  13,  13,  13,  13,  13,  13, 108, // state 33 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  35,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 34 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 35 <end:8>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  37,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 36 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  38,  13,  13,  13,  13,  13, 108, // state 37 <end:9>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  39,  13,  13,  13,  13, 108, // state 38 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  40,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 39 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  41, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 40 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 41 <end:10>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  43,  13,  13,  13,  13,  13, 108, // state 42 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  44,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 43 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  45,  13,  13,  13,  13,  13,  13, 108, // state 44 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  46, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 45 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  47,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 46 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 47 <end:11>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  49,  13,  13,  13,  13,  13,  13,  50,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 48 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  51,  13,  13,  13,  13,  13,  13, 108, // state 49 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  54,  13,  13,  13,  13, 108, // state 50 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  52,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 51 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  53, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 52 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 53 <end:12>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  55, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 54 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 55 <end:13>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  58,  13, 108, // state 56 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  61,  13,  13, 108, // state 57 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  59,  13,  13,  13,  13,  13,  13, 108, // state 58 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  60, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 59 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 60 <end:14>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  62,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 61 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 62 <end:15>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  65,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 63 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  67,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 64 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  66,  13,  13,  13,  13,  13,  13, 108, // state 65 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 66 <end:16>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  68, 108, // state 67 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  69,  13,  13,  13,  13,  13, 108, // state 68 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  70,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 69 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  71,  13,  13,  13,  13, 108, // state 70 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  72,  13,  13,  13,  13,  13,  13, 108, // state 71 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 72 <end:17>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  75, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 73 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  80, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 74 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  76,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 75 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  77,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 76 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  78,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 77 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  79,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 78 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 79 <end:18>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  81,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 80 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  82,  13,  13,  13,  13, 108, // state 81 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  83, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 82 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 83 <end:19>
        108, 108, 108, 108,  13, 108, 108, 108,  85, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 84 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  87,  13, 108, 108, 108, 108, 108,  13,  13, 108,  86,  13,  13,  13,  13,  13,  13, 108, // state 85 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  88,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 86 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  91,  13,  13,  13,  13,  13,  13, 108, // state 87 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  89, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 88 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  90,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 89 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 90 <end:20>
        108, 108, 108, 108,  13, 108, 108, 108,  92, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 91 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 92 <end:21>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  95, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 93 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  96,  13,  13,  13,  13,  13, 108, // state 94 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 95 <end:22>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  97,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 96 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  98,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 97 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 98 <end:23>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13, 100,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 99 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13, 101,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 100 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108, 102,  13,  13,  13,  13,  13,  13, 108, // state 101 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13,  13,  13,  13,  13,  13, 108, // state 102 <end:24>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108,  13,  13, 104,  13,  13,  13,  13, 108, // state 103 <end:25>
        108, 108, 108, 108,  13, 108, 108, 108,  13, 108, 108,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13,  13, 108, 108, 108, 108, 108,  13,  13, 108, 102,  13,  13,  13,  13,  13,  13, 108, // state 104 <end:25>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 105 <end:27>
        106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 108, // state 106 <skip>
        108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, // state 107 <skip>
        108 // error group in [nbr_state * nbr_group + nbr_group]
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

    // [config_lexer]
}

#[allow(unused)]
mod config_parser {
    // [config_parser]

    use lexigram_core::{AltId, TokenId, VarId, fixed_sym_table::FixedSymTable, lexer::PosSpan, log::Logger, parser::{Call, ListenerWrapper, OpCode, Parser, Terminate}};
    use super::listener_types::*;

    const PARSER_NUM_T: usize = 28;
    const PARSER_NUM_NT: usize = 19;
    static SYMBOLS_T: [(&str, Option<&str>); PARSER_NUM_T] = [("Colon", Some(":")), ("Comma", Some(",")), ("Equal", Some("=")), ("Lbracket", Some("{")), ("LSbracket", Some("[")), ("Rbracket", Some("}")), ("RSbracket", Some("]")), ("Semicolon", Some(";")), ("Combined", Some("combined")), ("Def", Some("def")), ("Default", Some("default")), ("Headers", Some("headers")), ("Indent", Some("indent")), ("Input", Some("input")), ("Lexer", Some("lexer")), ("Libs", Some("libs")), ("None", Some("none")), ("NTValue", Some("nt_value")), ("Options", Some("options")), ("Output", Some("output")), ("Parents", Some("parents")), ("Parser", Some("parser")), ("Set", Some("set")), ("Spans", Some("spans")), ("BoolLiteral", None), ("Id", None), ("NumLiteral", None), ("StrLiteral", None)];
    static SYMBOLS_NT: [&str; PARSER_NUM_NT] = ["config", "item", "definition", "lexer", "parser", "options", "io_options", "io_option", "tag_opt", "global_options", "global_option", "value", "nt_value", "io_options_1", "io_option_1", "global_options_1", "global_option_1", "nt_value_1", "config_1"];
    static ALT_VAR: [VarId; 41] = [0, 1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7, 8, 8, 9, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18];
    static PARSING_TABLE: [AltId; 551] = [41, 41, 41, 41, 41, 41, 41, 41, 41, 0, 41, 41, 41, 41, 0, 41, 41, 41, 0, 41, 41, 0, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 1, 41, 41, 41, 41, 2, 41, 41, 41, 4, 41, 41, 3, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 5, 41, 41, 41, 41, 42, 41, 41, 41, 42, 41, 41, 42, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 6, 41, 41, 41, 42, 41, 41, 42, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 42, 41, 41, 41, 42, 41, 41, 7, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 42, 41, 41, 41, 8, 41, 41, 42, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 42, 41, 41, 9, 41, 41, 9, 9, 9, 41, 41, 41, 41, 41, 9, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 42, 41, 41, 10, 41, 41, 14, 13, 11, 41, 41, 41, 41, 41, 12, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 16, 41, 41, 15, 16, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 17, 41, 17, 41, 41, 41, 41, 41, 17, 41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 18, 41, 19, 41, 41, 41, 41, 41, 20, 41, 41, 41, 41, 41, 41, 42, 41, 41, 42, 42, 42, 42, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 21, 24, 22, 23, 41, 41, 42, 41, 41, 41, 42, 41, 41, 41, 41, 25, 41, 41, 41, 41, 41, 26, 41, 41, 41, 27, 41, 28, 41, 41, 41, 41, 41, 41, 41, 29, 41, 41, 41, 30, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 31, 41, 41, 41, 32, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 33, 41, 41, 41, 34, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 35, 41, 41, 41, 36, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 37, 41, 41, 41, 38, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 39, 41, 41, 41, 41, 39, 41, 41, 41, 39, 41, 41, 39, 41, 41, 41, 41, 41, 41, 40];
    static OPCODES: [&[OpCode]; 41] = [&[OpCode::NT(18), OpCode::Exit(0), OpCode::NT(1)], &[OpCode::Exit(1), OpCode::NT(2)], &[OpCode::Exit(2), OpCode::NT(3)], &[OpCode::Exit(3), OpCode::NT(4)], &[OpCode::Exit(4), OpCode::NT(5)], &[OpCode::Exit(5), OpCode::T(7), OpCode::NT(11), OpCode::T(2), OpCode::T(25), OpCode::T(9)], &[OpCode::Exit(6), OpCode::T(5), OpCode::NT(6), OpCode::T(3), OpCode::T(14)], &[OpCode::Exit(7), OpCode::T(5), OpCode::NT(6), OpCode::T(3), OpCode::T(21)], &[OpCode::Exit(8), OpCode::T(5), OpCode::NT(9), OpCode::T(3), OpCode::T(18)], &[OpCode::Exit(9), OpCode::NT(13), OpCode::NT(7)], &[OpCode::Exit(10), OpCode::NT(8), OpCode::NT(11), OpCode::T(0), OpCode::T(8)], &[OpCode::Exit(11), OpCode::NT(8), OpCode::NT(11), OpCode::T(0), OpCode::T(13)], &[OpCode::Exit(12), OpCode::NT(8), OpCode::NT(11), OpCode::T(0), OpCode::T(19)], &[OpCode::Exit(13), OpCode::NT(11), OpCode::T(0), OpCode::T(12)], &[OpCode::Exit(14), OpCode::T(5), OpCode::NT(14), OpCode::NT(11), OpCode::T(3), OpCode::T(0), OpCode::T(11)], &[OpCode::Exit(15), OpCode::T(6), OpCode::NT(11), OpCode::T(4)], &[OpCode::Exit(16)], &[OpCode::Exit(17), OpCode::NT(15), OpCode::NT(10)], &[OpCode::Exit(18), OpCode::T(5), OpCode::NT(16), OpCode::NT(11), OpCode::T(3), OpCode::T(0), OpCode::T(15)], &[OpCode::Exit(19), OpCode::NT(12), OpCode::T(0), OpCode::T(17)], &[OpCode::Exit(20), OpCode::NT(11), OpCode::T(0), OpCode::T(23)], &[OpCode::Exit(21), OpCode::T(24)], &[OpCode::Exit(22), OpCode::T(26)], &[OpCode::Exit(23), OpCode::T(27)], &[OpCode::Exit(24), OpCode::T(25)], &[OpCode::Exit(25), OpCode::T(10)], &[OpCode::Exit(26), OpCode::T(16)], &[OpCode::Exit(27), OpCode::T(20)], &[OpCode::Exit(28), OpCode::T(5), OpCode::NT(17), OpCode::T(25), OpCode::T(3), OpCode::T(22)], &[OpCode::Loop(13), OpCode::Exit(29), OpCode::NT(7), OpCode::T(1)], &[OpCode::Exit(30)], &[OpCode::Loop(14), OpCode::Exit(31), OpCode::NT(11), OpCode::T(1)], &[OpCode::Exit(32)], &[OpCode::Loop(15), OpCode::Exit(33), OpCode::NT(10), OpCode::T(1)], &[OpCode::Exit(34)], &[OpCode::Loop(16), OpCode::Exit(35), OpCode::NT(11), OpCode::T(1)], &[OpCode::Exit(36)], &[OpCode::Loop(17), OpCode::Exit(37), OpCode::T(25), OpCode::T(1)], &[OpCode::Exit(38)], &[OpCode::Loop(18), OpCode::Exit(39), OpCode::NT(1)], &[OpCode::Exit(40)]];
    static INIT_OPCODES: [OpCode; 2] = [OpCode::End, OpCode::NT(0)];
    static START_SYMBOL: VarId = 0;

    pub fn build_parser() -> Parser<'static> {{
        let symbol_table = FixedSymTable::new(
            SYMBOLS_T.into_iter().map(|(s, os)| (s.to_string(), os.map(|s| s.to_string()))).collect(),
            SYMBOLS_NT.into_iter().map(|s| s.to_string()).collect()
        );
        Parser::new(
            PARSER_NUM_NT, PARSER_NUM_T + 1,
            &ALT_VAR,
            Vec::new(),
            OPCODES.into_iter().map(|strip| strip.to_vec()).collect(),
            INIT_OPCODES.to_vec(),
            &PARSING_TABLE,
            symbol_table,
            START_SYMBOL
        )
    }}

    #[derive(Debug)]
    pub enum CtxConfig {
        /// `config -> config item`
        V1 { config: SynConfig, item: SynItem },
        /// `config -> item`
        V2 { item: SynItem },
    }
    #[derive(Debug)]
    pub enum CtxItem {
        /// `item -> definition`
        V1 { definition: SynDefinition },
        /// `item -> lexer`
        V2 { lexer: SynLexer },
        /// `item -> parser`
        V3 { parser: SynParser },
        /// `item -> options`
        V4 { options: SynOptions },
    }
    #[derive(Debug)]
    pub enum CtxDefinition {
        /// `definition -> "def" Id "=" value ";"`
        V1 { id: String, value: SynValue },
    }
    #[derive(Debug)]
    pub enum CtxLexer {
        /// `lexer -> "lexer" "{" io_options "}"`
        V1 { io_options: SynIoOptions },
    }
    #[derive(Debug)]
    pub enum CtxParser {
        /// `parser -> "parser" "{" io_options "}"`
        V1 { io_options: SynIoOptions },
    }
    #[derive(Debug)]
    pub enum CtxOptions {
        /// `options -> "options" "{" global_options "}"`
        V1 { global_options: SynGlobalOptions },
    }
    #[derive(Debug)]
    pub enum CtxIoOptions {
        /// `io_options -> io_option ("," io_option)*`
        V1 { star: SynIoOptions1 },
    }
    #[derive(Debug)]
    pub enum CtxIoOption {
        /// `io_option -> "combined" ":" value tag_opt`
        V1 { value: SynValue, tag_opt: SynTagOpt },
        /// `io_option -> "input" ":" value tag_opt`
        V2 { value: SynValue, tag_opt: SynTagOpt },
        /// `io_option -> "output" ":" value tag_opt`
        V3 { value: SynValue, tag_opt: SynTagOpt },
        /// `io_option -> "indent" ":" value`
        V4 { value: SynValue },
        /// `io_option -> "headers" ":" "{" value ("," value)* "}"`
        V5 { star: SynIoOption1 },
    }
    #[derive(Debug)]
    pub enum CtxTagOpt {
        /// `tag_opt -> "[" value "]"`
        V1 { value: SynValue },
        /// `tag_opt -> ε`
        V2,
    }
    #[derive(Debug)]
    pub enum CtxGlobalOptions {
        /// `global_options -> global_option ("," global_option)*`
        V1 { star: SynGlobalOptions1 },
    }
    #[derive(Debug)]
    pub enum CtxGlobalOption {
        /// `global_option -> "libs" ":" "{" value ("," value)* "}"`
        V1 { star: SynGlobalOption1 },
        /// `global_option -> "nt_value" ":" nt_value`
        V2 { nt_value: SynNtValue },
        /// `global_option -> "spans" ":" value`
        V3 { value: SynValue },
    }
    #[derive(Debug)]
    pub enum CtxValue {
        /// `value -> BoolLiteral`
        V1 { boolliteral: String },
        /// `value -> NumLiteral`
        V2 { numliteral: String },
        /// `value -> StrLiteral`
        V3 { strliteral: String },
        /// `value -> Id`
        V4 { id: String },
    }
    #[derive(Debug)]
    pub enum CtxNtValue {
        /// `nt_value -> "default"`
        V1,
        /// `nt_value -> "none"`
        V2,
        /// `nt_value -> "parents"`
        V3,
        /// `nt_value -> "set" "{" Id ("," Id)* "}"`
        V4 { star: SynNtValue1 },
    }

    /// Computed `("," io_option)*` array in `io_options -> io_option  ►► ("," io_option)* ◄◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynIoOptions1(pub Vec<SynIoOption>);
    /// Computed `("," value)*` array in `io_option -> "combined" ":" value tag_opt | "input" ":" value tag_opt | "output" ":" value tag_opt | "indent" ":" value | "headers" ":" "{" value  ►► ("," value)* ◄◄  "}"`
    #[derive(Debug, PartialEq)]
    pub struct SynIoOption1(pub Vec<SynValue>);
    /// Computed `("," global_option)*` array in `global_options -> global_option  ►► ("," global_option)* ◄◄ `
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOptions1(pub Vec<SynGlobalOption>);
    /// Computed `("," value)*` array in `global_option -> "libs" ":" "{" value  ►► ("," value)* ◄◄  "}" | "nt_value" ":" nt_value | "spans" ":" value`
    #[derive(Debug, PartialEq)]
    pub struct SynGlobalOption1(pub Vec<SynValue>);
    /// Computed `("," Id)*` array in `nt_value -> "default" | "none" | "parents" | "set" "{" Id  ►► ("," Id)* ◄◄  "}"`
    #[derive(Debug, PartialEq)]
    pub struct SynNtValue1(pub Vec<String>);

    #[derive(Debug)]
    enum EnumSynValue { Config(SynConfig), Item(SynItem), Definition(SynDefinition), Lexer(SynLexer), Parser(SynParser), Options(SynOptions), IoOptions(SynIoOptions), IoOption(SynIoOption), TagOpt(SynTagOpt), GlobalOptions(SynGlobalOptions), GlobalOption(SynGlobalOption), Value(SynValue), NtValue(SynNtValue), IoOptions1(SynIoOptions1), IoOption1(SynIoOption1), GlobalOptions1(SynGlobalOptions1), GlobalOption1(SynGlobalOption1), NtValue1(SynNtValue1) }

    impl EnumSynValue {
        fn get_config(self) -> SynConfig {
            if let EnumSynValue::Config(val) = self { val } else { panic!() }
        }
        fn get_item(self) -> SynItem {
            if let EnumSynValue::Item(val) = self { val } else { panic!() }
        }
        fn get_definition(self) -> SynDefinition {
            if let EnumSynValue::Definition(val) = self { val } else { panic!() }
        }
        fn get_lexer(self) -> SynLexer {
            if let EnumSynValue::Lexer(val) = self { val } else { panic!() }
        }
        fn get_parser(self) -> SynParser {
            if let EnumSynValue::Parser(val) = self { val } else { panic!() }
        }
        fn get_options(self) -> SynOptions {
            if let EnumSynValue::Options(val) = self { val } else { panic!() }
        }
        fn get_io_options(self) -> SynIoOptions {
            if let EnumSynValue::IoOptions(val) = self { val } else { panic!() }
        }
        fn get_io_option(self) -> SynIoOption {
            if let EnumSynValue::IoOption(val) = self { val } else { panic!() }
        }
        fn get_tag_opt(self) -> SynTagOpt {
            if let EnumSynValue::TagOpt(val) = self { val } else { panic!() }
        }
        fn get_global_options(self) -> SynGlobalOptions {
            if let EnumSynValue::GlobalOptions(val) = self { val } else { panic!() }
        }
        fn get_global_option(self) -> SynGlobalOption {
            if let EnumSynValue::GlobalOption(val) = self { val } else { panic!() }
        }
        fn get_value(self) -> SynValue {
            if let EnumSynValue::Value(val) = self { val } else { panic!() }
        }
        fn get_nt_value(self) -> SynNtValue {
            if let EnumSynValue::NtValue(val) = self { val } else { panic!() }
        }
        fn get_io_options1(self) -> SynIoOptions1 {
            if let EnumSynValue::IoOptions1(val) = self { val } else { panic!() }
        }
        fn get_io_option1(self) -> SynIoOption1 {
            if let EnumSynValue::IoOption1(val) = self { val } else { panic!() }
        }
        fn get_global_options1(self) -> SynGlobalOptions1 {
            if let EnumSynValue::GlobalOptions1(val) = self { val } else { panic!() }
        }
        fn get_global_option1(self) -> SynGlobalOption1 {
            if let EnumSynValue::GlobalOption1(val) = self { val } else { panic!() }
        }
        fn get_nt_value1(self) -> SynNtValue1 {
            if let EnumSynValue::NtValue1(val) = self { val } else { panic!() }
        }
    }

    pub trait ConfigListener {
        /// Checks if the listener requests an abort. This happens if an error is too difficult to recover from
        /// and may corrupt the stack content. In that case, the parser immediately stops and returns `ParserError::AbortRequest`.
        fn check_abort_request(&self) -> Terminate { Terminate::None }
        fn get_log_mut(&mut self) -> &mut impl Logger;
        #[allow(unused_variables)]
        fn intercept_token(&mut self, token: TokenId, text: &str) -> TokenId { token }
        #[allow(unused_variables)]
        fn exit(&mut self, config: SynConfig) {}
        #[allow(unused_variables)]
        fn abort(&mut self, terminate: Terminate) {}
        fn init_config(&mut self) {}
        fn exit_config(&mut self, ctx: CtxConfig) -> SynConfig;
        #[allow(unused_variables)]
        fn exitloop_config(&mut self, config: &mut SynConfig) {}
        fn init_item(&mut self) {}
        fn exit_item(&mut self, ctx: CtxItem) -> SynItem;
        fn init_definition(&mut self) {}
        fn exit_definition(&mut self, ctx: CtxDefinition) -> SynDefinition;
        fn init_lexer(&mut self) {}
        fn exit_lexer(&mut self, ctx: CtxLexer) -> SynLexer;
        fn init_parser(&mut self) {}
        fn exit_parser(&mut self, ctx: CtxParser) -> SynParser;
        fn init_options(&mut self) {}
        fn exit_options(&mut self, ctx: CtxOptions) -> SynOptions;
        fn init_io_options(&mut self) {}
        fn exit_io_options(&mut self, ctx: CtxIoOptions) -> SynIoOptions;
        fn init_io_option(&mut self) {}
        fn exit_io_option(&mut self, ctx: CtxIoOption) -> SynIoOption;
        fn init_tag_opt(&mut self) {}
        fn exit_tag_opt(&mut self, ctx: CtxTagOpt) -> SynTagOpt;
        fn init_global_options(&mut self) {}
        fn exit_global_options(&mut self, ctx: CtxGlobalOptions) -> SynGlobalOptions;
        fn init_global_option(&mut self) {}
        fn exit_global_option(&mut self, ctx: CtxGlobalOption) -> SynGlobalOption;
        fn init_value(&mut self) {}
        fn exit_value(&mut self, ctx: CtxValue) -> SynValue;
        fn init_nt_value(&mut self) {}
        fn exit_nt_value(&mut self, ctx: CtxNtValue) -> SynNtValue;
    }

    pub struct Wrapper<T> {
        verbose: bool,
        listener: T,
        stack: Vec<EnumSynValue>,
        max_stack: usize,
        stack_t: Vec<String>,
    }

    impl<T: ConfigListener> ListenerWrapper for Wrapper<T> {
        fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
            if self.verbose {
                println!("switch: call={call:?}, nt={nt}, alt={alt_id}, t_data={t_data:?}");
            }
            if let Some(mut t_data) = t_data {
                self.stack_t.append(&mut t_data);
            }
            match call {
                Call::Enter => {
                    match nt {
                        0 => self.listener.init_config(),           // config
                        18 => {}                                    // config_1
                        1 => self.listener.init_item(),             // item
                        2 => self.listener.init_definition(),       // definition
                        3 => self.listener.init_lexer(),            // lexer
                        4 => self.listener.init_parser(),           // parser
                        5 => self.listener.init_options(),          // options
                        6 => self.listener.init_io_options(),       // io_options
                        13 => self.init_io_options1(),              // io_options_1
                        7 => self.listener.init_io_option(),        // io_option
                        14 => self.init_io_option1(),               // io_option_1
                        8 => self.listener.init_tag_opt(),          // tag_opt
                        9 => self.listener.init_global_options(),   // global_options
                        15 => self.init_global_options1(),          // global_options_1
                        10 => self.listener.init_global_option(),   // global_option
                        16 => self.init_global_option1(),           // global_option_1
                        11 => self.listener.init_value(),           // value
                        12 => self.listener.init_nt_value(),        // nt_value
                        17 => self.init_nt_value1(),                // nt_value_1
                        _ => panic!("unexpected enter nonterminal id: {nt}")
                    }
                }
                Call::Loop => {}
                Call::Exit => {
                    match alt_id {
                        0 => self.inter_config(),                   // config -> item config_1
                        39 => self.exit_config1(),                  // config_1 -> item config_1
                        40 => self.exitloop_config1(),              // config_1 -> ε
                        1 |                                         // item -> definition
                        2 |                                         // item -> lexer
                        3 |                                         // item -> parser
                        4 => self.exit_item(alt_id),                // item -> options
                        5 => self.exit_definition(),                // definition -> "def" Id "=" value ";"
                        6 => self.exit_lexer(),                     // lexer -> "lexer" "{" io_options "}"
                        7 => self.exit_parser(),                    // parser -> "parser" "{" io_options "}"
                        8 => self.exit_options(),                   // options -> "options" "{" global_options "}"
                        9 => self.exit_io_options(),                // io_options -> io_option io_options_1
                        29 => self.exit_io_options1(),              // io_options_1 -> "," io_option io_options_1
                        30 => {}                                    // io_options_1 -> ε
                        10 |                                        // io_option -> "combined" ":" value tag_opt
                        11 |                                        // io_option -> "input" ":" value tag_opt
                        12 |                                        // io_option -> "output" ":" value tag_opt
                        13 |                                        // io_option -> "indent" ":" value
                        14 => self.exit_io_option(alt_id),          // io_option -> "headers" ":" "{" value io_option_1 "}"
                        31 => self.exit_io_option1(),               // io_option_1 -> "," value io_option_1
                        32 => {}                                    // io_option_1 -> ε
                        15 |                                        // tag_opt -> "[" value "]"
                        16 => self.exit_tag_opt(alt_id),            // tag_opt -> ε
                        17 => self.exit_global_options(),           // global_options -> global_option global_options_1
                        33 => self.exit_global_options1(),          // global_options_1 -> "," global_option global_options_1
                        34 => {}                                    // global_options_1 -> ε
                        18 |                                        // global_option -> "libs" ":" "{" value global_option_1 "}"
                        19 |                                        // global_option -> "nt_value" ":" nt_value
                        20 => self.exit_global_option(alt_id),      // global_option -> "spans" ":" value
                        35 => self.exit_global_option1(),           // global_option_1 -> "," value global_option_1
                        36 => {}                                    // global_option_1 -> ε
                        21 |                                        // value -> BoolLiteral
                        22 |                                        // value -> NumLiteral
                        23 |                                        // value -> StrLiteral
                        24 => self.exit_value(alt_id),              // value -> Id
                        25 |                                        // nt_value -> "default"
                        26 |                                        // nt_value -> "none"
                        27 |                                        // nt_value -> "parents"
                        28 => self.exit_nt_value(alt_id),           // nt_value -> "set" "{" Id nt_value_1 "}"
                        37 => self.exit_nt_value1(),                // nt_value_1 -> "," Id nt_value_1
                        38 => {}                                    // nt_value_1 -> ε
                        _ => panic!("unexpected exit alternative id: {alt_id}")
                    }
                }
                Call::End(terminate) => {
                    match terminate {
                        Terminate::None => {
                            let val = self.stack.pop().unwrap().get_config();
                            self.listener.exit(val);
                        }
                        Terminate::Abort | Terminate::Conclude => self.listener.abort(terminate),
                    }
                }
            }
            self.max_stack = std::cmp::max(self.max_stack, self.stack.len());
            if self.verbose {
                println!("> stack_t:   {}", self.stack_t.join(", "));
                println!("> stack:     {}", self.stack.iter().map(|it| format!("{it:?}")).collect::<Vec<_>>().join(", "));
            }
        }

        fn check_abort_request(&self) -> Terminate {
            self.listener.check_abort_request()
        }

        fn abort(&mut self) {
            self.stack.clear();
            self.stack_t.clear();
        }

        fn get_log_mut(&mut self) -> &mut impl Logger {
            self.listener.get_log_mut()
        }

        fn is_stack_empty(&self) -> bool {
            self.stack.is_empty()
        }

        fn is_stack_t_empty(&self) -> bool {
            self.stack_t.is_empty()
        }

        fn intercept_token(&mut self, token: TokenId, text: &str, _span: &PosSpan) -> TokenId {
            self.listener.intercept_token(token, text)
        }
    }

    impl<T: ConfigListener> Wrapper<T> {
        pub fn new(listener: T, verbose: bool) -> Self {
            Wrapper { verbose, listener, stack: Vec::new(), max_stack: 0, stack_t: Vec::new() }
        }

        pub fn get_listener(&self) -> &T {
            &self.listener
        }

        pub fn get_listener_mut(&mut self) -> &mut T {
            &mut self.listener
        }

        pub fn give_listener(self) -> T {
            self.listener
        }

        pub fn set_verbose(&mut self, verbose: bool) {
            self.verbose = verbose;
        }

        fn inter_config(&mut self) {
            let item = self.stack.pop().unwrap().get_item();
            let ctx = CtxConfig::V2 { item };
            let val = self.listener.exit_config(ctx);
            self.stack.push(EnumSynValue::Config(val));
        }

        fn exit_config1(&mut self) {
            let item = self.stack.pop().unwrap().get_item();
            let config = self.stack.pop().unwrap().get_config();
            let ctx = CtxConfig::V1 { config, item };
            let val = self.listener.exit_config(ctx);
            self.stack.push(EnumSynValue::Config(val));
        }

        fn exitloop_config1(&mut self) {
            let EnumSynValue::Config(config) = self.stack.last_mut().unwrap() else { panic!() };
            self.listener.exitloop_config(config);
        }

        fn exit_item(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                1 => {
                    let definition = self.stack.pop().unwrap().get_definition();
                    CtxItem::V1 { definition }
                }
                2 => {
                    let lexer = self.stack.pop().unwrap().get_lexer();
                    CtxItem::V2 { lexer }
                }
                3 => {
                    let parser = self.stack.pop().unwrap().get_parser();
                    CtxItem::V3 { parser }
                }
                4 => {
                    let options = self.stack.pop().unwrap().get_options();
                    CtxItem::V4 { options }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_item")
            };
            let val = self.listener.exit_item(ctx);
            self.stack.push(EnumSynValue::Item(val));
        }

        fn exit_definition(&mut self) {
            let value = self.stack.pop().unwrap().get_value();
            let id = self.stack_t.pop().unwrap();
            let ctx = CtxDefinition::V1 { id, value };
            let val = self.listener.exit_definition(ctx);
            self.stack.push(EnumSynValue::Definition(val));
        }

        fn exit_lexer(&mut self) {
            let io_options = self.stack.pop().unwrap().get_io_options();
            let ctx = CtxLexer::V1 { io_options };
            let val = self.listener.exit_lexer(ctx);
            self.stack.push(EnumSynValue::Lexer(val));
        }

        fn exit_parser(&mut self) {
            let io_options = self.stack.pop().unwrap().get_io_options();
            let ctx = CtxParser::V1 { io_options };
            let val = self.listener.exit_parser(ctx);
            self.stack.push(EnumSynValue::Parser(val));
        }

        fn exit_options(&mut self) {
            let global_options = self.stack.pop().unwrap().get_global_options();
            let ctx = CtxOptions::V1 { global_options };
            let val = self.listener.exit_options(ctx);
            self.stack.push(EnumSynValue::Options(val));
        }

        fn exit_io_options(&mut self) {
            let star = self.stack.pop().unwrap().get_io_options1();
            let ctx = CtxIoOptions::V1 { star };
            let val = self.listener.exit_io_options(ctx);
            self.stack.push(EnumSynValue::IoOptions(val));
        }

        fn init_io_options1(&mut self) {
            let io_option = self.stack.pop().unwrap().get_io_option();
            self.stack.push(EnumSynValue::IoOptions1(SynIoOptions1(vec![io_option])));
        }

        fn exit_io_options1(&mut self) {
            let io_option = self.stack.pop().unwrap().get_io_option();
            let Some(EnumSynValue::IoOptions1(SynIoOptions1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynIoOptions1 item on wrapper stack");
            };
            star_acc.push(io_option);
        }

        fn exit_io_option(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                10 => {
                    let tag_opt = self.stack.pop().unwrap().get_tag_opt();
                    let value = self.stack.pop().unwrap().get_value();
                    CtxIoOption::V1 { value, tag_opt }
                }
                11 => {
                    let tag_opt = self.stack.pop().unwrap().get_tag_opt();
                    let value = self.stack.pop().unwrap().get_value();
                    CtxIoOption::V2 { value, tag_opt }
                }
                12 => {
                    let tag_opt = self.stack.pop().unwrap().get_tag_opt();
                    let value = self.stack.pop().unwrap().get_value();
                    CtxIoOption::V3 { value, tag_opt }
                }
                13 => {
                    let value = self.stack.pop().unwrap().get_value();
                    CtxIoOption::V4 { value }
                }
                14 => {
                    let star = self.stack.pop().unwrap().get_io_option1();
                    CtxIoOption::V5 { star }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_io_option")
            };
            let val = self.listener.exit_io_option(ctx);
            self.stack.push(EnumSynValue::IoOption(val));
        }

        fn init_io_option1(&mut self) {
            let value = self.stack.pop().unwrap().get_value();
            self.stack.push(EnumSynValue::IoOption1(SynIoOption1(vec![value])));
        }

        fn exit_io_option1(&mut self) {
            let value = self.stack.pop().unwrap().get_value();
            let Some(EnumSynValue::IoOption1(SynIoOption1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynIoOption1 item on wrapper stack");
            };
            star_acc.push(value);
        }

        fn exit_tag_opt(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                15 => {
                    let value = self.stack.pop().unwrap().get_value();
                    CtxTagOpt::V1 { value }
                }
                16 => {
                    CtxTagOpt::V2
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_tag_opt")
            };
            let val = self.listener.exit_tag_opt(ctx);
            self.stack.push(EnumSynValue::TagOpt(val));
        }

        fn exit_global_options(&mut self) {
            let star = self.stack.pop().unwrap().get_global_options1();
            let ctx = CtxGlobalOptions::V1 { star };
            let val = self.listener.exit_global_options(ctx);
            self.stack.push(EnumSynValue::GlobalOptions(val));
        }

        fn init_global_options1(&mut self) {
            let global_option = self.stack.pop().unwrap().get_global_option();
            self.stack.push(EnumSynValue::GlobalOptions1(SynGlobalOptions1(vec![global_option])));
        }

        fn exit_global_options1(&mut self) {
            let global_option = self.stack.pop().unwrap().get_global_option();
            let Some(EnumSynValue::GlobalOptions1(SynGlobalOptions1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynGlobalOptions1 item on wrapper stack");
            };
            star_acc.push(global_option);
        }

        fn exit_global_option(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                18 => {
                    let star = self.stack.pop().unwrap().get_global_option1();
                    CtxGlobalOption::V1 { star }
                }
                19 => {
                    let nt_value = self.stack.pop().unwrap().get_nt_value();
                    CtxGlobalOption::V2 { nt_value }
                }
                20 => {
                    let value = self.stack.pop().unwrap().get_value();
                    CtxGlobalOption::V3 { value }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_global_option")
            };
            let val = self.listener.exit_global_option(ctx);
            self.stack.push(EnumSynValue::GlobalOption(val));
        }

        fn init_global_option1(&mut self) {
            let value = self.stack.pop().unwrap().get_value();
            self.stack.push(EnumSynValue::GlobalOption1(SynGlobalOption1(vec![value])));
        }

        fn exit_global_option1(&mut self) {
            let value = self.stack.pop().unwrap().get_value();
            let Some(EnumSynValue::GlobalOption1(SynGlobalOption1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynGlobalOption1 item on wrapper stack");
            };
            star_acc.push(value);
        }

        fn exit_value(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                21 => {
                    let boolliteral = self.stack_t.pop().unwrap();
                    CtxValue::V1 { boolliteral }
                }
                22 => {
                    let numliteral = self.stack_t.pop().unwrap();
                    CtxValue::V2 { numliteral }
                }
                23 => {
                    let strliteral = self.stack_t.pop().unwrap();
                    CtxValue::V3 { strliteral }
                }
                24 => {
                    let id = self.stack_t.pop().unwrap();
                    CtxValue::V4 { id }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_value")
            };
            let val = self.listener.exit_value(ctx);
            self.stack.push(EnumSynValue::Value(val));
        }

        fn exit_nt_value(&mut self, alt_id: AltId) {
            let ctx = match alt_id {
                25 => {
                    CtxNtValue::V1
                }
                26 => {
                    CtxNtValue::V2
                }
                27 => {
                    CtxNtValue::V3
                }
                28 => {
                    let star = self.stack.pop().unwrap().get_nt_value1();
                    CtxNtValue::V4 { star }
                }
                _ => panic!("unexpected alt id {alt_id} in fn exit_nt_value")
            };
            let val = self.listener.exit_nt_value(ctx);
            self.stack.push(EnumSynValue::NtValue(val));
        }

        fn init_nt_value1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            self.stack.push(EnumSynValue::NtValue1(SynNtValue1(vec![id])));
        }

        fn exit_nt_value1(&mut self) {
            let id = self.stack_t.pop().unwrap();
            let Some(EnumSynValue::NtValue1(SynNtValue1(star_acc))) = self.stack.last_mut() else {
                panic!("expected SynNtValue1 item on wrapper stack");
            };
            star_acc.push(id);
        }
    }

    // [config_parser]
}
