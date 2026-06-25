// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexigram_core::log::{LogReader, LogStatus};
use lexigram_core::{strip, CollectJoin};
use crate::{btreemap, symbols};
use crate::grammar::tests::TestRules;
use crate::parsergen::{NTValue, ParserGen, ParserType};
use crate::parsergen::tests::wrapper_source::{build_items, BuildItemsTestEntry, BuildItemsTestSpec};

// List in decreasing order of test parser number of the file were they're generated:
static WRAPPER_FILENAMES: &[(u32, &str)] = &[
    (980, "tests/out/wrapper_source4.rs"),      //       n >= 980 are generated here
    (630, "tests/out/wrapper_source3.rs"),      // 980 > n >= 630 are generated here
    (300, "tests/out/wrapper_source2.rs"),      // 630 > n >= 300
    (200, "tests/out/wrapper_source1.rs"),      // ...
    (  0, "tests/out/wrapper_source.rs"),
];

fn get_ll1_tests() -> Vec<BuildItemsTestEntry> {
    vec![
        // BuildItemsTestEntry fields for each test:
        // - TestRules #
        // - test sources?
        // - test sources include parser?
        // - use super::super::wrapper_code::...?
        // - start NT
        // - NT types
        // - expected opcodes, span, items for each alt
        // - which symbols have a value
        // - expected alt groups
        //
        // CAUTION! Empty the first btreemap if the NTs have changed
        // ---------------------------------------------------------------------------
        // a -> A B
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // +-------------------------+
        (1, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 1, t 0],              2, symbols![t 0, t 1]), //  0: a -> A B | ◄0 B! A! | 2 | A B
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- NT/T simple mix
        // s -> Id "=" val | "exit" | "return" val
        // val -> Id | Num
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | s    | y  |       |
        // |   1 | val  | y  |       |
        // +-------------------------+
        (13, true, false, true, 0, btreemap![
            0 => "SynS".to_string(),
            1 => "SynVal".to_string(),
        ], vec![
            (strip![exit 0, nt 1, t 1, t 0],        3, symbols![t 0, nt 1]), //  0: s -> Id "=" val   | ◄0 ►val "=" Id!  | 3 | Id val
            (strip![exit 1, t 2],                   1, symbols![]),          //  1: s -> "exit"       | ◄1 "exit"        | 1 |
            (strip![exit 2, nt 1, t 3],             2, symbols![nt 1]),      //  2: s -> "return" val | ◄2 ►val "return" | 2 | val
            (strip![exit 3, t 0],                   1, symbols![t 0]),       //  3: val -> Id         | ◄3 Id!           | 1 | Id
            (strip![exit 4, t 4],                   1, symbols![t 4]),       //  4: val -> Num        | ◄4 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),

        // --------------------------------------------------------------------------- NT with/without value
        // a -> b c | c
        // b -> Op c
        // c -> Id
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // |   1 | b    | y  |       |
        // |   2 | c    | y  |       |
        // +-------------------------+
        (14, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]), //  0: a -> b c  | ◄0 ►c ►b  | 2 | b c
            (strip![exit 1, nt 2],                  1, symbols![nt 2]),       //  1: a -> c    | ◄1 ►c     | 1 | c
            (strip![exit 2, nt 2, t 0],             2, symbols![t 0, nt 2]),  //  2: b -> Op c | ◄2 ►c Op! | 2 | Op c
            (strip![exit 3, t 1],                   1, symbols![t 1]),        //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], true, NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
        // a: y, b: y, c: n
        (14, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1]), //  0: a -> b c  | ◄0 ►c ►b  | 2 | b
            (strip![exit 1, nt 2],                  1, symbols![]),     //  1: a -> c    | ◄1 ►c     | 1 |
            (strip![exit 2, nt 2, t 0],             2, symbols![t 0]),  //  2: b -> Op c | ◄2 ►c Op! | 2 | Op
            (strip![exit 3, t 1],                   1, symbols![t 1]),  //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], true, NTValue::SetIds(vec![0, 1]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
        // a: y, b: n, c: y
        (14, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 2]),      //  0: a -> b c  | ◄0 ►c ►b  | 2 | c
            (strip![exit 1, nt 2],                  1, symbols![nt 2]),      //  1: a -> c    | ◄1 ►c     | 1 | c
            (strip![exit 2, nt 2, t 0],             2, symbols![t 0, nt 2]), //  2: b -> Op c | ◄2 ►c Op! | 2 | Op c
            (strip![exit 3, t 1],                   1, symbols![t 1]),       //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], true, NTValue::SetIds(vec![0, 2]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
        // a: y, b: n, c: n
        (14, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![]),    //  0: a -> b c  | ◄0 ►c ►b  | 2 |
            (strip![exit 1, nt 2],                  1, symbols![]),    //  1: a -> c    | ◄1 ►c     | 1 |
            (strip![exit 2, nt 2, t 0],             2, symbols![t 0]), //  2: b -> Op c | ◄2 ►c Op! | 2 | Op
            (strip![exit 3, t 1],                   1, symbols![t 1]), //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),

        // a -> b | c | d
        // b -> Op d
        // c -> Id
        // d -> Num
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | b    | y  |       |
        // |   1 | d    | y  |       |
        // +-------------------------+
        (15, true, false, true, 1, btreemap![
        ], vec![
            (strip![exit 0, nt 1, t 0],             2, symbols![t 0, nt 1]), //  0: b -> Op d | ◄0 ►d Op! | 2 | Op d
            (strip![exit 1, t 2],                   1, symbols![t 2]),       //  1: d -> Num  | ◄1 Num!   | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // --------------------------------------------------------------------------- +_or_*
        // a -> A B* C
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*  |
        // +----------------------------------+
        (102, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, nt 1, t 2]), //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),      //  1: a_1 -> B a_1 | ●a_1 ◄1 B!    | 2 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: a_1 -> ε     | ◄2            | 1 | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: n, a_1: y
        (102, false, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, nt 1, t 2]), //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),      //  1: a_1 -> B a_1 | ●a_1 ◄1 B!    | 2 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: a_1 -> ε     | ◄2            | 1 | a_1
        ], true, NTValue::None, btreemap![0 => vec![0]]),

        // a -> A B+ C
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*, plus                  |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   2 | .   a_2 |    | child_left_fact                      |
        // +-----------------------------------------------------------+
        (103, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, nt 1, t 2]), //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
            (strip![nt 2, t 1],                     0, symbols![]),               //  1: a_1 -> B a_2 | ►a_2 B!       | 0 |
            (strip![loop 1, exit 2],                2, symbols![nt 1, t 1]),      //  2: a_2 -> a_1   | ●a_1 ◄2       | 2 | a_1 B
            (strip![exit 3],                        2, symbols![nt 1, t 1]),      //  3: a_2 -> ε     | ◄3            | 2 | a_1 B
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (b A b B A)*
        // b -> C
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   2 | . a_1 | y  | child_+_or_*  |
        // |   1 | b     | y  |               |
        // +----------------------------------+
        (104, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2],                              1, symbols![nt 2]),                            //  0: a -> a_1             | ◄0 ►a_1                | 1 | a_1
            (strip![exit 1, t 2],                               1, symbols![t 2]),                             //  1: b -> C               | ◄1 C!                  | 1 | C
            (strip![loop 2, exit 2, t 0, t 1, nt 1, t 0, nt 1], 6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  2: a_1 -> b A b B A a_1 | ●a_1 ◄2 A! B! ►b A! ►b | 6 | a_1 b A b B A
            (strip![exit 3],                                    1, symbols![nt 2]),                            //  3: a_1 -> ε             | ◄3                     | 1 | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (b A b B A)+
        // b -> C
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*, plus                  |
        // |   2 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   3 | .   a_2 |    | child_left_fact                      |
        // |   1 | b       | y  |                                      |
        // +-----------------------------------------------------------+
        (105, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2],                    1, symbols![nt 2]),                            //  0: a -> a_1             | ◄0 ►a_1             | 1 | a_1
            (strip![exit 1, t 2],                     1, symbols![t 2]),                             //  1: b -> C               | ◄1 C!               | 1 | C
            (strip![nt 3, t 0, t 1, nt 1, t 0, nt 1], 0, symbols![]),                                //  2: a_1 -> b A b B A a_2 | ►a_2 A! B! ►b A! ►b | 0 |
            (strip![loop 2, exit 3],                  6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  3: a_2 -> a_1           | ●a_1 ◄3             | 6 | a_1 b A b B A
            (strip![exit 4],                          6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  4: a_2 -> ε             | ◄4                  | 6 | a_1 b A b B A
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (A (b ",")* ";")* C
        // b -> B
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*               |
        // |   3 | . a_2   | y  | child_+_or_*, parent_+_or_* |
        // |   2 | .   a_1 | y  | child_+_or_*                |
        // |   1 | b       | y  |                             |
        // +--------------------------------------------------+
        (106, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 3],              2, symbols![nt 3, t 3]),       //  0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
            (strip![exit 1, t 4],                    1, symbols![t 4]),             //  1: b -> B               | ◄1 B!               | 1 | B
            (strip![loop 2, exit 2, t 1, nt 1],      3, symbols![nt 2, nt 1]),      //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 | a_1 b
            (strip![exit 3],                         1, symbols![nt 2]),            //  3: a_1 -> ε             | ◄3                  | 1 | a_1
            (strip![loop 3, exit 4, t 2, nt 2, t 0], 4, symbols![nt 3, t 0, nt 2]), //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A a_1
            (strip![exit 5],                         1, symbols![nt 3]),            //  5: a_2 -> ε             | ◄5                  | 1 | a_2
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),
        // a: y, b: n, a_1: n, a_2: y
        (106, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 3],              2, symbols![nt 3, t 3]), //  0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
            (strip![exit 1, t 4],                    1, symbols![t 4]),       //  1: b -> B               | ◄1 B!               | 1 | B
            (strip![loop 2, exit 2, t 1, nt 1],      3, symbols![]),          //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 |
            (strip![exit 3],                         1, symbols![]),          //  3: a_1 -> ε             | ◄3                  | 1 |
            (strip![loop 3, exit 4, t 2, nt 2, t 0], 4, symbols![nt 3, t 0]), //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A
            (strip![exit 5],                         1, symbols![nt 3]),      //  5: a_2 -> ε             | ◄5                  | 1 | a_2
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (A (b ",")+ ";")+ C
        // b -> B
        //
        //   NT    name       val   flags
        // +----------------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                                 |
        // |   3 | . a_2     | y  | child_+_or_*, parent_left_fact, parent_+_or_*, plus |
        // |   2 | .   a_1   |    | child_+_or_*, parent_left_fact, plus                |
        // |   4 | .   . a_3 |    | child_left_fact                                     |
        // |   5 | .   a_4   |    | child_left_fact                                     |
        // |   1 | b         |    |                                                     |
        // +----------------------------------------------------------------------------+
        (107, false, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 3],             2, symbols![nt 3, t 3]), //  0: a -> a_2 C           | ◄0 C! ►a_2       | 2 | a_2 C
            (strip![exit 1, t 4],                   1, symbols![t 4]),       //  1: b -> B               | ◄1 B!            | 1 | B
            (strip![nt 4, t 1, nt 1],               0, symbols![]),          //  2: a_1 -> b "," a_3     | ►a_3 "," ►b      | 0 |
            (strip![nt 5, t 2, nt 2, t 0],          0, symbols![]),          //  3: a_2 -> A a_1 ";" a_4 | ►a_4 ";" ►a_1 A! | 0 |
            (strip![loop 2, exit 4],                3, symbols![]),          //  4: a_3 -> a_1           | ●a_1 ◄4          | 3 |
            (strip![exit 5],                        3, symbols![]),          //  5: a_3 -> ε             | ◄5               | 3 |
            (strip![loop 3, exit 6],                4, symbols![nt 3, t 0]), //  6: a_4 -> a_2           | ●a_2 ◄6          | 4 | a_2 A
            (strip![exit 7],                        4, symbols![nt 3, t 0]), //  7: a_4 -> ε             | ◄7               | 4 | a_2 A
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> A "B"* C
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 |    | child_+_or_*  |
        // +----------------------------------+
        (108, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, t 2]), //  0: a -> A a_1 C   | ◄0 C! ►a_1 A! | 3 | A C
            (strip![loop 1, exit 1, t 1],           2, symbols![]),         //  1: a_1 -> "B" a_1 | ●a_1 ◄1 "B"   | 2 |
            (strip![exit 2],                        1, symbols![]),         //  2: a_1 -> ε       | ◄2            | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> Id "(" (Id ":" type / ",")+ ")"
        // type -> Id
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   2 | . a_1 | y  | child_+_or_*, sep_list |
        // |   1 | type  | y  |                        |
        // +-------------------------------------------+
        (109, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 2, nt 1, t 2, t 0, t 1, t 0], 4, symbols![t 0, nt 2]),       //  0: a -> Id "(" Id ":" type a_1 ")" | ◄0 ")" ►a_1 ►type ":" Id! "(" Id! | 4    | Id a_1
            (strip![exit 1, t 0],                                 1, symbols![t 0]),             //  1: type -> Id                      | ◄1 Id!                            | 1    | Id
            (strip![loop 2, exit 2, nt 1, t 2, t 0, t 3],         5, symbols![nt 2, t 0, nt 1]), //  2: a_1 -> "," Id ":" type a_1      | ●a_1 ◄2 ►type ":" Id! ","         | 5, 3 | a_1 Id type
            (strip![exit 3],                                      1, symbols![nt 2]),            //  3: a_1 -> ε                        | ◄3                                | 1    | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> Id "(" ((Id ":" type / ",")+)? ")"
        // type -> Id
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   2 | . a_1 | y  | child_+_or_*, sep_list          |
        // |   3 | . a_2 |    | child_left_fact                 |
        // |   1 | type  | y  |                                 |
        // +----------------------------------------------------+
        (110, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 3, t 1, t 0],                      0, symbols![]),                //  0: a -> Id "(" a_2            | ►a_2 "(" Id!              | 0    |
            (strip![exit 1, t 0],                         1, symbols![t 0]),             //  1: type -> Id                 | ◄1 Id!                    | 1    | Id
            (strip![loop 2, exit 2, nt 1, t 2, t 0, t 3], 5, symbols![nt 2, t 0, nt 1]), //  2: a_1 -> "," Id ":" type a_1 | ●a_1 ◄2 ►type ":" Id! "," | 5, 3 | a_1 Id type
            (strip![exit 3],                              1, symbols![nt 2]),            //  3: a_1 -> ε                   | ◄3                        | 1    | a_1
            (strip![exit 4, t 4, nt 2, nt 1, t 2, t 0],   4, symbols![t 0, nt 2]),       //  4: a_2 -> Id ":" type a_1 ")" | ◄4 ")" ►a_1 ►type ":" Id! | 4    | Id a_1
            (strip![exit 5, t 4],                         3, symbols![t 0]),             //  5: a_2 -> ")"                 | ◄5 ")"                    | 3    | Id
        ], true, NTValue::Default, btreemap![0 => vec![4, 5], 1 => vec![1]]),

        // a -> Id "(" (Id / ",")+ "/" (Id / ",")+ ")"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // |   2 | . a_2 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (111, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 2, t 0, t 3, nt 1, t 0, t 1, t 0], 6, symbols![t 0, nt 1, nt 2]), //  0: a -> Id "(" Id a_1 "/" Id a_2 ")" | ◄0 ")" ►a_2 Id! "/" ►a_1 Id! "(" Id! | 6    | Id a_1 a_2
            (strip![loop 1, exit 1, t 0, t 2],                         3, symbols![nt 1, t 0]),       //  1: a_1 -> "," Id a_1                 | ●a_1 ◄1 Id! ","                      | 3, 1 | a_1 Id
            (strip![exit 2],                                           1, symbols![nt 1]),            //  2: a_1 -> ε                          | ◄2                                   | 1    | a_1
            (strip![loop 2, exit 3, t 0, t 2],                         3, symbols![nt 2, t 0]),       //  3: a_2 -> "," Id a_2                 | ●a_2 ◄3 Id! ","                      | 3, 1 | a_2 Id
            (strip![exit 4],                                           1, symbols![nt 2]),            //  4: a_2 -> ε                          | ◄4                                   | 1    | a_2
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> "let" (Id / ",")+ "=" (Num / ",")+ ";"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // |   2 | . a_2 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (112, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 5, nt 2, t 4, t 3, nt 1, t 1, t 0], 5, symbols![nt 1, nt 2]), //  0: a -> "let" Id a_1 "=" Num a_2 ";" | ◄0 ";" ►a_2 Num! "=" ►a_1 Id! "let" | 5    | a_1 a_2
            (strip![loop 1, exit 1, t 1, t 2],                    3, symbols![nt 1, t 1]),  //  1: a_1 -> "," Id a_1                 | ●a_1 ◄1 Id! ","                     | 3, 1 | a_1 Id
            (strip![exit 2],                                      1, symbols![nt 1]),       //  2: a_1 -> ε                          | ◄2                                  | 1    | a_1
            (strip![loop 2, exit 3, t 4, t 2],                    3, symbols![nt 2, t 4]),  //  3: a_2 -> "," Num a_2                | ●a_2 ◄3 Num! ","                    | 3, 1 | a_2 Num
            (strip![exit 4],                                      1, symbols![nt 2]),       //  4: a_2 -> ε                          | ◄4                                  | 1    | a_2
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> X (B / ",")+ B? Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list          |
        // |   2 | . a_2 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (113, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1, t 1, t 0],          0, symbols![]),                    //  0: a -> X B a_1 a_2 | ►a_2 ►a_1 B! X! | 0    |
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),           //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! ","  | 3, 1 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),                //  2: a_1 -> ε         | ◄2              | 1    | a_1
            (strip![exit 3, t 3, t 1],              4, symbols![t 0, nt 1, t 1, t 3]), //  3: a_2 -> B Z       | ◄3 Z! B!        | 4    | X a_1 B Z
            (strip![exit 4, t 3],                   3, symbols![t 0, nt 1, t 3]),      //  4: a_2 -> Z         | ◄4 Z!           | 3    | X a_1 Z
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X? (B / ",")+ Z
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (114, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![t 0, nt 1, t 3]), //  0: a -> X B a_1 Z   | ◄0 Z! ►a_1 B! X! | 3    | X a_1 Z
            (strip![exit 1, t 3, nt 1, t 1],        2, symbols![nt 1, t 3]),      //  1: a -> B a_1 Z     | ◄1 Z! ►a_1 B!    | 2    | a_1 Z
            (strip![loop 1, exit 2, t 1, t 2],      3, symbols![nt 1, t 1]),      //  2: a_1 -> "," B a_1 | ●a_1 ◄2 B! ","   | 3, 1 | a_1 B
            (strip![exit 3],                        1, symbols![nt 1]),           //  3: a_1 -> ε         | ◄3               | 1    | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> X Y? (B / ",")+ Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list          |
        // |   2 | . a_2 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (115, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 2, t 0],                     0, symbols![]),                    //  0: a -> X a_2       | ►a_2 X!          | 0    |
            (strip![loop 1, exit 1, t 2, t 3],      3, symbols![nt 1, t 2]),           //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! ","   | 3, 1 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),                //  2: a_1 -> ε         | ◄2               | 1    | a_1
            (strip![exit 3, t 4, nt 1, t 2, t 1],   4, symbols![t 0, t 1, nt 1, t 4]), //  3: a_2 -> Y B a_1 Z | ◄3 Z! ►a_1 B! Y! | 4    | X Y a_1 Z
            (strip![exit 4, t 4, nt 1, t 2],        3, symbols![t 0, nt 1, t 4]),      //  4: a_2 -> B a_1 Z   | ◄4 Z! ►a_1 B!    | 3    | X a_1 Z
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X B? ("," B)* Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*                    |
        // |   2 | . a_2 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (116, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 2, t 0],                     0, symbols![]),                    //  0: a -> X a_2       | ►a_2 X!        | 0 |
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),           //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! "," | 3 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),                //  2: a_1 -> ε         | ◄2             | 1 | a_1
            (strip![exit 3, t 3, nt 1, t 1],        4, symbols![t 0, t 1, nt 1, t 3]), //  3: a_2 -> B a_1 Z   | ◄3 Z! ►a_1 B!  | 4 | X B a_1 Z
            (strip![exit 4, t 3, nt 1],             3, symbols![t 0, nt 1, t 3]),      //  4: a_2 -> a_1 Z     | ◄4 Z! ►a_1     | 3 | X a_1 Z
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> "var" (Id / ",")+ ";"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (118, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![nt 1]),      //  0: a -> "var" Id a_1 ";" | ◄0 ";" ►a_1 Id! "var" | 3    | a_1
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]), //  1: a_1 -> "," Id a_1     | ●a_1 ◄1 Id! ","       | 3, 1 | a_1 Id
            (strip![exit 2],                        1, symbols![nt 1]),      //  2: a_1 -> ε              | ◄2                    | 1    | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> X (B / ",")+ Z
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (119, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![t 0, nt 1, t 3]), //  0: a -> X B a_1 Z   | ◄0 Z! ►a_1 B! X! | 3    | X a_1 Z
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),      //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! ","   | 3, 1 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: a_1 -> ε         | ◄2               | 1    | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A B* C+ D
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                        |
        // |   1 | . a_1   | y  | child_+_or_*                         |
        // |   2 | . a_2   | y  | child_+_or_*, parent_left_fact, plus |
        // |   3 | .   a_3 |    | child_left_fact                      |
        // +-----------------------------------------------------------+
        (123, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 2, nt 1, t 0],  4, symbols![t 0, nt 1, nt 2, t 3]), //  0: a -> A a_1 a_2 D | ◄0 D! ►a_2 ►a_1 A! | 4 | A a_1 a_2 D
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),            //  1: a_1 -> B a_1     | ●a_1 ◄1 B!         | 2 | a_1 B
            (strip![exit 2],                        1, symbols![nt 1]),                 //  2: a_1 -> ε         | ◄2                 | 1 | a_1
            (strip![nt 3, t 2],                     0, symbols![]),                     //  3: a_2 -> C a_3     | ►a_3 C!            | 0 |
            (strip![loop 2, exit 4],                2, symbols![nt 2, t 2]),            //  4: a_3 -> a_2       | ●a_2 ◄4            | 2 | a_2 C
            (strip![exit 5],                        2, symbols![nt 2, t 2]),            //  5: a_3 -> ε         | ◄5                 | 2 | a_2 C
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (Id / ",")+* B
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*               |
        // |   2 | . a_2   | y  | child_+_or_*, parent_+_or_* |
        // |   1 | .   a_1 | y  | child_+_or_*, sep_list      |
        // +--------------------------------------------------+
        (125, false, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 2, t 0],        3, symbols![t 0, nt 2, t 3]), //  0: a -> A a_2 B      | ◄0 B! ►a_2 A!    | 3    | A a_2 B
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),      //  1: a_1 -> "," Id a_1 | ●a_1 ◄1 Id! ","  | 3, 1 | a_1 Id
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: a_1 -> ε          | ◄2               | 1    | a_1
            (strip![loop 2, exit 3, nt 1, t 1],     2, symbols![nt 2, nt 1]),     //  3: a_2 -> Id a_1 a_2 | ●a_2 ◄3 ►a_1 Id! | 2    | a_2 a_1
            (strip![exit 4],                        1, symbols![nt 2]),           //  4: a_2 -> ε          | ◄4               | 1    | a_2
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (Id / ",")++ B
        //
        //   NT    name     val   flags
        // +--------------------------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                                       |
        // |   2 | . a_2   | y  | child_+_or_*, parent_left_fact, parent_+_or_*, plus |
        // |   1 | .   a_1 | y  | child_+_or_*, sep_list                              |
        // |   3 | .   a_3 |    | child_left_fact                                     |
        // +--------------------------------------------------------------------------+
        (126, false, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 2, t 0],        3, symbols![t 0, nt 2, t 3]), //  0: a -> A a_2 B      | ◄0 B! ►a_2 A!   | 3    | A a_2 B
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),      //  1: a_1 -> "," Id a_1 | ●a_1 ◄1 Id! "," | 3, 1 | a_1 Id
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: a_1 -> ε          | ◄2              | 1    | a_1
            (strip![nt 3, nt 1, t 1],               0, symbols![]),               //  3: a_2 -> Id a_1 a_3 | ►a_3 ►a_1 Id!   | 0    |
            (strip![loop 2, exit 4],                2, symbols![nt 2, nt 1]),     //  4: a_3 -> a_2        | ●a_2 ◄4         | 2    | a_2 a_1
            (strip![exit 5],                        2, symbols![nt 2, nt 1]),     //  5: a_3 -> ε          | ◄5              | 2    | a_2 a_1
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- norm+/* alternatives
        // a -> A (B | C D)* E
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*  |
        // +----------------------------------+
        (150, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 1, t 0],        3, symbols![t 0, nt 1, t 4]), //  0: a -> A a_1 E   | ◄0 E! ►a_1 A! | 3 | A a_1 E
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),      //  1: a_1 -> B a_1   | ●a_1 ◄1 B!    | 2 | a_1 B
            (strip![loop 1, exit 2, t 3, t 2],      3, symbols![nt 1, t 2, t 3]), //  2: a_1 -> C D a_1 | ●a_1 ◄2 D! C! | 3 | a_1 C D
            (strip![exit 3],                        1, symbols![nt 1]),           //  3: a_1 -> ε       | ◄3            | 1 | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (B | C D)+ E
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                        |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   2 | .   a_2 |    | child_left_fact                      |
        // |   3 | .   a_3 |    | child_left_fact                      |
        // +-----------------------------------------------------------+
        (151, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 1, t 0],        3, symbols![t 0, nt 1, t 4]), //  0: a -> A a_1 E   | ◄0 E! ►a_1 A! | 3 | A a_1 E
            (strip![nt 2, t 1],                     0, symbols![]),               //  1: a_1 -> B a_2   | ►a_2 B!       | 0 |
            (strip![nt 3, t 3, t 2],                0, symbols![]),               //  2: a_1 -> C D a_3 | ►a_3 D! C!    | 0 |
            (strip![loop 1, exit 3],                2, symbols![nt 1, t 1]),      //  3: a_2 -> a_1     | ●a_1 ◄3       | 2 | a_1 B
            (strip![exit 4],                        2, symbols![nt 1, t 1]),      //  4: a_2 -> ε       | ◄4            | 2 | a_1 B
            (strip![loop 1, exit 5],                3, symbols![nt 1, t 2, t 3]), //  5: a_3 -> a_1     | ●a_1 ◄5       | 3 | a_1 C D
            (strip![exit 6],                        3, symbols![nt 1, t 2, t 3]), //  6: a_3 -> ε       | ◄6            | 3 | a_1 C D
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (B | b C b B C | E)* F
        // b -> D
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   2 | . a_1 | y  | child_+_or_*  |
        // |   1 | b     | y  |               |
        // +----------------------------------+
        (152, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 2, t 0],                    3, symbols![t 0, nt 2, t 4]),                  //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!          | 3 | A a_1 F
            (strip![exit 1, t 5],                               1, symbols![t 5]),                             //  1: b -> D               | ◄1 D!                  | 1 | D
            (strip![loop 2, exit 2, t 1],                       2, symbols![nt 2, t 1]),                       //  2: a_1 -> B a_1         | ●a_1 ◄2 B!             | 2 | a_1 B
            (strip![loop 2, exit 3, t 2, t 1, nt 1, t 2, nt 1], 6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), //  3: a_1 -> b C b B C a_1 | ●a_1 ◄3 C! B! ►b C! ►b | 6 | a_1 b C b B C
            (strip![loop 2, exit 4, t 3],                       2, symbols![nt 2, t 3]),                       //  4: a_1 -> E a_1         | ●a_1 ◄4 E!             | 2 | a_1 E
            (strip![exit 5],                                    1, symbols![nt 2]),                            //  5: a_1 -> ε             | ◄5                     | 1 | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> A (B | b C b B C | E)+ F
        // b -> D
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*, plus                  |
        // |   2 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   3 | .   a_2 |    | child_left_fact                      |
        // |   4 | .   a_3 |    | child_left_fact                      |
        // |   5 | .   a_4 |    | child_left_fact                      |
        // |   1 | b       | y  |                                      |
        // +-----------------------------------------------------------+
        (153, true, true, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 2, t 0],          3, symbols![t 0, nt 2, t 4]),                  //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!       | 3 | A a_1 F
            (strip![exit 1, t 5],                     1, symbols![t 5]),                             //  1: b -> D               | ◄1 D!               | 1 | D
            (strip![nt 3, t 1],                       0, symbols![]),                                //  2: a_1 -> B a_2         | ►a_2 B!             | 0 |
            (strip![nt 4, t 3],                       0, symbols![]),                                //  3: a_1 -> E a_3         | ►a_3 E!             | 0 |
            (strip![nt 5, t 2, t 1, nt 1, t 2, nt 1], 0, symbols![]),                                //  4: a_1 -> b C b B C a_4 | ►a_4 C! B! ►b C! ►b | 0 |
            (strip![loop 2, exit 5],                  2, symbols![nt 2, t 1]),                       //  5: a_2 -> a_1           | ●a_1 ◄5             | 2 | a_1 B
            (strip![exit 6],                          2, symbols![nt 2, t 1]),                       //  6: a_2 -> ε             | ◄6                  | 2 | a_1 B
            (strip![loop 2, exit 7],                  2, symbols![nt 2, t 3]),                       //  7: a_3 -> a_1           | ●a_1 ◄7             | 2 | a_1 E
            (strip![exit 8],                          2, symbols![nt 2, t 3]),                       //  8: a_3 -> ε             | ◄8                  | 2 | a_1 E
            (strip![loop 2, exit 9],                  6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), //  9: a_4 -> a_1           | ●a_1 ◄9             | 6 | a_1 b C b B C
            (strip![exit 10],                         6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), // 10: a_4 -> ε             | ◄10                 | 6 | a_1 b C b B C
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (A | A B | C)*
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                  |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact |
        // |   2 | .   a_2 |    | child_left_fact                |
        // +-----------------------------------------------------+
        (154, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),           //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
            (strip![nt 2, t 0],                     0, symbols![]),               //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
            (strip![loop 1, exit 2, t 2],           2, symbols![nt 1, t 2]),      //  2: a_1 -> C a_1 | ●a_1 ◄2 C! | 2 | a_1 C
            (strip![exit 3],                        1, symbols![nt 1]),           //  3: a_1 -> ε     | ◄3         | 1 | a_1
            (strip![loop 1, exit 4, t 1],           3, symbols![nt 1, t 0, t 1]), //  4: a_2 -> B a_1 | ●a_1 ◄4 B! | 3 | a_1 A B
            (strip![loop 1, exit 5],                2, symbols![nt 1, t 0]),      //  5: a_2 -> a_1   | ●a_1 ◄5    | 2 | a_1 A
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A | A B | C)+
        //
        //   NT    name       val   flags
        // +-------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                  |
        // |   1 | . a_1     | y  | child_+_or_*, parent_left_fact, plus |
        // |   2 | .   a_2   |    | parent_left_fact, child_left_fact    |
        // |   4 | .   . a_4 |    | child_left_fact                      |
        // |   3 | .   a_3   |    | child_left_fact                      |
        // +-------------------------------------------------------------+
        (155, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),           //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
            (strip![nt 2, t 0],                     0, symbols![]),               //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
            (strip![nt 3, t 2],                     0, symbols![]),               //  2: a_1 -> C a_3 | ►a_3 C! | 0 |
            (strip![nt 4, t 1],                     0, symbols![]),               //  3: a_2 -> B a_4 | ►a_4 B! | 0 |
            (strip![loop 1, exit 4],                2, symbols![nt 1, t 0]),      //  4: a_2 -> a_1   | ●a_1 ◄4 | 2 | a_1 A
            (strip![exit 5],                        2, symbols![nt 1, t 0]),      //  5: a_2 -> ε     | ◄5      | 2 | a_1 A
            (strip![loop 1, exit 6],                2, symbols![nt 1, t 2]),      //  6: a_3 -> a_1   | ●a_1 ◄6 | 2 | a_1 C
            (strip![exit 7],                        2, symbols![nt 1, t 2]),      //  7: a_3 -> ε     | ◄7      | 2 | a_1 C
            (strip![loop 1, exit 8],                3, symbols![nt 1, t 0, t 1]), //  8: a_4 -> a_1   | ●a_1 ◄8 | 3 | a_1 A B
            (strip![exit 9],                        3, symbols![nt 1, t 0, t 1]), //  9: a_4 -> ε     | ◄9      | 3 | a_1 A B
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A ((B C | D)* E | F)* G
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*               |
        // |   2 | . a_2   | y  | child_+_or_*, parent_+_or_* |
        // |   1 | .   a_1 | y  | child_+_or_*                |
        // +--------------------------------------------------+
        (156, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 6, nt 2, t 0],        3, symbols![t 0, nt 2, t 6]),  //  0: a -> A a_2 G     | ◄0 G! ►a_2 A!   | 3 | A a_2 G
            (strip![loop 1, exit 1, t 2, t 1],      3, symbols![nt 1, t 1, t 2]),  //  1: a_1 -> B C a_1   | ●a_1 ◄1 C! B!   | 3 | a_1 B C
            (strip![loop 1, exit 2, t 3],           2, symbols![nt 1, t 3]),       //  2: a_1 -> D a_1     | ●a_1 ◄2 D!      | 2 | a_1 D
            (strip![exit 3],                        1, symbols![nt 1]),            //  3: a_1 -> ε         | ◄3              | 1 | a_1
            (strip![loop 2, exit 4, t 4, nt 1],     3, symbols![nt 2, nt 1, t 4]), //  4: a_2 -> a_1 E a_2 | ●a_2 ◄4 E! ►a_1 | 3 | a_2 a_1 E
            (strip![loop 2, exit 5, t 5],           2, symbols![nt 2, t 5]),       //  5: a_2 -> F a_2     | ●a_2 ◄5 F!      | 2 | a_2 F
            (strip![exit 6],                        1, symbols![nt 2]),            //  6: a_2 -> ε         | ◄6              | 1 | a_2
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A ((B C | D)+ E | F)+ G
        //
        //   NT    name       val   flags
        // +----------------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                                 |
        // |   2 | . a_2     | y  | child_+_or_*, parent_left_fact, parent_+_or_*, plus |
        // |   1 | .   a_1   | y  | child_+_or_*, parent_left_fact, plus                |
        // |   3 | .   . a_3 |    | child_left_fact                                     |
        // |   4 | .   . a_4 |    | child_left_fact                                     |
        // |   5 | .   a_5   |    | child_left_fact                                     |
        // |   6 | .   a_6   |    | child_left_fact                                     |
        // +----------------------------------------------------------------------------+
        (157, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 6, nt 2, t 0],        3, symbols![t 0, nt 2, t 6]),  //  0: a -> A a_2 G     | ◄0 G! ►a_2 A! | 3 | A a_2 G
            (strip![nt 3, t 2, t 1],                0, symbols![]),                //  1: a_1 -> B C a_3   | ►a_3 C! B!    | 0 |
            (strip![nt 4, t 3],                     0, symbols![]),                //  2: a_1 -> D a_4     | ►a_4 D!       | 0 |
            (strip![nt 5, t 5],                     0, symbols![]),                //  3: a_2 -> F a_5     | ►a_5 F!       | 0 |
            (strip![nt 6, t 4, nt 1],               0, symbols![]),                //  4: a_2 -> a_1 E a_6 | ►a_6 E! ►a_1  | 0 |
            (strip![loop 1, exit 5],                3, symbols![nt 1, t 1, t 2]),  //  5: a_3 -> a_1       | ●a_1 ◄5       | 3 | a_1 B C
            (strip![exit 6],                        3, symbols![nt 1, t 1, t 2]),  //  6: a_3 -> ε         | ◄6            | 3 | a_1 B C
            (strip![loop 1, exit 7],                2, symbols![nt 1, t 3]),       //  7: a_4 -> a_1       | ●a_1 ◄7       | 2 | a_1 D
            (strip![exit 8],                        2, symbols![nt 1, t 3]),       //  8: a_4 -> ε         | ◄8            | 2 | a_1 D
            (strip![loop 2, exit 9],                2, symbols![nt 2, t 5]),       //  9: a_5 -> a_2       | ●a_2 ◄9       | 2 | a_2 F
            (strip![exit 10],                       2, symbols![nt 2, t 5]),       // 10: a_5 -> ε         | ◄10           | 2 | a_2 F
            (strip![loop 2, exit 11],               3, symbols![nt 2, nt 1, t 4]), // 11: a_6 -> a_2       | ●a_2 ◄11      | 3 | a_2 a_1 E
            (strip![exit 12],                       3, symbols![nt 2, nt 1, t 4]), // 12: a_6 -> ε         | ◄12           | 3 | a_2 a_1 E
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- +_or_* <L>
        // a -> A (<L=i> B)* C
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // +----------------------------------------+
        (200, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynI".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, nt 1, t 2]), //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),      //  1: i -> B i   | ●i ◄1 B!    | 2 | i B
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: i -> ε     | ◄2          | 1 | i
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n
        (200, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, t 2]), //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
            (strip![loop 1, exit 1, t 1],           2, symbols![t 1]),      //  1: i -> B i   | ●i ◄1 B!    | 2 | B
            (strip![exit 2],                        1, symbols![]),         //  2: i -> ε     | ◄2          | 1 |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> A (<L=i> B)+ C
        //
        //   NT    name     val   flags
        // +-------------------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*, plus                          |
        // |   1 | . i     | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |   2 | .   i_1 |    | child_left_fact                              |
        // +-------------------------------------------------------------------+
        (201, true, false, true, 0, btreemap![
            0 => "SynMyA".to_string(),
            1 => "SynMyI".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, nt 1, t 2]), //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
            (strip![nt 2, t 1],                     0, symbols![]),               //  1: i -> B i_1 | ►i_1 B!     | 0 |
            (strip![loop 1, exit 2],                2, symbols![nt 1, t 1]),      //  2: i_1 -> i   | ●i ◄2       | 2 | i B
            (strip![exit 3],                        2, symbols![nt 1, t 1]),      //  3: i_1 -> ε   | ◄3          | 2 | i B
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, i_1: n
        (201, true, false, true, 0, btreemap![
            0 => "SynMyA".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, t 2]), //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
            (strip![nt 2, t 1],                     0, symbols![]),         //  1: i -> B i_1 | ►i_1 B!     | 0 |
            (strip![loop 1, exit 2],                2, symbols![t 1]),      //  2: i_1 -> i   | ●i ◄2       | 2 | B
            (strip![exit 3],                        2, symbols![t 1]),      //  3: i_1 -> ε   | ◄3          | 2 | B
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),
        // a: n, i: y, i_1: n
        (201, true, false, true, 0, btreemap![
            0 => "SynMyA".to_string(),
            1 => "SynMyI".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, nt 1, t 2]), //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
            (strip![nt 2, t 1],                     0, symbols![]),               //  1: i -> B i_1 | ►i_1 B!     | 0 |
            (strip![loop 1, exit 2],                2, symbols![nt 1, t 1]),      //  2: i_1 -> i   | ●i ◄2       | 2 | i B
            (strip![exit 3],                        2, symbols![nt 1, t 1]),      //  3: i_1 -> ε   | ◄3          | 2 | i B
        ], true, NTValue::SetIds(vec![1]), btreemap![0 => vec![0]]),

        // a -> (<L=i> b A b B A)*
        // b -> C
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // |   2 | b    | y  |                      |
        // +----------------------------------------+
        (202, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                              1, symbols![nt 1]),                            //  0: a -> i           | ◄0 ►i                | 1 | i
            (strip![loop 1, exit 1, t 0, t 1, nt 2, t 0, nt 2], 6, symbols![nt 1, nt 2, t 0, nt 2, t 1, t 0]), //  1: i -> b A b B A i | ●i ◄1 A! B! ►b A! ►b | 6 | i b A b B A
            (strip![exit 2],                                    1, symbols![nt 1]),                            //  2: i -> ε           | ◄2                   | 1 | i
            (strip![exit 3, t 2],                               1, symbols![t 2]),                             //  3: b -> C           | ◄3 C!                | 1 | C
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> (A (<L=j> B ",")* ";")* C
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*               |
        // |   2 | . a_1 | y  | child_+_or_*, parent_+_or_* |
        // |   1 | .   j | y  | child_+_or_*, L-form        |
        // +------------------------------------------------+
        (206, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynAiter".to_string(),
            2 => "SynA1".to_string(),
        ], vec![
            (strip![exit 0, t 4, nt 2],              2, symbols![nt 2, t 4]),       //  0: a -> a_1 C         | ◄0 C! ►a_1        | 2 | a_1 C
            (strip![loop 1, exit 1, t 2, t 1],       3, symbols![nt 1, t 1]),       //  1: j -> B "," j       | ●j ◄1 "," B!      | 3 | j B
            (strip![exit 2],                         1, symbols![nt 1]),            //  2: j -> ε             | ◄2                | 1 | j
            (strip![loop 2, exit 3, t 3, nt 1, t 0], 4, symbols![nt 2, t 0, nt 1]), //  3: a_1 -> A j ";" a_1 | ●a_1 ◄3 ";" ►j A! | 4 | a_1 A j
            (strip![exit 4],                         1, symbols![nt 2]),            //  4: a_1 -> ε           | ◄4                | 1 | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A (<L=j> b ",")* ";")* C
        // b -> B
        //
        //   NT    name   val   flags
        // +--------------------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*                       |
        // |   1 | . i   | y  | child_+_or_*, L-form, parent_+_or_* |
        // |   2 | .   j | y  | child_+_or_*, L-form                |
        // |   3 | b     | y  |                                     |
        // +--------------------------------------------------------+
        (208, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1],              2, symbols![nt 1, t 3]),       //  0: a -> i C       | ◄0 C! ►i        | 2 | i C
            (strip![loop 1, exit 1, t 2, nt 2, t 0], 4, symbols![nt 1, t 0, nt 2]), //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | i A j
            (strip![exit 2],                         1, symbols![nt 1]),            //  2: i -> ε         | ◄2              | 1 | i
            (strip![loop 2, exit 3, t 1, nt 3],      3, symbols![nt 2, nt 3]),      //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 | j b
            (strip![exit 4],                         1, symbols![nt 2]),            //  4: j -> ε         | ◄4              | 1 | j
            (strip![exit 5, t 4],                    1, symbols![t 4]),             //  5: b -> B         | ◄5 B!           | 1 | B
        ], true, NTValue::Default, btreemap![0 => vec![0], 3 => vec![5]]),
        // a: y, i: y, j: n, b: n
        (208, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1],              2, symbols![nt 1, t 3]), //  0: a -> i C       | ◄0 C! ►i        | 2 | i C
            (strip![loop 1, exit 1, t 2, nt 2, t 0], 4, symbols![nt 1, t 0]), //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | i A
            (strip![exit 2],                         1, symbols![nt 1]),      //  2: i -> ε         | ◄2              | 1 | i
            (strip![loop 2, exit 3, t 1, nt 3],      3, symbols![]),          //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
            (strip![exit 4],                         1, symbols![]),          //  4: j -> ε         | ◄4              | 1 |
            (strip![exit 5, t 4],                    1, symbols![t 4]),       //  5: b -> B         | ◄5 B!           | 1 | B
        ], true, NTValue::SetIds(vec![0, 1]), btreemap![0 => vec![0], 3 => vec![5]]),
        // a: y, i: n, j: n, b: n
        (208, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1],              2, symbols![t 3]), //  0: a -> i C       | ◄0 C! ►i        | 2 | C
            (strip![loop 1, exit 1, t 2, nt 2, t 0], 4, symbols![t 0]), //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | A
            (strip![exit 2],                         1, symbols![]),    //  2: i -> ε         | ◄2              | 1 |
            (strip![loop 2, exit 3, t 1, nt 3],      3, symbols![]),    //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
            (strip![exit 4],                         1, symbols![]),    //  4: j -> ε         | ◄4              | 1 |
            (strip![exit 5, t 4],                    1, symbols![t 4]), //  5: b -> B         | ◄5 B!           | 1 | B
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 3 => vec![5]]),
        // a: n, i: n, j: n, b: n
        (208, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1],              2, symbols![t 3]), //  0: a -> i C       | ◄0 C! ►i        | 2 | C
            (strip![loop 1, exit 1, t 2, nt 2, t 0], 4, symbols![t 0]), //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | A
            (strip![exit 2],                         1, symbols![]),    //  2: i -> ε         | ◄2              | 1 |
            (strip![loop 2, exit 3, t 1, nt 3],      3, symbols![]),    //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
            (strip![exit 4],                         1, symbols![]),    //  4: j -> ε         | ◄4              | 1 |
            (strip![exit 5, t 4],                    1, symbols![t 4]), //  5: b -> B         | ◄5 B!           | 1 | B
        ], true, NTValue::None, btreemap![0 => vec![0], 3 => vec![5]]),

        // a -> A (<L=i> "B")* C
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  |    | child_+_or_*, L-form |
        // +----------------------------------------+
        (210, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], vec![
            (strip![exit 0, t 2, nt 1, t 0],        3, symbols![t 0, t 2]), //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
            (strip![loop 1, exit 1, t 1],           2, symbols![]),         //  1: i -> "B" i | ●i ◄1 "B"   | 2 |
            (strip![exit 2],                        1, symbols![]),         //  2: i -> ε     | ◄2          | 1 |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> A A (B <L=i>)* C | A C (B <L=i>)* C
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form            |
        // |   2 | . a_1 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (211, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynI".to_string(),
        ], vec![
            (strip![nt 2, t 0],                     0, symbols![]),                    //  0: a -> A a_1   | ►a_1 A!     | 0 |
            (strip![loop 1, exit 1, t 2],           2, symbols![nt 1, t 2]),           //  1: i -> B i     | ●i ◄1 B!    | 2 | i B
            (strip![exit 2],                        1, symbols![nt 1]),                //  2: i -> ε       | ◄2          | 1 | i
            (strip![exit 3, t 1, nt 1, t 0],        4, symbols![t 0, t 0, nt 1, t 1]), //  3: a_1 -> A i C | ◄3 C! ►i A! | 4 | A A i C
            (strip![exit 4, t 1, nt 1, t 1],        4, symbols![t 0, t 1, nt 1, t 1]), //  4: a_1 -> C i C | ◄4 C! ►i C! | 4 | A C i C
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> Id "(" (<L=i> Id ":" type / "<" ">")+ ")"
        // type -> Id
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // |   2 | type | y  |                                |
        // +--------------------------------------------------+
        (212, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 5, nt 1, nt 2, t 2, t 0, t 1, t 0], 4, symbols![t 0, nt 1]),       //  0: a -> Id "(" Id ":" type i ")" | ◄0 ")" ►i ►type ":" Id! "(" Id! | 4    | Id i
            (strip![loop 1, exit 1, nt 2, t 2, t 0, t 4, t 3],    6, symbols![nt 1, t 0, nt 2]), //  1: i -> "<" ">" Id ":" type i    | ●i ◄1 ►type ":" Id! ">" "<"     | 6, 3 | i Id type
            (strip![exit 2],                                      1, symbols![nt 1]),            //  2: i -> ε                        | ◄2                              | 1    | i
            (strip![exit 3, t 0],                                 1, symbols![t 0]),             //  3: type -> Id                    | ◄3 Id!                          | 1    | Id
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> Id "(" ((<L=i> Id ":" type / ",")+)? ")"
        // type -> Id
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form, sep_list  |
        // |   3 | . a_1 |    | child_left_fact                 |
        // |   2 | type  | y  |                                 |
        // +----------------------------------------------------+
        (213, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 3, t 1, t 0],                      0, symbols![]),                //  0: a -> Id "(" a_1          | ►a_1 "(" Id!            | 0    |
            (strip![loop 1, exit 1, nt 2, t 2, t 0, t 3], 5, symbols![nt 1, t 0, nt 2]), //  1: i -> "," Id ":" type i   | ●i ◄1 ►type ":" Id! "," | 5, 3 | i Id type
            (strip![exit 2],                              1, symbols![nt 1]),            //  2: i -> ε                   | ◄2                      | 1    | i
            (strip![exit 3, t 0],                         1, symbols![t 0]),             //  3: type -> Id               | ◄3 Id!                  | 1    | Id
            (strip![exit 4, t 4, nt 1, nt 2, t 2, t 0],   4, symbols![t 0, nt 1]),       //  4: a_1 -> Id ":" type i ")" | ◄4 ")" ►i ►type ":" Id! | 4    | Id i
            (strip![exit 5, t 4],                         3, symbols![t 0]),             //  5: a_1 -> ")"               | ◄5 ")"                  | 3    | Id
        ], true, NTValue::Default, btreemap![0 => vec![4, 5], 2 => vec![3]]),

        // a -> Id "(" (<L=i> Id / ",")+ "/" (<L=j> Id / ",")+ ")"
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // |   2 | . j  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (214, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 2, t 0, t 3, nt 1, t 0, t 1, t 0], 6, symbols![t 0, nt 1, nt 2]), //  0: a -> Id "(" Id i "/" Id j ")" | ◄0 ")" ►j Id! "/" ►i Id! "(" Id! | 6    | Id i j
            (strip![loop 1, exit 1, t 0, t 2],                         3, symbols![nt 1, t 0]),       //  1: i -> "," Id i                 | ●i ◄1 Id! ","                    | 3, 1 | i Id
            (strip![exit 2],                                           1, symbols![nt 1]),            //  2: i -> ε                        | ◄2                               | 1    | i
            (strip![loop 2, exit 3, t 0, t 2],                         3, symbols![nt 2, t 0]),       //  3: j -> "," Id j                 | ●j ◄3 Id! ","                    | 3, 1 | j Id
            (strip![exit 4],                                           1, symbols![nt 2]),            //  4: j -> ε                        | ◄4                               | 1    | j
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> X (<L=i> B / ",")+ B? Z;
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form, sep_list  |
        // |   2 | . a_1 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (216, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1, t 1, t 0],          0, symbols![]),                    //  0: a -> X B i a_1 | ►a_1 ►i B! X! | 0    |
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),           //  1: i -> "," B i   | ●i ◄1 B! ","  | 3, 1 | i B
            (strip![exit 2],                        1, symbols![nt 1]),                //  2: i -> ε         | ◄2            | 1    | i
            (strip![exit 3, t 3, t 1],              4, symbols![t 0, nt 1, t 1, t 3]), //  3: a_1 -> B Z     | ◄3 Z! B!      | 4    | X i B Z
            (strip![exit 4, t 3],                   3, symbols![t 0, nt 1, t 3]),      //  4: a_1 -> Z       | ◄4 Z!         | 3    | X i Z
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X? (<L=i> B / ",")+ Z
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (217, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![t 0, nt 1, t 3]), //  0: a -> X B i Z | ◄0 Z! ►i B! X! | 3    | X i Z
            (strip![exit 1, t 3, nt 1, t 1],        2, symbols![nt 1, t 3]),      //  1: a -> B i Z   | ◄1 Z! ►i B!    | 2    | i Z
            (strip![loop 1, exit 2, t 1, t 2],      3, symbols![nt 1, t 1]),      //  2: i -> "," B i | ●i ◄2 B! ","   | 3, 1 | i B
            (strip![exit 3],                        1, symbols![nt 1]),           //  3: i -> ε       | ◄3             | 1    | i
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> X Y? (<L=i> B / ",")+ Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form, sep_list  |
        // |   2 | . a_1 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (218, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 2, t 0],                     0, symbols![]),                    //  0: a -> X a_1     | ►a_1 X!        | 0    |
            (strip![loop 1, exit 1, t 2, t 3],      3, symbols![nt 1, t 2]),           //  1: i -> "," B i   | ●i ◄1 B! ","   | 3, 1 | i B
            (strip![exit 2],                        1, symbols![nt 1]),                //  2: i -> ε         | ◄2             | 1    | i
            (strip![exit 3, t 4, nt 1, t 2, t 1],   4, symbols![t 0, t 1, nt 1, t 4]), //  3: a_1 -> Y B i Z | ◄3 Z! ►i B! Y! | 4    | X Y i Z
            (strip![exit 4, t 4, nt 1, t 2],        3, symbols![t 0, nt 1, t 4]),      //  4: a_1 -> B i Z   | ◄4 Z! ►i B!    | 3    | X i Z
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X (<L=i> B / ",")+ Z
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (219, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![t 0, nt 1, t 3]), //  0: a -> X B i Z | ◄0 Z! ►i B! X! | 3    | X i Z
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]),      //  1: i -> "," B i | ●i ◄1 B! ","   | 3, 1 | i B
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: i -> ε       | ◄2             | 1    | i
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> "var" (<L=i> Id / ",")+ ";"
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (220, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![nt 1]),      //  0: a -> "var" Id i ";" | ◄0 ";" ►i Id! "var" | 3    | i
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]), //  1: i -> "," Id i       | ●i ◄1 Id! ","       | 3, 1 | i Id
            (strip![exit 2],                        1, symbols![nt 1]),      //  2: i -> ε              | ◄2                  | 1    | i
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n
        (220, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![]),    //  0: a -> "var" Id i ";" | ◄0 ";" ►i Id! "var" | 3    |
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![t 1]), //  1: i -> "," Id i       | ●i ◄1 Id! ","       | 3, 1 | Id
            (strip![exit 2],                        1, symbols![]),    //  2: i -> ε              | ◄2                  | 1    |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),
        // a: n, i: y
        (220, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 1, t 1, t 0],   3, symbols![nt 1]),      //  0: a -> "var" Id i ";" | ◄0 ";" ►i Id! "var" | 3    | i
            (strip![loop 1, exit 1, t 1, t 2],      3, symbols![nt 1, t 1]), //  1: i -> "," Id i       | ●i ◄1 Id! ","       | 3, 1 | i Id
            (strip![exit 2],                        1, symbols![nt 1]),      //  2: i -> ε              | ◄2                  | 1    | i
        ], true, NTValue::SetIds(vec![1]), btreemap![0 => vec![0]]),

        // a -> A (<L=i> B)* C+ D
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                        |
        // |   1 | . i     | y  | child_+_or_*, L-form                 |
        // |   2 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   3 | .   a_2 |    | child_left_fact                      |
        // +-----------------------------------------------------------+
        (221, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 3, nt 2, nt 1, t 0],  4, symbols![t 0, nt 1, nt 2, t 3]), //  0: a -> A i a_1 D | ◄0 D! ►a_1 ►i A! | 4 | A i a_1 D
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),            //  1: i -> B i       | ●i ◄1 B!         | 2 | i B
            (strip![exit 2],                        1, symbols![nt 1]),                 //  2: i -> ε         | ◄2               | 1 | i
            (strip![nt 3, t 2],                     0, symbols![]),                     //  3: a_1 -> C a_2   | ►a_2 C!          | 0 |
            (strip![loop 2, exit 4],                2, symbols![nt 2, t 2]),            //  4: a_2 -> a_1     | ●a_1 ◄4          | 2 | a_1 C
            (strip![exit 5],                        2, symbols![nt 2, t 2]),            //  5: a_2 -> ε       | ◄5               | 2 | a_1 C
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> Id "(" (<L=i> Id ":" type / ",")+ ")"
        // type -> Id
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // |   2 | type | y  |                                |
        // +--------------------------------------------------+
        (222, true, true, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 1, nt 2, t 2, t 0, t 1, t 0], 4, symbols![t 0, nt 1]),       //  0: a -> Id "(" Id ":" type i ")" | ◄0 ")" ►i ►type ":" Id! "(" Id! | 4    | Id i
            (strip![loop 1, exit 1, nt 2, t 2, t 0, t 3],         5, symbols![nt 1, t 0, nt 2]), //  1: i -> "," Id ":" type i        | ●i ◄1 ►type ":" Id! ","         | 5, 3 | i Id type
            (strip![exit 2],                                      1, symbols![nt 1]),            //  2: i -> ε                        | ◄2                              | 1    | i
            (strip![exit 3, t 0],                                 1, symbols![t 0]),             //  3: type -> Id                    | ◄3 Id!                          | 1    | Id
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> A (<L=i> B | C D)* E
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // +----------------------------------------+
        (250, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 1, t 0],        3, symbols![t 0, nt 1, t 4]), //  0: a -> A i E | ◄0 E! ►i A! | 3 | A i E
            (strip![loop 1, exit 1, t 1],           2, symbols![nt 1, t 1]),      //  1: i -> B i   | ●i ◄1 B!    | 2 | i B
            (strip![loop 1, exit 2, t 3, t 2],      3, symbols![nt 1, t 2, t 3]), //  2: i -> C D i | ●i ◄2 D! C! | 3 | i C D
            (strip![exit 3],                        1, symbols![nt 1]),           //  3: i -> ε     | ◄3          | 1 | i
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (<L=i> B | C D)+ E
        //
        //   NT    name     val   flags
        // +-------------------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                                |
        // |   1 | . i     | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |   2 | .   i_1 |    | child_left_fact                              |
        // |   3 | .   i_2 |    | child_left_fact                              |
        // +-------------------------------------------------------------------+
        (251, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 1, t 0],        3, symbols![t 0, nt 1, t 4]), //  0: a -> A i E   | ◄0 E! ►i A! | 3 | A i E
            (strip![nt 2, t 1],                     0, symbols![]),               //  1: i -> B i_1   | ►i_1 B!     | 0 |
            (strip![nt 3, t 3, t 2],                0, symbols![]),               //  2: i -> C D i_2 | ►i_2 D! C!  | 0 |
            (strip![loop 1, exit 3],                2, symbols![nt 1, t 1]),      //  3: i_1 -> i     | ●i ◄3       | 2 | i B
            (strip![exit 4],                        2, symbols![nt 1, t 1]),      //  4: i_1 -> ε     | ◄4          | 2 | i B
            (strip![loop 1, exit 5],                3, symbols![nt 1, t 2, t 3]), //  5: i_2 -> i     | ●i ◄5       | 3 | i C D
            (strip![exit 6],                        3, symbols![nt 1, t 2, t 3]), //  6: i_2 -> ε     | ◄6          | 3 | i C D
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A ((<L=j> b C b B C | D)+ E | F)+ G
        // b -> H
        //
        //   NT    name       val   flags
        // +----------------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                                 |
        // |   3 | . a_1     | y  | child_+_or_*, parent_left_fact, parent_+_or_*, plus |
        // |   1 | .   j     | y  | child_+_or_*, parent_left_fact, L-form, plus        |
        // |   4 | .   . j_1 |    | child_left_fact                                     |
        // |   5 | .   . j_2 |    | child_left_fact                                     |
        // |   6 | .   a_2   |    | child_left_fact                                     |
        // |   7 | .   a_3   |    | child_left_fact                                     |
        // |   2 | b         | y  |                                                     |
        // +----------------------------------------------------------------------------+
        (252, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 6, nt 3, t 0],          3, symbols![t 0, nt 3, t 6]),                  //  0: a -> A a_1 G       | ◄0 G! ►a_1 A!       | 3 | A a_1 G
            (strip![nt 4, t 3],                       0, symbols![]),                                //  1: j -> D j_1         | ►j_1 D!             | 0 |
            (strip![nt 5, t 1, t 2, nt 2, t 1, nt 2], 0, symbols![]),                                //  2: j -> b C b B C j_2 | ►j_2 C! B! ►b C! ►b | 0 |
            (strip![exit 3, t 7],                     1, symbols![t 7]),                             //  3: b -> H             | ◄3 H!               | 1 | H
            (strip![nt 6, t 5],                       0, symbols![]),                                //  4: a_1 -> F a_2       | ►a_2 F!             | 0 |
            (strip![nt 7, t 4, nt 1],                 0, symbols![]),                                //  5: a_1 -> j E a_3     | ►a_3 E! ►j          | 0 |
            (strip![loop 1, exit 6],                  2, symbols![nt 1, t 3]),                       //  6: j_1 -> j           | ●j ◄6               | 2 | j D
            (strip![exit 7],                          2, symbols![nt 1, t 3]),                       //  7: j_1 -> ε           | ◄7                  | 2 | j D
            (strip![loop 1, exit 8],                  6, symbols![nt 1, nt 2, t 1, nt 2, t 2, t 1]), //  8: j_2 -> j           | ●j ◄8               | 6 | j b C b B C
            (strip![exit 9],                          6, symbols![nt 1, nt 2, t 1, nt 2, t 2, t 1]), //  9: j_2 -> ε           | ◄9                  | 6 | j b C b B C
            (strip![loop 3, exit 10],                 2, symbols![nt 3, t 5]),                       // 10: a_2 -> a_1         | ●a_1 ◄10            | 2 | a_1 F
            (strip![exit 11],                         2, symbols![nt 3, t 5]),                       // 11: a_2 -> ε           | ◄11                 | 2 | a_1 F
            (strip![loop 3, exit 12],                 3, symbols![nt 3, nt 1, t 4]),                 // 12: a_3 -> a_1         | ●a_1 ◄12            | 3 | a_1 j E
            (strip![exit 13],                         3, symbols![nt 3, nt 1, t 4]),                 // 13: a_3 -> ε           | ◄13                 | 3 | a_1 j E
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> A (<L=i> (b C b B C | D)+ E | F)+ G
        // b -> H
        //
        //   NT    name       val   flags
        // +------------------------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                                         |
        // |   1 | . i       | y  | child_+_or_*, parent_left_fact, L-form, parent_+_or_*, plus |
        // |   3 | .   a_1   | y  | child_+_or_*, parent_left_fact, plus                        |
        // |   6 | .   . a_2 |    | child_left_fact                                             |
        // |   7 | .   . a_3 |    | child_left_fact                                             |
        // |   4 | .   i_1   |    | child_left_fact                                             |
        // |   5 | .   i_2   |    | child_left_fact                                             |
        // |   2 | b         | y  |                                                             |
        // +------------------------------------------------------------------------------------+
        (253, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 6, nt 1, t 0],          3, symbols![t 0, nt 1, t 6]),                  //  0: a -> A i G           | ◄0 G! ►i A!         | 3 | A i G
            (strip![nt 4, t 5],                       0, symbols![]),                                //  1: i -> F i_1           | ►i_1 F!             | 0 |
            (strip![nt 5, t 4, nt 3],                 0, symbols![]),                                //  2: i -> a_1 E i_2       | ►i_2 E! ►a_1        | 0 |
            (strip![exit 3, t 7],                     1, symbols![t 7]),                             //  3: b -> H               | ◄3 H!               | 1 | H
            (strip![nt 6, t 3],                       0, symbols![]),                                //  4: a_1 -> D a_2         | ►a_2 D!             | 0 |
            (strip![nt 7, t 1, t 2, nt 2, t 1, nt 2], 0, symbols![]),                                //  5: a_1 -> b C b B C a_3 | ►a_3 C! B! ►b C! ►b | 0 |
            (strip![loop 1, exit 6],                  2, symbols![nt 1, t 5]),                       //  6: i_1 -> i             | ●i ◄6               | 2 | i F
            (strip![exit 7],                          2, symbols![nt 1, t 5]),                       //  7: i_1 -> ε             | ◄7                  | 2 | i F
            (strip![loop 1, exit 8],                  3, symbols![nt 1, nt 3, t 4]),                 //  8: i_2 -> i             | ●i ◄8               | 3 | i a_1 E
            (strip![exit 9],                          3, symbols![nt 1, nt 3, t 4]),                 //  9: i_2 -> ε             | ◄9                  | 3 | i a_1 E
            (strip![loop 3, exit 10],                 2, symbols![nt 3, t 3]),                       // 10: a_2 -> a_1           | ●a_1 ◄10            | 2 | a_1 D
            (strip![exit 11],                         2, symbols![nt 3, t 3]),                       // 11: a_2 -> ε             | ◄11                 | 2 | a_1 D
            (strip![loop 3, exit 12],                 6, symbols![nt 3, nt 2, t 1, nt 2, t 2, t 1]), // 12: a_3 -> a_1           | ●a_1 ◄12            | 6 | a_1 b C b B C
            (strip![exit 13],                         6, symbols![nt 3, nt 2, t 1, nt 2, t 2, t 1]), // 13: a_3 -> ε             | ◄13                 | 6 | a_1 b C b B C
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> A (<L=i> (<L=j> b C b B C | D)* E | F)* G
        // b -> H
        //
        //   NT    name   val   flags
        // +--------------------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*                       |
        // |   1 | . i   | y  | child_+_or_*, L-form, parent_+_or_* |
        // |   2 | .   j | y  | child_+_or_*, L-form                |
        // |   3 | b     | y  |                                     |
        // +--------------------------------------------------------+
        (254, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 6, nt 1, t 0],                    3, symbols![t 0, nt 1, t 6]),                  //  0: a -> A i G       | ◄0 G! ►i A!          | 3 | A i G
            (strip![loop 1, exit 1, t 4, nt 2],                 3, symbols![nt 1, nt 2, t 4]),                 //  1: i -> j E i       | ●i ◄1 E! ►j          | 3 | i j E
            (strip![loop 1, exit 2, t 5],                       2, symbols![nt 1, t 5]),                       //  2: i -> F i         | ●i ◄2 F!             | 2 | i F
            (strip![exit 3],                                    1, symbols![nt 1]),                            //  3: i -> ε           | ◄3                   | 1 | i
            (strip![loop 2, exit 4, t 1, t 2, nt 3, t 1, nt 3], 6, symbols![nt 2, nt 3, t 1, nt 3, t 2, t 1]), //  4: j -> b C b B C j | ●j ◄4 C! B! ►b C! ►b | 6 | j b C b B C
            (strip![loop 2, exit 5, t 3],                       2, symbols![nt 2, t 3]),                       //  5: j -> D j         | ●j ◄5 D!             | 2 | j D
            (strip![exit 6],                                    1, symbols![nt 2]),                            //  6: j -> ε           | ◄6                   | 1 | j
            (strip![exit 7, t 7],                               1, symbols![t 7]),                             //  7: b -> H           | ◄7 H!                | 1 | H
        ], true, NTValue::Default, btreemap![0 => vec![0], 3 => vec![7]]),

        // a -> A (<L=i> B A | B A C b | D)+ E
        // b -> F
        //
        //   NT    name       val   flags
        // +---------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                          |
        // |   1 | . i       | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |   3 | .   i_1   |    | parent_left_fact, child_left_fact            |
        // |   5 | .   . a_1 |    | child_left_fact                              |
        // |   4 | .   i_2   |    | child_left_fact                              |
        // |   2 | b         | y  |                                              |
        // +---------------------------------------------------------------------+
        (256, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 4, nt 1, t 0],        3, symbols![t 0, nt 1, t 4]),            //  0: a -> A i E     | ◄0 E! ►i A! | 3 | A i E
            (strip![nt 3, t 0, t 1],                0, symbols![]),                          //  1: i -> B A i_1   | ►i_1 A! B!  | 0 |
            (strip![nt 4, t 3],                     0, symbols![]),                          //  2: i -> D i_2     | ►i_2 D!     | 0 |
            (strip![exit 3, t 5],                   1, symbols![t 5]),                       //  3: b -> F         | ◄3 F!       | 1 | F
            (strip![nt 5, nt 2, t 2],               0, symbols![]),                          //  4: i_1 -> C b a_1 | ►a_1 ►b C!  | 0 |
            (strip![loop 1, exit 5],                3, symbols![nt 1, t 1, t 0]),            //  5: i_1 -> i       | ●i ◄5       | 3 | i B A
            (strip![exit 6],                        3, symbols![nt 1, t 1, t 0]),            //  6: i_1 -> ε       | ◄6          | 3 | i B A
            (strip![loop 1, exit 7],                2, symbols![nt 1, t 3]),                 //  7: i_2 -> i       | ●i ◄7       | 2 | i D
            (strip![exit 8],                        2, symbols![nt 1, t 3]),                 //  8: i_2 -> ε       | ◄8          | 2 | i D
            (strip![loop 1, exit 9],                5, symbols![nt 1, t 1, t 0, t 2, nt 2]), //  9: a_1 -> i       | ●i ◄9       | 5 | i B A C b
            (strip![exit 10],                       5, symbols![nt 1, t 1, t 0, t 2, nt 2]), // 10: a_1 -> ε       | ◄10         | 5 | i B A C b
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> (<L=i> A | A B A | C)+
        //
        //   NT    name       val   flags
        // +---------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                          |
        // |   1 | . i       | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |   2 | .   i_1   |    | parent_left_fact, child_left_fact            |
        // |   4 | .   . a_1 |    | child_left_fact                              |
        // |   3 | .   i_2   |    | child_left_fact                              |
        // +---------------------------------------------------------------------+
        (257, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),                //  0: a -> i         | ◄0 ►i      | 1 | i
            (strip![nt 2, t 0],                     0, symbols![]),                    //  1: i -> A i_1     | ►i_1 A!    | 0 |
            (strip![nt 3, t 2],                     0, symbols![]),                    //  2: i -> C i_2     | ►i_2 C!    | 0 |
            (strip![nt 4, t 0, t 1],                0, symbols![]),                    //  3: i_1 -> B A a_1 | ►a_1 A! B! | 0 |
            (strip![loop 1, exit 4],                2, symbols![nt 1, t 0]),           //  4: i_1 -> i       | ●i ◄4      | 2 | i A
            (strip![exit 5],                        2, symbols![nt 1, t 0]),           //  5: i_1 -> ε       | ◄5         | 2 | i A
            (strip![loop 1, exit 6],                2, symbols![nt 1, t 2]),           //  6: i_2 -> i       | ●i ◄6      | 2 | i C
            (strip![exit 7],                        2, symbols![nt 1, t 2]),           //  7: i_2 -> ε       | ◄7         | 2 | i C
            (strip![loop 1, exit 8],                4, symbols![nt 1, t 0, t 1, t 0]), //  8: a_1 -> i       | ●i ◄8      | 4 | i A B A
            (strip![exit 9],                        4, symbols![nt 1, t 0, t 1, t 0]), //  9: a_1 -> ε       | ◄9         | 4 | i A B A
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | A B | C | D (<L=j> E | E F | G)*)*
        //
        //   NT    name       val   flags
        // +------------------------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*                                         |
        // |   1 | . i       | y  | child_+_or_*, parent_left_fact, L-form, parent_+_or_* |
        // |   2 | .   j     | y  | child_+_or_*, parent_left_fact, L-form                |
        // |   4 | .   . j_1 |    | child_left_fact                                       |
        // |   3 | .   i_1   |    | child_left_fact                                       |
        // +------------------------------------------------------------------------------+
        (258, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),            //  0: a -> i     | ◄0 ►i       | 1 | i
            (strip![nt 3, t 0],                     0, symbols![]),                //  1: i -> A i_1 | ►i_1 A!     | 0 |
            (strip![loop 1, exit 2, t 2],           2, symbols![nt 1, t 2]),       //  2: i -> C i   | ●i ◄2 C!    | 2 | i C
            (strip![loop 1, exit 3, nt 2, t 3],     3, symbols![nt 1, t 3, nt 2]), //  3: i -> D j i | ●i ◄3 ►j D! | 3 | i D j
            (strip![exit 4],                        1, symbols![nt 1]),            //  4: i -> ε     | ◄4          | 1 | i
            (strip![nt 4, t 4],                     0, symbols![]),                //  5: j -> E j_1 | ►j_1 E!     | 0 |
            (strip![loop 2, exit 6, t 6],           2, symbols![nt 2, t 6]),       //  6: j -> G j   | ●j ◄6 G!    | 2 | j G
            (strip![exit 7],                        1, symbols![nt 2]),            //  7: j -> ε     | ◄7          | 1 | j
            (strip![loop 1, exit 8, t 1],           3, symbols![nt 1, t 0, t 1]),  //  8: i_1 -> B i | ●i ◄8 B!    | 3 | i A B
            (strip![loop 1, exit 9],                2, symbols![nt 1, t 0]),       //  9: i_1 -> i   | ●i ◄9       | 2 | i A
            (strip![loop 2, exit 10, t 5],          3, symbols![nt 2, t 4, t 5]),  // 10: j_1 -> F j | ●j ◄10 F!   | 3 | j E F
            (strip![loop 2, exit 11],               2, symbols![nt 2, t 4]),       // 11: j_1 -> j   | ●j ◄11      | 2 | j E
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | A B | C | D (<L=j> E | E F | G)+)+
        //
        //   NT    name         val   flags
        // +--------------------------------------------------------------------------------------+
        // |   0 | a           | y  | parent_+_or_*, plus                                         |
        // |   1 | . i         | y  | child_+_or_*, parent_left_fact, L-form, parent_+_or_*, plus |
        // |   2 | .   j       | y  | child_+_or_*, parent_left_fact, L-form, plus                |
        // |   6 | .   . j_1   |    | parent_left_fact, child_left_fact                           |
        // |   9 | .   .   a_2 |    | child_left_fact                                             |
        // |   7 | .   . j_2   |    | child_left_fact                                             |
        // |   3 | .   i_1     |    | parent_left_fact, child_left_fact                           |
        // |   8 | .   . a_1   |    | child_left_fact                                             |
        // |   4 | .   i_2     |    | child_left_fact                                             |
        // |   5 | .   i_3     |    | child_left_fact                                             |
        // +--------------------------------------------------------------------------------------+
        (259, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),            //  0: a -> i       | ◄0 ►i      | 1 | i
            (strip![nt 3, t 0],                     0, symbols![]),                //  1: i -> A i_1   | ►i_1 A!    | 0 |
            (strip![nt 4, t 2],                     0, symbols![]),                //  2: i -> C i_2   | ►i_2 C!    | 0 |
            (strip![nt 5, nt 2, t 3],               0, symbols![]),                //  3: i -> D j i_3 | ►i_3 ►j D! | 0 |
            (strip![nt 6, t 4],                     0, symbols![]),                //  4: j -> E j_1   | ►j_1 E!    | 0 |
            (strip![nt 7, t 6],                     0, symbols![]),                //  5: j -> G j_2   | ►j_2 G!    | 0 |
            (strip![nt 8, t 1],                     0, symbols![]),                //  6: i_1 -> B a_1 | ►a_1 B!    | 0 |
            (strip![loop 1, exit 7],                2, symbols![nt 1, t 0]),       //  7: i_1 -> i     | ●i ◄7      | 2 | i A
            (strip![exit 8],                        2, symbols![nt 1, t 0]),       //  8: i_1 -> ε     | ◄8         | 2 | i A
            (strip![loop 1, exit 9],                2, symbols![nt 1, t 2]),       //  9: i_2 -> i     | ●i ◄9      | 2 | i C
            (strip![exit 10],                       2, symbols![nt 1, t 2]),       // 10: i_2 -> ε     | ◄10        | 2 | i C
            (strip![loop 1, exit 11],               3, symbols![nt 1, t 3, nt 2]), // 11: i_3 -> i     | ●i ◄11     | 3 | i D j
            (strip![exit 12],                       3, symbols![nt 1, t 3, nt 2]), // 12: i_3 -> ε     | ◄12        | 3 | i D j
            (strip![nt 9, t 5],                     0, symbols![]),                // 13: j_1 -> F a_2 | ►a_2 F!    | 0 |
            (strip![loop 2, exit 14],               2, symbols![nt 2, t 4]),       // 14: j_1 -> j     | ●j ◄14     | 2 | j E
            (strip![exit 15],                       2, symbols![nt 2, t 4]),       // 15: j_1 -> ε     | ◄15        | 2 | j E
            (strip![loop 2, exit 16],               2, symbols![nt 2, t 6]),       // 16: j_2 -> j     | ●j ◄16     | 2 | j G
            (strip![exit 17],                       2, symbols![nt 2, t 6]),       // 17: j_2 -> ε     | ◄17        | 2 | j G
            (strip![loop 1, exit 18],               3, symbols![nt 1, t 0, t 1]),  // 18: a_1 -> i     | ●i ◄18     | 3 | i A B
            (strip![exit 19],                       3, symbols![nt 1, t 0, t 1]),  // 19: a_1 -> ε     | ◄19        | 3 | i A B
            (strip![loop 2, exit 20],               3, symbols![nt 2, t 4, t 5]),  // 20: a_2 -> j     | ●j ◄20     | 3 | j E F
            (strip![exit 21],                       3, symbols![nt 2, t 4, t 5]),  // 21: a_2 -> ε     | ◄21        | 3 | j E F
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- right_rec
        // expr -> Id "." expr | "(" Num ")"
        //
        //   NT    name  val   flags
        // +-----------------------------+
        // |   0 | expr | y  | right_rec |
        // +-----------------------------+
        (301, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 0, t 1, t 0],        3, symbols![t 0, nt 0]), //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id expr
            (strip![exit 1, t 4, t 3, t 2],         3, symbols![t 3]),       //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),
        // expr: n
        (301, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 0, t 1, t 0],        3, symbols![t 0]), //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id
            (strip![exit 1, t 4, t 3, t 2],         3, symbols![t 3]), //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
        ], true, NTValue::None, btreemap![0 => vec![0, 1]]),

        // Code: without the <L>, the right-recursive loop starts with `Num` and proceeds right to left,
        //       handing the current `expr` and the previous `Id` to generate the updated `expr`:
        //
        // fn exit_expr(&mut self, alt_id: AltId) {
        //     let ctx = match alt_id {
        //         0 => {
        //             let expr = self.stack.pop().unwrap().get_expr();
        //             let id = self.stack_t.pop().unwrap();
        //             CtxExpr::Expr1 { id, expr }
        //         }
        //         1 => {
        //             let num = self.stack_t.pop().unwrap();
        //             CtxExpr::Expr2 { num }
        //         }
        //         _ => panic!("unexpected alt id {alt_id} in fn exit_expr")
        //     };
        //     let val = self.listener.exit_expr(ctx);
        //     self.stack.push(SynValue::Expr(val));
        // }

        // --------------------------------------------------------------------------- right_rec <L>
        // expr -> <L> Id "." expr | "(" Num ")"
        //
        //   NT    name  val   flags
        // +-------------------------------------+
        // |   0 | expr | y  | right_rec, L-form |
        // +-------------------------------------+
        (401, true, false, true, 0, btreemap![
        ], vec![
            (strip![loop 0, exit 0, t 1, t 0],      3, symbols![nt 0, t 0]), //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | expr Id
            (strip![exit 1, t 4, t 3, t 2],         4, symbols![nt 0, t 3]), //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | expr Num
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),
        // expr: n
        (401, true, false, true, 0, btreemap![
        ], vec![
            (strip![loop 0, exit 0, t 1, t 0],      3, symbols![t 0]), //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | Id
            (strip![exit 1, t 4, t 3, t 2],         4, symbols![t 3]), //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | Num
        ], true, NTValue::None, btreemap![0 => vec![0, 1]]),

        // Code: with <L>, the right-recursive loop starts with an initialization of `expr`, then all
        //       the `Id` are scanned from left to right, handing the current `expr` and the next `Id`,
        //       and the loop ends with the accumulated value of `expr` and the final `Num`:
        //
        // fn init_expr(&mut self) {
        //     let val = self.listener.init_expr();
        //     self.stack.push(SynValue::Expr(val));
        // }
        //
        // fn exit_expr(&mut self, alt_id: AltId) {
        //     let ctx = match alt_id {
        //         0 => {
        //             let id = self.stack_t.pop().unwrap();
        //             let expr = self.stack.pop().unwrap().get_expr();
        //             CtxExpr::Expr1 { expr, id }
        //         }
        //         1 => {
        //             let num = self.stack_t.pop().unwrap();
        //             let expr = self.stack.pop().unwrap().get_expr();
        //             CtxExpr::Expr2 { expr, num }
        //         }
        //         _ => panic!("unexpected alt id {alt_id} in fn exit_expr")
        //     };
        //     let val = self.listener.exit_expr(ctx);
        //     self.stack.push(SynValue::Expr(val));
        // }

        // --------------------------------------------------------------------------- left_rec
        // a -> a "b" | a "c" | "a"
        //
        //   NT    name   val   flags
        // +------------------------------------+
        // |   0 | a     | y  | parent_left_rec |
        // |   1 | . a_1 |    | child_left_rec  |
        // +------------------------------------+
        (501, false, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, t 2],             1, symbols![]),     //  0: a -> "a" a_1   | ►a_1 ◄0 "a" | 1 |
            (strip![loop 1, exit 1, t 0],           2, symbols![nt 0]), //  1: a_1 -> "b" a_1 | ●a_1 ◄1 "b" | 2 | a
            (strip![loop 1, exit 2, t 1],           2, symbols![nt 0]), //  2: a_1 -> "c" a_1 | ●a_1 ◄2 "c" | 2 | a
            (strip![exit 3],                        1, symbols![nt 0]), //  3: a_1 -> ε       | ◄3          | 1 | a
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> f | e "." Id
        // f -> Id
        //
        //   NT    name   val   flags
        // +------------------------------------+
        // |   0 | e     | y  | parent_left_rec |
        // |   2 | . e_1 |    | child_left_rec  |
        // |   1 | f     | y  |                 |
        // +------------------------------------+
        (502, true, false, true, 0, btreemap![
            0 => "SynE".to_string(),
            1 => "SynF".to_string(),
        ], vec![
            (strip![nt 2, exit 0, nt 1],            1, symbols![nt 1]),      //  0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
            (strip![exit 1, t 1],                   1, symbols![t 1]),       //  1: f -> Id           | ◄1 Id!          | 1 | Id
            (strip![loop 2, exit 2, t 1, t 0],      3, symbols![nt 0, t 1]), //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | e Id
            (strip![exit 3],                        1, symbols![nt 0]),      //  3: e_1 -> ε          | ◄3              | 1 | e
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),
        // e: n, f: y, e_1: n
        (502, true, false, true, 0, btreemap![
            1 => "SynF".to_string(),
        ], vec![
            (strip![nt 2, exit 0, nt 1],            1, symbols![nt 1]), //  0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
            (strip![exit 1, t 1],                   1, symbols![t 1]),  //  1: f -> Id           | ◄1 Id!          | 1 | Id
            (strip![loop 2, exit 2, t 1, t 0],      3, symbols![t 1]),  //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | Id
            (strip![exit 3],                        1, symbols![]),     //  3: e_1 -> ε          | ◄3              | 1 |
        ], true, NTValue::SetIds(vec![1]), btreemap![0 => vec![0], 1 => vec![1]]),

        // --------------------------------------------------------------------------- right_rec + left_rec
        // e -> e "!" | "-" e | Num
        //
        //   NT    name   val   flags
        // +-----------------------------------------------+
        // |   0 | e     | y  | right_rec, parent_left_rec |
        // |   1 | . e_1 |    | child_left_rec             |
        // +-----------------------------------------------+
        (580, true, false, true, 0, btreemap![
            0 => "SynE".to_string(),
        ], vec![
            (strip![exit 0, nt 0, t 1],             2, symbols![nt 0]), //  0: e -> "-" e     | ◄0 ►e "-"    | 2 | e
            (strip![nt 1, exit 1, t 2],             1, symbols![t 2]),  //  1: e -> Num e_1   | ►e_1 ◄1 Num! | 1 | Num
            (strip![loop 1, exit 2, t 0],           2, symbols![nt 0]), //  2: e_1 -> "!" e_1 | ●e_1 ◄2 "!"  | 2 | e
            (strip![exit 3],                        1, symbols![nt 0]), //  3: e_1 -> ε       | ◄3           | 1 | e
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // --------------------------------------------------------------------------- left_rec ambig
        // e -> e "+" e | Num
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | e     | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1 |    | child_left_rec              |
        // |   2 | . e_2 |    |                             |
        // +------------------------------------------------+
        (600, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 2],            1, symbols![nt 0]),       //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            (strip![loop 1, exit 1, nt 2, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "+" e_2 e_1 | ●e_1 ◄1 ►e_2 "+" | 3 | e e
            (strip![exit 2],                        1, symbols![nt 0]),       //  2: e_1 -> ε           | ◄2               | 1 | e
            (strip![exit 3, t 1],                   1, symbols![t 1]),        //  3: e_2 -> Num         | ◄3 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" e | "!" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | right_rec                   |
        // +--------------------------------------------------+
        (603, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            (strip![exit 3],                        1, symbols![nt 0]),       //  3: e_1 -> ε           | ◄3               | 1 | e
            (strip![nt 3, exit 4, nt 4],            1, symbols![nt 0]),       //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
            (strip![loop 3, exit 5, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
            (strip![exit 6],                        1, symbols![nt 0]),       //  6: e_3 -> ε           | ◄6               | 1 | e
            (strip![exit 7, nt 0, t 2],             2, symbols![nt 0]),       //  7: e_4 -> "!" e       | ◄7 ►e "!"        | 2 | e
            (strip![exit 8, t 3],                   1, symbols![t 3]),        //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | "!" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | right_rec                   |
        // +--------------------------------------------------+
        (604, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            (strip![exit 3],                        1, symbols![nt 0]),       //  3: e_1 -> ε           | ◄3               | 1 | e
            (strip![nt 3, exit 4, nt 4],            1, symbols![nt 0]),       //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
            (strip![loop 3, exit 5, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
            (strip![exit 6],                        1, symbols![nt 0]),       //  6: e_3 -> ε           | ◄6               | 1 | e
            (strip![exit 7, nt 2, t 2],             2, symbols![nt 0]),       //  7: e_4 -> "!" e_2     | ◄7 ►e_2 "!"      | 2 | e
            (strip![exit 8, t 3],                   1, symbols![t 3]),        //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "!" e | e "*" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | right_rec                   |
        // +--------------------------------------------------+
        (605, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            (strip![exit 3],                        1, symbols![nt 0]),       //  3: e_1 -> ε           | ◄3               | 1 | e
            (strip![nt 3, exit 4, nt 4],            1, symbols![nt 0]),       //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
            (strip![loop 3, exit 5, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
            (strip![exit 6],                        1, symbols![nt 0]),       //  6: e_3 -> ε           | ◄6               | 1 | e
            (strip![exit 7, nt 4, t 2],             2, symbols![nt 0]),       //  7: e_4 -> "!" e_4     | ◄7 ►e_4 "!"      | 2 | e
            (strip![exit 8, t 3],                   1, symbols![t 3]),        //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" e | <R> e "!" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    |                             |
        // +--------------------------------------------------+
        (606, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            (strip![loop 1, exit 3, nt 0, t 2],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "!" e e_1   | ●e_1 ◄3 ►e "!"   | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 4],            1, symbols![nt 0]),       //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            (strip![loop 3, exit 6, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            (strip![exit 7],                        1, symbols![nt 0]),       //  7: e_3 -> ε           | ◄7               | 1 | e
            (strip![exit 8, t 3],                   1, symbols![t 3]),        //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | <R> e "!" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    |                             |
        // +--------------------------------------------------+
        (607, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "!" e_2 e_1 | ●e_1 ◄2 ►e_2 "!" | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 4],            1, symbols![nt 0]),       //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            (strip![loop 3, exit 6, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            (strip![loop 3, exit 7, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "!" e_2 e_3 | ●e_3 ◄7 ►e_2 "!" | 3 | e e
            (strip![exit 8],                        1, symbols![nt 0]),       //  8: e_3 -> ε           | ◄8               | 1 | e
            (strip![exit 9, t 3],                   1, symbols![t 3]),        //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> <R> e "!" e | e "*" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | parent_left_rec             |
        // |   5 | .   e_5 |    | child_left_rec              |
        // |   6 | . e_6   |    |                             |
        // +--------------------------------------------------+
        (608, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 6],            1, symbols![nt 0]),       //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | 1 | e
            (strip![loop 1, exit 1, nt 4, t 2],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "!" e_4 e_1 | ●e_1 ◄1 ►e_4 "!"  | 3 | e e
            (strip![loop 1, exit 2, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4                | 1 | e
            (strip![nt 3, exit 5, nt 6],            1, symbols![nt 0]),       //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | 1 | e
            (strip![loop 3, exit 6, nt 4, t 2],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "!" e_4 e_3 | ●e_3 ◄6 ►e_4 "!"  | 3 | e e
            (strip![loop 3, exit 7, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | 3 | e e
            (strip![exit 8],                        1, symbols![nt 0]),       //  8: e_3 -> ε           | ◄8                | 1 | e
            (strip![nt 5, exit 9, nt 6],            1, symbols![nt 0]),       //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | 1 | e
            (strip![loop 5, exit 10, nt 4, t 2],    3, symbols![nt 0, nt 0]), // 10: e_5 -> "!" e_4 e_5 | ●e_5 ◄10 ►e_4 "!" | 3 | e e
            (strip![exit 11],                       1, symbols![nt 0]),       // 11: e_5 -> ε           | ◄11               | 1 | e
            (strip![exit 12, t 3],                  1, symbols![t 3]),        // 12: e_6 -> Num         | ◄12 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" e | e "!" | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    |                             |
        // +--------------------------------------------------+
        (609, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            (strip![loop 1, exit 3, t 2],           2, symbols![nt 0]),       //  3: e_1 -> "!" e_1     | ●e_1 ◄3 "!"      | 2 | e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 4],            1, symbols![nt 0]),       //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            (strip![loop 3, exit 6, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            (strip![exit 7],                        1, symbols![nt 0]),       //  7: e_3 -> ε           | ◄7               | 1 | e
            (strip![exit 8, t 3],                   1, symbols![t 3]),        //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "!" | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    |                             |
        // +--------------------------------------------------+
        (610, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, t 2],           2, symbols![nt 0]),       //  2: e_1 -> "!" e_1     | ●e_1 ◄2 "!"      | 2 | e
            (strip![loop 1, exit 3, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 4],            1, symbols![nt 0]),       //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            (strip![loop 3, exit 6, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            (strip![loop 3, exit 7, t 2],           2, symbols![nt 0]),       //  7: e_3 -> "!" e_3     | ●e_3 ◄7 "!"      | 2 | e
            (strip![exit 8],                        1, symbols![nt 0]),       //  8: e_3 -> ε           | ◄8               | 1 | e
            (strip![exit 9, t 3],                   1, symbols![t 3]),        //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "!" | e "*" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | parent_left_rec             |
        // |   5 | .   e_5 |    | child_left_rec              |
        // |   6 | . e_6   |    |                             |
        // +--------------------------------------------------+
        (611, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 6],            1, symbols![nt 0]),       //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6     | 1 | e
            (strip![loop 1, exit 1, t 2],           2, symbols![nt 0]),       //  1: e_1 -> "!" e_1     | ●e_1 ◄1 "!"      | 2 | e
            (strip![loop 1, exit 2, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 6],            1, symbols![nt 0]),       //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6     | 1 | e
            (strip![loop 3, exit 6, t 2],           2, symbols![nt 0]),       //  6: e_3 -> "!" e_3     | ●e_3 ◄6 "!"      | 2 | e
            (strip![loop 3, exit 7, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | 3 | e e
            (strip![exit 8],                        1, symbols![nt 0]),       //  8: e_3 -> ε           | ◄8               | 1 | e
            (strip![nt 5, exit 9, nt 6],            1, symbols![nt 0]),       //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6     | 1 | e
            (strip![loop 5, exit 10, t 2],          2, symbols![nt 0]),       // 10: e_5 -> "!" e_5     | ●e_5 ◄10 "!"     | 2 | e
            (strip![exit 11],                       1, symbols![nt 0]),       // 11: e_5 -> ε           | ◄11              | 1 | e
            (strip![exit 12, t 3],                  1, symbols![t 3]),        // 12: e_6 -> Num         | ◄12 Num!         | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "!" e | e "*" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | parent_left_rec             |
        // |   5 | .   e_5 |    | child_left_rec              |
        // |   6 | . e_6   |    |                             |
        // +--------------------------------------------------+
        (612, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 6],            1, symbols![nt 0]),       //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | 1 | e
            (strip![loop 1, exit 1, nt 6, t 2],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "!" e_6 e_1 | ●e_1 ◄1 ►e_6 "!"  | 3 | e e
            (strip![loop 1, exit 2, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4                | 1 | e
            (strip![nt 3, exit 5, nt 6],            1, symbols![nt 0]),       //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | 1 | e
            (strip![loop 3, exit 6, nt 6, t 2],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "!" e_6 e_3 | ●e_3 ◄6 ►e_6 "!"  | 3 | e e
            (strip![loop 3, exit 7, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | 3 | e e
            (strip![exit 8],                        1, symbols![nt 0]),       //  8: e_3 -> ε           | ◄8                | 1 | e
            (strip![nt 5, exit 9, nt 6],            1, symbols![nt 0]),       //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | 1 | e
            (strip![loop 5, exit 10, nt 6, t 2],    3, symbols![nt 0, nt 0]), // 10: e_5 -> "!" e_6 e_5 | ●e_5 ◄10 ►e_6 "!" | 3 | e e
            (strip![exit 11],                       1, symbols![nt 0]),       // 11: e_5 -> ε           | ◄11               | 1 | e
            (strip![exit 12, t 3],                  1, symbols![t 3]),        // 12: e_6 -> Num         | ◄12 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" e | <P> e "!" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    |                             |
        // +--------------------------------------------------+
        (613, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "!" e_2 e_1 | ●e_1 ◄3 ►e_2 "!" | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 4],            1, symbols![nt 0]),       //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            (strip![loop 3, exit 6, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            (strip![exit 7],                        1, symbols![nt 0]),       //  7: e_3 -> ε           | ◄7               | 1 | e
            (strip![exit 8, t 3],                   1, symbols![t 3]),        //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | <P> e "!" e | e "+" e | Num
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    |                             |
        // +--------------------------------------------------+
        (614, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 4, t 2],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "!" e_4 e_1 | ●e_1 ◄2 ►e_4 "!" | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            (strip![exit 4],                        1, symbols![nt 0]),       //  4: e_1 -> ε           | ◄4               | 1 | e
            (strip![nt 3, exit 5, nt 4],            1, symbols![nt 0]),       //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            (strip![loop 3, exit 6, nt 4, t 0],     3, symbols![nt 0, nt 0]), //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            (strip![loop 3, exit 7, nt 4, t 2],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "!" e_4 e_3 | ●e_3 ◄7 ►e_4 "!" | 3 | e e
            (strip![exit 8],                        1, symbols![nt 0]),       //  8: e_3 -> ε           | ◄8               | 1 | e
            (strip![exit 9, t 3],                   1, symbols![t 3]),        //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" | "!" e | Num
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | e     | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1 |    | child_left_rec              |
        // |   2 | . e_2 |    | right_rec                   |
        // +------------------------------------------------+
        (630, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 2],            1, symbols![nt 0]),       //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            (strip![loop 1, exit 1, nt 2, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            (strip![loop 1, exit 2, t 1],           2, symbols![nt 0]),       //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
            (strip![exit 3],                        1, symbols![nt 0]),       //  3: e_1 -> ε           | ◄3               | 1 | e
            (strip![exit 4, nt 0, t 2],             2, symbols![nt 0]),       //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
            (strip![exit 5, t 3],                   1, symbols![t 3]),        //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" | <R> "!" e | Num
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | e     | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1 |    | child_left_rec              |
        // |   2 | . e_2 |    | right_rec                   |
        // +------------------------------------------------+
        (631, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 2],            1, symbols![nt 0]),       //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            (strip![loop 1, exit 1, nt 2, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            (strip![loop 1, exit 2, t 1],           2, symbols![nt 0]),       //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
            (strip![exit 3],                        1, symbols![nt 0]),       //  3: e_1 -> ε           | ◄3               | 1 | e
            (strip![exit 4, nt 0, t 2],             2, symbols![nt 0]),       //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
            (strip![exit 5, t 3],                   1, symbols![t 3]),        //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | <R> e "+" | "!" e | Num
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | e     | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1 |    | child_left_rec              |
        // |   2 | . e_2 |    | right_rec                   |
        // +------------------------------------------------+
        (632, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 2],            1, symbols![nt 0]),       //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            (strip![loop 1, exit 1, nt 2, t 0],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            (strip![loop 1, exit 2, t 1],           2, symbols![nt 0]),       //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
            (strip![exit 3],                        1, symbols![nt 0]),       //  3: e_1 -> ε           | ◄3               | 1 | e
            (strip![exit 4, nt 0, t 2],             2, symbols![nt 0]),       //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
            (strip![exit 5, t 3],                   1, symbols![t 3]),        //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "-" e | e "*" e | e "/" <P> e | e "+" e | e "-" <P> e | Id
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | right_rec                   |
        // +--------------------------------------------------+
        (640, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 4, t 1],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 4, t 2],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "/" e_4 e_1 | ●e_1 ◄2 ►e_4 "/" | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 3],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            (strip![loop 1, exit 4, nt 2, t 0],     3, symbols![nt 0, nt 0]), //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | 3 | e e
            (strip![exit 5],                        1, symbols![nt 0]),       //  5: e_1 -> ε           | ◄5               | 1 | e
            (strip![nt 3, exit 6, nt 4],            1, symbols![nt 0]),       //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
            (strip![loop 3, exit 7, nt 4, t 1],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | 3 | e e
            (strip![loop 3, exit 8, nt 4, t 2],     3, symbols![nt 0, nt 0]), //  8: e_3 -> "/" e_4 e_3 | ●e_3 ◄8 ►e_4 "/" | 3 | e e
            (strip![exit 9],                        1, symbols![nt 0]),       //  9: e_3 -> ε           | ◄9               | 1 | e
            (strip![exit 10, nt 4, t 0],            2, symbols![nt 0]),       // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
            (strip![exit 11, t 4],                  1, symbols![t 4]),        // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "-" e | <R> e "*" e | <R> e "/" <P> e | <R> e "+" e | <R> e "-" <P> e | Id
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | right_rec                   |
        // +--------------------------------------------------+
        (641, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | 3 | e e
            (strip![loop 1, exit 3, nt 0, t 3],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e e_1   | ●e_1 ◄3 ►e "+"   | 3 | e e
            (strip![loop 1, exit 4, nt 0, t 0],     3, symbols![nt 0, nt 0]), //  4: e_1 -> "-" e e_1   | ●e_1 ◄4 ►e "-"   | 3 | e e
            (strip![exit 5],                        1, symbols![nt 0]),       //  5: e_1 -> ε           | ◄5               | 1 | e
            (strip![nt 3, exit 6, nt 4],            1, symbols![nt 0]),       //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
            (strip![loop 3, exit 7, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | 3 | e e
            (strip![loop 3, exit 8, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | 3 | e e
            (strip![exit 9],                        1, symbols![nt 0]),       //  9: e_3 -> ε           | ◄9               | 1 | e
            (strip![exit 10, nt 4, t 0],            2, symbols![nt 0]),       // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
            (strip![exit 11, t 4],                  1, symbols![t 4]),        // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "-" e | <R> e "*" e | <R> e "/" <P> e | e "+" e | e "-" <P> e | Id
        //
        //   NT    name     val   flags
        // +--------------------------------------------------+
        // |   0 | e       | y  | parent_left_rec, parent_amb |
        // |   1 | . e_1   |    | child_left_rec              |
        // |   2 | . e_2   |    | parent_left_rec             |
        // |   3 | .   e_3 |    | child_left_rec              |
        // |   4 | . e_4   |    | right_rec                   |
        // +--------------------------------------------------+
        (642, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 4],            1, symbols![nt 0]),       //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            (strip![loop 1, exit 1, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            (strip![loop 1, exit 2, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | 3 | e e
            (strip![loop 1, exit 3, nt 2, t 3],     3, symbols![nt 0, nt 0]), //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            (strip![loop 1, exit 4, nt 2, t 0],     3, symbols![nt 0, nt 0]), //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | 3 | e e
            (strip![exit 5],                        1, symbols![nt 0]),       //  5: e_1 -> ε           | ◄5               | 1 | e
            (strip![nt 3, exit 6, nt 4],            1, symbols![nt 0]),       //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
            (strip![loop 3, exit 7, nt 2, t 1],     3, symbols![nt 0, nt 0]), //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | 3 | e e
            (strip![loop 3, exit 8, nt 2, t 2],     3, symbols![nt 0, nt 0]), //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | 3 | e e
            (strip![exit 9],                        1, symbols![nt 0]),       //  9: e_3 -> ε           | ◄9               | 1 | e
            (strip![exit 10, nt 4, t 0],            2, symbols![nt 0]),       // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
            (strip![exit 11, t 4],                  1, symbols![t 4]),        // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a A a a | B
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | a     | y  | parent_left_rec, parent_amb |
        // |   1 | . a_1 |    | child_left_rec              |
        // |   2 | . a_2 |    |                             |
        // +------------------------------------------------+
        (650, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1, exit 0, nt 2],              1, symbols![nt 0]),                  //  0: a -> a_2 a_1       | ►a_1 ◄0 ►a_2       | 1 | a
            (strip![loop 1, exit 1, nt 2, nt 0, t 0], 4, symbols![nt 0, t 0, nt 0, nt 0]), //  1: a_1 -> A a a_2 a_1 | ●a_1 ◄1 ►a_2 ►a A! | 4 | a A a a
            (strip![exit 2],                          1, symbols![nt 0]),                  //  2: a_1 -> ε           | ◄2                 | 1 | a
            (strip![exit 3, t 1],                     1, symbols![t 1]),                   //  3: a_2 -> B           | ◄3 B!              | 1 | B
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a "*" a | a (Id / ".")+ a | a "+" a | Num
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------------+
        // |   0 | a       | y  | parent_left_rec, parent_amb, parent_+_or_* |
        // |   1 | . a_1   | y  | child_+_or_*, sep_list                     |
        // |   2 | . a_2   |    | child_left_rec                             |
        // |   3 | . a_3   |    | parent_left_rec                            |
        // |   4 | .   a_4 |    | child_left_rec                             |
        // |   5 | . a_5   |    | parent_left_rec                            |
        // |   6 | .   a_6 |    | child_left_rec                             |
        // |   7 | . a_7   |    |                                            |
        // +-----------------------------------------------------------------+
        (680, false, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, exit 0, nt 7],              1, symbols![nt 0]),             //  0: a -> a_7 a_2          | ►a_2 ◄0 ►a_7          | 1    | a
            (strip![loop 1, exit 1, t 1, t 2],        3, symbols![nt 1, t 1]),        //  1: a_1 -> "." Id a_1     | ●a_1 ◄1 Id! "."       | 3, 1 | a_1 Id
            (strip![exit 2],                          1, symbols![nt 1]),             //  2: a_1 -> ε              | ◄2                    | 1    | a_1
            (strip![loop 2, exit 3, nt 7, t 0],       3, symbols![nt 0, nt 0]),       //  3: a_2 -> "*" a_7 a_2    | ●a_2 ◄3 ►a_7 "*"      | 3    | a a
            (strip![loop 2, exit 4, nt 5, nt 1, t 1], 3, symbols![nt 0, nt 1, nt 0]), //  4: a_2 -> Id a_1 a_5 a_2 | ●a_2 ◄4 ►a_5 ►a_1 Id! | 3    | a a_1 a
            (strip![loop 2, exit 5, nt 3, t 3],       3, symbols![nt 0, nt 0]),       //  5: a_2 -> "+" a_3 a_2    | ●a_2 ◄5 ►a_3 "+"      | 3    | a a
            (strip![exit 6],                          1, symbols![nt 0]),             //  6: a_2 -> ε              | ◄6                    | 1    | a
            (strip![nt 4, exit 7, nt 7],              1, symbols![nt 0]),             //  7: a_3 -> a_7 a_4        | ►a_4 ◄7 ►a_7          | 1    | a
            (strip![loop 4, exit 8, nt 7, t 0],       3, symbols![nt 0, nt 0]),       //  8: a_4 -> "*" a_7 a_4    | ●a_4 ◄8 ►a_7 "*"      | 3    | a a
            (strip![loop 4, exit 9, nt 5, nt 1, t 1], 3, symbols![nt 0, nt 1, nt 0]), //  9: a_4 -> Id a_1 a_5 a_4 | ●a_4 ◄9 ►a_5 ►a_1 Id! | 3    | a a_1 a
            (strip![exit 10],                         1, symbols![nt 0]),             // 10: a_4 -> ε              | ◄10                   | 1    | a
            (strip![nt 6, exit 11, nt 7],             1, symbols![nt 0]),             // 11: a_5 -> a_7 a_6        | ►a_6 ◄11 ►a_7         | 1    | a
            (strip![loop 6, exit 12, nt 7, t 0],      3, symbols![nt 0, nt 0]),       // 12: a_6 -> "*" a_7 a_6    | ●a_6 ◄12 ►a_7 "*"     | 3    | a a
            (strip![exit 13],                         1, symbols![nt 0]),             // 13: a_6 -> ε              | ◄13                   | 1    | a
            (strip![exit 14, t 4],                    1, symbols![t 4]),              // 14: a_7 -> Num            | ◄14 Num!              | 1    | Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- left_fact
        // a -> A | A B
        //
        //   NT    name   val   flags
        // +-------------------------------------+
        // |   0 | a     | y  | parent_left_fact |
        // |   1 | . a_1 |    | child_left_fact  |
        // +-------------------------------------+
        (700, false, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1, t 0],                     0, symbols![]),         //  0: a -> A a_1 | ►a_1 A! | 0 |
            (strip![exit 1, t 1],                   2, symbols![t 0, t 1]), //  1: a_1 -> B   | ◄1 B!   | 2 | A B
            (strip![exit 2],                        1, symbols![t 0]),      //  2: a_1 -> ε   | ◄2      | 1 | A
        ], true, NTValue::Default, btreemap![0 => vec![1, 2]]),

        // a -> A | A B | A B C | A B D | E
        //
        //   NT    name     val   flags
        // +--------------------------------------------------------+
        // |   0 | a       | y  | parent_left_fact                  |
        // |   1 | . a_1   |    | parent_left_fact, child_left_fact |
        // |   2 | .   a_2 |    | child_left_fact                   |
        // +--------------------------------------------------------+
        (705, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], vec![
            (strip![nt 1, t 0],                     0, symbols![]),              //  0: a -> A a_1   | ►a_1 A! | 0 |
            (strip![exit 1, t 4],                   1, symbols![t 4]),           //  1: a -> E       | ◄1 E!   | 1 | E
            (strip![nt 2, t 1],                     0, symbols![]),              //  2: a_1 -> B a_2 | ►a_2 B! | 0 |
            (strip![exit 3],                        1, symbols![t 0]),           //  3: a_1 -> ε     | ◄3      | 1 | A
            (strip![exit 4, t 2],                   3, symbols![t 0, t 1, t 2]), //  4: a_2 -> C     | ◄4 C!   | 3 | A B C
            (strip![exit 5, t 3],                   3, symbols![t 0, t 1, t 3]), //  5: a_2 -> D     | ◄5 D!   | 3 | A B D
            (strip![exit 6],                        2, symbols![t 0, t 1]),      //  6: a_2 -> ε     | ◄6      | 2 | A B
        ], true, NTValue::Default, btreemap![0 => vec![1, 3, 4, 5, 6]]),

        // --------------------------------------------------------------------------- combinations

        // --------------------------------------------------------------------------- +_or_* and right_rec
        // a -> A* B a | C
        //
        //   NT    name   val   flags
        // +---------------------------------------------+
        // |   0 | a     | y  | right_rec, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*             |
        // +---------------------------------------------+
        (810, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 0, t 1, nt 1],       3, symbols![nt 1, t 1, nt 0]), //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | 3 | a_1 B a
            (strip![exit 1, t 2],                   1, symbols![t 2]),             //  1: a -> C       | ◄1 C!         | 1 | C
            (strip![loop 1, exit 2, t 0],           2, symbols![nt 1, t 0]),       //  2: a_1 -> A a_1 | ●a_1 ◄2 A!    | 2 | a_1 A
            (strip![exit 3],                        1, symbols![nt 1]),            //  3: a_1 -> ε     | ◄3            | 1 | a_1
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> A+ B a | C
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | right_rec, parent_+_or_*, plus       |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   2 | .   a_2 |    | child_left_fact                      |
        // +-----------------------------------------------------------+
        (811, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 0, t 1, nt 1],       3, symbols![nt 1, t 1, nt 0]), //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | 3 | a_1 B a
            (strip![exit 1, t 2],                   1, symbols![t 2]),             //  1: a -> C       | ◄1 C!         | 1 | C
            (strip![nt 2, t 0],                     0, symbols![]),                //  2: a_1 -> A a_2 | ►a_2 A!       | 0 |
            (strip![loop 1, exit 3],                2, symbols![nt 1, t 0]),       //  3: a_2 -> a_1   | ●a_1 ◄3       | 2 | a_1 A
            (strip![exit 4],                        2, symbols![nt 1, t 0]),       //  4: a_2 -> ε     | ◄4            | 2 | a_1 A
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // --------------------------------------------------------------------------- +_or_* and left_rec
        // a -> a A* C | B
        //
        //   NT    name   val   flags
        // +---------------------------------------------------+
        // |   0 | a     | y  | parent_left_rec, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*                   |
        // |   2 | . a_2 |    | child_left_rec                 |
        // +---------------------------------------------------+
        (820, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], vec![
            (strip![nt 2, exit 0, t 2],             1, symbols![t 2]),             //  0: a -> B a_2       | ►a_2 ◄0 B!      | 1 | B
            (strip![loop 1, exit 1, t 0],           2, symbols![nt 1, t 0]),       //  1: a_1 -> A a_1     | ●a_1 ◄1 A!      | 2 | a_1 A
            (strip![exit 2],                        1, symbols![nt 1]),            //  2: a_1 -> ε         | ◄2              | 1 | a_1
            (strip![loop 2, exit 3, t 1, nt 1],     3, symbols![nt 0, nt 1, t 1]), //  3: a_2 -> a_1 C a_2 | ●a_2 ◄3 C! ►a_1 | 3 | a a_1 C
            (strip![exit 4],                        1, symbols![nt 0]),            //  4: a_2 -> ε         | ◄4              | 1 | a
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a A+ C | B
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_left_rec, parent_+_or_*, plus |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   3 | .   a_3 |    | child_left_fact                      |
        // |   2 | . a_2   |    | child_left_rec                       |
        // +-----------------------------------------------------------+
        (821, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], vec![
            (strip![nt 2, exit 0, t 2],             1, symbols![t 2]),             //  0: a -> B a_2       | ►a_2 ◄0 B!      | 1 | B
            (strip![nt 3, t 0],                     0, symbols![]),                //  1: a_1 -> A a_3     | ►a_3 A!         | 0 |
            (strip![loop 2, exit 2, t 1, nt 1],     3, symbols![nt 0, nt 1, t 1]), //  2: a_2 -> a_1 C a_2 | ●a_2 ◄2 C! ►a_1 | 3 | a a_1 C
            (strip![exit 3],                        1, symbols![nt 0]),            //  3: a_2 -> ε         | ◄3              | 1 | a
            (strip![loop 1, exit 4],                2, symbols![nt 1, t 0]),       //  4: a_3 -> a_1       | ●a_1 ◄4         | 2 | a_1 A
            (strip![exit 5],                        2, symbols![nt 1, t 0]),       //  5: a_3 -> ε         | ◄5              | 2 | a_1 A
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a "x" a | a "*" "[" Num+ "]" | "-" a | Id
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------------------+
        // |   0 | a       | y  | parent_left_rec, parent_amb, parent_+_or_*, plus |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact, plus             |
        // |   4 | .   a_4 |    | child_left_fact                                  |
        // |   2 | . a_2   |    | child_left_rec                                   |
        // |   3 | . a_3   |    | right_rec                                        |
        // +-----------------------------------------------------------------------+
        (835, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 2, exit 0, nt 3],                  1, symbols![nt 0]),       //  0: a -> a_3 a_2               | ►a_2 ◄0 ►a_3             | 1 | a
            (strip![nt 4, t 3],                           0, symbols![]),           //  1: a_1 -> Num a_4             | ►a_4 Num!                | 0 |
            (strip![loop 2, exit 2, nt 3, t 0],           3, symbols![nt 0, nt 0]), //  2: a_2 -> "x" a_3 a_2         | ●a_2 ◄2 ►a_3 "x"         | 3 | a a
            (strip![loop 2, exit 3, t 4, nt 1, t 2, t 1], 5, symbols![nt 0, nt 1]), //  3: a_2 -> "*" "[" a_1 "]" a_2 | ●a_2 ◄3 "]" ►a_1 "[" "*" | 5 | a a_1
            (strip![exit 4],                              1, symbols![nt 0]),       //  4: a_2 -> ε                   | ◄4                       | 1 | a
            (strip![exit 5, nt 0, t 5],                   2, symbols![nt 0]),       //  5: a_3 -> "-" a               | ◄5 ►a "-"                | 2 | a
            (strip![exit 6, t 6],                         1, symbols![t 6]),        //  6: a_3 -> Id                  | ◄6 Id!                   | 1 | Id
            (strip![loop 1, exit 7],                      2, symbols![nt 1, t 3]),  //  7: a_4 -> a_1                 | ●a_1 ◄7                  | 2 | a_1 Num
            (strip![exit 8],                              2, symbols![nt 1, t 3]),  //  8: a_4 -> ε                   | ◄8                       | 2 | a_1 Num
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- +_or_* and left_fact
        // a -> (A B | A C)*
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*                  |
        // |   1 | . a_1   | y  | child_+_or_*, parent_left_fact |
        // |   2 | .   a_2 |    | child_left_fact                |
        // +-----------------------------------------------------+
        (840, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),           //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
            (strip![nt 2, t 0],                     0, symbols![]),               //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
            (strip![exit 2],                        1, symbols![nt 1]),           //  2: a_1 -> ε     | ◄2         | 1 | a_1
            (strip![loop 1, exit 3, t 1],           3, symbols![nt 1, t 0, t 1]), //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | 3 | a_1 A B
            (strip![loop 1, exit 4, t 2],           3, symbols![nt 1, t 0, t 2]), //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | 3 | a_1 A C
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A B | A C)+
        //
        //   NT    name       val   flags
        // +-------------------------------------------------------------+
        // |   0 | a         | y  | parent_+_or_*, plus                  |
        // |   1 | . a_1     | y  | child_+_or_*, parent_left_fact, plus |
        // |   2 | .   a_2   |    | parent_left_fact, child_left_fact    |
        // |   3 | .   . a_3 |    | child_left_fact                      |
        // |   4 | .   . a_4 |    | child_left_fact                      |
        // +-------------------------------------------------------------+
        (841, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),           //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
            (strip![nt 2, t 0],                     0, symbols![]),               //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
            (strip![nt 3, t 1],                     0, symbols![]),               //  2: a_2 -> B a_3 | ►a_3 B! | 0 |
            (strip![nt 4, t 2],                     0, symbols![]),               //  3: a_2 -> C a_4 | ►a_4 C! | 0 |
            (strip![loop 1, exit 4],                3, symbols![nt 1, t 0, t 1]), //  4: a_3 -> a_1   | ●a_1 ◄4 | 3 | a_1 A B
            (strip![exit 5],                        3, symbols![nt 1, t 0, t 1]), //  5: a_3 -> ε     | ◄5      | 3 | a_1 A B
            (strip![loop 1, exit 6],                3, symbols![nt 1, t 0, t 2]), //  6: a_4 -> a_1   | ●a_1 ◄6 | 3 | a_1 A C
            (strip![exit 7],                        3, symbols![nt 1, t 0, t 2]), //  7: a_4 -> ε     | ◄7      | 3 | a_1 A C
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- right_rec + left_fact
        // a -> A B a | A C a | D
        //
        //   NT    name   val   flags
        // +------------------------------------------------+
        // |   0 | a     | y  | right_rec, parent_left_fact |
        // |   1 | . a_1 |    | child_left_fact             |
        // +------------------------------------------------+
        (860, false, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1, t 0],                     0, symbols![]),               //  0: a -> A a_1 | ►a_1 A!  | 0 |
            (strip![exit 1, t 3],                   1, symbols![t 3]),            //  1: a -> D     | ◄1 D!    | 1 | D
            (strip![exit 2, nt 0, t 1],             3, symbols![t 0, t 1, nt 0]), //  2: a_1 -> B a | ◄2 ►a B! | 3 | A B a
            (strip![exit 3, nt 0, t 2],             3, symbols![t 0, t 2, nt 0]), //  3: a_1 -> C a | ◄3 ►a C! | 3 | A C a
        ], true, NTValue::Default, btreemap![0 => vec![1, 2, 3]]),

        // expr -> <L> Num "^" expr | Num
        //
        //   NT    name      val   flags
        // +-----------------------------------------------------------+
        // |   0 | expr     | y  | right_rec, parent_left_fact, L-form |
        // |   1 | . expr_1 |    | child_left_fact                     |
        // +-----------------------------------------------------------+
        (862, true, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1, t 0],                     0, symbols![]),          //  0: expr -> Num expr_1 | ►expr_1 Num! | 0 |
            (strip![loop 0, exit 1, t 1],           3, symbols![nt 0, t 0]), //  1: expr_1 -> "^" expr | ●expr ◄1 "^" | 3 | expr Num
            (strip![exit 2],                        2, symbols![nt 0, t 0]), //  2: expr_1 -> ε        | ◄2           | 2 | expr Num
        ], true, NTValue::Default, btreemap![0 => vec![1, 2]]),

        // --------------------------------------------------------------------------- left_rec [left_fact]
        // a -> a A | B C | B D
        //
        //   NT    name   val   flags
        // +------------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_left_rec |
        // |   1 | . a_1 |    | child_left_rec                    |
        // |   2 | . a_2 |    | child_left_fact                   |
        // +------------------------------------------------------+
        (870, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], vec![
            (strip![nt 2, t 1],                     0, symbols![]),          //  0: a -> B a_2   | ►a_2 B!    | 0 |
            (strip![loop 1, exit 1, t 0],           2, symbols![nt 0, t 0]), //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | 2 | a A
            (strip![exit 2],                        1, symbols![nt 0]),      //  2: a_1 -> ε     | ◄2         | 1 | a
            (strip![nt 1, exit 3, t 2],             2, symbols![t 1, t 2]),  //  3: a_2 -> C a_1 | ►a_1 ◄3 C! | 2 | B C
            (strip![nt 1, exit 4, t 3],             2, symbols![t 1, t 3]),  //  4: a_2 -> D a_1 | ►a_1 ◄4 D! | 2 | B D
        ], true, NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> a A B | a A C | D
        //
        //   NT    name     val   flags
        // +-------------------------------------------------------+
        // |   0 | a       | y  | parent_left_rec                  |
        // |   1 | . a_1   |    | child_left_rec, parent_left_fact |
        // |   2 | .   a_2 |    | child_left_fact                  |
        // +-------------------------------------------------------+
        (871, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], vec![
            (strip![nt 1, exit 0, t 3],             1, symbols![t 3]),            //  0: a -> D a_1   | ►a_1 ◄0 D! | 1 | D
            (strip![nt 2, t 0],                     0, symbols![]),               //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
            (strip![exit 2],                        1, symbols![nt 0]),           //  2: a_1 -> ε     | ◄2         | 1 | a
            (strip![loop 1, exit 3, t 1],           3, symbols![nt 0, t 0, t 1]), //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | 3 | a A B
            (strip![loop 1, exit 4, t 2],           3, symbols![nt 0, t 0, t 2]), //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | 3 | a A C
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- misc
        // file -> header file_item* | file_item*
        // file_item -> option | declaration | rule
        // header -> "lexicon" Id ";"
        // declaration -> "mode" Id ";"
        // option -> "channels" "{" (Id / ",")+ "}"
        // rule -> "fragment" Id ":" match ";" | Id ":" match "->" actions ";" | Id ":" match ";"
        // actions -> (action / ",")+
        // action -> "mode" "(" Id ")" | "push" "(" Id ")" | "pop" | "skip" | "more" | "type" "(" Id ")" | "channel" "(" Id ")"
        // match -> alt_items
        // alt_items -> (alt_item / "|")+
        // alt_item -> repeat_item+
        // repeat_item -> item "*" "?" | item "*" | item "+" "?" | item "+" | item "?" | item
        // item -> Id | CharLit ".." CharLit | CharLit | StrLit | char_set | "(" alt_items ")" | "~" item
        // char_set -> "[" char_set_one+ "]" | "." | FixedSet
        // char_set_one -> SetChar "-" SetChar | SetChar | FixedSet
        //
        //   NT    name               val   flags
        // +---------------------------------------------------------------------+
        // |   0 | file              | y  | parent_+_or_*                        |
        // |  15 | . file_1          | y  | child_+_or_*                         |
        // |   1 | file_item         | y  |                                      |
        // |   2 | header            | y  |                                      |
        // |   3 | declaration       | y  |                                      |
        // |   4 | option            | y  | parent_+_or_*                        |
        // |  16 | . option_1        | y  | child_+_or_*, sep_list               |
        // |   5 | rule              | y  | parent_left_fact                     |
        // |  21 | . rule_1          |    | child_left_fact                      |
        // |   6 | actions           | y  | parent_+_or_*                        |
        // |  17 | . actions_1       | y  | child_+_or_*, sep_list               |
        // |   7 | action            | y  |                                      |
        // |   8 | match             | y  |                                      |
        // |   9 | alt_items         | y  | parent_+_or_*                        |
        // |  18 | . alt_items_1     | y  | child_+_or_*, sep_list               |
        // |  10 | alt_item          | y  | parent_+_or_*, plus                  |
        // |  19 | . alt_item_1      | y  | child_+_or_*, parent_left_fact, plus |
        // |  25 | .   alt_item_2    |    | child_left_fact                      |
        // |  11 | repeat_item       | y  | parent_left_fact                     |
        // |  22 | . repeat_item_1   |    | parent_left_fact, child_left_fact    |
        // |  27 | .   repeat_item_2 |    | child_left_fact                      |
        // |  28 | .   repeat_item_3 |    | child_left_fact                      |
        // |  12 | item              | y  | right_rec, parent_left_fact          |
        // |  23 | . item_1          |    | child_left_fact                      |
        // |  13 | char_set          | y  | parent_+_or_*, plus                  |
        // |  20 | . char_set_1      | y  | child_+_or_*, parent_left_fact, plus |
        // |  26 | .   char_set_2    |    | child_left_fact                      |
        // |  14 | char_set_one      | y  | parent_left_fact                     |
        // |  24 | . char_set_one_1  |    | child_left_fact                      |
        // +---------------------------------------------------------------------+
        (901, true, false, true, 0, btreemap![
            0 => "SynFile".to_string(),
            1 => "SynFileItem".to_string(),
            2 => "SynHeader".to_string(),
            3 => "SynDeclaration".to_string(),
            4 => "SynOption".to_string(),
            5 => "SynRule".to_string(),
            6 => "SynActions".to_string(),
            7 => "SynAction".to_string(),
            8 => "SynMatch".to_string(),
            9 => "SynAltItems".to_string(),
            10 => "SynAltItem".to_string(),
            11 => "SynRepeatItem".to_string(),
            12 => "SynItem".to_string(),
            13 => "SynCharSet".to_string(),
            14 => "SynCharSetOne".to_string(),
            15 => "SynFile1".to_string(),
            16 => "SynOption1".to_string(),
            17 => "SynActions1".to_string(),
            18 => "SynAltItems1".to_string(),
            19 => "SynAltItem1".to_string(),
            20 => "SynCharSet1".to_string(),
        ], vec![
            (strip![exit 0, nt 15, nt 2],                  2, symbols![nt 2, nt 15]),      //  0: file -> header file_1                    | ◄0 ►file_1 ►header                  | 2    | header file_1
            (strip![exit 1, nt 15],                        1, symbols![nt 15]),            //  1: file -> file_1                           | ◄1 ►file_1                          | 1    | file_1
            (strip![exit 2, nt 4],                         1, symbols![nt 4]),             //  2: file_item -> option                      | ◄2 ►option                          | 1    | option
            (strip![exit 3, nt 3],                         1, symbols![nt 3]),             //  3: file_item -> declaration                 | ◄3 ►declaration                     | 1    | declaration
            (strip![exit 4, nt 5],                         1, symbols![nt 5]),             //  4: file_item -> rule                        | ◄4 ►rule                            | 1    | rule
            (strip![exit 5, t 14, t 27, t 18],             3, symbols![t 27]),             //  5: header -> "lexicon" Id ";"               | ◄5 ";" Id! "lexicon"                | 3    | Id
            (strip![exit 6, t 14, t 27, t 19],             3, symbols![t 27]),             //  6: declaration -> "mode" Id ";"             | ◄6 ";" Id! "mode"                   | 3    | Id
            (strip![exit 7, t 12, nt 16, t 27, t 5, t 16], 4, symbols![nt 16]),            //  7: option -> "channels" "{" Id option_1 "}" | ◄7 "}" ►option_1 Id! "{" "channels" | 4    | option_1
            (strip![exit 8, t 14, nt 8, t 1, t 27, t 17],  5, symbols![t 27, nt 8]),       //  8: rule -> "fragment" Id ":" match ";"      | ◄8 ";" ►match ":" Id! "fragment"    | 5    | Id match
            (strip![nt 21, nt 8, t 1, t 27],               0, symbols![]),                 //  9: rule -> Id ":" match rule_1              | ►rule_1 ►match ":" Id!              | 0    |
            (strip![exit 10, nt 17, nt 7],                 1, symbols![nt 17]),            // 10: actions -> action actions_1              | ◄10 ►actions_1 ►action              | 1    | actions_1
            (strip![exit 11, t 13, t 27, t 6, t 19],       4, symbols![t 27]),             // 11: action -> "mode" "(" Id ")"              | ◄11 ")" Id! "(" "mode"              | 4    | Id
            (strip![exit 12, t 13, t 27, t 6, t 21],       4, symbols![t 27]),             // 12: action -> "push" "(" Id ")"              | ◄12 ")" Id! "(" "push"              | 4    | Id
            (strip![exit 13, t 20],                        1, symbols![]),                 // 13: action -> "pop"                          | ◄13 "pop"                           | 1    |
            (strip![exit 14, t 23],                        1, symbols![]),                 // 14: action -> "skip"                         | ◄14 "skip"                          | 1    |
            (strip![exit 15, t 22],                        1, symbols![]),                 // 15: action -> "more"                         | ◄15 "more"                          | 1    |
            (strip![exit 16, t 13, t 27, t 6, t 24],       4, symbols![t 27]),             // 16: action -> "type" "(" Id ")"              | ◄16 ")" Id! "(" "type"              | 4    | Id
            (strip![exit 17, t 13, t 27, t 6, t 25],       4, symbols![t 27]),             // 17: action -> "channel" "(" Id ")"           | ◄17 ")" Id! "(" "channel"           | 4    | Id
            (strip![exit 18, nt 9],                        1, symbols![nt 9]),             // 18: match -> alt_items                       | ◄18 ►alt_items                      | 1    | alt_items
            (strip![exit 19, nt 18, nt 10],                1, symbols![nt 18]),            // 19: alt_items -> alt_item alt_items_1        | ◄19 ►alt_items_1 ►alt_item          | 1    | alt_items_1
            (strip![exit 20, nt 19],                       1, symbols![nt 19]),            // 20: alt_item -> alt_item_1                   | ◄20 ►alt_item_1                     | 1    | alt_item_1
            (strip![nt 22, nt 12],                         0, symbols![]),                 // 21: repeat_item -> item repeat_item_1        | ►repeat_item_1 ►item                | 0    |
            (strip![exit 22, t 13, nt 9, t 6],             3, symbols![nt 9]),             // 22: item -> "(" alt_items ")"                | ◄22 ")" ►alt_items "("              | 3    | alt_items
            (strip![exit 23, nt 12, t 7],                  2, symbols![nt 12]),            // 23: item -> "~" item                         | ◄23 ►item "~"                       | 2    | item
            (strip![exit 24, t 27],                        1, symbols![t 27]),             // 24: item -> Id                               | ◄24 Id!                             | 1    | Id
            (strip![nt 23, t 28],                          0, symbols![]),                 // 25: item -> CharLit item_1                   | ►item_1 CharLit!                    | 0    |
            (strip![exit 26, t 29],                        1, symbols![t 29]),             // 26: item -> StrLit                           | ◄26 StrLit!                         | 1    | StrLit
            (strip![exit 27, nt 13],                       1, symbols![nt 13]),            // 27: item -> char_set                         | ◄27 ►char_set                       | 1    | char_set
            (strip![exit 28, t 32, nt 20, t 31],           3, symbols![nt 20]),            // 28: char_set -> "[" char_set_1 "]"           | ◄28 "]" ►char_set_1 "["             | 3    | char_set_1
            (strip![exit 29, t 3],                         1, symbols![]),                 // 29: char_set -> "."                          | ◄29 "."                             | 1    |
            (strip![exit 30, t 30],                        1, symbols![t 30]),             // 30: char_set -> FixedSet                     | ◄30 FixedSet!                       | 1    | FixedSet
            (strip![exit 31, t 30],                        1, symbols![t 30]),             // 31: char_set_one -> FixedSet                 | ◄31 FixedSet!                       | 1    | FixedSet
            (strip![nt 24, t 33],                          0, symbols![]),                 // 32: char_set_one -> SetChar char_set_one_1   | ►char_set_one_1 SetChar!            | 0    |
            (strip![loop 15, exit 33, nt 1],               2, symbols![nt 15, nt 1]),      // 33: file_1 -> file_item file_1               | ●file_1 ◄33 ►file_item              | 2    | file_1 file_item
            (strip![exit 34],                              1, symbols![nt 15]),            // 34: file_1 -> ε                              | ◄34                                 | 1    | file_1
            (strip![loop 16, exit 35, t 27, t 2],          3, symbols![nt 16, t 27]),      // 35: option_1 -> "," Id option_1              | ●option_1 ◄35 Id! ","               | 3, 1 | option_1 Id
            (strip![exit 36],                              1, symbols![nt 16]),            // 36: option_1 -> ε                            | ◄36                                 | 1    | option_1
            (strip![loop 17, exit 37, nt 7, t 2],          3, symbols![nt 17, nt 7]),      // 37: actions_1 -> "," action actions_1        | ●actions_1 ◄37 ►action ","          | 3, 1 | actions_1 action
            (strip![exit 38],                              1, symbols![nt 17]),            // 38: actions_1 -> ε                           | ◄38                                 | 1    | actions_1
            (strip![loop 18, exit 39, nt 10, t 10],        3, symbols![nt 18, nt 10]),     // 39: alt_items_1 -> "|" alt_item alt_items_1  | ●alt_items_1 ◄39 ►alt_item "|"      | 3, 1 | alt_items_1 alt_item
            (strip![exit 40],                              1, symbols![nt 18]),            // 40: alt_items_1 -> ε                         | ◄40                                 | 1    | alt_items_1
            (strip![nt 25, nt 11],                         0, symbols![]),                 // 41: alt_item_1 -> repeat_item alt_item_2     | ►alt_item_2 ►repeat_item            | 0    |
            (strip![nt 26, nt 14],                         0, symbols![]),                 // 42: char_set_1 -> char_set_one char_set_2    | ►char_set_2 ►char_set_one           | 0    |
            (strip![exit 43, t 14, nt 6, t 0],             6, symbols![t 27, nt 8, nt 6]), // 43: rule_1 -> "->" actions ";"               | ◄43 ";" ►actions "->"               | 6    | Id match actions
            (strip![exit 44, t 14],                        4, symbols![t 27, nt 8]),       // 44: rule_1 -> ";"                            | ◄44 ";"                             | 4    | Id match
            (strip![nt 27, t 9],                           0, symbols![]),                 // 45: repeat_item_1 -> "+" repeat_item_2       | ►repeat_item_2 "+"                  | 0    |
            (strip![exit 46, t 11],                        2, symbols![nt 12]),            // 46: repeat_item_1 -> "?"                     | ◄46 "?"                             | 2    | item
            (strip![nt 28, t 15],                          0, symbols![]),                 // 47: repeat_item_1 -> "*" repeat_item_3       | ►repeat_item_3 "*"                  | 0    |
            (strip![exit 48],                              1, symbols![nt 12]),            // 48: repeat_item_1 -> ε                       | ◄48                                 | 1    | item
            (strip![exit 49, t 28, t 4],                   3, symbols![t 28, t 28]),       // 49: item_1 -> ".." CharLit                   | ◄49 CharLit! ".."                   | 3    | CharLit CharLit
            (strip![exit 50],                              1, symbols![t 28]),             // 50: item_1 -> ε                              | ◄50                                 | 1    | CharLit
            (strip![exit 51, t 33, t 8],                   3, symbols![t 33, t 33]),       // 51: char_set_one_1 -> "-" SetChar            | ◄51 SetChar! "-"                    | 3    | SetChar SetChar
            (strip![exit 52],                              1, symbols![t 33]),             // 52: char_set_one_1 -> ε                      | ◄52                                 | 1    | SetChar
            (strip![loop 19, exit 53],                     2, symbols![nt 19, nt 11]),     // 53: alt_item_2 -> alt_item_1                 | ●alt_item_1 ◄53                     | 2    | alt_item_1 repeat_item
            (strip![exit 54],                              2, symbols![nt 19, nt 11]),     // 54: alt_item_2 -> ε                          | ◄54                                 | 2    | alt_item_1 repeat_item
            (strip![loop 20, exit 55],                     2, symbols![nt 20, nt 14]),     // 55: char_set_2 -> char_set_1                 | ●char_set_1 ◄55                     | 2    | char_set_1 char_set_one
            (strip![exit 56],                              2, symbols![nt 20, nt 14]),     // 56: char_set_2 -> ε                          | ◄56                                 | 2    | char_set_1 char_set_one
            (strip![exit 57, t 11],                        3, symbols![nt 12]),            // 57: repeat_item_2 -> "?"                     | ◄57 "?"                             | 3    | item
            (strip![exit 58],                              2, symbols![nt 12]),            // 58: repeat_item_2 -> ε                       | ◄58                                 | 2    | item
            (strip![exit 59, t 11],                        3, symbols![nt 12]),            // 59: repeat_item_3 -> "?"                     | ◄59 "?"                             | 3    | item
            (strip![exit 60],                              2, symbols![nt 12]),            // 60: repeat_item_3 -> ε                       | ◄60                                 | 2    | item
        ], true, NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2, 3, 4], 2 => vec![5], 3 => vec![6], 4 => vec![7], 5 => vec![8, 43, 44], 6 => vec![10], 7 => vec![11, 12, 13, 14, 15, 16, 17], 8 => vec![18], 9 => vec![19], 10 => vec![20], 11 => vec![46, 48, 57, 58, 59, 60], 12 => vec![22, 23, 24, 26, 27, 49, 50], 13 => vec![28, 29, 30], 14 => vec![31, 51, 52]]),

        // program -> (<L=decl_i> decl)* (<L=inst_i> inst)+
        // decl -> Type (<L=id_i> Id / ",")+ ";" | "typedef" Type Id ";"
        // inst -> "let" Id "=" expr ";" | "print" expr ";"
        // expr -> "-" expr | expr "+" expr | expr <P> "-" expr | Id | Num
        //
        //   NT    name          val   flags
        // +------------------------------------------------------------------------+
        // |   0 | program      | y  | parent_+_or_*, plus                          |
        // |   1 | . decl_i     | y  | child_+_or_*, L-form                         |
        // |   2 | . inst_i     | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |   9 | .   inst_i_1 |    | child_left_fact                              |
        // |   3 | decl         | y  | parent_+_or_*                                |
        // |   4 | . id_i       | y  | child_+_or_*, L-form, sep_list               |
        // |   5 | inst         | y  |                                              |
        // |   6 | expr         | y  | parent_left_rec, parent_amb                  |
        // |   7 | . expr_1     |    | child_left_rec                               |
        // |   8 | . expr_2     |    | right_rec                                    |
        // +------------------------------------------------------------------------+
        (902, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],               2, symbols![nt 1, nt 2]), //  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i         | 2    | decl_i inst_i
            (strip![loop 1, exit 1, nt 3],             2, symbols![nt 1, nt 3]), //  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl           | 2    | decl_i decl
            (strip![exit 2],                           1, symbols![nt 1]),       //  2: decl_i -> ε                   | ◄2                         | 1    | decl_i
            (strip![nt 9, nt 5],                       0, symbols![]),           //  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst            | 0    |
            (strip![exit 4, t 4, nt 4, t 1, t 2],      3, symbols![t 2, nt 4]),  //  4: decl -> Type Id id_i ";"      | ◄4 ";" ►id_i Id! Type!     | 3    | Type id_i
            (strip![exit 5, t 4, t 1, t 2, t 5],       4, symbols![t 2, t 1]),   //  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef" | 4    | Type Id
            (strip![loop 4, exit 6, t 1, t 3],         3, symbols![nt 4, t 1]),  //  6: id_i -> "," Id id_i           | ●id_i ◄6 Id! ","           | 3, 1 | id_i Id
            (strip![exit 7],                           1, symbols![nt 4]),       //  7: id_i -> ε                     | ◄7                         | 1    | id_i
            (strip![exit 8, t 4, nt 6, t 7, t 1, t 6], 5, symbols![t 1, nt 6]),  //  8: inst -> "let" Id "=" expr ";" | ◄8 ";" ►expr "=" Id! "let" | 5    | Id expr
            (strip![exit 9, t 4, nt 6, t 8],           3, symbols![nt 6]),       //  9: inst -> "print" expr ";"      | ◄9 ";" ►expr "print"       | 3    | expr
            (strip![nt 7, exit 10, nt 8],              1, symbols![nt 6]),       // 10: expr -> expr_2 expr_1         | ►expr_1 ◄10 ►expr_2        | 1    | expr
            (strip![loop 7, exit 11, nt 8, t 10],      3, symbols![nt 6, nt 6]), // 11: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄11 ►expr_2 "+"    | 3    | expr expr
            (strip![loop 7, exit 12, nt 8, t 9],       3, symbols![nt 6, nt 6]), // 12: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "-"    | 3    | expr expr
            (strip![exit 13],                          1, symbols![nt 6]),       // 13: expr_1 -> ε                   | ◄13                        | 1    | expr
            (strip![exit 14, nt 8, t 9],               2, symbols![nt 6]),       // 14: expr_2 -> "-" expr_2          | ◄14 ►expr_2 "-"            | 2    | expr
            (strip![exit 15, t 1],                     1, symbols![t 1]),        // 15: expr_2 -> Id                  | ◄15 Id!                    | 1    | Id
            (strip![exit 16, t 0],                     1, symbols![t 0]),        // 16: expr_2 -> Num                 | ◄16 Num!                   | 1    | Num
            (strip![loop 2, exit 17],                  2, symbols![nt 2, nt 5]), // 17: inst_i_1 -> inst_i            | ●inst_i ◄17                | 2    | inst_i inst
            (strip![exit 18],                          2, symbols![nt 2, nt 5]), // 18: inst_i_1 -> ε                 | ◄18                        | 2    | inst_i inst
        ], true, NTValue::Default, btreemap![0 => vec![0], 3 => vec![4, 5], 5 => vec![8, 9], 6 => vec![10]]),
        // program: n, decl_i: n, inst_i: n, decl: n, id_i: n, inst: n, expr: n, expr_1: n, expr_2: n, inst_i_1: n
        (902, true, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],               2, symbols![]),         //  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i         | 2    |
            (strip![loop 1, exit 1, nt 3],             2, symbols![]),         //  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl           | 2    |
            (strip![exit 2],                           1, symbols![]),         //  2: decl_i -> ε                   | ◄2                         | 1    |
            (strip![nt 9, nt 5],                       0, symbols![]),         //  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst            | 0    |
            (strip![exit 4, t 4, nt 4, t 1, t 2],      3, symbols![t 2]),      //  4: decl -> Type Id id_i ";"      | ◄4 ";" ►id_i Id! Type!     | 3    | Type
            (strip![exit 5, t 4, t 1, t 2, t 5],       4, symbols![t 2, t 1]), //  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef" | 4    | Type Id
            (strip![loop 4, exit 6, t 1, t 3],         3, symbols![t 1]),      //  6: id_i -> "," Id id_i           | ●id_i ◄6 Id! ","           | 3, 1 | Id
            (strip![exit 7],                           1, symbols![]),         //  7: id_i -> ε                     | ◄7                         | 1    |
            (strip![exit 8, t 4, nt 6, t 7, t 1, t 6], 5, symbols![t 1]),      //  8: inst -> "let" Id "=" expr ";" | ◄8 ";" ►expr "=" Id! "let" | 5    | Id
            (strip![exit 9, t 4, nt 6, t 8],           3, symbols![]),         //  9: inst -> "print" expr ";"      | ◄9 ";" ►expr "print"       | 3    |
            (strip![nt 7, exit 10, nt 8],              1, symbols![]),         // 10: expr -> expr_2 expr_1         | ►expr_1 ◄10 ►expr_2        | 1    |
            (strip![loop 7, exit 11, nt 8, t 10],      3, symbols![]),         // 11: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄11 ►expr_2 "+"    | 3    |
            (strip![loop 7, exit 12, nt 8, t 9],       3, symbols![]),         // 12: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "-"    | 3    |
            (strip![exit 13],                          1, symbols![]),         // 13: expr_1 -> ε                   | ◄13                        | 1    |
            (strip![exit 14, nt 8, t 9],               2, symbols![]),         // 14: expr_2 -> "-" expr_2          | ◄14 ►expr_2 "-"            | 2    |
            (strip![exit 15, t 1],                     1, symbols![t 1]),      // 15: expr_2 -> Id                  | ◄15 Id!                    | 1    | Id
            (strip![exit 16, t 0],                     1, symbols![t 0]),      // 16: expr_2 -> Num                 | ◄16 Num!                   | 1    | Num
            (strip![loop 2, exit 17],                  2, symbols![]),         // 17: inst_i_1 -> inst_i            | ●inst_i ◄17                | 2    |
            (strip![exit 18],                          2, symbols![]),         // 18: inst_i_1 -> ε                 | ◄18                        | 2    |
        ], true, NTValue::None, btreemap![0 => vec![0], 3 => vec![4, 5], 5 => vec![8, 9], 6 => vec![10]]),

        // program -> (<L=stmt_i> stmt)*
        // stmt -> decl | inst
        // decl -> Type (Id / ",")+ ";" | "typedef" Type Id ";"
        // inst -> Id "=" expr ";" | "print" expr ";"
        // expr -> "-" expr | expr "+" expr | expr <P> "-" expr | Id | Num
        //
        //   NT    name      val   flags
        // +---------------------------------------------------+
        // |   0 | program  | y  | parent_+_or_*               |
        // |   1 | . stmt_i | y  | child_+_or_*, L-form        |
        // |   2 | stmt     | y  |                             |
        // |   3 | decl     | y  | parent_+_or_*               |
        // |   6 | . decl_1 | y  | child_+_or_*, sep_list      |
        // |   4 | inst     | y  |                             |
        // |   5 | expr     | y  | parent_left_rec, parent_amb |
        // |   7 | . expr_1 |    | child_left_rec              |
        // |   8 | . expr_2 |    | right_rec                   |
        // +---------------------------------------------------+
        (903, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 1],                  1, symbols![nt 1]),       //  0: program -> stmt_i             | ◄0 ►stmt_i                 | 1    | stmt_i
            (strip![loop 1, exit 1, nt 2],          2, symbols![nt 1, nt 2]), //  1: stmt_i -> stmt stmt_i         | ●stmt_i ◄1 ►stmt           | 2    | stmt_i stmt
            (strip![exit 2],                        1, symbols![nt 1]),       //  2: stmt_i -> ε                   | ◄2                         | 1    | stmt_i
            (strip![exit 3, nt 3],                  1, symbols![nt 3]),       //  3: stmt -> decl                  | ◄3 ►decl                   | 1    | decl
            (strip![exit 4, nt 4],                  1, symbols![nt 4]),       //  4: stmt -> inst                  | ◄4 ►inst                   | 1    | inst
            (strip![exit 5, t 4, nt 6, t 1, t 2],   3, symbols![t 2, nt 6]),  //  5: decl -> Type Id decl_1 ";"    | ◄5 ";" ►decl_1 Id! Type!   | 3    | Type decl_1
            (strip![exit 6, t 4, t 1, t 2, t 5],    4, symbols![t 2, t 1]),   //  6: decl -> "typedef" Type Id ";" | ◄6 ";" Id! Type! "typedef" | 4    | Type Id
            (strip![exit 7, t 4, nt 5, t 6, t 1],   4, symbols![t 1, nt 5]),  //  7: inst -> Id "=" expr ";"       | ◄7 ";" ►expr "=" Id!       | 4    | Id expr
            (strip![exit 8, t 4, nt 5, t 7],        3, symbols![nt 5]),       //  8: inst -> "print" expr ";"      | ◄8 ";" ►expr "print"       | 3    | expr
            (strip![nt 7, exit 9, nt 8],            1, symbols![nt 5]),       //  9: expr -> expr_2 expr_1         | ►expr_1 ◄9 ►expr_2         | 1    | expr
            (strip![loop 6, exit 10, t 1, t 3],     3, symbols![nt 6, t 1]),  // 10: decl_1 -> "," Id decl_1       | ●decl_1 ◄10 Id! ","        | 3, 1 | decl_1 Id
            (strip![exit 11],                       1, symbols![nt 6]),       // 11: decl_1 -> ε                   | ◄11                        | 1    | decl_1
            (strip![loop 7, exit 12, nt 8, t 9],    3, symbols![nt 5, nt 5]), // 12: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "+"    | 3    | expr expr
            (strip![loop 7, exit 13, nt 8, t 8],    3, symbols![nt 5, nt 5]), // 13: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄13 ►expr_2 "-"    | 3    | expr expr
            (strip![exit 14],                       1, symbols![nt 5]),       // 14: expr_1 -> ε                   | ◄14                        | 1    | expr
            (strip![exit 15, nt 8, t 8],            2, symbols![nt 5]),       // 15: expr_2 -> "-" expr_2          | ◄15 ►expr_2 "-"            | 2    | expr
            (strip![exit 16, t 1],                  1, symbols![t 1]),        // 16: expr_2 -> Id                  | ◄16 Id!                    | 1    | Id
            (strip![exit 17, t 0],                  1, symbols![t 0]),        // 17: expr_2 -> Num                 | ◄17 Num!                   | 1    | Num
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3, 4], 3 => vec![5, 6], 4 => vec![7, 8], 5 => vec![9]]),

        // statement -> assign ";" | print ";"
        // assign -> "let" Id "=" value
        // print -> "print" value
        // value -> Id | Num
        //
        //   NT    name       val   flags
        // +------------------------------+
        // |   0 | statement | y  |       |
        // |   1 | assign    | y  |       |
        // |   2 | print     | y  |       |
        // |   3 | value     | y  |       |
        // +------------------------------+
        (904, false, false, true, 0, btreemap![
        ], vec![
            (strip![exit 0, t 2, nt 1],             2, symbols![nt 1]),      //  0: statement -> assign ";"      | ◄0 ";" ►assign          | 2 | assign
            (strip![exit 1, t 2, nt 2],             2, symbols![nt 2]),      //  1: statement -> print ";"       | ◄1 ";" ►print           | 2 | print
            (strip![exit 2, nt 3, t 4, t 0, t 3],   4, symbols![t 0, nt 3]), //  2: assign -> "let" Id "=" value | ◄2 ►value "=" Id! "let" | 4 | Id value
            (strip![exit 3, nt 3, t 5],             2, symbols![nt 3]),      //  3: print -> "print" value       | ◄3 ►value "print"       | 2 | value
            (strip![exit 4, t 0],                   1, symbols![t 0]),       //  4: value -> Id                  | ◄4 Id!                  | 1 | Id
            (strip![exit 5, t 1],                   1, symbols![t 1]),       //  5: value -> Num                 | ◄5 Num!                 | 1 | Num
        ], true, NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3], 3 => vec![4, 5]]),

        // a -> s p
        // s -> vs ns xs
        // p -> vp np xp
        // vs -> A B*
        // ns -> "A" "B"*
        // xs -> A x*
        // vp -> A B+
        // np -> "A" "B"+
        // xp -> A x+
        // x -> "X"
        //
        //   NT    name      val   flags
        // +------------------------------------------------------------+
        // |   0 | a        | y  |                                      |
        // |   1 | s        | y  |                                      |
        // |   2 | p        | y  |                                      |
        // |   3 | vs       | y  | parent_+_or_*                        |
        // |  10 | . vs_1   | y  | child_+_or_*                         |
        // |   4 | ns       | y  | parent_+_or_*                        |
        // |  11 | . ns_1   |    | child_+_or_*                         |
        // |   5 | xs       | y  | parent_+_or_*                        |
        // |  12 | . xs_1   |    | child_+_or_*                         |
        // |   6 | vp       | y  | parent_+_or_*                        |
        // |  13 | . vp_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |  16 | .   vp_2 |    | child_left_fact                      |
        // |   7 | np       | y  | parent_+_or_*                        |
        // |  14 | . np_1   |    | child_+_or_*, parent_left_fact, plus |
        // |  17 | .   np_2 |    | child_left_fact                      |
        // |   8 | xp       | y  | parent_+_or_*                        |
        // |  15 | . xp_1   |    | child_+_or_*, parent_left_fact, plus |
        // |  18 | .   xp_2 |    | child_left_fact                      |
        // |   9 | x        |    |                                      |
        // +------------------------------------------------------------+
        (980, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),       //  0: a -> s p         | ◄0 ►p ►s       | 2 | s p
            (strip![exit 1, nt 5, nt 4, nt 3],      3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs    | ◄1 ►xs ►ns ►vs | 3 | vs ns xs
            (strip![exit 2, nt 8, nt 7, nt 6],      3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp    | ◄2 ►xp ►np ►vp | 3 | vp np xp
            (strip![exit 3, nt 10, t 0],            2, symbols![t 0, nt 10]),       //  3: vs -> A vs_1     | ◄3 ►vs_1 A!    | 2 | A vs_1
            (strip![exit 4, nt 11, t 2],            2, symbols![]),                 //  4: ns -> "A" ns_1   | ◄4 ►ns_1 "A"   | 2 |
            (strip![exit 5, nt 12, t 0],            2, symbols![t 0]),              //  5: xs -> A xs_1     | ◄5 ►xs_1 A!    | 2 | A
            (strip![exit 6, nt 13, t 0],            2, symbols![t 0, nt 13]),       //  6: vp -> A vp_1     | ◄6 ►vp_1 A!    | 2 | A vp_1
            (strip![exit 7, nt 14, t 2],            2, symbols![]),                 //  7: np -> "A" np_1   | ◄7 ►np_1 "A"   | 2 |
            (strip![exit 8, nt 15, t 0],            2, symbols![t 0]),              //  8: xp -> A xp_1     | ◄8 ►xp_1 A!    | 2 | A
            (strip![exit 9, t 4],                   1, symbols![]),                 //  9: x -> "X"         | ◄9 "X"         | 1 |
            (strip![loop 10, exit 10, t 1],         2, symbols![nt 10, t 1]),       // 10: vs_1 -> B vs_1   | ●vs_1 ◄10 B!   | 2 | vs_1 B
            (strip![exit 11],                       1, symbols![nt 10]),            // 11: vs_1 -> ε        | ◄11            | 1 | vs_1
            (strip![loop 11, exit 12, t 3],         2, symbols![]),                 // 12: ns_1 -> "B" ns_1 | ●ns_1 ◄12 "B"  | 2 |
            (strip![exit 13],                       1, symbols![]),                 // 13: ns_1 -> ε        | ◄13            | 1 |
            (strip![loop 12, exit 14, nt 9],        2, symbols![]),                 // 14: xs_1 -> x xs_1   | ●xs_1 ◄14 ►x   | 2 |
            (strip![exit 15],                       1, symbols![]),                 // 15: xs_1 -> ε        | ◄15            | 1 |
            (strip![nt 16, t 1],                    0, symbols![]),                 // 16: vp_1 -> B vp_2   | ►vp_2 B!       | 0 |
            (strip![nt 17, t 3],                    0, symbols![]),                 // 17: np_1 -> "B" np_2 | ►np_2 "B"      | 0 |
            (strip![nt 18, nt 9],                   0, symbols![]),                 // 18: xp_1 -> x xp_2   | ►xp_2 ►x       | 0 |
            (strip![loop 13, exit 19],              2, symbols![nt 13, t 1]),       // 19: vp_2 -> vp_1     | ●vp_1 ◄19      | 2 | vp_1 B
            (strip![exit 20],                       2, symbols![nt 13, t 1]),       // 20: vp_2 -> ε        | ◄20            | 2 | vp_1 B
            (strip![loop 14, exit 21],              2, symbols![]),                 // 21: np_2 -> np_1     | ●np_1 ◄21      | 2 |
            (strip![exit 22],                       2, symbols![]),                 // 22: np_2 -> ε        | ◄22            | 2 |
            (strip![loop 15, exit 23],              2, symbols![]),                 // 23: xp_2 -> xp_1     | ●xp_1 ◄23      | 2 |
            (strip![exit 24],                       2, symbols![]),                 // 24: xp_2 -> ε        | ◄24            | 2 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 6, 7, 8]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5], 6 => vec![6], 7 => vec![7], 8 => vec![8], 9 => vec![9]]),
        // a: y, s: y, p: y, vs: y, ns: y, xs: y, vp: y, np: y, xp: y, x: n, vs_1: y, ns_1: n, xs_1: n, vp_1: y, np_1: n, xp_1: n, vp_2: n, np_2: n, xp_2: n
        (980, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),       //  0: a -> s p         | ◄0 ►p ►s       | 2 | s p
            (strip![exit 1, nt 5, nt 4, nt 3],      3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs    | ◄1 ►xs ►ns ►vs | 3 | vs ns xs
            (strip![exit 2, nt 8, nt 7, nt 6],      3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp    | ◄2 ►xp ►np ►vp | 3 | vp np xp
            (strip![exit 3, nt 10, t 0],            2, symbols![t 0, nt 10]),       //  3: vs -> A vs_1     | ◄3 ►vs_1 A!    | 2 | A vs_1
            (strip![exit 4, nt 11, t 2],            2, symbols![]),                 //  4: ns -> "A" ns_1   | ◄4 ►ns_1 "A"   | 2 |
            (strip![exit 5, nt 12, t 0],            2, symbols![t 0]),              //  5: xs -> A xs_1     | ◄5 ►xs_1 A!    | 2 | A
            (strip![exit 6, nt 13, t 0],            2, symbols![t 0, nt 13]),       //  6: vp -> A vp_1     | ◄6 ►vp_1 A!    | 2 | A vp_1
            (strip![exit 7, nt 14, t 2],            2, symbols![]),                 //  7: np -> "A" np_1   | ◄7 ►np_1 "A"   | 2 |
            (strip![exit 8, nt 15, t 0],            2, symbols![t 0]),              //  8: xp -> A xp_1     | ◄8 ►xp_1 A!    | 2 | A
            (strip![exit 9, t 4],                   1, symbols![]),                 //  9: x -> "X"         | ◄9 "X"         | 1 |
            (strip![loop 10, exit 10, t 1],         2, symbols![nt 10, t 1]),       // 10: vs_1 -> B vs_1   | ●vs_1 ◄10 B!   | 2 | vs_1 B
            (strip![exit 11],                       1, symbols![nt 10]),            // 11: vs_1 -> ε        | ◄11            | 1 | vs_1
            (strip![loop 11, exit 12, t 3],         2, symbols![]),                 // 12: ns_1 -> "B" ns_1 | ●ns_1 ◄12 "B"  | 2 |
            (strip![exit 13],                       1, symbols![]),                 // 13: ns_1 -> ε        | ◄13            | 1 |
            (strip![loop 12, exit 14, nt 9],        2, symbols![]),                 // 14: xs_1 -> x xs_1   | ●xs_1 ◄14 ►x   | 2 |
            (strip![exit 15],                       1, symbols![]),                 // 15: xs_1 -> ε        | ◄15            | 1 |
            (strip![nt 16, t 1],                    0, symbols![]),                 // 16: vp_1 -> B vp_2   | ►vp_2 B!       | 0 |
            (strip![nt 17, t 3],                    0, symbols![]),                 // 17: np_1 -> "B" np_2 | ►np_2 "B"      | 0 |
            (strip![nt 18, nt 9],                   0, symbols![]),                 // 18: xp_1 -> x xp_2   | ►xp_2 ►x       | 0 |
            (strip![loop 13, exit 19],              2, symbols![nt 13, t 1]),       // 19: vp_2 -> vp_1     | ●vp_1 ◄19      | 2 | vp_1 B
            (strip![exit 20],                       2, symbols![nt 13, t 1]),       // 20: vp_2 -> ε        | ◄20            | 2 | vp_1 B
            (strip![loop 14, exit 21],              2, symbols![]),                 // 21: np_2 -> np_1     | ●np_1 ◄21      | 2 |
            (strip![exit 22],                       2, symbols![]),                 // 22: np_2 -> ε        | ◄22            | 2 |
            (strip![loop 15, exit 23],              2, symbols![]),                 // 23: xp_2 -> xp_1     | ●xp_1 ◄23      | 2 |
            (strip![exit 24],                       2, symbols![]),                 // 24: xp_2 -> ε        | ◄24            | 2 |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 6, 7, 8]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5], 6 => vec![6], 7 => vec![7], 8 => vec![8], 9 => vec![9]]),

        // a -> s p
        // s -> vs ns xs
        // p -> vp np xp
        // vs -> A (B | C D)*
        // ns -> "A" ("B" | "C" "D")*
        // xs -> A (x | y x)*
        // vp -> A (B | C D)+
        // np -> "A" ("B" | "C" "D")+
        // xp -> A (x | y x)+
        // x -> "X"
        // y -> "Y"
        //
        //   NT    name      val   flags
        // +------------------------------------------------------------+
        // |   0 | a        | y  |                                      |
        // |   1 | s        | y  |                                      |
        // |   2 | p        | y  |                                      |
        // |   3 | vs       | y  | parent_+_or_*                        |
        // |  11 | . vs_1   | y  | child_+_or_*                         |
        // |   4 | ns       | y  | parent_+_or_*                        |
        // |  12 | . ns_1   |    | child_+_or_*                         |
        // |   5 | xs       | y  | parent_+_or_*                        |
        // |  13 | . xs_1   |    | child_+_or_*                         |
        // |   6 | vp       | y  | parent_+_or_*                        |
        // |  14 | . vp_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |  17 | .   vp_2 |    | child_left_fact                      |
        // |  18 | .   vp_3 |    | child_left_fact                      |
        // |   7 | np       | y  | parent_+_or_*                        |
        // |  15 | . np_1   |    | child_+_or_*, parent_left_fact, plus |
        // |  19 | .   np_2 |    | child_left_fact                      |
        // |  20 | .   np_3 |    | child_left_fact                      |
        // |   8 | xp       | y  | parent_+_or_*                        |
        // |  16 | . xp_1   |    | child_+_or_*, parent_left_fact, plus |
        // |  21 | .   xp_2 |    | child_left_fact                      |
        // |  22 | .   xp_3 |    | child_left_fact                      |
        // |   9 | x        |    |                                      |
        // |  10 | y        |    |                                      |
        // +------------------------------------------------------------+
        (981, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),       //  0: a -> s p             | ◄0 ►p ►s          | 2 | s p
            (strip![exit 1, nt 5, nt 4, nt 3],      3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs        | ◄1 ►xs ►ns ►vs    | 3 | vs ns xs
            (strip![exit 2, nt 8, nt 7, nt 6],      3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp        | ◄2 ►xp ►np ►vp    | 3 | vp np xp
            (strip![exit 3, nt 11, t 0],            2, symbols![t 0, nt 11]),       //  3: vs -> A vs_1         | ◄3 ►vs_1 A!       | 2 | A vs_1
            (strip![exit 4, nt 12, t 4],            2, symbols![]),                 //  4: ns -> "A" ns_1       | ◄4 ►ns_1 "A"      | 2 |
            (strip![exit 5, nt 13, t 0],            2, symbols![t 0]),              //  5: xs -> A xs_1         | ◄5 ►xs_1 A!       | 2 | A
            (strip![exit 6, nt 14, t 0],            2, symbols![t 0, nt 14]),       //  6: vp -> A vp_1         | ◄6 ►vp_1 A!       | 2 | A vp_1
            (strip![exit 7, nt 15, t 4],            2, symbols![]),                 //  7: np -> "A" np_1       | ◄7 ►np_1 "A"      | 2 |
            (strip![exit 8, nt 16, t 0],            2, symbols![t 0]),              //  8: xp -> A xp_1         | ◄8 ►xp_1 A!       | 2 | A
            (strip![exit 9, t 8],                   1, symbols![]),                 //  9: x -> "X"             | ◄9 "X"            | 1 |
            (strip![exit 10, t 9],                  1, symbols![]),                 // 10: y -> "Y"             | ◄10 "Y"           | 1 |
            (strip![loop 11, exit 11, t 1],         2, symbols![nt 11, t 1]),       // 11: vs_1 -> B vs_1       | ●vs_1 ◄11 B!      | 2 | vs_1 B
            (strip![loop 11, exit 12, t 3, t 2],    3, symbols![nt 11, t 2, t 3]),  // 12: vs_1 -> C D vs_1     | ●vs_1 ◄12 D! C!   | 3 | vs_1 C D
            (strip![exit 13],                       1, symbols![nt 11]),            // 13: vs_1 -> ε            | ◄13               | 1 | vs_1
            (strip![loop 12, exit 14, t 5],         2, symbols![]),                 // 14: ns_1 -> "B" ns_1     | ●ns_1 ◄14 "B"     | 2 |
            (strip![loop 12, exit 15, t 7, t 6],    3, symbols![]),                 // 15: ns_1 -> "C" "D" ns_1 | ●ns_1 ◄15 "D" "C" | 3 |
            (strip![exit 16],                       1, symbols![]),                 // 16: ns_1 -> ε            | ◄16               | 1 |
            (strip![loop 13, exit 17, nt 9],        2, symbols![]),                 // 17: xs_1 -> x xs_1       | ●xs_1 ◄17 ►x      | 2 |
            (strip![loop 13, exit 18, nt 9, nt 10], 3, symbols![]),                 // 18: xs_1 -> y x xs_1     | ●xs_1 ◄18 ►x ►y   | 3 |
            (strip![exit 19],                       1, symbols![]),                 // 19: xs_1 -> ε            | ◄19               | 1 |
            (strip![nt 17, t 1],                    0, symbols![]),                 // 20: vp_1 -> B vp_2       | ►vp_2 B!          | 0 |
            (strip![nt 18, t 3, t 2],               0, symbols![]),                 // 21: vp_1 -> C D vp_3     | ►vp_3 D! C!       | 0 |
            (strip![nt 19, t 5],                    0, symbols![]),                 // 22: np_1 -> "B" np_2     | ►np_2 "B"         | 0 |
            (strip![nt 20, t 7, t 6],               0, symbols![]),                 // 23: np_1 -> "C" "D" np_3 | ►np_3 "D" "C"     | 0 |
            (strip![nt 21, nt 9],                   0, symbols![]),                 // 24: xp_1 -> x xp_2       | ►xp_2 ►x          | 0 |
            (strip![nt 22, nt 9, nt 10],            0, symbols![]),                 // 25: xp_1 -> y x xp_3     | ►xp_3 ►x ►y       | 0 |
            (strip![loop 14, exit 26],              2, symbols![nt 14, t 1]),       // 26: vp_2 -> vp_1         | ●vp_1 ◄26         | 2 | vp_1 B
            (strip![exit 27],                       2, symbols![nt 14, t 1]),       // 27: vp_2 -> ε            | ◄27               | 2 | vp_1 B
            (strip![loop 14, exit 28],              3, symbols![nt 14, t 2, t 3]),  // 28: vp_3 -> vp_1         | ●vp_1 ◄28         | 3 | vp_1 C D
            (strip![exit 29],                       3, symbols![nt 14, t 2, t 3]),  // 29: vp_3 -> ε            | ◄29               | 3 | vp_1 C D
            (strip![loop 15, exit 30],              2, symbols![]),                 // 30: np_2 -> np_1         | ●np_1 ◄30         | 2 |
            (strip![exit 31],                       2, symbols![]),                 // 31: np_2 -> ε            | ◄31               | 2 |
            (strip![loop 15, exit 32],              3, symbols![]),                 // 32: np_3 -> np_1         | ●np_1 ◄32         | 3 |
            (strip![exit 33],                       3, symbols![]),                 // 33: np_3 -> ε            | ◄33               | 3 |
            (strip![loop 16, exit 34],              2, symbols![]),                 // 34: xp_2 -> xp_1         | ●xp_1 ◄34         | 2 |
            (strip![exit 35],                       2, symbols![]),                 // 35: xp_2 -> ε            | ◄35               | 2 |
            (strip![loop 16, exit 36],              3, symbols![]),                 // 36: xp_3 -> xp_1         | ●xp_1 ◄36         | 3 |
            (strip![exit 37],                       3, symbols![]),                 // 37: xp_3 -> ε            | ◄37               | 3 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 6, 7, 8]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5], 6 => vec![6], 7 => vec![7], 8 => vec![8], 9 => vec![9], 10 => vec![10]]),
        // a: y, s: y, p: y, vs: y, ns: y, xs: y, vp: y, np: y, xp: y, x: n, y: n, vs_1: y, ns_1: n, xs_1: n, vp_1: y, np_1: n, xp_1: n, vp_2: n, vp_3: n, np_2: n, np_3: n, xp_2: n, xp_3: n
        (981, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),       //  0: a -> s p             | ◄0 ►p ►s          | 2 | s p
            (strip![exit 1, nt 5, nt 4, nt 3],      3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs        | ◄1 ►xs ►ns ►vs    | 3 | vs ns xs
            (strip![exit 2, nt 8, nt 7, nt 6],      3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp        | ◄2 ►xp ►np ►vp    | 3 | vp np xp
            (strip![exit 3, nt 11, t 0],            2, symbols![t 0, nt 11]),       //  3: vs -> A vs_1         | ◄3 ►vs_1 A!       | 2 | A vs_1
            (strip![exit 4, nt 12, t 4],            2, symbols![]),                 //  4: ns -> "A" ns_1       | ◄4 ►ns_1 "A"      | 2 |
            (strip![exit 5, nt 13, t 0],            2, symbols![t 0]),              //  5: xs -> A xs_1         | ◄5 ►xs_1 A!       | 2 | A
            (strip![exit 6, nt 14, t 0],            2, symbols![t 0, nt 14]),       //  6: vp -> A vp_1         | ◄6 ►vp_1 A!       | 2 | A vp_1
            (strip![exit 7, nt 15, t 4],            2, symbols![]),                 //  7: np -> "A" np_1       | ◄7 ►np_1 "A"      | 2 |
            (strip![exit 8, nt 16, t 0],            2, symbols![t 0]),              //  8: xp -> A xp_1         | ◄8 ►xp_1 A!       | 2 | A
            (strip![exit 9, t 8],                   1, symbols![]),                 //  9: x -> "X"             | ◄9 "X"            | 1 |
            (strip![exit 10, t 9],                  1, symbols![]),                 // 10: y -> "Y"             | ◄10 "Y"           | 1 |
            (strip![loop 11, exit 11, t 1],         2, symbols![nt 11, t 1]),       // 11: vs_1 -> B vs_1       | ●vs_1 ◄11 B!      | 2 | vs_1 B
            (strip![loop 11, exit 12, t 3, t 2],    3, symbols![nt 11, t 2, t 3]),  // 12: vs_1 -> C D vs_1     | ●vs_1 ◄12 D! C!   | 3 | vs_1 C D
            (strip![exit 13],                       1, symbols![nt 11]),            // 13: vs_1 -> ε            | ◄13               | 1 | vs_1
            (strip![loop 12, exit 14, t 5],         2, symbols![]),                 // 14: ns_1 -> "B" ns_1     | ●ns_1 ◄14 "B"     | 2 |
            (strip![loop 12, exit 15, t 7, t 6],    3, symbols![]),                 // 15: ns_1 -> "C" "D" ns_1 | ●ns_1 ◄15 "D" "C" | 3 |
            (strip![exit 16],                       1, symbols![]),                 // 16: ns_1 -> ε            | ◄16               | 1 |
            (strip![loop 13, exit 17, nt 9],        2, symbols![]),                 // 17: xs_1 -> x xs_1       | ●xs_1 ◄17 ►x      | 2 |
            (strip![loop 13, exit 18, nt 9, nt 10], 3, symbols![]),                 // 18: xs_1 -> y x xs_1     | ●xs_1 ◄18 ►x ►y   | 3 |
            (strip![exit 19],                       1, symbols![]),                 // 19: xs_1 -> ε            | ◄19               | 1 |
            (strip![nt 17, t 1],                    0, symbols![]),                 // 20: vp_1 -> B vp_2       | ►vp_2 B!          | 0 |
            (strip![nt 18, t 3, t 2],               0, symbols![]),                 // 21: vp_1 -> C D vp_3     | ►vp_3 D! C!       | 0 |
            (strip![nt 19, t 5],                    0, symbols![]),                 // 22: np_1 -> "B" np_2     | ►np_2 "B"         | 0 |
            (strip![nt 20, t 7, t 6],               0, symbols![]),                 // 23: np_1 -> "C" "D" np_3 | ►np_3 "D" "C"     | 0 |
            (strip![nt 21, nt 9],                   0, symbols![]),                 // 24: xp_1 -> x xp_2       | ►xp_2 ►x          | 0 |
            (strip![nt 22, nt 9, nt 10],            0, symbols![]),                 // 25: xp_1 -> y x xp_3     | ►xp_3 ►x ►y       | 0 |
            (strip![loop 14, exit 26],              2, symbols![nt 14, t 1]),       // 26: vp_2 -> vp_1         | ●vp_1 ◄26         | 2 | vp_1 B
            (strip![exit 27],                       2, symbols![nt 14, t 1]),       // 27: vp_2 -> ε            | ◄27               | 2 | vp_1 B
            (strip![loop 14, exit 28],              3, symbols![nt 14, t 2, t 3]),  // 28: vp_3 -> vp_1         | ●vp_1 ◄28         | 3 | vp_1 C D
            (strip![exit 29],                       3, symbols![nt 14, t 2, t 3]),  // 29: vp_3 -> ε            | ◄29               | 3 | vp_1 C D
            (strip![loop 15, exit 30],              2, symbols![]),                 // 30: np_2 -> np_1         | ●np_1 ◄30         | 2 |
            (strip![exit 31],                       2, symbols![]),                 // 31: np_2 -> ε            | ◄31               | 2 |
            (strip![loop 15, exit 32],              3, symbols![]),                 // 32: np_3 -> np_1         | ●np_1 ◄32         | 3 |
            (strip![exit 33],                       3, symbols![]),                 // 33: np_3 -> ε            | ◄33               | 3 |
            (strip![loop 16, exit 34],              2, symbols![]),                 // 34: xp_2 -> xp_1         | ●xp_1 ◄34         | 2 |
            (strip![exit 35],                       2, symbols![]),                 // 35: xp_2 -> ε            | ◄35               | 2 |
            (strip![loop 16, exit 36],              3, symbols![]),                 // 36: xp_3 -> xp_1         | ●xp_1 ◄36         | 3 |
            (strip![exit 37],                       3, symbols![]),                 // 37: xp_3 -> ε            | ◄37               | 3 |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 6, 7, 8]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5], 6 => vec![6], 7 => vec![7], 8 => vec![8], 9 => vec![9], 10 => vec![10]]),

        // a -> s p
        // s -> vs ns xs
        // p -> vp np xp
        // vs -> A (<L=ivs> B)*
        // ns -> "A" (<L=ins> "B")*
        // xs -> A (<L=ixs> x)*
        // vp -> A (<L=ivp> B)+
        // np -> "A" (<L=inp> "B")+
        // xp -> A (<L=ixp> x)+
        // x -> "X"
        //
        //   NT    name       val   flags
        // +---------------------------------------------------------------------+
        // |   0 | a         | y  |                                              |
        // |   1 | s         | y  |                                              |
        // |   2 | p         | y  |                                              |
        // |   3 | vs        | y  | parent_+_or_*                                |
        // |   4 | . ivs     | y  | child_+_or_*, L-form                         |
        // |   5 | ns        | y  | parent_+_or_*                                |
        // |   6 | . ins     |    | child_+_or_*, L-form                         |
        // |   7 | xs        | y  | parent_+_or_*                                |
        // |   8 | . ixs     |    | child_+_or_*, L-form                         |
        // |   9 | vp        | y  | parent_+_or_*                                |
        // |  10 | . ivp     | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |  16 | .   ivp_1 |    | child_left_fact                              |
        // |  11 | np        | y  | parent_+_or_*                                |
        // |  12 | . inp     |    | child_+_or_*, parent_left_fact, L-form, plus |
        // |  17 | .   inp_1 |    | child_left_fact                              |
        // |  13 | xp        | y  | parent_+_or_*                                |
        // |  14 | . ixp     |    | child_+_or_*, parent_left_fact, L-form, plus |
        // |  18 | .   ixp_1 |    | child_left_fact                              |
        // |  15 | x         |    |                                              |
        // +---------------------------------------------------------------------+
        (982, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),         //  0: a -> s p         | ◄0 ►p ►s       | 2 | s p
            (strip![exit 1, nt 7, nt 5, nt 3],      3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs    | ◄1 ►xs ►ns ►vs | 3 | vs ns xs
            (strip![exit 2, nt 13, nt 11, nt 9],    3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp    | ◄2 ►xp ►np ►vp | 3 | vp np xp
            (strip![exit 3, nt 4, t 0],             2, symbols![t 0, nt 4]),          //  3: vs -> A ivs      | ◄3 ►ivs A!     | 2 | A ivs
            (strip![loop 4, exit 4, t 1],           2, symbols![nt 4, t 1]),          //  4: ivs -> B ivs     | ●ivs ◄4 B!     | 2 | ivs B
            (strip![exit 5],                        1, symbols![nt 4]),               //  5: ivs -> ε         | ◄5             | 1 | ivs
            (strip![exit 6, nt 6, t 2],             2, symbols![]),                   //  6: ns -> "A" ins    | ◄6 ►ins "A"    | 2 |
            (strip![loop 6, exit 7, t 3],           2, symbols![]),                   //  7: ins -> "B" ins   | ●ins ◄7 "B"    | 2 |
            (strip![exit 8],                        1, symbols![]),                   //  8: ins -> ε         | ◄8             | 1 |
            (strip![exit 9, nt 8, t 0],             2, symbols![t 0]),                //  9: xs -> A ixs      | ◄9 ►ixs A!     | 2 | A
            (strip![loop 8, exit 10, nt 15],        2, symbols![]),                   // 10: ixs -> x ixs     | ●ixs ◄10 ►x    | 2 |
            (strip![exit 11],                       1, symbols![]),                   // 11: ixs -> ε         | ◄11            | 1 |
            (strip![exit 12, nt 10, t 0],           2, symbols![t 0, nt 10]),         // 12: vp -> A ivp      | ◄12 ►ivp A!    | 2 | A ivp
            (strip![nt 16, t 1],                    0, symbols![]),                   // 13: ivp -> B ivp_1   | ►ivp_1 B!      | 0 |
            (strip![exit 14, nt 12, t 2],           2, symbols![]),                   // 14: np -> "A" inp    | ◄14 ►inp "A"   | 2 |
            (strip![nt 17, t 3],                    0, symbols![]),                   // 15: inp -> "B" inp_1 | ►inp_1 "B"     | 0 |
            (strip![exit 16, nt 14, t 0],           2, symbols![t 0]),                // 16: xp -> A ixp      | ◄16 ►ixp A!    | 2 | A
            (strip![nt 18, nt 15],                  0, symbols![]),                   // 17: ixp -> x ixp_1   | ►ixp_1 ►x      | 0 |
            (strip![exit 18, t 4],                  1, symbols![]),                   // 18: x -> "X"         | ◄18 "X"        | 1 |
            (strip![loop 10, exit 19],              2, symbols![nt 10, t 1]),         // 19: ivp_1 -> ivp     | ●ivp ◄19       | 2 | ivp B
            (strip![exit 20],                       2, symbols![nt 10, t 1]),         // 20: ivp_1 -> ε       | ◄20            | 2 | ivp B
            (strip![loop 12, exit 21],              2, symbols![]),                   // 21: inp_1 -> inp     | ●inp ◄21       | 2 |
            (strip![exit 22],                       2, symbols![]),                   // 22: inp_1 -> ε       | ◄22            | 2 |
            (strip![loop 14, exit 23],              2, symbols![]),                   // 23: ixp_1 -> ixp     | ●ixp ◄23       | 2 |
            (strip![exit 24],                       2, symbols![]),                   // 24: ixp_1 -> ε       | ◄24            | 2 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![6], 7 => vec![9], 9 => vec![12], 11 => vec![14], 13 => vec![16], 15 => vec![18]]),
        // a: y, s: y, p: y, vs: y, ivs: y, ns: y, ins: n, xs: y, ixs: n, vp: y, ivp: y, np: y, inp: n, xp: y, ixp: n, x: n, ivp_1: n, inp_1: n, ixp_1: n
        (982, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),         //  0: a -> s p         | ◄0 ►p ►s       | 2 | s p
            (strip![exit 1, nt 7, nt 5, nt 3],      3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs    | ◄1 ►xs ►ns ►vs | 3 | vs ns xs
            (strip![exit 2, nt 13, nt 11, nt 9],    3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp    | ◄2 ►xp ►np ►vp | 3 | vp np xp
            (strip![exit 3, nt 4, t 0],             2, symbols![t 0, nt 4]),          //  3: vs -> A ivs      | ◄3 ►ivs A!     | 2 | A ivs
            (strip![loop 4, exit 4, t 1],           2, symbols![nt 4, t 1]),          //  4: ivs -> B ivs     | ●ivs ◄4 B!     | 2 | ivs B
            (strip![exit 5],                        1, symbols![nt 4]),               //  5: ivs -> ε         | ◄5             | 1 | ivs
            (strip![exit 6, nt 6, t 2],             2, symbols![]),                   //  6: ns -> "A" ins    | ◄6 ►ins "A"    | 2 |
            (strip![loop 6, exit 7, t 3],           2, symbols![]),                   //  7: ins -> "B" ins   | ●ins ◄7 "B"    | 2 |
            (strip![exit 8],                        1, symbols![]),                   //  8: ins -> ε         | ◄8             | 1 |
            (strip![exit 9, nt 8, t 0],             2, symbols![t 0]),                //  9: xs -> A ixs      | ◄9 ►ixs A!     | 2 | A
            (strip![loop 8, exit 10, nt 15],        2, symbols![]),                   // 10: ixs -> x ixs     | ●ixs ◄10 ►x    | 2 |
            (strip![exit 11],                       1, symbols![]),                   // 11: ixs -> ε         | ◄11            | 1 |
            (strip![exit 12, nt 10, t 0],           2, symbols![t 0, nt 10]),         // 12: vp -> A ivp      | ◄12 ►ivp A!    | 2 | A ivp
            (strip![nt 16, t 1],                    0, symbols![]),                   // 13: ivp -> B ivp_1   | ►ivp_1 B!      | 0 |
            (strip![exit 14, nt 12, t 2],           2, symbols![]),                   // 14: np -> "A" inp    | ◄14 ►inp "A"   | 2 |
            (strip![nt 17, t 3],                    0, symbols![]),                   // 15: inp -> "B" inp_1 | ►inp_1 "B"     | 0 |
            (strip![exit 16, nt 14, t 0],           2, symbols![t 0]),                // 16: xp -> A ixp      | ◄16 ►ixp A!    | 2 | A
            (strip![nt 18, nt 15],                  0, symbols![]),                   // 17: ixp -> x ixp_1   | ►ixp_1 ►x      | 0 |
            (strip![exit 18, t 4],                  1, symbols![]),                   // 18: x -> "X"         | ◄18 "X"        | 1 |
            (strip![loop 10, exit 19],              2, symbols![nt 10, t 1]),         // 19: ivp_1 -> ivp     | ●ivp ◄19       | 2 | ivp B
            (strip![exit 20],                       2, symbols![nt 10, t 1]),         // 20: ivp_1 -> ε       | ◄20            | 2 | ivp B
            (strip![loop 12, exit 21],              2, symbols![]),                   // 21: inp_1 -> inp     | ●inp ◄21       | 2 |
            (strip![exit 22],                       2, symbols![]),                   // 22: inp_1 -> ε       | ◄22            | 2 |
            (strip![loop 14, exit 23],              2, symbols![]),                   // 23: ixp_1 -> ixp     | ●ixp ◄23       | 2 |
            (strip![exit 24],                       2, symbols![]),                   // 24: ixp_1 -> ε       | ◄24            | 2 |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![6], 7 => vec![9], 9 => vec![12], 11 => vec![14], 13 => vec![16], 15 => vec![18]]),

        // a -> s p
        // s -> vs ns xs
        // p -> vp np xp
        // vs -> A (<L=ivs> B | C D)*
        // ns -> "A" (<L=ins> "B" | "C" "D")*
        // xs -> A (<L=ixs> x | y x)*
        // vp -> A (<L=ivp> B | C D)+
        // np -> "A" (<L=inp> "B" | "C" "D")+
        // xp -> A (<L=ixp> x | y x)+
        // x -> "X"
        // y -> "Y"
        //
        //   NT    name       val   flags
        // +---------------------------------------------------------------------+
        // |   0 | a         | y  |                                              |
        // |   1 | s         | y  |                                              |
        // |   2 | p         | y  |                                              |
        // |   3 | vs        | y  | parent_+_or_*                                |
        // |   4 | . ivs     | y  | child_+_or_*, L-form                         |
        // |   5 | ns        | y  | parent_+_or_*                                |
        // |   6 | . ins     |    | child_+_or_*, L-form                         |
        // |   7 | xs        | y  | parent_+_or_*                                |
        // |   8 | . ixs     |    | child_+_or_*, L-form                         |
        // |   9 | vp        | y  | parent_+_or_*                                |
        // |  10 | . ivp     | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |  17 | .   ivp_1 |    | child_left_fact                              |
        // |  18 | .   ivp_2 |    | child_left_fact                              |
        // |  11 | np        | y  | parent_+_or_*                                |
        // |  12 | . inp     |    | child_+_or_*, parent_left_fact, L-form, plus |
        // |  19 | .   inp_1 |    | child_left_fact                              |
        // |  20 | .   inp_2 |    | child_left_fact                              |
        // |  13 | xp        | y  | parent_+_or_*                                |
        // |  14 | . ixp     |    | child_+_or_*, parent_left_fact, L-form, plus |
        // |  21 | .   ixp_1 |    | child_left_fact                              |
        // |  22 | .   ixp_2 |    | child_left_fact                              |
        // |  15 | x         |    |                                              |
        // |  16 | y         |    |                                              |
        // +---------------------------------------------------------------------+
        (983, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),         //  0: a -> s p             | ◄0 ►p ►s        | 2 | s p
            (strip![exit 1, nt 7, nt 5, nt 3],      3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs        | ◄1 ►xs ►ns ►vs  | 3 | vs ns xs
            (strip![exit 2, nt 13, nt 11, nt 9],    3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp        | ◄2 ►xp ►np ►vp  | 3 | vp np xp
            (strip![exit 3, nt 4, t 0],             2, symbols![t 0, nt 4]),          //  3: vs -> A ivs          | ◄3 ►ivs A!      | 2 | A ivs
            (strip![loop 4, exit 4, t 1],           2, symbols![nt 4, t 1]),          //  4: ivs -> B ivs         | ●ivs ◄4 B!      | 2 | ivs B
            (strip![loop 4, exit 5, t 3, t 2],      3, symbols![nt 4, t 2, t 3]),     //  5: ivs -> C D ivs       | ●ivs ◄5 D! C!   | 3 | ivs C D
            (strip![exit 6],                        1, symbols![nt 4]),               //  6: ivs -> ε             | ◄6              | 1 | ivs
            (strip![exit 7, nt 6, t 4],             2, symbols![]),                   //  7: ns -> "A" ins        | ◄7 ►ins "A"     | 2 |
            (strip![loop 6, exit 8, t 5],           2, symbols![]),                   //  8: ins -> "B" ins       | ●ins ◄8 "B"     | 2 |
            (strip![loop 6, exit 9, t 7, t 6],      3, symbols![]),                   //  9: ins -> "C" "D" ins   | ●ins ◄9 "D" "C" | 3 |
            (strip![exit 10],                       1, symbols![]),                   // 10: ins -> ε             | ◄10             | 1 |
            (strip![exit 11, nt 8, t 0],            2, symbols![t 0]),                // 11: xs -> A ixs          | ◄11 ►ixs A!     | 2 | A
            (strip![loop 8, exit 12, nt 15],        2, symbols![]),                   // 12: ixs -> x ixs         | ●ixs ◄12 ►x     | 2 |
            (strip![loop 8, exit 13, nt 15, nt 16], 3, symbols![]),                   // 13: ixs -> y x ixs       | ●ixs ◄13 ►x ►y  | 3 |
            (strip![exit 14],                       1, symbols![]),                   // 14: ixs -> ε             | ◄14             | 1 |
            (strip![exit 15, nt 10, t 0],           2, symbols![t 0, nt 10]),         // 15: vp -> A ivp          | ◄15 ►ivp A!     | 2 | A ivp
            (strip![nt 17, t 1],                    0, symbols![]),                   // 16: ivp -> B ivp_1       | ►ivp_1 B!       | 0 |
            (strip![nt 18, t 3, t 2],               0, symbols![]),                   // 17: ivp -> C D ivp_2     | ►ivp_2 D! C!    | 0 |
            (strip![exit 18, nt 12, t 4],           2, symbols![]),                   // 18: np -> "A" inp        | ◄18 ►inp "A"    | 2 |
            (strip![nt 19, t 5],                    0, symbols![]),                   // 19: inp -> "B" inp_1     | ►inp_1 "B"      | 0 |
            (strip![nt 20, t 7, t 6],               0, symbols![]),                   // 20: inp -> "C" "D" inp_2 | ►inp_2 "D" "C"  | 0 |
            (strip![exit 21, nt 14, t 0],           2, symbols![t 0]),                // 21: xp -> A ixp          | ◄21 ►ixp A!     | 2 | A
            (strip![nt 21, nt 15],                  0, symbols![]),                   // 22: ixp -> x ixp_1       | ►ixp_1 ►x       | 0 |
            (strip![nt 22, nt 15, nt 16],           0, symbols![]),                   // 23: ixp -> y x ixp_2     | ►ixp_2 ►x ►y    | 0 |
            (strip![exit 24, t 8],                  1, symbols![]),                   // 24: x -> "X"             | ◄24 "X"         | 1 |
            (strip![exit 25, t 9],                  1, symbols![]),                   // 25: y -> "Y"             | ◄25 "Y"         | 1 |
            (strip![loop 10, exit 26],              2, symbols![nt 10, t 1]),         // 26: ivp_1 -> ivp         | ●ivp ◄26        | 2 | ivp B
            (strip![exit 27],                       2, symbols![nt 10, t 1]),         // 27: ivp_1 -> ε           | ◄27             | 2 | ivp B
            (strip![loop 10, exit 28],              3, symbols![nt 10, t 2, t 3]),    // 28: ivp_2 -> ivp         | ●ivp ◄28        | 3 | ivp C D
            (strip![exit 29],                       3, symbols![nt 10, t 2, t 3]),    // 29: ivp_2 -> ε           | ◄29             | 3 | ivp C D
            (strip![loop 12, exit 30],              2, symbols![]),                   // 30: inp_1 -> inp         | ●inp ◄30        | 2 |
            (strip![exit 31],                       2, symbols![]),                   // 31: inp_1 -> ε           | ◄31             | 2 |
            (strip![loop 12, exit 32],              3, symbols![]),                   // 32: inp_2 -> inp         | ●inp ◄32        | 3 |
            (strip![exit 33],                       3, symbols![]),                   // 33: inp_2 -> ε           | ◄33             | 3 |
            (strip![loop 14, exit 34],              2, symbols![]),                   // 34: ixp_1 -> ixp         | ●ixp ◄34        | 2 |
            (strip![exit 35],                       2, symbols![]),                   // 35: ixp_1 -> ε           | ◄35             | 2 |
            (strip![loop 14, exit 36],              3, symbols![]),                   // 36: ixp_2 -> ixp         | ●ixp ◄36        | 3 |
            (strip![exit 37],                       3, symbols![]),                   // 37: ixp_2 -> ε           | ◄37             | 3 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![7], 7 => vec![11], 9 => vec![15], 11 => vec![18], 13 => vec![21], 15 => vec![24], 16 => vec![25]]),
        // a: y, s: y, p: y, vs: y, ivs: y, ns: y, ins: n, xs: y, ixs: n, vp: y, ivp: y, np: y, inp: n, xp: y, ixp: n, x: n, y: n, ivp_1: n, ivp_2: n, inp_1: n, inp_2: n, ixp_1: n, ixp_2: n
        (983, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 2, nt 1],            2, symbols![nt 1, nt 2]),         //  0: a -> s p             | ◄0 ►p ►s        | 2 | s p
            (strip![exit 1, nt 7, nt 5, nt 3],      3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs        | ◄1 ►xs ►ns ►vs  | 3 | vs ns xs
            (strip![exit 2, nt 13, nt 11, nt 9],    3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp        | ◄2 ►xp ►np ►vp  | 3 | vp np xp
            (strip![exit 3, nt 4, t 0],             2, symbols![t 0, nt 4]),          //  3: vs -> A ivs          | ◄3 ►ivs A!      | 2 | A ivs
            (strip![loop 4, exit 4, t 1],           2, symbols![nt 4, t 1]),          //  4: ivs -> B ivs         | ●ivs ◄4 B!      | 2 | ivs B
            (strip![loop 4, exit 5, t 3, t 2],      3, symbols![nt 4, t 2, t 3]),     //  5: ivs -> C D ivs       | ●ivs ◄5 D! C!   | 3 | ivs C D
            (strip![exit 6],                        1, symbols![nt 4]),               //  6: ivs -> ε             | ◄6              | 1 | ivs
            (strip![exit 7, nt 6, t 4],             2, symbols![]),                   //  7: ns -> "A" ins        | ◄7 ►ins "A"     | 2 |
            (strip![loop 6, exit 8, t 5],           2, symbols![]),                   //  8: ins -> "B" ins       | ●ins ◄8 "B"     | 2 |
            (strip![loop 6, exit 9, t 7, t 6],      3, symbols![]),                   //  9: ins -> "C" "D" ins   | ●ins ◄9 "D" "C" | 3 |
            (strip![exit 10],                       1, symbols![]),                   // 10: ins -> ε             | ◄10             | 1 |
            (strip![exit 11, nt 8, t 0],            2, symbols![t 0]),                // 11: xs -> A ixs          | ◄11 ►ixs A!     | 2 | A
            (strip![loop 8, exit 12, nt 15],        2, symbols![]),                   // 12: ixs -> x ixs         | ●ixs ◄12 ►x     | 2 |
            (strip![loop 8, exit 13, nt 15, nt 16], 3, symbols![]),                   // 13: ixs -> y x ixs       | ●ixs ◄13 ►x ►y  | 3 |
            (strip![exit 14],                       1, symbols![]),                   // 14: ixs -> ε             | ◄14             | 1 |
            (strip![exit 15, nt 10, t 0],           2, symbols![t 0, nt 10]),         // 15: vp -> A ivp          | ◄15 ►ivp A!     | 2 | A ivp
            (strip![nt 17, t 1],                    0, symbols![]),                   // 16: ivp -> B ivp_1       | ►ivp_1 B!       | 0 |
            (strip![nt 18, t 3, t 2],               0, symbols![]),                   // 17: ivp -> C D ivp_2     | ►ivp_2 D! C!    | 0 |
            (strip![exit 18, nt 12, t 4],           2, symbols![]),                   // 18: np -> "A" inp        | ◄18 ►inp "A"    | 2 |
            (strip![nt 19, t 5],                    0, symbols![]),                   // 19: inp -> "B" inp_1     | ►inp_1 "B"      | 0 |
            (strip![nt 20, t 7, t 6],               0, symbols![]),                   // 20: inp -> "C" "D" inp_2 | ►inp_2 "D" "C"  | 0 |
            (strip![exit 21, nt 14, t 0],           2, symbols![t 0]),                // 21: xp -> A ixp          | ◄21 ►ixp A!     | 2 | A
            (strip![nt 21, nt 15],                  0, symbols![]),                   // 22: ixp -> x ixp_1       | ►ixp_1 ►x       | 0 |
            (strip![nt 22, nt 15, nt 16],           0, symbols![]),                   // 23: ixp -> y x ixp_2     | ►ixp_2 ►x ►y    | 0 |
            (strip![exit 24, t 8],                  1, symbols![]),                   // 24: x -> "X"             | ◄24 "X"         | 1 |
            (strip![exit 25, t 9],                  1, symbols![]),                   // 25: y -> "Y"             | ◄25 "Y"         | 1 |
            (strip![loop 10, exit 26],              2, symbols![nt 10, t 1]),         // 26: ivp_1 -> ivp         | ●ivp ◄26        | 2 | ivp B
            (strip![exit 27],                       2, symbols![nt 10, t 1]),         // 27: ivp_1 -> ε           | ◄27             | 2 | ivp B
            (strip![loop 10, exit 28],              3, symbols![nt 10, t 2, t 3]),    // 28: ivp_2 -> ivp         | ●ivp ◄28        | 3 | ivp C D
            (strip![exit 29],                       3, symbols![nt 10, t 2, t 3]),    // 29: ivp_2 -> ε           | ◄29             | 3 | ivp C D
            (strip![loop 12, exit 30],              2, symbols![]),                   // 30: inp_1 -> inp         | ●inp ◄30        | 2 |
            (strip![exit 31],                       2, symbols![]),                   // 31: inp_1 -> ε           | ◄31             | 2 |
            (strip![loop 12, exit 32],              3, symbols![]),                   // 32: inp_2 -> inp         | ●inp ◄32        | 3 |
            (strip![exit 33],                       3, symbols![]),                   // 33: inp_2 -> ε           | ◄33             | 3 |
            (strip![loop 14, exit 34],              2, symbols![]),                   // 34: ixp_1 -> ixp         | ●ixp ◄34        | 2 |
            (strip![exit 35],                       2, symbols![]),                   // 35: ixp_1 -> ε           | ◄35             | 2 |
            (strip![loop 14, exit 36],              3, symbols![]),                   // 36: ixp_2 -> ixp         | ●ixp ◄36        | 3 |
            (strip![exit 37],                       3, symbols![]),                   // 37: ixp_2 -> ε           | ◄37             | 3 |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![7], 7 => vec![11], 9 => vec![15], 11 => vec![18], 13 => vec![21], 15 => vec![24], 16 => vec![25]]),

        // a -> vp np xp
        // vp -> A (B C / ",")+
        // np -> "A" ("B" "C" / ",")+
        // xp -> A (x y / ",")+
        // x -> "X"
        // y -> "Y"
        //
        //   NT    name    val   flags
        // +--------------------------------------------+
        // |   0 | a      | y  |                        |
        // |   1 | vp     | y  | parent_+_or_*          |
        // |   6 | . vp_1 | y  | child_+_or_*, sep_list |
        // |   2 | np     | y  | parent_+_or_*          |
        // |   7 | . np_1 |    | child_+_or_*, sep_list |
        // |   3 | xp     | y  | parent_+_or_*          |
        // |   8 | . xp_1 |    | child_+_or_*, sep_list |
        // |   4 | x      |    |                        |
        // |   5 | y      |    |                        |
        // +--------------------------------------------+
        (984, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 3, nt 2, nt 1],         3, symbols![nt 1, nt 2, nt 3]), //  0: a -> vp np xp            | ◄0 ►xp ►np ►vp       | 3    | vp np xp
            (strip![exit 1, nt 6, t 2, t 1, t 0],      2, symbols![t 0, nt 6]),        //  1: vp -> A B C vp_1         | ◄1 ►vp_1 C! B! A!    | 2    | A vp_1
            (strip![exit 2, nt 7, t 6, t 5, t 4],      2, symbols![]),                 //  2: np -> "A" "B" "C" np_1   | ◄2 ►np_1 "C" "B" "A" | 2    |
            (strip![exit 3, nt 8, nt 5, nt 4, t 0],    2, symbols![t 0]),              //  3: xp -> A x y xp_1         | ◄3 ►xp_1 ►y ►x A!    | 2    | A
            (strip![exit 4, t 7],                      1, symbols![]),                 //  4: x -> "X"                 | ◄4 "X"               | 1    |
            (strip![exit 5, t 8],                      1, symbols![]),                 //  5: y -> "Y"                 | ◄5 "Y"               | 1    |
            (strip![loop 6, exit 6, t 2, t 1, t 3],    4, symbols![nt 6, t 1, t 2]),   //  6: vp_1 -> "," B C vp_1     | ●vp_1 ◄6 C! B! ","   | 4, 2 | vp_1 B C
            (strip![exit 7],                           1, symbols![nt 6]),             //  7: vp_1 -> ε                | ◄7                   | 1    | vp_1
            (strip![loop 7, exit 8, t 6, t 5, t 3],    4, symbols![]),                 //  8: np_1 -> "," "B" "C" np_1 | ●np_1 ◄8 "C" "B" "," | 4, 2 |
            (strip![exit 9],                           1, symbols![]),                 //  9: np_1 -> ε                | ◄9                   | 1    |
            (strip![loop 8, exit 10, nt 5, nt 4, t 3], 4, symbols![]),                 // 10: xp_1 -> "," x y xp_1     | ●xp_1 ◄10 ►y ►x ","  | 4, 2 |
            (strip![exit 11],                          1, symbols![]),                 // 11: xp_1 -> ε                | ◄11                  | 1    |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5]]),
        // a: y, vp: y, np: y, xp: y, x: n, y: n, vp_1: y, np_1: n, xp_1: n
        (984, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 3, nt 2, nt 1],         3, symbols![nt 1, nt 2, nt 3]), //  0: a -> vp np xp            | ◄0 ►xp ►np ►vp       | 3    | vp np xp
            (strip![exit 1, nt 6, t 2, t 1, t 0],      2, symbols![t 0, nt 6]),        //  1: vp -> A B C vp_1         | ◄1 ►vp_1 C! B! A!    | 2    | A vp_1
            (strip![exit 2, nt 7, t 6, t 5, t 4],      2, symbols![]),                 //  2: np -> "A" "B" "C" np_1   | ◄2 ►np_1 "C" "B" "A" | 2    |
            (strip![exit 3, nt 8, nt 5, nt 4, t 0],    2, symbols![t 0]),              //  3: xp -> A x y xp_1         | ◄3 ►xp_1 ►y ►x A!    | 2    | A
            (strip![exit 4, t 7],                      1, symbols![]),                 //  4: x -> "X"                 | ◄4 "X"               | 1    |
            (strip![exit 5, t 8],                      1, symbols![]),                 //  5: y -> "Y"                 | ◄5 "Y"               | 1    |
            (strip![loop 6, exit 6, t 2, t 1, t 3],    4, symbols![nt 6, t 1, t 2]),   //  6: vp_1 -> "," B C vp_1     | ●vp_1 ◄6 C! B! ","   | 4, 2 | vp_1 B C
            (strip![exit 7],                           1, symbols![nt 6]),             //  7: vp_1 -> ε                | ◄7                   | 1    | vp_1
            (strip![loop 7, exit 8, t 6, t 5, t 3],    4, symbols![]),                 //  8: np_1 -> "," "B" "C" np_1 | ●np_1 ◄8 "C" "B" "," | 4, 2 |
            (strip![exit 9],                           1, symbols![]),                 //  9: np_1 -> ε                | ◄9                   | 1    |
            (strip![loop 8, exit 10, nt 5, nt 4, t 3], 4, symbols![]),                 // 10: xp_1 -> "," x y xp_1     | ●xp_1 ◄10 ►y ►x ","  | 4, 2 |
            (strip![exit 11],                          1, symbols![]),                 // 11: xp_1 -> ε                | ◄11                  | 1    |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5]]),

        // a -> vp np xp
        // vp -> A (<L=ivp> B C / ",")+
        // np -> "A" (<L=inp> "B" "C" / ",")+
        // xp -> A (<L=ixp> x y / ",")+
        // x -> "X"
        // y -> "Y"
        //
        //   NT    name   val   flags
        // +---------------------------------------------------+
        // |   0 | a     | y  |                                |
        // |   1 | vp    | y  | parent_+_or_*                  |
        // |   2 | . ivp | y  | child_+_or_*, L-form, sep_list |
        // |   3 | np    | y  | parent_+_or_*                  |
        // |   4 | . inp |    | child_+_or_*, L-form, sep_list |
        // |   5 | xp    |    | parent_+_or_*                  |
        // |   6 | . ixp |    | child_+_or_*, L-form, sep_list |
        // |   7 | x     |    |                                |
        // |   8 | y     |    |                                |
        // +---------------------------------------------------+
        (985, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 5, nt 3, nt 1],        3, symbols![nt 1, nt 3]),     //  0: a -> vp np xp          | ◄0 ►xp ►np ►vp      | 3    | vp np
            (strip![exit 1, nt 2, t 2, t 1, t 0],     2, symbols![t 0, nt 2]),      //  1: vp -> A B C ivp        | ◄1 ►ivp C! B! A!    | 2    | A ivp
            (strip![loop 2, exit 2, t 2, t 1, t 3],   4, symbols![nt 2, t 1, t 2]), //  2: ivp -> "," B C ivp     | ●ivp ◄2 C! B! ","   | 4, 2 | ivp B C
            (strip![exit 3],                          1, symbols![nt 2]),           //  3: ivp -> ε               | ◄3                  | 1    | ivp
            (strip![exit 4, nt 4, t 6, t 5, t 4],     2, symbols![]),               //  4: np -> "A" "B" "C" inp  | ◄4 ►inp "C" "B" "A" | 2    |
            (strip![loop 4, exit 5, t 6, t 5, t 3],   4, symbols![]),               //  5: inp -> "," "B" "C" inp | ●inp ◄5 "C" "B" "," | 4, 2 |
            (strip![exit 6],                          1, symbols![]),               //  6: inp -> ε               | ◄6                  | 1    |
            (strip![exit 7, nt 6, nt 8, nt 7, t 0],   2, symbols![t 0]),            //  7: xp -> A x y ixp        | ◄7 ►ixp ►y ►x A!    | 2    | A
            (strip![loop 6, exit 8, nt 8, nt 7, t 3], 4, symbols![]),               //  8: ixp -> "," x y ixp     | ●ixp ◄8 ►y ►x ","   | 4, 2 |
            (strip![exit 9],                          1, symbols![]),               //  9: ixp -> ε               | ◄9                  | 1    |
            (strip![exit 10, t 7],                    1, symbols![]),               // 10: x -> "X"               | ◄10 "X"             | 1    |
            (strip![exit 11, t 8],                    1, symbols![]),               // 11: y -> "Y"               | ◄11 "Y"             | 1    |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3]), btreemap![0 => vec![0], 1 => vec![1], 3 => vec![4], 5 => vec![7], 7 => vec![10], 8 => vec![11]]),
        // a: y, vp: y, ivp: y, np: y, inp: n, xp: n, ixp: n, x: n, y: n
        (985, true, false, false, 0, btreemap![
        ], vec![
            (strip![exit 0, nt 5, nt 3, nt 1],        3, symbols![nt 1, nt 3]),     //  0: a -> vp np xp          | ◄0 ►xp ►np ►vp      | 3    | vp np
            (strip![exit 1, nt 2, t 2, t 1, t 0],     2, symbols![t 0, nt 2]),      //  1: vp -> A B C ivp        | ◄1 ►ivp C! B! A!    | 2    | A ivp
            (strip![loop 2, exit 2, t 2, t 1, t 3],   4, symbols![nt 2, t 1, t 2]), //  2: ivp -> "," B C ivp     | ●ivp ◄2 C! B! ","   | 4, 2 | ivp B C
            (strip![exit 3],                          1, symbols![nt 2]),           //  3: ivp -> ε               | ◄3                  | 1    | ivp
            (strip![exit 4, nt 4, t 6, t 5, t 4],     2, symbols![]),               //  4: np -> "A" "B" "C" inp  | ◄4 ►inp "C" "B" "A" | 2    |
            (strip![loop 4, exit 5, t 6, t 5, t 3],   4, symbols![]),               //  5: inp -> "," "B" "C" inp | ●inp ◄5 "C" "B" "," | 4, 2 |
            (strip![exit 6],                          1, symbols![]),               //  6: inp -> ε               | ◄6                  | 1    |
            (strip![exit 7, nt 6, nt 8, nt 7, t 0],   2, symbols![t 0]),            //  7: xp -> A x y ixp        | ◄7 ►ixp ►y ►x A!    | 2    | A
            (strip![loop 6, exit 8, nt 8, nt 7, t 3], 4, symbols![]),               //  8: ixp -> "," x y ixp     | ●ixp ◄8 ►y ►x ","   | 4, 2 |
            (strip![exit 9],                          1, symbols![]),               //  9: ixp -> ε               | ◄9                  | 1    |
            (strip![exit 10, t 7],                    1, symbols![]),               // 10: x -> "X"               | ◄10 "X"             | 1    |
            (strip![exit 11, t 8],                    1, symbols![]),               // 11: y -> "Y"               | ◄11 "Y"             | 1    |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3]), btreemap![0 => vec![0], 1 => vec![1], 3 => vec![4], 5 => vec![7], 7 => vec![10], 8 => vec![11]]),

        // =========================================================================== mix
        /* template:
        (, false, false, false, 0, btreemap![], vec![], true, NTValue::Default, btreemap![]),
        */
    ]
}

#[test]
fn check_build_items() {
    let spec = BuildItemsTestSpec {
        enable_test_source: true,
        tests_all: true,
        replace_source: false,
        parser_type: ParserType::LL1,
        wrapper_filenames: WRAPPER_FILENAMES,
        tests: get_ll1_tests(),
    };
    build_items(spec);
}

#[ignore]
#[test]
fn write_build_items() {
    let spec = BuildItemsTestSpec {
        enable_test_source: true,
        tests_all: true,
        replace_source: true,
        parser_type: ParserType::LL1,
        wrapper_filenames: WRAPPER_FILENAMES,
        tests: get_ll1_tests(),
    };
    build_items(spec);
}

#[test]
fn build_errors() {
    static TESTS: &[(u32, &[&str])] = &[
        (109, &[]),
        (124, &[r#"separator used in `Id / "." b X` item in `a -> A ( ►► Id / "." b X ◄◄ )+ C` contains valuable items: b, X"#]),
    ];

    const VERBOSE: bool = false;
    for &(tr_id, expected_errors) in TESTS {
        let ll1_maybe = TestRules(tr_id).to_prs_ll1_with_start(0);
        if ll1_maybe.is_none() { continue }
        let ll1 = ll1_maybe.unwrap();
        let mut builder = ParserGen::build_from_rules_ll1(ll1, "Test".to_string());
        builder.set_gen_span_params(true);
        builder.set_nt_value(NTValue::Default);
        builder.set_gen_parser(true);
        let _src = builder.gen_source_code();
        if VERBOSE { println!("Log:{}", builder.get_log()); }
        let errors = builder.get_errors().map(|e| e.get_inner_str()).to_vec();
        assert_eq!(errors, expected_errors, "test {tr_id} failed");
    }
}