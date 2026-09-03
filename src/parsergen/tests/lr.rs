// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexigram_core::strip;
use crate::{btreemap, symbols};
use crate::parsergen::{NTValue, ParserType};
use crate::parsergen::tests::wrapper_source::{build_items, BuildItemsTestEntry, BuildItemsTestSpec};

// List in decreasing order of test parser number of the file were they're generated:
static WRAPPER_FILENAMES: &[(u32, &str)] = &[
    (600, "tests/out_lr/wrapper_source1.rs"),   //       n >= 600
    (  0, "tests/out_lr/wrapper_source.rs"),    // 600 > n >= 0
];

fn get_lr_tests() -> Vec<BuildItemsTestEntry> {
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
        // ===========================================================================
        // a -> A B
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // +-------------------------+
        (1, false, false, false, 0, btreemap![
        ], vec![
            (strip![t 1, t 0],                      2, symbols![t 0, t 1]), //  0: a -> A B    | B! A! | 2 | A B
            (strip![nt 0],                          1, symbols![]),         //  1: <goal> -> a | ►a    | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // =========================================================================== NT/T simple mix
        // s -> Id "=" val | "exit" | "return" val
        // val -> Id | Num
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | s    | y  |       |
        // |   1 | val  | y  |       |
        // +-------------------------+
        (13, true, true, false, 0, btreemap![
            0 => "SynS".to_string(),
            1 => "SynVal".to_string(),
        ], vec![
            (strip![nt 1, t 1, t 0],                3, symbols![t 0, nt 1]), //  0: s -> Id "=" val   | ►val "=" Id!  | 3 | Id val
            (strip![t 2],                           1, symbols![]),          //  1: s -> "exit"       | "exit"        | 1 |
            (strip![nt 1, t 3],                     2, symbols![nt 1]),      //  2: s -> "return" val | ►val "return" | 2 | val
            (strip![t 0],                           1, symbols![t 0]),       //  3: val -> Id         | Id!           | 1 | Id
            (strip![t 4],                           1, symbols![t 4]),       //  4: val -> Num        | Num!          | 1 | Num
            (strip![nt 0],                          1, symbols![]),          //  5: <goal> -> s       | ►s            | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),

        // =========================================================================== +_or_*
        // a -> A B* C
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*  |
        // +----------------------------------+
        (102, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, nt 1, t 2]), //  0: a -> A a_1 C | C! ►a_1 A! | 3 | A a_1 C
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 B | B! ●a_1    | 2 | a_1 B
            (strip![],                              1, symbols![nt 1]),           //  2: a_1 -> ε     |            | 1 | a_1
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a  | ►a         | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A B+ C
        //
        //   NT    name   val   flags
        // +----------------------------------------+
        // |   0 | a     | y  | parent_+_or_*, plus |
        // |   1 | . a_1 | y  | child_+_or_*, plus  |
        // +----------------------------------------+
        (103, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, nt 1, t 2]), //  0: a -> A a_1 C | C! ►a_1 A! | 3 | A a_1 C
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 B | B! ●a_1    | 2 | a_1 B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: a_1 -> B     | B!         | 2 | a_1 B
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a  | ►a         | 1 |
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
        (109, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 2, t 1, t 0],           4, symbols![t 0, nt 2]),       //  0: a -> Id "(" a_1 ")"        | ")" ►a_1 "(" Id!       | 4    | Id a_1
            (strip![t 0],                           1, symbols![t 0]),             //  1: type -> Id                 | Id!                    | 1    | Id
            (strip![nt 1, t 2, t 0, t 3, loop 2],   5, symbols![nt 2, t 0, nt 1]), //  2: a_1 -> a_1 "," Id ":" type | ►type ":" Id! "," ●a_1 | 5, 3 | a_1 Id type
            (strip![nt 1, t 2, t 0],                4, symbols![nt 2, t 0, nt 1]), //  3: a_1 -> Id ":" type         | ►type ":" Id!          | 4    | a_1 Id type
            (strip![nt 0],                          1, symbols![]),                //  4: <goal> -> a                | ►a                     | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> Id "(" (Id / ",")+ "/" (Id / ",")+ ")"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // |   2 | . a_2 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (111, false, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 2, t 3, nt 1, t 1, t 0], 6, symbols![t 0, nt 1, nt 2]), //  0: a -> Id "(" a_1 "/" a_2 ")" | ")" ►a_2 "/" ►a_1 "(" Id! | 6    | Id a_1 a_2
            (strip![t 0, t 2, loop 1],               3, symbols![nt 1, t 0]),       //  1: a_1 -> a_1 "," Id           | Id! "," ●a_1              | 3, 1 | a_1 Id
            (strip![t 0],                            2, symbols![nt 1, t 0]),       //  2: a_1 -> Id                   | Id!                       | 2    | a_1 Id
            (strip![t 0, t 2, loop 2],               3, symbols![nt 2, t 0]),       //  3: a_2 -> a_2 "," Id           | Id! "," ●a_2              | 3, 1 | a_2 Id
            (strip![t 0],                            2, symbols![nt 2, t 0]),       //  4: a_2 -> Id                   | Id!                       | 2    | a_2 Id
            (strip![nt 0],                           1, symbols![]),                //  5: <goal> -> a                 | ►a                        | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> "let" (Id / ",")+ "=" (Num / ",")+ ";"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // |   2 | . a_2 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (112, false, false, false, 0, btreemap![
        ], vec![
            (strip![t 5, nt 2, t 3, nt 1, t 0],     5, symbols![nt 1, nt 2]), //  0: a -> "let" a_1 "=" a_2 ";" | ";" ►a_2 "=" ►a_1 "let" | 5    | a_1 a_2
            (strip![t 1, t 2, loop 1],              3, symbols![nt 1, t 1]),  //  1: a_1 -> a_1 "," Id          | Id! "," ●a_1            | 3, 1 | a_1 Id
            (strip![t 1],                           2, symbols![nt 1, t 1]),  //  2: a_1 -> Id                  | Id!                     | 2    | a_1 Id
            (strip![t 4, t 2, loop 2],              3, symbols![nt 2, t 4]),  //  3: a_2 -> a_2 "," Num         | Num! "," ●a_2           | 3, 1 | a_2 Num
            (strip![t 4],                           2, symbols![nt 2, t 4]),  //  4: a_2 -> Num                 | Num!                    | 2    | a_2 Num
            (strip![nt 0],                          1, symbols![]),           //  5: <goal> -> a                | ►a                      | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> X (B / ",")+ Z
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (119, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 3, nt 1, t 0],                3, symbols![t 0, nt 1, t 3]), //  0: a -> X a_1 Z     | Z! ►a_1 X!  | 3    | X a_1 Z
            (strip![t 1, t 2, loop 1],              3, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 "," B | B! "," ●a_1 | 3, 1 | a_1 B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: a_1 -> B         | B!          | 2    | a_1 B
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a      | ►a          | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (Id / ",")+ C
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (120, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 3, nt 1, t 0],                3, symbols![t 0, nt 1, t 3]), //  0: a -> A a_1 C      | C! ►a_1 A!   | 3    | A a_1 C
            (strip![t 1, t 2, loop 1],              3, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 "," Id | Id! "," ●a_1 | 3, 1 | a_1 Id
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: a_1 -> Id         | Id!          | 2    | a_1 Id
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a       | ►a           | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (B | C D)* E
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*  |
        // +----------------------------------+
        (150, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 1, t 0],                3, symbols![t 0, nt 1, t 4]), //  0: a -> A a_1 E   | E! ►a_1 A! | 3 | A a_1 E
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 B   | B! ●a_1    | 2 | a_1 B
            (strip![t 3, t 2, loop 1],              3, symbols![nt 1, t 2, t 3]), //  2: a_1 -> a_1 C D | D! C! ●a_1 | 3 | a_1 C D
            (strip![],                              1, symbols![nt 1]),           //  3: a_1 -> ε       |            | 1 | a_1
            (strip![nt 0],                          1, symbols![]),               //  4: <goal> -> a    | ►a         | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (B | C D)+ E
        //
        //   NT    name   val   flags
        // +---------------------------------------+
        // |   0 | a     | y  | parent_+_or_*      |
        // |   1 | . a_1 | y  | child_+_or_*, plus |
        // +---------------------------------------+
        (151, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 1, t 0],                3, symbols![t 0, nt 1, t 4]), //  0: a -> A a_1 E   | E! ►a_1 A! | 3 | A a_1 E
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 B   | B! ●a_1    | 2 | a_1 B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: a_1 -> B       | B!         | 2 | a_1 B
            (strip![t 3, t 2, loop 1],              3, symbols![nt 1, t 2, t 3]), //  3: a_1 -> a_1 C D | D! C! ●a_1 | 3 | a_1 C D
            (strip![t 3, t 2],                      3, symbols![nt 1, t 2, t 3]), //  4: a_1 -> C D     | D! C!      | 3 | a_1 C D
            (strip![nt 0],                          1, symbols![]),               //  5: <goal> -> a    | ►a         | 1 |
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
        (152, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 2, t 0],                    3, symbols![t 0, nt 2, t 4]),                  //  0: a -> A a_1 F         | F! ►a_1 A!          | 3 | A a_1 F
            (strip![t 5],                               1, symbols![t 5]),                             //  1: b -> D               | D!                  | 1 | D
            (strip![t 1, loop 2],                       2, symbols![nt 2, t 1]),                       //  2: a_1 -> a_1 B         | B! ●a_1             | 2 | a_1 B
            (strip![t 2, t 1, nt 1, t 2, nt 1, loop 2], 6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), //  3: a_1 -> a_1 b C b B C | C! B! ►b C! ►b ●a_1 | 6 | a_1 b C b B C
            (strip![t 3, loop 2],                       2, symbols![nt 2, t 3]),                       //  4: a_1 -> a_1 E         | E! ●a_1             | 2 | a_1 E
            (strip![],                                  1, symbols![nt 2]),                            //  5: a_1 -> ε             |                     | 1 | a_1
            (strip![nt 0],                              1, symbols![]),                                //  6: <goal> -> a          | ►a                  | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (A | A B | C)*
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*  |
        // +----------------------------------+
        (154, false, false, true, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),           //  0: a -> a_1       | ►a_1       | 1 | a_1
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]),      //  1: a_1 -> a_1 A   | A! ●a_1    | 2 | a_1 A
            (strip![t 1, t 0, loop 1],              3, symbols![nt 1, t 0, t 1]), //  2: a_1 -> a_1 A B | B! A! ●a_1 | 3 | a_1 A B
            (strip![t 2, loop 1],                   2, symbols![nt 1, t 2]),      //  3: a_1 -> a_1 C   | C! ●a_1    | 2 | a_1 C
            (strip![],                              1, symbols![nt 1]),           //  4: a_1 -> ε       |            | 1 | a_1
            (strip![nt 0],                          1, symbols![]),               //  5: <goal> -> a    | ►a         | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A | A B | C)+
        //
        //   NT    name   val   flags               
        // +----------------------------------------+
        // |   0 | a     | y  | parent_+_or_*, plus |
        // |   1 | . a_1 | y  | child_+_or_*, plus  |
        // +----------------------------------------+
        (155, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),           //  0: a -> a_1       | ►a_1       | 1 | a_1
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]),      //  1: a_1 -> a_1 A   | A! ●a_1    | 2 | a_1 A
            (strip![t 0],                           2, symbols![nt 1, t 0]),      //  2: a_1 -> A       | A!         | 2 | a_1 A
            (strip![t 1, t 0, loop 1],              3, symbols![nt 1, t 0, t 1]), //  3: a_1 -> a_1 A B | B! A! ●a_1 | 3 | a_1 A B
            (strip![t 1, t 0],                      3, symbols![nt 1, t 0, t 1]), //  4: a_1 -> A B     | B! A!      | 3 | a_1 A B
            (strip![t 2, loop 1],                   2, symbols![nt 1, t 2]),      //  5: a_1 -> a_1 C   | C! ●a_1    | 2 | a_1 C
            (strip![t 2],                           2, symbols![nt 1, t 2]),      //  6: a_1 -> C       | C!         | 2 | a_1 C
            (strip![nt 0],                          1, symbols![]),               //  7: <goal> -> a    | ►a         | 1 | 
        ], true, NTValue::Default, btreemap![0 => vec![0]]),

        // =========================================================================== +_or_* <L>
        // a -> A (<L=i> B)* C
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // +----------------------------------------+
        (200, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, nt 1, t 2]), //  0: a -> A i C  | C! ►i A! | 3 | A i C
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: i -> i B    | B! ●i    | 2 | i B
            (strip![],                              1, symbols![nt 1]),           //  2: i -> ε      |          | 1 | i
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a | ►a       | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, <goal>: n
        (200, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, t 2]), //  0: a -> A i C  | C! ►i A! | 3 | A C
            (strip![t 1, loop 1],                   2, symbols![t 1]),      //  1: i -> i B    | B! ●i    | 2 | B
            (strip![],                              1, symbols![]),         //  2: i -> ε      |          | 1 |
            (strip![nt 0],                          1, symbols![]),         //  3: <goal> -> a | ►a       | 1 |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> A (<L=i> B)+ C
        //
        //   NT    name  val   flags
        // +----------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*, plus        |
        // |   1 | . i  | y  | child_+_or_*, L-form, plus |
        // +----------------------------------------------+
        (201, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, nt 1, t 2]), //  0: a -> A i C  | C! ►i A! | 3 | A i C
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: i -> i B    | B! ●i    | 2 | i B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: i -> B      | B!       | 2 | i B
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a | ►a       | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, <goal>: n
        (201, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, t 2]), //  0: a -> A i C  | C! ►i A! | 3 | A C
            (strip![t 1, loop 1],                   2, symbols![t 1]),      //  1: i -> i B    | B! ●i    | 2 | B
            (strip![t 1],                           2, symbols![t 1]),      //  2: i -> B      | B!       | 2 | B
            (strip![nt 0],                          1, symbols![]),         //  3: <goal> -> a | ►a       | 1 |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> Id "(" (<L=i> Id ":" type / "<" ">")+ ")"
        // type -> Id
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // |   2 | type | y  |                                |
        // +--------------------------------------------------+
        (212, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 5, nt 1, t 1, t 0],              4, symbols![t 0, nt 1]),       //  0: a -> Id "(" i ")"          | ")" ►i "(" Id!           | 4    | Id i
            (strip![nt 2, t 2, t 0, t 4, t 3, loop 1], 6, symbols![nt 1, t 0, nt 2]), //  1: i -> i "<" ">" Id ":" type | ►type ":" Id! ">" "<" ●i | 6, 3 | i Id type
            (strip![nt 2, t 2, t 0],                   4, symbols![nt 1, t 0, nt 2]), //  2: i -> Id ":" type           | ►type ":" Id!            | 4    | i Id type
            (strip![t 0],                              1, symbols![t 0]),             //  3: type -> Id                 | Id!                      | 1    | Id
            (strip![nt 0],                             1, symbols![]),                //  4: <goal> -> a                | ►a                       | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> X (<L=i> B / ",")+ Z
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (219, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 3, nt 1, t 0],                3, symbols![t 0, nt 1, t 3]), //  0: a -> X i Z   | Z! ►i X!  | 3    | X i Z
            (strip![t 1, t 2, loop 1],              3, symbols![nt 1, t 1]),      //  1: i -> i "," B | B! "," ●i | 3, 1 | i B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: i -> B       | B!        | 2    | i B
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a  | ►a        | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, <goal>: n
        (219, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 3, nt 1, t 0],                3, symbols![t 0, t 3]), //  0: a -> X i Z   | Z! ►i X!  | 3    | X Z
            (strip![t 1, t 2, loop 1],              3, symbols![t 1]),      //  1: i -> i "," B | B! "," ●i | 3, 1 | B
            (strip![t 1],                           2, symbols![t 1]),      //  2: i -> B       | B!        | 2    | B
            (strip![nt 0],                          1, symbols![]),         //  3: <goal> -> a  | ►a        | 1    |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

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
            (strip![t 4, nt 1, t 1, t 0],           4, symbols![t 0, nt 1]),       //  0: a -> Id "(" i ")"      | ")" ►i "(" Id!       | 4    | Id i
            (strip![nt 2, t 2, t 0, t 3, loop 1],   5, symbols![nt 1, t 0, nt 2]), //  1: i -> i "," Id ":" type | ►type ":" Id! "," ●i | 5, 3 | i Id type
            (strip![nt 2, t 2, t 0],                4, symbols![nt 1, t 0, nt 2]), //  2: i -> Id ":" type       | ►type ":" Id!        | 4    | i Id type
            (strip![t 0],                           1, symbols![t 0]),             //  3: type -> Id             | Id!                  | 1    | Id
            (strip![nt 0],                          1, symbols![]),                //  4: <goal> -> a            | ►a                   | 1    |
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
            (strip![t 4, nt 1, t 0],                3, symbols![t 0, nt 1, t 4]), //  0: a -> A i E  | E! ►i A! | 3 | A i E
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: i -> i B    | B! ●i    | 2 | i B
            (strip![t 3, t 2, loop 1],              3, symbols![nt 1, t 2, t 3]), //  2: i -> i C D  | D! C! ●i | 3 | i C D
            (strip![],                              1, symbols![nt 1]),           //  3: i -> ε      |          | 1 | i
            (strip![nt 0],                          1, symbols![]),               //  4: <goal> -> a | ►a       | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, <goal>: n
        (250, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 1, t 0],                3, symbols![t 0, t 4]), //  0: a -> A i E  | E! ►i A! | 3 | A E
            (strip![t 1, loop 1],                   2, symbols![t 1]),      //  1: i -> i B    | B! ●i    | 2 | B
            (strip![t 3, t 2, loop 1],              3, symbols![t 2, t 3]), //  2: i -> i C D  | D! C! ●i | 3 | C D
            (strip![],                              1, symbols![]),         //  3: i -> ε      |          | 1 |
            (strip![nt 0],                          1, symbols![]),         //  4: <goal> -> a | ►a       | 1 |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> A (<L=i> B | C D)+ E
        //
        //   NT    name  val   flags
        // +----------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*              |
        // |   1 | . i  | y  | child_+_or_*, L-form, plus |
        // +----------------------------------------------+
        (251, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 1, t 0],                3, symbols![t 0, nt 1, t 4]), //  0: a -> A i E  | E! ►i A! | 3 | A i E
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: i -> i B    | B! ●i    | 2 | i B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: i -> B      | B!       | 2 | i B
            (strip![t 3, t 2, loop 1],              3, symbols![nt 1, t 2, t 3]), //  3: i -> i C D  | D! C! ●i | 3 | i C D
            (strip![t 3, t 2],                      3, symbols![nt 1, t 2, t 3]), //  4: i -> C D    | D! C!    | 3 | i C D
            (strip![nt 0],                          1, symbols![]),               //  5: <goal> -> a | ►a       | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, <goal>: n
        (251, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 1, t 0],                3, symbols![t 0, t 4]), //  0: a -> A i E  | E! ►i A! | 3 | A E
            (strip![t 1, loop 1],                   2, symbols![t 1]),      //  1: i -> i B    | B! ●i    | 2 | B
            (strip![t 1],                           2, symbols![t 1]),      //  2: i -> B      | B!       | 2 | B
            (strip![t 3, t 2, loop 1],              3, symbols![t 2, t 3]), //  3: i -> i C D  | D! C! ●i | 3 | C D
            (strip![t 3, t 2],                      3, symbols![t 2, t 3]), //  4: i -> C D    | D! C!    | 3 | C D
            (strip![nt 0],                          1, symbols![]),         //  5: <goal> -> a | ►a       | 1 |
        ], true, NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // =========================================================================== right_rec
        // a -> A a | B
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // +-------------------------+
        (303, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 0, t 0],                     2, symbols![t 0, nt 0]), //  0: a -> A a    | ►a A! | 2 | A a
            (strip![t 1],                           1, symbols![t 1]),       //  1: a -> B      | B!    | 1 | B
            (strip![nt 0],                          1, symbols![]),          //  2: <goal> -> a | ►a    | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // e -> f | e "." Id
        // f -> Id
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | e    | y  |       |
        // |   1 | f    | y  |       |
        // +-------------------------+
        (502, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),      //  0: e -> f        | ►f         | 1 | f
            (strip![t 1, t 0, nt 0],                3, symbols![nt 0, t 1]), //  1: e -> e "." Id | Id! "." ►e | 3 | e Id
            (strip![t 1],                           1, symbols![t 1]),       //  2: f -> Id       | Id!        | 1 | Id
            (strip![nt 0],                          1, symbols![]),          //  3: <goal> -> e   | ►e         | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2]]),

        // =========================================================================== left_rec
        // a -> a B | A
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // +-------------------------+
        (503, false, false, false, 0, btreemap![
        ], vec![
            (strip![t 0, nt 0],                     2, symbols![nt 0, t 0]), //  0: a -> a B    | B! ►a | 2 | a B
            (strip![t 1],                           1, symbols![t 1]),       //  1: a -> A      | A!    | 1 | A
            (strip![nt 0],                          1, symbols![]),          //  2: <goal> -> a | ►a    | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> a A | a B | C | D
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // +-------------------------+
        (504, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 0, nt 0],                     2, symbols![nt 0, t 0]), //  0: a -> a A    | A! ►a | 2 | a A
            (strip![t 1, nt 0],                     2, symbols![nt 0, t 1]), //  1: a -> a B    | B! ►a | 2 | a B
            (strip![t 2],                           1, symbols![t 2]),       //  2: a -> C      | C!    | 1 | C
            (strip![t 3],                           1, symbols![t 3]),       //  3: a -> D      | D!    | 1 | D
            (strip![nt 0],                          1, symbols![]),          //  4: <goal> -> a | ►a    | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0, 1, 2, 3]]),

        // =========================================================================== ambiguous

        // e -> e "*" e | <R> e "!" e | e "+" e | Num
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | e    | y  |       |
        // +-------------------------+
        (607, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 0, t 0, nt 0],               3, symbols![nt 0, nt 0]), //  0: e -> e "*" e | ►e "*" ►e | 3 | e e
            (strip![nt 0, t 2, nt 0],               3, symbols![nt 0, nt 0]), //  1: e -> e "!" e | ►e "!" ►e | 3 | e e
            (strip![nt 0, t 1, nt 0],               3, symbols![nt 0, nt 0]), //  2: e -> e "+" e | ►e "+" ►e | 3 | e e
            (strip![t 3],                           1, symbols![t 3]),        //  3: e -> Num     | Num!      | 1 | Num
            (strip![nt 0],                          1, symbols![]),           //  4: <goal> -> e  | ►e        | 1 |
        ], true, NTValue::Default, btreemap![0 => vec![0, 1, 2, 3]]),

        // =========================================================================== mix
        // program -> (<L=stmt_i> stmt)*
        // stmt -> decl | inst
        // decl -> Type (Id / ",")+ ";" | "typedef" Type Id ";"
        // inst -> Id "=" expr ";" | "print" expr ";"
        // expr -> "-" expr | expr "+" expr | expr <P> "-" expr | Id | Num
        //
        //   NT    name      val   flags
        // +----------------------------------------------+
        // |   0 | program  | y  | parent_+_or_*          |
        // |   1 | . stmt_i | y  | child_+_or_*, L-form   |
        // |   2 | stmt     | y  |                        |
        // |   3 | decl     | y  | parent_+_or_*          |
        // |   6 | . decl_1 | y  | child_+_or_*, sep_list |
        // |   4 | inst     | y  |                        |
        // |   5 | expr     | y  |                        |
        // +----------------------------------------------+
        // #[cfg(any())] // disabled because the wrapper source code has been modified
        (903, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),       //  0: program -> stmt_i             | ►stmt_i                 | 1    | stmt_i
            (strip![nt 2, loop 1],                  2, symbols![nt 1, nt 2]), //  1: stmt_i -> stmt_i stmt         | ►stmt ●stmt_i           | 2    | stmt_i stmt
            (strip![],                              1, symbols![nt 1]),       //  2: stmt_i -> ε                   |                         | 1    | stmt_i
            (strip![nt 3],                          1, symbols![nt 3]),       //  3: stmt -> decl                  | ►decl                   | 1    | decl
            (strip![nt 4],                          1, symbols![nt 4]),       //  4: stmt -> inst                  | ►inst                   | 1    | inst
            (strip![t 4, nt 6, t 2],                3, symbols![t 2, nt 6]),  //  5: decl -> Type decl_1 ";"       | ";" ►decl_1 Type!       | 3    | Type decl_1
            (strip![t 4, t 1, t 2, t 5],            4, symbols![t 2, t 1]),   //  6: decl -> "typedef" Type Id ";" | ";" Id! Type! "typedef" | 4    | Type Id
            (strip![t 4, nt 5, t 6, t 1],           4, symbols![t 1, nt 5]),  //  7: inst -> Id "=" expr ";"       | ";" ►expr "=" Id!       | 4    | Id expr
            (strip![t 4, nt 5, t 7],                3, symbols![nt 5]),       //  8: inst -> "print" expr ";"      | ";" ►expr "print"       | 3    | expr
            (strip![nt 5, t 8],                     2, symbols![nt 5]),       //  9: expr -> "-" expr              | ►expr "-"               | 2    | expr
            (strip![nt 5, t 9, nt 5],               3, symbols![nt 5, nt 5]), // 10: expr -> expr "+" expr         | ►expr "+" ►expr         | 3    | expr expr
            (strip![nt 5, t 8, nt 5],               3, symbols![nt 5, nt 5]), // 11: expr -> expr "-" expr         | ►expr "-" ►expr         | 3    | expr expr
            (strip![t 1],                           1, symbols![t 1]),        // 12: expr -> Id                    | Id!                     | 1    | Id
            (strip![t 0],                           1, symbols![t 0]),        // 13: expr -> Num                   | Num!                    | 1    | Num
            (strip![t 1, t 3, loop 6],              3, symbols![nt 6, t 1]),  // 14: decl_1 -> decl_1 "," Id       | Id! "," ●decl_1         | 3, 1 | decl_1 Id
            (strip![t 1],                           2, symbols![nt 6, t 1]),  // 15: decl_1 -> Id                  | Id!                     | 2    | decl_1 Id
            (strip![nt 0],                          1, symbols![]),           // 16: <goal> -> program             | ►program                | 1    |
        ], true, NTValue::Default, btreemap![0 => vec![0], 2 => vec![3, 4], 3 => vec![5, 6], 4 => vec![7, 8], 5 => vec![9, 10, 11, 12, 13]]),

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
        //   NT    name    val   flags
        // +----------------------------------------+
        // |   0 | a      | y  |                    |
        // |   1 | s      | y  |                    |
        // |   2 | p      | y  |                    |
        // |   3 | vs     | y  | parent_+_or_*      |
        // |  10 | . vs_1 | y  | child_+_or_*       |
        // |   4 | ns     | y  | parent_+_or_*      |
        // |  11 | . ns_1 |    | child_+_or_*       |
        // |   5 | xs     | y  | parent_+_or_*      |
        // |  12 | . xs_1 |    | child_+_or_*       |
        // |   6 | vp     | y  | parent_+_or_*      |
        // |  13 | . vp_1 | y  | child_+_or_*, plus |
        // |   7 | np     | y  | parent_+_or_*      |
        // |  14 | . np_1 |    | child_+_or_*, plus |
        // |   8 | xp     | y  | parent_+_or_*      |
        // |  15 | . xp_1 |    | child_+_or_*, plus |
        // |   9 | x      |    |                    |
        // +----------------------------------------+
        (980, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),       //  0: a -> s p         | ►p ►s       | 2 | s p
            (strip![nt 5, nt 4, nt 3],              3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs    | ►xs ►ns ►vs | 3 | vs ns xs
            (strip![nt 8, nt 7, nt 6],              3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp    | ►xp ►np ►vp | 3 | vp np xp
            (strip![nt 10, t 0],                    2, symbols![t 0, nt 10]),       //  3: vs -> A vs_1     | ►vs_1 A!    | 2 | A vs_1
            (strip![nt 11, t 2],                    2, symbols![]),                 //  4: ns -> "A" ns_1   | ►ns_1 "A"   | 2 |
            (strip![nt 12, t 0],                    2, symbols![t 0]),              //  5: xs -> A xs_1     | ►xs_1 A!    | 2 | A
            (strip![nt 13, t 0],                    2, symbols![t 0, nt 13]),       //  6: vp -> A vp_1     | ►vp_1 A!    | 2 | A vp_1
            (strip![nt 14, t 2],                    2, symbols![]),                 //  7: np -> "A" np_1   | ►np_1 "A"   | 2 |
            (strip![nt 15, t 0],                    2, symbols![t 0]),              //  8: xp -> A xp_1     | ►xp_1 A!    | 2 | A
            (strip![t 4],                           1, symbols![]),                 //  9: x -> "X"         | "X"         | 1 |
            (strip![t 1, loop 10],                  2, symbols![nt 10, t 1]),       // 10: vs_1 -> vs_1 B   | B! ●vs_1    | 2 | vs_1 B
            (strip![],                              1, symbols![nt 10]),            // 11: vs_1 -> ε        |             | 1 | vs_1
            (strip![t 3, loop 11],                  2, symbols![]),                 // 12: ns_1 -> ns_1 "B" | "B" ●ns_1   | 2 |
            (strip![],                              1, symbols![]),                 // 13: ns_1 -> ε        |             | 1 |
            (strip![nt 9, loop 12],                 2, symbols![]),                 // 14: xs_1 -> xs_1 x   | ►x ●xs_1    | 2 |
            (strip![],                              1, symbols![]),                 // 15: xs_1 -> ε        |             | 1 |
            (strip![t 1, loop 13],                  2, symbols![nt 13, t 1]),       // 16: vp_1 -> vp_1 B   | B! ●vp_1    | 2 | vp_1 B
            (strip![t 1],                           2, symbols![nt 13, t 1]),       // 17: vp_1 -> B        | B!          | 2 | vp_1 B
            (strip![t 3, loop 14],                  2, symbols![]),                 // 18: np_1 -> np_1 "B" | "B" ●np_1   | 2 |
            (strip![t 3],                           2, symbols![]),                 // 19: np_1 -> "B"      | "B"         | 2 |
            (strip![nt 9, loop 15],                 2, symbols![]),                 // 20: xp_1 -> xp_1 x   | ►x ●xp_1    | 2 |
            (strip![nt 9],                          2, symbols![]),                 // 21: xp_1 -> x        | ►x          | 2 |
            (strip![nt 0],                          1, symbols![]),                 // 22: <goal> -> a      | ►a          | 1 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 6, 7, 8]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5], 6 => vec![6], 7 => vec![7], 8 => vec![8], 9 => vec![9]]),
        // a: y, s: y, p: y, vs: y, ns: y, xs: y, vp: y, np: y, xp: y, x: n, vs_1: y, ns_1: n, xs_1: n, vp_1: y, np_1: n, xp_1: n, <goal>: n
        (980, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),       //  0: a -> s p         | ►p ►s       | 2 | s p
            (strip![nt 5, nt 4, nt 3],              3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs    | ►xs ►ns ►vs | 3 | vs ns xs
            (strip![nt 8, nt 7, nt 6],              3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp    | ►xp ►np ►vp | 3 | vp np xp
            (strip![nt 10, t 0],                    2, symbols![t 0, nt 10]),       //  3: vs -> A vs_1     | ►vs_1 A!    | 2 | A vs_1
            (strip![nt 11, t 2],                    2, symbols![]),                 //  4: ns -> "A" ns_1   | ►ns_1 "A"   | 2 |
            (strip![nt 12, t 0],                    2, symbols![t 0]),              //  5: xs -> A xs_1     | ►xs_1 A!    | 2 | A
            (strip![nt 13, t 0],                    2, symbols![t 0, nt 13]),       //  6: vp -> A vp_1     | ►vp_1 A!    | 2 | A vp_1
            (strip![nt 14, t 2],                    2, symbols![]),                 //  7: np -> "A" np_1   | ►np_1 "A"   | 2 |
            (strip![nt 15, t 0],                    2, symbols![t 0]),              //  8: xp -> A xp_1     | ►xp_1 A!    | 2 | A
            (strip![t 4],                           1, symbols![]),                 //  9: x -> "X"         | "X"         | 1 |
            (strip![t 1, loop 10],                  2, symbols![nt 10, t 1]),       // 10: vs_1 -> vs_1 B   | B! ●vs_1    | 2 | vs_1 B
            (strip![],                              1, symbols![nt 10]),            // 11: vs_1 -> ε        |             | 1 | vs_1
            (strip![t 3, loop 11],                  2, symbols![]),                 // 12: ns_1 -> ns_1 "B" | "B" ●ns_1   | 2 |
            (strip![],                              1, symbols![]),                 // 13: ns_1 -> ε        |             | 1 |
            (strip![nt 9, loop 12],                 2, symbols![]),                 // 14: xs_1 -> xs_1 x   | ►x ●xs_1    | 2 |
            (strip![],                              1, symbols![]),                 // 15: xs_1 -> ε        |             | 1 |
            (strip![t 1, loop 13],                  2, symbols![nt 13, t 1]),       // 16: vp_1 -> vp_1 B   | B! ●vp_1    | 2 | vp_1 B
            (strip![t 1],                           2, symbols![nt 13, t 1]),       // 17: vp_1 -> B        | B!          | 2 | vp_1 B
            (strip![t 3, loop 14],                  2, symbols![]),                 // 18: np_1 -> np_1 "B" | "B" ●np_1   | 2 |
            (strip![t 3],                           2, symbols![]),                 // 19: np_1 -> "B"      | "B"         | 2 |
            (strip![nt 9, loop 15],                 2, symbols![]),                 // 20: xp_1 -> xp_1 x   | ►x ●xp_1    | 2 |
            (strip![nt 9],                          2, symbols![]),                 // 21: xp_1 -> x        | ►x          | 2 |
            (strip![nt 0],                          1, symbols![]),                 // 22: <goal> -> a      | ►a          | 1 |
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
        //   NT    name    val   flags              
        // +----------------------------------------+
        // |   0 | a      | y  |                    |
        // |   1 | s      | y  |                    |
        // |   2 | p      | y  |                    |
        // |   3 | vs     | y  | parent_+_or_*      |
        // |  11 | . vs_1 | y  | child_+_or_*       |
        // |   4 | ns     | y  | parent_+_or_*      |
        // |  12 | . ns_1 |    | child_+_or_*       |
        // |   5 | xs     | y  | parent_+_or_*      |
        // |  13 | . xs_1 |    | child_+_or_*       |
        // |   6 | vp     | y  | parent_+_or_*      |
        // |  14 | . vp_1 | y  | child_+_or_*, plus |
        // |   7 | np     | y  | parent_+_or_*      |
        // |  15 | . np_1 |    | child_+_or_*, plus |
        // |   8 | xp     | y  | parent_+_or_*      |
        // |  16 | . xp_1 |    | child_+_or_*, plus |
        // |   9 | x      |    |                    |
        // |  10 | y      |    |                    |
        // +----------------------------------------+
        (981, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),       //  0: a -> s p             | ►p ►s         | 2 | s p
            (strip![nt 5, nt 4, nt 3],              3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs        | ►xs ►ns ►vs   | 3 | vs ns xs
            (strip![nt 8, nt 7, nt 6],              3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp        | ►xp ►np ►vp   | 3 | vp np xp
            (strip![nt 11, t 0],                    2, symbols![t 0, nt 11]),       //  3: vs -> A vs_1         | ►vs_1 A!      | 2 | A vs_1
            (strip![nt 12, t 4],                    2, symbols![]),                 //  4: ns -> "A" ns_1       | ►ns_1 "A"     | 2 | 
            (strip![nt 13, t 0],                    2, symbols![t 0]),              //  5: xs -> A xs_1         | ►xs_1 A!      | 2 | A
            (strip![nt 14, t 0],                    2, symbols![t 0, nt 14]),       //  6: vp -> A vp_1         | ►vp_1 A!      | 2 | A vp_1
            (strip![nt 15, t 4],                    2, symbols![]),                 //  7: np -> "A" np_1       | ►np_1 "A"     | 2 | 
            (strip![nt 16, t 0],                    2, symbols![t 0]),              //  8: xp -> A xp_1         | ►xp_1 A!      | 2 | A
            (strip![t 8],                           1, symbols![]),                 //  9: x -> "X"             | "X"           | 1 | 
            (strip![t 9],                           1, symbols![]),                 // 10: y -> "Y"             | "Y"           | 1 | 
            (strip![t 1, loop 11],                  2, symbols![nt 11, t 1]),       // 11: vs_1 -> vs_1 B       | B! ●vs_1      | 2 | vs_1 B
            (strip![t 3, t 2, loop 11],             3, symbols![nt 11, t 2, t 3]),  // 12: vs_1 -> vs_1 C D     | D! C! ●vs_1   | 3 | vs_1 C D
            (strip![],                              1, symbols![nt 11]),            // 13: vs_1 -> ε            |               | 1 | vs_1
            (strip![t 5, loop 12],                  2, symbols![]),                 // 14: ns_1 -> ns_1 "B"     | "B" ●ns_1     | 2 | 
            (strip![t 7, t 6, loop 12],             3, symbols![]),                 // 15: ns_1 -> ns_1 "C" "D" | "D" "C" ●ns_1 | 3 | 
            (strip![],                              1, symbols![]),                 // 16: ns_1 -> ε            |               | 1 | 
            (strip![nt 9, loop 13],                 2, symbols![]),                 // 17: xs_1 -> xs_1 x       | ►x ●xs_1      | 2 | 
            (strip![nt 9, nt 10, loop 13],          3, symbols![]),                 // 18: xs_1 -> xs_1 y x     | ►x ►y ●xs_1   | 3 | 
            (strip![],                              1, symbols![]),                 // 19: xs_1 -> ε            |               | 1 | 
            (strip![t 1, loop 14],                  2, symbols![nt 14, t 1]),       // 20: vp_1 -> vp_1 B       | B! ●vp_1      | 2 | vp_1 B
            (strip![t 1],                           2, symbols![nt 14, t 1]),       // 21: vp_1 -> B            | B!            | 2 | vp_1 B
            (strip![t 3, t 2, loop 14],             3, symbols![nt 14, t 2, t 3]),  // 22: vp_1 -> vp_1 C D     | D! C! ●vp_1   | 3 | vp_1 C D
            (strip![t 3, t 2],                      3, symbols![nt 14, t 2, t 3]),  // 23: vp_1 -> C D          | D! C!         | 3 | vp_1 C D
            (strip![t 5, loop 15],                  2, symbols![]),                 // 24: np_1 -> np_1 "B"     | "B" ●np_1     | 2 | 
            (strip![t 5],                           2, symbols![]),                 // 25: np_1 -> "B"          | "B"           | 2 | 
            (strip![t 7, t 6, loop 15],             3, symbols![]),                 // 26: np_1 -> np_1 "C" "D" | "D" "C" ●np_1 | 3 | 
            (strip![t 7, t 6],                      3, symbols![]),                 // 27: np_1 -> "C" "D"      | "D" "C"       | 3 | 
            (strip![nt 9, loop 16],                 2, symbols![]),                 // 28: xp_1 -> xp_1 x       | ►x ●xp_1      | 2 | 
            (strip![nt 9],                          2, symbols![]),                 // 29: xp_1 -> x            | ►x            | 2 | 
            (strip![nt 9, nt 10, loop 16],          3, symbols![]),                 // 30: xp_1 -> xp_1 y x     | ►x ►y ●xp_1   | 3 | 
            (strip![nt 9, nt 10],                   3, symbols![]),                 // 31: xp_1 -> y x          | ►x ►y         | 3 | 
            (strip![nt 0],                          1, symbols![]),                 // 32: <goal> -> a          | ►a            | 1 | 
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 6, 7, 8]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5], 6 => vec![6], 7 => vec![7], 8 => vec![8], 9 => vec![9], 10 => vec![10]]),
        // a: y, s: y, p: y, vs: y, ns: y, xs: y, vp: y, np: y, xp: y, x: n, y: n, vs_1: y, ns_1: n, xs_1: n, vp_1: y, np_1: n, xp_1: n, <goal>: n
        (981, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),       //  0: a -> s p             | ►p ►s         | 2 | s p
            (strip![nt 5, nt 4, nt 3],              3, symbols![nt 3, nt 4, nt 5]), //  1: s -> vs ns xs        | ►xs ►ns ►vs   | 3 | vs ns xs
            (strip![nt 8, nt 7, nt 6],              3, symbols![nt 6, nt 7, nt 8]), //  2: p -> vp np xp        | ►xp ►np ►vp   | 3 | vp np xp
            (strip![nt 11, t 0],                    2, symbols![t 0, nt 11]),       //  3: vs -> A vs_1         | ►vs_1 A!      | 2 | A vs_1
            (strip![nt 12, t 4],                    2, symbols![]),                 //  4: ns -> "A" ns_1       | ►ns_1 "A"     | 2 | 
            (strip![nt 13, t 0],                    2, symbols![t 0]),              //  5: xs -> A xs_1         | ►xs_1 A!      | 2 | A
            (strip![nt 14, t 0],                    2, symbols![t 0, nt 14]),       //  6: vp -> A vp_1         | ►vp_1 A!      | 2 | A vp_1
            (strip![nt 15, t 4],                    2, symbols![]),                 //  7: np -> "A" np_1       | ►np_1 "A"     | 2 | 
            (strip![nt 16, t 0],                    2, symbols![t 0]),              //  8: xp -> A xp_1         | ►xp_1 A!      | 2 | A
            (strip![t 8],                           1, symbols![]),                 //  9: x -> "X"             | "X"           | 1 | 
            (strip![t 9],                           1, symbols![]),                 // 10: y -> "Y"             | "Y"           | 1 | 
            (strip![t 1, loop 11],                  2, symbols![nt 11, t 1]),       // 11: vs_1 -> vs_1 B       | B! ●vs_1      | 2 | vs_1 B
            (strip![t 3, t 2, loop 11],             3, symbols![nt 11, t 2, t 3]),  // 12: vs_1 -> vs_1 C D     | D! C! ●vs_1   | 3 | vs_1 C D
            (strip![],                              1, symbols![nt 11]),            // 13: vs_1 -> ε            |               | 1 | vs_1
            (strip![t 5, loop 12],                  2, symbols![]),                 // 14: ns_1 -> ns_1 "B"     | "B" ●ns_1     | 2 | 
            (strip![t 7, t 6, loop 12],             3, symbols![]),                 // 15: ns_1 -> ns_1 "C" "D" | "D" "C" ●ns_1 | 3 | 
            (strip![],                              1, symbols![]),                 // 16: ns_1 -> ε            |               | 1 | 
            (strip![nt 9, loop 13],                 2, symbols![]),                 // 17: xs_1 -> xs_1 x       | ►x ●xs_1      | 2 | 
            (strip![nt 9, nt 10, loop 13],          3, symbols![]),                 // 18: xs_1 -> xs_1 y x     | ►x ►y ●xs_1   | 3 | 
            (strip![],                              1, symbols![]),                 // 19: xs_1 -> ε            |               | 1 | 
            (strip![t 1, loop 14],                  2, symbols![nt 14, t 1]),       // 20: vp_1 -> vp_1 B       | B! ●vp_1      | 2 | vp_1 B
            (strip![t 1],                           2, symbols![nt 14, t 1]),       // 21: vp_1 -> B            | B!            | 2 | vp_1 B
            (strip![t 3, t 2, loop 14],             3, symbols![nt 14, t 2, t 3]),  // 22: vp_1 -> vp_1 C D     | D! C! ●vp_1   | 3 | vp_1 C D
            (strip![t 3, t 2],                      3, symbols![nt 14, t 2, t 3]),  // 23: vp_1 -> C D          | D! C!         | 3 | vp_1 C D
            (strip![t 5, loop 15],                  2, symbols![]),                 // 24: np_1 -> np_1 "B"     | "B" ●np_1     | 2 | 
            (strip![t 5],                           2, symbols![]),                 // 25: np_1 -> "B"          | "B"           | 2 | 
            (strip![t 7, t 6, loop 15],             3, symbols![]),                 // 26: np_1 -> np_1 "C" "D" | "D" "C" ●np_1 | 3 | 
            (strip![t 7, t 6],                      3, symbols![]),                 // 27: np_1 -> "C" "D"      | "D" "C"       | 3 | 
            (strip![nt 9, loop 16],                 2, symbols![]),                 // 28: xp_1 -> xp_1 x       | ►x ●xp_1      | 2 | 
            (strip![nt 9],                          2, symbols![]),                 // 29: xp_1 -> x            | ►x            | 2 | 
            (strip![nt 9, nt 10, loop 16],          3, symbols![]),                 // 30: xp_1 -> xp_1 y x     | ►x ►y ●xp_1   | 3 | 
            (strip![nt 9, nt 10],                   3, symbols![]),                 // 31: xp_1 -> y x          | ►x ►y         | 3 | 
            (strip![nt 0],                          1, symbols![]),                 // 32: <goal> -> a          | ►a            | 1 | 
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
        //   NT    name   val   flags
        // +-----------------------------------------------+
        // |   0 | a     | y  |                            |
        // |   1 | s     | y  |                            |
        // |   2 | p     | y  |                            |
        // |   3 | vs    | y  | parent_+_or_*              |
        // |   4 | . ivs | y  | child_+_or_*, L-form       |
        // |   5 | ns    | y  | parent_+_or_*              |
        // |   6 | . ins |    | child_+_or_*, L-form       |
        // |   7 | xs    | y  | parent_+_or_*              |
        // |   8 | . ixs |    | child_+_or_*, L-form       |
        // |   9 | vp    | y  | parent_+_or_*              |
        // |  10 | . ivp | y  | child_+_or_*, L-form, plus |
        // |  11 | np    | y  | parent_+_or_*              |
        // |  12 | . inp |    | child_+_or_*, L-form, plus |
        // |  13 | xp    | y  | parent_+_or_*              |
        // |  14 | . ixp |    | child_+_or_*, L-form, plus |
        // |  15 | x     |    |                            |
        // +-----------------------------------------------+
        (982, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),         //  0: a -> s p       | ►p ►s       | 2 | s p
            (strip![nt 7, nt 5, nt 3],              3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs  | ►xs ►ns ►vs | 3 | vs ns xs
            (strip![nt 13, nt 11, nt 9],            3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp  | ►xp ►np ►vp | 3 | vp np xp
            (strip![nt 4, t 0],                     2, symbols![t 0, nt 4]),          //  3: vs -> A ivs    | ►ivs A!     | 2 | A ivs
            (strip![t 1, loop 4],                   2, symbols![nt 4, t 1]),          //  4: ivs -> ivs B   | B! ●ivs     | 2 | ivs B
            (strip![],                              1, symbols![nt 4]),               //  5: ivs -> ε       |             | 1 | ivs
            (strip![nt 6, t 2],                     2, symbols![]),                   //  6: ns -> "A" ins  | ►ins "A"    | 2 |
            (strip![t 3, loop 6],                   2, symbols![]),                   //  7: ins -> ins "B" | "B" ●ins    | 2 |
            (strip![],                              1, symbols![]),                   //  8: ins -> ε       |             | 1 |
            (strip![nt 8, t 0],                     2, symbols![t 0]),                //  9: xs -> A ixs    | ►ixs A!     | 2 | A
            (strip![nt 15, loop 8],                 2, symbols![]),                   // 10: ixs -> ixs x   | ►x ●ixs     | 2 |
            (strip![],                              1, symbols![]),                   // 11: ixs -> ε       |             | 1 |
            (strip![nt 10, t 0],                    2, symbols![t 0, nt 10]),         // 12: vp -> A ivp    | ►ivp A!     | 2 | A ivp
            (strip![t 1, loop 10],                  2, symbols![nt 10, t 1]),         // 13: ivp -> ivp B   | B! ●ivp     | 2 | ivp B
            (strip![t 1],                           2, symbols![nt 10, t 1]),         // 14: ivp -> B       | B!          | 2 | ivp B
            (strip![nt 12, t 2],                    2, symbols![]),                   // 15: np -> "A" inp  | ►inp "A"    | 2 |
            (strip![t 3, loop 12],                  2, symbols![]),                   // 16: inp -> inp "B" | "B" ●inp    | 2 |
            (strip![t 3],                           2, symbols![]),                   // 17: inp -> "B"     | "B"         | 2 |
            (strip![nt 14, t 0],                    2, symbols![t 0]),                // 18: xp -> A ixp    | ►ixp A!     | 2 | A
            (strip![nt 15, loop 14],                2, symbols![]),                   // 19: ixp -> ixp x   | ►x ●ixp     | 2 |
            (strip![nt 15],                         2, symbols![]),                   // 20: ixp -> x       | ►x          | 2 |
            (strip![t 4],                           1, symbols![]),                   // 21: x -> "X"       | "X"         | 1 |
            (strip![nt 0],                          1, symbols![]),                   // 22: <goal> -> a    | ►a          | 1 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![6], 7 => vec![9], 9 => vec![12], 11 => vec![15], 13 => vec![18], 15 => vec![21]]),
        // a: y, s: y, p: y, vs: y, ivs: y, ns: y, ins: n, xs: y, ixs: n, vp: y, ivp: y, np: y, inp: n, xp: y, ixp: n, x: n, <goal>: n
        (982, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),         //  0: a -> s p       | ►p ►s       | 2 | s p
            (strip![nt 7, nt 5, nt 3],              3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs  | ►xs ►ns ►vs | 3 | vs ns xs
            (strip![nt 13, nt 11, nt 9],            3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp  | ►xp ►np ►vp | 3 | vp np xp
            (strip![nt 4, t 0],                     2, symbols![t 0, nt 4]),          //  3: vs -> A ivs    | ►ivs A!     | 2 | A ivs
            (strip![t 1, loop 4],                   2, symbols![nt 4, t 1]),          //  4: ivs -> ivs B   | B! ●ivs     | 2 | ivs B
            (strip![],                              1, symbols![nt 4]),               //  5: ivs -> ε       |             | 1 | ivs
            (strip![nt 6, t 2],                     2, symbols![]),                   //  6: ns -> "A" ins  | ►ins "A"    | 2 |
            (strip![t 3, loop 6],                   2, symbols![]),                   //  7: ins -> ins "B" | "B" ●ins    | 2 |
            (strip![],                              1, symbols![]),                   //  8: ins -> ε       |             | 1 |
            (strip![nt 8, t 0],                     2, symbols![t 0]),                //  9: xs -> A ixs    | ►ixs A!     | 2 | A
            (strip![nt 15, loop 8],                 2, symbols![]),                   // 10: ixs -> ixs x   | ►x ●ixs     | 2 |
            (strip![],                              1, symbols![]),                   // 11: ixs -> ε       |             | 1 |
            (strip![nt 10, t 0],                    2, symbols![t 0, nt 10]),         // 12: vp -> A ivp    | ►ivp A!     | 2 | A ivp
            (strip![t 1, loop 10],                  2, symbols![nt 10, t 1]),         // 13: ivp -> ivp B   | B! ●ivp     | 2 | ivp B
            (strip![t 1],                           2, symbols![nt 10, t 1]),         // 14: ivp -> B       | B!          | 2 | ivp B
            (strip![nt 12, t 2],                    2, symbols![]),                   // 15: np -> "A" inp  | ►inp "A"    | 2 |
            (strip![t 3, loop 12],                  2, symbols![]),                   // 16: inp -> inp "B" | "B" ●inp    | 2 |
            (strip![t 3],                           2, symbols![]),                   // 17: inp -> "B"     | "B"         | 2 |
            (strip![nt 14, t 0],                    2, symbols![t 0]),                // 18: xp -> A ixp    | ►ixp A!     | 2 | A
            (strip![nt 15, loop 14],                2, symbols![]),                   // 19: ixp -> ixp x   | ►x ●ixp     | 2 |
            (strip![nt 15],                         2, symbols![]),                   // 20: ixp -> x       | ►x          | 2 |
            (strip![t 4],                           1, symbols![]),                   // 21: x -> "X"       | "X"         | 1 |
            (strip![nt 0],                          1, symbols![]),                   // 22: <goal> -> a    | ►a          | 1 |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![6], 7 => vec![9], 9 => vec![12], 11 => vec![15], 13 => vec![18], 15 => vec![21]]),

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
        //   NT    name   val   flags
        // +-----------------------------------------------+
        // |   0 | a     | y  |                            |
        // |   1 | s     | y  |                            |
        // |   2 | p     | y  |                            |
        // |   3 | vs    | y  | parent_+_or_*              |
        // |   4 | . ivs | y  | child_+_or_*, L-form       |
        // |   5 | ns    | y  | parent_+_or_*              |
        // |   6 | . ins |    | child_+_or_*, L-form       |
        // |   7 | xs    | y  | parent_+_or_*              |
        // |   8 | . ixs |    | child_+_or_*, L-form       |
        // |   9 | vp    | y  | parent_+_or_*              |
        // |  10 | . ivp | y  | child_+_or_*, L-form, plus |
        // |  11 | np    | y  | parent_+_or_*              |
        // |  12 | . inp |    | child_+_or_*, L-form, plus |
        // |  13 | xp    | y  | parent_+_or_*              |
        // |  14 | . ixp |    | child_+_or_*, L-form, plus |
        // |  15 | x     |    |                            |
        // |  16 | y     |    |                            |
        // +-----------------------------------------------+
        (983, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),         //  0: a -> s p           | ►p ►s        | 2 | s p
            (strip![nt 7, nt 5, nt 3],              3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs      | ►xs ►ns ►vs  | 3 | vs ns xs
            (strip![nt 13, nt 11, nt 9],            3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp      | ►xp ►np ►vp  | 3 | vp np xp
            (strip![nt 4, t 0],                     2, symbols![t 0, nt 4]),          //  3: vs -> A ivs        | ►ivs A!      | 2 | A ivs
            (strip![t 1, loop 4],                   2, symbols![nt 4, t 1]),          //  4: ivs -> ivs B       | B! ●ivs      | 2 | ivs B
            (strip![t 3, t 2, loop 4],              3, symbols![nt 4, t 2, t 3]),     //  5: ivs -> ivs C D     | D! C! ●ivs   | 3 | ivs C D
            (strip![],                              1, symbols![nt 4]),               //  6: ivs -> ε           |              | 1 | ivs
            (strip![nt 6, t 4],                     2, symbols![]),                   //  7: ns -> "A" ins      | ►ins "A"     | 2 |
            (strip![t 5, loop 6],                   2, symbols![]),                   //  8: ins -> ins "B"     | "B" ●ins     | 2 |
            (strip![t 7, t 6, loop 6],              3, symbols![]),                   //  9: ins -> ins "C" "D" | "D" "C" ●ins | 3 |
            (strip![],                              1, symbols![]),                   // 10: ins -> ε           |              | 1 |
            (strip![nt 8, t 0],                     2, symbols![t 0]),                // 11: xs -> A ixs        | ►ixs A!      | 2 | A
            (strip![nt 15, loop 8],                 2, symbols![]),                   // 12: ixs -> ixs x       | ►x ●ixs      | 2 |
            (strip![nt 15, nt 16, loop 8],          3, symbols![]),                   // 13: ixs -> ixs y x     | ►x ►y ●ixs   | 3 |
            (strip![],                              1, symbols![]),                   // 14: ixs -> ε           |              | 1 |
            (strip![nt 10, t 0],                    2, symbols![t 0, nt 10]),         // 15: vp -> A ivp        | ►ivp A!      | 2 | A ivp
            (strip![t 1, loop 10],                  2, symbols![nt 10, t 1]),         // 16: ivp -> ivp B       | B! ●ivp      | 2 | ivp B
            (strip![t 1],                           2, symbols![nt 10, t 1]),         // 17: ivp -> B           | B!           | 2 | ivp B
            (strip![t 3, t 2, loop 10],             3, symbols![nt 10, t 2, t 3]),    // 18: ivp -> ivp C D     | D! C! ●ivp   | 3 | ivp C D
            (strip![t 3, t 2],                      3, symbols![nt 10, t 2, t 3]),    // 19: ivp -> C D         | D! C!        | 3 | ivp C D
            (strip![nt 12, t 4],                    2, symbols![]),                   // 20: np -> "A" inp      | ►inp "A"     | 2 |
            (strip![t 5, loop 12],                  2, symbols![]),                   // 21: inp -> inp "B"     | "B" ●inp     | 2 |
            (strip![t 5],                           2, symbols![]),                   // 22: inp -> "B"         | "B"          | 2 |
            (strip![t 7, t 6, loop 12],             3, symbols![]),                   // 23: inp -> inp "C" "D" | "D" "C" ●inp | 3 |
            (strip![t 7, t 6],                      3, symbols![]),                   // 24: inp -> "C" "D"     | "D" "C"      | 3 |
            (strip![nt 14, t 0],                    2, symbols![t 0]),                // 25: xp -> A ixp        | ►ixp A!      | 2 | A
            (strip![nt 15, loop 14],                2, symbols![]),                   // 26: ixp -> ixp x       | ►x ●ixp      | 2 |
            (strip![nt 15],                         2, symbols![]),                   // 27: ixp -> x           | ►x           | 2 |
            (strip![nt 15, nt 16, loop 14],         3, symbols![]),                   // 28: ixp -> ixp y x     | ►x ►y ●ixp   | 3 |
            (strip![nt 15, nt 16],                  3, symbols![]),                   // 29: ixp -> y x         | ►x ►y        | 3 |
            (strip![t 8],                           1, symbols![]),                   // 30: x -> "X"           | "X"          | 1 |
            (strip![t 9],                           1, symbols![]),                   // 31: y -> "Y"           | "Y"          | 1 |
            (strip![nt 0],                          1, symbols![]),                   // 32: <goal> -> a        | ►a           | 1 |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![7], 7 => vec![11], 9 => vec![15], 11 => vec![20], 13 => vec![25], 15 => vec![30], 16 => vec![31]]),
        // a: y, s: y, p: y, vs: y, ivs: y, ns: y, ins: n, xs: y, ixs: n, vp: y, ivp: y, np: y, inp: n, xp: y, ixp: n, x: n, y: n, <goal>: n
        (983, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 2, nt 1],                    2, symbols![nt 1, nt 2]),         //  0: a -> s p           | ►p ►s        | 2 | s p
            (strip![nt 7, nt 5, nt 3],              3, symbols![nt 3, nt 5, nt 7]),   //  1: s -> vs ns xs      | ►xs ►ns ►vs  | 3 | vs ns xs
            (strip![nt 13, nt 11, nt 9],            3, symbols![nt 9, nt 11, nt 13]), //  2: p -> vp np xp      | ►xp ►np ►vp  | 3 | vp np xp
            (strip![nt 4, t 0],                     2, symbols![t 0, nt 4]),          //  3: vs -> A ivs        | ►ivs A!      | 2 | A ivs
            (strip![t 1, loop 4],                   2, symbols![nt 4, t 1]),          //  4: ivs -> ivs B       | B! ●ivs      | 2 | ivs B
            (strip![t 3, t 2, loop 4],              3, symbols![nt 4, t 2, t 3]),     //  5: ivs -> ivs C D     | D! C! ●ivs   | 3 | ivs C D
            (strip![],                              1, symbols![nt 4]),               //  6: ivs -> ε           |              | 1 | ivs
            (strip![nt 6, t 4],                     2, symbols![]),                   //  7: ns -> "A" ins      | ►ins "A"     | 2 |
            (strip![t 5, loop 6],                   2, symbols![]),                   //  8: ins -> ins "B"     | "B" ●ins     | 2 |
            (strip![t 7, t 6, loop 6],              3, symbols![]),                   //  9: ins -> ins "C" "D" | "D" "C" ●ins | 3 |
            (strip![],                              1, symbols![]),                   // 10: ins -> ε           |              | 1 |
            (strip![nt 8, t 0],                     2, symbols![t 0]),                // 11: xs -> A ixs        | ►ixs A!      | 2 | A
            (strip![nt 15, loop 8],                 2, symbols![]),                   // 12: ixs -> ixs x       | ►x ●ixs      | 2 |
            (strip![nt 15, nt 16, loop 8],          3, symbols![]),                   // 13: ixs -> ixs y x     | ►x ►y ●ixs   | 3 |
            (strip![],                              1, symbols![]),                   // 14: ixs -> ε           |              | 1 |
            (strip![nt 10, t 0],                    2, symbols![t 0, nt 10]),         // 15: vp -> A ivp        | ►ivp A!      | 2 | A ivp
            (strip![t 1, loop 10],                  2, symbols![nt 10, t 1]),         // 16: ivp -> ivp B       | B! ●ivp      | 2 | ivp B
            (strip![t 1],                           2, symbols![nt 10, t 1]),         // 17: ivp -> B           | B!           | 2 | ivp B
            (strip![t 3, t 2, loop 10],             3, symbols![nt 10, t 2, t 3]),    // 18: ivp -> ivp C D     | D! C! ●ivp   | 3 | ivp C D
            (strip![t 3, t 2],                      3, symbols![nt 10, t 2, t 3]),    // 19: ivp -> C D         | D! C!        | 3 | ivp C D
            (strip![nt 12, t 4],                    2, symbols![]),                   // 20: np -> "A" inp      | ►inp "A"     | 2 |
            (strip![t 5, loop 12],                  2, symbols![]),                   // 21: inp -> inp "B"     | "B" ●inp     | 2 |
            (strip![t 5],                           2, symbols![]),                   // 22: inp -> "B"         | "B"          | 2 |
            (strip![t 7, t 6, loop 12],             3, symbols![]),                   // 23: inp -> inp "C" "D" | "D" "C" ●inp | 3 |
            (strip![t 7, t 6],                      3, symbols![]),                   // 24: inp -> "C" "D"     | "D" "C"      | 3 |
            (strip![nt 14, t 0],                    2, symbols![t 0]),                // 25: xp -> A ixp        | ►ixp A!      | 2 | A
            (strip![nt 15, loop 14],                2, symbols![]),                   // 26: ixp -> ixp x       | ►x ●ixp      | 2 |
            (strip![nt 15],                         2, symbols![]),                   // 27: ixp -> x           | ►x           | 2 |
            (strip![nt 15, nt 16, loop 14],         3, symbols![]),                   // 28: ixp -> ixp y x     | ►x ►y ●ixp   | 3 |
            (strip![nt 15, nt 16],                  3, symbols![]),                   // 29: ixp -> y x         | ►x ►y        | 3 |
            (strip![t 8],                           1, symbols![]),                   // 30: x -> "X"           | "X"          | 1 |
            (strip![t 9],                           1, symbols![]),                   // 31: y -> "Y"           | "Y"          | 1 |
            (strip![nt 0],                          1, symbols![]),                   // 32: <goal> -> a        | ►a           | 1 |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 5 => vec![7], 7 => vec![11], 9 => vec![15], 11 => vec![20], 13 => vec![25], 15 => vec![30], 16 => vec![31]]),

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
            (strip![nt 3, nt 2, nt 1],              3, symbols![nt 1, nt 2, nt 3]), //  0: a -> vp np xp            | ►xp ►np ►vp       | 3    | vp np xp
            (strip![nt 6, t 0],                     2, symbols![t 0, nt 6]),        //  1: vp -> A vp_1             | ►vp_1 A!          | 2    | A vp_1
            (strip![nt 7, t 4],                     2, symbols![]),                 //  2: np -> "A" np_1           | ►np_1 "A"         | 2    |
            (strip![nt 8, t 0],                     2, symbols![t 0]),              //  3: xp -> A xp_1             | ►xp_1 A!          | 2    | A
            (strip![t 7],                           1, symbols![]),                 //  4: x -> "X"                 | "X"               | 1    |
            (strip![t 8],                           1, symbols![]),                 //  5: y -> "Y"                 | "Y"               | 1    |
            (strip![t 2, t 1, t 3, loop 6],         4, symbols![nt 6, t 1, t 2]),   //  6: vp_1 -> vp_1 "," B C     | C! B! "," ●vp_1   | 4, 2 | vp_1 B C
            (strip![t 2, t 1],                      3, symbols![nt 6, t 1, t 2]),   //  7: vp_1 -> B C              | C! B!             | 3    | vp_1 B C
            (strip![t 6, t 5, t 3, loop 7],         4, symbols![]),                 //  8: np_1 -> np_1 "," "B" "C" | "C" "B" "," ●np_1 | 4, 2 |
            (strip![t 6, t 5],                      3, symbols![]),                 //  9: np_1 -> "B" "C"          | "C" "B"           | 3    |
            (strip![nt 5, nt 4, t 3, loop 8],       4, symbols![]),                 // 10: xp_1 -> xp_1 "," x y     | ►y ►x "," ●xp_1   | 4, 2 |
            (strip![nt 5, nt 4],                    3, symbols![]),                 // 11: xp_1 -> x y              | ►y ►x             | 3    |
            (strip![nt 0],                          1, symbols![]),                 // 12: <goal> -> a              | ►a                | 1    |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3]), btreemap![0 => vec![0], 1 => vec![1], 2 => vec![2], 3 => vec![3], 4 => vec![4], 5 => vec![5]]),
        // a: y, vp: y, np: y, xp: y, x: n, y: n, vp_1: y, np_1: n, xp_1: n, <goal>: n
        (984, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 3, nt 2, nt 1],              3, symbols![nt 1, nt 2, nt 3]), //  0: a -> vp np xp            | ►xp ►np ►vp       | 3    | vp np xp
            (strip![nt 6, t 0],                     2, symbols![t 0, nt 6]),        //  1: vp -> A vp_1             | ►vp_1 A!          | 2    | A vp_1
            (strip![nt 7, t 4],                     2, symbols![]),                 //  2: np -> "A" np_1           | ►np_1 "A"         | 2    |
            (strip![nt 8, t 0],                     2, symbols![t 0]),              //  3: xp -> A xp_1             | ►xp_1 A!          | 2    | A
            (strip![t 7],                           1, symbols![]),                 //  4: x -> "X"                 | "X"               | 1    |
            (strip![t 8],                           1, symbols![]),                 //  5: y -> "Y"                 | "Y"               | 1    |
            (strip![t 2, t 1, t 3, loop 6],         4, symbols![nt 6, t 1, t 2]),   //  6: vp_1 -> vp_1 "," B C     | C! B! "," ●vp_1   | 4, 2 | vp_1 B C
            (strip![t 2, t 1],                      3, symbols![nt 6, t 1, t 2]),   //  7: vp_1 -> B C              | C! B!             | 3    | vp_1 B C
            (strip![t 6, t 5, t 3, loop 7],         4, symbols![]),                 //  8: np_1 -> np_1 "," "B" "C" | "C" "B" "," ●np_1 | 4, 2 |
            (strip![t 6, t 5],                      3, symbols![]),                 //  9: np_1 -> "B" "C"          | "C" "B"           | 3    |
            (strip![nt 5, nt 4, t 3, loop 8],       4, symbols![]),                 // 10: xp_1 -> xp_1 "," x y     | ►y ►x "," ●xp_1   | 4, 2 |
            (strip![nt 5, nt 4],                    3, symbols![]),                 // 11: xp_1 -> x y              | ►y ►x             | 3    |
            (strip![nt 0],                          1, symbols![]),                 // 12: <goal> -> a              | ►a                | 1    |
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
        // |   5 | xp    | y  | parent_+_or_*                  |
        // |   6 | . ixp |    | child_+_or_*, L-form, sep_list |
        // |   7 | x     |    |                                |
        // |   8 | y     |    |                                |
        // +---------------------------------------------------+
        (985, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 5, nt 3, nt 1],              3, symbols![nt 1, nt 3, nt 5]), //  0: a -> vp np xp          | ►xp ►np ►vp      | 3    | vp np xp
            (strip![nt 2, t 0],                     2, symbols![t 0, nt 2]),        //  1: vp -> A ivp            | ►ivp A!          | 2    | A ivp
            (strip![t 2, t 1, t 3, loop 2],         4, symbols![nt 2, t 1, t 2]),   //  2: ivp -> ivp "," B C     | C! B! "," ●ivp   | 4, 2 | ivp B C
            (strip![t 2, t 1],                      3, symbols![nt 2, t 1, t 2]),   //  3: ivp -> B C             | C! B!            | 3    | ivp B C
            (strip![nt 4, t 4],                     2, symbols![]),                 //  4: np -> "A" inp          | ►inp "A"         | 2    |
            (strip![t 6, t 5, t 3, loop 4],         4, symbols![]),                 //  5: inp -> inp "," "B" "C" | "C" "B" "," ●inp | 4, 2 |
            (strip![t 6, t 5],                      3, symbols![]),                 //  6: inp -> "B" "C"         | "C" "B"          | 3    |
            (strip![nt 6, t 0],                     2, symbols![t 0]),              //  7: xp -> A ixp            | ►ixp A!          | 2    | A
            (strip![nt 8, nt 7, t 3, loop 6],       4, symbols![]),                 //  8: ixp -> ixp "," x y     | ►y ►x "," ●ixp   | 4, 2 |
            (strip![nt 8, nt 7],                    3, symbols![]),                 //  9: ixp -> x y             | ►y ►x            | 3    |
            (strip![t 7],                           1, symbols![]),                 // 10: x -> "X"               | "X"              | 1    |
            (strip![t 8],                           1, symbols![]),                 // 11: y -> "Y"               | "Y"              | 1    |
            (strip![nt 0],                          1, symbols![]),                 // 12: <goal> -> a            | ►a               | 1    |
        ], true, NTValue::SetIds(vec![0, 1, 2, 3, 5]), btreemap![0 => vec![0], 1 => vec![1], 3 => vec![4], 5 => vec![7], 7 => vec![10], 8 => vec![11]]),
        // a: y, vp: y, ivp: y, np: y, inp: n, xp: y, ixp: n, x: n, y: n, <goal>: n
        (985, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 5, nt 3, nt 1],              3, symbols![nt 1, nt 3, nt 5]), //  0: a -> vp np xp          | ►xp ►np ►vp      | 3    | vp np xp
            (strip![nt 2, t 0],                     2, symbols![t 0, nt 2]),        //  1: vp -> A ivp            | ►ivp A!          | 2    | A ivp
            (strip![t 2, t 1, t 3, loop 2],         4, symbols![nt 2, t 1, t 2]),   //  2: ivp -> ivp "," B C     | C! B! "," ●ivp   | 4, 2 | ivp B C
            (strip![t 2, t 1],                      3, symbols![nt 2, t 1, t 2]),   //  3: ivp -> B C             | C! B!            | 3    | ivp B C
            (strip![nt 4, t 4],                     2, symbols![]),                 //  4: np -> "A" inp          | ►inp "A"         | 2    |
            (strip![t 6, t 5, t 3, loop 4],         4, symbols![]),                 //  5: inp -> inp "," "B" "C" | "C" "B" "," ●inp | 4, 2 |
            (strip![t 6, t 5],                      3, symbols![]),                 //  6: inp -> "B" "C"         | "C" "B"          | 3    |
            (strip![nt 6, t 0],                     2, symbols![t 0]),              //  7: xp -> A ixp            | ►ixp A!          | 2    | A
            (strip![nt 8, nt 7, t 3, loop 6],       4, symbols![]),                 //  8: ixp -> ixp "," x y     | ►y ►x "," ●ixp   | 4, 2 |
            (strip![nt 8, nt 7],                    3, symbols![]),                 //  9: ixp -> x y             | ►y ►x            | 3    |
            (strip![t 7],                           1, symbols![]),                 // 10: x -> "X"               | "X"              | 1    |
            (strip![t 8],                           1, symbols![]),                 // 11: y -> "Y"               | "Y"              | 1    |
            (strip![nt 0],                          1, symbols![]),                 // 12: <goal> -> a            | ►a               | 1    |
        ], false, NTValue::SetIds(vec![0, 1, 2, 3, 5]), btreemap![0 => vec![0], 1 => vec![1], 3 => vec![4], 5 => vec![7], 7 => vec![10], 8 => vec![11]]),

        // ===========================================================================
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
        parser_type: ParserType::LALR,
        wrapper_filenames: WRAPPER_FILENAMES,
        tests: get_lr_tests(),
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
        parser_type: ParserType::LALR,
        wrapper_filenames: WRAPPER_FILENAMES,
        tests: get_lr_tests(),
    };
    build_items(spec);
}
