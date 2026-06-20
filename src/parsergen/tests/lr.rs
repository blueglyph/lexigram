// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use lexigram_core::strip;
use crate::{btreemap, symbols};
use crate::parsergen::{NTValue, ParserType};
use crate::parsergen::tests::wrapper_source::{build_items, BuildItemsTestEntry, BuildItemsTestSpec};

// List in decreasing order of test parser number of the file were they're generated:
static WRAPPER_FILENAMES: &[(u32, &str)] = &[
//  (  N, "tests/out_lr/wrapper_source1.rs"),   //       n >= N are generated here
    (  0, "tests/out_lr/wrapper_source.rs"),    //   N > n >= 0
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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A | B)*
        //
        //   NT    name   val   flags
        // +----------------------------------+
        // |   0 | a     | y  | parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*  |
        // +----------------------------------+
        (150, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),      //  0: a -> a_1     | ►a_1    | 1 | a_1
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]), //  1: a_1 -> a_1 A | A! ●a_1 | 2 | a_1 A
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]), //  2: a_1 -> a_1 B | B! ●a_1 | 2 | a_1 B
            (strip![],                              1, symbols![nt 1]),      //  3: a_1 -> ε     |         | 1 | a_1
            (strip![nt 0],                          1, symbols![]),          //  4: <goal> -> a  | ►a      | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A | B)+
        //
        //   NT    name   val   flags
        // +----------------------------------------+
        // |   0 | a     | y  | parent_+_or_*, plus |
        // |   1 | . a_1 | y  | child_+_or_*, plus  |
        // +----------------------------------------+
        (151, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),      //  0: a -> a_1     | ►a_1    | 1 | a_1
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]), //  1: a_1 -> a_1 A | A! ●a_1 | 2 | a_1 A
            (strip![t 0],                           2, symbols![nt 1, t 0]), //  2: a_1 -> A     | A!      | 2 | a_1 A
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]), //  3: a_1 -> a_1 B | B! ●a_1 | 2 | a_1 B
            (strip![t 1],                           2, symbols![nt 1, t 1]), //  4: a_1 -> B     | B!      | 2 | a_1 B
            (strip![nt 0],                          1, symbols![]),          //  5: <goal> -> a  | ►a      | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // a: y, i: n, <goal>: n
        (219, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 3, nt 1, t 0],                3, symbols![t 0, t 3]), //  0: a -> X i Z   | Z! ►i X!  | 3    | X Z
            (strip![t 1, t 2, loop 1],              3, symbols![t 1]),      //  1: i -> i "," B | B! "," ●i | 3, 1 | B
            (strip![t 1],                           2, symbols![t 1]),      //  2: i -> B       | B!        | 2    | B
            (strip![nt 0],                          1, symbols![]),         //  3: <goal> -> a  | ►a        | 1    |
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

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
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> (<L=i> A | B C)*
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // +----------------------------------------+
        (250, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),           //  0: a -> i      | ►i       | 1 | i
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]),      //  1: i -> i A    | A! ●i    | 2 | i A
            (strip![t 2, t 1, loop 1],              3, symbols![nt 1, t 1, t 2]), //  2: i -> i B C  | C! B! ●i | 3 | i B C
            (strip![],                              1, symbols![nt 1]),           //  3: i -> ε      |          | 1 | i
            (strip![nt 0],                          1, symbols![]),               //  4: <goal> -> a | ►a       | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | B C)+
        //
        //   NT    name  val   flags
        // +----------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*              |
        // |   1 | . i  | y  | child_+_or_*, L-form, plus |
        // +----------------------------------------------+
        (251, true, true, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),           //  0: a -> i      | ►i       | 1 | i
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]),      //  1: i -> i A    | A! ●i    | 2 | i A
            (strip![t 0],                           2, symbols![nt 1, t 0]),      //  2: i -> A      | A!       | 2 | i A
            (strip![t 2, t 1, loop 1],              3, symbols![nt 1, t 1, t 2]), //  3: i -> i B C  | C! B! ●i | 3 | i B C
            (strip![t 2, t 1],                      3, symbols![nt 1, t 1, t 2]), //  4: i -> B C    | C! B!    | 3 | i B C
            (strip![nt 0],                          1, symbols![]),               //  5: <goal> -> a | ►a       | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),
        (251, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![]),         //  0: a -> i      | ►i       | 1 |
            (strip![t 0, loop 1],                   2, symbols![t 0]),      //  1: i -> i A    | A! ●i    | 2 | A
            (strip![t 0],                           2, symbols![t 0]),      //  2: i -> A      | A!       | 2 | A
            (strip![t 2, t 1, loop 1],              3, symbols![t 1, t 2]), //  3: i -> i B C  | C! B! ●i | 3 | B C
            (strip![t 2, t 1],                      3, symbols![t 1, t 2]), //  4: i -> B C    | C! B!    | 3 | B C
            (strip![nt 0],                          1, symbols![]),         //  5: <goal> -> a | ►a       | 1 |
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // =========================================================================== right_rec
        // a -> A a | B
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | a    | y  |       |
        // +-------------------------+
        (303, false, false, false, 0, btreemap![
        ], vec![
            (strip![nt 0, t 0],                     2, symbols![t 0, nt 0]), //  0: a -> A a    | ►a A! | 2 | A a
            (strip![t 1],                           1, symbols![t 1]),       //  1: a -> B      | B!    | 1 | B
            (strip![nt 0],                          1, symbols![]),          //  2: <goal> -> a | ►a    | 1 |
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

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
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

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
        ], NTValue::Default, btreemap![0 => vec![0, 1, 2, 3]]),

        // ===========================================================================
        /* template:
        (, false, false, false, 0, btreemap![], vec![], NTValue::Default, btreemap![]),
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
