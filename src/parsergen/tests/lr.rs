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

        // a -> Id "(" Id ":" type ("," Id ":" type)* ")"
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
            (strip![nt 1, t 2, t 0, t 3, loop 2],   5, symbols![nt 2, t 0, nt 1]), //  2: a_1 -> a_1 "," Id ":" type | ►type ":" Id! "," ●a_1 | 5    | a_1 Id type
            (strip![nt 1, t 2, t 0],                1, symbols![nt 2, t 0, nt 1]), //  3: a_1 -> Id ":" type         | ►type ":" Id!          | 1, 3 | a_1 Id type
            (strip![nt 0],                          1, symbols![]),                //  4: <goal> -> a                | ►a                     | 1    |
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> Id "(" Id ("," Id)* "/" Id ("," Id)* ")"
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
            (strip![t 0, t 2, loop 1],               3, symbols![nt 1, t 0]),       //  1: a_1 -> a_1 "," Id           | Id! "," ●a_1              | 3    | a_1 Id
            (strip![t 0],                            1, symbols![nt 1, t 0]),       //  2: a_1 -> Id                   | Id!                       | 1, 1 | a_1 Id
            (strip![t 0, t 2, loop 2],               3, symbols![nt 2, t 0]),       //  3: a_2 -> a_2 "," Id           | Id! "," ●a_2              | 3    | a_2 Id
            (strip![t 0],                            1, symbols![nt 2, t 0]),       //  4: a_2 -> Id                   | Id!                       | 1, 1 | a_2 Id
            (strip![nt 0],                           1, symbols![]),                //  5: <goal> -> a                 | ►a                        | 1    |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> "let" Id ("," Id)* "=" Num ("," Num)* ";"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // |   2 | . a_2 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (112, false, false, false, 0, btreemap![
        ], vec![
            (strip![t 5, nt 2, t 3, nt 1, t 0],     5, symbols![nt 1, nt 2]), //  0: a -> "let" Id a_1 "=" Num a_2 ";" | ";" ►a_2 "=" ►a_1 "let" | 5    | a_1 a_2
            (strip![t 1, t 2, loop 1],              3, symbols![nt 1, t 1]),  //  1: a_1 -> a_1 "," Id                 | Id! "," ●a_1            | 3    | a_1 Id
            (strip![t 1],                           1, symbols![nt 1, t 1]),  //  2: a_1 -> Id                         | Id!                     | 1, 1 | a_1 Id
            (strip![t 4, t 2, loop 2],              3, symbols![nt 2, t 4]),  //  3: a_2 -> a_2 "," Num                | Num! "," ●a_2           | 3    | a_2 Num
            (strip![t 4],                           1, symbols![nt 2, t 4]),  //  4: a_2 -> Id                         | Num!                    | 1, 1 | a_2 Num
            (strip![nt 0],                          1, symbols![]),           //  5: <goal> -> a                       | ►a                      | 1    |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> Id "(" Id ":" type ("," Id ":" type)+ ")"
        // type -> Id
        //
        //   NT    name   val   flags
        // +----------------------------------------+
        // |   0 | a     | y  | parent_+_or_*, plus |
        // |   2 | . a_1 | y  | child_+_or_*, plus  |
        // |   1 | type  | y  |                     |
        // +----------------------------------------+
        (119, false, false, false, 0, btreemap![
        ], vec![
            (strip![t 4, nt 2, nt 1, t 2, t 0, t 1, t 0], 7, symbols![t 0, t 0, nt 1, nt 2]), //  0: a -> Id "(" Id ":" type a_1 ")" | ")" ►a_1 ►type ":" Id! "(" Id! | 7 | Id Id type a_1
            (strip![t 0],                                 1, symbols![t 0]),                  //  1: type -> Id                      | Id!                            | 1 | Id
            (strip![nt 1, t 2, t 0, t 3, loop 2],         5, symbols![nt 2, t 0, nt 1]),      //  2: a_1 -> a_1 "," Id ":" type      | ►type ":" Id! "," ●a_1         | 5 | a_1 Id type
            (strip![nt 1, t 2, t 0, t 3],                 5, symbols![nt 2, t 0, nt 1]),      //  3: a_1 -> "," Id ":" type          | ►type ":" Id! ","              | 5 | a_1 Id type
            (strip![nt 0],                                1, symbols![]),                     //  4: <goal> -> a                     | ►a                             | 1 |
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> A Id ("," Id)* C
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (120, true, true, false, 0, btreemap![
        ], vec![
            (strip![t 3, nt 1, t 0],                    3, symbols![t 0, nt 1, t 3]), //  0: a -> A a_1 C      | C! ►a_1 A!   | 3    | A a_1 C
            (strip![t 1, t 2, loop 1],                  3, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 "," Id | Id! "," ●a_1 | 3    | a_1 Id
            (strip![t 1],                               1, symbols![nt 1, t 1]),      //  2: a_1 -> Id         | Id!          | 1, 1 | a_1 Id
            (strip![nt 0],                              1, symbols![]),               //  3: <goal> -> a       | ►a           | 1    |
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

        // =========================================================================== +_or_* <L>
        // a -> A (<L=i> B)* C
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // +----------------------------------------+
        (200, true, false, false, 0, btreemap![
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
        (201, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, nt 1, t 2]), //  0: a -> A i C  | C! ►i A! | 3 | A i C
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: i -> i B    | B! ●i    | 2 | i B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: i -> B      | B!       | 2 | i B
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a | ►a       | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | B)*
        //
        //   NT    name  val   flags
        // +----------------------------------------+
        // |   0 | a    | y  | parent_+_or_*        |
        // |   1 | . i  | y  | child_+_or_*, L-form |
        // +----------------------------------------+
        (250, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),      //  0: a -> i      | ►i    | 1 | i
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]), //  1: i -> i A    | A! ●i | 2 | i A
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]), //  2: i -> i B    | B! ●i | 2 | i B
            (strip![],                              1, symbols![nt 1]),      //  3: i -> ε      |       | 1 | i
            (strip![nt 0],                          1, symbols![]),          //  4: <goal> -> a | ►a    | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | B)+
        //
        //   NT    name  val   flags
        // +----------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*, plus        |
        // |   1 | . i  | y  | child_+_or_*, L-form, plus |
        // +----------------------------------------------+
        (251, true, false, false, 0, btreemap![
        ], vec![
            (strip![nt 1],                          1, symbols![nt 1]),      //  0: a -> i      | ►i    | 1 | i
            (strip![t 0, loop 1],                   2, symbols![nt 1, t 0]), //  1: i -> i A    | A! ●i | 2 | i A
            (strip![t 0],                           2, symbols![nt 1, t 0]), //  2: i -> A      | A!    | 2 | i A
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]), //  3: i -> i B    | B! ●i | 2 | i B
            (strip![t 1],                           2, symbols![nt 1, t 1]), //  4: i -> B      | B!    | 2 | i B
            (strip![nt 0],                          1, symbols![]),          //  5: <goal> -> a | ►a    | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
