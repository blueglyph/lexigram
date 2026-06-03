#![cfg(test)]

use lexigram_core::strip;
use crate::{btreemap, symbols};
use crate::parsergen::{NTValue, ParserType};
use crate::parsergen::tests::wrapper_source::{build_items, BuildItemsTestEntry, BuildItemsTestSpec};

const WRAPPER_FILENAME: &str = "tests/out_lr/wrapper_source.rs";

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
        (13, true, false, false, 0, btreemap![
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
        (102, true, false, false, 0, btreemap![
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
        (103, true, false, false, 0, btreemap![
        ], vec![
            (strip![t 2, nt 1, t 0],                3, symbols![t 0, nt 1, t 2]), //  0: a -> A a_1 C | C! ►a_1 A! | 3 | A a_1 C
            (strip![t 1, loop 1],                   2, symbols![nt 1, t 1]),      //  1: a_1 -> a_1 B | B! ●a_1    | 2 | a_1 B
            (strip![t 1],                           2, symbols![nt 1, t 1]),      //  2: a_1 -> B     | B!         | 2 | a_1 B
            (strip![nt 0],                          1, symbols![]),               //  3: <goal> -> a  | ►a         | 1 |
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
        wrapper_filename: WRAPPER_FILENAME,
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
        parser_type: ParserType::LALR,
        wrapper_filename: WRAPPER_FILENAME,
        tests: get_ll1_tests(),
    };
    build_items(spec);
}
