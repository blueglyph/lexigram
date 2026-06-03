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
        // - expected span, opcodes, items for each alt
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
        (1, false, false, false, 0, btreemap![
        ], vec![
            (2, symbols![t 0, t 1],                 strip![t 1, t 0]), //  0: a -> A B    | B! A! | 2 | A B
            (1, symbols![],                         strip![nt 0]),     //  1: <goal> -> a | ►a    | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- NT/T simple mix
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
            (3, symbols![t 0, nt 1],                strip![nt 1, t 1, t 0]), //  0: s -> Id "=" val   | ►val "=" Id!  | 3 | Id val
            (1, symbols![],                         strip![t 2]),            //  1: s -> "exit"       | "exit"        | 1 |
            (2, symbols![nt 1],                     strip![nt 1, t 3]),      //  2: s -> "return" val | ►val "return" | 2 | val
            (1, symbols![t 0],                      strip![t 0]),            //  3: val -> Id         | Id!           | 1 | Id
            (1, symbols![t 4],                      strip![t 4]),            //  4: val -> Num        | Num!          | 1 | Num
            (1, symbols![],                         strip![nt 0]),           //  5: <goal> -> s       | ►s            | 1 |
        ], NTValue::Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),
        
        /*
        (, false, false, false, 0, btreemap![], btreemap![], NTValue::Default, btreemap![]),
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
