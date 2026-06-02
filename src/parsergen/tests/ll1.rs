#![cfg(test)]

use crate::{btreemap, symbols};
use crate::parsergen::{NTValue, ParserType};
use crate::parsergen::tests::wrapper_source::{build_items, BuildItemsTestEntry, BuildItemsTestSpec};

const WRAPPER_FILENAME: &str = "tests/out/wrapper_source.rs";

fn get_ll1_tests() -> Vec<BuildItemsTestEntry> {
    vec![
        // BuildItemsTestEntry fields for each test:
        // - TestRules #
        // - test sources?
        // - test sources include parser?
        // - use super::super::wrapper_code::...?
        // - start NT
        // - NT types
        // - expected items
        // - which symbols have a value
        // - expected alt groups
        //
        // CAUTION! Empty the first btreemap if the NTs have changed
        // ---------------------------------------------------------------------------
        // a -> A B
        (1, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![t 0, t 1]),           //  0: a -> A B | ◄0 B! A! | 2 | A B
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- NT/T simple mix
        // s -> Id "=" val | "exit" | "return" val
        // val -> Id | Num
        (13, true, false, true, 0, btreemap![
            0 => "SynS".to_string(),
            1 => "SynVal".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1]),          //  0: s -> Id "=" val   | ◄0 ►val "=" Id!  | 3 | Id val
            1 => (1, symbols![]),                   //  1: s -> "exit"       | ◄1 "exit"        | 1 |
            2 => (2, symbols![nt 1]),               //  2: s -> "return" val | ◄2 ►val "return" | 2 | val
            3 => (1, symbols![t 0]),                //  3: val -> Id         | ◄3 Id!           | 1 | Id
            4 => (1, symbols![t 4]),                //  4: val -> Num        | ◄4 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0, 1, 2], 1 => vec![3, 4]]),

        // --------------------------------------------------------------------------- NT with/without value
        // a -> b c | c
        // b -> Op c
        // c -> Id
        (14, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 1, nt 2]),         //  0: a -> b c  | ◄0 ►c ►b  | 2 | b c
            1 => (1, symbols![nt 2]),               //  1: a -> c    | ◄1 ►c     | 1 | c
            2 => (2, symbols![t 0, nt 2]),          //  2: b -> Op c | ◄2 ►c Op! | 2 | Op c
            3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
        (14, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 1]),               //  0: a -> b c  | ◄0 ►c ►b  | 2 | b
            1 => (1, symbols![]),                   //  1: a -> c    | ◄1 ►c     | 1 |
            2 => (2, symbols![t 0]),                //  2: b -> Op c | ◄2 ►c Op! | 2 | Op
            3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], NTValue::SetIds(vec![0, 1]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
        (14, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 2]),               //  0: a -> b c  | ◄0 ►c ►b  | 2 | c
            1 => (1, symbols![nt 2]),               //  1: a -> c    | ◄1 ►c     | 1 | c
            2 => (2, symbols![t 0, nt 2]),          //  2: b -> Op c | ◄2 ►c Op! | 2 | Op c
            3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], NTValue::SetIds(vec![0, 2]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),
        (14, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![]),                   //  0: a -> b c  | ◄0 ►c ►b  | 2 |
            1 => (1, symbols![]),                   //  1: a -> c    | ◄1 ►c     | 1 |
            2 => (2, symbols![t 0]),                //  2: b -> Op c | ◄2 ►c Op! | 2 | Op
            3 => (1, symbols![t 1]),                //  3: c -> Id   | ◄3 Id!    | 1 | Id
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3]]),

        // a -> b | c | d
        // b -> Op d        <-- start
        // c -> Id
        // d -> Num
        //
        //   NT    name  val   flags
        // +-------------------------+
        // |   0 | b    | y  |       |
        // |   1 | d    | y  |       |
        // +-------------------------+
        (15, true, false, true, 1, btreemap![
        ], btreemap![
            0 => (2, symbols![t 0, nt 1]),          //  0: b -> Op d | ◄0 ►d Op! | 2 | Op d
            1 => (1, symbols![t 2]),                //  1: d -> Num  | ◄1 Num!   | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // --------------------------------------------------------------------------- +_or_*
        // a -> A B* C
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        // parents:
        //  - a_1 -> a
        (102, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
            1 => (2, symbols![nt 1, t 1]),          //  1: a_1 -> B a_1 | ●a_1 ◄1 B!    | 2 | a_1 B
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε     | ◄2            | 1 | a_1
        ], NTValue::Default, btreemap![0 => vec![0]]),
        (102, false, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
            1 => (2, symbols![nt 1, t 1]),          //  1: a_1 -> B a_1 | ●a_1 ◄1 B!    | 2 | a_1 B
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε     | ◄2            | 1 | a_1
        ], NTValue::None, btreemap![0 => vec![0]]),

        // a -> A B+ C
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (103, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A a_1 C | ◄0 C! ►a_1 A! | 3 | A a_1 C
            1 => (0, symbols![]),                   //  1: a_1 -> B a_2 | ►a_2 B!       | 0 |
            2 => (2, symbols![nt 1, t 1]),          //  2: a_2 -> a_1   | ●a_1 ◄2       | 2 | a_1 B
            3 => (2, symbols![nt 1, t 1]),          //  3: a_2 -> ε     | ◄3            | 2 | a_1 B
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (b A b B A)*
        // b -> C
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        // parents:
        //  - a_1 -> a
        (104, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 2]),                            //  0: a -> a_1             | ◄0 ►a_1                | 1 | a_1
            1 => (1, symbols![t 2]),                             //  1: b -> C               | ◄1 C!                  | 1 | C
            2 => (6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  2: a_1 -> b A b B A a_1 | ●a_1 ◄2 A! B! ►b A! ►b | 6 | a_1 b A b B A
            3 => (1, symbols![nt 2]),                            //  3: a_1 -> ε             | ◄3                     | 1 | a_1
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (b A b B A)+
        // b -> C
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (105, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 2]),                            //  0: a -> a_1             | ◄0 ►a_1             | 1 | a_1
            1 => (1, symbols![t 2]),                             //  1: b -> C               | ◄1 C!               | 1 | C
            2 => (0, symbols![]),                                //  2: a_1 -> b A b B A a_2 | ►a_2 A! B! ►b A! ►b | 0 |
            3 => (6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  3: a_2 -> a_1           | ●a_1 ◄3             | 6 | a_1 b A b B A
            4 => (6, symbols![nt 2, nt 1, t 0, nt 1, t 1, t 0]), //  4: a_2 -> ε             | ◄4                  | 6 | a_1 b A b B A
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (A (b ",")* ";")* C
        // b -> B
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        //  - a_2: child_+_or_* | parent_+_or_* (2049)
        // parents:
        //  - a_1 -> a_2
        //  - a_2 -> a
        (106, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 3, t 3]),          //  0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
            1 => (1, symbols![t 4]),                //  1: b -> B               | ◄1 B!               | 1 | B
            2 => (3, symbols![nt 2, nt 1]),         //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 | a_1 b
            3 => (1, symbols![nt 2]),               //  3: a_1 -> ε             | ◄3                  | 1 | a_1
            4 => (4, symbols![nt 3, t 0, nt 2]),    //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A a_1
            5 => (1, symbols![nt 3]),               //  5: a_2 -> ε             | ◄5                  | 1 | a_2
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),
        (106, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 3, t 3]),          //  0: a -> a_2 C           | ◄0 C! ►a_2          | 2 | a_2 C
            1 => (1, symbols![t 4]),                //  1: b -> B               | ◄1 B!               | 1 | B
            2 => (3, symbols![]),                   //  2: a_1 -> b "," a_1     | ●a_1 ◄2 "," ►b      | 3 |
            3 => (1, symbols![]),                   //  3: a_1 -> ε             | ◄3                  | 1 |
            4 => (4, symbols![nt 3, t 0]),          //  4: a_2 -> A a_1 ";" a_2 | ●a_2 ◄4 ";" ►a_1 A! | 4 | a_2 A
            5 => (1, symbols![nt 3]),               //  5: a_2 -> ε             | ◄5                  | 1 | a_2
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> A "B"* C
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        // parents:
        //  - a_1 -> a
        (108, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, t 2]),           //  0: a -> A a_1 C   | ◄0 C! ►a_1 A! | 3 | A C
            1 => (2, symbols![]),                   //  1: a_1 -> "B" a_1 | ●a_1 ◄1 "B"   | 2 |
            2 => (1, symbols![]),                   //  2: a_1 -> ε       | ◄2            | 1 |
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> Id "(" Id ":" type ("," Id ":" type)* ")"
        // type -> Id
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* | sep_list (32769)
        // parents:
        //  - a_1 -> a
        (109, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (4, symbols![t 0, nt 2]),          //  0: a -> Id "(" Id ":" type a_1 ")" | ◄0 ")" ►a_1 ►type ":" Id! "(" Id! | 4    | Id a_1
            1 => (1, symbols![t 0]),                //  1: type -> Id                      | ◄1 Id!                            | 1    | Id
            2 => (5, symbols![nt 2, t 0, nt 1]),    //  2: a_1 -> "," Id ":" type a_1      | ●a_1 ◄2 ►type ":" Id! ","         | 5, 3 | a_1 Id type
            3 => (1, symbols![nt 2]),               //  3: a_1 -> ε                        | ◄3                                | 1    | a_1
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> Id "(" (Id ":" type ("," Id ":" type)*)? ")";
        //
        // a -> Id "(" Id ":" type ("," Id ":" type)* ")" | Id "(" ")"
        // type -> Id
        // NT flags:
        //  - a: parent_left_fact | parent_+_or_* (2080)
        //  - a_1: child_+_or_* | sep_list (32769)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        (110, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                   //  0: a -> Id "(" a_2            | ►a_2 "(" Id!              | 0    |
            1 => (1, symbols![t 0]),                //  1: type -> Id                 | ◄1 Id!                    | 1    | Id
            2 => (5, symbols![nt 2, t 0, nt 1]),    //  2: a_1 -> "," Id ":" type a_1 | ●a_1 ◄2 ►type ":" Id! "," | 5, 3 | a_1 Id type
            3 => (1, symbols![nt 2]),               //  3: a_1 -> ε                   | ◄3                        | 1    | a_1
            4 => (4, symbols![t 0, nt 2]),          //  4: a_2 -> Id ":" type a_1 ")" | ◄4 ")" ►a_1 ►type ":" Id! | 4    | Id a_1
            5 => (3, symbols![t 0]),                //  5: a_2 -> ")"                 | ◄5 ")"                    | 3    | Id
        ], NTValue::Default, btreemap![0 => vec![4, 5], 1 => vec![1]]),

        // a -> Id "(" Id ("," Id)* "/" Id ("," Id)* ")"
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* | sep_list (32769)
        //  - a_2: child_+_or_* | sep_list (32769)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        (111, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (6, symbols![t 0, nt 1, nt 2]),    //  0: a -> Id "(" Id a_1 "/" Id a_2 ")" | ◄0 ")" ►a_2 Id! "/" ►a_1 Id! "(" Id! | 6    | Id a_1 a_2
            1 => (3, symbols![nt 1, t 0]),          //  1: a_1 -> "," Id a_1                 | ●a_1 ◄1 Id! ","                      | 3, 1 | a_1 Id
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε                          | ◄2                                   | 1    | a_1
            3 => (3, symbols![nt 2, t 0]),          //  3: a_2 -> "," Id a_2                 | ●a_2 ◄3 Id! ","                      | 3, 1 | a_2 Id
            4 => (1, symbols![nt 2]),               //  4: a_2 -> ε                          | ◄4                                   | 1    | a_2
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> Id "(" Id ":" type ("," Id ":" type)+ ")"
        // type -> Id
        //
        //   NT    name     val   flags
        // +-----------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*, plus                  |
        // |   2 | . a_1   | y  | child_+_or_*, parent_left_fact, plus |
        // |   3 | .   a_2 |    | child_left_fact                      |
        // |   1 | type    | y  |                                      |
        // +-----------------------------------------------------------+
        (112, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (7, symbols![t 0, t 0, nt 1, nt 2]), //  0: a -> Id "(" Id ":" type a_1 ")" | ◄0 ")" ►a_1 ►type ":" Id! "(" Id! | 7 | Id Id type a_1
            1 => (1, symbols![t 0]),                  //  1: type -> Id                      | ◄1 Id!                            | 1 | Id
            2 => (0, symbols![]),                     //  2: a_1 -> "," Id ":" type a_2      | ►a_2 ►type ":" Id! ","            | 0 |
            3 => (5, symbols![nt 2, t 0, nt 1]),      //  3: a_2 -> a_1                      | ●a_1 ◄3                           | 5 | a_1 Id type
            4 => (5, symbols![nt 2, t 0, nt 1]),      //  4: a_2 -> ε                        | ◄4                                | 5 | a_1 Id type
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> X B ("," B)* B? Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*                    |
        // |   2 | . a_2 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (113, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> X B a_1 a_2 | ►a_2 ►a_1 B! X! | 0    |
            1 => (3, symbols![nt 1, t 1]),           //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! ","  | 3, 1 | a_1 B
            2 => (1, symbols![nt 1]),                //  2: a_1 -> ε         | ◄2              | 1    | a_1
            3 => (4, symbols![t 0, nt 1, t 1, t 3]), //  3: a_2 -> B Z       | ◄3 Z! B!        | 4    | X a_1 B Z
            4 => (3, symbols![t 0, nt 1, t 3]),      //  4: a_2 -> Z         | ◄4 Z!           | 3    | X a_1 Z
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X? B ("," B)* Z
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (114, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 3]),     //  0: a -> X B a_1 Z   | ◄0 Z! ►a_1 B! X! | 3    | X a_1 Z
            1 => (2, symbols![nt 1, t 3]),          //  1: a -> B a_1 Z     | ◄1 Z! ►a_1 B!    | 2    | a_1 Z
            2 => (3, symbols![nt 1, t 1]),          //  2: a_1 -> "," B a_1 | ●a_1 ◄2 B! ","   | 3, 1 | a_1 B
            3 => (1, symbols![nt 1]),               //  3: a_1 -> ε         | ◄3               | 1    | a_1
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> X Y? B ("," B)* Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list          |
        // |   2 | . a_2 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (115, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> X a_2       | ►a_2 X!          | 0    |
            1 => (3, symbols![nt 1, t 2]),           //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! ","   | 3, 1 | a_1 B
            2 => (1, symbols![nt 1]),                //  2: a_1 -> ε         | ◄2               | 1    | a_1
            3 => (4, symbols![t 0, t 1, nt 1, t 4]), //  3: a_2 -> Y B a_1 Z | ◄3 Z! ►a_1 B! Y! | 4    | X Y a_1 Z
            4 => (3, symbols![t 0, nt 1, t 4]),      //  4: a_2 -> B a_1 Z   | ◄4 Z! ►a_1 B!    | 3    | X a_1 Z
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X B? ("," B)* Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . a_1 | y  | child_+_or_*                    |
        // |   2 | . a_2 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (116, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> X a_2       | ►a_2 X!        | 0    |
            1 => (3, symbols![nt 1, t 1]),           //  1: a_1 -> "," B a_1 | ●a_1 ◄1 B! "," | 3, 1 | a_1 B
            2 => (1, symbols![nt 1]),                //  2: a_1 -> ε         | ◄2             | 1    | a_1
            3 => (4, symbols![t 0, t 1, nt 1, t 3]), //  3: a_2 -> B a_1 Z   | ◄3 Z! ►a_1 B!  | 4    | X B a_1 Z
            4 => (3, symbols![t 0, nt 1, t 3]),      //  4: a_2 -> a_1 Z     | ◄4 Z! ►a_1     | 3    | X a_1 Z
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X B B* Z
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (117, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> X B a_1 Z | ◄0 Z! ►a_1 B! X! | 3    | X a_1 Z
            1 => (2, symbols![nt 1, t 1]),          //  1: a_1 -> B a_1   | ●a_1 ◄1 B!       | 2, 1 | a_1 B
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε       | ◄2               | 1    | a_1
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> "var" Id ("," Id)* ";"
        //
        //   NT    name   val   flags
        // +-------------------------------------------+
        // |   0 | a     | y  | parent_+_or_*          |
        // |   1 | . a_1 | y  | child_+_or_*, sep_list |
        // +-------------------------------------------+
        (118, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![nt 1]),               //  0: a -> "var" Id a_1 ";" | ◄0 ";" ►a_1 Id! "var" | 3    | a_1
            1 => (3, symbols![nt 1, t 1]),          //  1: a_1 -> "," Id a_1     | ●a_1 ◄1 Id! ","       | 3, 1 | a_1 Id
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε              | ◄2                    | 1    | a_1
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- norm+/* alternatives
        // a -> (A | B)*
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        // parents:
        //  - a_1 -> a
        (150, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
            1 => (2, symbols![nt 1, t 0]),          //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | 2 | a_1 A
            2 => (2, symbols![nt 1, t 1]),          //  2: a_1 -> B a_1 | ●a_1 ◄2 B! | 2 | a_1 B
            3 => (1, symbols![nt 1]),               //  3: a_1 -> ε     | ◄3         | 1 | a_1
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A | B)+
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_fact (64)
        //  - a_3: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        //  - a_3 -> a_1
        (151, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
            1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
            2 => (0, symbols![]),                   //  2: a_1 -> B a_3 | ►a_3 B! | 0 |
            3 => (2, symbols![nt 1, t 0]),          //  3: a_2 -> a_1   | ●a_1 ◄3 | 2 | a_1 A
            4 => (2, symbols![nt 1, t 0]),          //  4: a_2 -> ε     | ◄4      | 2 | a_1 A
            5 => (2, symbols![nt 1, t 1]),          //  5: a_3 -> a_1   | ●a_1 ◄5 | 2 | a_1 B
            6 => (2, symbols![nt 1, t 1]),          //  6: a_3 -> ε     | ◄6      | 2 | a_1 B
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A (B | b C b B C | E)* F
        // b -> D
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        // parents:
        //  - a_1 -> a
        (152, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 2, t 4]),                  //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!          | 3 | A a_1 F
            1 => (1, symbols![t 5]),                             //  1: b -> D               | ◄1 D!                  | 1 | D
            2 => (2, symbols![nt 2, t 1]),                       //  2: a_1 -> B a_1         | ●a_1 ◄2 B!             | 2 | a_1 B
            3 => (6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), //  3: a_1 -> b C b B C a_1 | ●a_1 ◄3 C! B! ►b C! ►b | 6 | a_1 b C b B C
            4 => (2, symbols![nt 2, t 3]),                       //  4: a_1 -> E a_1         | ●a_1 ◄4 E!             | 2 | a_1 E
            5 => (1, symbols![nt 2]),                            //  5: a_1 -> ε             | ◄5                     | 1 | a_1
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> A (B | b C b B C | E)+ F
        // b -> D
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_fact (64)
        //  - a_3: child_left_fact (64)
        //  - a_4: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        //  - a_3 -> a_1
        //  - a_4 -> a_1
        (153, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 2, t 4]),                   //  0: a -> A a_1 F         | ◄0 F! ►a_1 A!       | 3 | A a_1 F
            1 => (1, symbols![t 5]),                              //  1: b -> D               | ◄1 D!               | 1 | D
            2 => (0, symbols![]),                                 //  2: a_1 -> B a_2         | ►a_2 B!             | 0 |
            3 => (0, symbols![]),                                 //  3: a_1 -> E a_3         | ►a_3 E!             | 0 |
            4 => (0, symbols![]),                                 //  4: a_1 -> b C b B C a_4 | ►a_4 C! B! ►b C! ►b | 0 |
            5 => (2, symbols![nt 2, t 1]),                        //  5: a_2 -> a_1           | ●a_1 ◄5             | 2 | a_1 B
            6 => (2, symbols![nt 2, t 1]),                        //  6: a_2 -> ε             | ◄6                  | 2 | a_1 B
            7 => (2, symbols![nt 2, t 3]),                        //  7: a_3 -> a_1           | ●a_1 ◄7             | 2 | a_1 E
            8 => (2, symbols![nt 2, t 3]),                        //  8: a_3 -> ε             | ◄8                  | 2 | a_1 E
            9 => (6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]),  //  9: a_4 -> a_1           | ●a_1 ◄9             | 6 | a_1 b C b B C
            10 => (6, symbols![nt 2, nt 1, t 2, nt 1, t 1, t 2]), // 10: a_4 -> ε             | ◄10                 | 6 | a_1 b C b B C
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),

        // a -> (A | A B | C)*
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* | parent_left_fact (33)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (154, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
            1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
            2 => (2, symbols![nt 1, t 2]),          //  2: a_1 -> C a_1 | ●a_1 ◄2 C! | 2 | a_1 C
            3 => (1, symbols![nt 1]),               //  3: a_1 -> ε     | ◄3         | 1 | a_1
            4 => (3, symbols![nt 1, t 0, t 1]),     //  4: a_2 -> B a_1 | ●a_1 ◄4 B! | 3 | a_1 A B
            5 => (2, symbols![nt 1, t 0]),          //  5: a_2 -> a_1   | ●a_1 ◄5    | 2 | a_1 A
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (A | A B | C)+
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: parent_left_fact | child_left_fact (96)
        //  - a_3: child_left_fact (64)
        //  - a_4: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        //  - a_3 -> a_1
        //  - a_4 -> a_2
        (155, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
            1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
            2 => (0, symbols![]),                   //  2: a_1 -> C a_3 | ►a_3 C! | 0 |
            3 => (0, symbols![]),                   //  3: a_2 -> B a_4 | ►a_4 B! | 0 |
            4 => (2, symbols![nt 1, t 0]),          //  4: a_2 -> a_1   | ●a_1 ◄4 | 2 | a_1 A
            5 => (2, symbols![nt 1, t 0]),          //  5: a_2 -> ε     | ◄5      | 2 | a_1 A
            6 => (2, symbols![nt 1, t 2]),          //  6: a_3 -> a_1   | ●a_1 ◄6 | 2 | a_1 C
            7 => (2, symbols![nt 1, t 2]),          //  7: a_3 -> ε     | ◄7      | 2 | a_1 C
            8 => (3, symbols![nt 1, t 0, t 1]),     //  8: a_4 -> a_1   | ●a_1 ◄8 | 3 | a_1 A B
            9 => (3, symbols![nt 1, t 0, t 1]),     //  9: a_4 -> ε     | ◄9      | 3 | a_1 A B
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A ((B C | D)* E | F)* G
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* (1)
        //  - a_2: child_+_or_* | parent_+_or_* (2049)
        // parents:
        //  - a_1 -> a_2
        //  - a_2 -> a
        (156, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 2, t 6]),     //  0: a -> A a_2 G     | ◄0 G! ►a_2 A!   | 3 | A a_2 G
            1 => (3, symbols![nt 1, t 1, t 2]),     //  1: a_1 -> B C a_1   | ●a_1 ◄1 C! B!   | 3 | a_1 B C
            2 => (2, symbols![nt 1, t 3]),          //  2: a_1 -> D a_1     | ●a_1 ◄2 D!      | 2 | a_1 D
            3 => (1, symbols![nt 1]),               //  3: a_1 -> ε         | ◄3              | 1 | a_1
            4 => (3, symbols![nt 2, nt 1, t 4]),    //  4: a_2 -> a_1 E a_2 | ●a_2 ◄4 E! ►a_1 | 3 | a_2 a_1 E
            5 => (2, symbols![nt 2, t 5]),          //  5: a_2 -> F a_2     | ●a_2 ◄5 F!      | 2 | a_2 F
            6 => (1, symbols![nt 2]),               //  6: a_2 -> ε         | ◄6              | 1 | a_2
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A ((B C | D)+ E | F)+ G
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
        //  - a_3: child_left_fact (64)
        //  - a_4: child_left_fact (64)
        //  - a_5: child_left_fact (64)
        //  - a_6: child_left_fact (64)
        // parents:
        //  - a_1 -> a_2
        //  - a_2 -> a
        //  - a_3 -> a_1
        //  - a_4 -> a_1
        //  - a_5 -> a_2
        //  - a_6 -> a_2
        (157, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 2, t 6]),     //  0: a -> A a_2 G     | ◄0 G! ►a_2 A! | 3 | A a_2 G
            1 => (0, symbols![]),                   //  1: a_1 -> B C a_3   | ►a_3 C! B!    | 0 |
            2 => (0, symbols![]),                   //  2: a_1 -> D a_4     | ►a_4 D!       | 0 |
            3 => (0, symbols![]),                   //  3: a_2 -> F a_5     | ►a_5 F!       | 0 |
            4 => (0, symbols![]),                   //  4: a_2 -> a_1 E a_6 | ►a_6 E! ►a_1  | 0 |
            5 => (3, symbols![nt 1, t 1, t 2]),     //  5: a_3 -> a_1       | ●a_1 ◄5       | 3 | a_1 B C
            6 => (3, symbols![nt 1, t 1, t 2]),     //  6: a_3 -> ε         | ◄6            | 3 | a_1 B C
            7 => (2, symbols![nt 1, t 3]),          //  7: a_4 -> a_1       | ●a_1 ◄7       | 2 | a_1 D
            8 => (2, symbols![nt 1, t 3]),          //  8: a_4 -> ε         | ◄8            | 2 | a_1 D
            9 => (2, symbols![nt 2, t 5]),          //  9: a_5 -> a_2       | ●a_2 ◄9       | 2 | a_2 F
            10 => (2, symbols![nt 2, t 5]),         // 10: a_5 -> ε         | ◄10           | 2 | a_2 F
            11 => (3, symbols![nt 2, nt 1, t 4]),   // 11: a_6 -> a_2       | ●a_2 ◄11      | 3 | a_2 a_1 E
            12 => (3, symbols![nt 2, nt 1, t 4]),   // 12: a_6 -> ε         | ◄12           | 3 | a_2 a_1 E
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- +_or_* <L>
        // a -> A (<L=i> B)* C
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | L-form (129)
        // parents:
        //  - i -> a
        (200, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynI".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
            1 => (2, symbols![nt 1, t 1]),          //  1: i -> B i   | ●i ◄1 B!    | 2 | i B
            2 => (1, symbols![nt 1]),               //  2: i -> ε     | ◄2          | 1 | i
        ], NTValue::Default, btreemap![0 => vec![0]]),
        (200, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, t 2]),           //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
            1 => (2, symbols![t 1]),                //  1: i -> B i   | ●i ◄1 B!    | 2 | B
            2 => (1, symbols![]),                   //  2: i -> ε     | ◄2          | 1 |
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> A (<L=i> B)+ C
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - i_1: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - i_1 -> i
        (201, true, false, true, 0, btreemap![
            0 => "SynMyA".to_string(),
            1 => "SynMyI".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
            1 => (0, symbols![]),                   //  1: i -> B i_1 | ►i_1 B!     | 0 |
            2 => (2, symbols![nt 1, t 1]),          //  2: i_1 -> i   | ●i ◄2       | 2 | i B
            3 => (2, symbols![nt 1, t 1]),          //  3: i_1 -> ε   | ◄3          | 2 | i B
        ], NTValue::Default, btreemap![0 => vec![0]]),
        (201, true, false, true, 0, btreemap![
            0 => "SynMyA".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, t 2]),           //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
            1 => (0, symbols![]),                   //  1: i -> B i_1 | ►i_1 B!     | 0 |
            2 => (2, symbols![t 1]),                //  2: i_1 -> i   | ●i ◄2       | 2 | B
            3 => (2, symbols![t 1]),                //  3: i_1 -> ε   | ◄3          | 2 | B
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),
        (201, true, false, true, 0, btreemap![
            0 => "SynMyA".to_string(),
            1 => "SynMyI".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 2]),     //  0: a -> A i C | ◄0 C! ►i A! | 3 | A i C
            1 => (0, symbols![]),                   //  1: i -> B i_1 | ►i_1 B!     | 0 |
            2 => (2, symbols![nt 1, t 1]),          //  2: i_1 -> i   | ●i ◄2       | 2 | i B
            3 => (2, symbols![nt 1, t 1]),          //  3: i_1 -> ε   | ◄3          | 2 | i B
        ], NTValue::SetIds(vec![1]), btreemap![0 => vec![0]]),

        // a -> (<L=i> b A b B A)*
        // b -> C
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | L-form (129)
        // parents:
        //  - i -> a
        (202, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),                            //  0: a -> i           | ◄0 ►i                | 1 | i
            1 => (6, symbols![nt 1, nt 2, t 0, nt 2, t 1, t 0]), //  1: i -> b A b B A i | ●i ◄1 A! B! ►b A! ►b | 6 | i b A b B A
            2 => (1, symbols![nt 1]),                            //  2: i -> ε           | ◄2                   | 1 | i
            3 => (1, symbols![t 2]),                             //  3: b -> C           | ◄3 C!                | 1 | C
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> (A (<L=j> B ",")* ";")* C
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - j: child_+_or_* | L-form (129)
        //  - a_1: child_+_or_* | parent_+_or_* (2049)
        // parents:
        //  - j -> a_1
        //  - a_1 -> a
        (206, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynAiter".to_string(),
            2 => "SynA1".to_string(),
        ], btreemap![
            0 => (2, symbols![nt 2, t 4]),          //  0: a -> a_1 C         | ◄0 C! ►a_1        | 2 | a_1 C
            1 => (3, symbols![nt 1, t 1]),          //  1: j -> B "," j       | ●j ◄1 "," B!      | 3 | j B
            2 => (1, symbols![nt 1]),               //  2: j -> ε             | ◄2                | 1 | j
            3 => (4, symbols![nt 2, t 0, nt 1]),    //  3: a_1 -> A j ";" a_1 | ●a_1 ◄3 ";" ►j A! | 4 | a_1 A j
            4 => (1, symbols![nt 2]),               //  4: a_1 -> ε           | ◄4                | 1 | a_1
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A (<L=j> b ",")* ";")* C
        // b -> B
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | L-form | parent_+_or_* (2177)
        //  - j: child_+_or_* | L-form (129)
        // parents:
        //  - i -> a
        //  - j -> i
        //
        // 1) All nonterminals have a value:
        (208, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 1, t 3]),          //  0: a -> i C       | ◄0 C! ►i        | 2 | i C
            1 => (4, symbols![nt 1, t 0, nt 2]),    //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | i A j
            2 => (1, symbols![nt 1]),               //  2: i -> ε         | ◄2              | 1 | i
            3 => (3, symbols![nt 2, nt 3]),         //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 | j b
            4 => (1, symbols![nt 2]),               //  4: j -> ε         | ◄4              | 1 | j
            5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
        ], NTValue::Default, btreemap![0 => vec![0], 3 => vec![5]]),
        //
        // 2) Here, 'i' needs to be in the list of valued nonterminals, or it'll generate the same
        // code as the 3rd example:
        (208, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 1, t 3]),          //  0: a -> i C       | ◄0 C! ►i        | 2 | i C
            1 => (4, symbols![nt 1, t 0]),          //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | i A
            2 => (1, symbols![nt 1]),               //  2: i -> ε         | ◄2              | 1 | i
            3 => (3, symbols![]),                   //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
            4 => (1, symbols![]),                   //  4: j -> ε         | ◄4              | 1 |
            5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
        ], NTValue::SetIds(vec![0, 1]), btreemap![0 => vec![0], 3 => vec![5]]),
        //
        // 3) Only 'a' has a value, the other exit (exit_i, exit_j, exit_b) don't return any value:
        (208, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![t 3]),                //  0: a -> i C       | ◄0 C! ►i        | 2 | C
            1 => (4, symbols![t 0]),                //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | A
            2 => (1, symbols![]),                   //  2: i -> ε         | ◄2              | 1 |
            3 => (3, symbols![]),                   //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
            4 => (1, symbols![]),                   //  4: j -> ε         | ◄4              | 1 |
            5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0], 3 => vec![5]]),
        //
        // 4) Same items, but 'a' doesn't have any value, so there's no SynA nor any value for 'a'
        // on the stack:
        (208, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![t 3]),                //  0: a -> i C       | ◄0 C! ►i        | 2 | C
            1 => (4, symbols![t 0]),                //  1: i -> A j ";" i | ●i ◄1 ";" ►j A! | 4 | A
            2 => (1, symbols![]),                   //  2: i -> ε         | ◄2              | 1 |
            3 => (3, symbols![]),                   //  3: j -> b "," j   | ●j ◄3 "," ►b    | 3 |
            4 => (1, symbols![]),                   //  4: j -> ε         | ◄4              | 1 |
            5 => (1, symbols![t 4]),                //  5: b -> B         | ◄5 B!           | 1 | B
        ], NTValue::None, btreemap![0 => vec![0], 3 => vec![5]]),

        // a -> A (<L=i> "B")* C
        (210, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], btreemap![
            0 => (3, symbols![t 0, t 2]),           //  0: a -> A i C | ◄0 C! ►i A! | 3 | A C
            1 => (2, symbols![]),                   //  1: i -> "B" i | ●i ◄1 "B"   | 2 |
            2 => (1, symbols![]),                   //  2: i -> ε     | ◄2          | 1 |
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),

        // a -> A A (B <L=i>)* C | A C (B <L=i>)* C
        // NT flags:
        //  - a: parent_left_fact | parent_+_or_* (2080)
        //  - i: child_+_or_* | L-form (129)
        //  - a_1: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - a_1 -> a
        (211, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynI".to_string(),
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> A a_1   | ►a_1 A!     | 0 |
            1 => (2, symbols![nt 1, t 2]),           //  1: i -> B i     | ●i ◄1 B!    | 2 | i B
            2 => (1, symbols![nt 1]),                //  2: i -> ε       | ◄2          | 1 | i
            3 => (4, symbols![t 0, t 0, nt 1, t 1]), //  3: a_1 -> A i C | ◄3 C! ►i A! | 4 | A A i C
            4 => (4, symbols![t 0, t 1, nt 1, t 1]), //  4: a_1 -> C i C | ◄4 C! ►i C! | 4 | A C i C
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> Id "(" Id ":" type (<L=i> "<" ">" Id ":" type)* ")"
        // type -> Id
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | L-form | sep_list (32897)
        // parents:
        //  - i -> a
        (212, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (4, symbols![t 0, nt 1]),          //  0: a -> Id "(" Id ":" type i ")" | ◄0 ")" ►i ►type ":" Id! "(" Id! | 4    | Id i
            1 => (6, symbols![nt 1, t 0, nt 2]),    //  1: i -> "<" ">" Id ":" type i    | ●i ◄1 ►type ":" Id! ">" "<"     | 6, 3 | i Id type
            2 => (1, symbols![nt 1]),               //  2: i -> ε                        | ◄2                              | 1    | i
            3 => (1, symbols![t 0]),                //  3: type -> Id                    | ◄3 Id!                          | 1    | Id
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> Id "(" Id ":" type (<L=i> "," Id ":" type)* ")" | Id "(" ")"
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
        ], btreemap![
            0 => (0, symbols![]),                   //  0: a -> Id "(" a_1          | ►a_1 "(" Id!            | 0    |
            1 => (5, symbols![nt 1, t 0, nt 2]),    //  1: i -> "," Id ":" type i   | ●i ◄1 ►type ":" Id! "," | 5, 3 | i Id type
            2 => (1, symbols![nt 1]),               //  2: i -> ε                   | ◄2                      | 1    | i
            3 => (1, symbols![t 0]),                //  3: type -> Id               | ◄3 Id!                  | 1    | Id
            4 => (4, symbols![t 0, nt 1]),          //  4: a_1 -> Id ":" type i ")" | ◄4 ")" ►i ►type ":" Id! | 4    | Id i
            5 => (3, symbols![t 0]),                //  5: a_1 -> ")"               | ◄5 ")"                  | 3    | Id
        ], NTValue::Default, btreemap![0 => vec![4, 5], 2 => vec![3]]),

        // a -> Id "(" Id (<L=i> "," Id)* "/" Id (<L=j> "," Id)* ")"
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // |   2 | . j  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (214, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (6, symbols![t 0, nt 1, nt 2]),    //  0: a -> Id "(" Id i "/" Id j ")" | ◄0 ")" ►j Id! "/" ►i Id! "(" Id! | 6    | Id i j
            1 => (3, symbols![nt 1, t 0]),          //  1: i -> "," Id i                 | ●i ◄1 Id! ","                    | 3, 1 | i Id
            2 => (1, symbols![nt 1]),               //  2: i -> ε                        | ◄2                               | 1    | i
            3 => (3, symbols![nt 2, t 0]),          //  3: j -> "," Id j                 | ●j ◄3 Id! ","                    | 3, 1 | j Id
            4 => (1, symbols![nt 2]),               //  4: j -> ε                        | ◄4                               | 1    | j
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> Id "(" Id ":" type (<L=i> "," Id ":" type)+ ")"
        // type -> Id
        //
        //   NT    name     val   flags
        // +-------------------------------------------------------------------+
        // |   0 | a       | y  | parent_+_or_*, plus                          |
        // |   1 | . i     | y  | child_+_or_*, parent_left_fact, L-form, plus |
        // |   3 | .   i_1 |    | child_left_fact                              |
        // |   2 | type    | y  |                                              |
        // +-------------------------------------------------------------------+
        (215, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (7, symbols![t 0, t 0, nt 2, nt 1]), //  0: a -> Id "(" Id ":" type i ")" | ◄0 ")" ►i ►type ":" Id! "(" Id! | 7 | Id Id type i
            1 => (0, symbols![]),                     //  1: i -> "," Id ":" type i_1      | ►i_1 ►type ":" Id! ","          | 0 |
            2 => (1, symbols![t 0]),                  //  2: type -> Id                    | ◄2 Id!                          | 1 | Id
            3 => (5, symbols![nt 1, t 0, nt 2]),      //  3: i_1 -> i                      | ●i ◄3                           | 5 | i Id type
            4 => (5, symbols![nt 1, t 0, nt 2]),      //  4: i_1 -> ε                      | ◄4                              | 5 | i Id type
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![2]]),

        // a -> X B (<L=i> "," B)* B? Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form, sep_list  |
        // |   2 | . a_1 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (216, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> X B i a_1 | ►a_1 ►i B! X! | 0    |
            1 => (3, symbols![nt 1, t 1]),           //  1: i -> "," B i   | ●i ◄1 B! ","  | 3, 1 | i B
            2 => (1, symbols![nt 1]),                //  2: i -> ε         | ◄2            | 1    | i
            3 => (4, symbols![t 0, nt 1, t 1, t 3]), //  3: a_1 -> B Z     | ◄3 Z! B!      | 4    | X i B Z
            4 => (3, symbols![t 0, nt 1, t 3]),      //  4: a_1 -> Z       | ◄4 Z!         | 3    | X i Z
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X? B (<L=i> "," B)* Z
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (217, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 3]),     //  0: a -> X B i Z | ◄0 Z! ►i B! X! | 3    | X i Z
            1 => (2, symbols![nt 1, t 3]),          //  1: a -> B i Z   | ◄1 Z! ►i B!    | 2    | i Z
            2 => (3, symbols![nt 1, t 1]),          //  2: i -> "," B i | ●i ◄2 B! ","   | 3, 1 | i B
            3 => (1, symbols![nt 1]),               //  3: i -> ε       | ◄3             | 1    | i
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> X Y? B (<L=i> "," B)* Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form, sep_list  |
        // |   2 | . a_1 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (218, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> X a_1     | ►a_1 X!        | 0    |
            1 => (3, symbols![nt 1, t 2]),           //  1: i -> "," B i   | ●i ◄1 B! ","   | 3, 1 | i B
            2 => (1, symbols![nt 1]),                //  2: i -> ε         | ◄2             | 1    | i
            3 => (4, symbols![t 0, t 1, nt 1, t 4]), //  3: a_1 -> Y B i Z | ◄3 Z! ►i B! Y! | 4    | X Y i Z
            4 => (3, symbols![t 0, nt 1, t 4]),      //  4: a_1 -> B i Z   | ◄4 Z! ►i B!    | 3    | X i Z
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> X B? (<L=i> "," B)* Z
        //
        //   NT    name   val   flags
        // +----------------------------------------------------+
        // |   0 | a     | y  | parent_left_fact, parent_+_or_* |
        // |   1 | . i   | y  | child_+_or_*, L-form            |
        // |   2 | . a_1 |    | child_left_fact                 |
        // +----------------------------------------------------+
        (219, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                    //  0: a -> X a_1   | ►a_1 X!      | 0 |
            1 => (3, symbols![nt 1, t 1]),           //  1: i -> "," B i | ●i ◄1 B! "," | 3 | i B
            2 => (1, symbols![nt 1]),                //  2: i -> ε       | ◄2           | 1 | i
            3 => (4, symbols![t 0, t 1, nt 1, t 3]), //  3: a_1 -> B i Z | ◄3 Z! ►i B!  | 4 | X B i Z
            4 => (3, symbols![t 0, nt 1, t 3]),      //  4: a_1 -> i Z   | ◄4 Z! ►i     | 3 | X i Z
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> "var" Id (<L=i> "," Id)* ";"
        //
        //   NT    name  val   flags
        // +--------------------------------------------------+
        // |   0 | a    | y  | parent_+_or_*                  |
        // |   1 | . i  | y  | child_+_or_*, L-form, sep_list |
        // +--------------------------------------------------+
        (220, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![nt 1]),               //  0: a -> "var" Id i ";" | ◄0 ";" ►i Id! "var" | 3    | i
            1 => (3, symbols![nt 1, t 1]),          //  1: i -> "," Id i       | ●i ◄1 Id! ","       | 3, 1 | i Id
            2 => (1, symbols![nt 1]),               //  2: i -> ε              | ◄2                  | 1    | i
        ], NTValue::Default, btreemap![0 => vec![0]]),
        (220, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![]),                   //  0: a -> "var" Id i ";" | ◄0 ";" ►i Id! "var" | 3    |
            1 => (3, symbols![t 1]),                //  1: i -> "," Id i       | ●i ◄1 Id! ","       | 3, 1 | Id
            2 => (1, symbols![]),                   //  2: i -> ε              | ◄2                  | 1    |
        ], NTValue::SetIds(vec![0]), btreemap![0 => vec![0]]),
        (220, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![nt 1]),               //  0: a -> "var" Id i ";" | ◄0 ";" ►i Id! "var" | 3    | i
            1 => (3, symbols![nt 1, t 1]),          //  1: i -> "," Id i       | ●i ◄1 Id! ","       | 3, 1 | i Id
            2 => (1, symbols![nt 1]),               //  2: i -> ε              | ◄2                  | 1    | i
        ], NTValue::SetIds(vec![1]), btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- norm+/* <L> alternatives
        // a -> (<L=i> A | B)*
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | L-form (129)
        // parents:
        //  - i -> a
        (250, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> i   | ◄0 ►i    | 1 | i
            1 => (2, symbols![nt 1, t 0]),          //  1: i -> A i | ●i ◄1 A! | 2 | i A
            2 => (2, symbols![nt 1, t 1]),          //  2: i -> B i | ●i ◄2 B! | 2 | i B
            3 => (1, symbols![nt 1]),               //  3: i -> ε   | ◄3       | 1 | i
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | B)+
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - i_1: child_left_fact (64)
        //  - i_2: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - i_1 -> i
        //  - i_2 -> i
        (251, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> i     | ◄0 ►i   | 1 | i
            1 => (0, symbols![]),                   //  1: i -> A i_1 | ►i_1 A! | 0 |
            2 => (0, symbols![]),                   //  2: i -> B i_2 | ►i_2 B! | 0 |
            3 => (2, symbols![nt 1, t 0]),          //  3: i_1 -> i   | ●i ◄3   | 2 | i A
            4 => (2, symbols![nt 1, t 0]),          //  4: i_1 -> ε   | ◄4      | 2 | i A
            5 => (2, symbols![nt 1, t 1]),          //  5: i_2 -> i   | ●i ◄5   | 2 | i B
            6 => (2, symbols![nt 1, t 1]),          //  6: i_2 -> ε   | ◄6      | 2 | i B
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> A ((<L=j> b C b B C | D)+ E | F)+ G
        // b -> H
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - j: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - a_1: child_+_or_* | parent_left_fact | parent_+_or_* | plus (6177)
        //  - j_1: child_left_fact (64)
        //  - j_2: child_left_fact (64)
        //  - a_2: child_left_fact (64)
        //  - a_3: child_left_fact (64)
        // parents:
        //  - j -> a_1
        //  - a_1 -> a
        //  - j_1 -> j
        //  - j_2 -> j
        //  - a_2 -> a_1
        //  - a_3 -> a_1
        (252, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 3, t 6]),                  //  0: a -> A a_1 G       | ◄0 G! ►a_1 A!       | 3 | A a_1 G
            1 => (0, symbols![]),                                //  1: j -> D j_1         | ►j_1 D!             | 0 |
            2 => (0, symbols![]),                                //  2: j -> b C b B C j_2 | ►j_2 C! B! ►b C! ►b | 0 |
            3 => (1, symbols![t 7]),                             //  3: b -> H             | ◄3 H!               | 1 | H
            4 => (0, symbols![]),                                //  4: a_1 -> F a_2       | ►a_2 F!             | 0 |
            5 => (0, symbols![]),                                //  5: a_1 -> j E a_3     | ►a_3 E! ►j          | 0 |
            6 => (2, symbols![nt 1, t 3]),                       //  6: j_1 -> j           | ●j ◄6               | 2 | j D
            7 => (2, symbols![nt 1, t 3]),                       //  7: j_1 -> ε           | ◄7                  | 2 | j D
            8 => (6, symbols![nt 1, nt 2, t 1, nt 2, t 2, t 1]), //  8: j_2 -> j           | ●j ◄8               | 6 | j b C b B C
            9 => (6, symbols![nt 1, nt 2, t 1, nt 2, t 2, t 1]), //  9: j_2 -> ε           | ◄9                  | 6 | j b C b B C
            10 => (2, symbols![nt 3, t 5]),                      // 10: a_2 -> a_1         | ●a_1 ◄10            | 2 | a_1 F
            11 => (2, symbols![nt 3, t 5]),                      // 11: a_2 -> ε           | ◄11                 | 2 | a_1 F
            12 => (3, symbols![nt 3, nt 1, t 4]),                // 12: a_3 -> a_1         | ●a_1 ◄12            | 3 | a_1 j E
            13 => (3, symbols![nt 3, nt 1, t 4]),                // 13: a_3 -> ε           | ◄13                 | 3 | a_1 j E
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> A (<L=i> (b C b B C | D)+ E | F)+ G
        // b -> H
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - i: child_+_or_* | parent_left_fact | L-form | parent_+_or_* | plus (6305)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - i_1: child_left_fact (64)
        //  - i_2: child_left_fact (64)
        //  - a_2: child_left_fact (64)
        //  - a_3: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - a_1 -> i
        //  - i_1 -> i
        //  - i_2 -> i
        //  - a_2 -> a_1
        //  - a_3 -> a_1
        (253, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 6]),                   //  0: a -> A i G           | ◄0 G! ►i A!         | 3 | A i G
            1 => (0, symbols![]),                                 //  1: i -> F i_1           | ►i_1 F!             | 0 |
            2 => (0, symbols![]),                                 //  2: i -> a_1 E i_2       | ►i_2 E! ►a_1        | 0 |
            3 => (1, symbols![t 7]),                              //  3: b -> H               | ◄3 H!               | 1 | H
            4 => (0, symbols![]),                                 //  4: a_1 -> D a_2         | ►a_2 D!             | 0 |
            5 => (0, symbols![]),                                 //  5: a_1 -> b C b B C a_3 | ►a_3 C! B! ►b C! ►b | 0 |
            6 => (2, symbols![nt 1, t 5]),                        //  6: i_1 -> i             | ●i ◄6               | 2 | i F
            7 => (2, symbols![nt 1, t 5]),                        //  7: i_1 -> ε             | ◄7                  | 2 | i F
            8 => (3, symbols![nt 1, nt 3, t 4]),                  //  8: i_2 -> i             | ●i ◄8               | 3 | i a_1 E
            9 => (3, symbols![nt 1, nt 3, t 4]),                  //  9: i_2 -> ε             | ◄9                  | 3 | i a_1 E
            10 => (2, symbols![nt 3, t 3]),                       // 10: a_2 -> a_1           | ●a_1 ◄10            | 2 | a_1 D
            11 => (2, symbols![nt 3, t 3]),                       // 11: a_2 -> ε             | ◄11                 | 2 | a_1 D
            12 => (6, symbols![nt 3, nt 2, t 1, nt 2, t 2, t 1]), // 12: a_3 -> a_1           | ●a_1 ◄12            | 6 | a_1 b C b B C
            13 => (6, symbols![nt 3, nt 2, t 1, nt 2, t 2, t 1]), // 13: a_3 -> ε             | ◄13                 | 6 | a_1 b C b B C
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> A (<L=i> (<L=j> b C b B C | D)* E | F)* G
        // b -> H
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | L-form | parent_+_or_* (2177)
        //  - j: child_+_or_* | L-form (129)
        // parents:
        //  - i -> a
        //  - j -> i
        (254, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 6]),                  //  0: a -> A i G       | ◄0 G! ►i A!          | 3 | A i G
            1 => (3, symbols![nt 1, nt 2, t 4]),                 //  1: i -> j E i       | ●i ◄1 E! ►j          | 3 | i j E
            2 => (2, symbols![nt 1, t 5]),                       //  2: i -> F i         | ●i ◄2 F!             | 2 | i F
            3 => (1, symbols![nt 1]),                            //  3: i -> ε           | ◄3                   | 1 | i
            4 => (6, symbols![nt 2, nt 3, t 1, nt 3, t 2, t 1]), //  4: j -> b C b B C j | ●j ◄4 C! B! ►b C! ►b | 6 | j b C b B C
            5 => (2, symbols![nt 2, t 3]),                       //  5: j -> D j         | ●j ◄5 D!             | 2 | j D
            6 => (1, symbols![nt 2]),                            //  6: j -> ε           | ◄6                   | 1 | j
            7 => (1, symbols![t 7]),                             //  7: b -> H           | ◄7 H!                | 1 | H
        ], NTValue::Default, btreemap![0 => vec![0], 3 => vec![7]]),

        // a -> A (<L=i> B A | B A C b | D)+ E
        // b -> F
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - i_1: parent_left_fact | child_left_fact (96)
        //  - i_2: child_left_fact (64)
        //  - a_1: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - i_1 -> i
        //  - i_2 -> i
        //  - a_1 -> i_1
        (256, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 1, t 4]),             //  0: a -> A i E     | ◄0 E! ►i A! | 3 | A i E
            1 => (0, symbols![]),                           //  1: i -> B A i_1   | ►i_1 A! B!  | 0 |
            2 => (0, symbols![]),                           //  2: i -> D i_2     | ►i_2 D!     | 0 |
            3 => (1, symbols![t 5]),                        //  3: b -> F         | ◄3 F!       | 1 | F
            4 => (0, symbols![]),                           //  4: i_1 -> C b a_1 | ►a_1 ►b C!  | 0 |
            5 => (3, symbols![nt 1, t 1, t 0]),             //  5: i_1 -> i       | ●i ◄5       | 3 | i B A
            6 => (3, symbols![nt 1, t 1, t 0]),             //  6: i_1 -> ε       | ◄6          | 3 | i B A
            7 => (2, symbols![nt 1, t 3]),                  //  7: i_2 -> i       | ●i ◄7       | 2 | i D
            8 => (2, symbols![nt 1, t 3]),                  //  8: i_2 -> ε       | ◄8          | 2 | i D
            9 => (5, symbols![nt 1, t 1, t 0, t 2, nt 2]),  //  9: a_1 -> i       | ●i ◄9       | 5 | i B A C b
            10 => (5, symbols![nt 1, t 1, t 0, t 2, nt 2]), // 10: a_1 -> ε       | ◄10         | 5 | i B A C b
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3]]),

        // a -> (<L=i> A | A B A | C)+
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - i: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - a_1: parent_left_fact | child_left_fact (96)
        //  - a_2: child_left_fact (64)
        //  - a_3: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - a_1 -> i
        //  - a_2 -> i
        //  - a_3 -> a_1
        (257, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),                //  0: a -> i         | ◄0 ►i      | 1 | i
            1 => (0, symbols![]),                    //  1: i -> A a_1     | ►a_1 A!    | 0 |
            2 => (0, symbols![]),                    //  2: i -> C a_2     | ►a_2 C!    | 0 |
            3 => (0, symbols![]),                    //  3: a_1 -> B A a_3 | ►a_3 A! B! | 0 |
            4 => (2, symbols![nt 1, t 0]),           //  4: a_1 -> i       | ●i ◄4      | 2 | i A
            5 => (2, symbols![nt 1, t 0]),           //  5: a_1 -> ε       | ◄5         | 2 | i A
            6 => (2, symbols![nt 1, t 2]),           //  6: a_2 -> i       | ●i ◄6      | 2 | i C
            7 => (2, symbols![nt 1, t 2]),           //  7: a_2 -> ε       | ◄7         | 2 | i C
            8 => (4, symbols![nt 1, t 0, t 1, t 0]), //  8: a_3 -> i       | ●i ◄8      | 4 | i A B A
            9 => (4, symbols![nt 1, t 0, t 1, t 0]), //  9: a_3 -> ε       | ◄9         | 4 | i A B A
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | A B | C | D (<L=j> E | E F | G)*)*
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - i: child_+_or_* | parent_left_fact | L-form | parent_+_or_* (2209)
        //  - j: child_+_or_* | parent_left_fact | L-form (161)
        //  - i_1: child_left_fact (64)
        //  - j_1: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - j -> i
        //  - i_1 -> i
        //  - j_1 -> j
        (258, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> i     | ◄0 ►i       | 1 | i
            1 => (0, symbols![]),                   //  1: i -> A i_1 | ►i_1 A!     | 0 |
            2 => (2, symbols![nt 1, t 2]),          //  2: i -> C i   | ●i ◄2 C!    | 2 | i C
            3 => (3, symbols![nt 1, t 3, nt 2]),    //  3: i -> D j i | ●i ◄3 ►j D! | 3 | i D j
            4 => (1, symbols![nt 1]),               //  4: i -> ε     | ◄4          | 1 | i
            5 => (0, symbols![]),                   //  5: j -> E j_1 | ►j_1 E!     | 0 |
            6 => (2, symbols![nt 2, t 6]),          //  6: j -> G j   | ●j ◄6 G!    | 2 | j G
            7 => (1, symbols![nt 2]),               //  7: j -> ε     | ◄7          | 1 | j
            8 => (3, symbols![nt 1, t 0, t 1]),     //  8: i_1 -> B i | ●i ◄8 B!    | 3 | i A B
            9 => (2, symbols![nt 1, t 0]),          //  9: i_1 -> i   | ●i ◄9       | 2 | i A
            10 => (3, symbols![nt 2, t 4, t 5]),    // 10: j_1 -> F j | ●j ◄10 F!   | 3 | j E F
            11 => (2, symbols![nt 2, t 4]),         // 11: j_1 -> j   | ●j ◄11      | 2 | j E
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> (<L=i> A | A B | C | D (<L=j> E | E F | G)+)+
        // NT flags:
        //  - a: parent_+_or_* | plus (6144)
        //  - i: child_+_or_* | parent_left_fact | L-form | parent_+_or_* | plus (6305)
        //  - j: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - i_1: parent_left_fact | child_left_fact (96)
        //  - i_2: child_left_fact (64)
        //  - i_3: child_left_fact (64)
        //  - j_1: parent_left_fact | child_left_fact (96)
        //  - j_2: child_left_fact (64)
        //  - a_1: child_left_fact (64)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - i -> a
        //  - j -> i
        //  - i_1 -> i
        //  - i_2 -> i
        //  - i_3 -> i
        //  - j_1 -> j
        //  - j_2 -> j
        //  - a_1 -> i_1
        //  - a_2 -> j_1
        (259, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> i       | ◄0 ►i      | 1 | i
            1 => (0, symbols![]),                   //  1: i -> A i_1   | ►i_1 A!    | 0 |
            2 => (0, symbols![]),                   //  2: i -> C i_2   | ►i_2 C!    | 0 |
            3 => (0, symbols![]),                   //  3: i -> D j i_3 | ►i_3 ►j D! | 0 |
            4 => (0, symbols![]),                   //  4: j -> E j_1   | ►j_1 E!    | 0 |
            5 => (0, symbols![]),                   //  5: j -> G j_2   | ►j_2 G!    | 0 |
            6 => (0, symbols![]),                   //  6: i_1 -> B a_1 | ►a_1 B!    | 0 |
            7 => (2, symbols![nt 1, t 0]),          //  7: i_1 -> i     | ●i ◄7      | 2 | i A
            8 => (2, symbols![nt 1, t 0]),          //  8: i_1 -> ε     | ◄8         | 2 | i A
            9 => (2, symbols![nt 1, t 2]),          //  9: i_2 -> i     | ●i ◄9      | 2 | i C
            10 => (2, symbols![nt 1, t 2]),         // 10: i_2 -> ε     | ◄10        | 2 | i C
            11 => (3, symbols![nt 1, t 3, nt 2]),   // 11: i_3 -> i     | ●i ◄11     | 3 | i D j
            12 => (3, symbols![nt 1, t 3, nt 2]),   // 12: i_3 -> ε     | ◄12        | 3 | i D j
            13 => (0, symbols![]),                  // 13: j_1 -> F a_2 | ►a_2 F!    | 0 |
            14 => (2, symbols![nt 2, t 4]),         // 14: j_1 -> j     | ●j ◄14     | 2 | j E
            15 => (2, symbols![nt 2, t 4]),         // 15: j_1 -> ε     | ◄15        | 2 | j E
            16 => (2, symbols![nt 2, t 6]),         // 16: j_2 -> j     | ●j ◄16     | 2 | j G
            17 => (2, symbols![nt 2, t 6]),         // 17: j_2 -> ε     | ◄17        | 2 | j G
            18 => (3, symbols![nt 1, t 0, t 1]),    // 18: a_1 -> i     | ●i ◄18     | 3 | i A B
            19 => (3, symbols![nt 1, t 0, t 1]),    // 19: a_1 -> ε     | ◄19        | 3 | i A B
            20 => (3, symbols![nt 2, t 4, t 5]),    // 20: a_2 -> j     | ●j ◄20     | 3 | j E F
            21 => (3, symbols![nt 2, t 4, t 5]),    // 21: a_2 -> ε     | ◄21        | 3 | j E F
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- right_rec
        // expr -> Id "." expr | "(" Num ")"
        // NT flags:
        //  - expr: right_rec (2)
        (301, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0, nt 0]),          //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id expr
            1 => (3, symbols![t 3]),                //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),
        (301, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0]),                //  0: expr -> Id "." expr | ◄0 ►expr "." Id! | 3 | Id
            1 => (3, symbols![t 3]),                //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 3 | Num
        ], NTValue::None, btreemap![0 => vec![0, 1]]),

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
        // NT flags:
        //  - expr: right_rec | L-form (130)
        (401, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![nt 0, t 0]),          //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | expr Id
            1 => (4, symbols![nt 0, t 3]),          //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | expr Num
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),
        (401, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![t 0]),                //  0: expr -> Id "." expr | ●expr ◄0 "." Id! | 3 | Id
            1 => (4, symbols![t 3]),                //  1: expr -> "(" Num ")" | ◄1 ")" Num! "("  | 4 | Num
        ], NTValue::None, btreemap![0 => vec![0, 1]]),

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
        // e -> f | e "." Id
        // f -> Id
        // NT flags:
        //  - e: parent_left_rec (512)
        //  - e_1: child_left_rec (4)
        // parents:
        //  - e_1 -> e
        (502, true, false, true, 0, btreemap![
            0 => "SynE".to_string(),
            1 => "SynF".to_string(),
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
            1 => (1, symbols![t 1]),                //  1: f -> Id           | ◄1 Id!          | 1 | Id
            2 => (3, symbols![nt 0, t 1]),          //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | e Id
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε          | ◄3              | 1 | e
        ], NTValue::Default, btreemap![0 => vec![0], 1 => vec![1]]),
        (502, true, false, true, 0, btreemap![
            1 => "SynF".to_string(),
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: e -> f e_1        | ►e_1 ◄0 ►f      | 1 | f
            1 => (1, symbols![t 1]),                //  1: f -> Id           | ◄1 Id!          | 1 | Id
            2 => (3, symbols![t 1]),                //  2: e_1 -> "." Id e_1 | ●e_1 ◄2 Id! "." | 3 | Id
            3 => (1, symbols![]),                   //  3: e_1 -> ε          | ◄3              | 1 |
        ], NTValue::SetIds(vec![1]), btreemap![0 => vec![0], 1 => vec![1]]),


        // --------------------------------------------------------------------------- right_rec + left_rec
        // e -> e "!" | "-" e | Num
        // NT flags:
        //  - e: right_rec | parent_left_rec (514)
        //  - e_1: child_left_rec (4)
        // parents:
        //  - e_1 -> e
        (580, true, false, true, 0, btreemap![
            0 => "SynE".to_string(),
        ], btreemap![
            0 => (2, symbols![nt 0]),               //  0: e -> "-" e     | ◄0 ►e "-"    | 2 | e
            1 => (1, symbols![t 2]),                //  1: e -> Num e_1   | ►e_1 ◄1 Num! | 1 | Num
            2 => (2, symbols![nt 0]),               //  2: e_1 -> "!" e_1 | ●e_1 ◄2 "!"  | 2 | e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε       | ◄3           | 1 | e
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

        // --------------------------------------------------------------------------- left_rec ambig

        // e -> e "+" e | Num
        // NT flags:
        //  - e: parent_left_rec | parent_amb (1536)
        //  - e_1: child_left_rec (4)
        // parents:
        //  - e_1 -> e
        //  - e_2 -> e
        (600, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "+" e_2 e_1 | ●e_1 ◄1 ►e_2 "+" | 3 | e e
            2 => (1, symbols![nt 0]),               //  2: e_1 -> ε           | ◄2               | 1 | e
            3 => (1, symbols![t 1]),                //  3: e_2 -> Num         | ◄3 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // e -> e "*" e | e "+" e | "!" e | Num
        (603, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
            4 => (1, symbols![nt 0]),               //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
            5 => (3, symbols![nt 0, nt 0]),         //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
            6 => (1, symbols![nt 0]),               //  6: e_3 -> ε           | ◄6               | 1 | e
            7 => (2, symbols![nt 0]),               //  7: e_4 -> "!" e       | ◄7 ►e "!"        | 2 | e
            8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | "!" e | e "+" e | Num
        (604, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
            4 => (1, symbols![nt 0]),               //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
            5 => (3, symbols![nt 0, nt 0]),         //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
            6 => (1, symbols![nt 0]),               //  6: e_3 -> ε           | ◄6               | 1 | e
            7 => (2, symbols![nt 0]),               //  7: e_4 -> "!" e_2     | ◄7 ►e_2 "!"      | 2 | e
            8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> "!" e | e "*" e | e "+" e | Num
        (605, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
            4 => (1, symbols![nt 0]),               //  4: e_2 -> e_4 e_3     | ►e_3 ◄4 ►e_4     | 1 | e
            5 => (3, symbols![nt 0, nt 0]),         //  5: e_3 -> "*" e_4 e_3 | ●e_3 ◄5 ►e_4 "*" | 3 | e e
            6 => (1, symbols![nt 0]),               //  6: e_3 -> ε           | ◄6               | 1 | e
            7 => (2, symbols![nt 0]),               //  7: e_4 -> "!" e_4     | ◄7 ►e_4 "!"      | 2 | e
            8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | e "+" e | <R> e "!" e | Num
        (606, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "!" e e_1   | ●e_1 ◄3 ►e "!"   | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            7 => (1, symbols![nt 0]),               //  7: e_3 -> ε           | ◄7               | 1 | e
            8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | <R> e "!" e | e "+" e | Num
        (607, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "!" e_2 e_1 | ●e_1 ◄2 ►e_2 "!" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "!" e_2 e_3 | ●e_3 ◄7 ►e_2 "!" | 3 | e e
            8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
            9 => (1, symbols![t 3]),                //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> <R> e "!" e | e "*" e | e "+" e | Num
        (608, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "!" e_4 e_1 | ●e_1 ◄1 ►e_4 "!"  | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4                | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "!" e_4 e_3 | ●e_3 ◄6 ►e_4 "!"  | 3 | e e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | 3 | e e
            8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8                | 1 | e
            9 => (1, symbols![nt 0]),               //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | 1 | e
            10 => (3, symbols![nt 0, nt 0]),        // 10: e_5 -> "!" e_4 e_5 | ●e_5 ◄10 ►e_4 "!" | 3 | e e
            11 => (1, symbols![nt 0]),              // 11: e_5 -> ε           | ◄11               | 1 | e
            12 => (1, symbols![t 3]),               // 12: e_6 -> Num         | ◄12 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | e "+" e | e "!" | Num
        (609, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            3 => (2, symbols![nt 0]),               //  3: e_1 -> "!" e_1     | ●e_1 ◄3 "!"      | 2 | e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            7 => (1, symbols![nt 0]),               //  7: e_3 -> ε           | ◄7               | 1 | e
            8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | e "!" | e "+" e | Num
        (610, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (2, symbols![nt 0]),               //  2: e_1 -> "!" e_1     | ●e_1 ◄2 "!"      | 2 | e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            7 => (2, symbols![nt 0]),               //  7: e_3 -> "!" e_3     | ●e_3 ◄7 "!"      | 2 | e
            8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
            9 => (1, symbols![t 3]),                //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "!" | e "*" e | e "+" e | Num
        (611, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6     | 1 | e
            1 => (2, symbols![nt 0]),               //  1: e_1 -> "!" e_1     | ●e_1 ◄1 "!"      | 2 | e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6     | 1 | e
            6 => (2, symbols![nt 0]),               //  6: e_3 -> "!" e_3     | ●e_3 ◄6 "!"      | 2 | e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | 3 | e e
            8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
            9 => (1, symbols![nt 0]),               //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6     | 1 | e
            10 => (2, symbols![nt 0]),              // 10: e_5 -> "!" e_5     | ●e_5 ◄10 "!"     | 2 | e
            11 => (1, symbols![nt 0]),              // 11: e_5 -> ε           | ◄11              | 1 | e
            12 => (1, symbols![t 3]),               // 12: e_6 -> Num         | ◄12 Num!         | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "!" e | e "*" e | e "+" e | Num
        (612, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_6 e_1       | ►e_1 ◄0 ►e_6      | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "!" e_6 e_1 | ●e_1 ◄1 ►e_6 "!"  | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "*" e_4 e_1 | ●e_1 ◄2 ►e_4 "*"  | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+"  | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4                | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_6 e_3     | ►e_3 ◄5 ►e_6      | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "!" e_6 e_3 | ●e_3 ◄6 ►e_6 "!"  | 3 | e e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*"  | 3 | e e
            8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8                | 1 | e
            9 => (1, symbols![nt 0]),               //  9: e_4 -> e_6 e_5     | ►e_5 ◄9 ►e_6      | 1 | e
            10 => (3, symbols![nt 0, nt 0]),        // 10: e_5 -> "!" e_6 e_5 | ●e_5 ◄10 ►e_6 "!" | 3 | e e
            11 => (1, symbols![nt 0]),              // 11: e_5 -> ε           | ◄11               | 1 | e
            12 => (1, symbols![t 3]),               // 12: e_6 -> Num         | ◄12 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | e "+" e | <P> e "!" e | Num
        (613, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "+" e_2 e_1 | ●e_1 ◄2 ►e_2 "+" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "!" e_2 e_1 | ●e_1 ◄3 ►e_2 "!" | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            7 => (1, symbols![nt 0]),               //  7: e_3 -> ε           | ◄7               | 1 | e
            8 => (1, symbols![t 3]),                //  8: e_4 -> Num         | ◄8 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | <P> e "!" e | e "+" e | Num
        (614, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "!" e_4 e_1 | ●e_1 ◄2 ►e_4 "!" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            4 => (1, symbols![nt 0]),               //  4: e_1 -> ε           | ◄4               | 1 | e
            5 => (1, symbols![nt 0]),               //  5: e_2 -> e_4 e_3     | ►e_3 ◄5 ►e_4     | 1 | e
            6 => (3, symbols![nt 0, nt 0]),         //  6: e_3 -> "*" e_4 e_3 | ●e_3 ◄6 ►e_4 "*" | 3 | e e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "!" e_4 e_3 | ●e_3 ◄7 ►e_4 "!" | 3 | e e
            8 => (1, symbols![nt 0]),               //  8: e_3 -> ε           | ◄8               | 1 | e
            9 => (1, symbols![t 3]),                //  9: e_4 -> Num         | ◄9 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | e "+" | "!" e | Num
        (630, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            2 => (2, symbols![nt 0]),               //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
            4 => (2, symbols![nt 0]),               //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
            5 => (1, symbols![t 3]),                //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | e "+" | <R> "!" e | Num
        (631, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            2 => (2, symbols![nt 0]),               //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
            4 => (2, symbols![nt 0]),               //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
            5 => (1, symbols![t 3]),                //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),
        // e -> e "*" e | <R> e "+" | "!" e | Num
        (632, true, true, false, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_2 e_1       | ►e_1 ◄0 ►e_2     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            2 => (2, symbols![nt 0]),               //  2: e_1 -> "+" e_1     | ●e_1 ◄2 "+"      | 2 | e
            3 => (1, symbols![nt 0]),               //  3: e_1 -> ε           | ◄3               | 1 | e
            4 => (2, symbols![nt 0]),               //  4: e_2 -> "!" e       | ◄4 ►e "!"        | 2 | e
            5 => (1, symbols![t 3]),                //  5: e_2 -> Num         | ◄5 Num!          | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "-" e | e "*" e | e "/" <P> e | e "+" e | e "-" <P> e | Id
        // NT flags:
        //  - e: parent_left_rec | parent_amb (1536)
        //  - e_1: child_left_rec (4)
        //  - e_2: parent_left_rec (512)
        //  - e_3: child_left_rec (4)
        //  - e_4: right_rec (2)
        // parents:
        //  - e_1 -> e
        //  - e_2 -> e
        //  - e_3 -> e_2
        //  - e_4 -> e
        (640, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_4 e_1 | ●e_1 ◄1 ►e_4 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "/" e_4 e_1 | ●e_1 ◄2 ►e_4 "/" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            4 => (3, symbols![nt 0, nt 0]),         //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | 3 | e e
            5 => (1, symbols![nt 0]),               //  5: e_1 -> ε           | ◄5               | 1 | e
            6 => (1, symbols![nt 0]),               //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_4 e_3 | ●e_3 ◄7 ►e_4 "*" | 3 | e e
            8 => (3, symbols![nt 0, nt 0]),         //  8: e_3 -> "/" e_4 e_3 | ●e_3 ◄8 ►e_4 "/" | 3 | e e
            9 => (1, symbols![nt 0]),               //  9: e_3 -> ε           | ◄9               | 1 | e
            10 => (2, symbols![nt 0]),              // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
            11 => (1, symbols![t 4]),               // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "-" e | <R> e "*" e | <R> e "/" <P> e | <R> e "+" e | <R> e "-" <P> e | Id
        // NT flags:
        //  - e: parent_left_rec | parent_amb (1536)
        //  - e_1: child_left_rec (4)
        //  - e_2: parent_left_rec (512)
        //  - e_3: child_left_rec (4)
        //  - e_4: right_rec (2)
        // parents:
        //  - e_1 -> e
        //  - e_2 -> e
        //  - e_3 -> e_2
        //  - e_4 -> e
        (641, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e e_1   | ●e_1 ◄3 ►e "+"   | 3 | e e
            4 => (3, symbols![nt 0, nt 0]),         //  4: e_1 -> "-" e e_1   | ●e_1 ◄4 ►e "-"   | 3 | e e
            5 => (1, symbols![nt 0]),               //  5: e_1 -> ε           | ◄5               | 1 | e
            6 => (1, symbols![nt 0]),               //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | 3 | e e
            8 => (3, symbols![nt 0, nt 0]),         //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | 3 | e e
            9 => (1, symbols![nt 0]),               //  9: e_3 -> ε           | ◄9               | 1 | e
            10 => (2, symbols![nt 0]),              // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
            11 => (1, symbols![t 4]),               // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // e -> "-" e | <R> e "*" e | <R> e "/" <P> e | e "+" e | e "-" <P> e | Id
        // NT flags:
        //  - e: parent_left_rec | parent_amb (1536)
        //  - e_1: child_left_rec (4)
        //  - e_2: parent_left_rec (512)
        //  - e_3: child_left_rec (4)
        //  - e_4: right_rec (2)
        // parents:
        //  - e_1 -> e
        //  - e_2 -> e
        //  - e_3 -> e_2
        //  - e_4 -> e
        (642, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: e -> e_4 e_1       | ►e_1 ◄0 ►e_4     | 1 | e
            1 => (3, symbols![nt 0, nt 0]),         //  1: e_1 -> "*" e_2 e_1 | ●e_1 ◄1 ►e_2 "*" | 3 | e e
            2 => (3, symbols![nt 0, nt 0]),         //  2: e_1 -> "/" e_2 e_1 | ●e_1 ◄2 ►e_2 "/" | 3 | e e
            3 => (3, symbols![nt 0, nt 0]),         //  3: e_1 -> "+" e_2 e_1 | ●e_1 ◄3 ►e_2 "+" | 3 | e e
            4 => (3, symbols![nt 0, nt 0]),         //  4: e_1 -> "-" e_2 e_1 | ●e_1 ◄4 ►e_2 "-" | 3 | e e
            5 => (1, symbols![nt 0]),               //  5: e_1 -> ε           | ◄5               | 1 | e
            6 => (1, symbols![nt 0]),               //  6: e_2 -> e_4 e_3     | ►e_3 ◄6 ►e_4     | 1 | e
            7 => (3, symbols![nt 0, nt 0]),         //  7: e_3 -> "*" e_2 e_3 | ●e_3 ◄7 ►e_2 "*" | 3 | e e
            8 => (3, symbols![nt 0, nt 0]),         //  8: e_3 -> "/" e_2 e_3 | ●e_3 ◄8 ►e_2 "/" | 3 | e e
            9 => (1, symbols![nt 0]),               //  9: e_3 -> ε           | ◄9               | 1 | e
            10 => (2, symbols![nt 0]),              // 10: e_4 -> "-" e_4     | ◄10 ►e_4 "-"     | 2 | e
            11 => (1, symbols![t 4]),               // 11: e_4 -> Id          | ◄11 Id!          | 1 | Id
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a A a a | B
        // NT flags:
        //  - a: parent_left_rec | parent_amb (1536)
        //  - a_1: child_left_rec (4)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        (650, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),                  //  0: a -> a_2 a_1       | ►a_1 ◄0 ►a_2       | 1 | a
            1 => (4, symbols![nt 0, t 0, nt 0, nt 0]), //  1: a_1 -> A a a_2 a_1 | ●a_1 ◄1 ►a_2 ►a A! | 4 | a A a a
            2 => (1, symbols![nt 0]),                  //  2: a_1 -> ε           | ◄2                 | 1 | a
            3 => (1, symbols![t 1]),                   //  3: a_2 -> B           | ◄3 B!              | 1 | B
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- left_fact
        // a -> A | A B | A B C | A B D | E
        // NT flags:
        //  - a: parent_left_fact (32)
        //  - a_1: parent_left_fact | child_left_fact (96)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (705, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], btreemap![
            0 => (0, symbols![]),                   //  0: a -> A a_1   | ►a_1 A! | 0 |
            1 => (1, symbols![t 4]),                //  1: a -> E       | ◄1 E!   | 1 | E
            2 => (0, symbols![]),                   //  2: a_1 -> B a_2 | ►a_2 B! | 0 |
            3 => (1, symbols![t 0]),                //  3: a_1 -> ε     | ◄3      | 1 | A
            4 => (3, symbols![t 0, t 1, t 2]),      //  4: a_2 -> C     | ◄4 C!   | 3 | A B C
            5 => (3, symbols![t 0, t 1, t 3]),      //  5: a_2 -> D     | ◄5 D!   | 3 | A B D
            6 => (2, symbols![t 0, t 1]),           //  6: a_2 -> ε     | ◄6      | 2 | A B
        ], NTValue::Default, btreemap![0 => vec![1, 3, 4, 5, 6]]),

        // --------------------------------------------------------------------------- combinations

        // --------------------------------------------------------------------------- +_or_* and right_rec
        // a -> A* B a | C
        // NT flags:
        //  - a: right_rec | parent_+_or_* (2050)
        //  - a_1: child_+_or_* (1)
        // parents:
        //  - a_1 -> a
        (810, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![nt 1, t 1, nt 0]),    //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | 3 | a_1 B a
            1 => (1, symbols![t 2]),                //  1: a -> C       | ◄1 C!         | 1 | C
            2 => (2, symbols![nt 1, t 0]),          //  2: a_1 -> A a_1 | ●a_1 ◄2 A!    | 2 | a_1 A
            3 => (1, symbols![nt 1]),               //  3: a_1 -> ε     | ◄3            | 1 | a_1
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

        // a -> A+ B a | C
        // NT flags:
        //  - a: right_rec | parent_+_or_* | plus (6146)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (811, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (3, symbols![nt 1, t 1, nt 0]),    //  0: a -> a_1 B a | ◄0 ►a B! ►a_1 | 3 | a_1 B a
            1 => (1, symbols![t 2]),                //  1: a -> C       | ◄1 C!         | 1 | C
            2 => (0, symbols![]),                   //  2: a_1 -> A a_2 | ►a_2 A!       | 0 |
            3 => (2, symbols![nt 1, t 0]),          //  3: a_2 -> a_1   | ●a_1 ◄3       | 2 | a_1 A
            4 => (2, symbols![nt 1, t 0]),          //  4: a_2 -> ε     | ◄4            | 2 | a_1 A
        ], NTValue::Default, btreemap![0 => vec![0, 1]]),

        // --------------------------------------------------------------------------- +_or_* and left_rec
        // a -> a A* C | B
        // NT flags:
        //  - a: parent_left_rec | parent_+_or_* (2560)
        //  - a_1: child_+_or_* (1)
        //  - a_2: child_left_rec (4)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        (820, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], btreemap![
            0 => (1, symbols![t 2]),                //  0: a -> B a_2       | ►a_2 ◄0 B!      | 1 | B
            1 => (2, symbols![nt 1, t 0]),          //  1: a_1 -> A a_1     | ●a_1 ◄1 A!      | 2 | a_1 A
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε         | ◄2              | 1 | a_1
            3 => (3, symbols![nt 0, nt 1, t 1]),    //  3: a_2 -> a_1 C a_2 | ●a_2 ◄3 C! ►a_1 | 3 | a a_1 C
            4 => (1, symbols![nt 0]),               //  4: a_2 -> ε         | ◄4              | 1 | a
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a A+ C | B
        // NT flags:
        //  - a: parent_left_rec | parent_+_or_* | plus (6656)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_rec (4)
        //  - a_3: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        //  - a_3 -> a_1
        (821, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
            1 => "SynA1".to_string(),
        ], btreemap![
            0 => (1, symbols![t 2]),                //  0: a -> B a_2       | ►a_2 ◄0 B!      | 1 | B
            1 => (0, symbols![]),                   //  1: a_1 -> A a_3     | ►a_3 A!         | 0 |
            2 => (3, symbols![nt 0, nt 1, t 1]),    //  2: a_2 -> a_1 C a_2 | ●a_2 ◄2 C! ►a_1 | 3 | a a_1 C
            3 => (1, symbols![nt 0]),               //  3: a_2 -> ε         | ◄3              | 1 | a
            4 => (2, symbols![nt 1, t 0]),          //  4: a_3 -> a_1       | ●a_1 ◄4         | 2 | a_1 A
            5 => (2, symbols![nt 1, t 0]),          //  5: a_3 -> ε         | ◄5              | 2 | a_1 A
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // a -> a "x" a | a "*" "[" Num+ "]" | "-" a | Id
        // NT flags:
        //  - a: parent_left_rec | parent_amb | parent_+_or_* | plus (7680)
        //  - a_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - a_2: child_left_rec (4)
        //  - a_3: right_rec (2)
        //  - a_4: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        //  - a_3 -> a
        //  - a_4 -> a_1
        (835, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 0]),               //  0: a -> a_3 a_2               | ►a_2 ◄0 ►a_3             | 1 | a
            1 => (0, symbols![]),                   //  1: a_1 -> Num a_4             | ►a_4 Num!                | 0 |
            2 => (3, symbols![nt 0, nt 0]),         //  2: a_2 -> "x" a_3 a_2         | ●a_2 ◄2 ►a_3 "x"         | 3 | a a
            3 => (5, symbols![nt 0, nt 1]),         //  3: a_2 -> "*" "[" a_1 "]" a_2 | ●a_2 ◄3 "]" ►a_1 "[" "*" | 5 | a a_1
            4 => (1, symbols![nt 0]),               //  4: a_2 -> ε                   | ◄4                       | 1 | a
            5 => (2, symbols![nt 0]),               //  5: a_3 -> "-" a               | ◄5 ►a "-"                | 2 | a
            6 => (1, symbols![t 6]),                //  6: a_3 -> Id                  | ◄6 Id!                   | 1 | Id
            7 => (2, symbols![nt 1, t 3]),          //  7: a_4 -> a_1                 | ●a_1 ◄7                  | 2 | a_1 Num
            8 => (2, symbols![nt 1, t 3]),          //  8: a_4 -> ε                   | ◄8                       | 2 | a_1 Num
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- +_or_* and left_fact
        // a -> (A B | A C)*
        // NT flags:
        //  - a: parent_+_or_* (2048)
        //  - a_1: child_+_or_* | parent_left_fact (33)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (840, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1    | 1 | a_1
            1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
            2 => (1, symbols![nt 1]),               //  2: a_1 -> ε     | ◄2         | 1 | a_1
            3 => (3, symbols![nt 1, t 0, t 1]),     //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | 3 | a_1 A B
            4 => (3, symbols![nt 1, t 0, t 2]),     //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | 3 | a_1 A C
        ], NTValue::Default, btreemap![0 => vec![0]]),

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
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: a -> a_1     | ◄0 ►a_1 | 1 | a_1
            1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A! | 0 |
            2 => (0, symbols![]),                   //  2: a_2 -> B a_3 | ►a_3 B! | 0 |
            3 => (0, symbols![]),                   //  3: a_2 -> C a_4 | ►a_4 C! | 0 |
            4 => (3, symbols![nt 1, t 0, t 1]),     //  4: a_3 -> a_1   | ●a_1 ◄4 | 3 | a_1 A B
            5 => (3, symbols![nt 1, t 0, t 1]),     //  5: a_3 -> ε     | ◄5      | 3 | a_1 A B
            6 => (3, symbols![nt 1, t 0, t 2]),     //  6: a_4 -> a_1   | ●a_1 ◄6 | 3 | a_1 A C
            7 => (3, symbols![nt 1, t 0, t 2]),     //  7: a_4 -> ε     | ◄7      | 3 | a_1 A C
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- right_rec + left_fact
        // expr -> <L> Num "^" expr | Num
        // NT flags:
        //  - expr: right_rec | parent_left_fact | L-form (162)
        //  - expr_1: child_left_fact (64)
        // parents:
        //  - expr_1 -> expr
        (862, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (0, symbols![]),                   //  0: expr -> Num expr_1 | ►expr_1 Num! | 0 |
            1 => (3, symbols![nt 0, t 0]),          //  1: expr_1 -> "^" expr | ●expr ◄1 "^" | 3 | expr Num
            2 => (2, symbols![nt 0, t 0]),          //  2: expr_1 -> ε        | ◄2           | 2 | expr Num
        ], NTValue::Default, btreemap![0 => vec![1, 2]]),

        // --------------------------------------------------------------------------- left_rec [left_fact]
        // a -> a A | B C | B D
        // NT flags:
        //  - a: parent_left_fact | parent_left_rec (544)
        //  - a_1: child_left_rec (4)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a
        (870, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], btreemap![
            0 => (0, symbols![]),                   //  0: a -> B a_2   | ►a_2 B!    | 0 |
            1 => (2, symbols![nt 0, t 0]),          //  1: a_1 -> A a_1 | ●a_1 ◄1 A! | 2 | a A
            2 => (1, symbols![nt 0]),               //  2: a_1 -> ε     | ◄2         | 1 | a
            3 => (2, symbols![t 1, t 2]),           //  3: a_2 -> C a_1 | ►a_1 ◄3 C! | 2 | B C
            4 => (2, symbols![t 1, t 3]),           //  4: a_2 -> D a_1 | ►a_1 ◄4 D! | 2 | B D
        ], NTValue::Default, btreemap![0 => vec![3, 4]]),

        // a -> a A B | a A C | D
        // NT flags:
        //  - a: parent_left_rec (512)
        //  - a_1: child_left_rec | parent_left_fact (36)
        //  - a_2: child_left_fact (64)
        // parents:
        //  - a_1 -> a
        //  - a_2 -> a_1
        (871, true, false, true, 0, btreemap![
            0 => "SynA".to_string(),
        ], btreemap![
            0 => (1, symbols![t 3]),                //  0: a -> D a_1   | ►a_1 ◄0 D! | 1 | D
            1 => (0, symbols![]),                   //  1: a_1 -> A a_2 | ►a_2 A!    | 0 |
            2 => (1, symbols![nt 0]),               //  2: a_1 -> ε     | ◄2         | 1 | a
            3 => (3, symbols![nt 0, t 0, t 1]),     //  3: a_2 -> B a_1 | ●a_1 ◄3 B! | 3 | a A B
            4 => (3, symbols![nt 0, t 0, t 2]),     //  4: a_2 -> C a_1 | ●a_1 ◄4 C! | 3 | a A C
        ], NTValue::Default, btreemap![0 => vec![0]]),

        // --------------------------------------------------------------------------- misc
        // NT flags:
        //  - file: parent_+_or_* (2048)
        //  - option: parent_+_or_* (2048)
        //  - rule: parent_left_fact (32)
        //  - actions: parent_+_or_* (2048)
        //  - alt_items: parent_+_or_* (2048)
        //  - alt_item: parent_+_or_* | plus (6144)
        //  - repeat_item: parent_left_fact (32)
        //  - item: right_rec | parent_left_fact (34)
        //  - char_set: parent_+_or_* | plus (6144)
        //  - char_set_one: parent_left_fact (32)
        //  - file_1: child_+_or_* (1)
        //  - option_1: child_+_or_* | sep_list (32769)
        //  - actions_1: child_+_or_* | sep_list (32769)
        //  - alt_items_1: child_+_or_* | sep_list (32769)
        //  - alt_item_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - char_set_1: child_+_or_* | parent_left_fact | plus (4129)
        //  - rule_1: child_left_fact (64)
        //  - repeat_item_1: parent_left_fact | child_left_fact (96)
        //  - item_1: child_left_fact (64)
        //  - char_set_one_1: child_left_fact (64)
        //  - alt_item_2: child_left_fact (64)
        //  - char_set_2: child_left_fact (64)
        //  - repeat_item_2: child_left_fact (64)
        //  - repeat_item_3: child_left_fact (64)
        // parents:
        //  - file_1 -> file
        //  - option_1 -> option
        //  - actions_1 -> actions
        //  - alt_items_1 -> alt_items
        //  - alt_item_1 -> alt_item
        //  - char_set_1 -> char_set
        //  - rule_1 -> rule
        //  - repeat_item_1 -> repeat_item
        //  - item_1 -> item
        //  - char_set_one_1 -> char_set_one
        //  - alt_item_2 -> alt_item_1
        //  - char_set_2 -> char_set_1
        //  - repeat_item_2 -> repeat_item_1
        //  - repeat_item_3 -> repeat_item_1
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
        ], btreemap![
            0 => (2, symbols![nt 2, nt 15]),        //  0: file -> header file_1                    | ◄0 ►file_1 ►header                  | 2    | header file_1
            1 => (1, symbols![nt 15]),              //  1: file -> file_1                           | ◄1 ►file_1                          | 1    | file_1
            2 => (1, symbols![nt 4]),               //  2: file_item -> option                      | ◄2 ►option                          | 1    | option
            3 => (1, symbols![nt 3]),               //  3: file_item -> declaration                 | ◄3 ►declaration                     | 1    | declaration
            4 => (1, symbols![nt 5]),               //  4: file_item -> rule                        | ◄4 ►rule                            | 1    | rule
            5 => (3, symbols![t 27]),               //  5: header -> "lexicon" Id ";"               | ◄5 ";" Id! "lexicon"                | 3    | Id
            6 => (3, symbols![t 27]),               //  6: declaration -> "mode" Id ";"             | ◄6 ";" Id! "mode"                   | 3    | Id
            7 => (4, symbols![nt 16]),              //  7: option -> "channels" "{" Id option_1 "}" | ◄7 "}" ►option_1 Id! "{" "channels" | 4    | option_1
            8 => (5, symbols![t 27, nt 8]),         //  8: rule -> "fragment" Id ":" match ";"      | ◄8 ";" ►match ":" Id! "fragment"    | 5    | Id match
            9 => (0, symbols![]),                   //  9: rule -> Id ":" match rule_1              | ►rule_1 ►match ":" Id!              | 0    |
            10 => (1, symbols![nt 17]),             // 10: actions -> action actions_1              | ◄10 ►actions_1 ►action              | 1    | actions_1
            11 => (4, symbols![t 27]),              // 11: action -> "mode" "(" Id ")"              | ◄11 ")" Id! "(" "mode"              | 4    | Id
            12 => (4, symbols![t 27]),              // 12: action -> "push" "(" Id ")"              | ◄12 ")" Id! "(" "push"              | 4    | Id
            13 => (1, symbols![]),                  // 13: action -> "pop"                          | ◄13 "pop"                           | 1    |
            14 => (1, symbols![]),                  // 14: action -> "skip"                         | ◄14 "skip"                          | 1    |
            15 => (1, symbols![]),                  // 15: action -> "more"                         | ◄15 "more"                          | 1    |
            16 => (4, symbols![t 27]),              // 16: action -> "type" "(" Id ")"              | ◄16 ")" Id! "(" "type"              | 4    | Id
            17 => (4, symbols![t 27]),              // 17: action -> "channel" "(" Id ")"           | ◄17 ")" Id! "(" "channel"           | 4    | Id
            18 => (1, symbols![nt 9]),              // 18: match -> alt_items                       | ◄18 ►alt_items                      | 1    | alt_items
            19 => (1, symbols![nt 18]),             // 19: alt_items -> alt_item alt_items_1        | ◄19 ►alt_items_1 ►alt_item          | 1    | alt_items_1
            20 => (1, symbols![nt 19]),             // 20: alt_item -> alt_item_1                   | ◄20 ►alt_item_1                     | 1    | alt_item_1
            21 => (0, symbols![]),                  // 21: repeat_item -> item repeat_item_1        | ►repeat_item_1 ►item                | 0    |
            22 => (3, symbols![nt 9]),              // 22: item -> "(" alt_items ")"                | ◄22 ")" ►alt_items "("              | 3    | alt_items
            23 => (2, symbols![nt 12]),             // 23: item -> "~" item                         | ◄23 ►item "~"                       | 2    | item
            24 => (1, symbols![t 27]),              // 24: item -> Id                               | ◄24 Id!                             | 1    | Id
            25 => (0, symbols![]),                  // 25: item -> CharLit item_1                   | ►item_1 CharLit!                    | 0    |
            26 => (1, symbols![t 29]),              // 26: item -> StrLit                           | ◄26 StrLit!                         | 1    | StrLit
            27 => (1, symbols![nt 13]),             // 27: item -> char_set                         | ◄27 ►char_set                       | 1    | char_set
            28 => (3, symbols![nt 20]),             // 28: char_set -> "[" char_set_1 "]"           | ◄28 "]" ►char_set_1 "["             | 3    | char_set_1
            29 => (1, symbols![]),                  // 29: char_set -> "."                          | ◄29 "."                             | 1    |
            30 => (1, symbols![t 30]),              // 30: char_set -> FixedSet                     | ◄30 FixedSet!                       | 1    | FixedSet
            31 => (1, symbols![t 30]),              // 31: char_set_one -> FixedSet                 | ◄31 FixedSet!                       | 1    | FixedSet
            32 => (0, symbols![]),                  // 32: char_set_one -> SetChar char_set_one_1   | ►char_set_one_1 SetChar!            | 0    |
            33 => (2, symbols![nt 15, nt 1]),       // 33: file_1 -> file_item file_1               | ●file_1 ◄33 ►file_item              | 2    | file_1 file_item
            34 => (1, symbols![nt 15]),             // 34: file_1 -> ε                              | ◄34                                 | 1    | file_1
            35 => (3, symbols![nt 16, t 27]),       // 35: option_1 -> "," Id option_1              | ●option_1 ◄35 Id! ","               | 3, 1 | option_1 Id
            36 => (1, symbols![nt 16]),             // 36: option_1 -> ε                            | ◄36                                 | 1    | option_1
            37 => (3, symbols![nt 17, nt 7]),       // 37: actions_1 -> "," action actions_1        | ●actions_1 ◄37 ►action ","          | 3, 1 | actions_1 action
            38 => (1, symbols![nt 17]),             // 38: actions_1 -> ε                           | ◄38                                 | 1    | actions_1
            39 => (3, symbols![nt 18, nt 10]),      // 39: alt_items_1 -> "|" alt_item alt_items_1  | ●alt_items_1 ◄39 ►alt_item "|"      | 3, 1 | alt_items_1 alt_item
            40 => (1, symbols![nt 18]),             // 40: alt_items_1 -> ε                         | ◄40                                 | 1    | alt_items_1
            41 => (0, symbols![]),                  // 41: alt_item_1 -> repeat_item alt_item_2     | ►alt_item_2 ►repeat_item            | 0    |
            42 => (0, symbols![]),                  // 42: char_set_1 -> char_set_one char_set_2    | ►char_set_2 ►char_set_one           | 0    |
            43 => (6, symbols![t 27, nt 8, nt 6]),  // 43: rule_1 -> "->" actions ";"               | ◄43 ";" ►actions "->"               | 6    | Id match actions
            44 => (4, symbols![t 27, nt 8]),        // 44: rule_1 -> ";"                            | ◄44 ";"                             | 4    | Id match
            45 => (0, symbols![]),                  // 45: repeat_item_1 -> "+" repeat_item_2       | ►repeat_item_2 "+"                  | 0    |
            46 => (2, symbols![nt 12]),             // 46: repeat_item_1 -> "?"                     | ◄46 "?"                             | 2    | item
            47 => (0, symbols![]),                  // 47: repeat_item_1 -> "*" repeat_item_3       | ►repeat_item_3 "*"                  | 0    |
            48 => (1, symbols![nt 12]),             // 48: repeat_item_1 -> ε                       | ◄48                                 | 1    | item
            49 => (3, symbols![t 28, t 28]),        // 49: item_1 -> ".." CharLit                   | ◄49 CharLit! ".."                   | 3    | CharLit CharLit
            50 => (1, symbols![t 28]),              // 50: item_1 -> ε                              | ◄50                                 | 1    | CharLit
            51 => (3, symbols![t 33, t 33]),        // 51: char_set_one_1 -> "-" SetChar            | ◄51 SetChar! "-"                    | 3    | SetChar SetChar
            52 => (1, symbols![t 33]),              // 52: char_set_one_1 -> ε                      | ◄52                                 | 1    | SetChar
            53 => (2, symbols![nt 19, nt 11]),      // 53: alt_item_2 -> alt_item_1                 | ●alt_item_1 ◄53                     | 2    | alt_item_1 repeat_item
            54 => (2, symbols![nt 19, nt 11]),      // 54: alt_item_2 -> ε                          | ◄54                                 | 2    | alt_item_1 repeat_item
            55 => (2, symbols![nt 20, nt 14]),      // 55: char_set_2 -> char_set_1                 | ●char_set_1 ◄55                     | 2    | char_set_1 char_set_one
            56 => (2, symbols![nt 20, nt 14]),      // 56: char_set_2 -> ε                          | ◄56                                 | 2    | char_set_1 char_set_one
            57 => (3, symbols![nt 12]),             // 57: repeat_item_2 -> "?"                     | ◄57 "?"                             | 3    | item
            58 => (2, symbols![nt 12]),             // 58: repeat_item_2 -> ε                       | ◄58                                 | 2    | item
            59 => (3, symbols![nt 12]),             // 59: repeat_item_3 -> "?"                     | ◄59 "?"                             | 3    | item
            60 => (2, symbols![nt 12]),             // 60: repeat_item_3 -> ε                       | ◄60                                 | 2    | item
        ], NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2, 3, 4], 2 => vec![5], 3 => vec![6], 4 => vec![7], 5 => vec![8, 43, 44], 6 => vec![10], 7 => vec![11, 12, 13, 14, 15, 16, 17], 8 => vec![18], 9 => vec![19], 10 => vec![20], 11 => vec![46, 48, 57, 58, 59, 60], 12 => vec![22, 23, 24, 26, 27, 49, 50], 13 => vec![28, 29, 30], 14 => vec![31, 51, 52]]),

        // NT flags:
        //  - program: parent_+_or_* | plus (6144)
        //  - decl_i: child_+_or_* | L-form (129)
        //  - inst_i: child_+_or_* | parent_left_fact | L-form | plus (4257)
        //  - decl: parent_+_or_* (2048)
        //  - id_i: child_+_or_* | L-form | sep_list (32897)
        //  - expr: parent_left_rec | parent_amb (1536)
        //  - expr_1: child_left_rec (4)
        //  - expr_2: right_rec (2)
        //  - inst_i_1: child_left_fact (64)
        // parents:
        //  - decl_i -> program
        //  - inst_i -> program
        //  - id_i -> decl
        //  - expr_1 -> expr
        //  - expr_2 -> expr
        //  - inst_i_1 -> inst_i
        (902, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![nt 1, nt 2]),         //  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i         | 2    | decl_i inst_i
            1 => (2, symbols![nt 1, nt 3]),         //  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl           | 2    | decl_i decl
            2 => (1, symbols![nt 1]),               //  2: decl_i -> ε                   | ◄2                         | 1    | decl_i
            3 => (0, symbols![]),                   //  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst            | 0    |
            4 => (3, symbols![t 2, nt 4]),          //  4: decl -> Type Id id_i ";"      | ◄4 ";" ►id_i Id! Type!     | 3    | Type id_i
            5 => (4, symbols![t 2, t 1]),           //  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef" | 4    | Type Id
            6 => (3, symbols![nt 4, t 1]),          //  6: id_i -> "," Id id_i           | ●id_i ◄6 Id! ","           | 3, 1 | id_i Id
            7 => (1, symbols![nt 4]),               //  7: id_i -> ε                     | ◄7                         | 1    | id_i
            8 => (5, symbols![t 1, nt 6]),          //  8: inst -> "let" Id "=" expr ";" | ◄8 ";" ►expr "=" Id! "let" | 5    | Id expr
            9 => (3, symbols![nt 6]),               //  9: inst -> "print" expr ";"      | ◄9 ";" ►expr "print"       | 3    | expr
            10 => (1, symbols![nt 6]),              // 10: expr -> expr_2 expr_1         | ►expr_1 ◄10 ►expr_2        | 1    | expr
            11 => (3, symbols![nt 6, nt 6]),        // 11: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄11 ►expr_2 "+"    | 3    | expr expr
            12 => (3, symbols![nt 6, nt 6]),        // 12: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "-"    | 3    | expr expr
            13 => (1, symbols![nt 6]),              // 13: expr_1 -> ε                   | ◄13                        | 1    | expr
            14 => (2, symbols![nt 6]),              // 14: expr_2 -> "-" expr_2          | ◄14 ►expr_2 "-"            | 2    | expr
            15 => (1, symbols![t 1]),               // 15: expr_2 -> Id                  | ◄15 Id!                    | 1    | Id
            16 => (1, symbols![t 0]),               // 16: expr_2 -> Num                 | ◄16 Num!                   | 1    | Num
            17 => (2, symbols![nt 2, nt 5]),        // 17: inst_i_1 -> inst_i            | ●inst_i ◄17                | 2    | inst_i inst
            18 => (2, symbols![nt 2, nt 5]),        // 18: inst_i_1 -> ε                 | ◄18                        | 2    | inst_i inst
        ], NTValue::Default, btreemap![0 => vec![0], 3 => vec![4, 5], 5 => vec![8, 9], 6 => vec![10]]),
        (902, true, false, true, 0, btreemap![
        ], btreemap![
            0 => (2, symbols![]),                   //  0: program -> decl_i inst_i      | ◄0 ►inst_i ►decl_i         | 2    |
            1 => (2, symbols![]),                   //  1: decl_i -> decl decl_i         | ●decl_i ◄1 ►decl           | 2    |
            2 => (1, symbols![]),                   //  2: decl_i -> ε                   | ◄2                         | 1    |
            3 => (0, symbols![]),                   //  3: inst_i -> inst inst_i_1       | ►inst_i_1 ►inst            | 0    |
            4 => (3, symbols![t 2]),                //  4: decl -> Type Id id_i ";"      | ◄4 ";" ►id_i Id! Type!     | 3    | Type
            5 => (4, symbols![t 2, t 1]),           //  5: decl -> "typedef" Type Id ";" | ◄5 ";" Id! Type! "typedef" | 4    | Type Id
            6 => (3, symbols![t 1]),                //  6: id_i -> "," Id id_i           | ●id_i ◄6 Id! ","           | 3, 1 | Id
            7 => (1, symbols![]),                   //  7: id_i -> ε                     | ◄7                         | 1    |
            8 => (5, symbols![t 1]),                //  8: inst -> "let" Id "=" expr ";" | ◄8 ";" ►expr "=" Id! "let" | 5    | Id
            9 => (3, symbols![]),                   //  9: inst -> "print" expr ";"      | ◄9 ";" ►expr "print"       | 3    |
            10 => (1, symbols![]),                  // 10: expr -> expr_2 expr_1         | ►expr_1 ◄10 ►expr_2        | 1    |
            11 => (3, symbols![]),                  // 11: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄11 ►expr_2 "+"    | 3    |
            12 => (3, symbols![]),                  // 12: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "-"    | 3    |
            13 => (1, symbols![]),                  // 13: expr_1 -> ε                   | ◄13                        | 1    |
            14 => (2, symbols![]),                  // 14: expr_2 -> "-" expr_2          | ◄14 ►expr_2 "-"            | 2    |
            15 => (1, symbols![t 1]),               // 15: expr_2 -> Id                  | ◄15 Id!                    | 1    | Id
            16 => (1, symbols![t 0]),               // 16: expr_2 -> Num                 | ◄16 Num!                   | 1    | Num
            17 => (2, symbols![]),                  // 17: inst_i_1 -> inst_i            | ●inst_i ◄17                | 2    |
            18 => (2, symbols![]),                  // 18: inst_i_1 -> ε                 | ◄18                        | 2    |
        ], NTValue::None, btreemap![0 => vec![0], 3 => vec![4, 5], 5 => vec![8, 9], 6 => vec![10]]),

        // NT flags:
        //  - program: parent_+_or_* (2048)
        //  - stmt_i: child_+_or_* | L-form (129)
        //  - decl: parent_+_or_* (2048)
        //  - expr: parent_left_rec | parent_amb (1536)
        //  - decl_1: child_+_or_* | sep_list (32769)
        //  - expr_1: child_left_rec (4)
        //  - expr_2: right_rec (2)
        // parents:
        //  - stmt_i -> program
        //  - decl_1 -> decl
        //  - expr_1 -> expr
        //  - expr_2 -> expr
        (903, false, false, true, 0, btreemap![
        ], btreemap![
            0 => (1, symbols![nt 1]),               //  0: program -> stmt_i             | ◄0 ►stmt_i                 | 1    | stmt_i
            1 => (2, symbols![nt 1, nt 2]),         //  1: stmt_i -> stmt stmt_i         | ●stmt_i ◄1 ►stmt           | 2    | stmt_i stmt
            2 => (1, symbols![nt 1]),               //  2: stmt_i -> ε                   | ◄2                         | 1    | stmt_i
            3 => (1, symbols![nt 3]),               //  3: stmt -> decl                  | ◄3 ►decl                   | 1    | decl
            4 => (1, symbols![nt 4]),               //  4: stmt -> inst                  | ◄4 ►inst                   | 1    | inst
            5 => (3, symbols![t 2, nt 6]),          //  5: decl -> Type Id decl_1 ";"    | ◄5 ";" ►decl_1 Id! Type!   | 3    | Type decl_1
            6 => (4, symbols![t 2, t 1]),           //  6: decl -> "typedef" Type Id ";" | ◄6 ";" Id! Type! "typedef" | 4    | Type Id
            7 => (4, symbols![t 1, nt 5]),          //  7: inst -> Id "=" expr ";"       | ◄7 ";" ►expr "=" Id!       | 4    | Id expr
            8 => (3, symbols![nt 5]),               //  8: inst -> "print" expr ";"      | ◄8 ";" ►expr "print"       | 3    | expr
            9 => (1, symbols![nt 5]),               //  9: expr -> expr_2 expr_1         | ►expr_1 ◄9 ►expr_2         | 1    | expr
            10 => (3, symbols![nt 6, t 1]),         // 10: decl_1 -> "," Id decl_1       | ●decl_1 ◄10 Id! ","        | 3, 1 | decl_1 Id
            11 => (1, symbols![nt 6]),              // 11: decl_1 -> ε                   | ◄11                        | 1    | decl_1
            12 => (3, symbols![nt 5, nt 5]),        // 12: expr_1 -> "+" expr_2 expr_1   | ●expr_1 ◄12 ►expr_2 "+"    | 3    | expr expr
            13 => (3, symbols![nt 5, nt 5]),        // 13: expr_1 -> "-" expr_2 expr_1   | ●expr_1 ◄13 ►expr_2 "-"    | 3    | expr expr
            14 => (1, symbols![nt 5]),              // 14: expr_1 -> ε                   | ◄14                        | 1    | expr
            15 => (2, symbols![nt 5]),              // 15: expr_2 -> "-" expr_2          | ◄15 ►expr_2 "-"            | 2    | expr
            16 => (1, symbols![t 1]),               // 16: expr_2 -> Id                  | ◄16 Id!                    | 1    | Id
            17 => (1, symbols![t 0]),               // 17: expr_2 -> Num                 | ◄17 Num!                   | 1    | Num
        ], NTValue::Default, btreemap![0 => vec![0], 2 => vec![3, 4], 3 => vec![5, 6], 4 => vec![7, 8], 5 => vec![9]]),

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
        ], btreemap![
            0 => (2, symbols![nt 1]),               //  0: statement -> assign ";"      | ◄0 ";" ►assign          | 2 | assign
            1 => (2, symbols![nt 2]),               //  1: statement -> print ";"       | ◄1 ";" ►print           | 2 | print
            2 => (4, symbols![t 0, nt 3]),          //  2: assign -> "let" Id "=" value | ◄2 ►value "=" Id! "let" | 4 | Id value
            3 => (2, symbols![nt 3]),               //  3: print -> "print" value       | ◄3 ►value "print"       | 2 | value
            4 => (1, symbols![t 0]),                //  4: value -> Id                  | ◄4 Id!                  | 1 | Id
            5 => (1, symbols![t 1]),                //  5: value -> Num                 | ◄5 Num!                 | 1 | Num
        ], NTValue::Default, btreemap![0 => vec![0, 1], 1 => vec![2], 2 => vec![3], 3 => vec![4, 5]]),
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
        parser_type: ParserType::LL1,
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
        parser_type: ParserType::LL1,
        wrapper_filename: WRAPPER_FILENAME,
        tests: get_ll1_tests(),
    };
    build_items(spec);
}
