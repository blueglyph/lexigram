// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

use lexigram_core::{CollectJoin, VarId};
use lexigram_core::log::LogStatus;
use crate::grammar::lr::LRParsingTable;
use crate::grammar::tests::prs::print_alts;
use crate::grammar::tests::TestRules;

#[test]
fn prs_calc_lr_table() {
    static TESTS: &[(u32, VarId, usize, &[&str], usize, &[&str], &[&str])] = &[
        (14, 0, 0, &[
            r#"  | Op Id  $  | a b c"#,
            r#"--+-----------+------"#,
            r#"0 | s1 s2  -  | 3 4 5"#,
            r#"1 | -  s2  -  | - - 6"#,
            r#"2 | -  r3 r3  | - - -"#,
            r#"3 | -  -  acc | - - -"#,
            r#"4 | -  s2  -  | - - 7"#,
            r#"5 | -  -  r1  | - - -"#,
            r#"6 | -  r2  -  | - - -"#,
            r#"7 | -  -  r0  | - - -"#,
            r#"--+-----------+------"#,
        ], 3, &[
            r#"  | Op Id  $  | a b c"#,
            r#"--+-----------+------"#,
            r#"0 | s1 s3  -  | 4 2 5"#,
            r#"1 | -  s3  -  | - - 6"#,
            r#"2 | -  s3  -  | - - 7"#,
            r#"3 | -  r3 r3  |      "#,
            r#"4 | -  -  acc |      "#,
            r#"5 | -  -  r1  |      "#,
            r#"6 | -  r2  -  |      "#,
            r#"7 | -  -  r0  |      "#,
            r#"--+-----------+------"#,
        ], &[]),
        (102, 0, 0, &[
            // a -> A B* C;
            // - 0: a -> A a_1 C
            // - 1: a_1 -> a_1 B
            // - 2: a_1 -> ε
            // - 3: <goal> -> a
            //
            r#"  | A  B  C   $  | a a_1"#,
            r#"--+--------------+------"#,
            r#"0 | s1 -  -   -  | 2  - "#,
            r#"1 | -  r2 r2  -  | -  3 "#,
            r#"2 | -  -  -  acc | -  - "#,
            r#"3 | -  s4 s5  -  | -  - "#,
            r#"4 | -  r1 r1  -  | -  - "#,
            r#"5 | -  -  -  r0  | -  - "#,
            r#"--+--------------+------"#,
        ], 2, &[
            r#"  | A  B  C   $  | a a_1"#,
            r#"--+--------------+------"#,
            r#"0 | s1 -  -   -  | 2  - "#,
            r#"1 | -  r2 r2  -  | -  3 "#,
            r#"2 | -  -  -  acc |      "#,
            r#"3 | -  s4 s5  -  |      "#,
            r#"4 | -  r1 r1  -  |      "#,
            r#"5 | -  -  -  r0  |      "#,
            r#"--+--------------+------"#,
        ], &[]),
        (103, 0, 0, &[
            r#"  | A  B  C   $  | a a_1"#,
            r#"--+--------------+------"#,
            r#"0 | s1 -  -   -  | 2  - "#,
            r#"1 | -  s3 -   -  | -  4 "#,
            r#"2 | -  -  -  acc | -  - "#,
            r#"3 | -  r2 r2  -  | -  - "#,
            r#"4 | -  s5 s6  -  | -  - "#,
            r#"5 | -  r1 r1  -  | -  - "#,
            r#"6 | -  -  -  r0  | -  - "#,
            r#"--+--------------+------"#,
        ], 2, &[
            r#"  | A  B  C   $  | a a_1"#,
            r#"--+--------------+------"#,
            r#"0 | s1 -  -   -  | 2  - "#,
            r#"1 | -  s3 -   -  | -  4 "#,
            r#"2 | -  -  -  acc |      "#,
            r#"3 | -  r2 r2  -  |      "#,
            r#"4 | -  s5 s6  -  |      "#,
            r#"5 | -  r1 r1  -  |      "#,
            r#"6 | -  -  -  r0  |      "#,
            r#"--+--------------+------"#,
        ], &[]),
        (121, 0, 0, &[
            // a -> A b* C; b -> Id;
            // - 0: a -> A a_1 C
            // - 1: b -> Id
            // - 2: a_1 -> a_1 b
            // - 3: a_1 -> ε
            // - 4: <goal> -> a
            //
            r#"  | A  C  Id  $  | a b a_1"#,
            r#"--+--------------+--------"#,
            r#"0 | s1 -  -   -  | 2 -  - "#,
            r#"1 | -  r3 r3  -  | - -  3 "#,
            r#"2 | -  -  -  acc | - -  - "#,
            r#"3 | -  s4 s5  -  | - 6  - "#,
            r#"4 | -  -  -  r0  | - -  - "#,
            r#"5 | -  r1 r1  -  | - -  - "#,
            r#"6 | -  r2 r2  -  | - -  - "#,
            r#"--+--------------+--------"#,
        ], 3, &[
            r#"  | A  C  Id  $  | a b a_1"#,
            r#"--+--------------+--------"#,
            r#"0 | s1 -  -   -  | 3 -  - "#,
            r#"1 | -  r3 r3  -  | - -  2 "#,
            r#"2 | -  s4 s5  -  | - 6  - "#,
            r#"3 | -  -  -  acc |        "#,
            r#"4 | -  -  -  r0  |        "#,
            r#"5 | -  r1 r1  -  |        "#,
            r#"6 | -  r2 r2  -  |        "#,
            r#"--+--------------+--------"#,
        ], &[]),
        (122, 0, 0, &[
            // a -> A b+ C; b -> Id;
            // - 0: a -> A a_1 C
            // - 1: b -> Id
            // - 2: a_1 -> a_1 b
            // - 3: a_1 -> b
            // - 4: <goal> -> a
            //
            r#"  | A  C  Id  $  | a b a_1"#,
            r#"--+--------------+--------"#,
            r#"0 | s1 -  -   -  | 2 -  - "#,
            r#"1 | -  -  s3  -  | - 4  5 "#,
            r#"2 | -  -  -  acc | - -  - "#,
            r#"3 | -  r1 r1  -  | - -  - "#,
            r#"4 | -  r3 r3  -  | - -  - "#,
            r#"5 | -  s6 s3  -  | - 7  - "#,
            r#"6 | -  -  -  r0  | - -  - "#,
            r#"7 | -  r2 r2  -  | - -  - "#,
            r#"--+--------------+--------"#,
        ], 3, &[
            r#"  | A  C  Id  $  | a b a_1"#,
            r#"--+--------------+--------"#,
            r#"0 | s1 -  -   -  | 3 -  - "#,
            r#"1 | -  -  s4  -  | - 5  2 "#,
            r#"2 | -  s6 s4  -  | - 7  - "#,
            r#"3 | -  -  -  acc |        "#,
            r#"4 | -  r1 r1  -  |        "#,
            r#"5 | -  r3 r3  -  |        "#,
            r#"6 | -  -  -  r0  |        "#,
            r#"7 | -  r2 r2  -  |        "#,
            r#"--+--------------+--------"#,
        ], &[]),
        (552, 0, 0, &[
            r#"  | "-" Id "(" ")"  $  | e t"#,
            r#"--+--------------------+----"#,
            r#"0 |  -  s1 s2   -   -  | 3 4"#,
            r#"1 | r2  -   -  r2  r2  | - -"#,
            r#"2 |  -  s1 s2   -   -  | 5 4"#,
            r#"3 | s6  -   -   -  acc | - -"#,
            r#"4 | r1  -   -  r1  r1  | - -"#,
            r#"5 | s6  -   -  s7   -  | - -"#,
            r#"6 |  -  s1 s2   -   -  | - 8"#,
            r#"7 | r3  -   -  r3  r3  | - -"#,
            r#"8 | r0  -   -  r0  r0  | - -"#,
            r#"--+--------------------+----"#,
        ], 3, &[
            r#"  | "-" Id "(" ")"  $  | e t"#,
            r#"--+--------------------+----"#,
            r#"0 |  -  s3 s1   -   -  | 4 5"#,
            r#"1 |  -  s3 s1   -   -  | 6 5"#,
            r#"2 |  -  s3 s1   -   -  | - 8"#,
            r#"3 | r2  -   -  r2  r2  |    "#,
            r#"4 | s2  -   -   -  acc |    "#,
            r#"5 | r1  -   -  r1  r1  |    "#,
            r#"6 | s2  -   -  s7   -  |    "#,
            r#"7 | r3  -   -  r3  r3  |    "#,
            r#"8 | r0  -   -  r0  r0  |    "#,
            r#"--+--------------------+----"#,
        ], &[]),
        // resolved conflicts
        (601, 0, 0, &[
            // - 0: e -> e "*" e
            // - 1: e -> e "+" e
            // - 2: e -> Num
            // - 3: e -> Id
            // - 4: <goal> -> e
            //
            r#"  | "*" "+" Num Id  $  | e"#,
            r#"--+--------------------+--"#,
            r#"0 |  -   -  s1  s2  -  | 3"#,
            r#"1 | r2  r2   -  -  r2  | -"#,
            r#"2 | r3  r3   -  -  r3  | -"#,
            r#"3 | s4  s5   -  -  acc | -"#,
            r#"4 |  -   -  s1  s2  -  | 6"#,
            r#"5 |  -   -  s1  s2  -  | 7"#,
            r#"6 | r0  r0   -  -  r0  | -"#,
            r#"7 | s4  r1   -  -  r1  | -"#,
            r#"--+--------------------+--"#,
        ], 3, &[
            r#"  | "*" "+" Num Id  $  | e"#,
            r#"--+--------------------+--"#,
            r#"0 |  -   -  s3  s4  -  | 5"#,
            r#"1 |  -   -  s3  s4  -  | 6"#,
            r#"2 |  -   -  s3  s4  -  | 7"#,
            r#"3 | r2  r2   -  -  r2  |  "#,
            r#"4 | r3  r3   -  -  r3  |  "#,
            r#"5 | s1  s2   -  -  acc |  "#,
            r#"6 | r0  r0   -  -  r0  |  "#,
            r#"7 | s1  r1   -  -  r1  |  "#,
            r#"--+--------------------+--"#,
        ], &[
            // conflict in state 6 for "*": r0 (e -> e "*" e) vs s4 (e -> e "*" • e) => resolved as r0
            // conflict in state 6 for "+": r0 (e -> e "*" e) vs s5 (e -> e "+" • e) => resolved as r0
            // conflict in state 7 for "*": r1 (e -> e "+" e) vs s4 (e -> e "*" • e) => resolved as s4
            // conflict in state 7 for "+": r1 (e -> e "+" e) vs s5 (e -> e "+" • e) => resolved as r1
        ]),
        (2000, 0, 1, &[
            r#"   | "a" "b"  $  | s  a  b  c  d "#,
            r#"---+-------------+---------------"#,
            r#" 0 | s1  s2   -  | 3  -  -  -  - "#,
            r#" 1 | s4   -   -  | -  5  6  -  - "#,
            r#" 2 | s7   -   -  | -  8  6  -  - "#,
            r#" 3 |  -   -  acc | -  -  -  -  - "#,
            r#" 4 | r4  s9   -  | -  -  -  -  - "#,
            r#" 5 | s10  -   -  | -  -  -  -  - "#,
            r#" 6 | r6  r6   -  | -  -  -  11 12"#,
            r#" 7 |  -  r4   -  | -  -  -  -  - "#,
            r#" 8 |  -  s13  -  | -  -  -  -  - "#,
            r#" 9 |  -   -  r1  | -  -  -  -  - "#,
            r#"10 |  -   -  r0  | -  -  -  -  - "#,
            r#"11 | r3  r3   -  | -  -  -  -  - "#,
            r#"12 | r5  r5   -  | -  -  -  -  - "#,
            r#"13 |  -   -  r2  | -  -  -  -  - "#,
            r#"---+-------------+---------------"#,
        ], 4, &[
            r#"   | "a" "b"  $  | s  a  b  c  d "#,
            r#"---+-------------+---------------"#,
            r#" 0 | s1  s2   -  | 4  -  -  -  - "#,
            r#" 1 | s5   -   -  | -  6  3  -  - "#,
            r#" 2 | s7   -   -  | -  8  3  -  - "#,
            r#" 3 | r6  r6   -  | -  -  -  11 12"#,
            r#" 4 |  -   -  acc |               "#,
            r#" 5 | r4  s9   -  |               "#,
            r#" 6 | s10  -   -  |               "#,
            r#" 7 |  -  r4   -  |               "#,
            r#" 8 |  -  s13  -  |               "#,
            r#" 9 |  -   -  r1  |               "#,
            r#"10 |  -   -  r0  |               "#,
            r#"11 | r3  r3   -  |               "#,
            r#"12 | r5  r5   -  |               "#,
            r#"13 |  -   -  r2  |               "#,
            r#"---+-------------+---------------"#,
        ], &[]),
        (2001, 0, 0, &[
            r#"  | ";" "*"  $  | a b a_1"#,
            r#"--+-------------+--------"#,
            r#"0 | r3  r3   -  | 1 -  2 "#,
            r#"1 |  -   -  acc | - -  - "#,
            r#"2 | s3  s4   -  | - 5  - "#,
            r#"3 |  -   -  r0  | - -  - "#,
            r#"4 | r1  r1   -  | - -  - "#,
            r#"5 | r2  r2   -  | - -  - "#,
            r#"--+-------------+--------"#,
        ], 2, &[
            r#"  | ";" "*"  $  | a b a_1"#,
            r#"--+-------------+--------"#,
            r#"0 | r3  r3   -  | 2 -  1 "#,
            r#"1 | s3  s4   -  | - 5  - "#,
            r#"2 |  -   -  acc |        "#,
            r#"3 |  -   -  r0  |        "#,
            r#"4 | r1  r1   -  |        "#,
            r#"5 | r2  r2   -  |        "#,
            r#"--+-------------+--------"#,
        ], &[]),
        (2002, 0, 0, &[
            // - 0: a -> "a" b
            // - 1: b -> c
            // - 2: b -> d
            // - 3: b -> e
            // - 4: b -> f
            // - 5: c -> "c"
            // - 6: d -> "d"
            // - 7: e -> "e"
            // - 8: f -> "f"
            // - 9: <goal> -> a
            //
            r#"   | "a" "c" "d" "e" "f"  $  | a  b  c  d  e  f "#,
            r#"---+-------------------------+------------------"#,
            r#" 0 | s1   -   -   -   -   -  | 2  -  -  -  -  - "#,
            r#" 1 |  -  s3  s4  s5  s6   -  | -  7  8  9  10 11"#,
            r#" 2 |  -   -   -   -   -  acc | -  -  -  -  -  - "#,
            r#" 3 |  -   -   -   -   -  r5  | -  -  -  -  -  - "#,
            r#" 4 |  -   -   -   -   -  r6  | -  -  -  -  -  - "#,
            r#" 5 |  -   -   -   -   -  r7  | -  -  -  -  -  - "#,
            r#" 6 |  -   -   -   -   -  r8  | -  -  -  -  -  - "#,
            r#" 7 |  -   -   -   -   -  r0  | -  -  -  -  -  - "#,
            r#" 8 |  -   -   -   -   -  r1  | -  -  -  -  -  - "#,
            r#" 9 |  -   -   -   -   -  r2  | -  -  -  -  -  - "#,
            r#"10 |  -   -   -   -   -  r3  | -  -  -  -  -  - "#,
            r#"11 |  -   -   -   -   -  r4  | -  -  -  -  -  - "#,
            r#"---+-------------------------+------------------"#,
        ], 2, &[
            r#"   | "a" "c" "d" "e" "f"  $  | a  b  c  d  e  f "#,
            r#"---+-------------------------+------------------"#,
            r#" 0 | s1   -   -   -   -   -  | 2  -  -  -  -  - "#,
            r#" 1 |  -  s3  s4  s5  s6   -  | -  7  8  9  10 11"#,
            r#" 2 |  -   -   -   -   -  acc |                  "#,
            r#" 3 |  -   -   -   -   -  r5  |                  "#,
            r#" 4 |  -   -   -   -   -  r6  |                  "#,
            r#" 5 |  -   -   -   -   -  r7  |                  "#,
            r#" 6 |  -   -   -   -   -  r8  |                  "#,
            r#" 7 |  -   -   -   -   -  r0  |                  "#,
            r#" 8 |  -   -   -   -   -  r1  |                  "#,
            r#" 9 |  -   -   -   -   -  r2  |                  "#,
            r#"10 |  -   -   -   -   -  r3  |                  "#,
            r#"11 |  -   -   -   -   -  r4  |                  "#,
            r#"---+-------------------------+------------------"#,
        ], &[]),
        (2003, 0, 0, &[
            // - 0: e -> e "+" t
            // - 1: e -> t
            // - 2: t -> t "*" f
            // - 3: t -> f
            // - 4: f -> Id
            // - 5: f -> "(" e ")"
            // - 6: <goal> -> e
            //
            r#"   | "+" "*" Id  "(" ")"  $  | e  t  f "#,
            r#"---+-------------------------+---------"#,
            r#" 0 |  -   -  s1  s2   -   -  | 3  4  5 "#,
            r#" 1 | r4  r4   -   -  r4  r4  | -  -  - "#,
            r#" 2 |  -   -  s1  s2   -   -  | 6  4  5 "#,
            r#" 3 | s7   -   -   -   -  acc | -  -  - "#,
            r#" 4 | r1  s8   -   -  r1  r1  | -  -  - "#,
            r#" 5 | r3  r3   -   -  r3  r3  | -  -  - "#,
            r#" 6 | s7   -   -   -  s9   -  | -  -  - "#,
            r#" 7 |  -   -  s1  s2   -   -  | -  10 5 "#,
            r#" 8 |  -   -  s1  s2   -   -  | -  -  11"#,
            r#" 9 | r5  r5   -   -  r5  r5  | -  -  - "#,
            r#"10 | r0  s8   -   -  r0  r0  | -  -  - "#,
            r#"11 | r2  r2   -   -  r2  r2  | -  -  - "#,
            r#"---+-------------------------+---------"#,
        ], 4, &[
            r#"   | "+" "*" Id  "(" ")"  $  | e  t  f "#,
            r#"---+-------------------------+---------"#,
            r#" 0 |  -   -  s4  s1   -   -  | 5  6  7 "#,
            r#" 1 |  -   -  s4  s1   -   -  | 8  6  7 "#,
            r#" 2 |  -   -  s4  s1   -   -  | -  10 7 "#,
            r#" 3 |  -   -  s4  s1   -   -  | -  -  11"#,
            r#" 4 | r4  r4   -   -  r4  r4  |         "#,
            r#" 5 | s2   -   -   -   -  acc |         "#,
            r#" 6 | r1  s3   -   -  r1  r1  |         "#,
            r#" 7 | r3  r3   -   -  r3  r3  |         "#,
            r#" 8 | s2   -   -   -  s9   -  |         "#,
            r#" 9 | r5  r5   -   -  r5  r5  |         "#,
            r#"10 | r0  s3   -   -  r0  r0  |         "#,
            r#"11 | r2  r2   -   -  r2  r2  |         "#,
            r#"---+-------------------------+---------"#,
        ], &[]),
        (2004, 0, 0, &[
            r#"   | "-" "^" "*" "/" "+" "(" ")" Id  Num  $  | amb_i"#,
            r#"---+-----------------------------------------+------"#,
            r#" 0 | s1   -   -   -   -  s2   -  s3  s4   -  |   5  "#,
            r#" 1 | s1   -   -   -   -  s2   -  s3  s4   -  |   6  "#,
            r#" 2 | s1   -   -   -   -  s2   -  s3  s4   -  |   7  "#,
            r#" 3 | r7  r7  r7  r7  r7   -  r7   -   -  r7  |   -  "#,
            r#" 4 | r8  r8  r8  r8  r8   -  r8   -   -  r8  |   -  "#,
            r#" 5 | s8  s9  s10 s11 s12  -   -   -   -  acc |   -  "#,
            r#" 6 | r0  r0  r0  r0  r0   -  r0   -   -  r0  |   -  "#,
            r#" 7 | s8  s9  s10 s11 s12  -  s13  -   -   -  |   -  "#,
            r#" 8 | s1   -   -   -   -  s2   -  s3  s4   -  |  14  "#,
            r#" 9 | s1   -   -   -   -  s2   -  s3  s4   -  |  15  "#,
            r#"10 | s1   -   -   -   -  s2   -  s3  s4   -  |  16  "#,
            r#"11 | s1   -   -   -   -  s2   -  s3  s4   -  |  17  "#,
            r#"12 | s1   -   -   -   -  s2   -  s3  s4   -  |  18  "#,
            r#"13 | r6  r6  r6  r6  r6   -  r6   -   -  r6  |   -  "#,
            r#"14 | r5  s9  s10 s11 r5   -  r5   -   -  r5  |   -  "#,
            r#"15 | r1  s9  r1  r1  r1   -  r1   -   -  r1  |   -  "#,
            r#"16 | r2  s9  r2  r2  r2   -  r2   -   -  r2  |   -  "#,
            r#"17 | r3  s9  r3  r3  r3   -  r3   -   -  r3  |   -  "#,
            r#"18 | r4  s9  s10 s11 r4   -  r4   -   -  r4  |   -  "#,
            r#"---+-----------------------------------------+------"#,
        ], 8, &[
            r#"   | "-" "^" "*" "/" "+" "(" ")" Id  Num  $  | amb_i"#,
            r#"---+-----------------------------------------+------"#,
            r#" 0 | s1   -   -   -   -  s2   -  s8  s9   -  |  10  "#,
            r#" 1 | s1   -   -   -   -  s2   -  s8  s9   -  |  11  "#,
            r#" 2 | s1   -   -   -   -  s2   -  s8  s9   -  |  12  "#,
            r#" 3 | s1   -   -   -   -  s2   -  s8  s9   -  |  14  "#,
            r#" 4 | s1   -   -   -   -  s2   -  s8  s9   -  |  15  "#,
            r#" 5 | s1   -   -   -   -  s2   -  s8  s9   -  |  16  "#,
            r#" 6 | s1   -   -   -   -  s2   -  s8  s9   -  |  17  "#,
            r#" 7 | s1   -   -   -   -  s2   -  s8  s9   -  |  18  "#,
            r#" 8 | r7  r7  r7  r7  r7   -  r7   -   -  r7  |      "#,
            r#" 9 | r8  r8  r8  r8  r8   -  r8   -   -  r8  |      "#,
            r#"10 | s3  s4  s5  s6  s7   -   -   -   -  acc |      "#,
            r#"11 | r0  r0  r0  r0  r0   -  r0   -   -  r0  |      "#,
            r#"12 | s3  s4  s5  s6  s7   -  s13  -   -   -  |      "#,
            r#"13 | r6  r6  r6  r6  r6   -  r6   -   -  r6  |      "#,
            r#"14 | r5  s4  s5  s6  r5   -  r5   -   -  r5  |      "#,
            r#"15 | r1  s4  r1  r1  r1   -  r1   -   -  r1  |      "#,
            r#"16 | r2  s4  r2  r2  r2   -  r2   -   -  r2  |      "#,
            r#"17 | r3  s4  r3  r3  r3   -  r3   -   -  r3  |      "#,
            r#"18 | r4  s4  s5  s6  r4   -  r4   -   -  r4  |      "#,
            r#"---+-----------------------------------------+------"#,
        ], &[]),
        (2005, 0, 0, &[
            r#"   | "-" "*" "/" "+" "(" ")" Id  Num  $  | amb_i"#,
            r#"---+-------------------------------------+------"#,
            r#" 0 | s1   -   -   -  s2   -  s3  s4   -  |   5  "#,
            r#" 1 | s1   -   -   -  s2   -  s3  s4   -  |   6  "#,
            r#" 2 | s1   -   -   -  s2   -  s3  s4   -  |   7  "#,
            r#" 3 | r7  r7  r7  r7   -  r7   -   -  r7  |   -  "#,
            r#" 4 | r8  r8  r8  r8   -  r8   -   -  r8  |   -  "#,
            r#" 5 | s8  s9  s10 s11  -   -   -   -  acc |   -  "#,
            r#" 6 | r0  r0  r0  r0   -  r0   -   -  r0  |   -  "#,
            r#" 7 | s8  s9  s10 s11  -  s12  -   -   -  |   -  "#,
            r#" 8 | s1   -   -   -  s2   -  s3  s4   -  |  13  "#,
            r#" 9 | s1  s14  -   -  s2   -  s3  s4   -  |  15  "#,
            r#"10 | s1   -   -   -  s2   -  s3  s4   -  |  16  "#,
            r#"11 | s1   -   -   -  s2   -  s3  s4   -  |  17  "#,
            r#"12 | r6  r6  r6  r6   -  r6   -   -  r6  |   -  "#,
            r#"13 | r5  s9  s10 r5   -  r5   -   -  r5  |   -  "#,
            r#"14 | s1   -   -   -  s2   -  s3  s4   -  |  18  "#,
            r#"15 | r2  s9  r2  r2   -  r2   -   -  r2  |   -  "#,
            r#"16 | r3  s9  r3  r3   -  r3   -   -  r3  |   -  "#,
            r#"17 | r4  s9  s10 r4   -  r4   -   -  r4  |   -  "#,
            r#"18 | r1  s9  r1  r1   -  r1   -   -  r1  |   -  "#,
            r#"---+-------------------------------------+------"#,
        ], 8, &[
            r#"   | "-" "*" "/" "+" "(" ")" Id  Num  $  | amb_i"#,
            r#"---+-------------------------------------+------"#,
            r#" 0 | s1   -   -   -  s2   -  s8  s9   -  |  10  "#,
            r#" 1 | s1   -   -   -  s2   -  s8  s9   -  |  11  "#,
            r#" 2 | s1   -   -   -  s2   -  s8  s9   -  |  12  "#,
            r#" 3 | s1   -   -   -  s2   -  s8  s9   -  |  14  "#,
            r#" 4 | s1  s7   -   -  s2   -  s8  s9   -  |  15  "#,
            r#" 5 | s1   -   -   -  s2   -  s8  s9   -  |  16  "#,
            r#" 6 | s1   -   -   -  s2   -  s8  s9   -  |  17  "#,
            r#" 7 | s1   -   -   -  s2   -  s8  s9   -  |  18  "#,
            r#" 8 | r7  r7  r7  r7   -  r7   -   -  r7  |      "#,
            r#" 9 | r8  r8  r8  r8   -  r8   -   -  r8  |      "#,
            r#"10 | s3  s4  s5  s6   -   -   -   -  acc |      "#,
            r#"11 | r0  r0  r0  r0   -  r0   -   -  r0  |      "#,
            r#"12 | s3  s4  s5  s6   -  s13  -   -   -  |      "#,
            r#"13 | r6  r6  r6  r6   -  r6   -   -  r6  |      "#,
            r#"14 | r5  s4  s5  r5   -  r5   -   -  r5  |      "#,
            r#"15 | r2  s4  r2  r2   -  r2   -   -  r2  |      "#,
            r#"16 | r3  s4  r3  r3   -  r3   -   -  r3  |      "#,
            r#"17 | r4  s4  s5  r4   -  r4   -   -  r4  |      "#,
            r#"18 | r1  s4  r1  r1   -  r1   -   -  r1  |      "#,
            r#"---+-------------------------------------+------"#,
        ], &[
            r#"state 15 for "*": r2 (amb_i -> amb_i "*" amb_i) vs s9 (amb_i -> amb_i "*" • "*" amb_i, amb_i -> amb_i "*" • amb_i), conflicting priorities"#,
            r#"state 16 for "*": r3 (amb_i -> amb_i "*" amb_i) vs s9 (amb_i -> amb_i "*" • "*" amb_i, amb_i -> amb_i "*" • amb_i), conflicting priorities"#,
            r#"state 18 for "*": r1 (amb_i -> <R> amb_i "*" "*" amb_i) vs s9 (amb_i -> amb_i "*" • "*" amb_i, amb_i -> amb_i "*" • amb_i), conflicting priorities"#,
        ]),
        (2006, 0, 0, &[
            // - 0: s -> "if" Num "then" s "else" s
            // - 1: s -> "if" Num "then" s
            // - 2: s -> Id
            // - 3: <goal> -> s
            //
            r#"  | "if" Num "then" "else" Id  $  | s"#,
            r#"--+-------------------------------+--"#,
            r#"0 |  s1   -    -      -    s2  -  | 3"#,
            r#"1 |  -   s4    -      -    -   -  | -"#,
            r#"2 |  -    -    -      r2   -  r2  | -"#,
            r#"3 |  -    -    -      -    -  acc | -"#,
            r#"4 |  -    -    s5     -    -   -  | -"#,
            r#"5 |  s1   -    -      -    s2  -  | 6"#,
            r#"6 |  -    -    -      s7   -  r1  | -"#,
            r#"7 |  s1   -    -      -    s2  -  | 8"#,
            r#"8 |  -    -    -      r0   -  r0  | -"#,
            r#"--+-------------------------------+--"#,
        ], 3, &[
            r#"  | "if" Num "then" "else" Id  $  | s"#,
            r#"--+-------------------------------+--"#,
            r#"0 |  s3   -    -      -    s4  -  | 5"#,
            r#"1 |  s3   -    -      -    s4  -  | 7"#,
            r#"2 |  s3   -    -      -    s4  -  | 8"#,
            r#"3 |  -   s6    -      -    -   -  |  "#,
            r#"4 |  -    -    -      r2   -  r2  |  "#,
            r#"5 |  -    -    -      -    -  acc |  "#,
            r#"6 |  -    -    s1     -    -   -  |  "#,
            r#"7 |  -    -    -      s2   -  r1  |  "#,
            r#"8 |  -    -    -      r0   -  r0  |  "#,
            r#"--+-------------------------------+--"#,
        ], &[
            // conflict in state 6 for "else": r1 (s -> "if" Num "then" s) vs s7 (s -> "if" Num "then" s "else" • s) => resolved as s7
        ]),
        (2007, 0, 0, &[
            // - 0: s -> "if" Num "then" s
            // - 1: s -> "if" Num "then" s "else" s
            // - 2: s -> Id
            // - 3: <goal> -> s
            //
            r#"  | "if" Num "then" "else" Id  $  | s"#,
            r#"--+-------------------------------+--"#,
            r#"0 |  s1   -    -      -    s2  -  | 3"#,
            r#"1 |  -   s4    -      -    -   -  | -"#,
            r#"2 |  -    -    -      r2   -  r2  | -"#,
            r#"3 |  -    -    -      -    -  acc | -"#,
            r#"4 |  -    -    s5     -    -   -  | -"#,
            r#"5 |  s1   -    -      -    s2  -  | 6"#,
            r#"6 |  -    -    -      r0   -  r0  | -"#,
            r#"7 |  s1   -    -      -    s2  -  | 8"#,
            r#"8 |  -    -    -      r1   -  r1  | -"#,
            r#"--+-------------------------------+--"#,
        ], 3, &[
            r#"  | "if" Num "then" "else" Id  $  | s"#,
            r#"--+-------------------------------+--"#,
            r#"0 |  s3   -    -      -    s4  -  | 5"#,
            r#"1 |  s3   -    -      -    s4  -  | 7"#,
            r#"2 |  s3   -    -      -    s4  -  | 8"#,
            r#"3 |  -   s6    -      -    -   -  |  "#,
            r#"4 |  -    -    -      r2   -  r2  |  "#,
            r#"5 |  -    -    -      -    -  acc |  "#,
            r#"6 |  -    -    s1     -    -   -  |  "#,
            r#"7 |  -    -    -      r0   -  r0  |  "#,
            r#"8 |  -    -    -      r1   -  r1  |  "#,
            r#"--+-------------------------------+--"#,
        ], &[
            // conflict in state 6 for "else": r0 (s -> "if" Num "then" s) vs s7 (s -> "if" Num "then" s "else" • s) => resolved as r0
        ]),
        (2008, 0, 0, &[
            // - 0: prog -> head inst ";"
            // - 1: head -> "fn" Id ":"
            // - 2: inst -> Type ids
            // - 3: inst -> "typedef" Type Id
            // - 4: inst -> Id "=" Num
            // - 5: ids -> ids "," Id
            // - 6: ids -> Id
            // - 7: <goal> -> prog
            //
            r#"   | Type ";" "fn" Id  ":" "typedef" "=" Num ","  $  | prog head inst ids"#,
            r#"---+-------------------------------------------------+-------------------"#,
            r#" 0 |  -    -   s1   -   -      -      -   -   -   -  |  2    3    -    - "#,
            r#" 1 |  -    -   -   s4   -      -      -   -   -   -  |  -    -    -    - "#,
            r#" 2 |  -    -   -    -   -      -      -   -   -  acc |  -    -    -    - "#,
            r#" 3 |  s5   -   -   s6   -     $7      -   -   -   -  |  -    -    8    - "#,
            r#" 4 |  -    -   -    -  $9      -      -   -   -   -  |  -    -    -    - "#,
            r#" 5 |  -    -   -   s10  -      -      -   -   -   -  |  -    -    -   11 "#,
            r#" 6 |  -    -   -    -   -      -     s12  -   -   -  |  -    -    -    - "#,
            r#" 7 | s13   -   -    -   -      -      -   -   -   -  |  -    -    -    - "#,
            r#" 8 |  -   s14  -    -   -      -      -   -   -   -  |  -    -    -    - "#,
            r#" 9 |  r1   -   -   r1   -     r1      -   -   -   -  |  -    -    -    - "#,
            r#"10 |  -   r6   -    -   -      -      -   -  r6   -  |  -    -    -    - "#,
            r#"11 |  -   r2   -    -   -      -      -   -  s15  -  |  -    -    -    - "#,
            r#"12 |  -    -   -    -   -      -      -  s16  -   -  |  -    -    -    - "#,
            r#"13 |  -    -   -   s17  -      -      -   -   -   -  |  -    -    -    - "#,
            r#"14 |  -    -   -    -   -      -      -   -   -  r0  |  -    -    -    - "#,
            r#"15 |  -    -   -   s18  -      -      -   -   -   -  |  -    -    -    - "#,
            r#"16 |  -   r4   -    -   -      -      -   -   -   -  |  -    -    -    - "#,
            r#"17 |  -   r3   -    -   -      -      -   -   -   -  |  -    -    -    - "#,
            r#"18 |  -   r5   -    -   -      -      -   -  r5   -  |  -    -    -    - "#,
            r#"---+-------------------------------------------------+-------------------"#,
        ], 3, &[
            r#"   | Type ";" "fn" Id  ":" "typedef" "=" Num ","  $  | prog head inst ids"#,
            r#"---+-------------------------------------------------+-------------------"#,
            r#" 0 |  -    -   s3   -   -      -      -   -   -   -  |  4    1    -    - "#,
            r#" 1 |  s2   -   -   s6   -     $7      -   -   -   -  |  -    -    8    - "#,
            r#" 2 |  -    -   -   s10  -      -      -   -   -   -  |  -    -    -   11 "#,
            r#" 3 |  -    -   -   s5   -      -      -   -   -   -  |                   "#,
            r#" 4 |  -    -   -    -   -      -      -   -   -  acc |                   "#,
            r#" 5 |  -    -   -    -  $9      -      -   -   -   -  |                   "#,
            r#" 6 |  -    -   -    -   -      -     s12  -   -   -  |                   "#,
            r#" 7 | s13   -   -    -   -      -      -   -   -   -  |                   "#,
            r#" 8 |  -   s14  -    -   -      -      -   -   -   -  |                   "#,
            r#" 9 |  r1   -   -   r1   -     r1      -   -   -   -  |                   "#,
            r#"10 |  -   r6   -    -   -      -      -   -  r6   -  |                   "#,
            r#"11 |  -   r2   -    -   -      -      -   -  s15  -  |                   "#,
            r#"12 |  -    -   -    -   -      -      -  s16  -   -  |                   "#,
            r#"13 |  -    -   -   s17  -      -      -   -   -   -  |                   "#,
            r#"14 |  -    -   -    -   -      -      -   -   -  r0  |                   "#,
            r#"15 |  -    -   -   s18  -      -      -   -   -   -  |                   "#,
            r#"16 |  -   r4   -    -   -      -      -   -   -   -  |                   "#,
            r#"17 |  -   r3   -    -   -      -      -   -   -   -  |                   "#,
            r#"18 |  -   r5   -    -   -      -      -   -  r5   -  |                   "#,
            r#"---+-------------------------------------------------+-------------------"#,
        ], &[]),
        (2400, 0, 0, &[
            // - 0: s -> "a" a "d"
            // - 1: s -> "b" b "d"
            // - 2: s -> "a" b "e"
            // - 3: s -> "b" a "e"
            // - 4: a -> "c"
            // - 5: b -> "c"
            // - 6: <goal> -> s
            //
            // state 0:                     state 5:
            // - s -> • "a" a "d"           - s -> "a" a • "d"
            // - s -> • "b" b "d"           state 6:
            // - s -> • "a" b "e"           - s -> "a" b • "e"
            // - s -> • "b" a "e"           state 7:
            // - <goal> -> • s              - s -> "b" a • "e"
            // state 1:                     state 8:
            // - a -> • "c"                 - s -> "b" b • "d"
            // - b -> • "c"                 state 9:
            // - s -> "a" • a "d"           - s -> "a" a "d" •, [$]
            // - s -> "a" • b "e"           state 10:
            // state 2:                     - s -> "a" b "e" •, [$]
            // - a -> • "c"                 state 11:
            // - b -> • "c"                 - s -> "b" a "e" •, [$]
            // - s -> "b" • b "d"           state 12:
            // - s -> "b" • a "e"           - s -> "b" b "d" •, [$]
            // state 3:
            // - <goal> -> s •, [$]
            // state 4:
            // - a -> "c" •, ["d","e"]      <== this state should be split for states 1 & 2
            // - b -> "c" •, ["d","e"]
            //
            r#"   | "a" "d" "b" "e" "c"  $  | s  a  b "#,
            r#"---+-------------------------+---------"#,
            r#" 0 | s1   -  s2   -   -   -  | 3  -  - "#,
            r#" 1 |  -   -   -   -  s4   -  | -  5  6 "#,
            r#" 2 |  -   -   -   -  s4   -  | -  7  8 "#,
            r#" 3 |  -   -   -   -   -  acc | -  -  - "#,
            r#" 4 |  -  r4   -  r4   -   -  | -  -  - "#,
            r#" 5 |  -  s9   -   -   -   -  | -  -  - "#,
            r#" 6 |  -   -   -  s10  -   -  | -  -  - "#,
            r#" 7 |  -   -   -  s11  -   -  | -  -  - "#,
            r#" 8 |  -  s12  -   -   -   -  | -  -  - "#,
            r#" 9 |  -   -   -   -   -  r0  | -  -  - "#,
            r#"10 |  -   -   -   -   -  r2  | -  -  - "#,
            r#"11 |  -   -   -   -   -  r3  | -  -  - "#,
            r#"12 |  -   -   -   -   -  r1  | -  -  - "#,
            r#"---+-------------------------+---------"#,
        ], 3, &[
            r#"   | "a" "d" "b" "e" "c"  $  | s  a  b "#,
            r#"---+-------------------------+---------"#,
            r#" 0 | s1   -  s2   -   -   -  | 3  -  - "#,
            r#" 1 |  -   -   -   -  s4   -  | -  5  6 "#,
            r#" 2 |  -   -   -   -  s4   -  | -  7  8 "#,
            r#" 3 |  -   -   -   -   -  acc |         "#,
            r#" 4 |  -  r4   -  r4   -   -  |         "#,
            r#" 5 |  -  s9   -   -   -   -  |         "#,
            r#" 6 |  -   -   -  s10  -   -  |         "#,
            r#" 7 |  -   -   -  s11  -   -  |         "#,
            r#" 8 |  -  s12  -   -   -   -  |         "#,
            r#" 9 |  -   -   -   -   -  r0  |         "#,
            r#"10 |  -   -   -   -   -  r2  |         "#,
            r#"11 |  -   -   -   -   -  r3  |         "#,
            r#"12 |  -   -   -   -   -  r1  |         "#,
            r#"---+-------------------------+---------"#,
        ], &[
            r#"- calc_table: conflict in state 4 for "d": r4/r5"#,
            r#"- calc_table: conflict in state 4 for "e": r4/r5"#,
        ]),
        // non-LR(1) grammar
        (2500, 0, 0, &[
            r#"  | A  B   $  | s a"#,
            r#"--+-----------+----"#,
            r#"0 | s1 s2  -  | 3 -"#,
            r#"1 | s4 -   -  | - 5"#,
            r#"2 | s4 -   -  | - 6"#,
            r#"3 | -  -  acc | - -"#,
            r#"4 | s7 r2  -  | - -"#,
            r#"5 | s8 -   -  | - -"#,
            r#"6 | -  s9  -  | - -"#,
            r#"7 | r3 r3  -  | - -"#,
            r#"8 | -  -  r0  | - -"#,
            r#"9 | -  -  r1  | - -"#,
            r#"--+-----------+----"#,
        ], 3, &[
            r#"  | A  B   $  | s a"#,
            r#"--+-----------+----"#,
            r#"0 | s1 s2  -  | 3 -"#,
            r#"1 | s4 -   -  | - 5"#,
            r#"2 | s4 -   -  | - 6"#,
            r#"3 | -  -  acc |    "#,
            r#"4 | s7 r2  -  |    "#,
            r#"5 | s8 -   -  |    "#,
            r#"6 | -  s9  -  |    "#,
            r#"7 | r3 r3  -  |    "#,
            r#"8 | -  -  r0  |    "#,
            r#"9 | -  -  r1  |    "#,
            r#"--+-----------+----"#,
        ], &[
            r#"- calc_table: conflict in state 4 for "A": s7/r2"#,
        ]),
        /* template:
        (, 0, 0, &[
        ], 0, &[
        ], &[]),
        */
    ];
    static INDENT0: &str = "        ";
    static INDENT1: &str = "            ";
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;
    const SHOW_RULES: bool = false;
    const SHOW_STATES: bool = false;
    let mut errors = 0;
    for &(test_id, start, expected_warnings, expected_lines, expected_ngoto, expected_compressed, expected_conflict) in TESTS {
        // if !matches!(test_id, 2006..2008) { continue }
        let expected_lines = expected_lines.into_iter().map(|s| s.to_string()).to_vec();
        if VERBOSE && !SHOW_ANSWER_ONLY {
            println!("{:=<80}\ntest {test_id}:", "");
        }
        let msg = format!("## ERROR ## test {test_id}, start={start}");
        let testrules = TestRules::new(test_id);
        let mut lr = testrules.to_prs_lr().unwrap();
        lr.set_start(start);
        let (mut parsing_table, states) = lr.make_parsing_table_with_states_lalr();
        parsing_table.apply_terminal_hooks(&lr.terminal_hooks, &mut lr.log);
        let mut compressed_table = parsing_table.clone();
        compressed_table.compress_goto();
        let fail = if lr.has_no_errors() {
            let LRParsingTable { num_t_full, num_states, alts, action, .. } = &parsing_table;
            if VERBOSE {
                let text = lr.log.get_messages().map(|m| m.to_string()).filter(|s| s.contains("calc_table")).to_vec();
                if !text.is_empty() {
                    println!("logs related to parsing table:\n{}", text.join("\n"));
                }
            }
            let result_conflict = lr.log.get_warnings()
                .map(|w| w.get_inner_str())
                .filter(|s| s.contains("calc_table: conflict"))
                .to_vec();
            let result_warnings = lr.log.num_warnings() - result_conflict.len();
            let has_conflict = !result_conflict.is_empty();
            if VERBOSE && action.len() != num_t_full * num_states {
                println!("{msg}: incorrect action table size");
            }
            let result_lines = parsing_table.to_str(lr.get_symbol_table());
            let result_ngoto = compressed_table.num_goto;
            let result_compressed = compressed_table.to_str(lr.get_symbol_table());
            if VERBOSE || SHOW_ANSWER_ONLY {
                if !SHOW_ANSWER_ONLY {
                    println!("table has {} conflict(s)", result_conflict.len());
                }
                println!("        ({test_id}, {start}, {result_warnings}, &[");
                if VERBOSE || SHOW_RULES {
                    print_alts(&alts, lr.get_symbol_table());
                    println!("{INDENT1}//");
                }
                if VERBOSE || SHOW_STATES {
                    let str = states.iter().enumerate()
                        .map(|(i, items)|
                            format!("{INDENT1}// state {i}:{}", items.iter().map(|i| format!("\n{INDENT1}// - {}", lr.item_to_str(i))).join("")))
                        .join("\n");
                    println!("{str}\n{INDENT1}//");
                }
                println!("{}", result_lines.iter().map(|s| format!("{INDENT1}r#\"{s}\"#,")).join("\n"));
                let compressed_str = result_compressed.iter().map(|s| format!("\n{INDENT1}r#\"{s}\"#,")).join("");
                if has_conflict {
                    println!(
                        "{INDENT0}], {result_ngoto}, &[{compressed_str}\n{INDENT0}], &[{}\n{INDENT0}]),",
                        result_conflict.iter().map(|s| format!("\n{INDENT1}r#\"{s}\"#,")).join(""));
                } else {
                    println!("{INDENT0}], {result_ngoto}, &[{compressed_str}\n{INDENT0}], &[]),", );
                }
            }
            let conflict_mismatch = expected_conflict.len() != result_conflict.len()
                || result_conflict.iter().zip(expected_conflict).any(|(&r, &e)| !r.contains(e));
            [
                false,
                conflict_mismatch,
                result_lines != expected_lines,
                result_ngoto != expected_ngoto,
                result_compressed != expected_compressed,
                !lr.log.has_no_errors(),
                result_warnings != expected_warnings,
            ]
        } else {
            [true, false, false, false, false, false, false]
        };
        if fail.iter().any(|f| *f) {
            errors += 1;
            if !SHOW_ANSWER_ONLY {
                print!("## ERROR ## test {test_id} failed");
                if fail[0] { print!(", couldn't generate parsing table"); }
                if fail[1] { print!(", conflicts mismatch"); }
                if fail[2] { print!(", wrong result"); }
                if fail[3] { print!(", wrong number of gotos"); }
                if fail[4] { print!(", wrong compressed result"); }
                if fail[5] { print!(", errors in log"); }
                if fail[6] { print!(", warnings in log"); }
                println!();
                if fail[0] || fail[5] || fail[6] {
                    println!("Log:\n{}", lr.log);
                }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}

mod parse {
    use lexigram_core::log::{BufLog, LogStatus, Logger};
    use lexigram_core::parser::{Call, ListenerWrapper, Symbol};
    use lexigram_core::{AltId, CollectJoin, VarId};
    use lexigram_core::parser::lr::LRParser;
    use crate::grammar::tests::TestRules;
    use crate::{make_stream, SymbolTable, LALR};
    use crate::build::BuildFrom;
    use crate::parsergen::lr::LRParserTables;

    #[test]
    fn make_parser_lalr() {
        struct Stub<'a> {
            log: BufLog,
            symtab: Option<&'a SymbolTable>,
        }

        impl ListenerWrapper for Stub<'_> {
            fn switch(&mut self, call: Call, nt: VarId, alt_id: AltId, t_data: Option<Vec<String>>) {
                if VERBOSE {
                    println!(
                        "=> call {call:?}, nt {}, alt {alt_id}, t_data [{}]",
                        Symbol::NT(nt).to_str(self.symtab),
                        t_data.map(|v| v.into_iter().map(|s| format!("{s:?}")).join(", ")).unwrap_or_else(|| String::new())
                    );
                }
            }

            fn get_log_mut(&mut self) -> &mut impl Logger {
                &mut self.log
            }
        }

        let tests = vec![
            // test_id, start, id_id, num_id, sequences to test
            (102, 0, 1, 999, vec![
                ("A x y z C", None),
            ]),
            (103, 0, 1, 999, vec![
                ("A x y z C", None),
            ]),
            (121, 0, 2, 999, vec![
                ("A x y z C", None),
            ]),
            (122, 0, 2, 999, vec![
                ("A x y z C", None),
            ]),
            (601, 0, 3, 2, vec![
                ("1 + 2 * 3", None),
            ]),
            (2000, 0, 2, 999, vec![
                // s -> "a" a "a" | "a" "a" "b" | "b" a "b";
                // a -> b c;
                // b -> "a";
                // c -> d;
                // d -> ε;
                ("a a a", None),
                ("a a b", None),
                ("b a b", None),
                ("a b", Some([r#"syntax error: unexpected token 'b' on "b", line 1, col 2"#])),
            ]),
            (2006, 0, 4, 1, vec![
                // - 0: s -> "if" Num "then" s "else" s
                // - 1: s -> "if" Num "then" s
                // - 2: s -> Id
                ("if 1 then if 2 then t2", None),
                //                    ^^
                //          ^^^^^^^^^^^^
                //^^^^^^^^^^^^^^^^^^^^^^ => if 1 then { if 2 then t2 }
                ("if 1 then if 2 then t2 else e2", None),
                //                    ^^
                //                            ^^
                //          ^^^^^^^^^^^^^^^^^^^^
                //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ => if 1 then { if 2 then t2 else e2 }
                ("if 1 then if 2 then t2 else e2 else e1", None),
                //                    ^^
                //                            ^^
                //          ^^^^^^^^^^^^^^^^^^^^
                //                                    ^^
                //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ => if 1 then { if 2 then t2 else e2 } else e1
            ]),
            (2007, 0, 4, 1, vec![
                // - 0: s -> "if" Num "then" s
                // - 1: s -> "if" Num "then" s "else" s
                // - 2: s -> Id
                ("if 1 then if 2 then t2", None),
                //                    ^^
                //          ^^^^^^^^^^^^
                //^^^^^^^^^^^^^^^^^^^^^^ => if 1 then { if 2 then t2 }
                ("if 1 then if 2 then t2 else e2", Some([r#"syntax error: unexpected token 'else' on "else", line 1, col 8"#])),
                //                    ^^
                //          ^^^^^^^^^^^^
                //^^^^^^^^^^^^^^^^^^^^^^
                //                       ^^^^ error
                ("if 1 then if 2 then t2 else e2 else e1", Some([r#"syntax error: unexpected token 'else' on "else", line 1, col 8"#])),
                //                    ^^
                //          ^^^^^^^^^^^^
                //^^^^^^^^^^^^^^^^^^^^^^
                //                       ^^^^ error
            ]),
            (2400, 0, 999, 999, vec![
                ("a c d", None),
                // ("b c d", None),     // <=== should work, but states got merged in LALR
                // ("a c e", None),     // <=== should work, but states got merged in LALR
                ("b c e", None),
            ]),
            /*
            (, 0, id, num, vec![
                ("", None),
            ]),
            */
        ];
        const VERBOSE: bool = false;
        for (test_id, (grammar_id, start, id_id, num_id, sequences)) in tests.into_iter().enumerate() {
            if !matches!(grammar_id, 2006|2007) { continue }
            if VERBOSE { println!("{:=<80}\ntest {test_id} with parser {grammar_id:?}/{start}", ""); }
            let mut lalr1 = TestRules::new(grammar_id).to_prs_lr().unwrap();
            lalr1.set_start(start);
            let symtab = lalr1.symbol_table.clone();
            if VERBOSE {
                lalr1.print_alts();
                println!("parsing table:\n{}", lalr1.make_parsing_table_lalr(false).to_str(lalr1.get_symbol_table()).join("\n"));
            }
            let ptables = LRParserTables::build_from(lalr1);
            let mut parser: LRParser<LALR> = ptables.make_parser();
            for (input, expected_errors) in sequences {
                let expected_errors = expected_errors.map(|v| v.to_vec());
                if VERBOSE { println!("{:-<60}\nnew input '{input}'", ""); }
                let stream = make_stream(input, symtab.as_ref().unwrap().get_terminals(), true, id_id, num_id, VERBOSE);
                let mut listener = Stub { log: BufLog::new(), symtab: symtab.as_ref() };
                let errors = match parser.parse_stream(&mut listener, stream) {
                    Ok(_) => {
                        if VERBOSE { println!("parsing completed successfully"); }
                        None
                    }
                    Err(e) => {
                        if VERBOSE { println!("parsing failed: {e}"); }
                        Some(listener.log.get_errors().map(|s| s.get_inner_str()).to_vec())
                    }
                };
                if VERBOSE {
                    let msg = listener.log.get_messages().map(|s| format!("- {s}")).join("\n");
                    if !msg.is_empty() {
                        println!("Messages:\n{msg}");
                    }
                }
                assert_eq!(errors, expected_errors, "test {test_id}/{grammar_id:?}/{start} failed for input {input}");
            }
        }
    }
}