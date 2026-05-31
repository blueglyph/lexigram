use lexigram_core::{CollectJoin, VarId};
use lexigram_core::log::LogStatus;
use crate::grammar::lr::LRParsingTable;
use crate::grammar::tests::prs::print_alts;
use crate::grammar::tests::TestRules;

#[test]
fn prs_calc_lr_table() {
    static TESTS: &[(u32, VarId, usize, &[&str], &[&str])] = &[
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
        ], &[]),
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
        ], &[]),

        // resolved conflicts
        (601, 0, 0, &[
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
        ], &[]),

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
        ], &[
            "conflict for state 4, terminal A: s7/r2",
        ]),
        (102, 0, 0, &[
            r#"  | A  B  C   $  | a a_1"#,
            r#"--+--------------+------"#,
            r#"0 | s1 -  -   -  | 2  - "#,
            r#"1 | -  s3 r2  -  | -  4 "#,
            r#"2 | -  -  -  acc | -  - "#,
            r#"3 | -  s3 r2  -  | -  5 "#,
            r#"4 | -  -  s6  -  | -  - "#,
            r#"5 | -  -  r1  -  | -  - "#,
            r#"6 | -  -  -  r0  | -  - "#,
        ], &[]),
        (103, 0, 0, &[
            r#"  | A  B  C   $  | a a_1"#,
            r#"--+--------------+------"#,
            r#"0 | s1 -  -   -  | 2  - "#,
            r#"1 | -  s3 -   -  | -  4 "#,
            r#"2 | -  -  -  acc | -  - "#,
            r#"3 | -  s3 r2  -  | -  5 "#,
            r#"4 | -  -  s6  -  | -  - "#,
            r#"5 | -  -  r1  -  | -  - "#,
            r#"6 | -  -  -  r0  | -  - "#,
        ], &[]),
        /* template:
        (, 0, 0, &[
        ], &[]),
        */
    ];
    static INDENT0: &str = "        ";
    static INDENT1: &str = "            ";
    const VERBOSE: bool = false;
    const SHOW_ANSWER_ONLY: bool = false;
    const SHOW_RULES: bool = false;
    const SHOW_STATES: bool = true;
    let mut errors = 0;
    for &(test_id, start, expected_warnings, expected_lines, expected_conflict) in TESTS {
        // if !matches!(test_id, 601) { continue }
        let expected_lines = expected_lines.into_iter().map(|s| s.to_string()).to_vec();
        if VERBOSE && !SHOW_ANSWER_ONLY {
            println!("{:=<80}\ntest {test_id}:", "");
        }
        let msg = format!("## ERROR ## test {test_id}, start={start}");
        let mut lr = TestRules(test_id).to_prs_lr().unwrap();
        lr.set_start(start);
        let fail = if let Ok((parsing_table, states)) = lr.make_parsing_table_with_states_lalr() {
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
                    println!("{str}");
                }
                println!("{}", result_lines.iter().map(|s| format!("{INDENT1}r#\"{s}\"#,")).join("\n"));
                if has_conflict {
                    println!("{INDENT0}], &[{}\n{INDENT0}]),", result_conflict.iter().map(|s| format!("\n{INDENT1}\"{s}\",")).join(""));
                } else {
                    println!("{INDENT0}], &[]),");
                }
            }
            let conflict_mismatch = expected_conflict.len() != result_conflict.len()
                || result_conflict.iter().zip(expected_conflict).any(|(&r, &e)| !r.contains(e));
            [
                false,
                conflict_mismatch,
                result_lines != expected_lines,
                !lr.log.has_no_errors(),
                result_warnings != expected_warnings,
            ]
        } else {
            [true, false, false, false, false]
        };
        if fail.iter().any(|f| *f) {
            errors += 1;
            if !SHOW_ANSWER_ONLY {
                print!("## ERROR ## test {test_id} failed");
                if fail[0] { print!(", couldn't generate parsing table"); }
                if fail[1] { print!(", conflicts mismatch"); }
                if fail[2] { print!(", wrong result"); }
                if fail[3] { print!(", errors in log"); }
                if fail[4] { print!(", warnings in log"); }
                println!();
                if fail[0] || fail[3] || fail[4] {
                    println!("Log:\n{}", lr.log);
                }
            }
        }
    }
    assert!(errors == 0, "{errors} error(s)");
}
