#![cfg(test)]

use crate::*;
use crate::dfa::tests::{build_re, print_graph};
use super::*;

#[test]
fn lexgen_partition_symbols() {
    let tests = [
        (1, btreemap![
            0 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1],
            1 => branch!['0' => 2],
            2 => branch!['0' => 1, '1' => 2, '2' => 2, '3' => 2],
            3 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 3]
        ], btreeset!["0", "12", "3"]),
        (2, btreemap![
            0 => branch!['0' => 3, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4],
            1 => branch!['0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5],
            2 => branch!['0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6,
                'A' => 6, 'B' => 6, 'C' => 6, 'D' => 6, 'E' => 6, 'F' => 6, 'a' => 6, 'b' => 6, 'c' => 6, 'd' => 6, 'e' => 6, 'f' => 6],
            3 => branch!['.' => 1, '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4, 'x' => 2],
            4 => branch!['.' => 1, '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4],
            5 => branch!['0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5],
            6 => branch!['0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6,
                'A' => 6, 'B' => 6, 'C' => 6, 'D' => 6, 'E' => 6, 'F' => 6, 'a' => 6, 'b' => 6, 'c' => 6, 'd' => 6, 'e' => 6, 'f' => 6],
        ], btreeset!["0", "123456789", "ABCDEFabcdef", ".", "x"]),
        (3, btreemap![
            0 => branch!['0' => 0, '1' => 1, '2' => 1, '3' => 1],
            1 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1],
            2 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 0]
        ], btreeset!["0", "12", "3"]),
    ];
    for (test_id, g, expected_groups) in tests {
        // println!("test {test_id}: --------------------------------------");
        let groups = partition_symbols(&g);
        let expected_groups = expected_groups.iter().map(|s| {
            let mut chars = s.chars().collect::<Vec<_>>();
            chars.sort();
            chars.iter().collect()
        }).collect();
        let result = groups.iter().map(|set| set.iter().map(|c| *c).collect::<String>()).collect::<BTreeSet<_>>();
        assert_eq!(result, expected_groups, "test {test_id} failed");
    }
}

#[test]
fn lexgen_symbol_tables() {
    let tests = [
        (2, btreemap![
            0 => branch!['0' => 3, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4],
            1 => branch!['0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5],
            2 => branch!['0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6,
                'A' => 6, 'B' => 6, 'C' => 6, 'D' => 6, 'E' => 6, 'F' => 6, 'a' => 6, 'b' => 6, 'c' => 6, 'd' => 6, 'e' => 6, 'f' => 6, '𝆕' => 6],
            3 => branch!['.' => 1, '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4, 'x' => 2, 'Δ' => 2],
            4 => branch!['.' => 1, '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4, '∑' => 4],
            5 => branch!['0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5, '∑' => 5],
            6 => branch!['0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6,
                'A' => 6, 'B' => 6, 'C' => 6, 'D' => 6, 'E' => 6, 'F' => 6, 'a' => 6, 'b' => 6, 'c' => 6, 'd' => 6, 'e' => 6, 'f' => 6, '𝆕' => 6],
        ], btreeset!["", "0", "123456789", "ABCDEFabcdef𝆕", ".", "xΔ", "∑"]),
    ];
    for (test_id, g, expected_set) in tests {
        let mut dfa = Dfa::from_graph(g, 0, btreemap![3 => Token(0), 4 => Token(0), 5 => Token(1), 6 => Token(2)]);
        dfa.normalize();
        let lexgen = LexGen::new(dfa);
        let mut ascii_vec = vec![BTreeSet::<char>::new(); lexgen.nbr_groups + 1];
        for i in 0..128_u8 {
            let c = char::from(i);
            let g = lexgen.ascii_to_group[i as usize];
            if g != 0 {
                ascii_vec[g].insert(c);
            }
        }
        for (c, g) in lexgen.utf8_to_group.iter() {
            ascii_vec[*g as usize].insert(*c);
        }
        let ascii_set = BTreeSet::from_iter(ascii_vec.iter().skip(1).map(|s| chars_to_string(s, false)));
        let expected_set = expected_set.iter().map(|s| s.to_string()).collect();
        assert_eq!(ascii_set, expected_set, "test {test_id} failed");
        print_source_code(&lexgen);
    }
}

#[test]
fn lexgen_state_tables() {
    let tests = vec![
        (10, btreemap![
            0 => vec!["0", "10", "9876543210"],
            1 => vec!["0.5", "9876543210.0123456789"],
            2 => vec!["0x0", "0x0123456789abcdef", "0x0123456789ABCDEF", "0xff"]
        ], vec!["a", ".5", "()", "9x0", "0x5y", "0.5a", "10f", ""])
    ];
    for (test_id, token_tests, err_tests) in tests {
        let mut dfa = DfaBuilder::new(build_re(test_id)).build();
        dfa.normalize();
        print_graph(&dfa);
        let lexgen = LexGen::new(dfa);
        print_source_code(&lexgen);
        for (exp_token, inputs) in token_tests {
            for input in inputs {
                let mut input = input.to_string();
                input.push(' ');
                // println!("{input}:");
                let result = sim_lexgen(&lexgen, input.clone());
                // println!("=> {}", result.clone().map(|t| format!("token {}", t.0)).unwrap_or("ERROR".to_string()));
                assert_eq!(result, Some(Token(exp_token)), "test {test_id} failed for input '{input}'");
            }
        }
        for input in err_tests {
            // println!("{input}:");
            let result = sim_lexgen(&lexgen, input.to_string());
            // println!("=> {}", result.clone().map(|t| format!("token {}", t.0)).unwrap_or("ERROR".to_string()));
            assert_eq!(result, None, "test {test_id} failed to trigger an error for input '{input}'");
        }

    }
}

fn sim_lexgen(lexgen: &LexGen, input: String) -> Option<Token> {
    const VERBOSE: bool = false;
    let mut state = lexgen.initial_state;
    let mut chars = input.chars();
    loop {
        if VERBOSE { print!("- state = {state}"); }
        if let Some(c) = chars.next() {
            let group = char_to_group(&lexgen.ascii_to_group, &lexgen.utf8_to_group, c);
            if VERBOSE { print!(", char '{c}' -> group {group}"); }
            if group == LexGen::ERROR_GROUP {
                if VERBOSE { println!(" <invalid input, stopping>"); }
                return if state >= lexgen.first_end_state && c.is_whitespace() {
                    Some(lexgen.token_table[state - lexgen.first_end_state].clone())
                } else {
                    None
                };
            } else {
                let new_state = lexgen.state_table[lexgen.nbr_groups * state + group];
                if new_state >= lexgen.nbr_states {
                    if VERBOSE { println!(" -> error"); }
                    return if state >= lexgen.first_end_state && c.is_whitespace() {
                        Some(lexgen.token_table[state - lexgen.first_end_state].clone())
                    } else {
                        None
                    };
                }
                if VERBOSE { println!(" -> state {new_state}"); }
                state = new_state;
            }
        } else {
            if VERBOSE { println!(" <end of input>"); }
            return if state >= lexgen.first_end_state { Some(lexgen.token_table[state - lexgen.first_end_state].clone()) } else { None };
        }
    }
}

fn print_source_code(lexgen: &LexGen) {
    // Create source code:
    println!("let ascii_to_group = [");
    for i in 0..8_usize {
        print!("    ");
        for j in 0..16_usize {
            print!("{:3}, ", lexgen.ascii_to_group[i * 16 + j]);
        }
        println!("  // {}-{}", i*16, i*16 + 15);
    }
    println!("];");
    println!("let utf8_to_group = hashmap![{}];",
             lexgen.utf8_to_group.iter().map(|(c, g)| format!("'{c}' => {g},")).collect::<String>()
    );
    println!("let nbr_groups = {};", lexgen.nbr_groups);
    println!("let initial_state = {};", lexgen.initial_state);
    println!("let first_end_state = {};", lexgen.first_end_state);
    println!("let error_state = {};", lexgen.nbr_states);
    println!("let token_table = [{}];", lexgen.token_table.iter().map(|t| t.0.to_string()).collect::<Vec<_>>().join(", "));
    println!("let state_table = [");
    for i in 0..lexgen.nbr_states {
        println!("    {}, // state {}{}",
            (0..lexgen.nbr_groups).map(|j| format!("{:3}", lexgen.state_table[i * lexgen.nbr_groups + j])).collect::<Vec<_>>().join(", "),
            i,
            if i >= lexgen.first_end_state { format!(" <END: {}>", lexgen.token_table[i - lexgen.first_end_state].0 ) } else { "".to_string() }
        );
    }
    println!("];")
}
