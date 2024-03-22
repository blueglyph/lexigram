#![cfg(test)]

use std::io::Cursor;
use crate::*;
use crate::dfa::tests::{build_re, print_graph};
use crate::io::CharReader;
use crate::lexgen::interpreter::{LexInterpret, LexScanError};
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
        ], btreeset!["0", "123456789", "ABCDEFabcdef𝆕", ".", "xΔ", "∑"]),
    ];
    for (test_id, g, expected_set) in tests {
        let mut dfa = Dfa::from_graph(g, 0, btreemap![3 => Token(0), 4 => Token(0), 5 => Token(1), 6 => Token(2)]);
        dfa.normalize();
        let lexgen = LexGen::new(dfa);
        let mut ascii_vec = vec![BTreeSet::<char>::new(); lexgen.nbr_groups];
        for i in 0..128_u8 {
            let c = char::from(i);
            let g = lexgen.ascii_to_group[i as usize];
            if g < lexgen.nbr_groups {
                ascii_vec[g].insert(c);
            }
        }
        for (c, g) in lexgen.utf8_to_group.iter() {
            ascii_vec[*g as usize].insert(*c);
        }
        let ascii_set = BTreeSet::from_iter(ascii_vec.iter().map(|s| chars_to_string(s, false)));
        let expected_set = expected_set.iter().map(|s| s.to_string()).collect();
        assert_eq!(ascii_set, expected_set, "test {test_id} failed");
        // print_source_code(&lexgen);
    }
}

#[test]
fn lexgen_interpreter() {
    const VERBOSE: bool = false;

    fn eval(result: &Result<Token, LexScanError>, verbose: bool) -> Option<Token> {
        match result {
            Ok(token) => {
                if verbose { println!("=> OK {}", token.0); }
                Some(token.clone())
            }
            Err(e) => {
                if verbose { print!("## {e}"); }
                if e.token.is_some() {
                    if verbose { println!(" => OK"); }
                    e.token.clone()
                } else {
                    if verbose { println!(" => Error"); }
                    None
                }
            }
        }
    }

    let tests: Vec<(usize, BTreeMap<TokenId, Vec<&str>>, Vec<(&str, u64)>, Vec<(&str, Vec<TokenId>)>)> = vec![
        // [0-9]+(<end:0>|\.[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
        (10, btreemap![
            0 => vec!["0", "0 ", "10", "9876543210"],
            1 => vec!["0.5", "0.1 ", "9876543210.0123456789"],
            2 => vec!["0x0", "0xF ", "0x0123456789abcdef", "0x0123456789ABCDEF", "0xff"]
        ],
         vec![("a", 0), (".5", 0), ("()", 0), ("0xy", 2), ("0.a", 2), ("", 0)],
         vec![("0 1 2 ", vec![0, 0, 0]), ("0x1 0.5 15", vec![2, 1, 0])],
        ),

        // [a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>
        (11, btreemap![
            0 => vec!["a", "id", "id05b", "ifa", "printa"],
            1 => vec!["if", "if x"],
            2 => vec!["print", "print "],
            3 => vec!["=", "=5", "=a"],
            4 => vec!["+", "+5", "+a"],
            5 => vec![";", ";a"]
        ],
         vec![("0", 0), ("", 0), ("-", 0), ("*", 0)],
         vec![("\ta = x; if i=j print b;\n", vec![0, 3, 0, 5, 1, 0, 3, 0, 2, 0, 5])]
        ),
    ];
    for (test_id, token_tests, err_tests, stream_tests) in tests {
        let mut dfa = DfaBuilder::new(build_re(test_id)).build();
        dfa.normalize();
        if VERBOSE { print_graph(&dfa); }
        let lexgen = LexGen::new(dfa);
        if VERBOSE { print_source_code(&lexgen); }
        let mut interpret = LexInterpret::new(lexgen);
        for (exp_token, inputs) in token_tests {
            for input in inputs {
                if VERBOSE { println!("\"{input}\": (should succeed)"); }
                let stream = CharReader::new(Cursor::new(input));
                interpret.attach_steam(stream);
                let result = interpret.get_token();
                let token = eval(&result, VERBOSE);
                assert_eq!(token, Some(Token(exp_token)), "test {test_id} failed for input '{input}'");
                interpret.detach_stream();
            }
        }
        for (input, expected_pos) in err_tests {
            if VERBOSE { println!("\"{input}\": (should fail)"); }
            let stream = CharReader::new(Cursor::new(input));
            interpret.attach_steam(stream);
            let result = interpret.get_token();
            let token = eval(&result, VERBOSE);
            assert_eq!(token, None, "test {test_id} failed for input '{input}'");
            if let Err(e) = result {
                assert_eq!(e.pos, expected_pos)
            }
        }
        for (input, expected_tokens) in stream_tests {
            if VERBOSE { print!("\"{}\":", escape_string(input)); }
            let stream = CharReader::new(Cursor::new(input));
            interpret.attach_steam(stream);
            let mut result = Vec::new();
            while interpret.is_open() {
                let token = &interpret.get_token();
                let t = eval(token, false);
                if let Some(token) = t {
                    if VERBOSE { print!(" {}", token.0); }
                    result.push(token.0);
                } else {
                    assert_eq!(t, None);
                    break;
                }
            }
            if VERBOSE { println!(); }
            assert_eq!(result, expected_tokens, "test {test_id} failed for input '{input}'");
        }
    }
}

fn print_source_code(lexgen: &LexGen) {
    // Create source code:
    let mut groups = vec![BTreeSet::new(); lexgen.nbr_groups];
    println!("let ascii_to_group = [");
    for i in 0..8_usize {
        print!("    ");
        for j in 0..16_usize {
            let ascii = i * 16 + j;
            let group = lexgen.ascii_to_group[i * 16 + j];
            print!("{:3}, ", group);
            if group < lexgen.nbr_groups {
                groups[group].insert(char::from(ascii as u8));
            }
        }
        println!("  // {}-{}", i*16, i*16 + 15);
    }
    println!("];");
    for (g, chars) in groups.iter().enumerate() {
        println!("// group[{:3}] = [{}]", g, chars.iter().map(|c| escape_char(*c)).collect::<String>());
    }
    println!("let utf8_to_group = hashmap![{}];",
             lexgen.utf8_to_group.iter().map(|(c, g)| format!("'{}' => {},", escape_char(*c), g)).collect::<String>()
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
