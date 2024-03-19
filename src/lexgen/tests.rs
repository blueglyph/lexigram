#![cfg(test)]

use crate::*;
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
        let dfa = Dfa::from_graph(g, 0, btreemap![3 => Token(0), 4 => Token(0), 5 => Token(1), 6 => Token(2)]);
        let mut lex = LexGen::new(dfa);
        lex.create_input_tables();
        let mut ascii_vec = vec![BTreeSet::<char>::new(); lex.nbr_groups + 1];
        for i in 0..128_u8 {
            let c = char::from(i);
            let g = lex.ascii_to_group[i as usize];
            if g != 0 {
                ascii_vec[g].insert(c);
            }
        }
        for (c, g) in lex.utf8_to_group.iter() {
            ascii_vec[*g as usize].insert(*c);
        }
        let ascii_set = BTreeSet::from_iter(ascii_vec.iter().skip(1).map(|s| chars_to_string(s, false)));
        let expected_set = expected_set.iter().map(|s| s.to_string()).collect();
        assert_eq!(ascii_set, expected_set, "test {test_id} failed");

        // // Create source code:
        // println!("let ascii_to_group = [");
        // for i in 0..8_usize {
        //     print!("    ");
        //     for j in 0..16_usize {
        //         print!("{:3}, ", lex.ascii_to_group[i * 16 + j]);
        //     }
        //     println!("  // {}-{}", i*16, i*16 + 15);
        // }
        // println!("];");
        // println!("let utf8_to_group = HashMap::<char, GroupId>::from([{}]);",
        //          lex.utf8_to_group.iter().map(|(c, g)| format!("('{c}', {g}),")).collect::<String>()
        // );
    }
}

