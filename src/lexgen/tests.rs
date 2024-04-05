#![cfg(test)]

use crate::*;
use super::*;

#[cfg(disabled)]
#[test]
fn lexgen_partition_symbols() {
    let tests = [
        (1, btreemap![
            0 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1],
            1 => branch!['0' => 2],
            2 => branch!['0' => 1, '1' => 2, '2' => 2, '3' => 2],
            3 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 3]
        ], intervals!['0'; '1', '2'; '3']), //["0", "12", "3"]),
/*
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
        ], intervals!['0'; '1', '9'; 'A', 'F'; 'a', 'f'; '.'; 'x']), // btreeset!["0", "123456789", "ABCDEFabcdef", ".", "x"]),
        (3, btreemap![
            0 => branch!['0' => 0, '1' => 1, '2' => 1, '3' => 1],
            1 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1],
            2 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 0]
        ], intervals!['0'; '1', '2'; '3']), //btreeset!["0", "12", "3"]),
*/
    ];
    for (test_id, g, expected_groups) in tests {
        // println!("test {test_id}: --------------------------------------");
        let groups = partition_symbols(&g); // todo: adapt `branch!`, make fn normalize_graph(g: &mut BTreeMap<Intervals, StateId>) - already done in Dfa::calc_states()
        // let expected_groups = expected_groups.iter().map(|s| {
        //     let mut chars = s.chars().collect::<Vec<_>>();
        //     chars.sort();
        //     chars.iter().collect()
        // }).collect();
        // let result = groups.iter().map(|set| set.iter().map(|c| *c).collect::<String>()).collect::<BTreeSet<_>>();
        assert_eq!(groups, expected_groups, "test {test_id} failed");
    }
}

#[cfg(disabled)]
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
        let mut dfa = Dfa::from_graph(g, 0, btreemap![3 => term![=0], 4 => term![=0], 5 => term![=1], 6 => term![=2]]);
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

pub(crate) fn print_source_code(lexgen: &LexGen) {
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
    println!("let token_table = [{}];", lexgen.terminal_table.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "));
    println!("let state_table = [");
    for i in 0..lexgen.nbr_states {
        println!("    {}, // state {}{}",
            (0..lexgen.nbr_groups).map(|j| format!("{:3}", lexgen.state_table[i * lexgen.nbr_groups + j])).collect::<Vec<_>>().join(", "),
            i,
            if i >= lexgen.first_end_state { format!(" {}", lexgen.terminal_table[i - lexgen.first_end_state] ) } else { "".to_string() }
        );
    }
    println!("];")
}
