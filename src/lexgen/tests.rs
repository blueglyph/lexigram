#![cfg(test)]

use crate::*;
use crate::intervals::{Seg, SegMap};
use super::*;

#[test]
fn lexgen_partition_symbols() {
    let tests = [
        (1, btreemap![
            0 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1, 'x' => 1],
            1 => branch!['0' => 2],
            2 => branch!['0' => 1, '1' => 2, '2' => 2, '3' => 2, 'x' => 2],
            3 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 3, 'x' => 2]
        ], vec![intervals!['0'], intervals!['1'-'2', 'x'], intervals!['3']]),

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
        ], vec![intervals!['0'], intervals!['1'-'9'], intervals!['A'-'F', 'a'-'f'], intervals!['.'], intervals!['x']]),
        (3, btreemap![
            0 => branch!['0' => 0, '1' => 1, '2' => 1, '3' => 1],
            1 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1],
            2 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 0]
        ], vec![intervals!['0'], intervals!['1'-'2'], intervals!['3']]),

    ];
    for (test_id, g, expected_groups) in tests {
        // println!("test {test_id}: --------------------------------------");
        let groups = partition_symbols(&g);
        assert_eq!(groups, expected_groups, "test {test_id} failed");
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
        let mut dfa = Dfa::from_graph(g, 0, btreemap![3 => term![=0], 4 => term![=0], 5 => term![=1], 6 => term![=2]]);
        dfa.normalize();
        let lexgen = LexGen::from_dfa(&dfa);
        let mut ascii_vec = vec![BTreeSet::<char>::new(); lexgen.nbr_groups as usize];
        for i in 0..128_u8 {
            let c = char::from(i);
            let g = lexgen.ascii_to_group[i as usize];
            if g < lexgen.nbr_groups {
                ascii_vec[g as usize].insert(c);
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
fn lexgen_symbol_tables_corner() {
    let tests: Vec<(u32,
                    BTreeMap<StateId, BTreeMap<Intervals, StateId>>,    // graph
                    BTreeMap<String, GroupId>,                          // ASCII (each string is a group of chars)
                    BTreeMap<String, GroupId>,                          // UTF-8 (each string is a group of chars)
                    BTreeMap<Seg, GroupId>)>                            // what didn't fit in UTF-8
        = vec![
        (1, btreemap![
            0 => branch!(127 => 0, 128 => 0, 129 => 0)
        ], btreemap!["\u{7f}".to_string() => 0], btreemap!["\u{80}".to_string() => 0], btreemap![Seg(129, 129) => 0]),
        (1024, btreemap![
            0 => branch!()
        ], btreemap![], btreemap![], btreemap![]),
        (32, btreemap![
            0 => BTreeMap::from_iter((0_u32..16).map(|x| (Intervals::new(Seg(x*16, x*16+15)), x as StateId))),
            1 => branch!(), 2 => branch!(), 3 => branch!(), 4 => branch!(), 5 => branch!(), 6 => branch!(), 7 => branch!(), 8 => branch!(),
            9 => branch!(), 10 => branch!(), 11 => branch!(), 12 => branch!(), 13 => branch!(), 14 => branch!(), 15 => branch!()
        ],
         BTreeMap::from_iter((0_u32..8).map(|x| ((0..16).map(|y| char::from_u32(x * 16 + y).unwrap()).collect::<String>(), x as GroupId))),
         BTreeMap::from_iter((8_u32..10).map(|x| ((0..16).map(|y| char::from_u32(x * 16 + y).unwrap()).collect::<String>(), x as GroupId))),
         BTreeMap::from_iter((10_u32..16).map(|x| (Seg(x * 16, x * 16 + 15), x as GroupId)))
        ),
    ];
    const VERBOSE: bool = true;
    for (test_id, (left, g, ascii, utf8, seg)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("Test {test_id}:"); }
        let end_states = g.values().flat_map(|x| x.values()).cloned().collect::<BTreeSet<StateId>>();
        let mut dfa = Dfa::from_graph(g, 0, end_states.iter().map(|s| (*s, term!(=0))).collect::<BTreeMap<StateId, Terminal>>());
        dfa.normalize();
        let mut lexgen = LexGen::new();
        lexgen.max_utf8_chars = left;
        lexgen.build(&dfa);
        let error_id = lexgen.nbr_groups as GroupId;
        let mut exp_ascii = vec![error_id; 128].into_boxed_slice();
        for (s, id) in ascii {
            for b in s.chars() {
                exp_ascii[b as usize] = id;
            }
        }
        let mut exp_utf8 = Box::new(HashMap::<char, GroupId>::new());
        for (s, id) in utf8 {
            for u in s.chars() {
                exp_utf8.insert(u, id);
            }
        }
        println!("LexGen: {}", std::mem::size_of::<LexGen>());
        println!("lexgen: {}", std::mem::size_of_val(&lexgen));
        println!("ascii_to_group: {}", std::mem::size_of_val(&lexgen.ascii_to_group));
        println!("utf8_to_group:  {}", std::mem::size_of_val(&lexgen.utf8_to_group));
        println!("seg_to_group:   {}", std::mem::size_of_val(&lexgen.seg_to_group));
        let exp_seg = SegMap::from_iter(seg.into_iter());
        assert_eq!(lexgen.ascii_to_group, exp_ascii, "test {test_id} failed");
        //assert_eq!(BTreeMap::from_iter(lexgen.utf8_to_group.iter()), BTreeMap::from_iter(exp_utf8.iter()), "test {test_id} failed");
        assert_eq!(lexgen.utf8_to_group, exp_utf8, "test {test_id} failed");
        assert_eq!(lexgen.seg_to_group, exp_seg, "test {test_id} failed");
    }
}

pub(crate) fn print_source_code(lexgen: &LexGen) {
    // Create source code:
    let mut groups = vec![BTreeSet::new(); lexgen.nbr_groups as usize];
    println!("let ascii_to_group = [");
    for i in 0..8_usize {
        print!("    ");
        for j in 0..16_usize {
            let ascii = i * 16 + j;
            let group = lexgen.ascii_to_group[i * 16 + j];
            print!("{:3}, ", group);
            if group < lexgen.nbr_groups {
                groups[group as usize].insert(char::from(ascii as u8));
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
    println!("let seg_to_group = SegMap::from_iter(...);");
    println!("let nbr_groups = {};", lexgen.nbr_groups);
    println!("let initial_state = {};", lexgen.initial_state);
    println!("let first_end_state = {};", lexgen.first_end_state);
    println!("let error_state = {};", lexgen.nbr_states);
    println!("let token_table = [{}];", lexgen.terminal_table.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "));
    println!("let state_table = [");
    for i in 0..lexgen.nbr_states as usize {
        println!("    {}, // state {}{}",
            (0..lexgen.nbr_groups as usize).map(|j| format!("{:3}", lexgen.state_table[i * lexgen.nbr_groups as usize + j])).collect::<Vec<_>>().join(", "),
            i,
            if i >= lexgen.first_end_state { format!(" {}", lexgen.terminal_table[i - lexgen.first_end_state] ) } else { "".to_string() }
        );
    }
    println!("];")
}

#[cfg(test)]
pub mod segments {
    use super::*;

    #[test]
    fn btree_seg() {
         let map = SegMap::from_iter([(Seg(1, 10), 1), (Seg(15, 20), 2), (Seg(21, 25), 3), (Seg(30, 40), 4), (Seg(99, 99), 5)]);
        let tests = vec![
            (0, 0), (5, 1), (10, 1), (12, 0), (15, 2), (99, 5), (100, 0), (50, 0), (35, 4), (100, 0)
        ];
        for (a, expected) in tests {
            let result = map.get(a).unwrap_or(0);
            assert_eq!(result, expected, "test on {a} failed");
        }

        let map = SegMap::from_iter([(Seg(0, 0), 1), (Seg(15, 20), 2), (Seg(21, 25), 3), (Seg(30, 40), 4), (Seg(99, 99), 5)]);
        let tests = vec![
            (0, 1), (1, 0), (12, 0), (15, 2), (99, 5), (100, 0), (50, 0), (35, 4), (100, 0)
        ];
        for (a, expected) in tests {
            let result = map.get(a).unwrap_or(0);
            assert_eq!(result, expected, "test on {a} failed");
        }
   }
}

#[cfg(possible_alternative)]
#[cfg(test)]
pub mod segments_alt {
    use std::collections::BTreeMap;
    use crate::lexgen::GroupId;

    #[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
    pub struct Seg(u32, u32);

    pub struct SegMap(BTreeMap<u32, (u32, GroupId)>);

    impl SegMap {
        pub fn new() -> Self {
            SegMap(BTreeMap::new())
        }

        pub fn from_iter<T: IntoIterator<Item = (Seg, GroupId)>>(iter: T) -> Self {
            SegMap(BTreeMap::from_iter(iter.into_iter().map(|(Seg(a, b), g)| (a, (b, g)))))
        }

        pub fn get(&self, value: u32) -> Option<GroupId> {
            let (_a, (b, group)) = self.0.range(0..=value).next_back()?;
            if *b >= value {
                Some(*group)
            } else {
                None
            }
        }
    }

    #[test]
    fn btree_seg() {
        let map = SegMap::from_iter([(Seg(1, 10), 1), (Seg(15, 20), 2), (Seg(21, 25), 3), (Seg(30, 40), 4), (Seg(99, 99), 5)]);
        let tests = vec![
            (0, 0), (5, 1), (10, 1), (12, 0), (15, 2), (99, 5), (100, 0), (50, 0), (35, 4), (100, 0)
        ];
        for (a, expected) in tests {
            let result = map.get(a).unwrap_or(0);
            assert_eq!(result, expected, "test on {a} failed");
        }

        let map = SegMap::from_iter([(Seg(0, 0), 1), (Seg(15, 20), 2), (Seg(21, 25), 3), (Seg(30, 40), 4), (Seg(99, 99), 5)]);
        let tests = vec![
            (0, 1), (1, 0), (12, 0), (15, 2), (99, 5), (100, 0), (50, 0), (35, 4), (100, 0)
        ];
        for (a, expected) in tests {
            let result = map.get(a).unwrap_or(0);
            assert_eq!(result, expected, "test on {a} failed");
        }
    }
}
