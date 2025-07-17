// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use crate::*;
use crate::dfa::tests::build_re;
use crate::segments::{Seg, SegMap};
use super::*;

fn chars_to_string(chars: &BTreeSet<char>, bracket: bool) -> String {
    let mut result = String::new();
    if bracket { result.push('['); }
    result.push_str(&chars.into_iter().map(|c| format!("{}", escape_char(*c))).collect::<String>());
    if bracket { result.push(']'); }
    result
}

#[test]
fn lexgen_partition_symbols() {
    let tests = [
        (1, btreemap![
            0 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1, 'x' => 1],
            1 => branch!['0' => 2],
            2 => branch!['0' => 1, '1' => 2, '2' => 2, '3' => 2, 'x' => 2],
            3 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 3, 'x' => 2]
        ], vec![segments!['0'], segments!['1'-'2', 'x'], segments!['3']]),

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
        ], vec![segments!['0'], segments!['1'-'9'], segments!['A'-'F', 'a'-'f'], segments!['.'], segments!['x']]),
        (3, btreemap![
            0 => branch!['0' => 0, '1' => 1, '2' => 1, '3' => 1],
            1 => branch!['0' => 1, '1' => 1, '2' => 1, '3' => 1],
            2 => branch!['0' => 2, '1' => 2, '2' => 2, '3' => 0]
        ], vec![segments!['0'], segments!['1'-'2'], segments!['3']]),

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
        let mut dfa_builder = DfaBuilder::new();
        let dfa = dfa_builder.build_from_graph(g, 0, btreemap![3 => term![=0], 4 => term![=0], 5 => term![=1], 6 => term![=2]])
            .expect(&format!("test {test_id} failed to build Dfa\n{}", dfa_builder.get_messages()));
        let dfa = dfa.normalize();
        let lexgen = LexerGen::from(dfa);
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
        // print_source_code(&lexergen);
    }
}

#[test]
fn lexgen_symbol_tables_corner() {
    let tests: Vec<(u32,
                    BTreeMap<StateId, BTreeMap<Segments, StateId>>,    // graph
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
            0 => BTreeMap::from_iter((0_u32..16).map(|x| (Segments::new(Seg(x*16, x*16+15)), x as StateId))),
            1 => branch!(), 2 => branch!(), 3 => branch!(), 4 => branch!(), 5 => branch!(), 6 => branch!(), 7 => branch!(), 8 => branch!(),
            9 => branch!(), 10 => branch!(), 11 => branch!(), 12 => branch!(), 13 => branch!(), 14 => branch!(), 15 => branch!()
        ],
         BTreeMap::from_iter((0_u32..8).map(|x| ((0..16).map(|y| char::from_u32(x * 16 + y).unwrap()).collect::<String>(), x as GroupId))),
         BTreeMap::from_iter((8_u32..10).map(|x| ((0..16).map(|y| char::from_u32(x * 16 + y).unwrap()).collect::<String>(), x as GroupId))),
         BTreeMap::from_iter((10_u32..16).map(|x| (Seg(x * 16, x * 16 + 15), x as GroupId)))
        ),
    ];
    const VERBOSE: bool = false;
    for (test_id, (left, g, ascii, utf8, seg)) in tests.into_iter().enumerate() {
        if VERBOSE { println!("Test {test_id}:"); }
        let end_states = g.values().flat_map(|x| x.values()).cloned().collect::<BTreeSet<StateId>>();
        let mut dfa_builder = DfaBuilder::new();
        let dfa = dfa_builder.build_from_graph(g, 0, end_states.iter().map(|s| (*s, term!(=0))).collect::<BTreeMap<StateId, Terminal>>())
            .expect(&format!("test {test_id} failed to build Dfa\n{}", dfa_builder.get_messages()));
        let dfa = dfa.normalize();
        let mut lexgen = LexerGen::new();
        lexgen.max_utf8_chars = left;
        lexgen.build_from_dfa(dfa);
        let error_id = lexgen.nbr_groups as GroupId;
        let mut exp_ascii = vec![error_id; 128];
        for (s, id) in ascii {
            for b in s.chars() {
                exp_ascii[b as usize] = id;
            }
        }
        let mut exp_utf8 = HashMap::<char, GroupId>::new();
        for (s, id) in utf8 {
            for u in s.chars() {
                exp_utf8.insert(u, id);
            }
        }
        if VERBOSE {
            println!("LexGen: {}", std::mem::size_of::<LexerGen>());
            println!("lexergen: {}", std::mem::size_of_val(&lexgen));
            println!("ascii_to_group: {}", std::mem::size_of_val(&lexgen.ascii_to_group));
            println!("utf8_to_group:  {}", std::mem::size_of_val(&lexgen.utf8_to_group));
            println!("seg_to_group:   {}", std::mem::size_of_val(&lexgen.seg_to_group));
            lexgen.write_source_code(None, 0).expect("Couldn't output the source code");
        }
        let exp_seg = SegMap::from_iter(seg.into_iter());
        assert_eq!(lexgen.ascii_to_group, exp_ascii, "test {test_id} failed");
        assert_eq!(lexgen.utf8_to_group, exp_utf8, "test {test_id} failed");
        assert_eq!(lexgen.seg_to_group, exp_seg, "test {test_id} failed");
    }
}

#[test]
fn lexgen_build() {
    const VERBOSE: bool = false;
    let mut test_id = 0;
    loop {
        let re = build_re(test_id);
        if re.len() == 0 {
            break;
        }
        let lexgen;
        time! { VERBOSE, {
                let re = build_re(test_id);
                let mut dfa_builder = DfaBuilder::from_re(re);
                let dfa = dfa_builder.build();
                let dfa = dfa.normalize();
                lexgen = LexerGen::from(dfa);
                if VERBOSE { print!("- {test_id:2}: "); }
            }
        }
        assert!(lexgen.state_table.len() > 0, "test {test_id} has an empty state table");
        test_id += 1;
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
