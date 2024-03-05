// Copyright 2023 Redglyph
//
// Unit tests

#![cfg(test)]

mod vectree;

use crate::*;

fn node_to_string(tree: &VecTree<ReNode>, index: usize, basic: bool) -> String {
    let node = tree.get(index);
    let mut result = String::new();
    if !basic {
        if node.nullable.is_none() {
            result.push('?');
        } else if node.nullable.unwrap() {
            result.push('!');
        }
    }
    result.push_str(&node.to_string());
    let children = tree.children(index);
    if !children.is_empty() {
        result.push_str("(");
        result.push_str(&children.iter().map(|&c| node_to_string(&tree, c, basic)).collect::<Vec<_>>().join(","));
        result.push_str(")");
    }
    result
}

fn tree_to_string(tree: &VecTree<ReNode>, basic: bool) -> String {
    if tree.len() > 0 {
        node_to_string(tree, 0, basic)
    } else {
        "None".to_string()
    }
}

fn build_re(test: usize) -> VecTree<ReNode> {
    let mut re = VecTree::new();
    match test {
        0 => {
            // (a|b)*abb
            let f = re.set_root(ReNode::new(ReType::Concat)).expect("expect empty tree");
            let e = re.add_iter(Some(f), [ReNode::new(ReType::Concat), ReNode::new(ReType::End)])[0];
            let d = re.add_iter(Some(e), [ReNode::new(ReType::Concat), ReNode::new(ReType::Char('b'))])[0];
            let c = re.add_iter(Some(d), [ReNode::new(ReType::Concat), ReNode::new(ReType::Char('b'))])[0];
            let b = re.add_iter(Some(c), [ReNode::new(ReType::Star), ReNode::new(ReType::Char('a'))])[0];
            let a = re.add(Some(b), ReNode::new(ReType::Or));
            re.add_iter(Some(a), [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
        },
        1 => {
            // (a|b)*abb
            let c = re.set_root(ReNode::new(ReType::Concat)).expect("expect empty tree");
            let b = re.add_iter(Some(c), [
                ReNode::new(ReType::Star),
                ReNode::new(ReType::Char('a')),
                ReNode::new(ReType::Char('b')),
                ReNode::new(ReType::Char('b')),
                ReNode::new(ReType::End)
            ])[0];
            let a = re.add(Some(b), ReNode::new(ReType::Or));
            re.add_iter(Some(a), [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
        },
        2 => {
            let c = re.set_root(ReNode::new(ReType::Concat)).expect("expect empty tree");
            let b = re.add_iter(Some(c), [
                ReNode::new(ReType::Star),
                ReNode::new(ReType::String("abb".to_string())),
                ReNode::new(ReType::End)
            ])[0];
            let a = re.add(Some(b), ReNode::new(ReType::Or));
            re.add_iter(Some(a), [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
        }
        3 => {
            let root = re.add(None, ReNode::new(ReType::Concat));
            re.add(Some(root), ReNode::new(ReType::String("abc".to_string())));
            let a = re.add(Some(root), ReNode::new(ReType::Or));
            re.add_iter(Some(a), [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
            re.add(Some(root), ReNode::new(ReType::End));
        }
        4 => {
            let root = re.add(None, ReNode::new(ReType::Concat));
            re.add(Some(root), ReNode::new(ReType::Char('s')));
            let a = re.add(Some(root), ReNode::new(ReType::Or));
            re.add_iter(Some(a), [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
            re.add(Some(root), ReNode::new(ReType::End));
        }
        5 => {
            let root = re.add(None, ReNode::new(ReType::Concat));
            re.add(Some(root), ReNode::new(ReType::Char('s')));
            let a = re.add(Some(root), ReNode::new(ReType::Or));
            let cd = re.add_iter(Some(a), [ReNode::new(ReType::Concat), ReNode::new(ReType::Concat)]);
            re.add_iter(Some(cd[0]), [ReNode::new(ReType::Char('a')), ReNode::new(ReType::End)]);
            re.add_iter(Some(cd[1]), [ReNode::new(ReType::Char('b')), ReNode::new(ReType::End)]);
            //re.add(Some(root), ReNode::new(ReType::End));
        }
        _ => panic!("test {test} doesn't exist")
    }
    re
}

fn debug_tree(tree: &VecTree<ReNode>) -> String {
    let mut result = String::new();
    let size = tree.len();
    for i in 0..size {
        let node = tree.get(i);
        result.push_str(&format!("[{i:3}] "));
        if let Some(id) = node.id {
            result.push_str(&format!("{id}:"));
        }
        if node.nullable.unwrap_or(false) {
            result.push('!');
        }
        result.push_str(&node.op.to_string());
        let children = tree.children(i).iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
        if children.len() > 0 {
            result.push_str(" -> [");
            result.push_str(&children);
            result.push(']');
        }
        let mut firstpos = node.firstpos.iter().collect::<Vec<_>>();
        firstpos.sort();
        let firstpos = firstpos.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
        if firstpos.len() > 0 {
            result.push_str(" fp:");
            result.push_str(&firstpos);
        }
        let mut lastpos = node.lastpos.iter().collect::<Vec<_>>();
        lastpos.sort();
        let lastpos = lastpos.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
        if lastpos.len() > 0 {
            result.push_str(" lp:");
            result.push_str(&lastpos);
        }
        result.push('\n');
    }
    result
}

fn print_graph(dfa: &DfaBuilder) {
    println!("  graph:      {:?}", dfa.state_graph);
    println!("  end states: {:?}", dfa.end_states);
    for (state, trans) in dfa.state_graph.clone() {
        println!("s{state}{}", if dfa.end_states.contains(&state) { " <END>" } else { "" });
        for (symbol, dest) in trans {
            println!("  {symbol} -> s{dest}");
        }
    }
}

mod test_node {
    use super::*;

    #[test]
    fn dfa_preprocess() {
        let tests = vec![
            ("?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end>)", "?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end>)"),
            ("?&(?*(?|(?'a',?'b')),?'a',?'b',?'b',?<end>)", "?&(?*(?|(?'a',?'b')),?'a',?'b',?'b',?<end>)"),
            ("?&(?*(?|(?'a',?'b')),?'abb',?<end>)", "?&(?*(?|(?'a',?'b')),?&(?'a',?'b',?'b'),?<end>)")
        ];
        for (test_id, (expected1, expected2)) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let result1 = tree_to_string(&re, false);
            let dfa = DfaBuilder::from_re(re);
            let result2 = tree_to_string(&dfa.re, false);
            assert_eq!(result1, expected1, "test {test_id} failed (1st part)");
            assert_eq!(result2, expected2, "test {test_id} failed (2nd part)");
        }
    }

    #[test]
    fn dfa_id() {
        let tests = vec![
            "&(&(&(&(*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end>)",
            "&(*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end>)",
            "&(*(|(1:'a',2:'b')),&(3:'a',4:'b',5:'b'),6:<end>)",
            "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
            "&(1:'s',|(2:'a',3:'b'),4:<end>)",
            "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
        ];
        for (test_id, expected) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            assert_eq!(tree_to_string(&dfa.re, true), expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_nullable() {
        let tests = vec![
            "&(&(&(&(!*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end>)",
            "&(!*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end>)",
            "&(!*(|(1:'a',2:'b')),&(3:'a',4:'b',5:'b'),6:<end>)",
            "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
            "&(1:'s',|(2:'a',3:'b'),4:<end>)",
            "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
        ];
        for (test_id, expected) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            assert_eq!(tree_to_string(&dfa.re, false), expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_firstpos() {
        let tests = vec![
            vec![
                vec![1], vec![2],   // a, b
                vec![1, 2],         // |
                vec![1, 2],         // *
                vec![3],            // a
                vec![1, 2, 3],      // &
                vec![4],            // b
                vec![1, 2, 3],      // &
                vec![5],            // b
                vec![1, 2, 3],      // &
                vec![6],            // <end>
                vec![1, 2, 3],      // &
            ],
            vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5], vec![6], // a, b, b, <end>
                vec![1, 2, 3],                      // &
            ],
            vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5],          // a, b, b
                vec![3],                            // &
                vec![6],                            // <end>
                vec![1, 2, 3],                      // &
            ],
            vec![
                vec![1], vec![2], vec![3],          // a, b, c
                vec![1],                            // &
                vec![4], vec![5],                   // a, b
                vec![4,5],                          // |
                vec![6],                            // <end>
                vec![1]                             // &
            ],
            vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, b
                vec![2, 3],                         // |
                vec![4],                            // <end>
                vec![1]                             // &
            ],
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
            vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, <end>
                vec![2],                            // &
                vec![4], vec![5],                   // b, <end>
                vec![4],                            // &
                vec![2,4],                          // |
                vec![1]                             // &
            ]
        ];
        for (test_id, expected) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            let mut result = Vec::new();
            for inode in dfa.re.iter_depth() {
                let mut firstpos = inode.firstpos.iter().map(|n| *n).collect::<Vec<_>>();
                firstpos.sort();
                result.push(firstpos)
            }
            assert_eq!(result, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_lastpos() {
        let tests = vec![
            vec![
                vec![1], vec![2],   // a, b
                vec![1, 2],         // |
                vec![1, 2],         // *
                vec![3],            // a
                vec![3],            // &
                vec![4],            // b
                vec![4],            // &
                vec![5],            // b
                vec![5],            // &
                vec![6],            // <end>
                vec![6],            // &
            ],
            vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5], vec![6], // a, b, b, <end>
                vec![6],                            // &
            ],
            vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5],          // a, b, b
                vec![5],                            // &
                vec![6],                            // <end>
                vec![6],                            // &
            ],
            vec![
                vec![1], vec![2], vec![3],          // a, b, c
                vec![3],                            // &
                vec![4], vec![5],                   // a, b
                vec![4,5],                          // |
                vec![6],                            // <end>
                vec![6]                             // &
            ],
            vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, b
                vec![2, 3],                         // |
                vec![4],                            // <end>
                vec![4]                             // &
            ],
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
            vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, <end>
                vec![3],                            // &
                vec![4], vec![5],                   // b, <end>
                vec![5],                            // &
                vec![3,5],                          // |
                vec![3,5]                             // &
            ]
        ];
        for (test_id, expected) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            let mut result = Vec::new();
            for inode in dfa.re.iter_depth() {
                let mut lastpos = inode.lastpos.iter().map(|n| *n).collect::<Vec<_>>();
                lastpos.sort();
                result.push(lastpos)
            }
            assert_eq!(result, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_followpos() {
        let tests = vec!{
            (0, hashmap![
                1 => hashset![1, 2, 3],
                2 => hashset![1, 2, 3],
                3 => hashset![4],
                4 => hashset![5],
                5 => hashset![6],
                6 => hashset![]
            ]),
            (1, hashmap![
                1 => hashset![1, 2, 3],
                2 => hashset![1, 2, 3],
                3 => hashset![4],
                4 => hashset![5],
                5 => hashset![6],
                6 => hashset![]
            ]),
            (2, hashmap![
                1 => hashset![1, 2, 3],
                2 => hashset![1, 2, 3],
                3 => hashset![4],
                4 => hashset![5],
                5 => hashset![6],
                6 => hashset![]
            ]),
            // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'))"
            (3, hashmap![
                1 => hashset![2],
                2 => hashset![3],
                3 => hashset![4, 5],
                4 => hashset![6],
                5 => hashset![6],
                6 => hashset![],
            ]),
            (4, hashmap![
                1 => hashset![2, 3],
                2 => hashset![4],
                3 => hashset![4],
                4 => hashset![],
            ]),
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
            (5, hashmap![
                1 => hashset![2, 4],
                2 => hashset![3],
                3 => hashset![],
                4 => hashset![5],
                5 => hashset![],
            ])
        };
        for (test_id, expected) in tests.into_iter() {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            assert_eq!(dfa.followpos, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_states() {
        let tests: Vec<(usize, BTreeMap<StateId, BTreeMap<ReType, StateId>>)> = vec![
            (0, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
                (1, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 2)])),
                (2, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 3)])),
                (3, BTreeMap::from([/*(ReType::End, 4),*/ (ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
            ])),
            (1, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
                (1, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 2)])),
                (2, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 3)])),
                (3, BTreeMap::from([/*(ReType::End, 4),*/ (ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
            ])),
            (2, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
                (1, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 2)])),
                (2, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 3)])),
                (3, BTreeMap::from([/*(ReType::End, 4),*/ (ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
            ])),
            // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
            (3, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('a'), 1)])),
                (1, BTreeMap::from([(ReType::Char('b'), 2)])),
                (2, BTreeMap::from([(ReType::Char('c'), 3)])),
                (3, BTreeMap::from([(ReType::Char('a'), 4), (ReType::Char('b'), 4)])),
                (4, BTreeMap::from([/*(ReType::End, 5)*/]))
            ])),
            // "&(1:'s',|(2:'a',3:'b'),4:<end>)",
            (4, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('s'), 1)])),
                (1, BTreeMap::from([(ReType::Char('a'), 2), (ReType::Char('b'), 2)])),
                (2, BTreeMap::from([/*(ReType::End, 3)*/])),
            ])),
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
            (5, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('s'), 1)])),
                (1, BTreeMap::from([(ReType::Char('a'), 2), (ReType::Char('b'), 3)])),
                (2, BTreeMap::from([/*(ReType::End, 4)*/])),
                (3, BTreeMap::from([/*(ReType::End, 4)*/])),
            ])),
        ];
        for (test_id, expected) in tests {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            dfa.calc_states();
            assert_eq!(dfa.state_graph, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn optimize_graphs() {
        let tests: Vec<(usize, BTreeMap<StateId, BTreeMap<ReType, StateId>>, Vec::<StateId>)> = vec![
            (0, BTreeMap::from([
                (0, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
                (1, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 2)])),
                (2, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 3)])),
                (3, BTreeMap::from([(ReType::Char('a'), 1), (ReType::Char('b'), 0)])),
            ]), vec![3]),

            ];
        for (test_id, graph, end_states) in tests {
            println!("{test_id}:");
            let mut dfa = DfaBuilder::from_graph(graph, 0, end_states);
            let tr = dfa.optimize_graph(true);
            println!("table: {}\n", tr.iter().map(|(a, b)| format!("{a} -> {b}")).collect::<Vec<_>>().join(", "));
        }
    }

    // just prints the debug info
    #[ignore]
    #[test]
    fn print_debug_calc() {
        let tests = [1, 4, 5];
        for test_id in tests {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.calc_node_pos();
            println!("{test_id}: {}\n{:}", tree_to_string(&dfa.re, true), debug_tree(&dfa.re));
            let mut keys = dfa.followpos.keys().map(|&k| k).collect::<Vec<_>>();
            keys.sort();
            for k in keys {
                let mut followpos = dfa.followpos.get(&k).unwrap().iter().map(|&id| id).collect::<Vec<_>>();
                followpos.sort();
                println!("followpos[{k}]: {}", followpos.iter().map(|id| id.to_string()).collect::<Vec<_>>().join(","));
            }
            println!();
        }
    }

    // just prints the state graph
    #[ignore]
    #[test]
    fn print_state_graph() {
        let tests = [1, 4, 5];
        for test_id in tests {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::from_re(re);
            dfa.build_dfa();
            println!("test {test_id}:");
            print_graph(&dfa);
            dfa.optimize_graph(true);
            println!();
        }
    }
}
