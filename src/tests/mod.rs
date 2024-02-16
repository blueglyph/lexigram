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
            let f = re.set_root(ReNode::new(ReType::Concat)).expect("expect empty tree");
            let e = re.add_iter(f, [ReNode::new(ReType::Concat), ReNode::new(ReType::End)])[0];
            let d = re.add_iter(e, [ReNode::new(ReType::Concat), ReNode::new(ReType::Char('b'))])[0];
            let c = re.add_iter(d, [ReNode::new(ReType::Concat), ReNode::new(ReType::Char('b'))])[0];
            let b = re.add_iter(c, [ReNode::new(ReType::Star), ReNode::new(ReType::Char('a'))])[0];
            let a = re.add(b, ReNode::new(ReType::Or));
            re.add_iter(a, [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
        },
        1 => {
            let c = re.set_root(ReNode::new(ReType::Concat)).expect("expect empty tree");
            let b = re.add_iter(c, [
                ReNode::new(ReType::Star),
                ReNode::new(ReType::Char('a')),
                ReNode::new(ReType::Char('b')),
                ReNode::new(ReType::Char('b')),
                ReNode::new(ReType::End)
            ])[0];
            let a = re.add(b, ReNode::new(ReType::Or));
            re.add_iter(a, [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
        },
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

mod test_node {
    use super::*;

    #[test]
    fn dfa_builder() {
        let re = build_re(0);
        assert_eq!(tree_to_string(&re, false), "?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end>)");
    }

    #[test]
    fn dfa_id() {
        let re = build_re(0);
        let mut dfa = DfaBuilder::new(re);
        dfa.calc_node();
        assert_eq!(tree_to_string(&dfa.re, true), "&(&(&(&(*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end>)");
    }

    #[test]
    fn dfa_nullable() {
        let expected = vec![
            "&(&(&(&(!*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end>)",
            "&(!*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end>)"
        ];
        for test_id in 0..=1 {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::new(re);
            dfa.calc_node();
            assert_eq!(tree_to_string(&dfa.re, false), expected[test_id]);
        }
    }

    #[test]
    fn dfa_firstpos() {
        let expected = vec![
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
            ]
        ];
        for test_id in 0..=1 {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::new(re);
            dfa.calc_node();
            let mut result = Vec::new();
            for inode in dfa.re.iter_depth() {
                let mut firstpos = inode.data.firstpos.iter().map(|n| *n).collect::<Vec<_>>();
                firstpos.sort();
                result.push(firstpos)
            }
            assert_eq!(result, expected[test_id]);
        }
    }

    #[test]
    fn dfa_lastpos() {
        let expected = vec![
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
            ]
        ];
        for test_id in 0..=1 {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::new(re);
            dfa.calc_node();
            let mut result = Vec::new();
            for inode in dfa.re.iter_depth() {
                let mut lastpos = inode.data.lastpos.iter().map(|n| *n).collect::<Vec<_>>();
                lastpos.sort();
                result.push(lastpos)
            }
            assert_eq!(result, expected[test_id]);
        }
    }

    #[test]
    fn dfa_followpos() {
        let expected = vec!{
            hashmap![
                1 => hashset![1, 2, 3],
                2 => hashset![1, 2, 3],
                3 => hashset![4],
                4 => hashset![5],
                5 => hashset![6]
            ],
            hashmap![
                1 => hashset![1, 2, 3],
                2 => hashset![1, 2, 3],
                3 => hashset![4],
                4 => hashset![5],
                5 => hashset![6]
            ]
        };
        for test_id in 0..=1 {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::new(re);
            dfa.calc_node();
            assert_eq!(dfa.followpos, expected[test_id]);
        }
    }

    // just prints the debug info
    #[ignore]
    #[test]
    fn print_debug_calc() {
        for test_id in 0..=1 {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::new(re);
            dfa.calc_node();
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

    // #[test]
    // fn test_basic() {
    //     let n1 = ReNode::str("abcd");
    //     let n2 = ReNode::concat([n1.clone(), ReNode::char('e')]);
    //     assert_eq!(n1, ReNode { id: RefCell::new(0), op: ReType::Concat(vec![
    //         ReNode { id: RefCell::new(0), op: ReType::Char('a') },
    //         ReNode { id: RefCell::new(0), op: ReType::Char('b') },
    //         ReNode { id: RefCell::new(0), op: ReType::Char('c') },
    //         ReNode { id: RefCell::new(0), op: ReType::Char('d') }]),
    //     });
    //     assert_eq!(n2, ReNode { id: RefCell::new(0), op: ReType::Concat(vec![
    //         ReNode { id: RefCell::new(0), op: ReType::Concat(vec![
    //             ReNode { id: RefCell::new(0), op: ReType::Char('a') },
    //             ReNode { id: RefCell::new(0), op: ReType::Char('b') },
    //             ReNode { id: RefCell::new(0), op: ReType::Char('c') },
    //             ReNode { id: RefCell::new(0), op: ReType::Char('d') }])
    //         },
    //         ReNode { id: RefCell::new(0), op: ReType::Char('e') },
    //     ])});
    // }
    //
    // #[test]
    // fn test_ids() {
    //     let n = ReNode::concat([
    //         ReNode::or([
    //             ReNode::char('e'),
    //             ReNode::char('f')
    //         ]),
    //         ReNode::star(
    //             ReNode::char('g')
    //         ),
    //         ReNode::char('d')
    //     ]);
    //     let mut builder = DfaBuilder::new(n);
    //     builder.build_dfa();
    //     assert_eq!(builder.re,
    //                ReNode { id: RefCell::new(6), op: ReType::Concat(vec![
    //                    ReNode { id: RefCell::new(5), op: ReType::Or(vec![
    //                        ReNode { id: RefCell::new(4), op: ReType::Char('e') },
    //                        ReNode { id: RefCell::new(3), op: ReType::Char('f') }]) },
    //                    ReNode { id: RefCell::new(2), op: ReType::Star(Box::new(
    //                        ReNode { id: RefCell::new(1), op: ReType::Char('g') })) },
    //                    ReNode { id: RefCell::new(0), op: ReType::Char('d') }]) });
    // }
    //
    // #[test]
    // fn test_nullable1() {
    //     let n = ReNode::concat([
    //         ReNode::or([
    //             ReNode::char('e'),
    //             ReNode::char('f')
    //         ]),
    //         ReNode::star(
    //             ReNode::char('g')
    //         ),
    //         ReNode::char('d')
    //     ]);
    //     let mut builder = DfaBuilder::new(n);
    //     builder.build_dfa();
    //     assert_eq!(builder.nullable, vec![false, false, true, false, false, false, false])
    // }
    //
    // #[test]
    // fn test_nullable2() {
    //     let n = ReNode::concat([
    //         ReNode::or([
    //             ReNode::empty(),
    //             ReNode::char('f')
    //         ]),
    //         ReNode::star(
    //             ReNode::char('g')
    //         ),
    //         ReNode::char('d')
    //     ]);
    //     let mut builder = DfaBuilder::new(n);
    //     builder.build_dfa();
    //     assert_eq!(builder.nullable, vec![false, false, true, false, true, true, false])
    // }

}

