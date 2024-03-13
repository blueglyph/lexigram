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

macro_rules! node {
    (chr $char:expr) => { ReNode::new(ReType::Char($char)) };
    (str $str:expr) => { ReNode::new(ReType::String($str.to_string())) };
    (= $id:expr ) => { ReNode::new(ReType::End(Token($id))) };
    (&) => { ReNode::new(ReType::Concat) };
    (|) => { ReNode::new(ReType::Or) };
    (*) => { ReNode::new(ReType::Star) };
    (-) => { ReNode::new(ReType::Empty) };
}

fn build_re(test: usize) -> VecTree<ReNode> {
    let mut re = VecTree::new();
    match test {
        0 => { // (a|b)*abb<end>
            let f = re.set_root(node!(&)).expect("expect empty tree");
            let e = re.add_iter(Some(f), [node!(&), node!(=0)])[0];
            let d = re.add_iter(Some(e), [node!(&), node!(chr 'b')])[0];
            let c = re.add_iter(Some(d), [node!(&), node!(chr 'b')])[0];
            let b = re.add_iter(Some(c), [node!(*), node!(chr 'a')])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        },
        1 => { // (a|b)*abb<end>
            let c = re.set_root(node!(&)).expect("expect empty tree");
            let b = re.add_iter(Some(c), [
                node!(*),
                node!(chr 'a'),
                node!(chr 'b'),
                node!(chr 'b'),
                node!(=0)
            ])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        },
        2 => { // (a|b)*abb<end>
            let c = re.set_root(node!(&)).expect("expect empty tree");
            let b = re.add_iter(Some(c), [
                node!(*),
                node!(str "abb"),
                node!(=0)
            ])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        }
        3 => { // abc(a|b)<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(str "abc"));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(=0));
        }
        4 => { // s(a|b)<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 's'));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(=0));
        }
        5 => {  // s(a<end>|b<end>)
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 's'));
            let a = re.add(Some(root), node!(|));
            let cd = re.add_iter(Some(a), [node!(&), node!(&)]);
            re.add_iter(Some(cd[0]), [node!(chr 'a'), node!(=0)]);
            re.add_iter(Some(cd[1]), [node!(chr 'b'), node!(=1)]);
        }
        6 => {  // a(bc)?d = a(bc|-)d<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let bc_opt = re.add(Some(root), node!(|));
            re.add_iter(Some(root), [node!(chr 'd'), node!(=0)]);
            re.add(Some(bc_opt), node!(str "bc"));
            re.add(Some(bc_opt), node!(-));
        },
        7 => {  // a(bc)?d?e = a(bc|-)(d|-)e<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let bc_opt = re.add(Some(root), node!(|));
            let d_opt = re.add(Some(root), node!(|));
            re.add_iter(Some(root), [node!(chr 'e'), node!(=0)]);
            re.add(Some(bc_opt), node!(str "bc"));
            re.add(Some(bc_opt), node!(-));
            re.add(Some(d_opt), node!(chr 'd'));
            re.add(Some(d_opt), node!(-));
        },
        8 => {  // a(<end>|b<end>)
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let b1 = re.add(Some(root), node!(|));
            re.add(Some(b1), node!(=0));
            let b2 = re.add(Some(b1), node!(&));
            re.add_iter(Some(b2), [node!(chr 'b'), node!(=1)]);
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

fn print_graph(dfa: &Dfa) {
    println!("  graph:      {:?}", dfa.state_graph);
    println!("  end states: {:?}", dfa.end_states);
    for (state, trans) in dfa.state_graph.clone() {
        println!("s{state}{}", if dfa.end_states.contains(&state) { " <END>" } else { "" });
        for (symbol, dest) in trans {
            println!("  {symbol} -> s{dest}");
        }
    }
}

macro_rules! branch {
    ($($key:expr => $value:expr,)+) => { branch!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => { btreemap![$(ReType::Char($key) => $value,)*] };
}

mod test_node {
    use super::*;

    #[test]
    fn dfa_preprocess() {
        let tests = vec![
            ("?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end:0>)", "?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end:0>)"),
            ("?&(?*(?|(?'a',?'b')),?'a',?'b',?'b',?<end:0>)", "?&(?*(?|(?'a',?'b')),?'a',?'b',?'b',?<end:0>)"),
            ("?&(?*(?|(?'a',?'b')),?'abb',?<end:0>)", "?&(?*(?|(?'a',?'b')),?&(?'a',?'b',?'b'),?<end:0>)")
        ];
        for (test_id, (expected1, expected2)) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let result1 = tree_to_string(&re, false);
            let dfa = DfaBuilder::new(re);
            let result2 = tree_to_string(&dfa.re, false);
            assert_eq!(result1, expected1, "test {test_id} failed (1st part)");
            assert_eq!(result2, expected2, "test {test_id} failed (2nd part)");
        }
    }

    #[test]
    fn dfa_id() {
        let tests = vec![
            "&(&(&(&(*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end:0>)",
            "&(*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end:0>)",
            "&(*(|(1:'a',2:'b')),&(3:'a',4:'b',5:'b'),6:<end:0>)",
            "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end:0>)",
            "&(1:'s',|(2:'a',3:'b'),4:<end:0>)",
            "&(1:'s',|(&(2:'a',3:<end:0>),&(4:'b',5:<end:1>)))",
        ];
        for (test_id, expected) in tests.into_iter().enumerate() {
            let re = build_re(test_id);
            let mut dfa = DfaBuilder::new(re);
            dfa.calc_node_pos();
            assert_eq!(tree_to_string(&dfa.re, true), expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_nullable() {
        let tests = vec![
            (0, "&(&(&(&(!*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end:0>)"),
            (1, "&(!*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end:0>)"),
            (2, "&(!*(|(1:'a',2:'b')),&(3:'a',4:'b',5:'b'),6:<end:0>)"),
            (3, "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end:0>)"),
            (4, "&(1:'s',|(2:'a',3:'b'),4:<end:0>)"),
            (5, "&(1:'s',|(&(2:'a',3:<end:0>),&(4:'b',5:<end:1>)))"),
            (6, "&(1:'a',!|(&(2:'b',3:'c'),!4:-),5:'d',6:<end:0>)"),
            (7, "&(1:'a',!|(&(2:'b',3:'c'),!4:-),!|(5:'d',!6:-),7:'e',8:<end:0>)"),
            (8, "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"),
        ];
        for (test_id, expected) in tests.into_iter() {
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            dfa_builder.calc_node_pos();
            assert_eq!(tree_to_string(&dfa_builder.re, false), expected, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_firstpos() {
        let tests = vec![
            (0, vec![
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
            ]),
            (1, vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5], vec![6], // a, b, b, <end>
                vec![1, 2, 3],                      // &
            ]),
            (2, vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5],          // a, b, b
                vec![3],                            // &
                vec![6],                            // <end>
                vec![1, 2, 3],                      // &
            ]),
            (3, vec![
                vec![1], vec![2], vec![3],          // a, b, c
                vec![1],                            // &
                vec![4], vec![5],                   // a, b
                vec![4,5],                          // |
                vec![6],                            // <end>
                vec![1]                             // &
            ]),
            (4, vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, b
                vec![2, 3],                         // |
                vec![4],                            // <end>
                vec![1]                             // &
            ]),
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
            (5, vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, <end>
                vec![2],                            // &
                vec![4], vec![5],                   // b, <end>
                vec![4],                            // &
                vec![2,4],                          // |
                vec![1]                             // &
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
            (6, vec![
                vec![1],        // a
                vec![2],        // b
                vec![3],        // c
                vec![2],        // &(b,c)
                vec![],         // -
                vec![2],        // |
                vec![5],        // d
                vec![6],        // <end>
                vec![1]         // &
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
            (7, vec![
                vec![1],        // a
                vec![2],        // b
                vec![3],        // c
                vec![2],        // &(b,c)
                vec![],         // -
                vec![2],        // |
                vec![5],        // d
                vec![],         // -
                vec![5],        // |
                vec![7],        // e
                vec![8],        // <end>
                vec![1]         // &
            ]),
            // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
            (8, vec![
                vec![1],        // a
                vec![2],        // <end:0>
                vec![3],        // b
                vec![4],        // <end:1>
                vec![3],        // &(b,<1>)
                vec![2,3],      // |(<0>,&)
                vec![1]         // &
            ])
        ];
        for (test_id, expected) in tests.into_iter() {
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            dfa_builder.calc_node_pos();
            let mut result = Vec::new();
            for inode in dfa_builder.re.iter_depth() {
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
            (0, vec![
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
            ]),
            (1, vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5], vec![6], // a, b, b, <end>
                vec![6],                            // &
            ]),
            (2, vec![
                vec![1], vec![2],                   // a, b
                vec![1, 2],                         // |
                vec![1, 2],                         // *
                vec![3], vec![4], vec![5],          // a, b, b
                vec![5],                            // &
                vec![6],                            // <end>
                vec![6],                            // &
            ]),
            (3, vec![
                vec![1], vec![2], vec![3],          // a, b, c
                vec![3],                            // &
                vec![4], vec![5],                   // a, b
                vec![4,5],                          // |
                vec![6],                            // <end>
                vec![6]                             // &
            ]),
            (4, vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, b
                vec![2, 3],                         // |
                vec![4],                            // <end>
                vec![4]                             // &
            ]),
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
            (5, vec![
                vec![1],                            // s
                vec![2], vec![3],                   // a, <end>
                vec![3],                            // &
                vec![4], vec![5],                   // b, <end>
                vec![5],                            // &
                vec![3,5],                          // |
                vec![3,5]                           // &
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
            (6, vec![
                vec![1],        // a
                vec![2],        // b
                vec![3],        // c
                vec![3],        // &(b,c)
                vec![],         // -
                vec![3],        // |
                vec![5],        // d
                vec![6],        // <end>
                vec![6]         // &
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
            (7, vec![
                vec![1],        // a
                vec![2],        // b
                vec![3],        // c
                vec![3],        // &(b,c)
                vec![],         // -
                vec![3],        // |
                vec![5],        // d
                vec![],         // -
                vec![5],        // |
                vec![7],        // e
                vec![8],        // <end>
                vec![8]         // &
            ]),
            // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
            (8, vec![
                vec![1],        // a
                vec![2],        // <end:0>
                vec![3],        // b
                vec![4],        // <end:1>
                vec![4],        // &(b,<1>)
                vec![2,4],      // |(<0>,&)
                vec![2,4]       // &
            ])
        ];
        for (test_id, expected) in tests.into_iter() {
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            dfa_builder.calc_node_pos();
            let mut result = Vec::new();
            for inode in dfa_builder.re.iter_depth() {
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
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
            (6, hashmap![
                1 => hashset![2, 5],
                2 => hashset![3],
                3 => hashset![5],
                5 => hashset![6],
                6 => hashset![]
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
            (7, hashmap![
                1 => hashset![2, 5, 7],
                2 => hashset![3],
                3 => hashset![5, 7],
                5 => hashset![7],
                7 => hashset![8],
                8 => hashset![]
            ]),
            // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
            (8, hashmap![
                1 => hashset![2, 3],
                2 => hashset![],
                3 => hashset![4],
                4 => hashset![],
            ])
        };
        for (test_id, expected) in tests.into_iter() {
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            dfa_builder.calc_node_pos();
            // to keep some things in order (easier for comparing):
            let res = BTreeMap::from_iter(dfa_builder.followpos);
            let exp = BTreeMap::from_iter(expected);
            assert_eq!(res, exp, "test {test_id} failed");
        }
    }

    #[test]
    fn dfa_states() {
        let tests = vec![
            (0, btreemap![
                0 => branch!['a' => 1, 'b' => 0],
                1 => branch!['a' => 1, 'b' => 2],
                2 => branch!['a' => 1, 'b' => 3],
                3 => branch!['a' => 1, 'b' => 0],
            ]),
            (1, btreemap![
                0 => branch!['a' => 1, 'b' => 0],
                1 => branch!['a' => 1, 'b' => 2],
                2 => branch!['a' => 1, 'b' => 3],
                3 => branch!['a' => 1, 'b' => 0],
            ]),
            (2, btreemap![
                0 => branch!['a' => 1, 'b' => 0],
                1 => branch!['a' => 1, 'b' => 2],
                2 => branch!['a' => 1, 'b' => 3],
                3 => branch!['a' => 1, 'b' => 0],
            ]),
            // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
            (3, btreemap![
                0 => branch!['a' => 1],
                1 => branch!['b' => 2],
                2 => branch!['c' => 3],
                3 => branch!['a' => 4, 'b' => 4],
                4 => branch![]
            ]),
            // "&(1:'s',|(2:'a',3:'b'),4:<end>)",
            (4, btreemap![
                0 => branch!['s' => 1],
                1 => branch!['a' => 2, 'b' => 2],
                2 => branch![],
            ]),
            // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
            (5, btreemap![
                0 => branch!['s' => 1],
                1 => branch!['a' => 2, 'b' => 3],
                2 => branch![],
                3 => branch![],
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
            (6, btreemap![
                0 => branch!['a' => 1],
                1 => branch!['b' => 2, 'd' => 3],
                2 => branch!['c' => 4],
                3 => branch![],
                4 => branch!['d' => 3]
            ]),
            // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
            (7, btreemap![
                0 => branch!['a' => 1],
                1 => branch!['b' => 2, 'd' => 3, 'e' => 4],
                2 => branch!['c' => 5],
                3 => branch!['e' => 4],
                4 => branch![],
                5 => branch!['d' => 3, 'e' => 4]
            ]),
            // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
            (8, btreemap![
                0 => branch!['a' => 1],
                1 => branch!['b' => 3],
                3 => branch![]
            ])
        ];
        for (test_id, expected) in tests {
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            let dfa = dfa_builder.build();
            assert_eq!(dfa.state_graph, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn optimize_graphs() {
        let tests = vec![
            (0, btreemap![
                0 => branch!['a' => 1, 'b' => 0],
                1 => branch!['a' => 1, 'b' => 2],
                2 => branch!['a' => 1, 'b' => 3],
                3 => branch!['a' => 1, 'b' => 0],
            ], vec![3],
             btreemap![ // 1 <-> 2
                 0 => branch!['a' => 2, 'b' => 0],
                 1 => branch!['a' => 2, 'b' => 3],
                 2 => branch!['a' => 2, 'b' => 1],
                 3 => branch!['a' => 2, 'b' => 0],
             ], vec![3]),

            (1, btreemap![
                0 => branch!['a' => 1, 'b' => 2],
                1 => branch!['a' => 1, 'b' => 3],
                2 => branch!['a' => 1, 'b' => 2],
                3 => branch!['a' => 1, 'b' => 4],
                4 => branch!['a' => 1, 'b' => 2],
            ], vec![4],
             btreemap![ // 0 -> 0, 1 -> 2, 2 -> 0, 3 -> 1, 4 -> 3
                0 => branch!['a' => 2, 'b' => 0],
                1 => branch!['a' => 2, 'b' => 3],
                2 => branch!['a' => 2, 'b' => 1],
                3 => branch!['a' => 2, 'b' => 0],
             ], vec![3]),

            (8, btreemap![
                0 => branch!['a' => 1],
                1 => branch!['b' => 3],
                3 => branch![]
            ], vec![1, 3],
            btreemap![
                0 => branch!['a' => 1],
                1 => branch!['b' => 2],
                2 => branch![],
            ], vec![1, 2])
        ];
        for (test_id, graph, end_states, exp_graph, exp_end_states) in tests {
            println!("{test_id}:");
            let mut dfa = Dfa::from_graph(graph, 0, end_states);
            let tr = dfa.optimize(true);
            println!("table: {}\n", tr.iter().map(|(a, b)| format!("{a} -> {b}")).collect::<Vec<_>>().join(", "));
            assert_eq!(dfa.state_graph, exp_graph, "test {test_id} failed");
            assert_eq!(dfa.end_states, BTreeSet::from_iter(exp_end_states.into_iter()), "test {test_id} failed");
        }
    }

    // just prints the debug info
    #[ignore]
    #[test]
    fn print_debug_calc() {
        let tests = [1, 4, 5];
        for test_id in tests {
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            dfa_builder.calc_node_pos();
            println!("{test_id}: {}\n{:}", tree_to_string(&dfa_builder.re, true), debug_tree(&dfa_builder.re));
            let mut keys = dfa_builder.followpos.keys().map(|&k| k).collect::<Vec<_>>();
            keys.sort();
            for k in keys {
                let mut followpos = dfa_builder.followpos.get(&k).unwrap().iter().map(|&id| id).collect::<Vec<_>>();
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
            let mut dfa_builder = DfaBuilder::new(re);
            let mut dfa = dfa_builder.build();
            println!("test {test_id}:");
            print_graph(&dfa);
            dfa.optimize(true);
            println!();
        }
    }
}
