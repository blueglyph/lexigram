// Copyright 2023 Redglyph
//
// Unit tests

#![cfg(test)]

mod vectree;
use crate::*;

fn node_to_string<T: Display>(tree: &VecTree<T>, index: usize) -> String {
    let mut result = tree.get(index).to_string();
    let children = tree.children(index);
    if !children.is_empty() {
        result.push_str("(");
        result.push_str(&children.iter().map(|&c| node_to_string(&tree, c)).collect::<Vec<_>>().join(","));
        result.push_str(")");
    }
    result
}

fn tree_to_string<T: Display>(tree: &VecTree<T>) -> String {
    if tree.len() > 0 {
        node_to_string(tree, 0)
    } else {
        "None".to_string()
    }
}

fn build_re() -> VecTree<ReNode> {
    let mut re = VecTree::new();
    let f = re.set_root(ReNode::new(ReType::Concat)).expect("expect empty tree");
    let e = re.add_iter(f, [ReNode::new(ReType::Concat), ReNode::new(ReType::End)])[0];
    let d = re.add_iter(e, [ReNode::new(ReType::Concat), ReNode::new(ReType::Char('b'))])[0];
    let c = re.add_iter(d, [ReNode::new(ReType::Concat), ReNode::new(ReType::Char('b'))])[0];
    let b = re.add_iter(c, [ReNode::new(ReType::Star), ReNode::new(ReType::Char('a'))])[0];
    let a = re.add(b, ReNode::new(ReType::Or));
    re.add_iter(a, [ReNode::new(ReType::Char('a')), ReNode::new(ReType::Char('b'))]);
    re
}

mod test_node {
    use super::*;

    #[test]
    fn dfa_builder() {
        let re = build_re();
        assert_eq!(tree_to_string(&re), "&(&(&(&(*(|('a','b')),'a'),'b'),'b'),<end>)");
    }

    #[test]
    fn dfa_id() {
        let re = build_re();
        let mut dfa = DfaBuilder::new(re);
        dfa.calc_leaf_id();
        assert_eq!(tree_to_string(&dfa.re), "&(&(&(&(*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end>)");
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

