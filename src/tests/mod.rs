// Copyright 2023 Redglyph
//
// Unit tests

#![cfg(test)]

mod test_node {
    use std::cell::RefCell;
    use crate::{DfaBuilder, ReNode, ReNodeId};

    #[test]
    fn test_basic() {
        let n1 = ReNodeId::str("abcd");
        let n2 = ReNodeId::concat([n1.clone(), ReNodeId::char('e')]);
        assert_eq!(n1, ReNodeId { id: RefCell::new(0), op: ReNode::Concat(vec![
            ReNodeId { id: RefCell::new(0), op: ReNode::Char('a') },
            ReNodeId { id: RefCell::new(0), op: ReNode::Char('b') },
            ReNodeId { id: RefCell::new(0), op: ReNode::Char('c') },
            ReNodeId { id: RefCell::new(0), op: ReNode::Char('d') }]),
        });
        assert_eq!(n2, ReNodeId { id: RefCell::new(0), op: ReNode::Concat(vec![
            ReNodeId { id: RefCell::new(0), op: ReNode::Concat(vec![
                ReNodeId { id: RefCell::new(0), op: ReNode::Char('a') },
                ReNodeId { id: RefCell::new(0), op: ReNode::Char('b') },
                ReNodeId { id: RefCell::new(0), op: ReNode::Char('c') },
                ReNodeId { id: RefCell::new(0), op: ReNode::Char('d') }])
            },
            ReNodeId { id: RefCell::new(0), op: ReNode::Char('e') },
        ])});
    }

    #[test]
    fn test_ids() {
        let n = ReNodeId::concat([
            ReNodeId::or([
                ReNodeId::char('e'),
                ReNodeId::char('f')
            ]),
            ReNodeId::star(
                ReNodeId::char('g')
            ),
            ReNodeId::char('d')
        ]);
        let builder = DfaBuilder::new(n);
        builder.re.print();
        //dbg!(builder.re);
    }
}

