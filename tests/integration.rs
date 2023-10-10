// Copyright 2023 Redglyph
//
// Integration tests: tests that all the functionalities are accessible and work as expected.

#![cfg(test)]

mod basic {
    use rlexer::{ReNodeId, ReNode};

    #[test]
    fn instantiation() {
        let n1 = ReNodeId::char('a');
        let n2 = ReNodeId::char('a');
        let n3 = ReNodeId::str("ab");
        assert_eq!(n1, n2);
        assert_ne!(n1, n3);
    }
}