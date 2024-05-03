#![cfg(test)]

use super::{GrNode, Symbol, VarId};
use crate::dfa::TokenId;
use crate::gnode;

#[test]
fn gnode() {
    let symbols = [
        ("Arrow".to_string(), Some("->".to_string())),
        ("Colon".to_string(), Some(":".to_string())),
        ("Id".to_string(), None),
    ];
    let t: GrNode = gnode!(t crate::regexgen::Id::Arrow);
    assert_eq!(t.to_string(), "[0]");
    assert_eq!(t.to_str(&symbols), "'->'");
    if let GrNode::Symbol(s) = t {
        assert_eq!(s.to_string(), "[0]");
        assert_eq!(s.to_str(&symbols), "'->'");
    } else {
        panic!();
    };
    assert_eq!(gnode!([1]), GrNode::Symbol(Symbol::T(1 as TokenId)));
    assert_eq!(gnode!(t 2), GrNode::Symbol(Symbol::T(2 as TokenId)));
    assert_eq!(gnode!(nt 3), GrNode::Symbol(Symbol::NT(3 as VarId)));
    assert_eq!(gnode!(e), GrNode::Symbol(Symbol::Empty));
    assert_eq!(gnode!(&), GrNode::Concat);
    assert_eq!(gnode!(|), GrNode::Or);
    assert_eq!(gnode!(?), GrNode::Maybe);
    assert_eq!(gnode!(+), GrNode::Plus);
    assert_eq!(gnode!(*), GrNode::Star);
}
