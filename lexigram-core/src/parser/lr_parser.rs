use std::fmt::{Display, Formatter};
use crate::AltId;

/// State index
pub type StateId = u16;

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub enum LRAction {
    #[default]
    Error,
    Shift(StateId),
    Reduce(AltId),
    Accept,
}

impl Display for LRAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LRAction::Error => write!(f, "-"),
            LRAction::Shift(s) => write!(f, "s{s}"),
            LRAction::Reduce(a) => write!(f, "r{a}"),
            LRAction::Accept => write!(f, "acc"),
        }
    }
}

/// Parser object. The [new(...)](LLParser::new) method creates a new instance.
pub struct LLParser {

}
