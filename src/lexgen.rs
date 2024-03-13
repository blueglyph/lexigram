#![allow(unused)]
use crate::Dfa;

pub struct LexGen {
    dfa: Dfa
}

impl LexGen {
    pub fn new(dfa: Dfa) -> Self {
        LexGen { dfa }
    }


}