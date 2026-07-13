// Copyright (c) 2026 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

mod ll1;
mod lalr;

// -------------------------------------------------------------------------
// test helper

#[allow(unused)]
mod level_string {
    use std::cmp::max;

    #[derive(Debug, PartialEq)]
    pub struct LevelString(pub u32, pub String);

    impl LevelString {
        pub fn get_string(self) -> String {
            self.1
        }
    }

    pub fn par(ls: LevelString) -> String {
        if ls.0 > 0 {
            format!("({})", ls.1)
        } else {
            ls.1
        }
    }

    pub fn ls_prefix_op(op: &str, ls: LevelString) -> LevelString {
        LevelString(ls.0 + 1, format!("{op} {}", par(ls)))
    }

    pub fn ls_suffix_op(op: &str, ls: LevelString) -> LevelString {
        LevelString(ls.0 + 1, format!("{} {op}", par(ls)))
    }

    pub fn ls_binary_op(op: &str, lsleft: LevelString, lsright: LevelString) -> LevelString {
        LevelString(max(lsleft.0, lsright.0) + 1, format!("{} {op} {}", par(lsleft), par(lsright)))
    }
}
