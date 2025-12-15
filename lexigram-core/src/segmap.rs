// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

// ---------------------------------------------------------------------------------------------
// Seg

use std::collections::{BTreeMap, HashMap};
use std::collections::btree_map::{IntoIter, Iter};
use std::fmt::{Display, Formatter};
use std::ops::Bound::Included;
use crate::char_reader::{UTF8_HIGH_MIN, UTF8_LOW_MAX, UTF8_MAX, UTF8_MIN};
use crate::char_reader::escape_char;

#[derive(Clone, Copy, PartialOrd, PartialEq, Eq, Ord, Debug)]
pub struct Seg(pub u32, pub u32);

impl Seg {
    /// low segment of Unicode codepoint values:
    pub const DOT_LOW: Seg = Seg(UTF8_MIN, UTF8_LOW_MAX);
    /// high segment of Unicode codepoint values:
    pub const DOT_HIGH: Seg = Seg(UTF8_HIGH_MIN, UTF8_MAX);
}

impl Display for Seg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0 == self.1 {
            write!(f, "'{}'", escape_char(char::from_u32(self.0).unwrap()))
        } else {
            write!(f, "'{}'-'{}'", escape_char(char::from_u32(self.0).unwrap()), escape_char(char::from_u32(self.1).unwrap()))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SegMap<T>(BTreeMap<Seg, T>);

impl<T: Clone> SegMap<T> {
    pub fn new() -> Self {
        SegMap(BTreeMap::new())
    }

    pub fn keys(&self) -> impl Iterator<Item = &Seg> {
        self.0.keys()
    }

    pub fn from_iter<I: IntoIterator<Item = (Seg, T)>>(iter: I) -> Self {
        SegMap(BTreeMap::from_iter(iter))
    }

    pub fn get(&self, value: u32) -> Option<T> {
        let (Seg(_a, b), data) = self.0.range((Included(&Seg(0, 0)), Included(&Seg(value, u32::MAX)))).next_back()?;
        if *b >= value {
            Some(data.clone())
        } else {
            None
        }
    }

    pub fn insert(&mut self, key: Seg, value: T) -> Option<T> {
        self.0.insert(key, value)
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn iter(&self) -> Iter<'_, Seg, T> {
        self.into_iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T: Clone, const N: usize> From<[(Seg, T); N]> for SegMap<T> {
    fn from(value: [(Seg, T); N]) -> Self {
        SegMap(BTreeMap::from(value))
    }
}

impl<T> IntoIterator for SegMap<T> {
    type Item = (Seg, T);
    type IntoIter = IntoIter<Seg, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a SegMap<T> {
    type Item = (&'a Seg, &'a T);
    type IntoIter = Iter<'a, Seg, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[inline]
pub fn char_to_group(ascii_to_group: &[GroupId], utf8_to_group: &HashMap<char, GroupId>, seg_to_group: &SegMap<GroupId>, symbol: char) -> Option<GroupId> {
    if symbol.len_utf8() == 1 {
        Some(ascii_to_group[u8::try_from(symbol).unwrap() as usize])
    } else {
        utf8_to_group.get(&symbol).cloned().or_else(|| seg_to_group.get(symbol as u32))
    }
}

pub mod macros {
    /// Generates a Seg (tuple of u32 values) from one or two values (characters or integers).
    ///
    /// # Example
    /// ```
    /// # use lexigram_lib::{seg };
    /// assert_eq!(seg!('a'), Seg('a' as u32, 'a' as u32));
    /// assert_eq!(seg!('0'-'9'), Seg('0' as u32, '9' as u32));
    /// ```
    #[macro_export]
    macro_rules! seg {
        ($($a1:literal)?$($a2:ident)? - $($b1:literal)?$($b2:ident)?) => { $crate::segmap::Seg($crate::utf8!($($a1)?$($a2)?), $crate::utf8!($($b1)?$($b2)?)) };
        ($($a1:literal)?$($a2:ident)?) => { $crate::segmap::Seg($crate::utf8!($($a1)?$($a2)?), $crate::utf8!($($a1)?$($a2)?)) };
    }
}

pub type GroupId = u32;