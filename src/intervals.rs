use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut, RangeInclusive};
use std::collections::btree_map::{IntoIter, Iter};
use std::ops::Bound::Included;
use crate::{btreeset, escape_char};

// ---------------------------------------------------------------------------------------------
// Intervals

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub struct Segments(pub BTreeSet<Seg>);

// impl Clone for Intervals {
//     fn clone(&self) -> Self {
//         println!("cloning {self:?}");
//         Intervals(self.0.clone())
//     }
// }

impl Segments {
    pub fn empty() -> Self {
        Segments(btreeset![])
    }

    pub fn new(Seg(a, b): Seg) -> Self {
        if a <= b {
            Segments(btreeset![Seg(a, b)])
        } else {
            Self::empty()
        }
    }

    pub fn insert(&mut self, seg: Seg) {
        if seg.0 <= seg.1 {
            self.0.insert(seg);
        }
    }

    pub fn from_char(char: char) -> Self {
        Segments(btreeset![Seg(char as u32, char as u32)])
    }

    pub fn to_char(&self) -> Option<char> {
        if self.len() == 1 {
            let first = self.first().unwrap();
            if first.0 == first.1 {
                return char::from_u32(first.0)
            }
        }
        None
    }

    pub fn intersect_char(&self, char: char) -> IntervalsCmp {
        let c = char as u32;
        let ci = Segments(btreeset![Seg(c, c)]);
        for &Seg(a, b) in &self.0 {
            if a <= c && c <= b {
                let mut inside = self.clone();
                inside.replace(Seg(a, b)).expect("cannot extract original data");
                if a < c {
                    inside.insert(Seg(a, c - 1));
                }
                if c < b {
                    inside.insert(Seg(c + 1, b));
                }
                return IntervalsCmp { common: ci, internal: inside, external: Self::empty() };
            }
        }
        IntervalsCmp {
            common: Self::empty(),
            internal: self.clone(),
            external: Self::empty(),
        }
    }

    // (a, b) inter (c, d) => (common, internal a-b, external a-b)
    // only processes a <= c || (a == c && b <= d)
    pub fn segment_intersect(Seg(a, b): Seg, Seg(c, d): Seg) -> IntervalsCmp {
        if a < c || (a == c && b <= d) {
            if a < c {
                if b < c {
                    IntervalsCmp { common: Segments::empty(), internal: Segments::new(Seg(a, b)), external: Segments::new(Seg(c, d)) }
                } else if b <= d {
                    IntervalsCmp { common: Segments::new(Seg(c, b)), internal: Segments::new(Seg(a, c - 1)), external: Segments::new(Seg(b + 1, d)) }
                } else {
                    IntervalsCmp { common: Segments::new(Seg(c, d)), internal: Segments(btreeset![Seg(a, c - 1), Seg(d + 1, b)]), external: Segments::empty() }
                }
            } else {
                IntervalsCmp { common: Segments::new(Seg(a, b)), internal: Segments::empty(), external: Segments::new(Seg(b + 1, d)) }
            }
        } else {
            Self::segment_intersect(Seg(c, d), Seg(a, b)).inverse()
        }
    }

    pub fn intersect(&self, other: &Self) -> IntervalsCmp {
        let mut ab_iter = self.iter();
        let mut cd_iter = other.iter();
        let mut ab = ab_iter.next().cloned();
        let mut cd = cd_iter.next().cloned();
        let mut result = IntervalsCmp::empty();
        while let (Some(new_ab), Some(new_cd)) = (ab, cd) {
            let mut cmp = Self::segment_intersect(new_ab, new_cd);
            if cmp.common.is_empty() {
                if new_ab.1 < new_cd.0 {
                    result.internal.insert(new_ab);
                    ab = ab_iter.next().cloned();
                } else {
                    result.external.insert(new_cd);
                    cd = cd_iter.next().cloned();
                }
            } else {
                if new_ab.1 > new_cd.1 { // processes the trailing segment
                    ab = cmp.internal.pop_last();
                } else {
                    ab = ab_iter.next().cloned();
                }
                if new_cd.1 > new_ab.1 {
                    cd = cmp.external.pop_last();
                } else {
                    cd = cd_iter.next().cloned();
                }
                result.extend(&cmp);
            }
        }
        if let Some(ab) = ab {
            result.internal.insert(ab);
            result.internal.extend(ab_iter);
        } else if let Some(cd) = cd {
            result.external.insert(cd);
            result.external.extend(cd_iter);
        }
        result
    }

    /// Partitions the intervals in fonction of `other`'s intervals, splitting the current segments
    /// according to `other` and adding intervals from `other`. Can be used iteratively on a collection
    /// of Intervals to obtain a partition of their segments.
    ///
    /// Returns `true` if the intervals were modified.
    ///
    /// Example:
    /// ```
    /// use std::collections::BTreeSet;
    /// use rlexer::intervals::{Segments, Seg};
    ///
    /// let mut a = Segments(BTreeSet::from([Seg(0, 10), Seg(20, 30)]));
    /// let b = Segments(BTreeSet::from([Seg(5, 6), Seg(15, 25)]));
    /// assert!(a.add_partition(&b));
    /// assert_eq!(a.0, BTreeSet::from([Seg(0, 4), Seg(5, 6), Seg(7, 10), Seg(15, 19), Seg(20, 25), Seg(26, 30)]));
    /// ```
    pub fn add_partition(&mut self, other: &Self) -> bool {
        let cmp = self.intersect(other);
        if !(cmp.common.is_empty() && cmp.external.is_empty()) {
            self.clear();
            self.extend(cmp.internal.0);
            self.extend(cmp.common.0);
            self.extend(cmp.external.0);
            true
        } else {
            false
        }
    }

    pub fn normalize(&mut self) {
        if !self.is_empty() {
            let mut new = BTreeSet::<Seg>::new();
            let mut intervals = std::mem::take(&mut self.0).into_iter();
            let mut last = intervals.next().unwrap();
            while let Some(Seg(a, b)) = intervals.next() {
                if a > last.1 + 1 {
                    new.insert(last);
                    last = Seg(a, b);
                } else {
                    last.1 = b;
                }
            }
            new.insert(last);
            self.0 = new;
        }
    }

    pub fn chars(&self) -> ReTypeCharIter {
        ReTypeCharIter { intervals: Some(self.0.clone()), range: None }
    }
}

impl Deref for Segments {
    type Target = BTreeSet<Seg>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Segments {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// #[inline]
// pub fn segment_to_string((a, b): &(u32, u32)) -> String {
//     if a == b {
//         format!("'{}'", escape_char(char::from_u32(*a).unwrap()))
//     } else {
//         format!("'{}'-'{}'", escape_char(char::from_u32(*a).unwrap()), escape_char(char::from_u32(*b).unwrap()))
//     }
// }

impl Display for Segments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = self.to_char() {
            write!(f, "'{}'", escape_char(c))
        } else {
            if self.0.len() == 1 {
                write!(f, "{}", self.0.iter()
                    .map(|seg| seg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
                )
            } else {
                write!(f, "[{}]", self.0.iter()
                    .map(|seg| seg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntervalsCmp {
    pub common: Segments,      // common to self and other
    pub internal: Segments,    // only in self, external to other
    pub external: Segments     // external to self, only in other
}

impl IntervalsCmp {
    pub fn empty() -> Self {
        IntervalsCmp { common: Segments::empty(), internal: Segments::empty(), external: Segments::empty() }
    }

    pub fn inverse(self) -> Self {
        IntervalsCmp { common: self.common, internal: self.external, external: self.internal }
    }

    pub fn extend(&mut self, other: &Self) {
        self.common.extend(other.common.iter());
        self.internal.extend(other.internal.iter());
        self.external.extend(other.external.iter());
    }

    pub fn normalize(&mut self) {
        self.common.normalize();
        self.internal.normalize();
        self.external.normalize();
    }
}

impl Display for IntervalsCmp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<common: {}, internal: {}, external: {}>", self.common, self.internal, self.external)
    }
}

pub struct ReTypeCharIter {
    intervals: Option<BTreeSet<Seg>>,
    range: Option<RangeInclusive<u32>>
}

impl Iterator for ReTypeCharIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next = self.range.as_mut().and_then(|r| r.next());
        if next.is_none() {
            if let Some(interval) = &mut self.intervals {
                if let Some(Seg(a, b)) = interval.pop_first() {
                    self.range = Some(a..=b);
                    next = self.range.as_mut().and_then(|r| r.next());
                }
            }
        }
        next.map(|code| char::from_u32(code).unwrap())
    }
}

// ---------------------------------------------------------------------------------------------
// Seg

#[derive(Clone, Copy, PartialOrd, PartialEq, Eq, Ord, Debug)]
pub struct Seg(pub u32, pub u32);

impl Display for Seg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0 == self.1 {
            write!(f, "'{}'", escape_char(char::from_u32(self.0).unwrap()))
        } else {
            write!(f, "'{}'-'{}'", escape_char(char::from_u32(self.0).unwrap()), escape_char(char::from_u32(self.1).unwrap()))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SegMap<T>(BTreeMap<Seg, T>);

impl<T: Clone> SegMap<T> {
    pub fn new() -> Self {
        SegMap(BTreeMap::new())
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

// ---------------------------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::fmt::{LowerHex, UpperHex};
    use crate::segments;
    use super::*;

    /// "{:x}" is used to show the raw intervals with codes
    impl LowerHex for Segments {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[{}]", self.iter()
                .map(|Seg(a, b)| if a == b { format!("{a}") } else { format!("{a}-{b}") })
                .collect::<Vec<_>>()
                .join(", ")
            )
        }
    }

    /// "{:X}" is used to show the raw intervals with characters
    impl UpperHex for Segments {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[{}]", self.iter()
                .map(|Seg(a, b)| if a == b {
                    format!("'{}'", escape_char(char::from_u32(*a).unwrap()))
                } else {
                    format!("'{}'-'{}'", escape_char(char::from_u32(*a).unwrap()), escape_char(char::from_u32(*b).unwrap()))
                })
                .collect::<Vec<_>>()
                .join(", ")
            )
        }
    }

    impl LowerHex for IntervalsCmp {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "<common: {:x}, internal: {:x}, external: {:x}>", self.common, self.internal, self.external)
        }
    }

    fn new_cmp(c: Seg, i: Seg, e: Seg) -> IntervalsCmp {
        IntervalsCmp { common: Segments::new(c), internal: Segments::new(i), external: Segments::new(e) }
    }

    fn build_intervals() -> Vec<(Seg, Seg, IntervalsCmp)> {
        vec![
            (Seg(1, 2), Seg(3, 4), new_cmp(Seg(9, 0), Seg(1, 2), Seg(3, 4))),
            (Seg(1, 2), Seg(2, 3), new_cmp(Seg(2, 2), Seg(1, 1), Seg(3, 3))),
            (Seg(1, 3), Seg(2, 4), new_cmp(Seg(2, 3), Seg(1, 1), Seg(4, 4))),
            (Seg(1, 3), Seg(2, 3), new_cmp(Seg(2, 3), Seg(1, 1), Seg(9, 0))),
            (Seg(1, 4), Seg(2, 3), IntervalsCmp { common: Segments::new(Seg(2, 3)), internal: Segments(btreeset![Seg(1, 1), Seg(4, 4)]), external: Segments::empty() }),
            (Seg(1, 2), Seg(1, 3), new_cmp(Seg(1, 2), Seg(9, 0), Seg(3, 3))),
            (Seg(1, 2), Seg(1, 2), new_cmp(Seg(1, 2), Seg(9, 0), Seg(9, 0))),
            (Seg(1, 3), Seg(1, 2), new_cmp(Seg(1, 2), Seg(3, 3), Seg(9, 0))),
            (Seg(2, 3), Seg(1, 4), IntervalsCmp { common: Segments::new(Seg(2, 3)), internal: Segments::empty(), external: Segments(btreeset![Seg(1, 1), Seg(4, 4)]) }),
            (Seg(2, 3), Seg(1, 3), new_cmp(Seg(2, 3), Seg(9, 0), Seg(1, 1))),
            (Seg(2, 4), Seg(1, 3), new_cmp(Seg(2, 3), Seg(4, 4), Seg(1, 1))),
            (Seg(2, 3), Seg(1, 2), new_cmp(Seg(2, 2), Seg(3, 3), Seg(1, 1))),
            (Seg(3, 4), Seg(1, 2), new_cmp(Seg(9, 0), Seg(3, 4), Seg(1, 2))),
        ]
    }

    #[test]
    fn interval_segment_intersect() {
        let tests = build_intervals();
        for (idx, (ab, cd, expected_cmp)) in tests.into_iter().enumerate() {
            let cmp = Segments::segment_intersect(ab, cd);
            // println!("{a}-{b}, {c}-{d}: {cmp:x}");
            assert_eq!(cmp, expected_cmp, "test {idx} failed");
        }
    }

    #[test]
    fn interval_intersect() {
        for scale in [10, 4] {
            let iv = build_intervals();
            let mut ab = Segments::empty();
            let mut cd = Segments::empty();
            let mut expected_cmp = IntervalsCmp::empty();
            for (idx, (Seg(a, b), Seg(c, d), cmp)) in iv.into_iter().enumerate() {
                let offset = scale * idx as u32;
                ab.insert(Seg(a + offset, b + offset));
                cd.insert(Seg(c + offset, d + offset));
                expected_cmp.common.extend(cmp.common.iter().map(|Seg(a, b)| Seg(*a + offset, *b + offset)));
                expected_cmp.internal.extend(cmp.internal.iter().map(|Seg(a, b)| Seg(*a + offset, *b + offset)));
                expected_cmp.external.extend(cmp.external.iter().map(|Seg(a, b)| Seg(*a + offset, *b + offset)));
            }
            let msg = format!("test failed for scale {scale}");
            let cmp = ab.intersect(&cd);
            assert_eq!(cmp, expected_cmp, "{}", msg);
            let cmp = cd.intersect(&ab);
            assert_eq!(cmp, expected_cmp.clone().inverse(), "{}", msg);
            let cmp = ab.intersect(&Segments::empty());
            assert_eq!(cmp, IntervalsCmp { common: Segments::empty(), internal: ab.clone(), external: Segments::empty() }, "{}", msg);
            let cmp = Segments::empty().intersect(&ab);
            assert_eq!(cmp, IntervalsCmp { common: Segments::empty(), internal: Segments::empty(), external: ab.clone() }, "{}", msg);
            ab.normalize();
            cd.normalize();
            expected_cmp.normalize();
            let cmp = ab.intersect(&cd);
            assert_eq!(cmp, expected_cmp);
            let cmp = cd.intersect(&ab);
            assert_eq!(cmp, expected_cmp.inverse());
        }
    }

    #[test]
    fn interval_intersect_corner() {
        let tests: Vec<(Segments, Segments, (Segments, Segments, Segments))> = vec![
            (segments![1 - 50], segments![10 - 20, 30 - 40],
             (segments![10-20, 30-40], segments![1-9, 21-29, 41-50], segments![])),
            (segments![1-10, 11-15, 16-20, 21-35, 36-37, 38-50], segments![10-20, 30-40],
             (segments![10-20, 30-40], segments![1-9, 21-29, 41-50], segments![])),
            (segments![0-9], segments![0-0, 1-9],
             (segments![0-9], segments![], segments![])),
            (segments![], segments![],
             (segments![], segments![], segments![])),
        ];
        for (idx, (ab, cd, expected_cmp)) in tests.into_iter().enumerate() {
            // let ab = Intervals(ab);
            // let cd = Intervals(cd);
            let expected_cmp = IntervalsCmp { common: expected_cmp.0, internal: expected_cmp.1, external: expected_cmp.2 };
            let mut cmp = ab.intersect(&cd);
            cmp.normalize();
            assert_eq!(cmp, expected_cmp, "test {idx} failed");
            let mut cmp = cd.intersect(&ab);
            cmp.normalize();
            assert_eq!(cmp, expected_cmp.inverse(), "test {idx} failed");
        }
    }

    #[test]
    fn interval_partition() {
        let tests: Vec<(Segments, Segments, Segments)> = vec![
            (segments![1-4], segments![3-6], segments![1-2, 3-4, 5-6]),
            (segments![1-4], segments![5-6], segments![1-4, 5-6]),
            (segments![1-6], segments![3-4], segments![1-2, 3-4, 5-6]),
            (segments![1-4, 5-10], segments![], segments![1-4, 5-10]),
            (segments![], segments![1-4, 5-10], segments![1-4, 5-10]),
            (segments![1-4, 5-10], segments![3-5], segments![1-2, 3-4, 5-5, 6-10]),
        ];
        for (idx, (mut ab, cd, exp)) in tests.into_iter().enumerate() {
            // let mut ab = Intervals(BTreeSet::<(u32, u32)>::from_iter(ab));
            // let cd = Intervals(BTreeSet::from_iter(cd));
            ab.add_partition(&cd);
            let expected = exp;
            assert_eq!(ab, expected, "test {idx} failed");
        }
    }

    #[test]
    fn interval_chars() {
        let tests = vec![
            (segments!['a'-'a'], "a"),
            (segments!['a'-'d'], "abcd"),
            (segments!['a'-'c', 'x'-'z'], "abcxyz"),
            (segments!['a'-'b', 'd'-'d', 'f'-'f', 'x'-'z'], "abdfxyz"),
        ];
        for (idx, (intervals, expected)) in tests.into_iter().enumerate() {
            let result = intervals.chars().collect::<String>();
            assert_eq!(result, expected, "test {idx} failed");
        }
    }
}