use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut, RangeInclusive};
use crate::{btreeset, escape_char};

#[derive(Clone, Debug, PartialEq, Default, PartialOrd, Eq, Ord)]
pub struct Intervals(pub BTreeSet<(u32, u32)>);

impl Intervals {
    pub fn empty() -> Intervals {
        Intervals(btreeset![])
    }

    pub fn new((a, b): (u32, u32)) -> Self {
        if a <= b {
            Intervals(btreeset![(a, b)])
        } else {
            Self::empty()
        }
    }

    pub fn insert(&mut self, (a, b): (u32, u32)) {
        if a <= b {
            self.0.insert((a, b));
        }
    }

    pub fn from_char(char: char) -> Self {
        Intervals(btreeset![(char as u32, char as u32)])
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
        let ci = Intervals(btreeset![(c, c)]);
        for &(a, b) in &self.0 {
            if a <= c && c <= b {
                let mut inside = self.clone();
                inside.replace((a, b)).expect("cannot extract original data");
                if a < c {
                    inside.insert((a, c - 1));
                }
                if c < b {
                    inside.insert((c + 1, b));
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
    pub fn segment_intersect((a, b): (u32, u32), (c, d): (u32, u32)) -> IntervalsCmp {
        if a < c || (a == c && b <= d) {
            if a < c {
                if b < c {
                    IntervalsCmp { common: Intervals::empty(), internal: Intervals::new((a, b)), external: Intervals::new((c, d)) }
                } else if b <= d {
                    IntervalsCmp { common: Intervals::new((c, b)), internal: Intervals::new((a, c - 1)), external: Intervals::new((b + 1, d)) }
                } else {
                    IntervalsCmp { common: Intervals::new((c, d)), internal: Intervals(btreeset![(a, c - 1), (d + 1, b)]), external: Intervals::empty() }
                }
            } else {
                IntervalsCmp { common: Intervals::new((a, b)), internal: Intervals::empty(), external: Intervals::new((b + 1, d)) }
            }
        } else {
            Self::segment_intersect((c, d), (a, b)).inverse()
        }
    }

    pub fn intersect(&self, other: &Intervals) -> IntervalsCmp {
        let mut ab_iter = self.iter();
        let mut cd_iter = other.iter();
        let mut ab = ab_iter.next().cloned();
        let mut cd = cd_iter.next().cloned();
        let mut result = IntervalsCmp::empty();
        while let (Some((a, b)), Some((c, d))) = (ab, cd) {
            let mut cmp = Self::segment_intersect((a, b), (c, d));
            if cmp.common.is_empty() {
                if b < c {
                    result.internal.insert((a, b));
                    ab = ab_iter.next().cloned();
                } else {
                    result.external.insert((c, d));
                    cd = cd_iter.next().cloned();
                }
            } else {
                if b > d { // processes the trailing segment
                    ab = cmp.internal.pop_last();
                } else {
                    ab = ab_iter.next().cloned();
                }
                if d > b {
                    cd = cmp.external.pop_last();
                } else {
                    cd = cd_iter.next().cloned()
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
    /// use rlexer::intervals::Intervals;
    ///
    /// let mut a = Intervals(BTreeSet::from([(0, 10), (20, 30)]));
    /// let b = Intervals(BTreeSet::from([(5, 6), (15, 25)]));
    /// assert!(a.partition(&b));
    /// assert_eq!(a.0, BTreeSet::<(u32, u32)>::from([(0, 4), (5, 6), (7, 10), (15, 19), (20, 25), (26, 30)]));
    /// ```
    pub fn partition(&mut self, other: &Self) -> bool {
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
            let mut new = BTreeSet::<(u32, u32)>::new();
            let mut intervals = std::mem::take(&mut self.0).into_iter();
            let mut last = intervals.next().unwrap();
            while let Some((a, b)) = intervals.next() {
                if a > last.1 + 1 {
                    new.insert(last);
                    last = (a, b);
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

impl Deref for Intervals {
    type Target = BTreeSet<(u32, u32)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Intervals {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[inline]
pub fn segment_to_string((a, b): &(u32, u32)) -> String {
    if a == b {
        format!("'{}'", escape_char(char::from_u32(*a).unwrap()))
    } else {
        format!("'{}'-'{}'", escape_char(char::from_u32(*a).unwrap()), escape_char(char::from_u32(*b).unwrap()))
    }
}

impl Display for Intervals {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = self.to_char() {
            write!(f, "'{}'", escape_char(c))
        } else {
            if self.0.len() == 1 {
                write!(f, "{}", self.0.iter()
                    .map(|seg| segment_to_string(seg))
                    .collect::<Vec<_>>()
                    .join(", ")
                )
            } else {
                write!(f, "[{}]", self.0.iter()
                    .map(|seg| segment_to_string(seg))
                    .collect::<Vec<_>>()
                    .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntervalsCmp {
    pub common: Intervals,      // common to self and other
    pub internal: Intervals,    // only in self, external to other
    pub external: Intervals     // external to self, only in other
}

impl IntervalsCmp {
    pub fn empty() -> Self {
        IntervalsCmp { common: Intervals::empty(), internal: Intervals::empty(), external: Intervals::empty() }
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
    intervals: Option<BTreeSet<(u32, u32)>>,
    range: Option<RangeInclusive<u32>>
}

impl Iterator for ReTypeCharIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next = self.range.as_mut().and_then(|r| r.next());
        if next.is_none() {
            if let Some(interval) = &mut self.intervals {
                if let Some((a, b)) = interval.pop_first() {
                    self.range = Some(a..=b);
                    next = self.range.as_mut().and_then(|r| r.next());
                }
            }
        }
        next.map(|code| char::from_u32(code).unwrap())
    }
}

// ---------------------------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::fmt::{LowerHex, UpperHex};
    use super::*;

    /// "{:x}" is used to show the raw intervals with codes
    impl LowerHex for Intervals {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[{}]", self.iter()
                .map(|(a, b)| if a == b { format!("{a}") } else { format!("{a}-{b}") })
                .collect::<Vec<_>>()
                .join(", ")
            )
        }
    }

    /// "{:X}" is used to show the raw intervals with characters
    impl UpperHex for Intervals {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[{}]", self.iter()
                .map(|(a, b)| if a == b {
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

    fn new_cmp(c: (u32, u32), i: (u32, u32), e: (u32, u32)) -> IntervalsCmp {
        IntervalsCmp { common: Intervals::new(c), internal: Intervals::new(i), external: Intervals::new(e) }
    }

    fn build_intervals() -> Vec<((u32, u32), (u32, u32), IntervalsCmp)> {
        vec![
            ((1, 2), (3, 4), new_cmp((9, 0), (1, 2), (3, 4))),
            ((1, 2), (2, 3), new_cmp((2, 2), (1, 1), (3, 3))),
            ((1, 3), (2, 4), new_cmp((2, 3), (1, 1), (4, 4))),
            ((1, 3), (2, 3), new_cmp((2, 3), (1, 1), (9, 0))),
            ((1, 4), (2, 3), IntervalsCmp { common: Intervals::new((2, 3)), internal: Intervals(btreeset![(1, 1), (4, 4)]), external: Intervals::empty() }),
            ((1, 2), (1, 3), new_cmp((1, 2), (9, 0), (3, 3))),
            ((1, 2), (1, 2), new_cmp((1, 2), (9, 0), (9, 0))),
            ((1, 3), (1, 2), new_cmp((1, 2), (3, 3), (9, 0))),
            ((2, 3), (1, 4), IntervalsCmp { common: Intervals::new((2, 3)), internal: Intervals::empty(), external: Intervals(btreeset![(1, 1), (4, 4)]) }),
            ((2, 3), (1, 3), new_cmp((2, 3), (9, 0), (1, 1))),
            ((2, 4), (1, 3), new_cmp((2, 3), (4, 4), (1, 1))),
            ((2, 3), (1, 2), new_cmp((2, 2), (3, 3), (1, 1))),
            ((3, 4), (1, 2), new_cmp((9, 0), (3, 4), (1, 2))),
        ]
    }

    #[test]
    fn interval_segment_intersect() {
        let tests = build_intervals();
        for (idx, ((a, b), (c, d), expected_cmp)) in tests.into_iter().enumerate() {
            let cmp = Intervals::segment_intersect((a, b), (c, d));
            // println!("{a}-{b}, {c}-{d}: {cmp:x}");
            assert_eq!(cmp, expected_cmp, "test {idx} failed");
        }
    }

    #[test]
    fn interval_intersect() {
        for scale in [10, 4] {
            let iv = build_intervals();
            let mut ab = Intervals::empty();
            let mut cd = Intervals::empty();
            let mut expected_cmp = IntervalsCmp::empty();
            for (idx, ((a, b), (c, d), cmp)) in iv.into_iter().enumerate() {
                let offset = scale * idx as u32;
                ab.insert((a + offset, b + offset));
                cd.insert((c + offset, d + offset));
                expected_cmp.common.extend(cmp.common.iter().map(|(a, b)| (*a + offset, *b + offset)));
                expected_cmp.internal.extend(cmp.internal.iter().map(|(a, b)| (*a + offset, *b + offset)));
                expected_cmp.external.extend(cmp.external.iter().map(|(a, b)| (*a + offset, *b + offset)));
            }
            let msg = format!("test failed for scale {scale}");
            let cmp = ab.intersect(&cd);
            assert_eq!(cmp, expected_cmp, "{}", msg);
            let cmp = cd.intersect(&ab);
            assert_eq!(cmp, expected_cmp.clone().inverse(), "{}", msg);
            let cmp = ab.intersect(&Intervals::empty());
            assert_eq!(cmp, IntervalsCmp { common: Intervals::empty(), internal: ab.clone(), external: Intervals::empty() }, "{}", msg);
            let cmp = Intervals::empty().intersect(&ab);
            assert_eq!(cmp, IntervalsCmp { common: Intervals::empty(), internal: Intervals::empty(), external: ab.clone() }, "{}", msg);
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
        let tests: Vec<(BTreeSet<(u32, u32)>, BTreeSet<(u32, u32)>, (BTreeSet<(u32, u32)>, BTreeSet<(u32, u32)>, BTreeSet<(u32, u32)>))> = vec![
            (btreeset![(1, 50)], btreeset![(10, 20), (30, 40)],
             (btreeset![(10, 20), (30, 40)], btreeset![(1, 9), (21, 29), (41, 50)], btreeset![])),
            (btreeset![(1, 10), (11, 15), (16, 20), (21, 35), (36, 37), (38, 50)], btreeset![(10, 20), (30, 40)],
             (btreeset![(10, 20), (30, 40)], btreeset![(1, 9), (21, 29), (41, 50)], btreeset![])),
            (btreeset![(0, 9)], btreeset![(0, 0), (1, 9)],
             (btreeset![(0, 9)], btreeset![], btreeset![])),
            (btreeset![], btreeset![],
             (btreeset![], btreeset![], btreeset![])),
        ];
        for (idx, (ab, cd, expected_cmp)) in tests.into_iter().enumerate() {
            let ab = Intervals(ab);
            let cd = Intervals(cd);
            let expected_cmp = IntervalsCmp { common: Intervals(expected_cmp.0), internal: Intervals(expected_cmp.1), external: Intervals(expected_cmp.2) };
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
        let tests: Vec<(Vec<(u32, u32)>, Vec<(u32, u32)>, Vec<(u32, u32)>)> = vec![
            (vec![(1, 4)], vec![(3, 6)], vec![(1, 2), (3, 4), (5, 6)]),
            (vec![(1, 4)], vec![(5, 6)], vec![(1, 4), (5, 6)]),
            (vec![(1, 6)], vec![(3, 4)], vec![(1, 2), (3, 4), (5, 6)]),
            (vec![(1, 4), (5, 10)], vec![], vec![(1, 4), (5, 10)]),
            (vec![], vec![(1, 4), (5, 10)], vec![(1, 4), (5, 10)]),
            (vec![(1, 4), (5, 10)], vec![(3, 5)], vec![(1, 2), (3, 4), (5, 5), (6, 10)]),
        ];
        for (idx, (ab, cd, exp)) in tests.into_iter().enumerate() {
            let mut ab = Intervals(BTreeSet::<(u32, u32)>::from_iter(ab));
            let cd = Intervals(BTreeSet::from_iter(cd));
            ab.partition(&cd);
            let expected = Intervals(BTreeSet::from_iter(exp));
            assert_eq!(ab, expected, "test {idx} failed");
        }
    }

    #[test]
    fn interval_chars() {
        let tests = vec![
            (btreeset![('a', 'a')], "a"),
            (btreeset![('a', 'd')], "abcd"),
            (btreeset![('a', 'c'), ('x', 'z')], "abcxyz"),
            (btreeset![('a', 'b'), ('d', 'd'), ('f', 'f'), ('x', 'z')], "abdfxyz"),
        ];
        for (idx, (set, expected)) in tests.into_iter().enumerate() {
            let intervals = Intervals(set.into_iter().map(|(a, b)| (a as u32, b as u32)).collect());
            let result = intervals.chars().collect::<String>();
            assert_eq!(result, expected, "test {idx} failed");
        }
    }
}