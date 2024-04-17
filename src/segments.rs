use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut, RangeInclusive};
use std::collections::btree_map::{IntoIter, Iter};
use std::ops::Bound::Included;
use crate::{btreeset, escape_char};
use crate::io::{UTF8_LOW_MAX, UTF8_HIGH_MIN, UTF8_MAX, UTF8_MIN, UTF8_GAP_MIN, UTF8_GAP_MAX};

#[cfg(test)]
use std::fmt::{LowerHex, UpperHex};

// ---------------------------------------------------------------------------------------------
// Segments

#[derive(Clone, PartialEq, Default, PartialOrd, Eq, Ord)]
pub struct Segments(pub BTreeSet<Seg>);

// impl Clone for Segments {
//     fn clone(&self) -> Self {
//         println!("cloning {self:?}");
//         Segments(self.0.clone())
//     }
// }

impl Segments {

    #[inline]
    pub fn empty() -> Self {    // TODO: rename to `new()`
        Segments(btreeset![])
    }

    pub fn new(Seg(a, b): Seg) -> Self {
        if a <= b {
            Segments(btreeset![Seg(a, b)])
        } else {
            Self::empty()
        }
    }

    #[inline]
    pub fn is_dot(&self) -> bool {
        self.len() == 2 && self.first().unwrap() == &Seg::DOT_LOW && self.last().unwrap() == &Seg::DOT_HIGH
    }

    #[inline]
    pub fn dot() -> Segments {
        Segments(BTreeSet::from([Seg::DOT_LOW, Seg::DOT_HIGH]))
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

    pub fn intersect_char(&self, char: char) -> SegmentsCmp {
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
                return SegmentsCmp { common: ci, internal: inside, external: Self::empty() };
            }
        }
        SegmentsCmp {
            common: Self::empty(),
            internal: self.clone(),
            external: Self::empty(),
        }
    }

    // (a, b) inter (c, d) => (common, internal a-b, external a-b)
    // only processes a <= c || (a == c && b <= d)
    pub fn segment_intersect(Seg(a, b): Seg, Seg(c, d): Seg) -> SegmentsCmp {
        if a < c || (a == c && b <= d) {
            if a < c {
                if b < c {
                    SegmentsCmp { common: Segments::empty(), internal: Segments::new(Seg(a, b)), external: Segments::new(Seg(c, d)) }
                } else if b <= d {
                    SegmentsCmp { common: Segments::new(Seg(c, b)), internal: Segments::new(Seg(a, c - 1)), external: Segments::new(Seg(b + 1, d)) }
                } else {
                    SegmentsCmp { common: Segments::new(Seg(c, d)), internal: Segments(btreeset![Seg(a, c - 1), Seg(d + 1, b)]), external: Segments::empty() }
                }
            } else {
                SegmentsCmp { common: Segments::new(Seg(a, b)), internal: Segments::empty(), external: Segments::new(Seg(b + 1, d)) }
            }
        } else {
            Self::segment_intersect(Seg(c, d), Seg(a, b)).inverse()
        }
    }

    pub fn intersect(&self, other: &Self) -> SegmentsCmp {
        let mut ab_iter = self.iter();
        let mut cd_iter = other.iter();
        let mut ab = ab_iter.next().cloned();
        let mut cd = cd_iter.next().cloned();
        let mut result = SegmentsCmp::empty();
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

    /// Partitions the segments in fonction of `other`'s segments, splitting the current segments
    /// according to `other` and adding segments from `other`. Can be used iteratively on a collection
    /// of Segments to obtain a partition of their segments.
    ///
    /// Returns `true` if the segments were modified.
    ///
    /// Example:
    /// ```
    /// use std::collections::BTreeSet;
    /// use rlexer::segments::{Segments, Seg};
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

    /// Slices the segments to match other's partition, but without merging self's initial partition.
    /// ```
    /// use std::collections::BTreeSet;
    /// use rlexer::segments::{Seg, Segments};
    /// let mut ab = Segments(BTreeSet::from([Seg(1 as u32, 50 as u32)]));
    /// let cd = Segments(BTreeSet::from([Seg(10 as u32, 20 as u32), Seg(30 as u32, 40 as u32)]));
    /// ab.slice_partitions(&cd);
    /// assert_eq!(ab, Segments(BTreeSet::from([
    ///     Seg(1 as u32, 9 as u32), Seg(10 as u32, 20 as u32), Seg(21 as u32, 29 as u32),
    ///     Seg(30 as u32, 40 as u32), Seg(41 as u32, 50 as u32)])));
    /// ```
    pub fn slice_partitions(&mut self, other: &Self) {
        let cmp = self.intersect(other);
        self.clear();
        self.extend(cmp.internal.0);
        self.extend(cmp.common.0);
    }

    pub fn normalize(&mut self) {
        if !self.is_empty() {
            let mut new = BTreeSet::<Seg>::new();
            let mut segments = std::mem::take(&mut self.0).into_iter();
            let mut last = segments.next().unwrap();
            while let Some(Seg(a, b)) = segments.next() {
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

    pub fn normalized(&self) -> Self {
        let mut n = self.clone();
        n.normalize();
        n
    }

    pub fn chars(&self) -> ReTypeCharIter {
        ReTypeCharIter { segments: Some(self.0.clone()), range: None }
    }

    /// Inserts Seg(start, stop) in the current segment, except the UTF-8 gap between
    /// UTF8_GAP_MIN (0xd800) and UTF8_GAP_MAX (0xdfff). If a part or the entirety of
    /// that gap is within [start-stop], then it's extruded first.
    pub fn insert_utf8(&mut self, start: u32, stop: u32) {
        if start <= stop {
            if stop < UTF8_GAP_MIN || start > UTF8_GAP_MAX {
                self.0.insert(Seg(start, stop));
            } else {
                if start < UTF8_GAP_MIN {
                    self.0.insert(Seg(start, UTF8_GAP_MIN - 1));
                }
                if stop > UTF8_GAP_MAX {
                    self.0.insert(Seg(UTF8_GAP_MAX + 1, stop));
                }
            }
        }
    }

    /// Negates the selection, except the UTF-8 gap between UTF8_GAP_MIN (0xd800) and
    /// UTF8_GAP_MAX (0xdfff), which is always excluded.
    pub fn not(&self) -> Self {
        let mut inv = Segments::empty();
        let mut start = 0;
        for seg in &self.0 {
            if seg.0 > start {
                inv.insert_utf8(start, seg.0 - 1);
            }
            start = seg.1 + 1;
        }
        if start < UTF8_MAX {
            inv.insert_utf8(start, UTF8_MAX);
        }
        inv
    }
}

impl<const N: usize> From<[Seg; N]> for Segments {
    /// Converts a `[Seg; N]` into a `Segments`.
    ///
    /// ```
    ///
    ///
    /// use rlexer::segments::{Seg, Segments};
    /// let set1 = Segments::from([Seg('a' as u32, 'z' as u32), Seg('0' as u32, '9' as u32)]);
    /// ```
    fn from(arr: [Seg; N]) -> Self {
        Segments(BTreeSet::from(arr))
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

impl Debug for Segments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Segments({})", self.0.iter().map(|seg| format!("Seg(0x{:x}, 0x{:x})", seg.0, seg.1)).collect::<Vec<_>>().join(", "))
    }
}

impl Display for Segments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = self.to_char() {
            write!(f, "'{}'", escape_char(c))
        } else {
            if self.is_dot() {
                write!(f, "DOT")
            } else {
                write!(f, "{}", self.0.iter()
                    .map(|seg| seg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
                )
            }
        }
    }
}

#[cfg(test)]
/// "{:x}" is used to show the raw segments with codes
impl LowerHex for Segments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.iter()
            .map(|Seg(a, b)| if a == b { format!("{a}") } else { format!("{a}-{b}") })
            .collect::<Vec<_>>()
            .join(", ")
        )
    }
}

#[cfg(test)]
/// "{:X}" is used to show the raw segments with characters
impl UpperHex for Segments {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.iter()
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

#[derive(Debug, Clone, PartialEq)]
pub struct SegmentsCmp {
    pub common: Segments,      // common to self and other
    pub internal: Segments,    // only in self, external to other
    pub external: Segments     // external to self, only in other
}

impl SegmentsCmp {
    pub fn empty() -> Self {
        SegmentsCmp { common: Segments::empty(), internal: Segments::empty(), external: Segments::empty() }
    }

    pub fn inverse(self) -> Self {
        SegmentsCmp { common: self.common, internal: self.external, external: self.internal }
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

impl Display for SegmentsCmp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<common: {}, internal: {}, external: {}>", self.common, self.internal, self.external)
    }
}

#[cfg(test)]
/// "{:x}" is used to show the raw segments with codes
impl LowerHex for SegmentsCmp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<common: {:x}, internal: {:x}, external: {:x}>", self.common, self.internal, self.external)
    }
}

pub struct ReTypeCharIter {
    segments: Option<BTreeSet<Seg>>,
    range: Option<RangeInclusive<u32>>
}

impl Iterator for ReTypeCharIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next = self.range.as_mut().and_then(|r| r.next());
        if next.is_none() {
            if let Some(segments) = &mut self.segments {
                if let Some(Seg(a, b)) = segments.pop_first() {
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

#[derive(Debug, PartialEq)]
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
    use crate::segments;
    use super::*;

    fn new_cmp(c: Seg, i: Seg, e: Seg) -> SegmentsCmp {
        SegmentsCmp { common: Segments::new(c), internal: Segments::new(i), external: Segments::new(e) }
    }

    fn build_segments() -> Vec<(Seg, Seg, SegmentsCmp)> {
        vec![
            (Seg(1, 2), Seg(3, 4), new_cmp(Seg(9, 0), Seg(1, 2), Seg(3, 4))),
            (Seg(1, 2), Seg(2, 3), new_cmp(Seg(2, 2), Seg(1, 1), Seg(3, 3))),
            (Seg(1, 3), Seg(2, 4), new_cmp(Seg(2, 3), Seg(1, 1), Seg(4, 4))),
            (Seg(1, 3), Seg(2, 3), new_cmp(Seg(2, 3), Seg(1, 1), Seg(9, 0))),
            (Seg(1, 4), Seg(2, 3), SegmentsCmp { common: Segments::new(Seg(2, 3)), internal: Segments(btreeset![Seg(1, 1), Seg(4, 4)]), external: Segments::empty() }),
            (Seg(1, 2), Seg(1, 3), new_cmp(Seg(1, 2), Seg(9, 0), Seg(3, 3))),
            (Seg(1, 2), Seg(1, 2), new_cmp(Seg(1, 2), Seg(9, 0), Seg(9, 0))),
            (Seg(1, 3), Seg(1, 2), new_cmp(Seg(1, 2), Seg(3, 3), Seg(9, 0))),
            (Seg(2, 3), Seg(1, 4), SegmentsCmp { common: Segments::new(Seg(2, 3)), internal: Segments::empty(), external: Segments(btreeset![Seg(1, 1), Seg(4, 4)]) }),
            (Seg(2, 3), Seg(1, 3), new_cmp(Seg(2, 3), Seg(9, 0), Seg(1, 1))),
            (Seg(2, 4), Seg(1, 3), new_cmp(Seg(2, 3), Seg(4, 4), Seg(1, 1))),
            (Seg(2, 3), Seg(1, 2), new_cmp(Seg(2, 2), Seg(3, 3), Seg(1, 1))),
            (Seg(3, 4), Seg(1, 2), new_cmp(Seg(9, 0), Seg(3, 4), Seg(1, 2))),
        ]
    }

    #[test]
    fn segs_segment_intersect() {
        let tests = build_segments();
        for (idx, (ab, cd, expected_cmp)) in tests.into_iter().enumerate() {
            let cmp = Segments::segment_intersect(ab, cd);
            assert_eq!(cmp, expected_cmp, "test {idx} failed");
        }
    }

    #[test]
    fn segs_intersect() {
        for scale in [10, 4] {
            let iv = build_segments();
            let mut ab = Segments::empty();
            let mut cd = Segments::empty();
            let mut expected_cmp = SegmentsCmp::empty();
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
            assert_eq!(cmp, SegmentsCmp { common: Segments::empty(), internal: ab.clone(), external: Segments::empty() }, "{}", msg);
            let cmp = Segments::empty().intersect(&ab);
            assert_eq!(cmp, SegmentsCmp { common: Segments::empty(), internal: Segments::empty(), external: ab.clone() }, "{}", msg);
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
    fn segs_intersect_corner() {
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
        const VERBOSE: bool = false;
        for (idx, (ab, cd, expected_cmp)) in tests.into_iter().enumerate() {
            let expected_cmp = SegmentsCmp { common: expected_cmp.0, internal: expected_cmp.1, external: expected_cmp.2 };
            let mut cmp = ab.intersect(&cd);
            if VERBOSE { println!("{ab:x} # {cd:x} = com: {:x}, int: {:x}, ext: {:x}", cmp.common, cmp.internal, cmp.external); }
            cmp.normalize();
            if VERBOSE { println!("  normalized: com: {:x}, int: {:x}, ext: {:x}", cmp.common, cmp.internal, cmp.external); }
            assert_eq!(cmp, expected_cmp, "test {idx} failed");
            let mut cmp = cd.intersect(&ab);
            cmp.normalize();
            assert_eq!(cmp, expected_cmp.inverse(), "test {idx} failed");
        }
    }

    #[test]
    fn segs_partition() {
        let tests: Vec<(Segments, Segments, Segments)> = vec![
            (segments![1-4], segments![3-6], segments![1-2, 3-4, 5-6]),
            (segments![1-4], segments![5-6], segments![1-4, 5-6]),
            (segments![1-6], segments![3-4], segments![1-2, 3-4, 5-6]),
            (segments![1-4, 5-10], segments![], segments![1-4, 5-10]),
            (segments![], segments![1-4, 5-10], segments![1-4, 5-10]),
            (segments![1-4, 5-10], segments![3-5], segments![1-2, 3-4, 5-5, 6-10]),
        ];
        for (idx, (mut ab, cd, exp)) in tests.into_iter().enumerate() {
            ab.add_partition(&cd);
            let expected = exp;
            assert_eq!(ab, expected, "test {idx} failed");
        }
    }

    #[test]
    fn segs_slice_partition() {
        let tests: Vec<(Segments, Segments, Segments)> = vec![
            (segments![1 - 50], segments![10 - 20, 30 - 40],
             segments![1-9, 10-20, 21-29, 30-40, 41-50]),
            (segments![10 - 20, 30 - 40], segments![1 - 50],
             segments![10-20, 30-40]),
            (segments![1-10, 11-15, 16-20, 21-35, 36-37, 38-50], segments![10-20, 30-40],
             segments![1-9, 10, 11-15, 16-20, 21-29, 30-35, 36-37, 38-40, 41-50]),
            (segments![0-9], segments![0-0, 1-9],
             segments![0, 1-9]),
            (segments![1-10, 30-40], segments![11-20, 25-29, 41-100],
             segments![1-10, 30-40]),
            (segments![], segments![],
             segments![]),
        ];
        const VERBOSE: bool = false;
        for (idx, (mut ab, cd, expected_part)) in tests.into_iter().enumerate() {
            if VERBOSE { print!("{ab:x} # {cd:x} => "); }
            ab.slice_partitions(&cd);
            if VERBOSE { println!("{ab:x}"); }
            assert_eq!(ab, expected_part, "test {idx} failed");
        }
    }

    #[test]
    fn segs_chars() {
        let tests = vec![
            (segments!['a'-'a'], "a"),
            (segments!['a'-'d'], "abcd"),
            (segments!['a'-'c', 'x'-'z'], "abcxyz"),
            (segments!['a'-'b', 'd'-'d', 'f'-'f', 'x'-'z'], "abdfxyz"),
        ];
        for (idx, (segments, expected)) in tests.into_iter().enumerate() {
            let result = segments.chars().collect::<String>();
            assert_eq!(result, expected, "test {idx} failed");
        }
    }

    #[test]
    fn segs_insert_utf8() {
        let tests = vec![
            (0, UTF8_MAX,                    segments![DOT]),
            (32, UTF8_GAP_MIN + 2,           segments![32-LOW_MAX]),
            (64, UTF8_GAP_MAX,               segments![64-LOW_MAX]),
            (96, UTF8_GAP_MAX + 1,           segments![96-LOW_MAX, HIGH_MIN]),
            (UTF8_GAP_MIN, UTF8_GAP_MAX,     segments![]),
            (UTF8_GAP_MIN, UTF8_GAP_MAX + 1, segments![HIGH_MIN]),
        ];
        for (test_id, (a, b, expected)) in tests.into_iter().enumerate() {
            let mut result = Segments::empty();
            result.insert_utf8(a, b);
            assert_eq!(result, expected, "test {test_id} failed");
        }
    }

    #[test]
    fn segs_not() {
        let tests = vec![
            (segments![DOT],                        segments![]),
            (segments![],                           segments![DOT]),
            (segments![0],                          segments![1-LOW_MAX, HIGH_MIN-MAX]),
            (segments![0-MAX],                      segments![]),
            (segments![1-0xd700],                   segments![0-0, 0xd701-LOW_MAX, HIGH_MIN-MAX]),
            (segments![2-0xd7fe],                   segments![0-1, 0xd7ff, HIGH_MIN-MAX]),
            (segments![3-LOW_MAX],                  segments![0-2, HIGH_MIN-MAX]),
            (segments![4-0xdfff],                   segments![0-3, HIGH_MIN-MAX]),
            (segments![5-HIGH_MIN],                 segments![0-4, 0xe001-MAX]),
            (segments![0-6, LOW_MAX-HIGH_MIN, MAX], segments![7-0xd7fe, 0xe001-0x10fffe]),
            (segments![0-7, GAP_MIN-GAP_MAX],       segments![8-LOW_MAX, HIGH_MIN-MAX]),
            (segments![0-8, GAP_MAX-0xe001],        segments![9-LOW_MAX, 0xe002-MAX]),
            (segments![0-9, HIGH_MIN-MAX],          segments![10-LOW_MAX]),
        ];
        for (test_id, (segments, expected)) in tests.into_iter().enumerate() {
            let result = segments.not();
            assert_eq!(result.normalized(), expected.normalized(), "test {test_id} failed");
        }
    }
}