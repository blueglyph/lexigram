#![allow(unused)]

// ---------------------------------------------------------------------------------------------

use crate::take_until::TakeMutUntilIterator;

pub struct RepeaterIter<T: Clone> {
    values: Vec<T>,
    cur: usize
}

pub trait Repeater<T>: Iterator<Item=T>
    where T: Clone, Self: Sized
{
    /// Takes an iterator and creates an infinite iterator that repeats the original
    /// sequence endlessly.
    ///
    /// Each output is a tuple `(T, bool)` where the flag indicates the last item
    /// of the original sequence when it's true.
    ///
    /// # Example
    /// ```#ignore
    /// let a = vec![1, 3, 4];
    /// let c = a.into_iter().repeat();
    /// let result = c.take(8).collect::<Vec<_>>();
    /// assert_eq!(result, [(1, false), (3, false), (4, true), (1, false),
    ///                     (3, false), (4, true), (1, false), (3, false)]);
    /// ```
    fn repeat(self) -> RepeaterIter<T> {
        RepeaterIter { values: self.collect::<Vec<_>>(), cur: 0 }
    }
}

impl<T: Clone> Iterator for RepeaterIter<T> {
    type Item = (T, bool);

    fn next(&mut self) -> Option<Self::Item> {
        if self.values.len() == 0 {
            None
        } else {
            if self.cur + 1 < self.values.len() {
                let value = Some((self.values[self.cur].clone(), false));
                self.cur += 1;
                value
            } else {
                let value = Some((self.values[self.cur].clone(), true));
                self.cur = 0;
                value
            }
        }
    }

    fn count(self) -> usize where Self: Sized {
        panic!("cannot count infinite iteration");
    }
}

impl<T: Clone> RepeaterIter<T> {
    pub fn cycle_len(&self) -> usize {
        self.values.len()
    }
}

impl<T: Clone, I: Iterator<Item=T>> Repeater<T> for I {}

// ---------------------------------------------------------------------------------------------

pub struct CProductIter<T: Clone> {
    tumblers: Vec<RepeaterIter<T>>,
    cur: Option<Vec<(T, bool)>>,
    len: usize
}

pub trait CProduct<T>
    where T: Clone,
          Self: Sized,
          Self: Iterator,
          Self::Item: IntoIterator<Item=T>,
{
    /// Takes an iterator of `IntoIterator` objects and outputs the cartesian product of their
    /// values. The last (rightmost) `IntoIterator` object is iterated through first. Once its
    /// last element has been output, it's reset and the previous one is taken to its next
    /// iteration, and so on.
    ///
    /// Objects that only have one iteration repeats the same value all the time. If any object
    /// has no iteration (e.g. an empty vector), the whole sequence is empty.
    ///
    /// # Example
    /// ```#ignore
    /// let source = vec![vec![3, 1], vec![0], vec![5, 6, 8]];
    /// let products = source.into_iter().cproduct().collect::<Vec<_>>();
    /// assert_eq!(products, vec![vec![3, 0, 5], vec![3, 0, 6], vec![3, 0, 8],
    ///                           vec![1, 0, 5], vec![1, 0, 6], vec![1, 0, 8]]);
    /// ```
    fn cproduct(self) -> CProductIter<T> {
        let mut tumblers = self.into_iter().map(|it| it.into_iter().repeat()).collect::<Vec<_>>();
        let n_tumblers = tumblers.len();
        let cur = tumblers.iter_mut().map(|t| t.next()).filter_map(|t| t).collect::<Vec<_>>();
        let len = tumblers.iter().map(|t| t.cycle_len()).product();
        CProductIter {
            tumblers,
            cur: if cur.len() < n_tumblers { None } else { Some(cur) },
            len
        }
    }
}

impl <T: Clone> Iterator for CProductIter<T> {
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cur) = self.cur.as_mut() {
            let val = Some(cur.iter().map(|(v, _)| v.clone()).collect());
            let mut last_carry = false;
            let count = cur.iter_mut().rev().zip(self.tumblers.iter_mut().rev())
                .take_mut_until(|((v, carry), t)| {
                    last_carry = *carry;
                    (*v, *carry) = t.next().unwrap();
                    !last_carry
                }).count();
            self.len -= 1;
            if count == self.tumblers.len() && last_carry {
                self.cur = None;
            }
            val
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    fn count(self) -> usize where Self: Sized {
        self.tumblers.iter().map(|t| t.cycle_len()).product()
    }
}

impl<T: Clone, I: Iterator> CProduct<T> for I
    where I::Item: IntoIterator<Item=T>
{}

// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::time;
    use super::*;
    use super::CProduct;

    #[test]
    fn repeat_basic() {
        let a = vec![1, 3, 4];
        let c = a.into_iter().repeat();
        let result = c.take(8).collect::<Vec<_>>();
        assert_eq!(result, [(1, false), (3, false), (4, true), (1, false), (3, false), (4, true), (1, false), (3, false)]);
    }

    #[test]
    fn repeat_empty() {
        let a = Vec::<u32>::new();
        let c = a.into_iter().repeat();
        let result = c.take(8).collect::<Vec<_>>();
        assert_eq!(result, []);
    }

    #[test]
    fn cproduct() {
        let ids = vec![vec![1, 2], vec![3, 4, 5], vec![8], vec![6, 7]];
        assert_eq!(ids.iter().cproduct().count(), 2 * 3 * 1 * 2);
        let result = ids.into_iter().cproduct().take(13).collect::<Vec<_>>();
        assert_eq!(result, vec![
            vec![1, 3, 8, 6],
            vec![1, 3, 8, 7],
            vec![1, 4, 8, 6],
            vec![1, 4, 8, 7],
            vec![1, 5, 8, 6],
            vec![1, 5, 8, 7],
            vec![2, 3, 8, 6],
            vec![2, 3, 8, 7],
            vec![2, 4, 8, 6],
            vec![2, 4, 8, 7],
            vec![2, 5, 8, 6],
            vec![2, 5, 8, 7],
        ]);
    }

    #[test]
    fn cproduct_empty() {
        let ids = vec![vec![1, 2], vec![3, 4, 5], vec![8], vec![]];
        assert_eq!(ids.iter().cproduct().count(), 2 * 3 * 1 * 0);
        let result = ids.into_iter().cproduct().take(13).collect::<Vec<_>>();
        assert_eq!(result, Vec::<Vec<i32>>::new());
    }

    #[test]
    fn cproduct_hint() {
        let ids = vec![vec![1, 2], vec![3, 4, 5], vec![8], vec![6, 7]];
        let mut it = ids.iter().cproduct();
        assert_eq!(it.size_hint(), (12, Some(12)));
        it.next();
        assert_eq!(it.size_hint(), (11, Some(11)));

        let ids = vec![vec![1, 2], vec![3, 4, 5], vec![8], vec![]];
        let mut it = ids.iter().cproduct();
        assert_eq!(it.size_hint(), (0, Some(0)));
        it.next();
        assert_eq!(it.size_hint(), (0, Some(0)));
    }
}
