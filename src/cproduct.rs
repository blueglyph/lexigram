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

// let ids = vec![vec![1, 2], vec![3, 4, 5], vec![8], vec![6, 7]];
// let result = ids.into_iter().cproduct().collect::<Vec<_>>();
//
pub trait CProduct<T>
    where T: Clone,
          Self: Sized,
          Self: Iterator,
          Self::Item: IntoIterator<Item=T>,
{
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

    /*
        fn next(&mut self) -> Option<Self::Item> {
            if let Some(cur) = self.cur.as_mut() {
                let val = Some(cur.iter().map(|(v, _)| v.clone()).collect());
                let mut it = cur.iter_mut().rev().zip(self.tumblers.iter_mut().rev());
                let mut last_carry = false;
                while let Some(((v, carry), t)) = it.next()  {
                    last_carry = *carry;
                    (*v, *carry) = t.next().unwrap();
                    if !last_carry {
                        break;
                    }
                }
                if it.next().is_none() && last_carry {
                    self.cur = None;
                }
                val
            } else {
                None
            }
        }
    */

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
