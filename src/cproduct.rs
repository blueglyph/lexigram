// ---------------------------------------------------------------------------------------------

use crate::take_until::TakeMutUntilIterator;

pub struct RepeaterIter<I: Iterator> {
    iter: I,
    cur_iter: I
}

impl<I:Iterator + Clone> Iterator for RepeaterIter<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.cur_iter.next();
        if value.is_none() {
            self.cur_iter = self.iter.clone();
        }
        value
    }

    fn count(self) -> usize where Self: Sized {
        panic!("cannot count infinite iteration");
    }
}

impl<I: Iterator + Clone> RepeaterIter<I> {
    pub fn cycle_len(&self) -> usize {
        self.iter.clone().count()
    }
}

// ---------------------------------------------------------------------------------------------

pub struct CProductIter<I: Iterator + Clone>
    where I::Item: Clone
{
    tumblers: Vec<RepeaterIter<I>>,
    empty: bool,
    cur: Option<Vec< <<Self as Iterator>::Item as IntoIterator>::Item >>
}

impl <I: Iterator + Clone> Iterator for CProductIter<I>
    where I::Item: Clone
{
    type Item = Vec<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.empty {
            None
        } else {
            if let Some(cur) = self.cur.as_mut() {
                let mut last_carry = false;
                let count = cur.iter_mut().rev().zip(self.tumblers.iter_mut().rev())
                    .take_mut_until(|(v, t)| {
                        let new = t.next();
                        last_carry = new.is_none();
                        **v = if last_carry { t.next().unwrap() } else { new.unwrap() };
                        !last_carry
                    }).count();
                if last_carry && count == self.tumblers.len() {
                    self.empty = true;
                    return None;
                }
            } else {
                // 1st time
                self.cur = Some(self.tumblers.iter_mut().map(|t| t.next().unwrap()).collect::<Vec<_>>());
            }
            self.cur.clone()
        }
    }

    fn count(self) -> usize where Self: Sized {
        self.tumblers.iter().map(|t| t.cycle_len()).product()
    }
}

pub trait CProduct: Iterator {
    /// Takes an iterator and creates an infinite iterator that repeats the original
    /// sequence endlessly.
    ///
    /// # Example
    /// ```#ignore
    /// let a = vec![1, 3, 4];
    /// let mut c = a.into_iter().repeat();
    /// assert_eq!(c.cycle_len(), 3);
    /// let result = (0..8).map(|_| c.next()).collect::<Vec<_>>();
    /// assert_eq!(result, [Some(1), Some(3), Some(4), None, Some(1), Some(3), Some(4), None]);
    /// ```
    fn repeat(self) -> RepeaterIter<Self>
        where Self: Clone + Sized
    {
        RepeaterIter { iter: self.clone(), cur_iter: self.clone() }
    }

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
    fn cproduct(self) -> CProductIter<<<Self as Iterator>::Item as IntoIterator>::IntoIter>
        where Self: Sized,
              Self: Iterator,
              Self::Item: IntoIterator,
              <<Self as Iterator>::Item as IntoIterator>::IntoIter: Clone,
              <<Self as Iterator>::Item as IntoIterator>::Item: Clone
    {
        let tumblers = self.map(|it| it.into_iter().repeat()).collect::<Vec<_>>();
        let empty = tumblers.iter().any(|t| t.cycle_len() == 0);
        CProductIter {
            tumblers,
            empty,
            cur: None
        }
    }
}

impl<I: Iterator> CProduct for I {}

// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::time;
    use super::CProduct;

    #[test]
    fn cycle_basic() {
        let a = vec![1, 3, 4];
        let mut c = a.into_iter().repeat();
        assert_eq!(c.cycle_len(), 3);
        let result = (0..8).map(|_| c.next()).collect::<Vec<_>>();
        assert_eq!(result, [Some(1), Some(3), Some(4), None, Some(1), Some(3), Some(4), None]);
    }

    #[test]
    fn cycle_empty() {
        let a = Vec::<u32>::new();
        let mut c = a.into_iter().repeat();
        let result = (0..8).map(|_| c.next()).collect::<Vec<_>>();
        assert_eq!(result, [None, None, None, None, None, None, None, None]);
    }

    #[test]
    fn cproduct() {
        let ids = vec![vec![1, 2], vec![3, 4, 5], vec![8], vec![6, 7]];
        assert_eq!(ids.iter().cproduct().count(), 2 * 3 * 1 * 2);
        let result = ids.into_iter().cproduct()
            .take(13).collect::<Vec<_>>();
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

    #[ignore]
    #[test]
    fn cproduct_perf() {
        for _ in 0..4 {
            const LENGTH: usize = 8;
            const WIDTH: usize = 10;
            let ids = (0..LENGTH).map(|_| (0..WIDTH).collect::<Vec<_>>()).collect::<Vec<_>>();
            assert_eq!(ids.iter().cproduct().count(), WIDTH.pow(LENGTH as u32));
            let mut count = 0;
            time!(true, { ids.iter().cproduct().for_each(|x| {
                count += 1;
                std::hint::black_box(x);
            }); });
            assert_eq!(count, WIDTH.pow(LENGTH as u32));
        }
    }
}
