// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#[derive(Clone)]
pub struct TakeUntil<I, P> {
    iter: I,
    end: bool,
    predicate: P
}

impl<I, P> Iterator for TakeUntil<I, P>
    where I: Iterator,
          P: FnMut(&I::Item) -> bool
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            None
        } else {
            let item = self.iter.next();
            if let Some(item) = item {
                self.end = (self.predicate)(&item);
                Some(item)
            } else {
                self.end = true;
                None
            }
        }
    }
}

pub trait TakeUntilIterator<T, P>: Iterator<Item=T>
    where P: FnMut(&T) -> bool
{
    fn take_until(self, predicate: P) -> TakeUntil<Self, P> where Self: Sized {
        TakeUntil { iter: self, end: false, predicate }
    }
}

impl<T, I: Iterator<Item=T>, P: FnMut(&T) -> bool> TakeUntilIterator<T, P> for I {}

// ---------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct TakeMutUntil<I, P> {
    iter: I,
    end: bool,
    predicate: P
}

impl<I, P> Iterator for TakeMutUntil<I, P>
    where I: Iterator,
          P: FnMut(&mut I::Item) -> bool
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            None
        } else {
            let item = self.iter.next();
            if let Some(mut item) = item {
                self.end = (self.predicate)(&mut item);
                Some(item)
            } else {
                self.end = true;
                None
            }
        }
    }
}

pub trait TakeMutUntilIterator<T, P>: Iterator<Item=T>
    where P: FnMut(&mut T) -> bool
{
    fn take_mut_until(self, predicate: P) -> TakeMutUntil<Self, P> where Self: Sized {
        TakeMutUntil { iter: self, end: false, predicate }
    }
}

impl<T, I: Iterator<Item=T>, P: FnMut(&mut T) -> bool> TakeMutUntilIterator<T, P> for I {}

// ---------------------------------------------------------------------------------------------

#[derive(Clone)]
pub struct FlagFirstLast<I: Iterator> {
    iter: I,
    first: bool,
    next: Option<I::Item>,
}

impl<I: Iterator> Iterator for FlagFirstLast<I> {
    type Item = (bool, bool, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let (is_first, val) = if !self.first {
            (false, self.next.take()?)
        } else {
            self.first = false;
            (true, self.iter.next()?)
        };
        self.next = self.iter.next();
        let is_last = self.next.is_none();
        Some((is_first, is_last, val))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    fn count(self) -> usize {
        self.iter.count()
    }
}

pub trait FlagLastIterator<T>: Iterator<Item=T> {
    /// Creates an iterator that gives status flags with the iteration value.
    ///
    /// The status flags are:
    /// * `is_first`: is it the first iteration?
    /// * `is_last`: is it the last iteration?
    ///
    /// The iterator values are (`is_first`, `is_last`, `value`), where `value` is
    /// the value of the original iterator.
    ///
    /// ## Example
    /// ```ignored
    /// let values = vec![1, 2, 3];
    /// let result = values.into_iter().flag_first_last().collect::<Vec<_>>();
    /// assert_eq!(result, vec![(true, false, 1), (false, false, 2), (false, true, 3)]);
    /// ```
    fn flag_first_last(self) -> FlagFirstLast<Self> where Self: Sized {
        FlagFirstLast { iter: self, first: true, next: None }
    }
}

impl<T, I: Iterator<Item=T>> FlagLastIterator<T> for I {}

// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super:: *;

    #[test]
    fn adapter_take_until() {
        let v = vec![1, 2, 3, 4, 5];
        let result = v.iter().take_until(|&x| *x >= 4).map(|&n| n).collect::<Vec<_>>();
        assert_eq!(result, vec![1, 2, 3, 4]);
    }

    // For comparison:
    //      let v = vec![1, 2, 3, 4, 5];
    //      let result = v.iter().take_while(|&x| *x < 4).map(|&n| n).collect::<Vec<_>>();
    //      assert_eq!(result, vec![1, 2, 3]);

    #[test]
    fn predicate_take_until_empty() {
        let v: Vec<i32> = vec![];
        let result = v.iter().take_until(|&x| *x >= 4).map(|&n| n).collect::<Vec<_>>();
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn predicate_take_until_false() {
        let v = vec![1, 2, 3, 4, 5];
        let result = v.iter().take_until(|&x| *x > 10).map(|&n| n).collect::<Vec<_>>();
        assert_eq!(result, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn predicate_take_until_true() {
        let v = vec![1, 2, 3, 4, 5];
        let result = v.iter().take_until(|&x| *x < 10).map(|&n| n).collect::<Vec<_>>();
        assert_eq!(result, vec![1]);
    }

    #[test]
    fn adapter_take_mut_until() {
        let mut v: Vec<i32> = vec![1, 2, 3, 4, 5];
        let result = v.iter_mut().take_mut_until(|x| {
            **x = **x + 1;
            **x >= 4
        }).map(|x| *x).collect::<Vec<_>>();
        assert_eq!(result, vec![2, 3, 4]);
        assert_eq!(v, vec![2, 3, 4, 4, 5]);
    }

    struct Dummy<T> {
        values: Vec<Option<T>>
    }

    impl<T: Sized> Dummy<T> {
        fn new<I: IntoIterator<Item=Option<T>>>(values: I) -> Self {
            Dummy { values: values.into_iter().collect() }
        }
    }

    impl<T> Iterator for Dummy<T> {
        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            self.values.pop().unwrap_or(None)
        }
    }
    #[test]
    fn flag_first_last() {
        assert_eq!(vec![1, 2, 3].into_iter().flag_first_last().collect::<Vec<_>>(), vec![(true, false, 1), (false, false, 2), (false, true, 3)]);
        assert_eq!(vec![1].into_iter().flag_first_last().collect::<Vec<_>>(), vec![(true, true, 1)]);
        assert_eq!(vec![].into_iter().flag_first_last().collect::<Vec<(bool, bool, i32)>>(), vec![]);
        let dummy = Dummy::new([Some(6), Some(5), None, Some(3), Some(2), Some(1)]);
        let mut it = dummy.into_iter().flag_first_last();
        let result = (0..6).map(|_| it.next()).collect::<Vec<_>>();
        assert_eq!(result, vec![Some((true, false, 1)), Some((false, false, 2)), Some((false, true, 3)), None, None, None]);
    }
}
