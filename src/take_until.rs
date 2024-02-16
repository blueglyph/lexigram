use std::fmt::Display;

#[derive(Clone)]
pub struct TakeUntil<I, P> {
    iter: I,
    end: bool,
    predicate: P
}

impl<I, P> TakeUntil<I, P>
    where I: Iterator,
          P: FnMut(&I::Item) -> bool
{
    fn new(iter: I, predicate: P) -> Self {
        TakeUntil { iter, end: false, predicate }
    }
}

impl<I, P> Iterator for TakeUntil<I, P>
    where I: Iterator,
          P: FnMut(&I::Item) -> bool,
          I::Item: Display
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
        TakeUntil::new(self, predicate)
    }
}

impl<T, I: Iterator<Item=T>, P: FnMut(&T) -> bool> TakeUntilIterator<T, P> for I {}

// ---------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::iter::TakeWhile;
    use super:: *;

    #[test]
    fn predicate_take_until_normal() {
        let v = vec![1, 2, 3, 4, 5];
        let iter = v.iter();
        let mut iter_until = TakeUntil::new(iter, |&x| *x >= 4 );
        let mut result: Vec<i32> = Vec::new();
        while let Some(value) = iter_until.next() {
            result.push(*value);
        }
        assert_eq!(result, vec![1, 2, 3, 4]);
    }

    #[test]
    fn predicate_take_until_empty() {
        let v = vec![];
        let iter = v.iter();
        let mut iter_until = TakeUntil::new(iter, |&x| *x >= 4 );
        let mut result: Vec<i32> = Vec::new();
        while let Some(value) = iter_until.next() {
            result.push(*value);
        }
        assert_eq!(result, vec![]);
    }

    #[test]
    fn predicate_take_until_false() {
        let v = vec![1, 2, 3, 4, 5];
        let iter = v.iter();
        let mut iter_until = TakeUntil::new(iter, |&x| *x > 10 );
        let mut result: Vec<i32> = Vec::new();
        while let Some(value) = iter_until.next() {
            result.push(*value);
        }
        assert_eq!(result, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn predicate_take_until_true() {
        let v = vec![1, 2, 3, 4, 5];
        let iter = v.iter();
        let mut iter_until = TakeUntil::new(iter, |&x| *x < 10 );
        let mut result: Vec<i32> = Vec::new();
        while let Some(value) = iter_until.next() {
            result.push(*value);
        }
        assert_eq!(result, vec![1]);
    }

    #[test]
    fn adapter_take_until() {
        let v = vec![1, 2, 3, 4, 5];
        let result = v.iter().take_until(|&x| *x >= 4).collect::<Vec<_>>();
        assert_eq!(result, vec![&1, &2, &3, &4]);
   }

    #[test]
    fn adapter_take_while() {
        let v = vec![1, 2, 3, 4, 5];
        let result = v.iter().take_while(|&x| *x < 4).collect::<Vec<_>>();
        assert_eq!(result, vec![&1, &2, &3]);
   }

}
