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
}
