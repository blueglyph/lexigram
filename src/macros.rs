// Rust lacks the HashMap and HashSet equivalents of vec!

/// Generates the code to initialize a [HashMap](std::collections::HashMap).
///
/// The macro can be followed by parentheses or square brackets.
///
/// # Example
/// ```
/// # #[macro_use] fn main() {
/// # use std::collections::HashMap;
/// # use rlexer::hashmap;
/// let days = hashmap![0 => "Monday", 1 => "Tuesday", 2 => "Wednesday"];
/// // => HashMap::from([(0, "Monday"), (1, "Tuesday"), (2, "Wednesday"), ])
/// assert_eq!(days, HashMap::from([(0, "Monday"), (1, "Tuesday"), (2, "Wednesday")]));
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! hashmap {
    () => { HashMap::new() };
    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        HashMap::from([ $(($key, $value),)* ])
    };
}

/// Generates the code to initialize a [HashSet](std::collections::HashSet).
///
/// The macro can be followed by parentheses or square brackets.
///
/// # Example
/// ```
/// # #[macro_use] fn main() {
/// # use std::collections::HashSet;
/// # use rlexer::hashset;
/// let days = hashset!["Monday", "Tuesday", "Wednesday"];
/// // => HashSet::from(["Monday", "Tuesday", "Wednesday", ])
/// assert_eq!(days, HashSet::from(["Monday", "Tuesday", "Wednesday"]));
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! hashset {
    () => { HashSet::new() };
    ($($key:expr,)+) => { hashmap!($($key),+) };
    ($($key:expr),*) => { HashSet::from([ $($key,)* ]) };
}

mod tests {
    use std::collections::{HashMap, HashSet};
    use super::*;

    #[test]
    fn hashmap() {
        let h = hashmap!(
            'a' => hashmap!(
                '1' => 'a',
                '2' => 'A'
            ),
            'b' => hashmap!['1' => 'b', '2' => 'B',],
            'c' => hashmap!()
        );
        assert_eq!(h, HashMap::from([('a', HashMap::from([('1', 'a'), ('2', 'A')])), ('b', HashMap::from([('1', 'b'), ('2', 'B')])), ('c', HashMap::new())]))
    }

    #[test]
    fn hashset() {
        let h1 = hashset![1, 3, 5, 7];
        let h2 = hashset!();
        assert_eq!(h1, HashSet::from([1, 3, 5, 7]));
        assert_eq!(h2, HashSet::<i32>::from([]));
    }
}
