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

/// Generates the code to initialize a [BTreeMap](std::collections::BTreeMap).
///
/// The macro can be followed by parentheses or square brackets.
///
/// # Example
/// ```
/// # #[macro_use] fn main() {
/// # use std::collections::BTreeMap;
/// # use rlexer::btreemap;
/// let days = btreemap![0 => "Monday", 1 => "Tuesday", 2 => "Wednesday"];
/// // => BTreeMap::from([(0, "Monday"), (1, "Tuesday"), (2, "Wednesday"), ])
/// assert_eq!(days, BTreeMap::from([(0, "Monday"), (1, "Tuesday"), (2, "Wednesday")]));
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! btreemap {
    () => { std::collections::BTreeMap::new() };
    ($($key:expr => $value:expr,)+) => { btreemap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        std::collections::BTreeMap::from([ $(($key, $value),)* ])
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
    ($($key:expr,)+) => { hashset!($($key),+) };
    ($($key:expr),*) => { HashSet::from([ $($key,)* ]) };
}

/// Generates the code to initialize a [BTreeSet](std::collections::BTreeSet).
///
/// The macro can be followed by parentheses or square brackets.
///
/// # Example
/// ```
/// # #[macro_use] fn main() {
/// # use std::collections::BTreeSet;
/// # use rlexer::btreeset;
/// let days = btreeset!["Monday", "Tuesday", "Wednesday"];
/// // => BTreeSet::from(["Monday", "Tuesday", "Wednesday", ])
/// assert_eq!(days, BTreeSet::from(["Monday", "Tuesday", "Wednesday"]));
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! btreeset {
    () => { BTreeSet::new() };
    ($($key:expr,)+) => { btreeset!($($key),+) };
    ($($key:expr),*) => { BTreeSet::from([ $($key,)* ]) };
}

#[cfg(test)]
#[macro_export(local_inner_macros)]
macro_rules! time {
    ($verbose:expr, $p:block) => {
        let macro_timer = std::time::Instant::now();
        $p
        let elapsed = macro_timer.elapsed().as_secs_f64();
        if ($verbose) { std::println!("elapsed time: {elapsed:.3} s"); }
    }
}

/// Makes the string argument a String, by using `String::from()` (see [String] for details).
///
/// # Example
/// ```
/// # #[macro_use] fn main() {
/// # use rlexer::s;
/// let text: String = s!("Hello");
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! s {
    ($arg:expr) => {{ String::from($arg) }}
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, BTreeMap, HashSet, BTreeSet};

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
    fn btreemap() {
        let h = btreemap!(
            'a' => btreemap!(
                '1' => 'a',
                '2' => 'A'
            ),
            'b' => btreemap!['1' => 'b', '2' => 'B',],
            'c' => btreemap!()
        );
        assert_eq!(h, BTreeMap::from([('a', BTreeMap::from([('1', 'a'), ('2', 'A')])), ('b', BTreeMap::from([('1', 'b'), ('2', 'B')])), ('c', BTreeMap::new())]))
    }

    #[test]
    fn hashset() {
        let h1 = hashset![1, 3, 5, 7];
        let h2 = hashset!();
        assert_eq!(h1, HashSet::from([1, 3, 5, 7]));
        assert_eq!(h2, HashSet::<i32>::from([]));
    }

    #[test]
    fn btreeset() {
        let h1 = btreeset![1, 3, 5, 7];
        let h2 = btreeset!();
        assert_eq!(h1, BTreeSet::from([1, 3, 5, 7]));
        assert_eq!(h2, BTreeSet::<i32>::from([]));
    }
}
