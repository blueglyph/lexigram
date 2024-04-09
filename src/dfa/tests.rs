#![cfg(test)]

use std::ops::Add;
use crate::*;
use crate::vectree::VecTree;
use crate::dfa::*;
#[allow(unused)] // the compiler doesn't see it's used in a macro
use crate::io::{UTF8_MAX, UTF8_MIN};

// ---------------------------------------------------------------------------------------------
// Macros

/// Generates an `ReNode` instance.
///
/// # Examples
/// ```
/// # use std::collections::BTreeSet;
/// # use rlexer::{dfa::*, node, io::{UTF8_MAX, UTF8_MIN}};
/// assert_eq!(node!(chr 'a'), ReNode::new(ReType::Char('a')));
/// assert_eq!(node!(['a','z'; '0','9']), ReType::CharRange(Box::new(Intervals(BTreeSet::from([('a' as u32, 'z' as u32), ('0' as u32, '9' as u32)])))));
/// assert_eq!(node!(.), ReNode::new(ReType::CharRange(Box::new(Intervals(BTreeSet::from([(UTF8_MIN, UTF8_MAX)]))))));
/// assert_eq!(node!(str "new"), ReNode::new(ReType::String(Box::new("new".to_string()))));
/// assert_eq!(node!(=5), ReNode::new(ReType::End(Box::new(Terminal { token: Some(Token(5)), channel: 0, push_mode: None, push_state: None, pop: false })));
/// assert_eq!(node!(&), ReNode::new(ReType::Concat));
/// assert_eq!(node!(|), ReNode::new(ReType::Or));
/// assert_eq!(node!(*), ReNode::new(ReType::Star));
/// assert_eq!(node!(+), ReNode::new(ReType::Plus));
/// assert_eq!(node!(-), ReNode::new(ReType::Empty));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! node {
    (chr $char:expr) => { ReNode::new(ReType::Char($char)) };
    (chr $char1:expr, $char2:expr $(;$char3:expr, $char4:expr)*) => { ($char1..=$char2)$(.chain($char3..=$char4))*.map(|c| ReNode::new(ReType::Char(c))) };
    ([$($char1:expr, $char2:expr);+]) => { ReNode::new(ReType::CharRange(Box::new(Intervals(btreeset![$( ($char1 as u32, $char2 as u32) ),+])))) };
    (.) => { node!([UTF8_MIN, UTF8_MAX]) };
    (str $str:expr) => { ReNode::new(ReType::String(Box::new($str.to_string()))) };
    (&) => { ReNode::new(ReType::Concat) };
    (|) => { ReNode::new(ReType::Or) };
    (*) => { ReNode::new(ReType::Star) };
    (+) => { ReNode::new(ReType::Plus) };
    (-) => { ReNode::new(ReType::Empty) };
    // actions:
    (= $id:expr) => { ReNode::new(ReType::End(Box::new(Terminal { token: Some(Token($id)), channel: 0, push_mode: None, push_state: None, pop: false })) ) };
    ($id:expr) => { ReNode::new(ReType::End(Box::new($id))) };
}

#[macro_export(local_inner_macros)]
macro_rules! term {
    (= $id:expr ) =>   { Terminal { token: Some(Token($id)), channel: 0, push_mode: None, push_state: None, pop: false } };
    (skip) =>          { Terminal { token: None, channel: 0, push_mode: None, push_state: None, pop: false } };
    (push $id:expr) => { Terminal { token: None, channel: 0, push_mode: Some($id), push_state: None, pop: false } };
    (pushst $id:expr) => { Terminal { token: None, channel: 0, push_mode: None, push_state: Some($id), pop: false } };
    (pop) =>           { Terminal { token: None, channel: 0, push_mode: None, push_state: None, pop: true } };
    (# $id:expr) =>    { Terminal { token: None, channel: $id, push_mode: None, push_state: None, pop: false } };
}

impl Add for Terminal {
    type Output = Terminal;

    fn add(self, rhs: Self) -> Self::Output {
        Terminal {
            token: if self.token.is_some() { self.token } else { rhs.token },
            channel: self.channel + rhs.channel,
            push_mode: if self.push_mode.is_some() { self.push_mode } else { rhs.push_mode },
            push_state: if self.push_state.is_some() { self.push_state } else { rhs.push_state },
            pop: self.pop || rhs.pop
        }
    }
}

/// Generates a tuple of u32 values from one or two values (characters or integers).
///
/// # Example
/// ```
/// #use rlexer::{btreeset, seg, intervals::Intervals};
/// let mut x = Intervals::empty();
/// x.insert(seg!('a'));
/// x.insert(seg!('0'-'9'));
/// assert_eq!(x, Intervals(btreeset![('a' as u32, 'a' as u32), ('0' as u32, '9' as u32)]));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! seg {
    ($a:literal - $b:literal) => { ($a as u32, $b as u32) };
    ($a:literal) => { ($a as u32, $a as u32) };
}

/// Generates an Intervals initialization from tuples of u32.
///
/// # Example
/// ```
/// #use rlexer::{btreeset, intervals, seg, intervals::Intervals};
/// let a = intervals!['a', 'b'-'z', '0'-'9'];
/// assert_eq!(a, Intervals(btreeset![('a' as u32, 'a' as u32), ('b' as u32, 'z' as u32), ('0' as u32, '9' as u32)]));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! intervals {
    ($($a:literal $(- $b:literal)?,)*) => { intervals![$($a$(- $b)?),*] };
    ($($a:literal $(- $b:literal)?),*) => { Intervals(BTreeSet::from([$(seg![$a$(- $b)?]),*]))};
}

/// Generates the key-value pairs corresponding to the `char => int` arguments, which can be
/// used to add values to `BTreeMap<char, StateId>` state transitions.
///
/// # Example
/// ```
/// # use std::collections::{BTreeMap, BTreeSet};
/// # use rlexer::{btreemap, intervals, branch, intervals::Intervals};
/// let transitions = btreemap![
///     0 => branch!['a'-'c' => 0, 'z' => 1, ['d'-'f', 'h'] => 2, ['x'-'z'] => 4],
///     1 => branch![['b'] => 3, '=' => 5],
///     3 => branch![]
/// ];
/// assert_eq!(transitions, BTreeMap::from([
///     (0, BTreeMap::from([
///         (Intervals(BTreeSet::from([('a' as u32, 'c' as u32)])), 0),
///         (Intervals(BTreeSet::from([('z' as u32, 'z' as u32)])), 1),
///         (Intervals(BTreeSet::from([('d' as u32, 'f' as u32), ('h' as u32, 'h' as u32)])), 2),
///         (Intervals(BTreeSet::from([('x' as u32, 'z' as u32)])), 4)])),
///     (1, BTreeMap::from([
///         (Intervals(BTreeSet::from([('b' as u32, 'b' as u32)])), 3),
///         (Intervals(BTreeSet::from([('=' as u32, '=' as u32)])), 5), ])),
///     (3, BTreeMap::new()), ]))
/// ```
#[macro_export(local_inner_macros)]
macro_rules! branch {
    ($( $([$($a:literal $(-$b:literal)?),+])? $($c:literal $(-$d:literal)?)? => $value:expr),*)
    => { btreemap![$(intervals![$($($a$(- $b)?,)+)? $($c $(-$d)?)?] => $value),*] };
    // a few guards for trailing comma:
    ($( $([$($a:literal $(-$b:literal)?),+])? $($c:literal $(-$d:literal)?)? => $value:expr,)+)
    => { btreemap![$(intervals![$($($a$(- $b)?,)+)? $($c $(-$d)?)?] => $value),+] };
    ($( $([$($a:literal $(-$b:literal)?,)+])? $($c:literal $(-$d:literal)?)? => $value:expr)*)
    => { btreemap![$(intervals![$($($a$(- $b)?,)+)? $($c $(-$d)?)?] => $value),*] };
    ($( $([$($a:literal $(-$b:literal)?,)+])? $($c:literal $(-$d:literal)?)? => $value:expr,)+)
    => { btreemap![$(intervals![$($($a$(- $b)?,)+)? $($c $(-$d)?)?] => $value),+] };
}

#[test]
fn macro_branch() {
    let transitions = btreemap![
        0 => branch!['a'-'c' => 0, 'z' => 1, ['d'-'f', 'h'] => 2, ['x'-'z'] => 4,],
        1 => branch![['b'] => 3, '=' => 5],
        3 => branch![]
    ];
    assert_eq!(transitions, BTreeMap::from([
        (0, BTreeMap::from([
            (Intervals(BTreeSet::from([('a' as u32, 'c' as u32)])), 0),
            (Intervals(BTreeSet::from([('z' as u32, 'z' as u32)])), 1),
            (Intervals(BTreeSet::from([('d' as u32, 'f' as u32), ('h' as u32, 'h' as u32)])), 2),
            (Intervals(BTreeSet::from([('x' as u32, 'z' as u32)])), 4)])),
        (1, BTreeMap::from([
            (Intervals(BTreeSet::from([('b' as u32, 'b' as u32)])), 3),
            (Intervals(BTreeSet::from([('=' as u32, '=' as u32)])), 5), ])),
        (3, BTreeMap::new()), ]))
}


// ---------------------------------------------------------------------------------------------
// Supporting functions

fn node_to_string(tree: &VecTree<ReNode>, index: usize, basic: bool) -> String {
    let node = tree.get(index);
    let mut result = String::new();
    if !basic {
        if node.nullable.is_none() {
            result.push('?');
        } else if node.nullable.unwrap() {
            result.push('!');
        }
    }
    result.push_str(&node.to_string());
    let children = tree.children(index);
    if !children.is_empty() {
        result.push_str("(");
        result.push_str(&children.iter().map(|&c| node_to_string(&tree, c, basic)).collect::<Vec<_>>().join(","));
        result.push_str(")");
    }
    result
}

#[allow(unused)]
pub(crate) fn term_to_string(t: &Terminal) -> String {
    let mut str = Vec::<String>::new();
    if let Some(token) = &t.token {
        str.push(format!("term!(={})", token.0));
    }
    if t.channel != 0 {
        str.push(format!("term!(#{})", t.channel));
    }
    if let Some(id) = t.push_mode {
        str.push(format!("term!(push {})", id));
    }
    if let Some(id) = t.push_state {
        str.push(format!("term!(pushst {})", id));
    }
    if t.pop {
        str.push("term!(pop)".to_string());
    }
    if str.is_empty() {
        "term!(skip)".to_string()
    } else {
        str.join(" + ")
    }
}

fn tree_to_string(tree: &VecTree<ReNode>, basic: bool) -> String {
    if tree.len() > 0 {
        node_to_string(tree, 0, basic)
    } else {
        "None".to_string()
    }
}

pub(crate) fn build_re(test: usize) -> VecTree<ReNode> {
    let mut re = VecTree::new();
    match test {
        0 => { // (a|b)*abb<end>
            let f = re.set_root(node!(&)).expect("expect empty tree");
            let e = re.add_iter(Some(f), [node!(&), node!(=0)])[0];
            let d = re.add_iter(Some(e), [node!(&), node!(chr 'b')])[0];
            let c = re.add_iter(Some(d), [node!(&), node!(chr 'b')])[0];
            let b = re.add_iter(Some(c), [node!(*), node!(chr 'a')])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        },
        1 => { // (a|b)*abb<end>
            let c = re.set_root(node!(&)).expect("expect empty tree");
            let b = re.add_iter(Some(c), [
                node!(*),
                node!(chr 'a'),
                node!(chr 'b'),
                node!(chr 'b'),
                node!(=0)
            ])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        },
        2 => { // (a|b)*abb<end>
            let c = re.set_root(node!(&)).expect("expect empty tree");
            let b = re.add_iter(Some(c), [
                node!(*),
                node!(str "abb"),
                node!(=0)
            ])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        }
        3 => { // abc(a|b)<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(str "abc"));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(=0));
        }
        4 => { // s(a|b)<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 's'));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(=0));
        }
        5 => {  // s(a<end>|b<end>)
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 's'));
            let a = re.add(Some(root), node!(|));
            let cd = re.add_iter(Some(a), [node!(&), node!(&)]);
            re.add_iter(Some(cd[0]), [node!(chr 'a'), node!(=0)]);
            re.add_iter(Some(cd[1]), [node!(chr 'b'), node!(=1)]);
        }
        6 => {  // a(bc)?d = a(bc|-)d<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let bc_opt = re.add(Some(root), node!(|));
            re.add_iter(Some(root), [node!(chr 'd'), node!(=0)]);
            re.add(Some(bc_opt), node!(str "bc"));
            re.add(Some(bc_opt), node!(-));
        },
        7 => {  // a(bc)?d?e = a(bc|-)(d|-)e<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let bc_opt = re.add(Some(root), node!(|));
            let d_opt = re.add(Some(root), node!(|));
            re.add_iter(Some(root), [node!(chr 'e'), node!(=0)]);
            re.add(Some(bc_opt), node!(str "bc"));
            re.add(Some(bc_opt), node!(-));
            re.add(Some(d_opt), node!(chr 'd'));
            re.add(Some(d_opt), node!(-));
        },
        8 => {  // a(<end>|b<end>)
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let b1 = re.add(Some(root), node!(|));
            re.add(Some(b1), node!(=0));
            let b2 = re.add(Some(b1), node!(&));
            re.add_iter(Some(b2), [node!(chr 'b'), node!(=1)]);
        },
        9 => {  // a(b|c)+d<end>
            let root = re.add(None, node!(&));
            re.add(Some(root), node!(chr 'a'));
            let plus = re.add(Some(root), node!(+));
            let or = re.add(Some(plus), node!(|));
            re.add_iter(Some(or), [node!(chr 'b'), node!(chr 'c')]);
            re.add(Some(root), node!(chr 'd'));
            re.add(Some(root), node!(=0));
        },
        10 => {
            // [ \t\n\r]*[0-9]+(<end:0>|\.[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
            // [------------------cc1-----------------]or1[-------cc3--------]
            //         s0 or3 p1      or4[----cc2-----]        or6    p3
            //                             or5 p2
            let or1 = re.add(None, node!(|));

            let cc1 = re.add(Some(or1), node!(&));
            let s0 = re.add(Some(cc1), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node![chr ' '], node![chr '\t'], node![chr '\n'], node![chr '\r']]);
            let plus1 = re.add(Some(cc1), node!(+));
            let or3 = re.add(Some(plus1), node!(|));
            re.add_iter(Some(or3), node![chr '0', '9']);
            let or4 = re.add(Some(cc1), node!(|));
            re.add(Some(or4), node!(=0));

            let cc2 = re.add(Some(or4), node!(&));
            re.add(Some(cc2), node!(chr '.'));
            let plus2 = re.add(Some(cc2), node!(+));
            let or5 = re.add(Some(plus2), node!(|));
            re.add_iter(Some(or5), node![chr '0','9']);
            re.add(Some(cc2), node!(=1));

            let cc3 = re.add(Some(or1), node!(&));
            re.add(Some(cc3), node!(chr '0'));
            re.add(Some(cc3), node!(chr 'x'));
            let plus3 = re.add(Some(cc3), node!(+));
            let or6 = re.add(Some(plus3), node!(|));
            re.add_iter(Some(or6), node![chr '0','9'; 'A','F'; 'a','f']);
            re.add(Some(cc3), node!(=2));
        },
        11 => {
            // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>)
            //       or1:                      ^         ^            ^        ^        ^
            //          cc--------------------- --------- ------------ -------- -------- --------
            //             or2    or3 star3
            let cc0 = re.add(None, node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node![chr ' '], node![chr '\t'], node![chr '\n'], node![chr '\r']]);
            let or1 = re.add(Some(cc0), node!(|));

            let cc = re.add(Some(or1), node!(&));
            let or2 = re.add(Some(cc), node!(|));
            re.add_iter(Some(or2), node!(chr 'a', 'z'));
            let star3 = re.add(Some(cc), node!(*));
            let or3 = re.add(Some(star3), node!(|));
            re.add_iter(Some(or3), node!(chr 'a', 'z'));
            re.add_iter(Some(or3), node!(chr '0', '9'));
            re.add(Some(cc), node!(=0));

            let cc = re.add(Some(or1), node!(&));
            re.add(Some(cc), node!(str "if"));
            re.add(Some(cc), node!(=1));

            let cc = re.add(Some(or1), node!(&));
            re.add(Some(cc), node!(str "print"));
            re.add(Some(cc), node!(=2));

            let cc = re.add(Some(or1), node!(&));
            re.add(Some(cc), node!(chr '='));
            re.add(Some(cc), node!(=3));

            let cc = re.add(Some(or1), node!(&));
            re.add(Some(cc), node!(chr '+'));
            re.add(Some(cc), node!(=4));

            let cc = re.add(Some(or1), node!(&));
            re.add(Some(cc), node!(chr ';'));
            re.add(Some(cc), node!(=5));
        },
        12 => {
            // (abs<end:0>|abi<end:1>|at<end:2>|ab<end:3>)
            let or = re.add(None, node!(|));
            let cc0 = re.add(Some(or), node!(&));
            re.add(Some(cc0), node!(str "abs"));
            re.add(Some(cc0), node!(=0));
            let cc1 = re.add(Some(or), node!(&));
            re.add(Some(cc1), node!(str "abi"));
            re.add(Some(cc1), node!(=1));
            let cc2 = re.add(Some(or), node!(&));
            re.add(Some(cc2), node!(str "at"));
            re.add(Some(cc2), node!(=2));
            let cc3 = re.add(Some(or), node!(&));
            re.add(Some(cc3), node!(str "ab"));
            re.add(Some(cc3), node!(=3));
        },
        13 => {
            // ([ \t\n\r]*<end:1,ch 1>|[0-9]+<end:0>)
            let or0 = re.add(None, node!(|));
            let cc0 = re.add(Some(or0), node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node![chr ' '], node![chr '\t'], node![chr '\n'], node![chr '\r']]);
            re.add_iter(Some(cc0), [node![term![#1] + term![=1]]]);

            let cc1 = re.add(Some(or0), node!(&));
            re.add_iter(Some(cc1), node![chr '0','9']);
            re.add(Some(cc1), node![=0]);
        },
        14 => {
            // \* /<end:0>|.+<end:1>
            let or = re.add(None, node!(|));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(=0)]);
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            re.add(Some(plus2), node!(chr '.'));
            re.add(Some(cc2), node!(=1));
        },
        15 => {
            // Ambiguous: [A-B]+<end:0>|[B-C]+<end:1>
            let or = re.add(None, node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            let or11 = re.add(Some(plus1), node!(|));
            re.add_iter(Some(or11), [node!(chr 'A'), node!(chr 'B')]);
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            let or21 = re.add(Some(plus2), node!(|));
            re.add_iter(Some(or21), [node!(chr 'B'), node!(chr 'C')]);
            re.add(Some(cc2), node!(=1));
        },
        16 => {
            // [A-B]+<end:0>|[B-C]+Z<end:1>
            let or = re.add(None, node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            let or11 = re.add(Some(plus1), node!(|));
            re.add_iter(Some(or11), [node!(chr 'A'), node!(chr 'B')]);
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            let or21 = re.add(Some(plus2), node!(|));
            re.add_iter(Some(or21), [node!(chr 'B'), node!(chr 'C')]);
            re.add(Some(cc2), node![chr 'Z']);
            re.add(Some(cc2), node!(=1));
        },
        17 => {
            // intervals: [a-f]+<end:0>|[d-i]+z<end:1>|ey<end:2>
            let or = re.add(None, node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            re.add(Some(plus1), node!(['a', 'f']));
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            re.add(Some(plus2), node!(['d', 'i']));
            re.add(Some(cc2), node![chr 'z']);
            re.add(Some(cc2), node!(=1));
            let cc3 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc3), [node!(chr 'e'), node!(chr 'y'), node!(=2)]);
        },
        18 => {
            // intervals: [a-f]+<end:0>|[d-i]+z<end:1>
            let or = re.add(None, node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            re.add(Some(plus1), node!(['a', 'f']));
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            re.add(Some(plus2), node!(['d', 'i']));
            re.add(Some(cc2), node![chr 'z']);
            re.add(Some(cc2), node!(=1));
        },
        _ => { }
    }
    re
}

pub(crate) fn build_dfa(test: usize) -> BTreeMap<ModeId, Dfa> {
    let mut re = VecTree::new();
    let modes: BTreeMap<ModeId, VecTree<ReNode>> = match test {
        1 => {
            // mode 0: ([ \t\n\r]*<skip>|/\*<push(1)><skip>|[0-9]+<end:0>)
            let or = re.add(None, node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node![chr ' '], node![chr '\t'], node![chr '\n'], node![chr '\r']]);
            re.add(Some(cc0), node![term![skip]]);
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node![chr '/'], node![chr '*'], node![term![push 1] + term![skip]]]);
            let cc2 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc2), node![chr '0','9']);
            re.add(Some(cc2), node![=0]);

            // mode 1: (\*/<pop>|[0-9]*<skip>)
            let mut re1 = VecTree::new();
            let or = re1.add(None, node!(|));
            let cc1 = re1.add(Some(or), node!(&));
            re1.add_iter(Some(cc1), [node![chr '/'], node![chr '*'], node!(term![pop])]);
            let cc2 = re1.add(Some(or), node!(&));
            let s2 = re1.add(Some(cc2), node![*]);
            let or2 = re1.add(Some(s2), node![|]);
            re1.add_iter(Some(or2), node![chr '0','9']);
            re1.add(Some(cc2), node![term![skip]]);

            btreemap![0 => re, 1 => re1]
        },
        2 => {
            // mode 0: ([ \t\n\r]*<skip>|/\*<push(1)>|[0-9]+<end:0>)
            // mode 1: (\*/<pop>|.*<skip>)

            btreemap![] // . not implemented yet
        },
        _ => btreemap![]
    };
    modes.into_iter()
        .map(|(n, re)| (n, DfaBuilder::new(re).build()))
        .collect()
}

#[allow(unused)]
fn debug_tree(tree: &VecTree<ReNode>) -> String {
    let mut result = String::new();
    let size = tree.len();
    for i in 0..size {
        let node = tree.get(i);
        result.push_str(&format!("[{i:3}] "));
        if let Some(id) = node.id {
            result.push_str(&format!("{id}:"));
        }
        if node.nullable.unwrap_or(false) {
            result.push('!');
        }
        result.push_str(&node.op.to_string());
        let children = tree.children(i).iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
        if children.len() > 0 {
            result.push_str(" -> [");
            result.push_str(&children);
            result.push(']');
        }
        let mut firstpos = node.firstpos.iter().collect::<Vec<_>>();
        firstpos.sort();
        let firstpos = firstpos.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
        if firstpos.len() > 0 {
            result.push_str(" fp:");
            result.push_str(&firstpos);
        }
        let mut lastpos = node.lastpos.iter().collect::<Vec<_>>();
        lastpos.sort();
        let lastpos = lastpos.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
        if lastpos.len() > 0 {
            result.push_str(" lp:");
            result.push_str(&lastpos);
        }
        result.push('\n');
    }
    result
}

#[allow(unused)]
pub(crate) fn print_dfa(dfa: &Dfa) {
    // println!("  graph:      {:?}", dfa.state_graph);
    println!("Graph:");
    print_graph(&dfa.state_graph, Some(&dfa.end_states));
    println!("End states: [{}]", dfa.end_states.iter().map(|(s, t)| format!("{} => {}", s, term_to_string(t))).collect::<Vec<_>>().join(", "));
}

pub(crate) fn print_graph(state_graph: &BTreeMap<StateId, BTreeMap<Intervals, StateId>>, end_states: Option<&BTreeMap<StateId, Terminal>>) {
    for (state, trans) in state_graph.clone() {
        println!("{:3} => branch!({}),{}",
                 state,
                 trans.iter().map(|(sym, st)| format!("{sym} => {st}"))
                     .collect::<Vec<_>>().join(", "),
                 end_states.and_then(|map| map.get(&state).map(|token| format!(" // {}", token))).unwrap_or("".to_string()),
        );
    }
}

// ---------------------------------------------------------------------------------------------
// Tests

#[test]
fn dfa_preprocess() {
    let tests = vec![
        ("?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end:0>)", "?&(?&(?&(?&(?*(?|(?'a',?'b')),?'a'),?'b'),?'b'),?<end:0>)"),
        ("?&(?*(?|(?'a',?'b')),?'a',?'b',?'b',?<end:0>)", "?&(?*(?|(?'a',?'b')),?'a',?'b',?'b',?<end:0>)"),
        ("?&(?*(?|(?'a',?'b')),?'abb',?<end:0>)", "?&(?*(?|(?'a',?'b')),?&(?'a',?'b',?'b'),?<end:0>)")
    ];
    for (test_id, (expected1, expected2)) in tests.into_iter().enumerate() {
        let re = build_re(test_id);
        let result1 = tree_to_string(&re, false);
        let dfa = DfaBuilder::new(re);
        let result2 = tree_to_string(&dfa.re, false);
        assert_eq!(result1, expected1, "test {test_id} failed (1st part)");
        assert_eq!(result2, expected2, "test {test_id} failed (2nd part)");
    }
}

#[test]
fn dfa_id() {
    let tests = vec![
        "&(&(&(&(*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end:0>)",
        "&(*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end:0>)",
        "&(*(|(1:'a',2:'b')),&(3:'a',4:'b',5:'b'),6:<end:0>)",
        "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end:0>)",
        "&(1:'s',|(2:'a',3:'b'),4:<end:0>)",
        "&(1:'s',|(&(2:'a',3:<end:0>),&(4:'b',5:<end:1>)))",
    ];
    for (test_id, expected) in tests.into_iter().enumerate() {
        let re = build_re(test_id);
        let mut dfa = DfaBuilder::new(re);
        dfa.calc_node_pos();
        assert_eq!(tree_to_string(&dfa.re, true), expected, "test {test_id} failed");
    }
}

#[test]
fn dfa_nullable() {
    let tests = vec![
        (0, "&(&(&(&(!*(|(1:'a',2:'b')),3:'a'),4:'b'),5:'b'),6:<end:0>)"),
        (1, "&(!*(|(1:'a',2:'b')),3:'a',4:'b',5:'b',6:<end:0>)"),
        (2, "&(!*(|(1:'a',2:'b')),&(3:'a',4:'b',5:'b'),6:<end:0>)"),
        (3, "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end:0>)"),
        (4, "&(1:'s',|(2:'a',3:'b'),4:<end:0>)"),
        (5, "&(1:'s',|(&(2:'a',3:<end:0>),&(4:'b',5:<end:1>)))"),
        (6, "&(1:'a',!|(&(2:'b',3:'c'),!4:-),5:'d',6:<end:0>)"),
        (7, "&(1:'a',!|(&(2:'b',3:'c'),!4:-),!|(5:'d',!6:-),7:'e',8:<end:0>)"),
        (8, "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"),
        (9, "&(1:'a',+(|(2:'b',3:'c')),4:'d',5:<end:0>)"),
        (10, "|(&(!*(|(1:' ',2:'\\t',3:'\\n',4:'\\r')),+(|(5:'0',6:'1',7:'2',8:'3',9:'4',10:'5',11:'6',12:'7',13:'8',14:'9')),|(15:<end:0>,&(16:'.',+(|(17:'0',18:'1',19:'2',20:'3',21:'4',22:'5',23:'6',24:'7',25:'8',26:'9')),27:<end:1>))),&(28:'0',29:'x',+(|(30:'0',31:'1',32:'2',33:'3',34:'4',35:'5',36:'6',37:'7',38:'8',39:'9',40:'A',41:'B',42:'C',43:'D',44:'E',45:'F',46:'a',47:'b',48:'c',49:'d',50:'e',51:'f')),52:<end:2>))"),
        (12, "|(&(&(1:'a',2:'b',3:'s'),4:<end:0>),&(&(5:'a',6:'b',7:'i'),8:<end:1>),&(&(9:'a',10:'t'),11:<end:2>),&(&(12:'a',13:'b'),14:<end:3>))"),
        (15, "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:<end:1>))"),
        (16, "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:'Z',7:<end:1>))"),
        (17, "|(&(+(1:'a'-'f'),2:<end:0>),&(+(3:'d'-'i'),4:'z',5:<end:1>),&(6:'e',7:'y',8:<end:2>))"),
    ];
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::new(re);
        dfa_builder.calc_node_pos();
        assert_eq!(tree_to_string(&dfa_builder.re, false), expected, "test {test_id} failed");
    }
}

#[test]
fn dfa_firstpos() {
    let tests = vec![
        (0, vec![
            vec![1], vec![2],   // a, b
            vec![1, 2],         // |
            vec![1, 2],         // *
            vec![3],            // a
            vec![1, 2, 3],      // &
            vec![4],            // b
            vec![1, 2, 3],      // &
            vec![5],            // b
            vec![1, 2, 3],      // &
            vec![6],            // <end>
            vec![1, 2, 3],      // &
        ]),
        (1, vec![
            vec![1], vec![2],                   // a, b
            vec![1, 2],                         // |
            vec![1, 2],                         // *
            vec![3], vec![4], vec![5], vec![6], // a, b, b, <end>
            vec![1, 2, 3],                      // &
        ]),
        (2, vec![
            vec![1], vec![2],                   // a, b
            vec![1, 2],                         // |
            vec![1, 2],                         // *
            vec![3], vec![4], vec![5],          // a, b, b
            vec![3],                            // &
            vec![6],                            // <end>
            vec![1, 2, 3],                      // &
        ]),
        (3, vec![
            vec![1], vec![2], vec![3],          // a, b, c
            vec![1],                            // &
            vec![4], vec![5],                   // a, b
            vec![4,5],                          // |
            vec![6],                            // <end>
            vec![1]                             // &
        ]),
        (4, vec![
            vec![1],                            // s
            vec![2], vec![3],                   // a, b
            vec![2, 3],                         // |
            vec![4],                            // <end>
            vec![1]                             // &
        ]),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
        (5, vec![
            vec![1],                            // s
            vec![2], vec![3],                   // a, <end>
            vec![2],                            // &
            vec![4], vec![5],                   // b, <end>
            vec![4],                            // &
            vec![2,4],                          // |
            vec![1]                             // &
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, vec![
            vec![1],        // a
            vec![2],        // b
            vec![3],        // c
            vec![2],        // &(b,c)
            vec![],         // -
            vec![2],        // |
            vec![5],        // d
            vec![6],        // <end>
            vec![1]         // &
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, vec![
            vec![1],        // a
            vec![2],        // b
            vec![3],        // c
            vec![2],        // &(b,c)
            vec![],         // -
            vec![2],        // |
            vec![5],        // d
            vec![],         // -
            vec![5],        // |
            vec![7],        // e
            vec![8],        // <end>
            vec![1]         // &
        ]),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, vec![
            vec![1],        // a
            vec![2],        // <end:0>
            vec![3],        // b
            vec![4],        // <end:1>
            vec![3],        // &(b,<1>)
            vec![2,3],      // |(<0>,&)
            vec![1]         // &
        ]),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, vec![
            vec![1],        // a
            vec![2],        // b
            vec![3],        // c
            vec![2,3],      // |(b,c)
            vec![2,3],      // +
            vec![4],        // d
            vec![5],        // <end>
            vec![1],        // &
        ]),
        // \*/<end:0>|.+<end:1>
        (14, vec![
            vec![1],
            vec![2],
            vec![3],
            vec![1],
            vec![4],
            vec![4],
            vec![5],
            vec![4],
            vec![1, 4]
        ]),
        // "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:<end:1>))"
        (15, vec![
            vec![1], vec![2], vec![1, 2], vec![1, 2],
            vec![3],
            vec![1, 2],
            vec![4], vec![5], vec![4, 5], vec![4, 5],
            vec![6],
            vec![4, 5],
            vec![1, 2, 4, 5],
        ]),
    ];
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::new(re);
        dfa_builder.calc_node_pos();
        let mut result = Vec::new();
        for inode in dfa_builder.re.iter_depth() {
            let mut firstpos = inode.firstpos.iter().map(|n| *n).collect::<Vec<_>>();
            firstpos.sort();
            result.push(firstpos)
        }
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

#[test]
fn dfa_lastpos() {
    let tests = vec![
        (0, vec![
            vec![1], vec![2],   // a, b
            vec![1, 2],         // |
            vec![1, 2],         // *
            vec![3],            // a
            vec![3],            // &
            vec![4],            // b
            vec![4],            // &
            vec![5],            // b
            vec![5],            // &
            vec![6],            // <end>
            vec![6],            // &
        ]),
        (1, vec![
            vec![1], vec![2],                   // a, b
            vec![1, 2],                         // |
            vec![1, 2],                         // *
            vec![3], vec![4], vec![5], vec![6], // a, b, b, <end>
            vec![6],                            // &
        ]),
        (2, vec![
            vec![1], vec![2],                   // a, b
            vec![1, 2],                         // |
            vec![1, 2],                         // *
            vec![3], vec![4], vec![5],          // a, b, b
            vec![5],                            // &
            vec![6],                            // <end>
            vec![6],                            // &
        ]),
        (3, vec![
            vec![1], vec![2], vec![3],          // a, b, c
            vec![3],                            // &
            vec![4], vec![5],                   // a, b
            vec![4,5],                          // |
            vec![6],                            // <end>
            vec![6]                             // &
        ]),
        (4, vec![
            vec![1],                            // s
            vec![2], vec![3],                   // a, b
            vec![2, 3],                         // |
            vec![4],                            // <end>
            vec![4]                             // &
        ]),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
        (5, vec![
            vec![1],                            // s
            vec![2], vec![3],                   // a, <end>
            vec![3],                            // &
            vec![4], vec![5],                   // b, <end>
            vec![5],                            // &
            vec![3,5],                          // |
            vec![3,5]                           // &
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, vec![
            vec![1],        // a
            vec![2],        // b
            vec![3],        // c
            vec![3],        // &(b,c)
            vec![],         // -
            vec![3],        // |
            vec![5],        // d
            vec![6],        // <end>
            vec![6]         // &
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, vec![
            vec![1],        // a
            vec![2],        // b
            vec![3],        // c
            vec![3],        // &(b,c)
            vec![],         // -
            vec![3],        // |
            vec![5],        // d
            vec![],         // -
            vec![5],        // |
            vec![7],        // e
            vec![8],        // <end>
            vec![8]         // &
        ]),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, vec![
            vec![1],        // a
            vec![2],        // <end:0>
            vec![3],        // b
            vec![4],        // <end:1>
            vec![4],        // &(b,<1>)
            vec![2,4],      // |(<0>,&)
            vec![2,4]       // &
        ]),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, vec![
            vec![1],        // a
            vec![2],        // b
            vec![3],        // c
            vec![2,3],      // |(b,c)
            vec![2,3],      // +
            vec![4],        // d
            vec![5],        // <end>
            vec![5],        // &
        ]),
        // \*/<end:0>|.+<end:1>
        (14, vec![
            vec![1],
            vec![2],
            vec![3],
            vec![3],
            vec![4],
            vec![4],
            vec![5],
            vec![5],
            vec![3, 5]
        ])
    ];
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::new(re);
        dfa_builder.calc_node_pos();
        let mut result = Vec::new();
        for inode in dfa_builder.re.iter_depth() {
            let mut lastpos = inode.lastpos.iter().map(|n| *n).collect::<Vec<_>>();
            lastpos.sort();
            result.push(lastpos)
        }
        assert_eq!(result, expected, "test {test_id} failed");
    }
}

#[test]
fn dfa_followpos() {
    let tests: Vec<(usize, BTreeMap<Id, BTreeSet<Id>>)> = vec!{
        (0, btreemap![
            1 => btreeset![1, 2, 3],
            2 => btreeset![1, 2, 3],
            3 => btreeset![4],
            4 => btreeset![5],
            5 => btreeset![6],
            6 => btreeset![]
        ]),
        (1, btreemap![
            1 => btreeset![1, 2, 3],
            2 => btreeset![1, 2, 3],
            3 => btreeset![4],
            4 => btreeset![5],
            5 => btreeset![6],
            6 => btreeset![]
        ]),
        (2, btreemap![
            1 => btreeset![1, 2, 3],
            2 => btreeset![1, 2, 3],
            3 => btreeset![4],
            4 => btreeset![5],
            5 => btreeset![6],
            6 => btreeset![]
        ]),
        // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'))"
        (3, btreemap![
            1 => btreeset![2],
            2 => btreeset![3],
            3 => btreeset![4, 5],
            4 => btreeset![6],
            5 => btreeset![6],
            6 => btreeset![],
        ]),
        (4, btreemap![
            1 => btreeset![2, 3],
            2 => btreeset![4],
            3 => btreeset![4],
            4 => btreeset![],
        ]),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
        (5, btreemap![
            1 => btreeset![2, 4],
            2 => btreeset![3],
            3 => btreeset![],
            4 => btreeset![5],
            5 => btreeset![],
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, btreemap![
            1 => btreeset![2, 5],
            2 => btreeset![3],
            3 => btreeset![5],
            5 => btreeset![6],
            6 => btreeset![]
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, btreemap![
            1 => btreeset![2, 5, 7],
            2 => btreeset![3],
            3 => btreeset![5, 7],
            5 => btreeset![7],
            7 => btreeset![8],
            8 => btreeset![]
        ]),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, btreemap![
            1 => btreeset![2, 3],
            2 => btreeset![],
            3 => btreeset![4],
            4 => btreeset![],
        ]),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, btreemap![
            1 => btreeset![2, 3],
            2 => btreeset![2, 3, 4],
            3 => btreeset![2, 3, 4],
            4 => btreeset![5],
            5 => btreeset![]
        ]),
        // "|(&(&(1:'a',2:'b',3:'s'),4:<end:0>),&(&(5:'a',6:'b',7:'i'),8:<end:1>),&(&(9:'a',10:'t'),11:<end:2>),&(&(12:'a',13:'b'),14:<end:3>))"
        (12, btreemap![
            1  => btreeset![2],
            2  => btreeset![3],
            3  => btreeset![4],
            4  => btreeset![],
            5  => btreeset![6],
            6  => btreeset![7],
            7  => btreeset![8],
            8  => btreeset![],
            9  => btreeset![10],
            10 => btreeset![11],
            11 => btreeset![],
            12 => btreeset![13],
            13 => btreeset![14],
            14 => btreeset![],
        ]),
        // \*/<end:0>|.+<end:1>
        (14, btreemap![
            1 => btreeset![2],
            2 => btreeset![3],
            3 => btreeset![],
            4 => btreeset![4, 5],
            5 => btreeset![],
        ]),
        // "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:<end:1>))"
        (15, btreemap![
            1 => btreeset![1, 2, 3],
            2 => btreeset![1, 2, 3],
            3 => btreeset![],
            4 => btreeset![4, 5, 6],
            5 => btreeset![4, 5, 6],
            6 => btreeset![]
        ]),
        // "|(&(+(1:['a'-'f']),2:<end:0>),&(+(3:['d'-'i']),4:'z',5:<end:1>))"
        (17, btreemap![
            1 => btreeset![1, 2],
            2 => btreeset![],
            3 => btreeset![3, 4],
            4 => btreeset![5],
            5 => btreeset![],
            6 => btreeset![7],
            7 => btreeset![8],
            8 => btreeset![]
        ]),
    };
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::new(re);
        dfa_builder.calc_node_pos();
        // to keep some things in order (easier for comparing):
        let res = BTreeMap::from_iter(dfa_builder.followpos.into_iter().map(|(s, st)| (s, BTreeSet::from_iter(st))));
        assert_eq!(res, expected, "test {test_id} failed");
    }
}

#[test]
fn dfa_states() {
    let tests = vec![
        (0, btreemap![
            0 => branch!('a' => 1, 'b' => 0),
            1 => branch!('a' => 1, 'b' => 2),
            2 => branch!('a' => 1, 'b' => 3),
            3 => branch!('a' => 1, 'b' => 0),
        ], btreemap![3 => term!(=0)]),
        (1, btreemap![
            0 => branch!('a' => 1, 'b' => 0),
            1 => branch!('a' => 1, 'b' => 2),
            2 => branch!('a' => 1, 'b' => 3),
            3 => branch!('a' => 1, 'b' => 0),
        ], btreemap![3 => term!(=0)]),
        (2, btreemap![
            0 => branch!('a' => 1, 'b' => 0),
            1 => branch!('a' => 1, 'b' => 2),
            2 => branch!('a' => 1, 'b' => 3),
            3 => branch!('a' => 1, 'b' => 0),
        ], btreemap![3 => term!(=0)]),
        // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
        (3, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2),
            2 => branch!('c' => 3),
            3 => branch!('a'-'b' => 4),
            4 => branch!()
        ], btreemap![4 => term!(=0)]),
        // "&(1:'s',|(2:'a',3:'b'),4:<end>)",
        (4, btreemap![
            0 => branch!('s' => 1),
            1 => branch!('a'-'b' => 2),
            2 => branch!(),
        ], btreemap![2 => term!(=0)]),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
        (5, btreemap![
            0 => branch!('s' => 1),
            1 => branch!('a' => 2, 'b' => 3),
            2 => branch!(),
            3 => branch!(),
        ], btreemap![2 => term!(=0), 3 => term!(=1)]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 'd' => 3),
            2 => branch!('c' => 4),
            3 => branch!(),
            4 => branch!('d' => 3)
        ], btreemap![3 => term!(=0)]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 'd' => 3, 'e' => 4),
            2 => branch!('c' => 5),
            3 => branch!('e' => 4),
            4 => branch!(),
            5 => branch!('d' => 3, 'e' => 4)
        ], btreemap![4 => term!(=0)]),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2),
            2 => branch!()
        ], btreemap![1 => term!(=0), 2 => term!(=1)]),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b'-'c' => 2),
            2 => branch!('b'-'c' => 2, 'd' => 3),
            3 => branch!()
        ], btreemap![3 => term!(=0)]),
        // [ \t\n\r]*[0-9]+(<end:0>|\.[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
        // "|(&(!*(|(1:' ',2:'\\t',3:'\\n',4:'\\r')),+(|(5:'0',6:'1',7:'2',8:'3',9:'4',10:'5',11:'6',12:'7',13:'8',14:'9')),|(15:<end:0>,&(16:'.',+(|(17:'0',18:'1',19:'2',20:'3',21:'4',22:'5',23:'6',24:'7',25:'8',26:'9')),27:<end:1>))),&(28:'0',29:'x',+(|(30:'0',31:'1',32:'2',33:'3',34:'4',35:'5',36:'6',37:'7',38:'8',39:'9',40:'A',41:'B',42:'C',43:'D',44:'E',45:'F',46:'a',47:'b',48:'c',49:'d',50:'e',51:'f')),52:<end:2>))"
        (10, btreemap![
            0 => branch!(['\t'-'\n', '\r', ' '] => 1, '0' => 2, '1'-'9' => 3),
            1 => branch!(['\t'-'\n', '\r', ' '] => 1, '0'-'9' => 3),
            2 => branch!('.' => 4, '0'-'9' => 3, 'x' => 5), // <end:0>
            3 => branch!('.' => 4, '0'-'9' => 3), // <end:0>
            4 => branch!('0'-'9' => 6),
            5 => branch!(['0'-'9', 'A'-'F', 'a'-'f'] => 7),
            6 => branch!('0'-'9' => 6), // <end:1>
            7 => branch!(['0'-'9', 'A'-'F', 'a'-'f'] => 7), // <end:2>
        ], btreemap![2 => term!(=0), 3 => term!(=0), 6 => term!(=1), 7 => term!(=2)]),
        (11, btreemap![
            0 => branch!(['\t'-'\n', '\r', ' '] => 0, '+' => 1, ';' => 2, '=' => 3, ['a'-'h', 'j'-'o', 'q'-'z'] => 4, 'i' => 5, 'p' => 6),
            1 => branch!(), // <end:4>
            2 => branch!(), // <end:5>
            3 => branch!(), // <end:3>
            4 => branch!(['0'-'9', 'a'-'z'] => 4), // <end:0>
            5 => branch!(['0'-'9', 'a'-'e', 'g'-'z'] => 4, 'f' => 7), // <end:0>
            6 => branch!(['0'-'9', 'a'-'q', 's'-'z'] => 4, 'r' => 8), // <end:0>
            7 => branch!(['0'-'9', 'a'-'z'] => 4), // <end:1>
            8 => branch!(['0'-'9', 'a'-'h', 'j'-'z'] => 4, 'i' => 9), // <end:0>
            9 => branch!(['0'-'9', 'a'-'m', 'o'-'z'] => 4, 'n' => 10), // <end:0>
            10 => branch!(['0'-'9', 'a'-'s', 'u'-'z'] => 4, 't' => 11), // <end:0>
            11 => branch!(['0'-'9', 'a'-'z'] => 4), // <end:2>
        ], btreemap![
            1 => term!(=4), 2 => term!(=5), 3 => term!(=3), 4 => term!(=0), 5 => term!(=0), 6 => term!(=0),
            7 => term!(=1), 8 => term!(=0), 9 => term!(=0), 10 => term!(=0), 11 => term!(=2)]),
        // (abs<end:0>|abi<end:1>|at<end:2>|ab<end:3>), to check if string paths are merged
        (12, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 't' => 3),
            2 => branch!('i' => 4, 's' => 5), // <end:3>
            3 => branch!(), // <end:2>
            4 => branch!(), // <end:1>
            5 => branch!(), // <end:0>
        ], btreemap![2 => term!(=3), 3 => term!(=2), 4 => term!(=1), 5 => term!(=0)]),
        // ([ \t\n\r]*{channel(1)}<end:1>|[0-9]+<end:0>)
        (13, btreemap![
            0 => branch![['\t'-'\n', '\r', ' '] => 0, '0' => 1],// <end:1,ch 1>
            1 => branch!('1' => 2),
            2 => branch!('2' => 3),
            3 => branch!('3' => 4),
            4 => branch!('4' => 5),
            5 => branch!('5' => 6),
            6 => branch!('6' => 7),
            7 => branch!('7' => 8),
            8 => branch!('8' => 9),
            9 => branch!('9' => 10),
            10 => branch!(),// <end:0>
        ], btreemap![0 => term!(=1) + term!(#1), 10 => term!(=0)]),
        // \* /<end:0>|.+<end:1>
        (14, btreemap![
            0 => branch!('*' => 1, '.' => 2),
            1 => branch!('/' => 3),
            2 => branch!('.' => 2),// <end:1>
            3 => branch!(),// <end:0>
        ], btreemap![
            2 => term!(=1), 3 => term!(=0)
        ]),
        // Ambiguous: [A-B]+<end:0>|[B-C]+<end:1>
        (15, btreemap![
            0 => branch!('A' => 1, 'B' => 2, 'C' => 3),
            1 => branch!('A'-'B' => 1),// <end:0>
            2 => branch!('A' => 1, 'B' => 2, 'C' => 3),// <end:1>
            3 => branch!('B'-'C' => 3),// <end:1>
        ], btreemap![1 => term!(=0), 2 => term!(=1), 3 => term!(=1)]),
        // [A-B]+<end:0>|[B-C]+Z<end:1>
        // "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:'Z',7:<end:1>))"
        (16, btreemap![
            0 => branch!('A' => 1, 'B' => 2, 'C' => 3),
            1 => branch!('A'-'B' => 1),// <end:0>
            2 => branch!('A' => 1, 'B' => 2, 'C' => 3, 'Z' => 4),// <end:0>
            3 => branch!('B'-'C' => 3, 'Z' => 4),
            4 => branch!(),// <end:1>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 4 => term!(=1)]),
        // intervals: [a-f]+<end:0>|[d-i]+z<end:1>|ey<end:2>
        // "|(&(+(1:['a'-'f']),2:<end:0>),&(+(3:['d'-'i']),4:'z',5:<end:1>),&(6:'e',7:'y',8:<end:2>))"
        (17, btreemap![
            0 => branch!('a'-'c' => 1, ['d', 'f'] => 2, 'e' => 3, 'g'-'i' => 4),
            1 => branch!('a'-'f' => 1), // <end:0>
            2 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 4, 'z' => 5), // <end:0>
            3 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 4, 'y' => 6, 'z' => 5), // <end:0>
            4 => branch!('d'-'i' => 4, 'z' => 5),
            5 => branch!(), // <end:1>
            6 => branch!(), // <end:2>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 3 => term!(=0), 5 => term!(=1), 6 => term!(=2)]),
        // [a-f]+<end:0>|[d-i]+z<end:1>
        // "|(&(+(1:['a'-'f']),2:<end:0>),&(+(3:['d'-'i']),4:'z',5:<end:1>))"
        (18, btreemap![
            0 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 3),
            1 => branch!('a'-'f' => 1),// <end:0>
            2 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 3, 'z' => 4),// <end:0>
            3 => branch!('d'-'i' => 3, 'z' => 4),
            4 => branch!(),// <end:1>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 4 => term!(=1)]),

    ];
    const VERBOSE: bool = false;
    for (test_id, expected, expected_ends) in tests {
        let re = build_re(test_id);
        if VERBOSE { println!("{test_id}:"); }
        let mut dfa_builder = DfaBuilder::new(re);
        let dfa = dfa_builder.build();
        if VERBOSE {
            print_dfa(&dfa);
            println!();
        }
        assert_eq!(dfa.state_graph, expected, "test {test_id} failed");
        assert_eq!(dfa.end_states, expected_ends, "test {test_id} failed");
    }
}

#[test]
fn dfa_normalize() {
    let mut test_id = 0;
    loop {
        let re = build_re(test_id);
        if re.len() == 0 {
            break;
        }
        let mut dfa = DfaBuilder::new(re).build();
        // println!("{test_id}: {}", if dfa.is_normalized() { "normalized" } else { "not normalized" });
        // print_dfa(&dfa);
        let _trans = dfa.normalize();
        // println!("{_trans:?}");
        // print_dfa(&dfa);
        assert!(dfa.is_normalized(), "test {test_id} failed");
        assert_eq!(dfa.first_end_state, Some(dfa.state_graph.len() - dfa.end_states.len()), "test {test_id} failed");
        // println!("-------------------------------------------------");
        test_id += 1;
    }
}

#[test]
fn dfa_modes() {
    let tests: Vec<(usize, BTreeMap<StateId, BTreeMap<Intervals, StateId>>, BTreeMap<StateId, Terminal>)> = vec![
        (1, btreemap![
            0 => branch!(['\t'-'\n', '\r', ' '] => 1, '/' => 2, '0' => 3), // <skip>
            1 => branch!(['\t'-'\n', '\r', ' '] => 1), // <skip>
            2 => branch!('*' => 4),
            3 => branch!('1' => 5),
            4 => branch!(), // <skip,push(mode 1,state 14)>
            5 => branch!('2' => 6),
            6 => branch!('3' => 7),
            7 => branch!('4' => 8),
            8 => branch!('5' => 9),
            9 => branch!('6' => 10),
            10 => branch!('7' => 11),
            11 => branch!('8' => 12),
            12 => branch!('9' => 13),
            13 => branch!(), // <end:0>
            14 => branch!('/' => 15, '0'-'9' => 16), // <skip>
            15 => branch!('*' => 17),
            16 => branch!('0'-'9' => 16), // <skip>
            17 => branch!(), // <skip,pop>
        ],
         btreemap![
             0 => term!(skip), 1 => term!(skip), 4 => term!(push 1) + term!(pushst 14),
             13 => term!(=0), 14 => term!(skip), 16 => term!(skip), 17 => term!(pop)]),
    ];

    const VERBOSE: bool = false;
    for (test_id, exp_graph, exp_ends) in tests {
        if VERBOSE { println!("{test_id}:"); }
        let dfas = build_dfa(test_id);
        for (id, dfa) in dfas.iter() {
            if VERBOSE {
                println!("## mode {id}");
                print_dfa(dfa);
            }
        }
        if VERBOSE { println!("## Merged:"); }
        let dfa = Dfa::from_dfa_modes(dfas);
        if VERBOSE {
            print_dfa(&dfa);
            println!("-------------------------------------------------");
        }
        assert_eq!(dfa.state_graph, exp_graph, "test {test_id} failed");
        assert_eq!(dfa.end_states, exp_ends, "test {test_id} failed");
    }
}

#[test]
fn dfa_optimize_graphs() {
    const VERBOSE: bool = false;
    let tests = vec![
        (0, btreemap![
            0 => branch!('a' => 1, 'b' => 0),
            1 => branch!('a' => 1, 'b' => 2),
            2 => branch!('a' => 1, 'b' => 3),
            3 => branch!('a' => 1, 'b' => 0),
        ], btreemap![3 => term!(=0)],
         btreemap![ // 1 <-> 2
             0 => branch!('a' => 2, 'b' => 0),
             1 => branch!('a' => 2, 'b' => 3),
             2 => branch!('a' => 2, 'b' => 1),
             3 => branch!('a' => 2, 'b' => 0),
         ], btreemap![3 => term!(=0)]),

        (1, btreemap![
            0 => branch!('a' => 1, 'b' => 2),
            1 => branch!('a' => 1, 'b' => 3),
            2 => branch!('a' => 1, 'b' => 2),
            3 => branch!('a' => 1, 'b' => 4),
            4 => branch!('a' => 1, 'b' => 2),
        ], btreemap![4 => term!(=0)],
         btreemap![ // 0 -> 0, 1 -> 2, 2 -> 0, 3 -> 1, 4 -> 3
            0 => branch!('a' => 2, 'b' => 0),
            1 => branch!('a' => 2, 'b' => 3),
            2 => branch!('a' => 2, 'b' => 1),
            3 => branch!('a' => 2, 'b' => 0),
         ], btreemap![3 => term!(=0)]),

        (8, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 3),
            3 => branch!()
        ], btreemap![1 => term!(=0), 3 => term!(=1)],
        btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2),
            2 => branch!(),
        ], btreemap![1 => term!(=0), 2 => term!(=1)]),

        (9, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 'c' => 2),
            2 => branch!('b' => 2, 'c' => 2, 'd' => 3),
            3 => branch!()
        ], btreemap![3 => term!(=0)],
        btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 'c' => 2),
            2 => branch!('b' => 2, 'c' => 2, 'd' => 3),
            3 => branch!()
        ], btreemap![3 => term!(=0)]),

        (10, btreemap![
            0 => branch!(
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 2, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3),
            1 => branch!(
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3),
            2 => branch!('.' => 5, '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3, 'x' => 6),// END: 0
            3 => branch!('.' => 5, '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3),// END: 0
            5 => branch!('0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7),
            6 => branch!(
                '0' => 8, '1' => 8, '2' => 8, '3' => 8, '4' => 8, '5' => 8, '6' => 8, '7' => 8, '8' => 8, '9' => 8,
                'A' => 8, 'B' => 8, 'C' => 8, 'D' => 8, 'E' => 8, 'F' => 8, 'a' => 8, 'b' => 8, 'c' => 8, 'd' => 8, 'e' => 8, 'f' => 8),
            7 => branch!('0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7),// END: 1
            8 => branch!(
                '0' => 8, '1' => 8, '2' => 8, '3' => 8, '4' => 8, '5' => 8, '6' => 8, '7' => 8, '8' => 8, '9' => 8,
                'A' => 8, 'B' => 8, 'C' => 8, 'D' => 8, 'E' => 8, 'F' => 8, 'a' => 8, 'b' => 8, 'c' => 8, 'd' => 8, 'e' => 8, 'f' => 8),// END: 2
        ], btreemap![2 => term!(=0), 3 => term!(=0), 7 => term!(=1), 8 => term!(=2)],
        btreemap![
            0 => branch!(
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 4, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5),
            1 => branch!(
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5),
            2 => branch!('0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6),
            3 => branch!(
                '0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7,
                'A' => 7, 'B' => 7, 'C' => 7, 'D' => 7, 'E' => 7, 'F' => 7, 'a' => 7, 'b' => 7, 'c' => 7, 'd' => 7, 'e' => 7, 'f' => 7),
            4 => branch!('.' => 2, '0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5, 'x' => 3),// END: 0
            5 => branch!('.' => 2, '0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5),// END: 0
            6 => branch!('0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6),// END: 1
            7 => branch!(
                '0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7,
                'A' => 7, 'B' => 7, 'C' => 7, 'D' => 7, 'E' => 7, 'F' => 7, 'a' => 7, 'b' => 7, 'c' => 7, 'd' => 7, 'e' => 7, 'f' => 7),// END: 2
        ], btreemap![4 => term!(=0), 5 => term!(=0), 6 => term!(=1), 7 => term!(=2)]),

        // (abs<end:0>|abi<end:1>|at<end:2>|ab<end:3>)
        (12, btreemap![], btreemap![], // from build_re(12)
        btreemap![ // no change
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 't' => 3),
            2 => branch!('i' => 5, 's' => 4),// <end:3>
            3 => branch!(),// <end:2>
            4 => branch!(),// <end:0>
            5 => branch!(),// <end:1>
        ], btreemap![2 => term!(=3), 3 => term!(=2), 4 => term!(=0), 5 => term!(=1)],
        ),

        (17, btreemap![], btreemap![],
         btreemap![
            0 => branch!('a' => 2, 'b' => 2, 'c' => 2, 'd' => 3, 'e' => 4, 'f' => 3, 'g' => 1, 'h' => 1, 'i' => 1),
            1 => branch!('d' => 1, 'e' => 1, 'f' => 1, 'g' => 1, 'h' => 1, 'i' => 1, 'z' => 5),
            2 => branch!('a' => 2, 'b' => 2, 'c' => 2, 'd' => 2, 'e' => 2, 'f' => 2), // <end:0>
            3 => branch!('a' => 2, 'b' => 2, 'c' => 2, 'd' => 3, 'e' => 3, 'f' => 3, 'g' => 1, 'h' => 1, 'i' => 1, 'z' => 5), // <end:0>
            4 => branch!('a' => 2, 'b' => 2, 'c' => 2, 'd' => 3, 'e' => 3, 'f' => 3, 'g' => 1, 'h' => 1, 'i' => 1, 'y' => 6, 'z' => 5), // <end:0>
            5 => branch!(), // <end:1>
            6 => branch!(), // <end:2>
        ], btreemap![2 => term!(=0), 3 => term!(=0), 4 => term!(=0), 5 => term!(=1), 6 => term!(=2)]),

    ];
    for (test_id, mut graph, mut end_states, exp_graph, exp_end_states) in tests {
        if VERBOSE { println!("{test_id}:"); }
        if graph.is_empty() {
            // fetches from the build_re
            let re = build_re(test_id);
            let mut dfa_builder = DfaBuilder::new(re);
            let dfa = dfa_builder.build();
            graph = dfa.state_graph;
            end_states = dfa.end_states;
        }
        let mut dfa = Dfa::from_graph(graph, 0, end_states);
        let _tr = dfa.optimize(true);
        if VERBOSE {
            println!("table: {}\n", _tr.iter().map(|(a, b)| format!("{a} -> {b}")).collect::<Vec<_>>().join(", "));
            print_dfa(&dfa);
        }
        assert_eq!(dfa.state_graph, exp_graph, "test {test_id} failed");
        assert_eq!(dfa.end_states, BTreeMap::from_iter(exp_end_states.into_iter()), "test {test_id} failed");
        assert_eq!(dfa.first_end_state, Some(dfa.state_graph.len() - dfa.end_states.len()), "test {test_id} failed");
    }
}
