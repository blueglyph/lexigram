#![cfg(test)]

use std::ops::Add;
use crate::*;
use crate::vectree::VecTree;
use crate::dfa::*;

// ---------------------------------------------------------------------------------------------
// Macros

/// Generates an `ReNode` instance.
///
/// # Examples
/// ```
/// # use rlexer::{dfa::*, node};
/// assert_eq!(node!(chr 'a'), ReNode::new(ReType::Char('a')));
/// assert_eq!(node!(str "new"), ReNode::new(ReType::String("new".to_string())));
/// assert_eq!(node!(= 5), ReNode::new(ReType::End(Terminal { token: Some(Token(5)), channel: 0, push_mode: None, pop: false })));
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
    (str $str:expr) => { ReNode::new(ReType::String(Box::new($str.to_string()))) };
    (= $id:expr) => { ReNode::new(ReType::End(Box::new(Terminal { token: Some(Token($id)), channel: 0, push_mode: None, push_state: None, pop: false })) ) };
    (&) => { ReNode::new(ReType::Concat) };
    (|) => { ReNode::new(ReType::Or) };
    (*) => { ReNode::new(ReType::Star) };
    (+) => { ReNode::new(ReType::Plus) };
    (-) => { ReNode::new(ReType::Empty) };
    // actions:
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

/// Generates the key-value pairs corresponding to the `char => int` arguments, which can be
/// used to add values to `BTreeMap<char, StateId>` state transitions.
///
/// # Example
/// ```
/// # use rlexer::{btreemap, branch};
/// let transitions = btreemap![
///     0 => branch!['a' => 1, 'b' => 1],
///     1 => branch!['b' => 3],
///     3 => branch![]
/// ];
/// // => BTreeMap::from([
/// //     (0, BTreeMap::from([('a', 1), ('b', 1), ])),
/// //     (1, BTreeMap::from([('b', 3), ])),
/// //     (3, BTreeMap::new()),
/// // ])
/// ```
#[macro_export(local_inner_macros)]
macro_rules! branch {
    ($($key:expr => $value:expr,)+) => { branch![$($key => $value),+] };
    ($($key:expr => $value:expr),*) => { btreemap![$($key => $value),*] };
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
pub(crate) fn print_graph(dfa: &Dfa) {
    // println!("  graph:      {:?}", dfa.state_graph);
    println!("  end states: {}", dfa.end_states.iter().map(|(s, t)| format!("{} => {}", s, term_to_string(t))).collect::<Vec<_>>().join(", "));
    println!();
    for (state, trans) in dfa.state_graph.clone() {
        // println!("s{state}{}", if dfa.end_states.contains(&state) { " <END>" } else { "" });
        // for (symbol, dest) in trans {
        //     println!("  {symbol} -> s{dest}");
        // }
        println!("{:3} => branch![{}],{}",
                 state,
                 trans.iter().map(|(sym, st)| format!("'{}' => {}", escape_char(*sym), st))
                     .collect::<Vec<_>>().join(", "),
                 dfa.end_states.get(&state).map(|token| format!("// {}", token)).unwrap_or("".to_string()),
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
    let tests = vec!{
        (0, hashmap![
            1 => hashset![1, 2, 3],
            2 => hashset![1, 2, 3],
            3 => hashset![4],
            4 => hashset![5],
            5 => hashset![6],
            6 => hashset![]
        ]),
        (1, hashmap![
            1 => hashset![1, 2, 3],
            2 => hashset![1, 2, 3],
            3 => hashset![4],
            4 => hashset![5],
            5 => hashset![6],
            6 => hashset![]
        ]),
        (2, hashmap![
            1 => hashset![1, 2, 3],
            2 => hashset![1, 2, 3],
            3 => hashset![4],
            4 => hashset![5],
            5 => hashset![6],
            6 => hashset![]
        ]),
        // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'))"
        (3, hashmap![
            1 => hashset![2],
            2 => hashset![3],
            3 => hashset![4, 5],
            4 => hashset![6],
            5 => hashset![6],
            6 => hashset![],
        ]),
        (4, hashmap![
            1 => hashset![2, 3],
            2 => hashset![4],
            3 => hashset![4],
            4 => hashset![],
        ]),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))"
        (5, hashmap![
            1 => hashset![2, 4],
            2 => hashset![3],
            3 => hashset![],
            4 => hashset![5],
            5 => hashset![],
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, hashmap![
            1 => hashset![2, 5],
            2 => hashset![3],
            3 => hashset![5],
            5 => hashset![6],
            6 => hashset![]
        ]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, hashmap![
            1 => hashset![2, 5, 7],
            2 => hashset![3],
            3 => hashset![5, 7],
            5 => hashset![7],
            7 => hashset![8],
            8 => hashset![]
        ]),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, hashmap![
            1 => hashset![2, 3],
            2 => hashset![],
            3 => hashset![4],
            4 => hashset![],
        ]),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, hashmap![
            1 => hashset![2, 3],
            2 => hashset![2, 3, 4],
            3 => hashset![2, 3, 4],
            4 => hashset![5],
            5 => hashset![]
        ]),
        // "|(&(&(1:'a',2:'b',3:'s'),4:<end:0>),&(&(5:'a',6:'b',7:'i'),8:<end:1>),&(&(9:'a',10:'t'),11:<end:2>),&(&(12:'a',13:'b'),14:<end:3>))"
        (12, hashmap![
            1  => hashset![2],
            2  => hashset![3],
            3  => hashset![4],
            4  => hashset![],
            5  => hashset![6],
            6  => hashset![7],
            7  => hashset![8],
            8  => hashset![],
            9  => hashset![10],
            10 => hashset![11],
            11 => hashset![],
            12 => hashset![13],
            13 => hashset![14],
            14 => hashset![],
        ])
    };
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::new(re);
        dfa_builder.calc_node_pos();
        // to keep some things in order (easier for comparing):
        let res = BTreeMap::from_iter(dfa_builder.followpos);
        let exp = BTreeMap::from_iter(expected);
        assert_eq!(res, exp, "test {test_id} failed");
    }
}

#[test]
fn dfa_states() {
    const VERBOSE: bool = false;
    let tests = vec![
        (0, btreemap![
            0 => branch!['a' => 1, 'b' => 0],
            1 => branch!['a' => 1, 'b' => 2],
            2 => branch!['a' => 1, 'b' => 3],
            3 => branch!['a' => 1, 'b' => 0],
        ], btreemap![3 => term![=0]]),
        (1, btreemap![
            0 => branch!['a' => 1, 'b' => 0],
            1 => branch!['a' => 1, 'b' => 2],
            2 => branch!['a' => 1, 'b' => 3],
            3 => branch!['a' => 1, 'b' => 0],
        ], btreemap![3 => term![=0]]),
        (2, btreemap![
            0 => branch!['a' => 1, 'b' => 0],
            1 => branch!['a' => 1, 'b' => 2],
            2 => branch!['a' => 1, 'b' => 3],
            3 => branch!['a' => 1, 'b' => 0],
        ], btreemap![3 => term![=0]]),
        // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
        (3, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2],
            2 => branch!['c' => 3],
            3 => branch!['a' => 4, 'b' => 4],
            4 => branch![]
        ], btreemap![4 => term![=0]]),
        // "&(1:'s',|(2:'a',3:'b'),4:<end>)",
        (4, btreemap![
            0 => branch!['s' => 1],
            1 => branch!['a' => 2, 'b' => 2],
            2 => branch![],
        ], btreemap![2 => term![=0]]),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
        (5, btreemap![
            0 => branch!['s' => 1],
            1 => branch!['a' => 2, 'b' => 3],
            2 => branch![],
            3 => branch![],
        ], btreemap![2 => term![=0], 3 => term![=1]]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 'd' => 3],
            2 => branch!['c' => 4],
            3 => branch![],
            4 => branch!['d' => 3]
        ], btreemap![3 => term![=0]]),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 'd' => 3, 'e' => 4],
            2 => branch!['c' => 5],
            3 => branch!['e' => 4],
            4 => branch![],
            5 => branch!['d' => 3, 'e' => 4]
        ], btreemap![4 => term![=0]]),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 3],
            3 => branch![]
        ], btreemap![1 => term![=0], 3 => term![=1]]),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 'c' => 2],
            2 => branch!['b' => 2, 'c' => 2, 'd' => 3],
            3 => branch![]
        ], btreemap![3 => term![=0]]),
        (10, btreemap![
            0 => branch![
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 2, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3],
            1 => branch![
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3],
            2 => branch!['.' => 5, '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3, 'x' => 6],// END: 0
            3 => branch!['.' => 5, '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3],// END: 0
            5 => branch!['0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7],
            6 => branch![
                '0' => 8, '1' => 8, '2' => 8, '3' => 8, '4' => 8, '5' => 8, '6' => 8, '7' => 8, '8' => 8, '9' => 8,
                'A' => 8, 'B' => 8, 'C' => 8, 'D' => 8, 'E' => 8, 'F' => 8, 'a' => 8, 'b' => 8, 'c' => 8, 'd' => 8, 'e' => 8, 'f' => 8],
            7 => branch!['0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7],// END: 1
            8 => branch![
                '0' => 8, '1' => 8, '2' => 8, '3' => 8, '4' => 8, '5' => 8, '6' => 8, '7' => 8, '8' => 8, '9' => 8,
                'A' => 8, 'B' => 8, 'C' => 8, 'D' => 8, 'E' => 8, 'F' => 8, 'a' => 8, 'b' => 8, 'c' => 8, 'd' => 8, 'e' => 8, 'f' => 8],// END: 2
        ], btreemap![2 => term![=0], 3 => term![=0], 7 => term![=1], 8 => term![=2]]),
        (11, btreemap![
            0 => branch![
                '\t' => 0, '\n' => 0, '\r' => 0, ' ' => 0,
                '+' => 1, ';' => 2, '=' => 3,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 5, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 6, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],
            1 => branch![],// END: 4
            2 => branch![],// END: 5
            3 => branch![],// END: 3
            4 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 0
            5 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 8, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 0
            6 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 9, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 0
            8 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 1
            9 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 10, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 0
            10 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 11, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 0
            11 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 12, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 0
            12 => branch![
                '0' => 4, '1' => 4, '2' => 4, '3' => 4, '4' => 4, '5' => 4, '6' => 4, '7' => 4, '8' => 4, '9' => 4,
                'a' => 4, 'b' => 4, 'c' => 4, 'd' => 4, 'e' => 4, 'f' => 4, 'g' => 4, 'h' => 4, 'i' => 4, 'j' => 4, 'k' => 4, 'l' => 4, 'm' => 4,
                'n' => 4, 'o' => 4, 'p' => 4, 'q' => 4, 'r' => 4, 's' => 4, 't' => 4, 'u' => 4, 'v' => 4, 'w' => 4, 'x' => 4, 'y' => 4, 'z' => 4],// END: 2
        ], btreemap![
            1 => term![=4], 2 => term![=5], 3 => term![=3], 4 => term![=0], 5 => term![=0], 6 => term![=0],
            8 => term![=1], 9 => term![=0], 10 => term![=0], 11 => term![=0], 12 => term![=2]]),
        // (abs<end:0>|abi<end:1>|at<end:2>|ab<end:3>), to check if string paths are merged
        (12, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 't' => 3],
            2 => branch!['i' => 5, 's' => 6],// <end:3>
            3 => branch![],// <end:2>
            5 => branch![],// <end:1>
            6 => branch![],// <end:0>
        ], btreemap![2 => term![=3], 3 => term![=2], 5 => term![=1], 6 => term![=0]]),
        // ([ \t\n\r]*{channel(1)}<end:1>|[0-9]+<end:0>)
        (13, btreemap![
            0 => branch!['\t' => 0, '\n' => 0, '\r' => 0, ' ' => 0, '0' => 2],// <end:1,ch 1>
            2 => branch!['1' => 3],
            3 => branch!['2' => 4],
            4 => branch!['3' => 5],
            5 => branch!['4' => 6],
            6 => branch!['5' => 7],
            7 => branch!['6' => 8],
            8 => branch!['7' => 9],
            9 => branch!['8' => 10],
            10 => branch!['9' => 11],
            11 => branch![],// <end:0>
        ], btreemap![0 => term!(=1) + term!(#1), 11 => term!(=0)]),
    ];
    for (test_id, expected, expected_ends) in tests {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::new(re);
        let dfa = dfa_builder.build();
        if VERBOSE {
            println!("{test_id}:");
            print_graph(&dfa);
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
        // print_graph(&dfa);
        let _trans = dfa.normalize();
        // println!("{_trans:?}");
        // print_graph(&dfa);
        assert!(dfa.is_normalized(), "test {test_id} failed");
        assert_eq!(dfa.first_end_state, Some(dfa.state_graph.len() - dfa.end_states.len()), "test {test_id} failed");
        // println!("-------------------------------------------------");
        test_id += 1;
    }
}

#[test]
fn dfa_modes() {
    let tests: Vec<(usize, BTreeMap<StateId, BTreeMap<char, StateId>>, BTreeMap<StateId, Terminal>)> = vec![
        (1, btreemap![
            0 => branch!['\t' => 2, '\n' => 2, '\r' => 2, ' ' => 2, '/' => 3, '0' => 4],// <skip>
            2 => branch!['\t' => 2, '\n' => 2, '\r' => 2, ' ' => 2],// <skip>
            3 => branch!['*' => 5],
            4 => branch!['1' => 6],
            5 => branch![],// <skip,push(mode 1,state 15)>
            6 => branch!['2' => 7],
            7 => branch!['3' => 8],
            8 => branch!['4' => 9],
            9 => branch!['5' => 10],
            10 => branch!['6' => 11],
            11 => branch!['7' => 12],
            12 => branch!['8' => 13],
            13 => branch!['9' => 14],
            14 => branch![],// <end:0>
            15 => branch!['/' => 17, '0' => 18, '1' => 18, '2' => 18, '3' => 18, '4' => 18, '5' => 18, '6' => 18, '7' => 18, '8' => 18, '9' => 18],// <skip>
            17 => branch!['*' => 19],
            18 => branch!['0' => 18, '1' => 18, '2' => 18, '3' => 18, '4' => 18, '5' => 18, '6' => 18, '7' => 18, '8' => 18, '9' => 18],// <skip>
            19 => branch![],// <skip,pop>
        ],
         btreemap![
            0 => term!(skip),
            2 => term!(skip),
            5 => term!(push 1) + term!(pushst 15),
            14 => term!(=0),
            15 => term!(skip),
            18 => term!(skip),
            19 => term!(pop)
         ])
    ];

    const VERBOSE: bool = false;
    for (test_id, exp_graph, exp_ends) in tests {
        if VERBOSE { println!("{test_id}:"); }
        let dfas = build_dfa(test_id);
        for (id, dfa) in dfas.iter() {
            if VERBOSE {
                println!("## mode {id}");
                print_graph(dfa);
            }
        }
        if VERBOSE { println!("## Merged:"); }
        let dfa = Dfa::from_dfa_modes(dfas);
        if VERBOSE {
            print_graph(&dfa);
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
            0 => branch!['a' => 1, 'b' => 0],
            1 => branch!['a' => 1, 'b' => 2],
            2 => branch!['a' => 1, 'b' => 3],
            3 => branch!['a' => 1, 'b' => 0],
        ], btreemap![3 => term!(=0)],
         btreemap![ // 1 <-> 2
             0 => branch!['a' => 2, 'b' => 0],
             1 => branch!['a' => 2, 'b' => 3],
             2 => branch!['a' => 2, 'b' => 1],
             3 => branch!['a' => 2, 'b' => 0],
         ], btreemap![3 => term!(=0)]),

        (1, btreemap![
            0 => branch!['a' => 1, 'b' => 2],
            1 => branch!['a' => 1, 'b' => 3],
            2 => branch!['a' => 1, 'b' => 2],
            3 => branch!['a' => 1, 'b' => 4],
            4 => branch!['a' => 1, 'b' => 2],
        ], btreemap![4 => term!(=0)],
         btreemap![ // 0 -> 0, 1 -> 2, 2 -> 0, 3 -> 1, 4 -> 3
            0 => branch!['a' => 2, 'b' => 0],
            1 => branch!['a' => 2, 'b' => 3],
            2 => branch!['a' => 2, 'b' => 1],
            3 => branch!['a' => 2, 'b' => 0],
         ], btreemap![3 => term!(=0)]),

        (8, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 3],
            3 => branch![]
        ], btreemap![1 => term!(=0), 3 => term!(=1)],
        btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2],
            2 => branch![],
        ], btreemap![1 => term!(=0), 2 => term!(=1)]),

        (9, btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 'c' => 2],
            2 => branch!['b' => 2, 'c' => 2, 'd' => 3],
            3 => branch![]
        ], btreemap![3 => term!(=0)],
        btreemap![
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 'c' => 2],
            2 => branch!['b' => 2, 'c' => 2, 'd' => 3],
            3 => branch![]
        ], btreemap![3 => term!(=0)]),

        (10, btreemap![
            0 => branch![
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 2, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3],
            1 => branch![
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3],
            2 => branch!['.' => 5, '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3, 'x' => 6],// END: 0
            3 => branch!['.' => 5, '0' => 3, '1' => 3, '2' => 3, '3' => 3, '4' => 3, '5' => 3, '6' => 3, '7' => 3, '8' => 3, '9' => 3],// END: 0
            5 => branch!['0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7],
            6 => branch![
                '0' => 8, '1' => 8, '2' => 8, '3' => 8, '4' => 8, '5' => 8, '6' => 8, '7' => 8, '8' => 8, '9' => 8,
                'A' => 8, 'B' => 8, 'C' => 8, 'D' => 8, 'E' => 8, 'F' => 8, 'a' => 8, 'b' => 8, 'c' => 8, 'd' => 8, 'e' => 8, 'f' => 8],
            7 => branch!['0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7],// END: 1
            8 => branch![
                '0' => 8, '1' => 8, '2' => 8, '3' => 8, '4' => 8, '5' => 8, '6' => 8, '7' => 8, '8' => 8, '9' => 8,
                'A' => 8, 'B' => 8, 'C' => 8, 'D' => 8, 'E' => 8, 'F' => 8, 'a' => 8, 'b' => 8, 'c' => 8, 'd' => 8, 'e' => 8, 'f' => 8],// END: 2
        ], btreemap![2 => term!(=0), 3 => term!(=0), 7 => term!(=1), 8 => term!(=2)],
        btreemap![
            0 => branch![
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 4, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5],
            1 => branch![
                '\t' => 1, '\n' => 1, '\r' => 1, ' ' => 1,
                '0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5],
            2 => branch!['0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6],
            3 => branch![
                '0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7,
                'A' => 7, 'B' => 7, 'C' => 7, 'D' => 7, 'E' => 7, 'F' => 7, 'a' => 7, 'b' => 7, 'c' => 7, 'd' => 7, 'e' => 7, 'f' => 7],
            4 => branch!['.' => 2, '0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5, 'x' => 3],// END: 0
            5 => branch!['.' => 2, '0' => 5, '1' => 5, '2' => 5, '3' => 5, '4' => 5, '5' => 5, '6' => 5, '7' => 5, '8' => 5, '9' => 5],// END: 0
            6 => branch!['0' => 6, '1' => 6, '2' => 6, '3' => 6, '4' => 6, '5' => 6, '6' => 6, '7' => 6, '8' => 6, '9' => 6],// END: 1
            7 => branch![
                '0' => 7, '1' => 7, '2' => 7, '3' => 7, '4' => 7, '5' => 7, '6' => 7, '7' => 7, '8' => 7, '9' => 7,
                'A' => 7, 'B' => 7, 'C' => 7, 'D' => 7, 'E' => 7, 'F' => 7, 'a' => 7, 'b' => 7, 'c' => 7, 'd' => 7, 'e' => 7, 'f' => 7],// END: 2
        ], btreemap![4 => term!(=0), 5 => term!(=0), 6 => term!(=1), 7 => term!(=2)]),

        // (abs<end:0>|abi<end:1>|at<end:2>|ab<end:3>)
        (12, btreemap![], btreemap![], // from build_re(12)
        btreemap![ // no change
            0 => branch!['a' => 1],
            1 => branch!['b' => 2, 't' => 3],
            2 => branch!['i' => 4, 's' => 5],// <end:3>
            3 => branch![],// <end:2>
            4 => branch![],// <end:1>
            5 => branch![],// <end:0>
        ], btreemap![2 => term!(=3), 3 => term!(=2), 4 => term!(=1), 5 => term!(=0)],
        )
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
            print_graph(&dfa);
        }
        assert_eq!(dfa.state_graph, exp_graph, "test {test_id} failed");
        assert_eq!(dfa.end_states, BTreeMap::from_iter(exp_end_states.into_iter()), "test {test_id} failed");
        assert_eq!(dfa.first_end_state, Some(dfa.state_graph.len() - dfa.end_states.len()), "test {test_id} failed");
    }
}
