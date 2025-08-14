// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

#![cfg(test)]

use crate::*;
use vectree::VecTree;
use crate::dfa::*;
use crate::log::TryBuildFrom;
// ---------------------------------------------------------------------------------------------
// Supporting functions

pub(crate) fn build_re(test: usize) -> VecTree<ReNode> {
    let mut re = VecTree::new();
    match test {
        0 => { // (a|b)*abb<end>
            let f = re.add_root(node!(&));
            let e = re.add_iter(Some(f), [node!(&), node!(=0)])[0];
            let d = re.add_iter(Some(e), [node!(&), node!(chr 'b')])[0];
            let c = re.add_iter(Some(d), [node!(&), node!(chr 'b')])[0];
            let b = re.add_iter(Some(c), [node!(*), node!(chr 'a')])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        },
        1 => { // (a|b)*abb<end>
            let c = re.add_root(node!(&));
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
            let c = re.add_root(node!(&));
            let b = re.add_iter(Some(c), [
                node!(*),
                node!(str "abb"),
                node!(=0)
            ])[0];
            let a = re.add(Some(b), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
        }
        3 => { // abc(a|b)<end>
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(str "abc"));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(=0));
        }
        4 => { // s(a|b)<end>
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(chr 's'));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(=0));
        }
        5 => {  // s(a<end>|b<end>)
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(chr 's'));
            let a = re.add(Some(root), node!(|));
            let cd = re.add_iter(Some(a), [node!(&), node!(&)]);
            re.add_iter(Some(cd[0]), [node!(chr 'a'), node!(=0)]);
            re.add_iter(Some(cd[1]), [node!(chr 'b'), node!(=1)]);
        }
        6 => {  // a(bc)?d = a(bc|-)d<end>
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(chr 'a'));
            let bc_opt = re.add(Some(root), node!(|));
            re.add_iter(Some(root), [node!(chr 'd'), node!(=0)]);
            re.add(Some(bc_opt), node!(str "bc"));
            re.add(Some(bc_opt), node!(e));
        },
        7 => {  // a(bc)?d?e = a(bc|-)(d|-)e<end>
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(chr 'a'));
            let bc_opt = re.add(Some(root), node!(|));
            let d_opt = re.add(Some(root), node!(|));
            re.add_iter(Some(root), [node!(chr 'e'), node!(=0)]);
            re.add(Some(bc_opt), node!(str "bc"));
            re.add(Some(bc_opt), node!(e));
            re.add(Some(d_opt), node!(chr 'd'));
            re.add(Some(d_opt), node!(e));
        },
        8 => {  // a(<end>|b<end>)
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(chr 'a'));
            let b1 = re.add(Some(root), node!(|));
            re.add(Some(b1), node!(=0));
            let b2 = re.add(Some(b1), node!(&));
            re.add_iter(Some(b2), [node!(chr 'b'), node!(=1)]);
        },
        9 => {  // a(b|c)+d<end>
            let root = re.add_root(node!(&));
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
            let or1 = re.add_root(node!(|));

            let cc1 = re.add(Some(or1), node!(&));
            let s0 = re.add(Some(cc1), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node!(chr ' '), node!(chr '\t'), node!(chr '\n'), node!(chr '\r')]);
            let plus1 = re.add(Some(cc1), node!(+));
            let or3 = re.add(Some(plus1), node!(|));
            re.add_iter(Some(or3), node!(chr '0', '9'));
            let or4 = re.add(Some(cc1), node!(|));
            re.add(Some(or4), node!(=0));

            let cc2 = re.add(Some(or4), node!(&));
            re.add(Some(cc2), node!(chr '.'));
            let plus2 = re.add(Some(cc2), node!(+));
            let or5 = re.add(Some(plus2), node!(|));
            re.add_iter(Some(or5), node!(chr '0','9'));
            re.add(Some(cc2), node!(=1));

            let cc3 = re.add(Some(or1), node!(&));
            re.add(Some(cc3), node!(chr '0'));
            re.add(Some(cc3), node!(chr 'x'));
            let plus3 = re.add(Some(cc3), node!(+));
            let or6 = re.add(Some(plus3), node!(|));
            re.add_iter(Some(or6), node!(chr '0','9'; 'A','F'; 'a','f'));
            re.add(Some(cc3), node!(=2));
        },
        11 => {
            // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>)
            //       or1:                      ^         ^            ^        ^        ^
            //          cc--------------------- --------- ------------ -------- -------- --------
            //             or2    or3 star3
            let cc0 = re.add_root(node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node!(chr ' '), node!(chr '\t'), node!(chr '\n'), node!(chr '\r')]);
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
            let or = re.add_root(node!(|));
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
            // ([ \t\n\r]+<end:1,ch 1>|[0-9]+<end:0>)
            let or0 = re.add_root(node!(|));
            let cc0 = re.add(Some(or0), node!(&));
            let p0 = re.add(Some(cc0), node!(+));
            let or0 = re.add(Some(p0), node!(|));
            re.add_iter(Some(or0), [node!(chr ' '), node!(chr '\t'), node!(chr '\n'), node!(chr '\r')]);
            re.add_iter(Some(cc0), [node!(term!(#1) + term!(=1))]);

            let cc1 = re.add(Some(or0), node!(&));
            re.add_iter(Some(cc1), node!(chr '0','9'));
            re.add(Some(cc1), node!(=0));
        },
        14 => {
            // \* /<end:0>|.+<end:1>
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(=0)]);
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            re.add(Some(plus2), node!(chr '.'));
            re.add(Some(cc2), node!(=1));
        },
        15 => {
            // Ambiguous: [A-B]+<end:0>|[B-C]+<end:1>
            let or = re.add_root(node!(|));
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
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            let or11 = re.add(Some(plus1), node!(|));
            re.add_iter(Some(or11), [node!(chr 'A'), node!(chr 'B')]);
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            let or21 = re.add(Some(plus2), node!(|));
            re.add_iter(Some(or21), [node!(chr 'B'), node!(chr 'C')]);
            re.add(Some(cc2), node!(chr 'Z'));
            re.add(Some(cc2), node!(=1));
        },
        17 => {
            // segments: [a-f]+<end:0>|[d-i]+z<end:1>|ey<end:2>
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            re.add(Some(plus1), node!(['a'-'f']));
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            re.add(Some(plus2), node!(['d'-'i']));
            re.add(Some(cc2), node!(chr 'z'));
            re.add(Some(cc2), node!(=1));
            let cc3 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc3), [node!(chr 'e'), node!(chr 'y'), node!(=2)]);
        },
        18 => {
            // segments: [a-f]+<end:0>|[d-i]+z<end:1>
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            re.add(Some(plus1), node!(['a'-'f']));
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            let plus2 = re.add(Some(cc2), node!(+));
            re.add(Some(plus2), node!(['d'-'i']));
            re.add(Some(cc2), node!(chr 'z'));
            re.add(Some(cc2), node!(=1));
        },
        19 => {
            // [0-9]+<end:0>|'.'<end:1>
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let plus1 = re.add(Some(cc1), node!(+));
            re.add(Some(plus1), node!(['a'-'f']));
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc2), [node!(chr '\''), node!([DOT]), node!(chr '\''), node!(=1)]);
        },
        20 => {
            // revisiting 11:
            // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>)
            //       or1:                      ^         ^            ^        ^        ^
            //          cc--------------------- --------- ------------ -------- -------- --------
            //                        star3
            let cc0 = re.add_root(node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            re.add(Some(s0), node!([' ', '\t', '\n', '\r']));
            let or1 = re.add(Some(cc0), node!(|));

            let cc = re.add(Some(or1), node!(&));
            re.add(Some(cc), node!(['a'-'z']));
            let star3 = re.add(Some(cc), node!(*));
            re.add(Some(star3), node!(['a'-'z', '0'-'9']));
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
        21 => {
            // skip on a nullable alternative => bad, will skip bad input indefinitely
            // mode 0: ([ \t\n\r]*<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add_root(node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add(Some(or0), node!([' ', '\t', '\n', '\r']));
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1) + term!(skip))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add(Some(or2), node!(['0'-'9']));
            re.add(Some(cc2), node!(=0));
        },
        22 => {
            // mode 0: ([ \t\n\r]+<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add_root(node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let p0 = re.add(Some(cc0), node!(+));
            let or0 = re.add(Some(p0), node!(|));
            re.add(Some(or0), node!([' ', '\t', '\n', '\r']));
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1) + term!(skip))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add(Some(or2), node!(['0'-'9']));
            re.add(Some(cc2), node!(=0));
        },
        // 23-26 don't work, need non-greedy repeaters
        23 => {
            // ('*'/<end:1>|.+<skip>)
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(=1))]);
            let cc2 = re.add(Some(or), node!(&));
            let l2 = re.add(Some(cc2), node!(??));
            let s2 = re.add(Some(l2), node!(+));
            re.add(Some(s2), node!([DOT]));
            re.add(Some(cc2), node!(term!(skip)));
        },
        24 => {
            // (.+?<skip>|'*'/<pop>)
            let or = re.add_root(node!(|));
            let cc2 = re.add(Some(or), node!(&));
            let l2 = re.add(Some(cc2), node!(??));
            let s2 = re.add(Some(l2), node!(+));
            re.add(Some(s2), node!([DOT]));
            re.add(Some(cc2), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(=1))]);
        },
        25 => {
            // /'*'.+?'*'/<end:0>
            let cc = re.add_root(node!(&));
            re.add_iter(Some(cc), [node!(chr '/'), node!(chr '*')]);
            let l0 = re.add(Some(cc), node!(??));
            let p0 = re.add(Some(l0), node!(+));
            re.add(Some(p0), node!([DOT]));
            re.add_iter(Some(cc), [node!(chr '*'), node!(chr '/'), node!(=0)]);
        },
        26 => {
            // /'*'.*?'*'/<end:0>
            let cc = re.add_root(node!(&));
            re.add_iter(Some(cc), [node!(chr '/'), node!(chr '*')]);
            let l0 = re.add(Some(cc), node!(??));
            let p0 = re.add(Some(l0), node!(*));
            re.add(Some(p0), node!([DOT]));
            re.add_iter(Some(cc), [node!(chr '*'), node!(chr '/'), node!(=0)]);
        },
        27 => {
            // [a-c]*?[c-e]*?c<end:0>
            let cc = re.add_root(node!(&));
            let l1 = re.add(Some(cc), node!(??));
            let s1 = re.add(Some(l1), node!(*));
            re.add(Some(s1), node!(['a'-'c']));
            let l2 = re.add(Some(cc), node!(??));
            let s2 = re.add(Some(l2), node!(*));
            re.add(Some(s2), node!(['c'-'e']));
            re.add_iter(Some(cc), [node!(chr 'c'), node!(=0)]);
        },
        28 => {
            // [a-c]*?a<end:0>|[a-c]*?b<end:1>|[a-c]*?c<end:2>
            // Ambiguous case that fails on most lexer/regex. Since the *s are non-greedy,
            // this should be equivalent to: (a<end:0>|b<end:1>|c<end:2>)
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let l1 = re.add(Some(cc1), node!(??));
            let s1 = re.add(Some(l1), node!(*));
            re.add(Some(s1), node!(['a'-'c']));
            re.add_iter(Some(cc1), [node!(chr 'a'), node!(=0)]);
            let cc2 = re.add(Some(or), node!(&));
            let l2 = re.add(Some(cc2), node!(??));
            let s2 = re.add(Some(l2), node!(*));
            re.add(Some(s2), node!(['a'-'c']));
            re.add_iter(Some(cc2), [node!(chr 'b'), node!(=1)]);
            let cc3 = re.add(Some(or), node!(&));
            let l3 = re.add(Some(cc3), node!(??));
            let s3 = re.add(Some(l3), node!(*));
            re.add(Some(s3), node!(['a'-'c']));
            re.add_iter(Some(cc3), [node!(chr 'c'), node!(=2)]);
        },
        29 => {
            // a<end:0>|b<end:1>|c<end:2>|[DOT]<end:3>
            // Priority of first terminal is used for a, b, and c, but it issues warnings.
            // To avoid the warning, use: a<end:0>|b<end:1>|c<end:2>|~[a-c]<end:3>
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            re.add(Some(cc1), node!(chr 'a'));
            re.add(Some(cc1), node!(=0));
            let cc2 = re.add(Some(or), node!(&));
            re.add(Some(cc2), node!(chr 'b'));
            re.add(Some(cc2), node!(=1));
            let cc3 = re.add(Some(or), node!(&));
            re.add(Some(cc3), node!(chr 'c'));
            re.add(Some(cc3), node!(=2));
            let cc4 = re.add(Some(or), node!(&));
            re.add(Some(cc4), node!([DOT]));
            re.add(Some(cc4), node!(=3));
        },
        30 => {
            // variation on 24: (.+?'/'<skip>|'*/'<pop>)
            let or = re.add_root(node!(|));
            let cc2 = re.add(Some(or), node!(&));
            let l2 = re.add(Some(cc2), node!(??));
            let s2 = re.add(Some(l2), node!(+));
            re.add(Some(s2), node!([DOT]));
            re.add(Some(cc2), node!(chr '/'));
            re.add(Some(cc2), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(=1))]);
        },
        31 => {
            // & and | with only one child: "|(&(&(1:'a'),2:<end:0>))"
            // a<end:0>
            let or = re.add_root(node!(|));
            let cc1 = re.add(Some(or), node!(&));
            let cc2 = re.add(Some(cc1), node!(&));
            re.add(Some(cc2), node!(chr 'a'));
            re.add(Some(cc1), node!(=0));
        },
        32 => {
            // .+?abc<end:0>
            let cc = re.add_root(node!(&));
            let l0 = re.add(Some(cc), node!(??));
            let p0 = re.add(Some(l0), node!(+));
            re.add(Some(p0), node!([DOT]));
            re.add_iter(Some(cc), [node!(chr 'a'), node!(chr 'b'), node!(chr 'c'), node!(=0)]);
        }
        33 => {
            // (if<end:0>|[a-z]+<end:1>)
            let or = re.add_root(node!(|));
            let cc0 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc0), [node!(str "if"), node!(=0)]);
            let cc1 = re.add(Some(or), node!(&));
            let p1 = re.add(Some(cc1), node!(+));
            re.add(Some(p1), node!(['a'-'z']));
            re.add(Some(cc1), node!(=1));
        }
        // error tests ---------------------------------------------------------
        200 => { // abc(a|b)""<end>
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(str "abc"));
            let a = re.add(Some(root), node!(|));
            re.add_iter(Some(a), [node!(chr 'a'), node!(chr 'b')]);
            re.add(Some(root), node!(str ""));
            re.add(Some(root), node!(|));
            re.add(Some(root), node!(=0));
        }
        201 => {
            let root = re.add_root(node!(&));
            let a = re.add(Some(root), node!(chr 'a'));
            re.add(Some(root), node!(=0));
            re.add_iter(Some(a), [node!(chr 'E'), node!(chr 'h'), node!(chr '?')]);
            re.add(Some(root), node!(*));
            re.addc_iter(Some(root), node!(+), [node!(chr '1'), node!(chr '2')]);
        }
        202 => {
            let root = re.add_root(node!(&));
            re.add(Some(root), node!(&));
            re.add(Some(root), node!(??));
            re.addc_iter(Some(root), node!(??), [node!(chr '1'), node!(chr '2')]);
        }
        203 => {
            let root = re.add_root(node!(|));
            re.addc_iter(Some(root), node!(&), [node!(chr 'a'), node!(= 0)]);
            re.addc_iter(Some(root), node!(&), [node!(chr 'b'), node!(= 1)]);
            re.addc(Some(root), node!(&), node!(chr 'c'));  // missing end
            re.addc_iter(Some(root), node!(&), [node!(chr 'd'), node!(= 1)]);
        }
        _ => { }
    }
    re
}

pub(crate) fn build_dfa(test: usize) -> BTreeMap<ModeId, Dfa<General>> {
    let mut re = VecTree::new();
    let modes: BTreeMap<ModeId, VecTree<ReNode>> = match test {
        1 => {
            // mode 0: ([ \t\n\r]*<skip>|/\*<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add_root(node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let s0 = re.add(Some(cc0), node!(*));
            let or0 = re.add(Some(s0), node!(|));
            re.add_iter(Some(or0), [node!(chr ' '), node!(chr '\t'), node!(chr '\n'), node!(chr '\r')]);
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1) + term!(skip))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add_iter(Some(or2), node!(chr '0','9'));
            re.add(Some(cc2), node!(=0));

            // mode 1: (\*/<pop>|[0-9]*<skip>)
            let mut re1 = VecTree::new();
            let or = re1.add_root(node!(|));
            let cc1 = re1.add(Some(or), node!(&));
            re1.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(pop))]);
            let cc2 = re1.add(Some(or), node!(&));
            let s2 = re1.add(Some(cc2), node!(*));
            let or2 = re1.add(Some(s2), node!(|));
            re1.add_iter(Some(or2), node!(chr '0','9'));
            re1.add(Some(cc2), node!(term!(skip)));

            btreemap![0 => re, 1 => re1]
        },
        2 => {
            // mode 0: ([ \t\n\r]+<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
            let or = re.add_root(node!(|));
            let cc0 = re.add(Some(or), node!(&));
            let p0 = re.add(Some(cc0), node!(+));
            let or0 = re.add(Some(p0), node!(|));
            re.add(Some(or0), node!([' ', '\t', '\n', '\r']));
            re.add(Some(cc0), node!(term!(skip)));
            let cc1 = re.add(Some(or), node!(&));
            re.add_iter(Some(cc1), [node!(chr '/'), node!(chr '*'), node!(term!(push 1) + term!(skip))]);
            let cc2 = re.add(Some(or), node!(&));
            let s2 = re.add(Some(cc2), node!(+));
            let or2 = re.add(Some(s2), node!(|));
            re.add(Some(or2), node!(['0'-'9']));
            re.add(Some(cc2), node!(=0));

            // mode 1: ('*'/<pop>|.+<skip>)
            let mut re1 = VecTree::new();
            let or = re1.add_root(node!(|));
            let cc1 = re1.add(Some(or), node!(&));
            re1.add_iter(Some(cc1), [node!(chr '*'), node!(chr '/'), node!(term!(pop))]);
            let cc2 = re1.add(Some(or), node!(&));
            let s2 = re1.add(Some(cc2), node!(+));
            re1.add(Some(s2), node!([DOT]));
            re1.add(Some(cc2), node!(term!(skip)));

            btreemap![0 => re, 1 => re1]
        },
        3 => {
            // mode 0: '\''<more,push(1)>|'.'<end:0>
            let or = re.add_root(node!(|));
            re.addc_iter(Some(or), node!(&), [node!(chr '\''), node!(term!(push 1) + term!(more))]);
            re.addc_iter(Some(or), node!(&), [node!(chr '.'), node!(term!(= 0))]);

            // mode 1: '\''<end:1,pop>|[' ', 'a'-'z']<more>
            let mut re1 = VecTree::new();
            let or = re1.add_root(node!(|));
            re1.addc_iter(Some(or), node!(&), [node!(chr '\''), node!(term!(= 1) + term!(pop))]);
            re1.addc_iter(Some(or), node!(&), [node!([' ', 'a'-'z']), node!(term!(more))]);

            btreemap![0 => re, 1 => re1]
        }
        _ => btreemap![]
    };
    modes.into_iter()
        .map(|(n, re)| (n, Dfa::build_from(DfaBuilder::build_from(re))))
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
        let children = tree.children(i).iter().map(|n| n.to_string()).join(",");
        if children.len() > 0 {
            result.push_str(" -> [");
            result.push_str(&children);
            result.push(']');
        }
        let mut firstpos = node.firstpos.iter().to_vec();
        firstpos.sort();
        let firstpos = firstpos.iter().map(|n| n.to_string()).join(",");
        if firstpos.len() > 0 {
            result.push_str(" fp:");
            result.push_str(&firstpos);
        }
        let mut lastpos = node.lastpos.iter().to_vec();
        lastpos.sort();
        let lastpos = lastpos.iter().map(|n| n.to_string()).join(",");
        if lastpos.len() > 0 {
            result.push_str(" lp:");
            result.push_str(&lastpos);
        }
        result.push('\n');
    }
    result
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
        let result1 = tree_to_string(&re, None, false);
        let dfa = DfaBuilder::build_from(re);
        let result2 = tree_to_string(&dfa.re, None, false);
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
        let mut dfa = DfaBuilder::build_from(re);
        dfa.calc_node_pos();
        assert_eq!(tree_to_string(&dfa.re, None, true), expected, "test {test_id} failed");
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
        (11, "&(!*(|(1:' ',2:'\\t',3:'\\n',4:'\\r')),|(&(|(5:'a',6:'b',7:'c',8:'d',9:'e',10:'f',11:'g',12:'h',13:'i',14:'j',15:'k',16:'l',17:'m',18:'n',19:'o',20:'p',21:'q',22:'r',23:'s',24:'t',25:'u',26:'v',27:'w',28:'x',29:'y',30:'z'),!*(|(31:'a',32:'b',33:'c',34:'d',35:'e',36:'f',37:'g',38:'h',39:'i',40:'j',41:'k',42:'l',43:'m',44:'n',45:'o',46:'p',47:'q',48:'r',49:'s',50:'t',51:'u',52:'v',53:'w',54:'x',55:'y',56:'z',57:'0',58:'1',59:'2',60:'3',61:'4',62:'5',63:'6',64:'7',65:'8',66:'9')),67:<end:0>),&(&(68:'i',69:'f'),70:<end:1>),&(&(71:'p',72:'r',73:'i',74:'n',75:'t'),76:<end:2>),&(77:'=',78:<end:3>),&(79:'+',80:<end:4>),&(81:';',82:<end:5>)))"),
        (12, "|(&(&(1:'a',2:'b',3:'s'),4:<end:0>),&(&(5:'a',6:'b',7:'i'),8:<end:1>),&(&(9:'a',10:'t'),11:<end:2>),&(&(12:'a',13:'b'),14:<end:3>))"),
        (15, "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:<end:1>))"),
        (16, "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:'Z',7:<end:1>))"),
        (17, "|(&(+(1:['a'-'f']),2:<end:0>),&(+(3:['d'-'i']),4:'z',5:<end:1>),&(6:'e',7:'y',8:<end:2>))"),
        (19, "|(&(+(1:['a'-'f']),2:<end:0>),&(3:'\\'',4:[DOT],5:'\\'',6:<end:1>))"),
        (20, "&(!*(1:['\\t'-'\\n', '\\r', ' ']),|(&(2:['a'-'z'],!*(3:['0'-'9', 'a'-'z']),4:<end:0>),&(&(5:'i',6:'f'),7:<end:1>),&(&(8:'p',9:'r',10:'i',11:'n',12:'t'),13:<end:2>),&(14:'=',15:<end:3>),&(16:'+',17:<end:4>),&(18:';',19:<end:5>)))"),
        (21, "|(&(!*(|(1:['\\t'-'\\n', '\\r', ' '])),2:<skip>),&(3:'/',4:'*',5:<skip,push(1)>),&(+(|(6:['0'-'9'])),7:<end:0>))"),
        (22, "|(&(+(|(1:['\\t'-'\\n', '\\r', ' '])),2:<skip>),&(3:'/',4:'*',5:<skip,push(1)>),&(+(|(6:['0'-'9'])),7:<end:0>))"),
        (23, "|(&(1:'*',2:'/',3:<end:1>),&(??(+(4:[DOT])),5:<skip>))"),
        (25, "&(1:'/',2:'*',??(+(3:[DOT])),4:'*',5:'/',6:<end:0>)"),
        (26, "&(1:'/',2:'*',!??(!*(3:[DOT])),4:'*',5:'/',6:<end:0>)"),
        (30, "|(&(??(+(1:[DOT])),2:'/',3:<skip>),&(4:'*',5:'/',6:<end:1>))"),
    ];
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::build_from(re);
        dfa_builder.calc_node_pos();
        assert_eq!(tree_to_string(&dfa_builder.re, None, false), expected, "test {test_id} failed");
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
        let mut dfa_builder = DfaBuilder::build_from(re);
        dfa_builder.calc_node_pos();
        let mut result = Vec::new();
        for inode in dfa_builder.re.iter_depth_simple() {
            let mut firstpos = inode.firstpos.iter().map(|n| *n).to_vec();
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
        let mut dfa_builder = DfaBuilder::build_from(re);
        dfa_builder.calc_node_pos();
        let mut result = Vec::new();
        for inode in dfa_builder.re.iter_depth_simple() {
            let mut lastpos = inode.lastpos.iter().map(|n| *n).to_vec();
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
        // (\*/<pop>|.+<skip>)
        // "|(&(1:'*',2:'/',3:<skip,pop>),&(+(4:[DOT]),5:<skip>))"
        (23, btreemap![
            1 => btreeset![2],
            2 => btreeset![3],
            3 => btreeset![],
            4 => btreeset![4, 5],
            5 => btreeset![]
        ])
    };
    for (test_id, expected) in tests.into_iter() {
        let re = build_re(test_id);
        let mut dfa_builder = DfaBuilder::build_from(re);
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
        ], btreemap![3 => term!(=0)], 0),
        // (a|b)*abb<end>
        (1, btreemap![
            0 => branch!('a' => 1, 'b' => 0),
            1 => branch!('a' => 1, 'b' => 2),
            2 => branch!('a' => 1, 'b' => 3),
            3 => branch!('a' => 1, 'b' => 0),
        ], btreemap![3 => term!(=0)], 0),
        (2, btreemap![
            0 => branch!('a' => 1, 'b' => 0),
            1 => branch!('a' => 1, 'b' => 2),
            2 => branch!('a' => 1, 'b' => 3),
            3 => branch!('a' => 1, 'b' => 0),
        ], btreemap![3 => term!(=0)], 0),
        // "&(&(1:'a',2:'b',3:'c'),|(4:'a',5:'b'),6:<end>)",
        (3, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2),
            2 => branch!('c' => 3),
            3 => branch!('a'-'b' => 4),
            4 => branch!()
        ], btreemap![4 => term!(=0)], 0),
        // "&(1:'s',|(2:'a',3:'b'),4:<end>)",
        (4, btreemap![
            0 => branch!('s' => 1),
            1 => branch!('a'-'b' => 2),
            2 => branch!(),
        ], btreemap![2 => term!(=0)], 0),
        // "&(1:'s',|(&(2:'a',3:<end>),&(4:'b',5:<end>)))",
        (5, btreemap![
            0 => branch!('s' => 1),
            1 => branch!('a' => 2, 'b' => 3),
            2 => branch!(),
            3 => branch!(),
        ], btreemap![2 => term!(=0), 3 => term!(=1)], 0),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),5:'d',6:<end>)"
        (6, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 'd' => 3),
            2 => branch!('c' => 4),
            3 => branch!(),
            4 => branch!('d' => 3)
        ], btreemap![3 => term!(=0)], 0),
        // "&(1:'a',|(&(2:'b',3:'c'),4:-),|(5:'d',6:-),7:'e',8:<end>)"
        (7, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 'd' => 3, 'e' => 4),
            2 => branch!('c' => 5),
            3 => branch!('e' => 4),
            4 => branch!(),
            5 => branch!('d' => 3, 'e' => 4)
        ], btreemap![4 => term!(=0)], 0),
        // "&(1:'a',|(2:<end:0>,&(3:'b',4:<end:1>)))"
        (8, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2),
            2 => branch!()
        ], btreemap![1 => term!(=0), 2 => term!(=1)], 0),
        // "&(1:'a',+(|(2:'b',3:'c')),4:'d')"
        (9, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b'-'c' => 2),
            2 => branch!('b'-'c' => 2, 'd' => 3),
            3 => branch!()
        ], btreemap![3 => term!(=0)], 0),
        // [ \t\n\r]*[0-9]+(<end:0>|\.[0-9]+<end:1>)|0x[0-9A-Fa-f]+<end:2>
        // "|(&(!*(|(1:' ',2:'\\t',3:'\\n',4:'\\r')),+(|(5:'0',6:'1',7:'2',8:'3',9:'4',10:'5',11:'6',12:'7',13:'8',14:'9')),|(15:<end:0>,&(16:'.',+(|(17:'0',18:'1',19:'2',20:'3',21:'4',22:'5',23:'6',24:'7',25:'8',26:'9')),27:<end:1>))),&(28:'0',29:'x',+(|(30:'0',31:'1',32:'2',33:'3',34:'4',35:'5',36:'6',37:'7',38:'8',39:'9',40:'A',41:'B',42:'C',43:'D',44:'E',45:'F',46:'a',47:'b',48:'c',49:'d',50:'e',51:'f')),52:<end:2>))"
        (10, btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 1, '0' => 2, '1'-'9' => 3),
            1 => branch!('\t'-'\n', '\r', ' ' => 1, '0'-'9' => 3),
            2 => branch!('.' => 4, '0'-'9' => 3, 'x' => 5), // <end:0>
            3 => branch!('.' => 4, '0'-'9' => 3), // <end:0>
            4 => branch!('0'-'9' => 6),
            5 => branch!(['0'-'9', 'A'-'F', 'a'-'f'] => 7),
            6 => branch!('0'-'9' => 6), // <end:1>
            7 => branch!(['0'-'9', 'A'-'F', 'a'-'f'] => 7), // <end:2>
        ], btreemap![2 => term!(=0), 3 => term!(=0), 6 => term!(=1), 7 => term!(=2)], 0),
        // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>)
        // "&(!*(|(1:' ',2:'\\t',3:'\\n',4:'\\r')),|(&(|(5:'a',6:'b',7:'c',8:'d',9:'e',10:'f',11:'g',12:'h',13:'i',14:'j',15:'k',16:'l',17:'m',18:'n',19:'o',20:'p',21:'q',22:'r',23:'s',24:'t',25:'u',26:'v',27:'w',28:'x',29:'y',30:'z'),!*(|(31:'a',32:'b',33:'c',34:'d',35:'e',36:'f',37:'g',38:'h',39:'i',40:'j',41:'k',42:'l',43:'m',44:'n',45:'o',46:'p',47:'q',48:'r',49:'s',50:'t',51:'u',52:'v',53:'w',54:'x',55:'y',56:'z',57:'0',58:'1',59:'2',60:'3',61:'4',62:'5',63:'6',64:'7',65:'8',66:'9')),67:<end:0>),&(&(68:'i',69:'f'),70:<end:1>),&(&(71:'p',72:'r',73:'i',74:'n',75:'t'),76:<end:2>),&(77:'=',78:<end:3>),&(79:'+',80:<end:4>),&(81:';',82:<end:5>)))"
        (11, btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 0, '+' => 1, ';' => 2, '=' => 3, 'a'-'h', 'j'-'o', 'q'-'z' => 4, 'i' => 5, 'p' => 6),
            1 => branch!(), // <end:4>
            2 => branch!(), // <end:5>
            3 => branch!(), // <end:3>
            4 => branch!('0'-'9', 'a'-'z' => 4), // <end:0>
            5 => branch!('0'-'9', 'a'-'e', 'g'-'z' => 4, 'f' => 7), // <end:0>
            6 => branch!('0'-'9', 'a'-'q', 's'-'z' => 4, 'r' => 8), // <end:0>
            7 => branch!('0'-'9', 'a'-'z' => 4), // <end:1>
            8 => branch!('0'-'9', 'a'-'h', 'j'-'z' => 4, 'i' => 9), // <end:0>
            9 => branch!('0'-'9', 'a'-'m', 'o'-'z' => 4, 'n' => 10), // <end:0>
            10 => branch!('0'-'9', 'a'-'s', 'u'-'z' => 4, 't' => 11), // <end:0>
            11 => branch!('0'-'9', 'a'-'z' => 4), // <end:2>
        ], btreemap![
            1 => term!(=4), 2 => term!(=5), 3 => term!(=3), 4 => term!(=0), 5 => term!(=0), 6 => term!(=0),
            7 => term!(=1), 8 => term!(=0), 9 => term!(=0), 10 => term!(=0), 11 => term!(=2)], 0),
        // (abs<end:0>|abi<end:1>|at<end:2>|ab<end:3>), to check if string paths are merged
        (12, btreemap![
            0 => branch!('a' => 1),
            1 => branch!('b' => 2, 't' => 3),
            2 => branch!('i' => 4, 's' => 5), // <end:3>
            3 => branch!(), // <end:2>
            4 => branch!(), // <end:1>
            5 => branch!(), // <end:0>
        ], btreemap![2 => term!(=3), 3 => term!(=2), 4 => term!(=1), 5 => term!(=0)], 0),
        // ([ \t\n\r]*{channel(1)}<end:1>|[0-9]+<end:0>)
        (13, btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 1, '0' => 2),
            1 => branch!('\t'-'\n', '\r', ' ' => 1, '0' => 2), // <end:1,ch 1>
            2 => branch!('1' => 3),
            3 => branch!('2' => 4),
            4 => branch!('3' => 5),
            5 => branch!('4' => 6),
            6 => branch!('5' => 7),
            7 => branch!('6' => 8),
            8 => branch!('7' => 9),
            9 => branch!('8' => 10),
            10 => branch!('9' => 11),
            11 => branch!(), // <end:0>
        ], btreemap![1 => term!(=1) + term!(#1), 11 => term!(=0)], 0),
        // \* /<end:0>|.+<end:1>
        (14, btreemap![
            0 => branch!('*' => 1, '.' => 2),
            1 => branch!('/' => 3),
            2 => branch!('.' => 2),// <end:1>
            3 => branch!(),// <end:0>
        ], btreemap![2 => term!(=1), 3 => term!(=0)], 0),
        // Ambiguous: [A-B]+<end:0>|[B-C]+<end:1>
        // "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:<end:1>))"
        (15, btreemap![
            0 => branch!('A' => 1, 'B' => 2, 'C' => 3),
            1 => branch!('A'-'B' => 1),// <end:0>
            2 => branch!('A' => 1, 'B' => 2, 'C' => 3),// <end:0>
            3 => branch!('B'-'C' => 3),// <end:1>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 3 => term!(=1)], 1),
        // [A-B]+<end:0>|[B-C]+Z<end:1>
        // "|(&(+(|(1:'A',2:'B')),3:<end:0>),&(+(|(4:'B',5:'C')),6:'Z',7:<end:1>))"
        (16, btreemap![
            0 => branch!('A' => 1, 'B' => 2, 'C' => 3),
            1 => branch!('A'-'B' => 1),// <end:0>
            2 => branch!('A' => 1, 'B' => 2, 'C' => 3, 'Z' => 4),// <end:0>
            3 => branch!('B'-'C' => 3, 'Z' => 4),
            4 => branch!(),// <end:1>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 4 => term!(=1)], 0),
        // segments: [a-f]+<end:0>|[d-i]+z<end:1>|ey<end:2>
        // "|(&(+(1:['a'-'f']),2:<end:0>),&(+(3:['d'-'i']),4:'z',5:<end:1>),&(6:'e',7:'y',8:<end:2>))"
        (17, btreemap![
            0 => branch!('a'-'c' => 1, 'd', 'f' => 2, 'e' => 3, 'g'-'i' => 4),
            1 => branch!('a'-'f' => 1), // <end:0>
            2 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 4, 'z' => 5), // <end:0>
            3 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 4, 'y' => 6, 'z' => 5), // <end:0>
            4 => branch!('d'-'i' => 4, 'z' => 5),
            5 => branch!(), // <end:1>
            6 => branch!(), // <end:2>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 3 => term!(=0), 5 => term!(=1), 6 => term!(=2)], 0),
        // [a-f]+<end:0>|[d-i]+z<end:1>
        // "|(&(+(1:['a'-'f']),2:<end:0>),&(+(3:['d'-'i']),4:'z',5:<end:1>))"
        (18, btreemap![
            0 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 3),
            1 => branch!('a'-'f' => 1),// <end:0>
            2 => branch!('a'-'c' => 1, 'd'-'f' => 2, 'g'-'i' => 3, 'z' => 4),// <end:0>
            3 => branch!('d'-'i' => 3, 'z' => 4),
            4 => branch!(),// <end:1>
        ], btreemap![1 => term!(=0), 2 => term!(=0), 4 => term!(=1)], 0),
        // [0-9]+<end:0>|'.'<end:1>
        // "|(&(+(1:'a'-'f'),2:<end:0>),&(3:'\\'',4:.,5:'\\'',6:<end:1>))"
        (19, btreemap![
            0 => branch!('\'' => 1, 'a'-'f' => 2),
            1 => branch!([DOT] => 3),
            2 => branch!('a'-'f' => 2), // <end:0>
            3 => branch!('\'' => 4),
            4 => branch!(), // <end:1>
        ], btreemap![2 => term!(=0), 4 => term!(=1)], 0),
        // [ \t\n\r]*([a-z][a-z0-9]*<end:0>|if<end:1>|print<end:2>|=<end:3>|+<end:4>|;<end:5>)
        // "&(!*(1:['\t'-'\n', '\r', ' ']),|(&(2:['a'-'z'],!*(3:['0'-'9', 'a'-'z']),4:<end:0>),&(&(5:'i',6:'f'),7:<end:1>),&(&(8:'p',9:'r',10:'i',11:'n',12:'t'),13:<end:2>),&(14:'=',15:<end:3>),&(16:'+',17:<end:4>),&(18:';',19:<end:5>)))"
        (20, btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 0, '+' => 1, ';' => 2, '=' => 3, 'a'-'h', 'j'-'o', 'q'-'z' => 4, 'i' => 5, 'p' => 6),
            1 => branch!(), // <end:4>
            2 => branch!(), // <end:5>
            3 => branch!(), // <end:3>
            4 => branch!('0'-'9', 'a'-'z' => 4), // <end:0>
            5 => branch!('0'-'9', 'a'-'e', 'g'-'z' => 4, 'f' => 7), // <end:0>
            6 => branch!('0'-'9', 'a'-'q', 's'-'z' => 4, 'r' => 8), // <end:0>
            7 => branch!('0'-'9', 'a'-'z' => 4), // <end:1>
            8 => branch!('0'-'9', 'a'-'h', 'j'-'z' => 4, 'i' => 9), // <end:0>
            9 => branch!('0'-'9', 'a'-'m', 'o'-'z' => 4, 'n' => 10), // <end:0>
            10 => branch!('0'-'9', 'a'-'s', 'u'-'z' => 4, 't' => 11), // <end:0>
            11 => branch!('0'-'9', 'a'-'z' => 4), // <end:2>
        ], btreemap![
            1 => term!(=4), 2 => term!(=5), 3 => term!(=3), 4 => term!(=0), 5 => term!(=0), 6 => term!(=0),
            7 => term!(=1), 8 => term!(=0), 9 => term!(=0), 10 => term!(=0), 11 => term!(=2)], 0),
        // mode 0: ([ \t\n\r]*<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
        // |(  &(!*(|(1:['\\t', '\\n', '\\r', ' '])),2:<skip>),
        //     &(3:'/',4:'*',5:<skip,push(mode 1)>),
        //     &(+(|(6:'0'-'9')),7:<end:0>))
        (21, btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 1, '/' => 2, '0'-'9' => 3), // <skip>
            1 => branch!('\t'-'\n', '\r', ' ' => 1), // <skip>
            2 => branch!('*' => 4),
            3 => branch!('0'-'9' => 3), // <end:0>
            4 => branch!(), // <skip,push(mode 1)>
        ], btreemap![0 => term!(skip), 1 => term!(skip), 3 => term!(=0), 4 => term!(push 1)], 1),
        // mode 0: ([ \t\n\r]+<skip>|/'*'<skip,push(1)>|[0-9]+<end:0>)
        // |(  &(+(|(1:['\\t', '\\n', '\\r', ' '])),2:<skip>),
        //     &(3:'/',4:'*',5:<skip,push(mode 1)>),
        //     &(+(|(6:'0'-'9')),7:<end:0>))
        (22, btreemap![
            0 => branch!('\t'-'\n', '\r', ' ' => 1, '/' => 2, '0'-'9' => 3),
            1 => branch!('\t'-'\n', '\r', ' ' => 1), // <skip>
            2 => branch!('*' => 4),
            3 => branch!('0'-'9' => 3), // <end:0>
            4 => branch!(), // <skip,push(mode 1)>
        ], btreemap![1 => term!(skip), 3 => term!(=0), 4 => term!(push 1)], 0),
        // 23-26 need non-greedy repeaters
        // ('*'/<end:1>|.+?<skip>)
        // "|(&(1:['*'],2:['/'],3:<end:1>),&(??(+(4:[DOT])),5:<skip>))"
        (23, btreemap![
            0 => branch!(~['*'] => 1, ['*'] => 2),
            1 => branch!(DOT => 1), // <skip>
            2 => branch!(~['/'] => 1, ['/'] => 3), // <skip>
            3 => branch!(), // <end:1>
        ], btreemap![1 => term!(skip), 2 => term!(skip), 3 => term!(=1)], 0),

        // (.+?<skip>|'*'/<end:1>)
        // "|(&(??(+(1:[DOT])),2:<skip>),&(3:['*'],4:['/'],5:<end:1>))"
        (24, btreemap![
            0 => branch!(~['*'] => 1, ['*'] => 2),
            1 => branch!(DOT => 1), // <skip>
            2 => branch!(~['/'] => 1, ['/'] => 3), // <skip>
            3 => branch!(), // <end:1>
        ], btreemap![1 => term!(skip), 2 => term!(skip), 3 => term!(=1)], 0),
        // /'*'.+?'*'/<end:0>
        // "&(1:['/'],2:['*'],??(+(3:[DOT])),4:['*'],5:['/'],6:<end:0>)"
        (25, btreemap![
            0 => branch!('/' => 1),
            1 => branch!('*' => 2),
            2 => branch!([DOT] => 3),
            3 => branch!(~['*'] => 3, ['*'] => 4),
            4 => branch!(~['*', '/'] => 3, ['*'] => 4, ['/'] => 5),
            5 => branch!(), // <end:0>
        ], btreemap![5 => term!(=0)], 0),
        // '/*'.*?'*/'<end:0>
        // "&(1:['/'],2:['*'],??(!*(3:[DOT])),4:['*'],5:['/'],6:<end:0>)"
        (26, btreemap![
            0 => branch!('/' => 1),
            1 => branch!('*' => 2),
            2 => branch!(~['*'] => 2, ['*'] => 3),
            3 => branch!(~['*', '/'] => 2, ['*'] => 3, ['/'] => 4),
            4 => branch!(), // <end:0>
        ], btreemap![4 => term!(=0)], 0),
        // [a-c]*?[c-e]*?c<end:0>
        // "&(!??(!*(1:['a'-'c'])),!??(!*(2:['c'-'e'])),3:['c'],4:<end:0>)"
        (27, btreemap![
            0 => branch!('a'-'b' => 0, 'c' => 1, 'd'-'e' => 2),
            1 => branch!(), // <end:0>
            2 => branch!('c' => 3, 'd'-'e' => 2),
            3 => branch!(), // <end:0>
        ], btreemap![1 => term!(=0), 3 => term!(=0)], 0),
        // ([a-c]*?a<end:0>|[a-c]*?b<end:1>|[a-c]*?c<end:2>)
        // "|(&(!??(!*(1:['a'-'c'])),2:['a'],3:<end:0>),&(!??(!*(4:['a'-'c'])),5:['b'],6:<end:1>),&(!??(!*(7:['a'-'c'])),8:['c'],9:<end:2>))"
        (28, btreemap![
            0 => branch!('a' => 1, 'b' => 2, 'c' => 3),
            1 => branch!(), // <end:0>
            2 => branch!(), // <end:1>
            3 => branch!(), // <end:2>
        ], btreemap![1 => term!(=0), 2 => term!(=1), 3 => term!(=2)], 0),
        // a<end:0>|b<end:1>|c<end:2>|[DOT]<end:3>
        // "|(&(1:'a',2:<end:0>),&(3:'b',4:<end:1>),&(5:'c',6:<end:2>),&(7:[DOT],8:<end:3>))"
        (29, btreemap![
            0 => branch!(~['a', 'b', 'c'] => 1, ['a'] => 2, ['b'] => 3, ['c'] => 4),
            1 => branch!(), // <end:3>
            2 => branch!(), // <end:0>
            3 => branch!(), // <end:1>
            4 => branch!(), // <end:2>
        ], btreemap![1 => term!(=3), 2 => term!(=0), 3 => term!(=1), 4 => term!(=2)], 3),
        // (.+?'/'<skip>|'*/'<end:1>)
        // "|(&(??(+(1:[DOT])),2:['/'],3:<skip>),&(4:['*'],5:['/'],6:<end:1>))"
        (30, btreemap![
            0 => branch!(~['*'] => 1, ['*'] => 2),
            1 => branch!(~['/'] => 1, ['/'] => 3),
            2 => branch!(~['/'] => 1, ['/'] => 4),
            3 => branch!(), // <skip>
            4 => branch!(), // <end:1>
        ], btreemap![3 => term!(skip), 4 => term!(=1)], 0),
        // tests that & and | work correclty with only one child
        // "|(&(&(1:'a'),2:<end:0>))"
        (31, btreemap![
            0 => branch!('a' => 1),
            1 => branch!(), // <end:0>
        ], btreemap![1 => term!(=0)], 0),
        // .+?abc<end:0>
        (32, btreemap![
            0 => branch!(DOT => 1),
            1 => branch!(~['a'] => 1, ['a'] => 2),
            2 => branch!(~['a'-'b'] => 1, ['a'] => 2, ['b'] => 3),
            3 => branch!(~['a', 'c'] => 1, ['a'] => 2, ['c'] => 4),
            4 => branch!(), // <end:0>
        ], btreemap![4 => term!(=0)], 0),
        // (if<end:0>|[a-z]+<end:1>)
        // "|(&(&(1:['i'],2:['f']),3:<end:0>),&(+(4:['a'-'z']),5:<end:1>))"
        (33, btreemap![
            0 => branch!('a'-'h', 'j'-'z' => 1, 'i' => 2),
            1 => branch!('a'-'z' => 1), // <end:1>
            2 => branch!('a'-'e', 'g'-'z' => 1, 'f' => 3), // <end:1>
            3 => branch!('a'-'z' => 1), // <end:0>
        ], btreemap![1 => term!(=1), 2 => term!(=1), 3 => term!(=0)], 0),
    ];
    const VERBOSE: bool = false;
    const RUN_ALL: bool = false;
    let mut errors = 0;
    for (test_id, expected, expected_ends, expected_warnings) in tests {
        if VERBOSE { println!("Test {test_id}:"); }
        let re = build_re(test_id);
        let dfa = Dfa::<General>::build_from(DfaBuilder::build_from(re));
        if VERBOSE {
            dfa.print(12);
            let msg = dfa.get_log().get_messages_str();
            if !msg.is_empty() {
                println!("{msg}");
            }
            println!();
        }
        if RUN_ALL {
            let mut msg = Vec::<String>::new();
            if dfa.state_graph != expected {
                msg.push("DFA state graph incorrect".to_string());
            }
            if dfa.end_states != expected_ends {
                msg.push("End states incorrect".to_string());
            }
            if dfa.get_log().num_warnings() != expected_warnings {
                msg.push("Number of warnings not as expected".to_string());
                msg.extend(dfa.get_log().get_warnings().cloned());
            }
            if !dfa.get_log().has_no_errors() {
                msg.push("Errors:".to_string());
                msg.extend(dfa.get_log().get_errors().cloned());
            }
            if msg.len() > 0 {
                println!("ERRORS in test {test_id}:");
                for m in msg {
                    println!("{m}");
                }
                println!();
                errors += 1;
            }
        } else {
            assert_eq!(dfa.state_graph, expected, "test {test_id} failed");
            assert_eq!(dfa.end_states, expected_ends, "test {test_id} failed");
            assert_eq!(dfa.log.num_warnings(), expected_warnings, "test {test_id} failed:\n{}", dfa.get_log().get_messages_str());
            assert_eq!(dfa.log.num_errors(), 0, "test {test_id} failed:\n{}", dfa.get_log().get_messages_str());
        }
    }
    assert_eq!(errors, 0, "{errors} error(s) during the tests");
}

#[test]
fn dfa_normalize() {
    let mut test_id = 0;
    loop {
        let re = build_re(test_id);
        if re.len() == 0 {
            break;
        }
        let dfa = Dfa::<General>::build_from(DfaBuilder::build_from(re));
        // println!("{test_id}: {}", if dfa.is_normalized() { "normalized" } else { "not normalized" });
        // print_dfa(&dfa);
        let dfa = dfa.normalize();
        // print_dfa(&dfa);
        assert!(dfa.is_normalized(), "test {test_id} failed");
        assert_eq!(dfa.first_end_state, Some(dfa.state_graph.len() - dfa.end_states.len()), "test {test_id} failed");
        // println!("-------------------------------------------------");
        test_id += 1;
    }
}

#[test]
fn dfa_modes() {
    let tests: Vec<(usize, BTreeMap<StateId, BTreeMap<Segments, StateId>>, BTreeMap<StateId, Terminal>)> = vec![
        (1, btreemap![
            // mode 0: ([ \t\n\r]*<skip>|/\*<skip,push(1)>|[0-9]+<end:0>)
            // mode 1: (\*/<pop>|[0-9]*<skip>)
            0 => branch!('\t'-'\n', '\r', ' ' => 1, '/' => 2, '0'-'9' => 3), // <skip>
            1 => branch!('\t'-'\n', '\r', ' ' => 1), // <skip>
            2 => branch!('*' => 4),
            3 => branch!('0'-'9' => 3), // <end:0>
            4 => branch!(), // <skip,push(mode 1,state 5)>
            5 => branch!('*' => 6, '0'-'9' => 7), // <skip>
            6 => branch!('/' => 8),
            7 => branch!('0'-'9' => 7), // <skip>
            8 => branch!(), // <skip,pop>
        ], btreemap![
            0 => term!(skip), 1 => term!(skip), 3 => term!(=0),
            4 => term!(push 1) + term!(pushst 5), 5 => term!(skip), 7 => term!(skip), 8 => term!(pop)]),
        (2, btreemap![
            // mode 0: ([ \t\n\r]*<skip>|/\*<skip,push(1)>|[0-9]+<end:0>)
            // mode 1: (\*/<pop>|.*<skip>)
            0 => branch!('\t'-'\n', '\r', ' ' => 1, '/' => 2, '0'-'9' => 3),
            1 => branch!('\t'-'\n', '\r', ' ' => 1), // <skip>
            2 => branch!('*' => 4),
            3 => branch!('0'-'9' => 3), // <end:0>
            4 => branch!(), // <skip,push(mode 1,state 5)>
            5 => branch!('\u{0}'-')', '+'-LOW_MAX, HIGH_MIN-MAX => 6, '*' => 7),
            6 => branch!(DOT => 6), // <skip>
            7 => branch!('\u{0}'-'.', '0'-LOW_MAX, HIGH_MIN-MAX => 6, '/' => 8), // <skip>
            8 => branch!(DOT => 6), // <pop>
        ], btreemap![
            1 => term!(skip), 3 => term!(=0), 4 => term!(push 1) + term!(pushst 5),
            6 => term!(skip), 7 => term!(skip), 8 => term!(pop)
        ]),
        (3, btreemap![
            // mode 0: '\''<more,push(1)>|'.'<end:0>
            // mode 1: '\''<end:1,pop>|[' ', 'a'-'z']<more>
            0 => branch!('\'' => 1, '.' => 2),
            1 => branch!(), // <more,push(mode 1,state 3)>
            2 => branch!(), // <end:0>
            3 => branch!(' ', 'a'-'z' => 4, '\'' => 5),
            4 => branch!(), // <more>
            5 => branch!(), // <end:0,pop>
        ], btreemap![
            1 => term!(more) + term!(push 1) + term!(pushst 3), 2 => term!(=0), 4 => term!(more), 5 => term!(=1) + term!(pop)
        ])
    ];

    const VERBOSE: bool = false;
    for (test_id, exp_graph, exp_ends) in tests {
        if VERBOSE { println!("{test_id}:"); }
        let dfas = build_dfa(test_id);
        for (id, dfa) in dfas.iter() {
            if VERBOSE {
                println!("## mode {id}");
                dfa.print(12);
            }
        }
        if VERBOSE { println!("## Merged:"); }
        let dfa = Dfa::<General>::build_from(DfaBundle::new(dfas));
        assert!(dfa.get_log().has_no_errors(), "test {test_id} failed to build Dfa:\n{}", dfa.get_log().get_messages_str());
        if VERBOSE {
            dfa.print(12);
            println!("-------------------------------------------------");
        }
        assert_eq!(dfa.state_graph, exp_graph, "test {test_id} failed");
        assert_eq!(dfa.end_states, exp_ends, "test {test_id} failed");
    }
}

#[test]
fn dfa_optimize_graphs() {
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
            2 => branch!('i' => 4, 's' => 5), // <end:3>
            3 => branch!(), // <end:2>
            4 => branch!(), // <end:1>
            5 => branch!(), // <end:0>
        ], btreemap![2 => term!(=3), 3 => term!(=2), 4 => term!(=1), 5 => term!(=0)],
        ),

        (17, btreemap![], btreemap![],
         btreemap![
            0 => branch!('a'-'c' => 2, 'd', 'f' => 3, 'e' => 4, 'g'-'i' => 1),
            1 => branch!('d'-'i' => 1, 'z' => 5),
            2 => branch!('a'-'f' => 2), // <end:0>
            3 => branch!('a'-'c' => 2, 'd'-'f' => 3, 'g'-'i' => 1, 'z' => 5), // <end:0>
            4 => branch!('a'-'c' => 2, 'd'-'f' => 3, 'g'-'i' => 1, 'y' => 6, 'z' => 5), // <end:0>
            5 => branch!(), // <end:1>
            6 => branch!(), // <end:2>
        ], btreemap![2 => term!(=0), 3 => term!(=0), 4 => term!(=0), 5 => term!(=1), 6 => term!(=2)]),

    ];
    const VERBOSE: bool = false;
    for (test_id, mut graph, mut end_states, exp_graph, exp_end_states) in tests {
        if VERBOSE { println!("{test_id}:"); }
        if graph.is_empty() {
            // fetches from the build_re
            let re = build_re(test_id);
            let dfa = Dfa::<General>::build_from(DfaBuilder::build_from(re));
            graph = dfa.state_graph;
            end_states = dfa.end_states;
        }
        let mut dfa_builder = DfaBuilder::new();
        let dfa = dfa_builder.build_from_graph(graph, 0, end_states)
            .expect(&format!("test {test_id} failed to build Dfa\n{}", dfa_builder.get_log().get_messages_str()));
        let dfa = dfa.optimize();
        if VERBOSE {
            dfa.print(12);
        }
        assert_eq!(dfa.state_graph, exp_graph, "test {test_id} failed");
        assert_eq!(dfa.end_states, BTreeMap::from_iter(exp_end_states.into_iter()), "test {test_id} failed");
        assert_eq!(dfa.first_end_state, Some(dfa.state_graph.len() - dfa.end_states.len()), "test {test_id} failed");
    }
}

#[test]
fn dfa_error() {
    let tests = vec![
        (200_usize, vec![
            "node #5 is an empty string",
            "node #6 is Or but has no children",
        ]),
        (201, vec![
            "node #1 'a' had 3 child(ren) but shouldn't have any",
            "node #6 is Star but has 0 child(ren) instead of 1 child",
            "node #7 is Plus but has 2 child(ren) instead of 1 child",
        ]),
        (202, vec![
            "node #1 is Concat but has no children",
            "node #2 is Lazy but has 0 child(ren) instead of 1 child",
            "node #3 is Lazy but has 2 child(ren) instead of 1 child",
        ]),
        (203, vec![
            "node #5 is not in followpos; is an accepting state missing? Orphan segment: 'c'"
        ]),
    ];
    const VERBOSE: bool = false;
    for (test_id, expected_error_msgs) in tests {
        if VERBOSE { println!("{:=<80}\n{test_id}:", ""); }
        let re = build_re(test_id);
        assert!(!re.is_empty(), "test {test_id}: tree is empty");
        let dfa_builder = DfaBuilder::build_from(re);
        if VERBOSE { println!("Messages DfaBuilder:\n{}", dfa_builder.get_log().get_messages_str())}
        let dfa = Dfa::<General>::build_from(dfa_builder.clone());
        if VERBOSE { println!("Messages Dfa:\n{}", dfa.get_log().get_messages_str()); }
        let result_error_msgs = dfa.get_log().get_errors().to_vec();
        assert_eq!(expected_error_msgs, result_error_msgs, "test {test_id} failed");

        let try_dfa = Dfa::<General>::try_build_from(dfa_builder);
        assert!(try_dfa.is_err(), "test {test_id} failed: try_from doesn't return an error");
    }
}
